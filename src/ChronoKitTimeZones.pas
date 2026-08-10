unit ChronoKitTimeZones;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, DateUtils;

type
  EChronoKitTimeZoneEngine = class(Exception);

  TChronoKitLocalTimeStatus = (
    ckLocalTimeValid,
    ckLocalTimeAmbiguous,
    ckLocalTimeNonexistent
  );

  TChronoKitZoneInfo = record
    Name: string;
    Offset: Integer;
    IsDST: Boolean;
  end;

  TChronoKitTimeZoneNames = array of string;

function CKNormalizeTimeZoneName(const ATimeZone: string): string;
function CKIsTimeZoneAvailable(const ATimeZone: string): Boolean;
function CKGetSystemTimeZone: string;
function CKGetTimeZoneNames: TChronoKitTimeZoneNames;
function CKResolveLocalTime(const ALocalValue: TDateTime;
  const ATimeZone: string; out AUTCValue: TDateTime;
  out AInfo: TChronoKitZoneInfo): TChronoKitLocalTimeStatus;
procedure CKConvertUTCToLocal(const AUTCValue: TDateTime;
  const ATimeZone: string; out ALocalValue: TDateTime;
  out AInfo: TChronoKitZoneInfo);

implementation

uses
  {$IFDEF WINDOWS}
  Windows, Registry;
  {$ELSE}
  BaseUnix, StrUtils;
  {$ENDIF}

const
  CKMinutesPerDay = 24 * 60;
  CKSecondsPerDay = 24 * 60 * 60;
  CKOneMillisecond = 1 / (CKSecondsPerDay * 1000);

function IsUTCName(const ATimeZone: string): Boolean;
begin
  Result := ATimeZone = 'UTC';
  {$IFDEF UNIX}
  Result := Result or (ATimeZone = 'Etc/UTC') or
    (ATimeZone = '/Etc/UTC');
  {$ENDIF}
end;

function CKNormalizeTimeZoneName(const ATimeZone: string): string;
begin
  if IsUTCName(ATimeZone) then
    Result := 'UTC'
  else
    Result := ATimeZone;
end;

procedure SetUTCInfo(out AInfo: TChronoKitZoneInfo);
begin
  AInfo.Name := 'UTC';
  AInfo.Offset := 0;
  AInfo.IsDST := False;
end;

{$IFDEF WINDOWS}

const
  WindowsTimeZonesKey =
    '\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Time Zones\';
  WindowsSystemTimeZoneKey =
    '\SYSTEM\CurrentControlSet\Control\TimeZoneInformation';

type
  TCKRegistryTimeZoneInformation = record
    Bias: LongInt;
    StandardBias: LongInt;
    DaylightBias: LongInt;
    StandardDate: Windows.TSystemTime;
    DaylightDate: Windows.TSystemTime;
  end;

function CKSystemTimeToTzSpecificLocalTime(
  ATimeZoneInformation: Windows.PTimeZoneInformation;
  AUniversalTime, ALocalTime: Windows.PSystemTime): Windows.BOOL; stdcall;
  external 'kernel32.dll' name 'SystemTimeToTzSpecificLocalTime';

procedure CopyToWideBuffer(const AValue: string;
  var ABuffer: array of WideChar);
var
  Index: Integer;
  WideValue: UnicodeString;
begin
  FillChar(ABuffer[0], Length(ABuffer) * SizeOf(WideChar), 0);
  WideValue := UnicodeString(AValue);
  for Index := 1 to Length(WideValue) do
  begin
    if Index > High(ABuffer) then
      Break;
    ABuffer[Index - 1] := WideValue[Index];
  end;
end;

function LoadWindowsTimeZone(const ATimeZone: string; const AYear: Word;
  out AData: Windows.TTimeZoneInformation): Boolean;
var
  BaseKey, DynamicKey, DaylightName, StandardName: string;
  FirstRuleYear, LastRuleYear, RuleYear: Integer;
  RawData: TCKRegistryTimeZoneInformation;
  Registry: TRegistry;
begin
  Result := False;
  FillChar(AData, SizeOf(AData), 0);
  FillChar(RawData, SizeOf(RawData), 0);
  BaseKey := WindowsTimeZonesKey + ATimeZone;
  Registry := TRegistry.Create(KEY_READ);
  try
    Registry.RootKey := HKEY_LOCAL_MACHINE;
    if not Registry.OpenKeyReadOnly(BaseKey) then
      Exit;
    if Registry.GetDataSize('TZI') <> SizeOf(RawData) then
      raise EChronoKitTimeZoneEngine.CreateFmt(
        'Timezone "%s" has invalid Windows rule data', [ATimeZone]);
    Registry.ReadBinaryData('TZI', RawData, SizeOf(RawData));
    if Registry.ValueExists('Std') then
      StandardName := Registry.ReadString('Std')
    else
      StandardName := ATimeZone;
    if Registry.ValueExists('Dlt') then
      DaylightName := Registry.ReadString('Dlt')
    else
      DaylightName := StandardName;
    Registry.CloseKey;

    DynamicKey := BaseKey + '\Dynamic DST';
    if Registry.OpenKeyReadOnly(DynamicKey) then
    begin
      RuleYear := AYear;
      if Registry.ValueExists('FirstEntry') and
        Registry.ValueExists('LastEntry') then
      begin
        FirstRuleYear := Registry.ReadInteger('FirstEntry');
        LastRuleYear := Registry.ReadInteger('LastEntry');
        if RuleYear < FirstRuleYear then
          RuleYear := FirstRuleYear
        else if RuleYear > LastRuleYear then
          RuleYear := LastRuleYear;
      end;
      if Registry.ValueExists(IntToStr(RuleYear)) and
        (Registry.GetDataSize(IntToStr(RuleYear)) = SizeOf(RawData)) then
        Registry.ReadBinaryData(IntToStr(RuleYear), RawData,
          SizeOf(RawData));
      Registry.CloseKey;
    end;

    AData.Bias := RawData.Bias;
    AData.StandardBias := RawData.StandardBias;
    AData.DaylightBias := RawData.DaylightBias;
    AData.StandardDate := RawData.StandardDate;
    AData.DaylightDate := RawData.DaylightDate;
    CopyToWideBuffer(StandardName, AData.StandardName);
    CopyToWideBuffer(DaylightName, AData.DaylightName);
    Result := True;
  finally
    Registry.Free;
  end;
end;

function WindowsSystemTimeZone: string;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create(KEY_READ);
  try
    Registry.RootKey := HKEY_LOCAL_MACHINE;
    if not Registry.OpenKeyReadOnly(WindowsSystemTimeZoneKey) or
      not Registry.ValueExists('TimeZoneKeyName') then
      raise EChronoKitTimeZoneEngine.Create(
        'Windows did not return a system timezone identifier');
    Result := Trim(Registry.ReadString('TimeZoneKeyName'));
  finally
    Registry.Free;
  end;
  if Result = '' then
    raise EChronoKitTimeZoneEngine.Create(
      'Windows did not return a system timezone identifier');
end;

function WindowsTimeZoneNames: TChronoKitTimeZoneNames;
var
  Index: Integer;
  Names: TStringList;
  Registry: TRegistry;
begin
  Result := nil;
  Names := TStringList.Create;
  Registry := TRegistry.Create(KEY_READ);
  try
    Registry.RootKey := HKEY_LOCAL_MACHINE;
    if not Registry.OpenKeyReadOnly(Copy(WindowsTimeZonesKey, 1,
      Length(WindowsTimeZonesKey) - 1)) then
      raise EChronoKitTimeZoneEngine.Create(
        'Windows timezone catalog is unavailable');
    Registry.GetKeyNames(Names);
    if Names.IndexOf('UTC') < 0 then
      Names.Insert(0, 'UTC');
    SetLength(Result, Names.Count);
    for Index := 0 to Names.Count - 1 do
      Result[Index] := Names[Index];
  finally
    Registry.Free;
    Names.Free;
  end;
end;

procedure WindowsConvertUTCWithData(const AUTCValue: TDateTime;
  const ATimeZone: string; const AData: Windows.TTimeZoneInformation;
  out ALocalValue: TDateTime; out AInfo: TChronoKitZoneInfo);
var
  LocalSystemTime, UTCSystemTime: Windows.TSystemTime;
  StandardOffset: Integer;
begin
  DateTimeToSystemTime(AUTCValue, UTCSystemTime);
  FillChar(LocalSystemTime, SizeOf(LocalSystemTime), 0);
  if not CKSystemTimeToTzSpecificLocalTime(@AData, @UTCSystemTime,
    @LocalSystemTime) then
    raise EChronoKitTimeZoneEngine.CreateFmt(
      'Windows could not convert UTC for timezone "%s" (error %d)',
      [ATimeZone, GetLastError]);

  ALocalValue := SystemTimeToDateTime(LocalSystemTime);
  AInfo.Name := ATimeZone;
  AInfo.Offset := Round((ALocalValue - AUTCValue) * CKMinutesPerDay);
  StandardOffset := -AData.Bias - AData.StandardBias;
  AInfo.IsDST := (AData.DaylightDate.wMonth <> 0) and
    (AInfo.Offset <> StandardOffset);
end;

procedure WindowsConvertUTCToLocal(const AUTCValue: TDateTime;
  const ATimeZone: string; out ALocalValue: TDateTime;
  out AInfo: TChronoKitZoneInfo);
var
  Data: Windows.TTimeZoneInformation;
  LocalYear: Word;
  ZoneName: string;
begin
  ZoneName := CKNormalizeTimeZoneName(ATimeZone);
  if ZoneName = 'UTC' then
  begin
    ALocalValue := AUTCValue;
    SetUTCInfo(AInfo);
    Exit;
  end;
  if not LoadWindowsTimeZone(ZoneName, YearOf(AUTCValue), Data) then
    raise EChronoKitTimeZoneEngine.CreateFmt(
      'Timezone "%s" not found', [ATimeZone]);
  WindowsConvertUTCWithData(AUTCValue, ZoneName, Data,
    ALocalValue, AInfo);
  LocalYear := YearOf(ALocalValue);
  if LocalYear <> YearOf(AUTCValue) then
  begin
    if not LoadWindowsTimeZone(ZoneName, LocalYear, Data) then
      raise EChronoKitTimeZoneEngine.CreateFmt(
        'Timezone "%s" not found', [ATimeZone]);
    WindowsConvertUTCWithData(AUTCValue, ZoneName, Data,
      ALocalValue, AInfo);
  end;
end;

procedure AddUniqueOffset(var AOffsets: array of Integer;
  var ACount: Integer; const AOffset: Integer);
var
  Index: Integer;
begin
  for Index := 0 to ACount - 1 do
    if AOffsets[Index] = AOffset then
      Exit;
  if ACount >= Length(AOffsets) then
    raise EChronoKitTimeZoneEngine.Create(
      'Timezone returned too many distinct UTC offsets');
  AOffsets[ACount] := AOffset;
  Inc(ACount);
end;

function WindowsResolveLocalTime(const ALocalValue: TDateTime;
  const ATimeZone: string; out AUTCValue: TDateTime;
  out AInfo: TChronoKitZoneInfo): TChronoKitLocalTimeStatus;
var
  CandidateInfo: TChronoKitZoneInfo;
  CandidateLocal, CandidateUTC: TDateTime;
  CandidateOffsets: array[0..1] of Integer;
  CandidateCount, MatchCount, OffsetIndex: Integer;
  TimeZoneData: Windows.TTimeZoneInformation;
  ZoneName: string;
begin
  ZoneName := CKNormalizeTimeZoneName(ATimeZone);
  if ZoneName = 'UTC' then
  begin
    AUTCValue := ALocalValue;
    SetUTCInfo(AInfo);
    Exit(ckLocalTimeValid);
  end;

  if not LoadWindowsTimeZone(ZoneName, YearOf(ALocalValue),
    TimeZoneData) then
    raise EChronoKitTimeZoneEngine.CreateFmt(
      'Timezone "%s" not found', [ATimeZone]);

  FillChar(CandidateOffsets, SizeOf(CandidateOffsets), 0);
  CandidateCount := 0;
  AddUniqueOffset(CandidateOffsets, CandidateCount,
    -TimeZoneData.Bias - TimeZoneData.StandardBias);
  if TimeZoneData.DaylightDate.wMonth <> 0 then
    AddUniqueOffset(CandidateOffsets, CandidateCount,
      -TimeZoneData.Bias - TimeZoneData.DaylightBias);

  MatchCount := 0;
  for OffsetIndex := 0 to CandidateCount - 1 do
  begin
    CandidateUTC := ALocalValue -
      (CandidateOffsets[OffsetIndex] / CKMinutesPerDay);
    WindowsConvertUTCWithData(CandidateUTC, ZoneName, TimeZoneData,
      CandidateLocal, CandidateInfo);
    if Abs(CandidateLocal - ALocalValue) <= CKOneMillisecond then
    begin
      if MatchCount = 0 then
      begin
        AUTCValue := CandidateUTC;
        AInfo := CandidateInfo;
      end;
      Inc(MatchCount);
    end;
  end;

  if MatchCount = 0 then
    Result := ckLocalTimeNonexistent
  else if MatchCount = 1 then
    Result := ckLocalTimeValid
  else
    Result := ckLocalTimeAmbiguous;
end;

{$ELSE}

const
  ZoneInfoRoot = '/usr/share/zoneinfo/';
  UnixEpoch: TDateTime = 25569.0;

type
  TPOSIXRuleKind = (prJulianNoLeap, prJulian, prMonthWeekDay);

  TPOSIXTransitionRule = record
    Kind: TPOSIXRuleKind;
    DayNumber: Integer;
    Month: Integer;
    Week: Integer;
    WeekDay: Integer;
    SecondsOfDay: Integer;
    TimeMode: Char;
  end;

  TPOSIXTimeZone = record
    Valid: Boolean;
    HasDST: Boolean;
    StandardOffsetSeconds: LongInt;
    DaylightOffsetSeconds: LongInt;
    StartRule: TPOSIXTransitionRule;
    EndRule: TPOSIXTransitionRule;
  end;

  TTZifHeader = record
    Version: Char;
    GMTCount: Cardinal;
    StandardCount: Cardinal;
    LeapCount: Cardinal;
    TransitionCount: Cardinal;
    TypeCount: Cardinal;
    CharacterCount: Cardinal;
  end;

  TTZifType = record
    OffsetSeconds: LongInt;
    IsDST: Boolean;
    AbbreviationIndex: Byte;
  end;

  TTZifTable = record
    TransitionTimes: array of Int64;
    TransitionTypes: array of Byte;
    Types: array of TTZifType;
    Abbreviations: RawByteString;
    DefaultType: Integer;
    FutureRule: TPOSIXTimeZone;
  end;

procedure ReadExact(const AStream: TStream; var ABuffer;
  const ACount: LongInt);
begin
  if (ACount < 0) or (AStream.Read(ABuffer, ACount) <> ACount) then
    raise EChronoKitTimeZoneEngine.Create(
      'Timezone data ended unexpectedly');
end;

function ReadByte(const AStream: TStream): Byte;
begin
  ReadExact(AStream, Result, 1);
end;

function ReadBECardinal(const AStream: TStream): Cardinal;
var
  Bytes: array[0..3] of Byte;
begin
  ReadExact(AStream, Bytes, SizeOf(Bytes));
  Result := (Cardinal(Bytes[0]) shl 24) or
    (Cardinal(Bytes[1]) shl 16) or
    (Cardinal(Bytes[2]) shl 8) or Cardinal(Bytes[3]);
end;

function ReadBELongInt(const AStream: TStream): LongInt;
begin
  Result := LongInt(ReadBECardinal(AStream));
end;

function ReadBEInt64(const AStream: TStream): Int64;
var
  Bytes: array[0..7] of Byte;
  Value: QWord;
begin
  ReadExact(AStream, Bytes, SizeOf(Bytes));
  Value := (QWord(Bytes[0]) shl 56) or
    (QWord(Bytes[1]) shl 48) or
    (QWord(Bytes[2]) shl 40) or
    (QWord(Bytes[3]) shl 32) or
    (QWord(Bytes[4]) shl 24) or
    (QWord(Bytes[5]) shl 16) or
    (QWord(Bytes[6]) shl 8) or QWord(Bytes[7]);
  Result := Int64(Value);
end;

function ReadTZifHeader(const AStream: TStream): TTZifHeader;
var
  Magic: array[0..3] of AnsiChar;
  Reserved: array[0..14] of Byte;
begin
  ReadExact(AStream, Magic, SizeOf(Magic));
  if (Magic[0] <> 'T') or (Magic[1] <> 'Z') or
    (Magic[2] <> 'i') or (Magic[3] <> 'f') then
    raise EChronoKitTimeZoneEngine.Create('Invalid TZif file signature');
  Result.Version := Char(ReadByte(AStream));
  ReadExact(AStream, Reserved, SizeOf(Reserved));
  Result.GMTCount := ReadBECardinal(AStream);
  Result.StandardCount := ReadBECardinal(AStream);
  Result.LeapCount := ReadBECardinal(AStream);
  Result.TransitionCount := ReadBECardinal(AStream);
  Result.TypeCount := ReadBECardinal(AStream);
  Result.CharacterCount := ReadBECardinal(AStream);

  if (Result.TypeCount = 0) or (Result.TypeCount > 256) or
    (Result.TransitionCount > 1000000) or
    (Result.CharacterCount > 1048576) or
    (Result.LeapCount > 1000000) then
    raise EChronoKitTimeZoneEngine.Create('Invalid TZif table counts');
end;

function TZifBlockSize(const AHeader: TTZifHeader;
  const ATimeSize: Integer): Int64;
begin
  Result := Int64(AHeader.TransitionCount) * ATimeSize +
    Int64(AHeader.TransitionCount) +
    Int64(AHeader.TypeCount) * 6 +
    Int64(AHeader.CharacterCount) +
    Int64(AHeader.LeapCount) * (ATimeSize + 4) +
    Int64(AHeader.StandardCount) + Int64(AHeader.GMTCount);
end;

function ParsePOSIXNumber(const AText: string; var AIndex: Integer;
  out AValue: Integer): Boolean;
begin
  Result := False;
  AValue := 0;
  if (AIndex > Length(AText)) or
    not (AText[AIndex] in ['0'..'9']) then
    Exit;
  while (AIndex <= Length(AText)) and
    (AText[AIndex] in ['0'..'9']) do
  begin
    if AValue > 1000000 then
      Exit;
    AValue := AValue * 10 + Ord(AText[AIndex]) - Ord('0');
    Inc(AIndex);
  end;
  Result := True;
end;

function ParsePOSIXClock(const AText: string; var AIndex: Integer;
  out ASeconds: Integer): Boolean;
var
  Hours, Minutes, Seconds, Sign: Integer;
begin
  Result := False;
  Sign := 1;
  if (AIndex <= Length(AText)) and (AText[AIndex] in ['+', '-']) then
  begin
    if AText[AIndex] = '-' then
      Sign := -1;
    Inc(AIndex);
  end;
  if not ParsePOSIXNumber(AText, AIndex, Hours) or (Hours > 167) then
    Exit;
  Minutes := 0;
  Seconds := 0;
  if (AIndex <= Length(AText)) and (AText[AIndex] = ':') then
  begin
    Inc(AIndex);
    if not ParsePOSIXNumber(AText, AIndex, Minutes) or
      (Minutes > 59) then
      Exit;
    if (AIndex <= Length(AText)) and (AText[AIndex] = ':') then
    begin
      Inc(AIndex);
      if not ParsePOSIXNumber(AText, AIndex, Seconds) or
        (Seconds > 59) then
        Exit;
    end;
  end;
  ASeconds := Sign * (Hours * 3600 + Minutes * 60 + Seconds);
  Result := True;
end;

function ParsePOSIXName(const AText: string; var AIndex: Integer): Boolean;
var
  NameStart: Integer;
begin
  Result := False;
  if AIndex > Length(AText) then
    Exit;
  if AText[AIndex] = '<' then
  begin
    Inc(AIndex);
    NameStart := AIndex;
    while (AIndex <= Length(AText)) and (AText[AIndex] <> '>') do
      Inc(AIndex);
    if (AIndex > Length(AText)) or (AIndex = NameStart) then
      Exit;
    Inc(AIndex);
    Exit(True);
  end;

  NameStart := AIndex;
  while (AIndex <= Length(AText)) and
    (AText[AIndex] in ['A'..'Z', 'a'..'z']) do
    Inc(AIndex);
  Result := AIndex - NameStart >= 3;
end;

function ParsePOSIXTransitionRule(const AText: string;
  var AIndex: Integer; out ARule: TPOSIXTransitionRule): Boolean;
var
  Value: Integer;
begin
  Result := False;
  FillChar(ARule, SizeOf(ARule), 0);
  ARule.SecondsOfDay := 2 * 3600;
  ARule.TimeMode := 'w';
  if AIndex > Length(AText) then
    Exit;

  if AText[AIndex] = 'M' then
  begin
    ARule.Kind := prMonthWeekDay;
    Inc(AIndex);
    if not ParsePOSIXNumber(AText, AIndex, ARule.Month) or
      (ARule.Month < 1) or (ARule.Month > 12) or
      (AIndex > Length(AText)) or (AText[AIndex] <> '.') then
      Exit;
    Inc(AIndex);
    if not ParsePOSIXNumber(AText, AIndex, ARule.Week) or
      (ARule.Week < 1) or (ARule.Week > 5) or
      (AIndex > Length(AText)) or (AText[AIndex] <> '.') then
      Exit;
    Inc(AIndex);
    if not ParsePOSIXNumber(AText, AIndex, ARule.WeekDay) or
      (ARule.WeekDay < 0) or (ARule.WeekDay > 6) then
      Exit;
  end
  else if AText[AIndex] = 'J' then
  begin
    ARule.Kind := prJulianNoLeap;
    Inc(AIndex);
    if not ParsePOSIXNumber(AText, AIndex, ARule.DayNumber) or
      (ARule.DayNumber < 1) or (ARule.DayNumber > 365) then
      Exit;
  end
  else
  begin
    ARule.Kind := prJulian;
    if not ParsePOSIXNumber(AText, AIndex, Value) or
      (Value < 0) or (Value > 365) then
      Exit;
    ARule.DayNumber := Value;
  end;

  if (AIndex <= Length(AText)) and (AText[AIndex] = '/') then
  begin
    Inc(AIndex);
    if not ParsePOSIXClock(AText, AIndex, ARule.SecondsOfDay) then
      Exit;
    if (AIndex <= Length(AText)) and
      (AText[AIndex] in ['w', 's', 'u', 'g', 'z']) then
    begin
      ARule.TimeMode := AText[AIndex];
      Inc(AIndex);
    end;
  end;
  Result := True;
end;

function ParsePOSIXTimeZone(const AText: string;
  out ATimeZone: TPOSIXTimeZone): Boolean;
var
  Index, OffsetSeconds: Integer;
begin
  FillChar(ATimeZone, SizeOf(ATimeZone), 0);
  Result := False;
  Index := 1;
  if not ParsePOSIXName(AText, Index) or
    not ParsePOSIXClock(AText, Index, OffsetSeconds) then
    Exit;
  ATimeZone.StandardOffsetSeconds := -OffsetSeconds;

  if Index > Length(AText) then
  begin
    ATimeZone.Valid := True;
    Exit(True);
  end;

  if not ParsePOSIXName(AText, Index) then
    Exit;
  ATimeZone.HasDST := True;
  ATimeZone.DaylightOffsetSeconds :=
    ATimeZone.StandardOffsetSeconds + 3600;
  if (Index <= Length(AText)) and (AText[Index] <> ',') then
  begin
    if not ParsePOSIXClock(AText, Index, OffsetSeconds) then
      Exit;
    ATimeZone.DaylightOffsetSeconds := -OffsetSeconds;
  end;

  if Index > Length(AText) then
  begin
    ATimeZone.StartRule.Kind := prMonthWeekDay;
    ATimeZone.StartRule.Month := 3;
    ATimeZone.StartRule.Week := 2;
    ATimeZone.StartRule.WeekDay := 0;
    ATimeZone.StartRule.SecondsOfDay := 2 * 3600;
    ATimeZone.StartRule.TimeMode := 'w';
    ATimeZone.EndRule.Kind := prMonthWeekDay;
    ATimeZone.EndRule.Month := 11;
    ATimeZone.EndRule.Week := 1;
    ATimeZone.EndRule.WeekDay := 0;
    ATimeZone.EndRule.SecondsOfDay := 2 * 3600;
    ATimeZone.EndRule.TimeMode := 'w';
  end
  else
  begin
    if AText[Index] <> ',' then
      Exit;
    Inc(Index);
    if not ParsePOSIXTransitionRule(AText, Index,
      ATimeZone.StartRule) or (Index > Length(AText)) or
      (AText[Index] <> ',') then
      Exit;
    Inc(Index);
    if not ParsePOSIXTransitionRule(AText, Index,
      ATimeZone.EndRule) or (Index <= Length(AText)) then
      Exit;
  end;

  ATimeZone.Valid := True;
  Result := True;
end;

function ReadTZifFooter(const AStream: TStream): string;
var
  Bytes: array of Byte;
  Index: Integer;
begin
  Result := '';
  if AStream.Position = AStream.Size then
    Exit;
  if (AStream.Size - AStream.Position > 4096) or
    (AStream.Size - AStream.Position < 2) then
    raise EChronoKitTimeZoneEngine.Create('Invalid TZif footer size');
  SetLength(Bytes, AStream.Size - AStream.Position);
  ReadExact(AStream, Bytes[0], Length(Bytes));
  if (Bytes[0] <> 10) or (Bytes[High(Bytes)] <> 10) then
    raise EChronoKitTimeZoneEngine.Create('Invalid TZif footer framing');
  SetLength(Result, Length(Bytes) - 2);
  for Index := 1 to High(Bytes) - 1 do
    Result[Index] := Char(Bytes[Index]);
end;

procedure ParseTZifBlock(const AStream: TStream;
  const AHeader: TTZifHeader; const ATimeSize: Integer;
  out ATable: TTZifTable);
var
  AbbreviationBytes: array of Byte;
  AncillarySize: Int64;
  Index: Integer;
begin
  FillChar(ATable.FutureRule, SizeOf(ATable.FutureRule), 0);
  SetLength(ATable.TransitionTimes, AHeader.TransitionCount);
  SetLength(ATable.TransitionTypes, AHeader.TransitionCount);
  SetLength(ATable.Types, AHeader.TypeCount);

  for Index := 0 to High(ATable.TransitionTimes) do
    if ATimeSize = 8 then
      ATable.TransitionTimes[Index] := ReadBEInt64(AStream)
    else
      ATable.TransitionTimes[Index] := ReadBELongInt(AStream);

  for Index := 0 to High(ATable.TransitionTypes) do
  begin
    ATable.TransitionTypes[Index] := ReadByte(AStream);
    if ATable.TransitionTypes[Index] >= AHeader.TypeCount then
      raise EChronoKitTimeZoneEngine.Create(
        'TZif transition references an invalid type');
  end;

  for Index := 0 to High(ATable.Types) do
  begin
    ATable.Types[Index].OffsetSeconds := ReadBELongInt(AStream);
    ATable.Types[Index].IsDST := ReadByte(AStream) <> 0;
    ATable.Types[Index].AbbreviationIndex := ReadByte(AStream);
    if ATable.Types[Index].AbbreviationIndex >= AHeader.CharacterCount then
      raise EChronoKitTimeZoneEngine.Create(
        'TZif type references an invalid abbreviation');
  end;

  SetLength(AbbreviationBytes, AHeader.CharacterCount);
  if Length(AbbreviationBytes) > 0 then
    ReadExact(AStream, AbbreviationBytes[0], Length(AbbreviationBytes));
  SetLength(ATable.Abbreviations, Length(AbbreviationBytes));
  for Index := 0 to High(AbbreviationBytes) do
    ATable.Abbreviations[Index + 1] := AnsiChar(AbbreviationBytes[Index]);

  ATable.DefaultType := 0;
  if (Length(ATable.TransitionTypes) > 0) and
    ATable.Types[ATable.TransitionTypes[0]].IsDST then
    for Index := 0 to High(ATable.Types) do
      if not ATable.Types[Index].IsDST then
      begin
        ATable.DefaultType := Index;
        Break;
      end;

  AncillarySize := Int64(AHeader.LeapCount) * (ATimeSize + 4) +
    Int64(AHeader.StandardCount) + Int64(AHeader.GMTCount);
  if (AncillarySize < 0) or
    (AStream.Position + AncillarySize > AStream.Size) then
    raise EChronoKitTimeZoneEngine.Create('Invalid TZif ancillary data size');
  AStream.Position := AStream.Position + AncillarySize;
end;

function SafeLinuxZoneName(const ATimeZone: string): Boolean;
begin
  Result := (ATimeZone <> '') and (ATimeZone[1] <> '/') and
    (Pos('..', ATimeZone) = 0) and (Pos('\', ATimeZone) = 0) and
    (Pos(':', ATimeZone) = 0) and (Pos(#0, ATimeZone) = 0);
end;

function LinuxZonePath(const ATimeZone: string): string;
var
  ZoneName: string;
begin
  ZoneName := CKNormalizeTimeZoneName(ATimeZone);
  if ZoneName = 'UTC' then
    Exit(ZoneInfoRoot + 'UTC');
  if not SafeLinuxZoneName(ZoneName) then
    Exit('');
  Result := ZoneInfoRoot + ZoneName;
end;

procedure LoadTZifTable(const ATimeZone: string; out ATable: TTZifTable);
var
  BlockEnd: Int64;
  Footer: string;
  FirstHeader, SecondHeader: TTZifHeader;
  Stream: TFileStream;
  ZonePath: string;
begin
  ZonePath := LinuxZonePath(ATimeZone);
  if (ZonePath = '') or not FileExists(ZonePath) then
    raise EChronoKitTimeZoneEngine.CreateFmt(
      'Timezone "%s" not found', [ATimeZone]);

  Stream := TFileStream.Create(ZonePath, fmOpenRead or fmShareDenyWrite);
  try
    FirstHeader := ReadTZifHeader(Stream);
    if FirstHeader.Version in ['2', '3', '4'] then
    begin
      BlockEnd := Stream.Position + TZifBlockSize(FirstHeader, 4);
      if (BlockEnd < Stream.Position) or (BlockEnd > Stream.Size) then
        raise EChronoKitTimeZoneEngine.Create('Invalid TZif block size');
      Stream.Position := BlockEnd;
      SecondHeader := ReadTZifHeader(Stream);
      ParseTZifBlock(Stream, SecondHeader, 8, ATable);
      Footer := ReadTZifFooter(Stream);
      if (Footer <> '') and not ParsePOSIXTimeZone(Footer,
        ATable.FutureRule) then
        raise EChronoKitTimeZoneEngine.Create(
          'Timezone has an unsupported TZif future rule');
    end
    else
      ParseTZifBlock(Stream, FirstHeader, 4, ATable);
  finally
    Stream.Free;
  end;
end;

function TZifTypeAtUTC(const ATable: TTZifTable;
  const AUTCSeconds: Int64): Integer;
var
  HighIndex, LowIndex, MiddleIndex, TransitionIndex: Integer;
begin
  if (Length(ATable.TransitionTimes) = 0) or
    (AUTCSeconds < ATable.TransitionTimes[0]) then
    Exit(ATable.DefaultType);

  LowIndex := 0;
  HighIndex := High(ATable.TransitionTimes);
  TransitionIndex := 0;
  while LowIndex <= HighIndex do
  begin
    MiddleIndex := LowIndex + (HighIndex - LowIndex) div 2;
    if ATable.TransitionTimes[MiddleIndex] <= AUTCSeconds then
    begin
      TransitionIndex := MiddleIndex;
      LowIndex := MiddleIndex + 1;
    end
    else
      HighIndex := MiddleIndex - 1;
  end;
  Result := ATable.TransitionTypes[TransitionIndex];
end;

function POSIXRuleDate(const AYear: Word;
  const ARule: TPOSIXTransitionRule): TDateTime;
var
  DayOfMonth, FirstWeekDay: Integer;
begin
  case ARule.Kind of
    prJulianNoLeap:
      begin
        Result := EncodeDate(AYear, 1, 1) + ARule.DayNumber - 1;
        if IsLeapYear(AYear) and (ARule.DayNumber >= 60) then
          Result := Result + 1;
      end;
    prJulian:
      Result := EncodeDate(AYear, 1, 1) + ARule.DayNumber;
    prMonthWeekDay:
      begin
        Result := EncodeDate(AYear, ARule.Month, 1);
        FirstWeekDay := DayOfWeek(Result) - 1;
        DayOfMonth := 1 +
          ((ARule.WeekDay - FirstWeekDay + 7) mod 7) +
          (ARule.Week - 1) * 7;
        if DayOfMonth > DaysInAMonth(AYear, ARule.Month) then
          Dec(DayOfMonth, 7);
        Result := EncodeDate(AYear, ARule.Month, DayOfMonth);
      end;
  end;
  Result := Result + (ARule.SecondsOfDay / CKSecondsPerDay);
end;

function POSIXTransitionUTC(const AYear: Word;
  const ARule: TPOSIXTransitionRule; const AIsStart: Boolean;
  const ATimeZone: TPOSIXTimeZone): TDateTime;
var
  OffsetSeconds: LongInt;
begin
  Result := POSIXRuleDate(AYear, ARule);
  case ARule.TimeMode of
    'u', 'g', 'z': OffsetSeconds := 0;
    's': OffsetSeconds := ATimeZone.StandardOffsetSeconds;
    else
      if AIsStart then
        OffsetSeconds := ATimeZone.StandardOffsetSeconds
      else
        OffsetSeconds := ATimeZone.DaylightOffsetSeconds;
  end;
  Result := Result - (OffsetSeconds / CKSecondsPerDay);
end;

procedure POSIXInfoAtUTC(const AUTCSeconds: Int64;
  const ATimeZone: TPOSIXTimeZone; out AOffsetSeconds: LongInt;
  out AIsDST: Boolean);
var
  EndUTC, StartUTC, UTCValue: TDateTime;
  UTCYear: Word;
begin
  AIsDST := False;
  AOffsetSeconds := ATimeZone.StandardOffsetSeconds;
  if not ATimeZone.HasDST then
    Exit;

  UTCValue := UnixEpoch + (AUTCSeconds / CKSecondsPerDay);
  UTCYear := YearOf(UTCValue);
  StartUTC := POSIXTransitionUTC(UTCYear, ATimeZone.StartRule,
    True, ATimeZone);
  EndUTC := POSIXTransitionUTC(UTCYear, ATimeZone.EndRule,
    False, ATimeZone);
  if StartUTC < EndUTC then
    AIsDST := (UTCValue >= StartUTC) and (UTCValue < EndUTC)
  else
    AIsDST := (UTCValue >= StartUTC) or (UTCValue < EndUTC);
  if AIsDST then
    AOffsetSeconds := ATimeZone.DaylightOffsetSeconds;
end;

procedure TZifInfoAtUTC(const ATable: TTZifTable;
  const AUTCSeconds: Int64; out AOffsetSeconds: LongInt;
  out AIsDST: Boolean);
var
  TypeIndex: Integer;
begin
  if ATable.FutureRule.Valid and
    ((Length(ATable.TransitionTimes) = 0) or
    (AUTCSeconds > ATable.TransitionTimes[High(ATable.TransitionTimes)])) then
  begin
    POSIXInfoAtUTC(AUTCSeconds, ATable.FutureRule,
      AOffsetSeconds, AIsDST);
    Exit;
  end;

  TypeIndex := TZifTypeAtUTC(ATable, AUTCSeconds);
  AOffsetSeconds := ATable.Types[TypeIndex].OffsetSeconds;
  AIsDST := ATable.Types[TypeIndex].IsDST;
end;

procedure AddName(const ANames: TStrings; const AName: string);
begin
  if (AName <> '') and (ANames.IndexOf(AName) < 0) then
    ANames.Add(AName);
end;

procedure LoadZoneTab(const AFileName: string; const ANames: TStrings);
var
  FirstTab, Index, SecondTab, ThirdTab: Integer;
  Line, ZoneName: string;
  Lines: TStringList;
begin
  if not FileExists(AFileName) then
    Exit;
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(AFileName);
    for Index := 0 to Lines.Count - 1 do
    begin
      Line := Lines[Index];
      if (Line = '') or (Line[1] = '#') then
        Continue;
      FirstTab := Pos(#9, Line);
      if FirstTab = 0 then
        Continue;
      SecondTab := PosEx(#9, Line, FirstTab + 1);
      if SecondTab = 0 then
        Continue;
      ThirdTab := PosEx(#9, Line, SecondTab + 1);
      if ThirdTab = 0 then
        ThirdTab := Length(Line) + 1;
      ZoneName := Copy(Line, SecondTab + 1, ThirdTab - SecondTab - 1);
      if FileExists(LinuxZonePath(ZoneName)) then
        AddName(ANames, ZoneName);
    end;
  finally
    Lines.Free;
  end;
end;

function LinuxTimeZoneNames: TChronoKitTimeZoneNames;
var
  Index: Integer;
  Names: TStringList;
begin
  Result := nil;
  Names := TStringList.Create;
  try
    Names.Sorted := True;
    Names.Duplicates := dupIgnore;
    AddName(Names, 'UTC');
    AddName(Names, 'Etc/UTC');
    AddName(Names, '/Etc/UTC');
    LoadZoneTab(ZoneInfoRoot + 'zone1970.tab', Names);
    LoadZoneTab(ZoneInfoRoot + 'zone.tab', Names);
    SetLength(Result, Names.Count);
    for Index := 0 to Names.Count - 1 do
      Result[Index] := Names[Index];
  finally
    Names.Free;
  end;
end;

function LinuxTimeZoneAvailable(const ATimeZone: string): Boolean;
var
  Index: Integer;
  Names: TChronoKitTimeZoneNames;
begin
  if IsUTCName(ATimeZone) then
    Exit(True);
  Result := False;
  Names := LinuxTimeZoneNames;
  for Index := Low(Names) to High(Names) do
    if Names[Index] = ATimeZone then
      Exit(True);
end;

function ExtractZoneInfoName(const APath: string): string;
const
  Marker = '/zoneinfo/';
var
  MarkerPosition: Integer;
begin
  MarkerPosition := Pos(Marker, APath);
  if MarkerPosition = 0 then
    Exit('');
  Result := Copy(APath, MarkerPosition + Length(Marker), MaxInt);
end;

function LinuxSystemTimeZone: string;
var
  Buffer: array[0..4095] of Char;
  EnvironmentName, LinkTarget: string;
  LinkLength: TSsize;
  Lines: TStringList;
begin
  EnvironmentName := GetEnvironmentVariable('TZ');
  if (EnvironmentName <> '') and (EnvironmentName[1] = ':') then
    Delete(EnvironmentName, 1, 1);
  if Pos(ZoneInfoRoot, EnvironmentName) = 1 then
    EnvironmentName := Copy(EnvironmentName, Length(ZoneInfoRoot) + 1, MaxInt);
  if LinuxTimeZoneAvailable(EnvironmentName) then
    Exit(CKNormalizeTimeZoneName(EnvironmentName));

  if FileExists('/etc/timezone') then
  begin
    Lines := TStringList.Create;
    try
      Lines.LoadFromFile('/etc/timezone');
      if Lines.Count > 0 then
      begin
        EnvironmentName := Trim(Lines[0]);
        if LinuxTimeZoneAvailable(EnvironmentName) then
          Exit(CKNormalizeTimeZoneName(EnvironmentName));
      end;
    finally
      Lines.Free;
    end;
  end;

  LinkLength := fpReadLink('/etc/localtime', @Buffer[0], SizeOf(Buffer));
  if LinkLength > 0 then
  begin
    SetString(LinkTarget, PChar(@Buffer[0]), LinkLength);
    EnvironmentName := ExtractZoneInfoName(LinkTarget);
    if LinuxTimeZoneAvailable(EnvironmentName) then
      Exit(CKNormalizeTimeZoneName(EnvironmentName));
  end;

  raise EChronoKitTimeZoneEngine.Create(
    'System timezone identifier could not be determined');
end;

procedure LinuxConvertUTCWithTable(const AUTCValue: TDateTime;
  const ATimeZone: string; const ATable: TTZifTable;
  out ALocalValue: TDateTime; out AInfo: TChronoKitZoneInfo);
var
  IsDST: Boolean;
  OffsetSeconds: LongInt;
  UTCSeconds: Int64;
begin
  UTCSeconds := Round((AUTCValue - UnixEpoch) * CKSecondsPerDay);
  TZifInfoAtUTC(ATable, UTCSeconds, OffsetSeconds, IsDST);
  ALocalValue := AUTCValue +
    (OffsetSeconds / CKSecondsPerDay);
  AInfo.Name := ATimeZone;
  AInfo.Offset := Round(OffsetSeconds / 60);
  AInfo.IsDST := IsDST;
end;

procedure LinuxConvertUTCToLocal(const AUTCValue: TDateTime;
  const ATimeZone: string; out ALocalValue: TDateTime;
  out AInfo: TChronoKitZoneInfo);
var
  Table: TTZifTable;
  ZoneName: string;
begin
  ZoneName := CKNormalizeTimeZoneName(ATimeZone);
  if ZoneName = 'UTC' then
  begin
    ALocalValue := AUTCValue;
    SetUTCInfo(AInfo);
    Exit;
  end;
  if not LinuxTimeZoneAvailable(ZoneName) then
    raise EChronoKitTimeZoneEngine.CreateFmt(
      'Timezone "%s" not found', [ATimeZone]);
  LoadTZifTable(ZoneName, Table);
  LinuxConvertUTCWithTable(AUTCValue, ZoneName, Table,
    ALocalValue, AInfo);
end;

function LinuxResolveLocalTime(const ALocalValue: TDateTime;
  const ATimeZone: string; out AUTCValue: TDateTime;
  out AInfo: TChronoKitZoneInfo): TChronoKitLocalTimeStatus;
var
  CandidateInfo: TChronoKitZoneInfo;
  CandidateLocal, CandidateUTC: TDateTime;
  CandidateOffsets: array of LongInt;
  MatchCount, OffsetIndex, TypeIndex: Integer;
  Table: TTZifTable;
  ZoneName: string;

  procedure AddCandidateOffset(const AOffset: LongInt);
  var
    Index: Integer;
  begin
    for Index := 0 to High(CandidateOffsets) do
      if CandidateOffsets[Index] = AOffset then
        Exit;
    SetLength(CandidateOffsets, Length(CandidateOffsets) + 1);
    CandidateOffsets[High(CandidateOffsets)] := AOffset;
  end;
begin
  ZoneName := CKNormalizeTimeZoneName(ATimeZone);
  if ZoneName = 'UTC' then
  begin
    AUTCValue := ALocalValue;
    SetUTCInfo(AInfo);
    Exit(ckLocalTimeValid);
  end;
  if not LinuxTimeZoneAvailable(ZoneName) then
    raise EChronoKitTimeZoneEngine.CreateFmt(
      'Timezone "%s" not found', [ATimeZone]);

  LoadTZifTable(ZoneName, Table);
  CandidateOffsets := nil;
  for TypeIndex := 0 to High(Table.Types) do
    AddCandidateOffset(Table.Types[TypeIndex].OffsetSeconds);
  if Table.FutureRule.Valid then
  begin
    AddCandidateOffset(Table.FutureRule.StandardOffsetSeconds);
    if Table.FutureRule.HasDST then
      AddCandidateOffset(Table.FutureRule.DaylightOffsetSeconds);
  end;

  MatchCount := 0;
  for OffsetIndex := 0 to High(CandidateOffsets) do
  begin
    CandidateUTC := ALocalValue -
      (CandidateOffsets[OffsetIndex] / CKSecondsPerDay);
    LinuxConvertUTCWithTable(CandidateUTC, ZoneName, Table,
      CandidateLocal, CandidateInfo);
    if Abs(CandidateLocal - ALocalValue) <= CKOneMillisecond then
    begin
      if MatchCount = 0 then
      begin
        AUTCValue := CandidateUTC;
        AInfo := CandidateInfo;
      end;
      Inc(MatchCount);
    end;
  end;

  if MatchCount = 0 then
    Result := ckLocalTimeNonexistent
  else if MatchCount = 1 then
    Result := ckLocalTimeValid
  else
    Result := ckLocalTimeAmbiguous;
end;

{$ENDIF}

function CKGetSystemTimeZone: string;
begin
  {$IFDEF WINDOWS}
  Result := WindowsSystemTimeZone;
  {$ELSE}
  Result := LinuxSystemTimeZone;
  {$ENDIF}
end;

function CKGetTimeZoneNames: TChronoKitTimeZoneNames;
begin
  {$IFDEF WINDOWS}
  Result := WindowsTimeZoneNames;
  {$ELSE}
  Result := LinuxTimeZoneNames;
  {$ENDIF}
end;

function CKIsTimeZoneAvailable(const ATimeZone: string): Boolean;
{$IFDEF WINDOWS}
var
  Data: Windows.TTimeZoneInformation;
{$ENDIF}
begin
  if IsUTCName(ATimeZone) then
    Exit(True);
  {$IFDEF WINDOWS}
  Result := LoadWindowsTimeZone(ATimeZone, YearOf(Date), Data);
  {$ELSE}
  Result := LinuxTimeZoneAvailable(ATimeZone);
  {$ENDIF}
end;

function CKResolveLocalTime(const ALocalValue: TDateTime;
  const ATimeZone: string; out AUTCValue: TDateTime;
  out AInfo: TChronoKitZoneInfo): TChronoKitLocalTimeStatus;
begin
  {$IFDEF WINDOWS}
  Result := WindowsResolveLocalTime(ALocalValue, ATimeZone,
    AUTCValue, AInfo);
  {$ELSE}
  Result := LinuxResolveLocalTime(ALocalValue, ATimeZone,
    AUTCValue, AInfo);
  {$ENDIF}
end;

procedure CKConvertUTCToLocal(const AUTCValue: TDateTime;
  const ATimeZone: string; out ALocalValue: TDateTime;
  out AInfo: TChronoKitZoneInfo);
begin
  {$IFDEF WINDOWS}
  WindowsConvertUTCToLocal(AUTCValue, ATimeZone, ALocalValue, AInfo);
  {$ELSE}
  LinuxConvertUTCToLocal(AUTCValue, ATimeZone, ALocalValue, AInfo);
  {$ENDIF}
end;

end.
