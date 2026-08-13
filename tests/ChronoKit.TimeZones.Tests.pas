unit ChronoKit.TimeZones.Tests;

{$mode objfpc}{$H+}{$J-}
{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  Classes, SysUtils, DateUtils, fpcunit, testregistry,
  ChronoKit;

type
  TTimeZoneTests = class(TTestCase)
  private
    function FixtureName(const AVariable: string): string;
    function FixtureDateTime(const AVariable: string): TDateTime;
    function NamedWallClockToUTC(const AValue: TDateTime;
      const ATimeZone: string): TDateTime;
  published
    procedure Test101_TimeZoneInfoIsBounded;
    procedure Test102_SystemTimeZoneIsListed;
    procedure Test103_PortableUTCIsListed;
    procedure Test104_SameZoneConversionIsIdentity;
    procedure Test105_LocalToUTCUsesSourceOffset;
    procedure Test106_UTCInterpretationUsesSystemOffset;
    procedure Test107_UTCRoundTripPreservesClock;
    procedure Test108_UnsupportedTimeZonesRaise;
    procedure Test109_UTCOffsetBounds;
    procedure Test110_DSTStartMatrix;
    procedure Test111_DSTEndMatrix;
    procedure Test112_LeapYearDSTMatrix;
    procedure Test113_MalformedTimeZonesRaise;
    procedure Test114_UTCOffsetOutOfRangeRaises;
    procedure Test115_DateBoundaryConversion;
    procedure Test119_SeasonalOffsetMatrix;
    procedure Test120_NamedTargetConversion;
    procedure Test121_SouthernHemisphereSeasonalRules;
    procedure Test122_NamedNonexistentTimeRaises;
    procedure Test123_NamedAmbiguousTimeRaises;
    procedure Test124_SystemNonexistentTimeRaises;
    procedure Test125_SystemAmbiguousTimeRaises;
    procedure Test126_LogicalZoneFixturesAreDiscoverable;
    procedure Test127_TargetZoneUsesDateSpecificOffset;
    procedure Test128_FutureRecurringRules;
    procedure Test166_ExplicitTimezoneNamesPreserveSemantics;
    procedure Test169_ConvertBetweenTimeZonesPreservesInstant;
  end;

implementation

type
  TStringArray = array of string;

function TTimeZoneTests.FixtureName(const AVariable: string): string;
begin
  Result := SysUtils.GetEnvironmentVariable(AVariable);
  AssertTrue(AVariable + ' must name a platform-native timezone fixture',
    Result <> '');
end;

function TTimeZoneTests.FixtureDateTime(const AVariable: string): TDateTime;
var
  FixtureText: string;
begin
  FixtureText := SysUtils.GetEnvironmentVariable(AVariable);
  AssertTrue(AVariable + ' must contain a system-local fixture',
    FixtureText <> '');
  Result := ScanDateTime('yyyy-mm-dd hh:nn:ss', FixtureText);
end;

function TTimeZoneTests.NamedWallClockToUTC(const AValue: TDateTime;
  const ATimeZone: string): TDateTime;
var
  SystemLocal: TDateTime;
begin
  SystemLocal := TChronoKit.ForceTimeZone(AValue, ATimeZone);
  Result := TChronoKit.WithTimeZone(SystemLocal, 'UTC');
end;

procedure TTimeZoneTests.Test101_TimeZoneInfoIsBounded;
var
  TestDate: TDateTime;
  TZInfo: TTimeZoneInfo;
begin
  TestDate := EncodeDateTime(2024, 6, 1, 12, 0, 0, 0);
  TZInfo := TChronoKit.GetTimeZone(TestDate);

  AssertTrue('Timezone name must not be empty', TZInfo.Name <> '');
  AssertTrue('Timezone offset must be within the supported contract range',
    (TZInfo.Offset >= -12 * 60) and (TZInfo.Offset <= 14 * 60));
end;

procedure TTimeZoneTests.Test102_SystemTimeZoneIsListed;
var
  I: Integer;
  IsListed: Boolean;
  SystemTZ: string;
  TZNames: TStringArray;
begin
  SystemTZ := TChronoKit.GetSystemTimeZone;
  TZNames := TChronoKit.GetTimeZoneNames;
  IsListed := False;

  for I := Low(TZNames) to High(TZNames) do
    if TZNames[I] = SystemTZ then
      IsListed := True;

  AssertTrue('System timezone must not be empty', SystemTZ <> '');
  AssertTrue('System timezone must be returned by GetTimeZoneNames', IsListed);
end;

procedure TTimeZoneTests.Test103_PortableUTCIsListed;
var
  I: Integer;
  HasUTC: Boolean;
  TZNames: TStringArray;
begin
  TZNames := TChronoKit.GetTimeZoneNames;
  HasUTC := False;

  AssertTrue('Timezone list must not be empty', Length(TZNames) > 0);
  for I := Low(TZNames) to High(TZNames) do
  begin
    AssertTrue('Timezone identifiers must not be empty', TZNames[I] <> '');
    if TZNames[I] = 'UTC' then
      HasUTC := True;
  end;

  AssertTrue('UTC must be available on every platform', HasUTC);
end;

procedure TTimeZoneTests.Test104_SameZoneConversionIsIdentity;
var
  ConvertedDate, TestDate: TDateTime;
begin
  TestDate := EncodeDateTime(2024, 6, 1, 12, 0, 0, 0);
  ConvertedDate := TChronoKit.WithTimeZone(
    TestDate, TChronoKit.GetSystemTimeZone);

  AssertEquals('Converting to the system timezone must be an identity',
    TestDate, ConvertedDate, OneMillisecond);
end;

procedure TTimeZoneTests.Test105_LocalToUTCUsesSourceOffset;
var
  ExpectedUTC, LocalDate, UTCDate: TDateTime;
  SourceTZ: TTimeZoneInfo;
begin
  LocalDate := EncodeDateTime(2024, 6, 1, 12, 0, 0, 0);
  SourceTZ := TChronoKit.GetTimeZone(LocalDate);
  ExpectedUTC := LocalDate - (SourceTZ.Offset / MinutesPerDay);

  UTCDate := TChronoKit.WithTimeZone(LocalDate, 'UTC');

  AssertEquals('Local-to-UTC conversion must use the source offset',
    ExpectedUTC, UTCDate, OneMillisecond);
end;

procedure TTimeZoneTests.Test106_UTCInterpretationUsesSystemOffset;
var
  LocalDate, UTCDate: TDateTime;
  LocalTZ: TTimeZoneInfo;
begin
  UTCDate := EncodeDateTime(2024, 6, 1, 12, 0, 0, 0);
  LocalTZ := TChronoKit.GetTimeZone(UTCDate);
  LocalDate := TChronoKit.ForceTimeZone(UTCDate, 'UTC');

  AssertEquals('Interpreting UTC must apply the system offset',
    UTCDate + (LocalTZ.Offset / MinutesPerDay),
    LocalDate, OneMillisecond);
end;

procedure TTimeZoneTests.Test107_UTCRoundTripPreservesClock;
var
  LocalDate, RoundTrip, UTCDate: TDateTime;
begin
  LocalDate := EncodeDateTime(2024, 6, 1, 12, 0, 0, 0);
  UTCDate := TChronoKit.WithTimeZone(LocalDate, 'UTC');
  RoundTrip := TChronoKit.ForceTimeZone(UTCDate, 'UTC');

  AssertEquals('Local-to-UTC round trip must preserve the local clock',
    LocalDate, RoundTrip, OneMillisecond);
end;

procedure TTimeZoneTests.Test108_UnsupportedTimeZonesRaise;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDateTime(2024, 6, 1, 12, 0, 0, 0);

  try
    TChronoKit.WithTimeZone(TestDate, '');
    Fail('Empty timezone must raise ETimeZoneError');
  except
    on E: ETimeZoneError do
      AssertTrue('Empty timezone diagnostic must identify lookup failure',
        Pos('not found', E.Message) > 0);
  end;

  try
    TChronoKit.WithTimeZone(TestDate, 'Invalid/Timezone');
    Fail('Unsupported timezone must raise ETimeZoneError');
  except
    on E: ETimeZoneError do
      AssertTrue('Unsupported timezone diagnostic must include the identifier',
        Pos('Invalid/Timezone', E.Message) > 0);
  end;
end;

procedure TTimeZoneTests.Test109_UTCOffsetBounds;
begin
  AssertEquals('Minimum UTC offset must be accepted',
    -12 * 60, TChronoKit.ValidateTimeZoneOffset(-12 * 60));
  AssertEquals('Maximum UTC offset must be accepted',
    14 * 60, TChronoKit.ValidateTimeZoneOffset(14 * 60));
end;

procedure TTimeZoneTests.Test110_DSTStartMatrix;
var
  AfterTransitionUTC, BeforeTransitionUTC: TDateTime;
  NewYork: string;
begin
  NewYork := FixtureName('CHRONOKIT_TEST_NEW_YORK');
  BeforeTransitionUTC := NamedWallClockToUTC(
    EncodeDateTime(2024, 3, 10, 1, 59, 59, 0), NewYork);
  AfterTransitionUTC := NamedWallClockToUTC(
    EncodeDateTime(2024, 3, 10, 3, 0, 0, 0), NewYork);

  AssertEquals('Last valid clock second before DST start uses UTC-05:00',
    EncodeDateTime(2024, 3, 10, 6, 59, 59, 0),
    BeforeTransitionUTC, OneMillisecond);
  AssertEquals('First valid clock second after DST start uses UTC-04:00',
    EncodeDateTime(2024, 3, 10, 7, 0, 0, 0),
    AfterTransitionUTC, OneMillisecond);
end;

procedure TTimeZoneTests.Test111_DSTEndMatrix;
var
  AfterTransitionUTC, BeforeTransitionUTC: TDateTime;
  NewYork: string;
begin
  NewYork := FixtureName('CHRONOKIT_TEST_NEW_YORK');
  BeforeTransitionUTC := NamedWallClockToUTC(
    EncodeDateTime(2024, 11, 3, 0, 59, 59, 0), NewYork);
  AfterTransitionUTC := NamedWallClockToUTC(
    EncodeDateTime(2024, 11, 3, 2, 0, 0, 0), NewYork);

  AssertEquals('Last unambiguous clock hour before DST end uses UTC-04:00',
    EncodeDateTime(2024, 11, 3, 4, 59, 59, 0),
    BeforeTransitionUTC, OneMillisecond);
  AssertEquals('First valid clock second after DST end uses UTC-05:00',
    EncodeDateTime(2024, 11, 3, 7, 0, 0, 0),
    AfterTransitionUTC, OneMillisecond);
end;

procedure TTimeZoneTests.Test112_LeapYearDSTMatrix;
var
  LeapDayUTC, PostTransitionUTC: TDateTime;
  NewYork: string;
begin
  NewYork := FixtureName('CHRONOKIT_TEST_NEW_YORK');
  LeapDayUTC := NamedWallClockToUTC(
    EncodeDateTime(2024, 2, 29, 23, 59, 59, 0), NewYork);
  PostTransitionUTC := NamedWallClockToUTC(
    EncodeDateTime(2024, 3, 10, 3, 0, 0, 0), NewYork);

  AssertEquals('Leap day uses New York standard time',
    EncodeDateTime(2024, 3, 1, 4, 59, 59, 0),
    LeapDayUTC, OneMillisecond);
  AssertEquals('DST transition remains correct in a leap year',
    EncodeDateTime(2024, 3, 10, 7, 0, 0, 0),
    PostTransitionUTC, OneMillisecond);
end;

procedure TTimeZoneTests.Test113_MalformedTimeZonesRaise;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDateTime(2024, 6, 1, 12, 0, 0, 0);

  try
    TChronoKit.ForceTimeZone(TestDate, 'UTC+24:00');
    Fail('Malformed positive offset name must raise ETimeZoneError');
  except
    on E: ETimeZoneError do
      AssertTrue('Malformed timezone diagnostic must include the identifier',
        Pos('UTC+24:00', E.Message) > 0);
  end;

  try
    TChronoKit.ForceTimeZone(TestDate, 'UTC-24:00');
    Fail('Malformed negative offset name must raise ETimeZoneError');
  except
    on E: ETimeZoneError do
      AssertTrue('Malformed timezone diagnostic must include the identifier',
        Pos('UTC-24:00', E.Message) > 0);
  end;
end;

procedure TTimeZoneTests.Test114_UTCOffsetOutOfRangeRaises;
begin
  try
    TChronoKit.ValidateTimeZoneOffset(-12 * 60 - 1);
    Fail('Offset below -12:00 must raise ETimeZoneError');
  except
    on E: ETimeZoneError do
      AssertTrue('Lower-bound diagnostic must include the valid range',
        Pos('-720', E.Message) > 0);
  end;

  try
    TChronoKit.ValidateTimeZoneOffset(14 * 60 + 1);
    Fail('Offset above +14:00 must raise ETimeZoneError');
  except
    on E: ETimeZoneError do
      AssertTrue('Upper-bound diagnostic must include the valid range',
        Pos('+840', E.Message) > 0);
  end;
end;

procedure TTimeZoneTests.Test115_DateBoundaryConversion;
var
  ExpectedUTC, LocalDate, UTCDate: TDateTime;
  SourceTZ: TTimeZoneInfo;
begin
  LocalDate := EncodeDateTime(2024, 1, 1, 12, 0, 0, 0);
  SourceTZ := TChronoKit.GetTimeZone(LocalDate);
  if SourceTZ.Offset > 0 then
    LocalDate := EncodeDateTime(2024, 1, 1, 1, 0, 0, 0)
  else if SourceTZ.Offset < 0 then
    LocalDate := EncodeDateTime(2024, 1, 1, 23, 0, 0, 0);
  SourceTZ := TChronoKit.GetTimeZone(LocalDate);
  ExpectedUTC := LocalDate - (SourceTZ.Offset / MinutesPerDay);
  UTCDate := TChronoKit.WithTimeZone(LocalDate, 'UTC');

  AssertEquals('Date-boundary conversion must apply the exact source offset',
    ExpectedUTC, UTCDate, OneMillisecond);
  if SourceTZ.Offset > 60 then
    AssertEquals('Positive offsets can cross into the previous UTC date',
      Trunc(LocalDate) - 1, Trunc(UTCDate))
  else if SourceTZ.Offset < -60 then
    AssertEquals('Negative offsets can cross into the next UTC date',
      Trunc(LocalDate) + 1, Trunc(UTCDate));
end;

procedure TTimeZoneTests.Test119_SeasonalOffsetMatrix;
var
  SummerUTC, WinterUTC: TDateTime;
  NewYork: string;
begin
  NewYork := FixtureName('CHRONOKIT_TEST_NEW_YORK');
  WinterUTC := NamedWallClockToUTC(
    EncodeDateTime(2024, 1, 15, 12, 0, 0, 0), NewYork);
  SummerUTC := NamedWallClockToUTC(
    EncodeDateTime(2024, 7, 15, 12, 0, 0, 0), NewYork);

  AssertEquals('New York winter wall clock must use UTC-05:00',
    EncodeDateTime(2024, 1, 15, 17, 0, 0, 0),
    WinterUTC, OneMillisecond);
  AssertEquals('New York summer wall clock must use UTC-04:00',
    EncodeDateTime(2024, 7, 15, 16, 0, 0, 0),
    SummerUTC, OneMillisecond);
end;

procedure TTimeZoneTests.Test120_NamedTargetConversion;
var
  LocalValue, TokyoValue, UTCValue: TDateTime;
  Tokyo: string;
begin
  Tokyo := FixtureName('CHRONOKIT_TEST_TOKYO');
  LocalValue := EncodeDateTime(2024, 1, 15, 12, 0, 0, 0);
  UTCValue := TChronoKit.WithTimeZone(LocalValue, 'UTC');
  TokyoValue := TChronoKit.WithTimeZone(LocalValue, Tokyo);

  AssertEquals('Named target conversion must apply Tokyo UTC+09:00',
    UTCValue + EncodeTime(9, 0, 0, 0), TokyoValue, OneMillisecond);
end;

procedure TTimeZoneTests.Test121_SouthernHemisphereSeasonalRules;
var
  SummerUTC, WinterUTC: TDateTime;
  Sydney: string;
begin
  Sydney := FixtureName('CHRONOKIT_TEST_SYDNEY');
  SummerUTC := NamedWallClockToUTC(
    EncodeDateTime(2024, 1, 15, 12, 0, 0, 0), Sydney);
  WinterUTC := NamedWallClockToUTC(
    EncodeDateTime(2024, 7, 15, 12, 0, 0, 0), Sydney);

  AssertEquals('Sydney summer wall clock must use UTC+11:00',
    EncodeDateTime(2024, 1, 15, 1, 0, 0, 0),
    SummerUTC, OneMillisecond);
  AssertEquals('Sydney winter wall clock must use UTC+10:00',
    EncodeDateTime(2024, 7, 15, 2, 0, 0, 0),
    WinterUTC, OneMillisecond);
end;

procedure TTimeZoneTests.Test122_NamedNonexistentTimeRaises;
var
  NewYork: string;
  RejectedValue: TDateTime;
begin
  NewYork := FixtureName('CHRONOKIT_TEST_NEW_YORK');
  RejectedValue := EncodeDateTime(2024, 3, 10, 2, 30, 0, 0);

  try
    TChronoKit.ForceTimeZone(RejectedValue, NewYork);
    Fail('A named-zone DST gap must raise ETimeZoneError');
  except
    on E: ETimeZoneError do
    begin
      AssertTrue('Gap diagnostic must classify the local value as nonexistent',
        Pos('nonexistent', LowerCase(E.Message)) > 0);
      AssertTrue('Gap diagnostic must identify the requested timezone',
        Pos(NewYork, E.Message) > 0);
      AssertTrue('Gap diagnostic must identify the rejected wall clock',
        Pos('2024-03-10 02:30:00', E.Message) > 0);
    end;
  end;
end;

procedure TTimeZoneTests.Test123_NamedAmbiguousTimeRaises;
var
  NewYork: string;
  RejectedValue: TDateTime;
begin
  NewYork := FixtureName('CHRONOKIT_TEST_NEW_YORK');
  RejectedValue := EncodeDateTime(2024, 11, 3, 1, 30, 0, 0);

  try
    TChronoKit.ForceTimeZone(RejectedValue, NewYork);
    Fail('A named-zone DST overlap must raise ETimeZoneError');
  except
    on E: ETimeZoneError do
    begin
      AssertTrue('Overlap diagnostic must classify the local value as ambiguous',
        Pos('ambiguous', LowerCase(E.Message)) > 0);
      AssertTrue('Overlap diagnostic must identify the requested timezone',
        Pos(NewYork, E.Message) > 0);
      AssertTrue('Overlap diagnostic must identify the rejected wall clock',
        Pos('2024-11-03 01:30:00', E.Message) > 0);
    end;
  end;
end;

procedure TTimeZoneTests.Test124_SystemNonexistentTimeRaises;
var
  RejectedValue: TDateTime;
  SystemTimeZone: string;
begin
  RejectedValue := FixtureDateTime('CHRONOKIT_TEST_SYSTEM_GAP');
  SystemTimeZone := TChronoKit.GetSystemTimeZone;

  try
    TChronoKit.GetTimeZone(RejectedValue);
    Fail('A system-zone DST gap must raise ETimeZoneError');
  except
    on E: ETimeZoneError do
    begin
      AssertTrue('System gap diagnostic must classify the value as nonexistent',
        Pos('nonexistent', LowerCase(E.Message)) > 0);
      AssertTrue('System gap diagnostic must identify the system timezone',
        Pos(SystemTimeZone, E.Message) > 0);
    end;
  end;
end;

procedure TTimeZoneTests.Test125_SystemAmbiguousTimeRaises;
var
  RejectedValue: TDateTime;
  SystemTimeZone: string;
begin
  RejectedValue := FixtureDateTime('CHRONOKIT_TEST_SYSTEM_OVERLAP');
  SystemTimeZone := TChronoKit.GetSystemTimeZone;

  try
    TChronoKit.WithTimeZone(RejectedValue, 'UTC');
    Fail('A system-zone DST overlap must raise ETimeZoneError');
  except
    on E: ETimeZoneError do
    begin
      AssertTrue('System overlap diagnostic must classify the value as ambiguous',
        Pos('ambiguous', LowerCase(E.Message)) > 0);
      AssertTrue('System overlap diagnostic must identify the system timezone',
        Pos(SystemTimeZone, E.Message) > 0);
    end;
  end;
end;

procedure TTimeZoneTests.Test126_LogicalZoneFixturesAreDiscoverable;
var
  FixtureIndex, NameIndex: Integer;
  FixtureFound: Boolean;
  FixtureNames: array[0..4] of string;
  TimeZoneNames: TStringArray;
begin
  FixtureNames[0] := FixtureName('CHRONOKIT_TEST_NEW_YORK');
  FixtureNames[1] := FixtureName('CHRONOKIT_TEST_LONDON');
  FixtureNames[2] := FixtureName('CHRONOKIT_TEST_SYDNEY');
  FixtureNames[3] := FixtureName('CHRONOKIT_TEST_TOKYO');
  FixtureNames[4] := FixtureName('CHRONOKIT_TEST_AUCKLAND');
  TimeZoneNames := TChronoKit.GetTimeZoneNames;

  for FixtureIndex := Low(FixtureNames) to High(FixtureNames) do
  begin
    FixtureFound := False;
    for NameIndex := Low(TimeZoneNames) to High(TimeZoneNames) do
      if TimeZoneNames[NameIndex] = FixtureNames[FixtureIndex] then
        FixtureFound := True;
    AssertTrue('Logical fixture must be returned by GetTimeZoneNames: ' +
      FixtureNames[FixtureIndex], FixtureFound);
  end;
end;

procedure TTimeZoneTests.Test127_TargetZoneUsesDateSpecificOffset;
var
  LocalSummer, LocalWinter, SydneySummer, SydneyWinter: TDateTime;
  Sydney: string;
  UTCSummer, UTCWinter: TDateTime;
begin
  Sydney := FixtureName('CHRONOKIT_TEST_SYDNEY');
  LocalSummer := EncodeDateTime(2024, 1, 15, 12, 0, 0, 0);
  LocalWinter := EncodeDateTime(2024, 7, 15, 12, 0, 0, 0);
  UTCSummer := TChronoKit.WithTimeZone(LocalSummer, 'UTC');
  UTCWinter := TChronoKit.WithTimeZone(LocalWinter, 'UTC');

  SydneySummer := TChronoKit.WithTimeZone(LocalSummer, Sydney);
  SydneyWinter := TChronoKit.WithTimeZone(LocalWinter, Sydney);

  AssertEquals('Sydney target conversion must use summer UTC+11:00',
    UTCSummer + EncodeTime(11, 0, 0, 0), SydneySummer, OneMillisecond);
  AssertEquals('Sydney target conversion must use winter UTC+10:00',
    UTCWinter + EncodeTime(10, 0, 0, 0), SydneyWinter, OneMillisecond);
end;

procedure TTimeZoneTests.Test128_FutureRecurringRules;
var
  NewYork, Sydney: string;
  NewYorkSummerUTC, NewYorkWinterUTC: TDateTime;
  RejectedValue: TDateTime;
  SydneySummerUTC, SydneyWinterUTC: TDateTime;
begin
  NewYork := FixtureName('CHRONOKIT_TEST_NEW_YORK');
  Sydney := FixtureName('CHRONOKIT_TEST_SYDNEY');
  NewYorkWinterUTC := NamedWallClockToUTC(
    EncodeDateTime(2050, 1, 15, 12, 0, 0, 0), NewYork);
  NewYorkSummerUTC := NamedWallClockToUTC(
    EncodeDateTime(2050, 7, 15, 12, 0, 0, 0), NewYork);
  SydneySummerUTC := NamedWallClockToUTC(
    EncodeDateTime(2050, 1, 15, 12, 0, 0, 0), Sydney);
  SydneyWinterUTC := NamedWallClockToUTC(
    EncodeDateTime(2050, 7, 15, 12, 0, 0, 0), Sydney);

  AssertEquals('Future New York winter must use recurring UTC-05:00 rules',
    EncodeDateTime(2050, 1, 15, 17, 0, 0, 0),
    NewYorkWinterUTC, OneMillisecond);
  AssertEquals('Future New York summer must use recurring UTC-04:00 rules',
    EncodeDateTime(2050, 7, 15, 16, 0, 0, 0),
    NewYorkSummerUTC, OneMillisecond);
  AssertEquals('Future Sydney summer must use recurring UTC+11:00 rules',
    EncodeDateTime(2050, 1, 15, 1, 0, 0, 0),
    SydneySummerUTC, OneMillisecond);
  AssertEquals('Future Sydney winter must use recurring UTC+10:00 rules',
    EncodeDateTime(2050, 7, 15, 2, 0, 0, 0),
    SydneyWinterUTC, OneMillisecond);

  RejectedValue := EncodeDateTime(2050, 3, 13, 2, 30, 0, 0);
  try
    TChronoKit.ForceTimeZone(RejectedValue, NewYork);
    Fail('A future recurring-rule DST gap must raise ETimeZoneError');
  except
    on E: ETimeZoneError do
      AssertTrue('Future recurring-rule gap must be classified as nonexistent',
        Pos('nonexistent', LowerCase(E.Message)) > 0);
  end;

  RejectedValue := EncodeDateTime(2050, 11, 6, 1, 30, 0, 0);
  try
    TChronoKit.ForceTimeZone(RejectedValue, NewYork);
    Fail('A future recurring-rule DST overlap must raise ETimeZoneError');
  except
    on E: ETimeZoneError do
      AssertTrue('Future recurring-rule overlap must be classified as ambiguous',
        Pos('ambiguous', LowerCase(E.Message)) > 0);
  end;
end;

procedure TTimeZoneTests.Test166_ExplicitTimezoneNamesPreserveSemantics;
var
  LocalValue, UTCValue, LegacyUTC, ExplicitLocal, LegacyLocal: TDateTime;
  ExplicitInfo, LegacyInfo: TTimeZoneInfo;
begin
  LocalValue := EncodeDateTime(2024, 7, 15, 12, 0, 0, 0);
  UTCValue := TChronoKit.SystemLocalToTimeZone(LocalValue, 'UTC');
  LegacyUTC := TChronoKit.WithTimeZone(LocalValue, 'UTC');
  AssertEquals('Explicit local-to-target name preserves conversion',
    LegacyUTC, UTCValue, OneMillisecond);

  ExplicitLocal := TChronoKit.TimeZoneToSystemLocal(UTCValue, 'UTC');
  LegacyLocal := TChronoKit.ForceTimeZone(UTCValue, 'UTC');
  AssertEquals('Explicit source-to-local name preserves conversion',
    LegacyLocal, ExplicitLocal, OneMillisecond);

  ExplicitInfo := TChronoKit.GetSystemTimeZoneInfo(LocalValue);
  LegacyInfo := TChronoKit.GetTimeZone(LocalValue);
  AssertEquals('Explicit timezone info preserves name',
    LegacyInfo.Name, ExplicitInfo.Name);
  AssertEquals('Explicit timezone info preserves offset',
    LegacyInfo.Offset, ExplicitInfo.Offset);
  AssertEquals('Explicit timezone info preserves DST state',
    LegacyInfo.IsDST, ExplicitInfo.IsDST);
end;

procedure TTimeZoneTests.Test169_ConvertBetweenTimeZonesPreservesInstant;
var
  Converted, SourceValue: TDateTime;
  NewYork, London: string;
  Raised: Boolean;
begin
  NewYork := FixtureName('CHRONOKIT_TEST_NEW_YORK');
  London := FixtureName('CHRONOKIT_TEST_LONDON');
  SourceValue := EncodeDateTime(2024, 1, 15, 8, 30, 0, 0);
  Converted := TChronoKit.ConvertBetweenTimeZones(SourceValue, NewYork,
    London);
  AssertEquals('Named conversion represents the same instant in the target',
    EncodeDateTime(2024, 1, 15, 13, 30, 0, 0), Converted, OneMillisecond);
  AssertEquals('A named zone is an identity target for the same source',
    SourceValue, TChronoKit.ConvertBetweenTimeZones(SourceValue, NewYork,
      NewYork), OneMillisecond);

  Raised := False;
  try
    TChronoKit.ConvertBetweenTimeZones(
      EncodeDateTime(2024, 3, 10, 2, 30, 0, 0), NewYork, 'UTC');
  except
    on E: ETimeZoneError do
      Raised := Pos('nonexistent', LowerCase(E.Message)) > 0;
  end;
  AssertTrue('A nonexistent source wall clock must be rejected', Raised);

  Raised := False;
  try
    TChronoKit.ConvertBetweenTimeZones(
      EncodeDateTime(2024, 11, 3, 1, 30, 0, 0), NewYork, 'UTC');
  except
    on E: ETimeZoneError do
      Raised := Pos('ambiguous', LowerCase(E.Message)) > 0;
  end;
  AssertTrue('An ambiguous source wall clock must be rejected', Raised);
end;

initialization
  RegisterTest(TTimeZoneTests);

end.
