unit ChronoKitLegacy;

{$mode objfpc}{$H+}{$J-}

interface

uses
  ChronoKitInternalTypes;

function CKLegacyCreatePeriod(const AYears, AMonths, ADays, AHours,
  AMinutes, ASeconds, AMilliseconds: Integer): TCKLegacySpan;
function CKLegacyCreateDuration(const AYears, AMonths, ADays, AHours,
  AMinutes, ASeconds, AMilliseconds: Integer): TCKLegacySpan;
function CKLegacyCreateInterval(const AStart, AEnd: TDateTime): TCKLegacyInterval;
function CKLegacyAddSpan(const AValue: TDateTime;
  const ASpan: TCKLegacySpan): TDateTime;
function CKLegacySubtractSpan(const AValue: TDateTime;
  const ASpan: TCKLegacySpan): TDateTime;
function CKLegacySpanBetween(const AStart, AEnd: TDateTime;
  const AKind: TCKLegacySpanKind): TCKLegacySpan;
function CKLegacyIsWithinInterval(const AValue: TDateTime;
  const AInterval: TCKLegacyInterval): Boolean;
function CKLegacyIntervalsOverlap(const AFirst,
  ASecond: TCKLegacyInterval): Boolean;
function CKLegacyIntervalLength(const AInterval: TCKLegacyInterval;
  const AKind: TCKLegacySpanKind): TCKLegacySpan;
function CKLegacyYMD(const AValue: string): TDateTime;
function CKLegacyMDY(const AValue: string): TDateTime;
function CKLegacyDMY(const AValue: string): TDateTime;
function CKLegacyYQ(const AValue: string): TDateTime;
function CKLegacyGetEpiYear(const AValue: TDateTime): Integer;
function CKLegacyGetEpiWeek(const AValue: TDateTime): Integer;
function CKLegacyPeriodToSeconds(const APeriod: TCKLegacySpan): Int64;
function CKLegacySecondsToPeriod(const ASeconds: Int64): TCKLegacySpan;
function CKLegacyStandardizePeriod(
  const AValue: TCKLegacySpan): TCKLegacySpan;
function CKLegacyIntervalAlign(const AFirst,
  ASecond: TCKLegacyInterval): Boolean;
function CKLegacyIntervalGap(const AFirst,
  ASecond: TCKLegacyInterval): TCKLegacySpan;
function CKLegacyIntervalSetdiff(const AFirst,
  ASecond: TCKLegacyInterval): TCKLegacyInterval;
function CKLegacyIntervalUnion(const AFirst,
  ASecond: TCKLegacyInterval): TCKLegacyInterval;
function CKLegacyIntervalIntersection(const AFirst,
  ASecond: TCKLegacyInterval): TCKLegacyInterval;

implementation

uses
  SysUtils, DateUtils, StrUtils, Types, ChronoKitCalendar;

const
  LegacySecondsPerMinute = 60;
  LegacyMinutesPerHour = 60;
  LegacyHoursPerDay = 24;
  LegacySecondsPerHour = 3600;
  LegacySecondsPerDay = 86400;
  LegacySecondsPerMonth = 2592000;
  LegacySecondsPerYear = 31536000;
  LegacyMillisecondsPerSecond = 1000;
  LegacyMonthsPerYear = 12;

function CKLegacyCreatePeriod(const AYears, AMonths, ADays, AHours,
  AMinutes, ASeconds, AMilliseconds: Integer): TCKLegacySpan;
begin
  Result.Kind := cklskPeriod;
  Result.Years := AYears;
  Result.Months := AMonths;
  Result.Days := ADays;
  Result.Hours := AHours;
  Result.Minutes := AMinutes;
  Result.Seconds := ASeconds;
  Result.Milliseconds := AMilliseconds;
end;

function CKLegacyCreateDuration(const AYears, AMonths, ADays, AHours,
  AMinutes, ASeconds, AMilliseconds: Integer): TCKLegacySpan;
begin
  Result.Kind := cklskDuration;
  Result.Years := 0;
  Result.Months := 0;
  Result.Days := 0;
  Result.Hours := 0;
  Result.Minutes := 0;
  Result.Seconds := ASeconds + AMinutes * LegacySecondsPerMinute +
    AHours * LegacySecondsPerHour + ADays * LegacySecondsPerDay +
    AMonths * LegacySecondsPerMonth + AYears * LegacySecondsPerYear;
  Result.Milliseconds := AMilliseconds;
end;

function CKLegacyCreateInterval(const AStart, AEnd: TDateTime): TCKLegacyInterval;
begin
  if CompareDateTime(AStart, AEnd) > 0 then
    raise EArgumentException.Create(
      'Interval start must not be later than interval end');
  Result.StartDate := AStart;
  Result.EndDate := AEnd;
end;

function CKLegacyAddSpan(const AValue: TDateTime;
  const ASpan: TCKLegacySpan): TDateTime;
begin
  Result := AValue;
  case ASpan.Kind of
    cklskPeriod:
      begin
        Result := AValue;
        if ASpan.Years <> 0 then Result := IncYear(Result, ASpan.Years);
        if ASpan.Months <> 0 then Result := IncMonth(Result, ASpan.Months);
        if ASpan.Days <> 0 then Result := IncDay(Result, ASpan.Days);
        if (ASpan.Hours <> 0) or (ASpan.Minutes <> 0) or
           (ASpan.Seconds <> 0) or (ASpan.Milliseconds <> 0) then
          Result := Result + EncodeTime(ASpan.Hours, ASpan.Minutes,
            ASpan.Seconds, ASpan.Milliseconds);
      end;
    cklskDuration:
      Result := AValue + ASpan.Seconds / SecsPerDay +
        ASpan.Milliseconds / (SecsPerDay * LegacyMillisecondsPerSecond);
  end;
end;

function CKLegacySubtractSpan(const AValue: TDateTime;
  const ASpan: TCKLegacySpan): TDateTime;
var
  NegativeSpan: TCKLegacySpan;
begin
  NegativeSpan := ASpan;
  NegativeSpan.Years := -ASpan.Years;
  NegativeSpan.Months := -ASpan.Months;
  NegativeSpan.Days := -ASpan.Days;
  NegativeSpan.Hours := -ASpan.Hours;
  NegativeSpan.Minutes := -ASpan.Minutes;
  NegativeSpan.Seconds := -ASpan.Seconds;
  NegativeSpan.Milliseconds := -ASpan.Milliseconds;
  Result := CKLegacyAddSpan(AValue, NegativeSpan);
end;

function CKLegacySpanBetween(const AStart, AEnd: TDateTime;
  const AKind: TCKLegacySpanKind): TCKLegacySpan;
var
  Y1, M1, D1, Y2, M2, D2: Word;
  H1, N1, S1, MS1, H2, N2, S2, MS2: Word;
  TempDate: TDateTime;
  TotalMilliseconds: Int64;
begin
  FillChar(Result, SizeOf(Result), 0);
  case AKind of
    cklskPeriod:
      begin
        DecodeDate(AStart, Y1, M1, D1);
        DecodeTime(AStart, H1, N1, S1, MS1);
        DecodeDate(AEnd, Y2, M2, D2);
        DecodeTime(AEnd, H2, N2, S2, MS2);
        TempDate := IncYear(AStart, Y2 - Y1);
        if CompareDateTime(TempDate, AEnd) = 0 then
          Exit(CKLegacyCreatePeriod(Y2 - Y1, 0, 0, 0, 0, 0, 0));
        TempDate := IncMonth(AStart, (Y2 - Y1) * 12 + (M2 - M1));
        if CompareDateTime(TempDate, AEnd) = 0 then
          Exit(CKLegacyCreatePeriod(Y2 - Y1, M2 - M1, 0, 0, 0, 0, 0));
        Result := CKLegacyCreatePeriod(Y2 - Y1, M2 - M1, D2 - D1,
          H2 - H1, N2 - N1, S2 - S1, MS2 - MS1);
        if Result.Milliseconds < 0 then begin Dec(Result.Seconds); Inc(Result.Milliseconds, 1000); end;
        if Result.Seconds < 0 then begin Dec(Result.Minutes); Inc(Result.Seconds, 60); end;
        if Result.Minutes < 0 then begin Dec(Result.Hours); Inc(Result.Minutes, 60); end;
        if Result.Hours < 0 then begin Dec(Result.Days); Inc(Result.Hours, 24); end;
        if Result.Days < 0 then begin Dec(Result.Months); Inc(Result.Days, DaysInMonth(EncodeDate(Y1, M1, 1))); end;
        if Result.Months < 0 then begin Dec(Result.Years); Inc(Result.Months, 12); end;
      end;
    cklskDuration:
      begin
        TotalMilliseconds := Round((AEnd - AStart) * SecsPerDay *
          LegacyMillisecondsPerSecond);
        Result := CKLegacyCreateDuration(0, 0, 0, 0, 0,
          TotalMilliseconds div LegacyMillisecondsPerSecond,
          TotalMilliseconds mod LegacyMillisecondsPerSecond);
      end;
  end;
end;

function CKLegacyIsWithinInterval(const AValue: TDateTime;
  const AInterval: TCKLegacyInterval): Boolean;
begin
  Result := (AValue >= AInterval.StartDate) and (AValue <= AInterval.EndDate);
end;

function CKLegacyIntervalsOverlap(const AFirst,
  ASecond: TCKLegacyInterval): Boolean;
begin
  Result := (AFirst.StartDate <= ASecond.EndDate) and
    (AFirst.EndDate >= ASecond.StartDate);
end;

function CKLegacyIntervalLength(const AInterval: TCKLegacyInterval;
  const AKind: TCKLegacySpanKind): TCKLegacySpan;
begin
  Result := CKLegacySpanBetween(AInterval.StartDate, AInterval.EndDate, AKind);
end;

function TryLegacyDate(const AValue, AFormat: string;
  out AResult: TDateTime): Boolean;
var
  Settings: TFormatSettings;
begin
  Settings := DefaultFormatSettings;
  Settings.DateSeparator := '-';
  Settings.ShortDateFormat := AFormat;
  Result := TryStrToDate(AValue, AResult, Settings);
  if not Result then
  begin
    Settings.DateSeparator := '/';
    Result := TryStrToDate(AValue, AResult, Settings);
  end;
end;

function CKLegacyYMD(const AValue: string): TDateTime;
begin
  if not TryLegacyDate(AValue, 'yyyy/mm/dd', Result) then
    raise EConvertError.CreateFmt(
      'Invalid YMD date "%s". Expected YYYY-MM-DD or YYYY/MM/DD with a valid calendar date',
      [AValue]);
end;

function CKLegacyMDY(const AValue: string): TDateTime;
begin
  if not TryLegacyDate(AValue, 'mm/dd/yyyy', Result) then
    raise EConvertError.CreateFmt(
      'Invalid MDY date "%s". Expected MM-DD-YYYY or MM/DD/YYYY with a ' +
      'valid calendar date; two-digit years are also accepted', [AValue]);
end;

function CKLegacyDMY(const AValue: string): TDateTime;
begin
  if not TryLegacyDate(AValue, 'dd/mm/yyyy', Result) then
    raise EConvertError.CreateFmt(
      'Invalid DMY date "%s". Expected DD-MM-YYYY or DD/MM/YYYY with a ' +
      'valid calendar date; two-digit years are also accepted', [AValue]);
end;

function CKLegacyYQ(const AValue: string): TDateTime;
var
  Year, Quarter: Integer;
  Parts: TStringDynArray;
begin
  Parts := SplitString(AValue, '-');
  if Length(Parts) <> 2 then
  begin
    Parts := SplitString(AValue, '/');
    if Length(Parts) <> 2 then
      raise EConvertError.CreateFmt(
        'Invalid YQ value "%s". Expected YYYY-Q or YYYY/Q', [AValue]);
  end;
  if not TryStrToInt(Parts[0], Year) or not TryStrToInt(Parts[1], Quarter) then
    raise EConvertError.CreateFmt(
      'Invalid YQ value "%s". Year and quarter must be numbers', [AValue]);
  if (Year < 1) or (Year > 9999) then
    raise EConvertError.CreateFmt(
      'Invalid YQ value "%s". Year must be between 1 and 9999', [AValue]);
  if (Quarter < 1) or (Quarter > 4) then
    raise EConvertError.CreateFmt(
      'Invalid YQ value "%s". Quarter must be between 1 and 4', [AValue]);
  Result := CKStartOfQuarter(Year, Quarter);
end;

function CKLegacyGetEpiYear(const AValue: TDateTime): Integer;
var
  Y, M, D: Word;
  Dec28, ThisWeekMon, LastWeekMon: TDateTime;
begin
  DecodeDate(AValue, Y, M, D);
  if M = 1 then
  begin
    Dec28 := EncodeDate(Y - 1, 12, 28);
    ThisWeekMon := Trunc(AValue) - ((DayOfTheWeek(AValue) + 5) mod 7);
    LastWeekMon := Trunc(Dec28) - ((DayOfTheWeek(Dec28) + 5) mod 7);
    if ThisWeekMon = LastWeekMon then Result := Y - 1 else Result := Y;
  end
  else if M = 12 then
  begin
    Dec28 := EncodeDate(Y, 12, 28);
    ThisWeekMon := Trunc(AValue) - ((DayOfTheWeek(AValue) + 5) mod 7);
    LastWeekMon := Trunc(Dec28) - ((DayOfTheWeek(Dec28) + 5) mod 7);
    if ThisWeekMon >= LastWeekMon then Result := Y + 1 else Result := Y;
  end
  else
    Result := Y;
end;

function CKLegacyGetEpiWeek(const AValue: TDateTime): Integer;
var
  Year: Integer;
  Jan4, ThisWeekMon, FirstWeekMon: TDateTime;
begin
  Year := CKLegacyGetEpiYear(AValue);
  Jan4 := EncodeDate(Year, 1, 4);
  ThisWeekMon := Trunc(AValue) - ((DayOfTheWeek(AValue) + 5) mod 7);
  FirstWeekMon := Trunc(Jan4) - ((DayOfTheWeek(Jan4) + 5) mod 7);
  Result := ((Trunc(ThisWeekMon) - Trunc(FirstWeekMon)) div 7) + 1;
  if (CKGetMonth(AValue) = 12) and (CKGetDay(AValue) >= 28) then
  begin
    Jan4 := EncodeDate(Year + 1, 1, 4);
    FirstWeekMon := Trunc(Jan4) - ((DayOfTheWeek(Jan4) + 5) mod 7);
    if ThisWeekMon < FirstWeekMon then Result := 53;
  end;
end;

function CKLegacyPeriodToSeconds(const APeriod: TCKLegacySpan): Int64;
begin
  Result := APeriod.Milliseconds div LegacyMillisecondsPerSecond +
    APeriod.Seconds + APeriod.Minutes * LegacySecondsPerMinute +
    APeriod.Hours * LegacySecondsPerHour + APeriod.Days * LegacySecondsPerDay +
    APeriod.Months * LegacySecondsPerMonth + APeriod.Years * LegacySecondsPerYear;
end;

function CKLegacySecondsToPeriod(const ASeconds: Int64): TCKLegacySpan;
var
  Remaining: Int64;
begin
  Result.Kind := cklskDuration;
  Result.Years := ASeconds div LegacySecondsPerYear;
  Remaining := ASeconds mod LegacySecondsPerYear;
  Result.Months := Remaining div LegacySecondsPerMonth;
  Remaining := Remaining mod LegacySecondsPerMonth;
  Result.Days := Remaining div LegacySecondsPerDay;
  Remaining := Remaining mod LegacySecondsPerDay;
  Result.Hours := Remaining div LegacySecondsPerHour;
  Remaining := Remaining mod LegacySecondsPerHour;
  Result.Minutes := Remaining div LegacySecondsPerMinute;
  Result.Seconds := Remaining mod LegacySecondsPerMinute;
  Result.Milliseconds := 0;
end;

function CKLegacyStandardizePeriod(
  const AValue: TCKLegacySpan): TCKLegacySpan;
var
  TotalHours: Integer;
begin
  Result := AValue;
  Inc(Result.Seconds, Result.Milliseconds div LegacyMillisecondsPerSecond);
  Result.Milliseconds := Result.Milliseconds mod LegacyMillisecondsPerSecond;
  Inc(Result.Minutes, Result.Seconds div LegacySecondsPerMinute);
  Result.Seconds := Result.Seconds mod LegacySecondsPerMinute;
  TotalHours := Result.Hours + Result.Minutes div LegacyMinutesPerHour;
  Result.Minutes := Result.Minutes mod LegacyMinutesPerHour;
  Inc(Result.Days, TotalHours div LegacyHoursPerDay);
  Result.Hours := TotalHours mod LegacyHoursPerDay;
  Inc(Result.Years, Result.Months div LegacyMonthsPerYear);
  Result.Months := Result.Months mod LegacyMonthsPerYear;
end;

function CKLegacyIntervalAlign(const AFirst,
  ASecond: TCKLegacyInterval): Boolean;
begin
  Result := (CompareDateTime(AFirst.EndDate, ASecond.StartDate) = 0) or
    (CompareDateTime(ASecond.EndDate, AFirst.StartDate) = 0);
end;

function CKLegacyIntervalGap(const AFirst,
  ASecond: TCKLegacyInterval): TCKLegacySpan;
begin
  Result := CKLegacyCreateDuration(0, 0, 0, 0, 0, 0, 0);
  if CompareDateTime(AFirst.EndDate, ASecond.StartDate) < 0 then
    Result := CKLegacySpanBetween(AFirst.EndDate, ASecond.StartDate,
      cklskDuration)
  else if CompareDateTime(ASecond.EndDate, AFirst.StartDate) < 0 then
    Result := CKLegacySpanBetween(ASecond.EndDate, AFirst.StartDate,
      cklskDuration);
  Result.Days := Result.Seconds div LegacySecondsPerDay;
  Result.Seconds := Result.Seconds mod LegacySecondsPerDay;
end;

function CKLegacyIntervalSetdiff(const AFirst,
  ASecond: TCKLegacyInterval): TCKLegacyInterval;
begin
  if not CKLegacyIntervalsOverlap(AFirst, ASecond) then Result := AFirst
  else if (CompareDateTime(ASecond.StartDate, AFirst.StartDate) <= 0) and
      (CompareDateTime(ASecond.EndDate, AFirst.EndDate) >= 0) then
  begin Result.StartDate := 0; Result.EndDate := 0; end
  else if CompareDateTime(ASecond.StartDate, AFirst.StartDate) <= 0 then
  begin Result.StartDate := ASecond.EndDate; Result.EndDate := AFirst.EndDate; end
  else if CompareDateTime(ASecond.EndDate, AFirst.EndDate) >= 0 then
  begin Result.StartDate := AFirst.StartDate; Result.EndDate := ASecond.StartDate; end
  else
  begin Result.StartDate := AFirst.StartDate; Result.EndDate := ASecond.StartDate; end;
end;

function CKLegacyIntervalUnion(const AFirst,
  ASecond: TCKLegacyInterval): TCKLegacyInterval;
begin
  if not CKLegacyIntervalsOverlap(AFirst, ASecond) and
     not CKLegacyIntervalAlign(AFirst, ASecond) then
  begin Result.StartDate := 0; Result.EndDate := 0; Exit; end;
  if CompareDateTime(AFirst.StartDate, ASecond.StartDate) <= 0 then
    Result.StartDate := AFirst.StartDate else Result.StartDate := ASecond.StartDate;
  if CompareDateTime(AFirst.EndDate, ASecond.EndDate) >= 0 then
    Result.EndDate := AFirst.EndDate else Result.EndDate := ASecond.EndDate;
end;

function CKLegacyIntervalIntersection(const AFirst,
  ASecond: TCKLegacyInterval): TCKLegacyInterval;
begin
  if not CKLegacyIntervalsOverlap(AFirst, ASecond) then
  begin Result.StartDate := 0; Result.EndDate := 0; Exit; end;
  if CompareDateTime(AFirst.StartDate, ASecond.StartDate) >= 0 then
    Result.StartDate := AFirst.StartDate else Result.StartDate := ASecond.StartDate;
  if CompareDateTime(AFirst.EndDate, ASecond.EndDate) <= 0 then
    Result.EndDate := AFirst.EndDate else Result.EndDate := ASecond.EndDate;
end;

end.
