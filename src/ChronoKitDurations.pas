unit ChronoKitDurations;

{$mode objfpc}{$H+}{$J-}

interface

uses
  ChronoKitInternalTypes;

function CKCreateCalendarPeriod(const AYears, AMonths, ADays, AHours,
  AMinutes, ASeconds, AMilliseconds: Integer): TCKCalendarPeriod;
function CKNormalizeCalendarPeriod(
  const AValue: TCKCalendarPeriod): TCKCalendarPeriod;
function CKDurationFromParts(const ADays, AHours, AMinutes, ASeconds,
  AMilliseconds: Int64): TCKDuration;
function CKDurationFromSeconds(const ASeconds: Int64): TCKDuration;
function CKAddPeriod(const AValue: TDateTime;
  const APeriod: TCKCalendarPeriod): TDateTime;
function CKSubtractPeriod(const AValue: TDateTime;
  const APeriod: TCKCalendarPeriod): TDateTime;
function CKAddDuration(const AValue: TDateTime;
  const ADuration: TCKDuration): TDateTime;
function CKSubtractDuration(const AValue: TDateTime;
  const ADuration: TCKDuration): TDateTime;
function CKDurationBetween(const AStart, AEnd: TDateTime): TCKDuration;

implementation

uses
  SysUtils, DateUtils;

const
  CKMillisecondsPerSecond = 1000;
  CKSecondsPerMinute = 60;
  CKMinutesPerHour = 60;
  CKHoursPerDay = 24;
  CKSecondsPerHour = CKSecondsPerMinute * CKMinutesPerHour;
  CKSecondsPerDay = CKSecondsPerHour * CKHoursPerDay;
  CKMonthsPerYear = 12;

function CheckedAdd(const ALeft, ARight: Int64): Int64;
begin
  if ((ARight > 0) and (ALeft > High(Int64) - ARight)) or
     ((ARight < 0) and (ALeft < Low(Int64) - ARight)) then
    raise ERangeError.Create('Duration exceeds the Int64 millisecond range');
  Result := ALeft + ARight;
end;

function CheckedMultiply(const AValue, AFactor: Int64): Int64;
begin
  if AFactor <= 0 then
    raise EArgumentException.Create('Duration factor must be positive');
  if (AValue > High(Int64) div AFactor) or
     (AValue < Low(Int64) div AFactor) then
    raise ERangeError.Create('Duration exceeds the Int64 millisecond range');
  Result := AValue * AFactor;
end;

procedure CheckCalendarComponent(const AValue: Int64);
begin
  if (AValue < Low(Integer)) or (AValue > High(Integer)) then
    raise ERangeError.Create(
      'Normalized calendar component exceeds Integer range');
end;

function NegateCalendarComponent(const AValue: Integer): Integer;
begin
  if AValue = Low(Integer) then
    raise ERangeError.Create(
      'Calendar component cannot be negated within the Integer range');
  Result := -AValue;
end;

function CKCreateCalendarPeriod(const AYears, AMonths, ADays, AHours,
  AMinutes, ASeconds, AMilliseconds: Integer): TCKCalendarPeriod;
begin
  Result.Years := AYears;
  Result.Months := AMonths;
  Result.Days := ADays;
  Result.Hours := AHours;
  Result.Minutes := AMinutes;
  Result.Seconds := ASeconds;
  Result.Milliseconds := AMilliseconds;
end;

function CKNormalizeCalendarPeriod(
  const AValue: TCKCalendarPeriod): TCKCalendarPeriod;
var
  Years, Months, Days, Hours, Minutes, Seconds, Milliseconds: Int64;
begin
  Years := AValue.Years;
  Months := AValue.Months;
  Days := AValue.Days;
  Hours := AValue.Hours;
  Minutes := AValue.Minutes;
  Seconds := AValue.Seconds;
  Milliseconds := AValue.Milliseconds;

  Seconds := Seconds + Milliseconds div CKMillisecondsPerSecond;
  Milliseconds := Milliseconds mod CKMillisecondsPerSecond;
  Minutes := Minutes + Seconds div CKSecondsPerMinute;
  Seconds := Seconds mod CKSecondsPerMinute;
  Hours := Hours + Minutes div CKMinutesPerHour;
  Minutes := Minutes mod CKMinutesPerHour;
  Days := Days + Hours div CKHoursPerDay;
  Hours := Hours mod CKHoursPerDay;
  Years := Years + Months div CKMonthsPerYear;
  Months := Months mod CKMonthsPerYear;

  CheckCalendarComponent(Years);
  CheckCalendarComponent(Months);
  CheckCalendarComponent(Days);
  CheckCalendarComponent(Hours);
  CheckCalendarComponent(Minutes);
  CheckCalendarComponent(Seconds);
  CheckCalendarComponent(Milliseconds);
  Result.Years := Years;
  Result.Months := Months;
  Result.Days := Days;
  Result.Hours := Hours;
  Result.Minutes := Minutes;
  Result.Seconds := Seconds;
  Result.Milliseconds := Milliseconds;
end;

function CKDurationFromParts(const ADays, AHours, AMinutes, ASeconds,
  AMilliseconds: Int64): TCKDuration;
var
  Total: Int64;
begin
  Total := CheckedMultiply(ADays,
    Int64(CKSecondsPerDay) * CKMillisecondsPerSecond);
  Total := CheckedAdd(Total, CheckedMultiply(AHours,
    Int64(CKSecondsPerHour) * CKMillisecondsPerSecond));
  Total := CheckedAdd(Total, CheckedMultiply(AMinutes,
    Int64(CKSecondsPerMinute) * CKMillisecondsPerSecond));
  Total := CheckedAdd(Total, CheckedMultiply(ASeconds,
    CKMillisecondsPerSecond));
  Result.Milliseconds := CheckedAdd(Total, AMilliseconds);
end;

function CKDurationFromSeconds(const ASeconds: Int64): TCKDuration;
begin
  Result.Milliseconds := CheckedMultiply(ASeconds,
    CKMillisecondsPerSecond);
end;

function CKAddPeriod(const AValue: TDateTime;
  const APeriod: TCKCalendarPeriod): TDateTime;
begin
  Result := AValue;
  if APeriod.Years <> 0 then
    Result := IncYear(Result, APeriod.Years);
  if APeriod.Months <> 0 then
    Result := IncMonth(Result, APeriod.Months);
  if APeriod.Days <> 0 then
    Result := IncDay(Result, APeriod.Days);
  if APeriod.Hours <> 0 then
    Result := IncHour(Result, APeriod.Hours);
  if APeriod.Minutes <> 0 then
    Result := IncMinute(Result, APeriod.Minutes);
  if APeriod.Seconds <> 0 then
    Result := IncSecond(Result, APeriod.Seconds);
  if APeriod.Milliseconds <> 0 then
    Result := IncMilliSecond(Result, APeriod.Milliseconds);
end;

function CKSubtractPeriod(const AValue: TDateTime;
  const APeriod: TCKCalendarPeriod): TDateTime;
begin
  Result := AValue;
  if APeriod.Years <> 0 then
    Result := IncYear(Result, NegateCalendarComponent(APeriod.Years));
  if APeriod.Months <> 0 then
    Result := IncMonth(Result, NegateCalendarComponent(APeriod.Months));
  if APeriod.Days <> 0 then
    Result := IncDay(Result, NegateCalendarComponent(APeriod.Days));
  if APeriod.Hours <> 0 then
    Result := IncHour(Result, NegateCalendarComponent(APeriod.Hours));
  if APeriod.Minutes <> 0 then
    Result := IncMinute(Result, NegateCalendarComponent(APeriod.Minutes));
  if APeriod.Seconds <> 0 then
    Result := IncSecond(Result, NegateCalendarComponent(APeriod.Seconds));
  if APeriod.Milliseconds <> 0 then
    Result := IncMilliSecond(Result,
      NegateCalendarComponent(APeriod.Milliseconds));
end;

function CKAddDuration(const AValue: TDateTime;
  const ADuration: TCKDuration): TDateTime;
begin
  Result := AValue + ADuration.Milliseconds /
    (Int64(CKSecondsPerDay) * CKMillisecondsPerSecond);
end;

function CKSubtractDuration(const AValue: TDateTime;
  const ADuration: TCKDuration): TDateTime;
begin
  Result := AValue - ADuration.Milliseconds /
    (Int64(CKSecondsPerDay) * CKMillisecondsPerSecond);
end;

function CKDurationBetween(const AStart, AEnd: TDateTime): TCKDuration;
begin
  Result.Milliseconds := Round((AEnd - AStart) * CKSecondsPerDay *
    CKMillisecondsPerSecond);
end;

end.
