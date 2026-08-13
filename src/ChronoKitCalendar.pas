unit ChronoKitCalendar;

{$mode objfpc}{$H+}{$J-}

interface

uses
  ChronoKitInternalTypes;

function CKGetYear(const AValue: TDateTime): Integer;
function CKGetMonth(const AValue: TDateTime): Integer;
function CKGetDay(const AValue: TDateTime): Integer;
function CKGetDayOfWeek(const AValue: TDateTime): Integer;
function CKGetDayOfYear(const AValue: TDateTime): Integer;
function CKGetHour(const AValue: TDateTime): Integer;
function CKGetMinute(const AValue: TDateTime): Integer;
function CKGetSecond(const AValue: TDateTime): Integer;
function CKGetMillisecond(const AValue: TDateTime): Integer;
function CKSetYear(const AValue: TDateTime; const AYear: Integer): TDateTime;
function CKSetMonth(const AValue: TDateTime; const AMonth: Integer): TDateTime;
function CKSetDay(const AValue: TDateTime; const ADay: Integer): TDateTime;
function CKSetHour(const AValue: TDateTime; const AHour: Integer): TDateTime;
function CKSetMinute(const AValue: TDateTime; const AMinute: Integer): TDateTime;
function CKSetSecond(const AValue: TDateTime; const ASecond: Integer): TDateTime;
function CKSetMillisecond(const AValue: TDateTime;
  const AMillisecond: Integer): TDateTime;
function CKAddYears(const AValue: TDateTime; const AYears: Integer): TDateTime;
function CKAddMonths(const AValue: TDateTime; const AMonths: Integer): TDateTime;
function CKAddDays(const AValue: TDateTime; const ADays: Integer): TDateTime;
function CKAddHours(const AValue: TDateTime; const AHours: Integer): TDateTime;
function CKAddMinutes(const AValue: TDateTime; const AMinutes: Integer): TDateTime;
function CKAddSeconds(const AValue: TDateTime; const ASeconds: Integer): TDateTime;
function CKFloorDate(const AValue: TDateTime; const AUnit: TCKDateUnit): TDateTime;
function CKCeilingDate(const AValue: TDateTime;
  const AUnit: TCKDateUnit): TDateTime;
function CKRoundDate(const AValue: TDateTime; const AUnit: TCKDateUnit): TDateTime;
function CKStartOfYear(const AValue: TDateTime): TDateTime;
function CKStartOfMonth(const AValue: TDateTime): TDateTime;
function CKStartOfWeek(const AValue: TDateTime): TDateTime;
function CKStartOfDay(const AValue: TDateTime): TDateTime;
function CKStartOfHour(const AValue: TDateTime): TDateTime;
function CKEndOfYear(const AValue: TDateTime): TDateTime;
function CKEndOfMonth(const AValue: TDateTime): TDateTime;
function CKEndOfWeek(const AValue: TDateTime): TDateTime;
function CKEndOfDay(const AValue: TDateTime): TDateTime;
function CKEndOfHour(const AValue: TDateTime): TDateTime;
function CKIsBefore(const AValue, ADateTime: TDateTime): Boolean;
function CKIsAfter(const AValue, ADateTime: TDateTime): Boolean;
function CKIsSameDay(const AValue, ADateTime: TDateTime): Boolean;
function CKIsSameMonth(const AValue, ADateTime: TDateTime): Boolean;
function CKIsSameYear(const AValue, ADateTime: TDateTime): Boolean;
function CKGetQuarter(const AValue: TDateTime): Integer;
function CKStartOfQuarter(const AYear, AQuarter: Integer): TDateTime; overload;
function CKStartOfQuarter(const AValue: TDateTime): TDateTime; overload;
function CKEndOfQuarter(const AValue: TDateTime): TDateTime;
function CKIsAM(const AValue: TDateTime): Boolean;
function CKIsPM(const AValue: TDateTime): Boolean;
function CKDecimalYearToDateTime(const AValue: Double): TDateTime;
function CKDateTimeToDecimalYear(const AValue: TDateTime): Double;
function CKGetISOYear(const AValue: TDateTime): Integer;
function CKGetISOWeek(const AValue: TDateTime): Integer;
function CKGetSemester(const AValue: TDateTime): Integer;

implementation

uses
  SysUtils, DateUtils, Math;

const
  CKOneMillisecond = 1 / (SecsPerDay * MSecsPerSec);

procedure DecodeDateParts(const AValue: TDateTime; out AYear, AMonth,
  ADay: Word);
begin
  DecodeDate(AValue, AYear, AMonth, ADay);
end;

procedure DecodeTimeParts(const AValue: TDateTime; out AHour, AMinute,
  ASecond, AMillisecond: Word);
begin
  DecodeTime(AValue, AHour, AMinute, ASecond, AMillisecond);
end;

function CKGetYear(const AValue: TDateTime): Integer;
var Y, M, D: Word;
begin DecodeDateParts(AValue, Y, M, D); Result := Y; end;

function CKGetMonth(const AValue: TDateTime): Integer;
var Y, M, D: Word;
begin DecodeDateParts(AValue, Y, M, D); Result := M; end;

function CKGetDay(const AValue: TDateTime): Integer;
var Y, M, D: Word;
begin DecodeDateParts(AValue, Y, M, D); Result := D; end;

function CKGetDayOfWeek(const AValue: TDateTime): Integer;
begin Result := SysUtils.DayOfWeek(AValue); end;

function CKGetDayOfYear(const AValue: TDateTime): Integer;
begin Result := DateUtils.DayOfTheYear(AValue); end;

function CKGetHour(const AValue: TDateTime): Integer;
var H, N, S, MS: Word;
begin DecodeTimeParts(AValue, H, N, S, MS); Result := H; end;

function CKGetMinute(const AValue: TDateTime): Integer;
var H, N, S, MS: Word;
begin DecodeTimeParts(AValue, H, N, S, MS); Result := N; end;

function CKGetSecond(const AValue: TDateTime): Integer;
var H, N, S, MS: Word;
begin DecodeTimeParts(AValue, H, N, S, MS); Result := S; end;

function CKGetMillisecond(const AValue: TDateTime): Integer;
var H, N, S, MS: Word;
begin DecodeTimeParts(AValue, H, N, S, MS); Result := MS; end;

function CKSetYear(const AValue: TDateTime; const AYear: Integer): TDateTime;
var Y, M, D, NewD: Word;
begin
  DecodeDateParts(AValue, Y, M, D);
  if (M = 2) and (D = 29) and not IsLeapYear(AYear) then NewD := 28
  else NewD := D;
  Result := EncodeDate(AYear, M, NewD) + Frac(AValue);
end;

function CKSetMonth(const AValue: TDateTime; const AMonth: Integer): TDateTime;
var Y, M, D, LastDay, NewD: Word;
begin
  DecodeDateParts(AValue, Y, M, D);
  LastDay := DaysInMonth(EncodeDate(Y, AMonth, 1));
  if D > LastDay then NewD := LastDay else NewD := D;
  Result := EncodeDate(Y, AMonth, NewD) + Frac(AValue);
end;

function CKSetDay(const AValue: TDateTime; const ADay: Integer): TDateTime;
var Y, M, D: Word;
begin DecodeDateParts(AValue, Y, M, D); Result := EncodeDate(Y, M, ADay) + Frac(AValue); end;

function CKSetHour(const AValue: TDateTime; const AHour: Integer): TDateTime;
var H, N, S, MS: Word;
begin DecodeTimeParts(AValue, H, N, S, MS); Result := Trunc(AValue) + EncodeTime(AHour, N, S, MS); end;

function CKSetMinute(const AValue: TDateTime; const AMinute: Integer): TDateTime;
var H, N, S, MS: Word;
begin DecodeTimeParts(AValue, H, N, S, MS); Result := Trunc(AValue) + EncodeTime(H, AMinute, S, MS); end;

function CKSetSecond(const AValue: TDateTime; const ASecond: Integer): TDateTime;
var H, N, S, MS: Word;
begin DecodeTimeParts(AValue, H, N, S, MS); Result := Trunc(AValue) + EncodeTime(H, N, ASecond, MS); end;

function CKSetMillisecond(const AValue: TDateTime;
  const AMillisecond: Integer): TDateTime;
var H, N, S, MS: Word;
begin DecodeTimeParts(AValue, H, N, S, MS); Result := Trunc(AValue) + EncodeTime(H, N, S, AMillisecond); end;

function CKAddYears(const AValue: TDateTime; const AYears: Integer): TDateTime;
begin Result := IncYear(AValue, AYears); end;
function CKAddMonths(const AValue: TDateTime; const AMonths: Integer): TDateTime;
begin Result := IncMonth(AValue, AMonths); end;
function CKAddDays(const AValue: TDateTime; const ADays: Integer): TDateTime;
begin Result := IncDay(AValue, ADays); end;
function CKAddHours(const AValue: TDateTime; const AHours: Integer): TDateTime;
begin Result := IncHour(AValue, AHours); end;
function CKAddMinutes(const AValue: TDateTime; const AMinutes: Integer): TDateTime;
begin Result := IncMinute(AValue, AMinutes); end;
function CKAddSeconds(const AValue: TDateTime; const ASeconds: Integer): TDateTime;
begin Result := IncSecond(AValue, ASeconds); end;

function CKFloorDate(const AValue: TDateTime; const AUnit: TCKDateUnit): TDateTime;
var Y, M, D, H, N, S, MS: Word; Weekday: Integer;
begin
  Result := AValue;
  DecodeDateParts(AValue, Y, M, D); DecodeTimeParts(AValue, H, N, S, MS);
  case AUnit of
    ckduSeason: raise EArgumentException.Create('Season rounding requires an explicit hemisphere and season definition');
    ckduYear: Result := EncodeDate(Y, 1, 1);
    ckduHalfYear: if M > 6 then Result := EncodeDate(Y, 7, 1) else Result := EncodeDate(Y, 1, 1);
    ckduQuarter: begin M := ((M - 1) div 3) * 3 + 1; Result := EncodeDate(Y, M, 1); end;
    ckduBiMonth: begin M := ((M - 1) div 2) * 2 + 1; Result := EncodeDate(Y, M, 1); end;
    ckduMonth: Result := EncodeDate(Y, M, 1);
    ckduWeek: begin Weekday := CKGetDayOfWeek(AValue); Result := IncDay(Trunc(AValue), -(Weekday - 1)); end;
    ckduDay: Result := Trunc(AValue);
    ckduHour: Result := Trunc(AValue) + EncodeTime(H, 0, 0, 0);
    ckduMinute: Result := Trunc(AValue) + EncodeTime(H, N, 0, 0);
    ckduSecond: Result := Trunc(AValue) + EncodeTime(H, N, S, 0);
  end;
end;

function CKCeilingDate(const AValue: TDateTime;
  const AUnit: TCKDateUnit): TDateTime;
var Y, M, D, H, N, S, MS: Word; Weekday: Integer;
begin
  Result := AValue;
  DecodeDateParts(AValue, Y, M, D); DecodeTimeParts(AValue, H, N, S, MS);
  case AUnit of
    ckduSeason: raise EArgumentException.Create('Season rounding requires an explicit hemisphere and season definition');
    ckduYear: if (M = 1) and (D = 1) and (H = 0) and (N = 0) and (S = 0) and (MS = 0) then Result := AValue else Result := EncodeDate(Y + 1, 1, 1);
    ckduHalfYear: if M <= 6 then Result := EncodeDate(Y, 7, 1) else Result := EncodeDate(Y + 1, 1, 1);
    ckduQuarter: begin M := ((M - 1) div 3) * 3 + 4; if M > 12 then begin Inc(Y); M := 1; end; Result := EncodeDate(Y, M, 1); end;
    ckduBiMonth: begin M := ((M - 1) div 2) * 2 + 3; if M > 12 then begin Inc(Y); M := 1; end; Result := EncodeDate(Y, M, 1); end;
    ckduMonth: if M = 12 then Result := EncodeDate(Y + 1, 1, 1) else Result := EncodeDate(Y, M + 1, 1);
    ckduWeek: begin Weekday := CKGetDayOfWeek(AValue); if (Weekday = 1) and (H = 0) and (N = 0) and (S = 0) and (MS = 0) then Result := AValue else Result := IncDay(Trunc(AValue), 8 - Weekday); end;
    ckduDay: Result := Trunc(AValue) + 1;
    ckduHour: Result := IncHour(CKFloorDate(AValue, ckduHour), 1);
    ckduMinute: Result := IncMinute(CKFloorDate(AValue, ckduMinute), 1);
    ckduSecond: Result := IncSecond(CKFloorDate(AValue, ckduSecond), 1);
  end;
end;

function CKRoundDate(const AValue: TDateTime; const AUnit: TCKDateUnit): TDateTime;
var FloorValue, CeilingValue, MidPoint: TDateTime; FloorDiff, CeilingDiff: Double; Y, M, D: Word;
begin
  FloorValue := CKFloorDate(AValue, AUnit); CeilingValue := CKCeilingDate(AValue, AUnit);
  case AUnit of
    ckduMonth: begin DecodeDateParts(AValue, Y, M, D); MidPoint := EncodeDate(Y, M, 15); if CompareDateTime(AValue, MidPoint) <= 0 then Result := FloorValue else Result := CeilingValue; end;
    ckduHalfYear: begin DecodeDateParts(AValue, Y, M, D); if M <= 6 then MidPoint := EncodeDate(Y, 3, 15) else MidPoint := EncodeDate(Y, 9, 15); if CompareDateTime(AValue, MidPoint) <= 0 then Result := FloorValue else Result := CeilingValue; end;
    else begin FloorDiff := Abs(AValue - FloorValue); CeilingDiff := Abs(CeilingValue - AValue); if FloorDiff <= CeilingDiff then Result := FloorValue else Result := CeilingValue; end;
  end;
end;

function CKStartOfYear(const AValue: TDateTime): TDateTime; begin Result := CKFloorDate(AValue, ckduYear); end;
function CKStartOfMonth(const AValue: TDateTime): TDateTime; begin Result := CKFloorDate(AValue, ckduMonth); end;
function CKStartOfWeek(const AValue: TDateTime): TDateTime; begin Result := CKFloorDate(AValue, ckduWeek); end;
function CKStartOfDay(const AValue: TDateTime): TDateTime; begin Result := CKFloorDate(AValue, ckduDay); end;
function CKStartOfHour(const AValue: TDateTime): TDateTime; begin Result := CKFloorDate(AValue, ckduHour); end;
function CKEndOfYear(const AValue: TDateTime): TDateTime; begin Result := IncYear(CKStartOfYear(AValue), 1) - CKOneMillisecond; end;
function CKEndOfMonth(const AValue: TDateTime): TDateTime; begin Result := CKCeilingDate(AValue, ckduMonth) - CKOneMillisecond; end;
function CKEndOfWeek(const AValue: TDateTime): TDateTime; begin Result := IncDay(CKStartOfWeek(AValue), 7) - CKOneMillisecond; end;
function CKEndOfDay(const AValue: TDateTime): TDateTime; begin Result := CKCeilingDate(AValue, ckduDay) - CKOneMillisecond; end;
function CKEndOfHour(const AValue: TDateTime): TDateTime; begin Result := CKCeilingDate(AValue, ckduHour) - CKOneMillisecond; end;

function CKIsBefore(const AValue, ADateTime: TDateTime): Boolean; begin Result := CompareDateTime(AValue, ADateTime) < 0; end;
function CKIsAfter(const AValue, ADateTime: TDateTime): Boolean; begin Result := CompareDateTime(AValue, ADateTime) > 0; end;
function CKIsSameDay(const AValue, ADateTime: TDateTime): Boolean; begin Result := SameDate(AValue, ADateTime); end;

function CKIsSameMonth(const AValue, ADateTime: TDateTime): Boolean;
var Y1, M1, D1, Y2, M2, D2: Word;
begin DecodeDateParts(AValue, Y1, M1, D1); DecodeDateParts(ADateTime, Y2, M2, D2); Result := (Y1 = Y2) and (M1 = M2); end;

function CKIsSameYear(const AValue, ADateTime: TDateTime): Boolean;
var Y1, M1, D1, Y2, M2, D2: Word;
begin DecodeDateParts(AValue, Y1, M1, D1); DecodeDateParts(ADateTime, Y2, M2, D2); Result := Y1 = Y2; end;

function CKGetQuarter(const AValue: TDateTime): Integer; begin Result := ((CKGetMonth(AValue) - 1) div 3) + 1; end;

function CKStartOfQuarter(const AYear, AQuarter: Integer): TDateTime;
begin
  if (AYear < 1) or (AYear > 9999) then raise EArgumentException.Create('Quarter year must be between 1 and 9999');
  if (AQuarter < 1) or (AQuarter > 4) then raise EArgumentException.Create('Quarter must be between 1 and 4');
  Result := EncodeDate(AYear, 1 + (AQuarter - 1) * 3, 1);
end;

function CKStartOfQuarter(const AValue: TDateTime): TDateTime;
begin Result := CKStartOfQuarter(CKGetYear(AValue), CKGetQuarter(AValue)); end;

function CKEndOfQuarter(const AValue: TDateTime): TDateTime;
var Month, Year: Word;
begin Year := CKGetYear(AValue); Month := CKGetQuarter(AValue) * 3; Result := EncodeDateTime(Year, Month, DaysInAMonth(Year, Month), 23, 59, 59, 999); end;

function CKIsAM(const AValue: TDateTime): Boolean; begin Result := CKGetHour(AValue) < 12; end;
function CKIsPM(const AValue: TDateTime): Boolean; begin Result := CKGetHour(AValue) >= 12; end;

function CKDecimalYearToDateTime(const AValue: Double): TDateTime;
var Year, DaysInYear: Integer; Fraction: Double; MillisecondsInYear, ElapsedMilliseconds: Int64;
begin
  if IsNan(AValue) or IsInfinite(AValue) or (AValue < 1.0) or (AValue >= 10000.0) then raise EArgumentException.Create('Decimal year must be a finite value with a year between 1 and 9999');
  Year := Trunc(AValue); Fraction := AValue - Year;
  if IsLeapYear(Year) then DaysInYear := 366 else DaysInYear := 365;
  MillisecondsInYear := Int64(DaysInYear) * SecsPerDay * MSecsPerSec;
  ElapsedMilliseconds := Round(Fraction * MillisecondsInYear);
  if ElapsedMilliseconds >= MillisecondsInYear then ElapsedMilliseconds := MillisecondsInYear - 1;
  Result := EncodeDate(Year, 1, 1) + ElapsedMilliseconds / (Int64(SecsPerDay) * MSecsPerSec);
end;

function CKDateTimeToDecimalYear(const AValue: TDateTime): Double;
var Year, Month, Day: Word; DaysInYear: Integer;
begin DecodeDateParts(AValue, Year, Month, Day); if IsLeapYear(Year) then DaysInYear := 366 else DaysInYear := 365; Result := Year + (AValue - EncodeDate(Year, 1, 1)) / DaysInYear; end;

function CKGetISOYear(const AValue: TDateTime): Integer;
var Y, M, D: Word; Jan4, ThisWeekMon, FirstWeekMon: TDateTime;
begin
  DecodeDateParts(AValue, Y, M, D); Jan4 := EncodeDate(Y, 1, 4);
  ThisWeekMon := Trunc(AValue) - ((DayOfTheWeek(AValue) + 5) mod 7);
  FirstWeekMon := Trunc(Jan4) - ((DayOfTheWeek(Jan4) + 5) mod 7);
  if ThisWeekMon < FirstWeekMon then Result := Y - 1
  else begin Jan4 := EncodeDate(Y + 1, 1, 4); FirstWeekMon := Trunc(Jan4) - ((DayOfTheWeek(Jan4) + 5) mod 7); if ThisWeekMon >= FirstWeekMon then Result := Y + 1 else Result := Y; end;
end;

function CKGetISOWeek(const AValue: TDateTime): Integer;
var Y: Integer; Jan4, ThisWeekMon, FirstWeekMon: TDateTime;
begin Y := CKGetISOYear(AValue); Jan4 := EncodeDate(Y, 1, 4); ThisWeekMon := Trunc(AValue) - ((DayOfTheWeek(AValue) + 5) mod 7); FirstWeekMon := Trunc(Jan4) - ((DayOfTheWeek(Jan4) + 5) mod 7); Result := ((Trunc(ThisWeekMon) - Trunc(FirstWeekMon)) div 7) + 1; end;

function CKGetSemester(const AValue: TDateTime): Integer; begin Result := ((CKGetMonth(AValue) - 1) div 6) + 1; end;

end.
