unit ChronoKitBusinessCalendars;

{$mode objfpc}{$H+}{$J-}

interface

uses
  ChronoKitInternalTypes;

function CKDefaultBusinessCalendar: TCKBusinessCalendar;
function CKCreateBusinessCalendar(const AWorkingDays: TCKBusinessWeek;
  const AHolidays: array of TDateTime): TCKBusinessCalendar;
function CKIsHoliday(const AValue: TDateTime;
  const ACalendar: TCKBusinessCalendar): Boolean;
function CKIsBusinessDay(const AValue: TDateTime;
  const ACalendar: TCKBusinessCalendar): Boolean;
function CKNextBusinessDay(const AValue: TDateTime;
  const ACalendar: TCKBusinessCalendar): TDateTime;
function CKPreviousBusinessDay(const AValue: TDateTime;
  const ACalendar: TCKBusinessCalendar): TDateTime;
function CKAddBusinessDays(const AValue: TDateTime; const ADays: Integer;
  const ACalendar: TCKBusinessCalendar): TDateTime;
function CKBusinessDaysBetween(const AStartDate, AEndDate: TDateTime;
  const ACalendar: TCKBusinessCalendar): Integer;

implementation

uses
  DateUtils;

function CKDefaultBusinessCalendar: TCKBusinessCalendar;
begin
  Result.WorkingDays := [ckbwdMonday, ckbwdTuesday, ckbwdWednesday,
    ckbwdThursday, ckbwdFriday];
  Result.Holidays := nil;
end;

function CKCreateBusinessCalendar(const AWorkingDays: TCKBusinessWeek;
  const AHolidays: array of TDateTime): TCKBusinessCalendar;
var
  I: Integer;
begin
  Result.WorkingDays := AWorkingDays;
  SetLength(Result.Holidays, Length(AHolidays));
  for I := Low(AHolidays) to High(AHolidays) do
    Result.Holidays[I] := AHolidays[I];
end;

function CKIsHoliday(const AValue: TDateTime;
  const ACalendar: TCKBusinessCalendar): Boolean;
var
  I: Integer;
begin
  for I := Low(ACalendar.Holidays) to High(ACalendar.Holidays) do
    if SameDate(AValue, ACalendar.Holidays[I]) then
      Exit(True);
  Result := False;
end;

function CKIsBusinessDay(const AValue: TDateTime;
  const ACalendar: TCKBusinessCalendar): Boolean;
var
  Weekday: TCKBusinessWeekday;
begin
  Weekday := TCKBusinessWeekday(DayOfTheWeek(AValue) mod 7);
  Result := (Weekday in ACalendar.WorkingDays) and
    not CKIsHoliday(AValue, ACalendar);
end;

function CKNextBusinessDay(const AValue: TDateTime;
  const ACalendar: TCKBusinessCalendar): TDateTime;
begin
  Result := AValue;
  repeat
    Result := IncDay(Result);
  until CKIsBusinessDay(Result, ACalendar);
end;

function CKPreviousBusinessDay(const AValue: TDateTime;
  const ACalendar: TCKBusinessCalendar): TDateTime;
begin
  Result := AValue;
  repeat
    Result := IncDay(Result, -1);
  until CKIsBusinessDay(Result, ACalendar);
end;

function CKAddBusinessDays(const AValue: TDateTime; const ADays: Integer;
  const ACalendar: TCKBusinessCalendar): TDateTime;
var
  Step: Integer;
  RemainingDays: Int64;
begin
  Result := AValue;
  if ADays = 0 then
    Exit;

  if ADays < 0 then
    Step := -1
  else
    Step := 1;
  RemainingDays := Abs(Int64(ADays));

  while RemainingDays > 0 do
  begin
    Result := IncDay(Result, Step);
    if CKIsBusinessDay(Result, ACalendar) then
      Dec(RemainingDays);
  end;
end;

function CKBusinessDaysBetween(const AStartDate, AEndDate: TDateTime;
  const ACalendar: TCKBusinessCalendar): Integer;
var
  CurrentDate, EndDate, StartDate: TDateTime;
begin
  StartDate := DateOf(AStartDate);
  EndDate := DateOf(AEndDate);
  if StartDate > EndDate then
    Exit(-CKBusinessDaysBetween(EndDate, StartDate, ACalendar));

  Result := 0;
  CurrentDate := StartDate;
  repeat
    if CKIsBusinessDay(CurrentDate, ACalendar) then
      Inc(Result);
    if CurrentDate = EndDate then
      Exit;
    CurrentDate := IncDay(CurrentDate);
  until False;
end;

end.
