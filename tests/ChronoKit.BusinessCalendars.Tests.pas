unit ChronoKit.BusinessCalendars.Tests;

{$mode objfpc}{$H+}{$J-}
{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  Classes, SysUtils, DateUtils, fpcunit, testregistry,
  ChronoKit;

type
  TBusinessCalendarTests = class(TTestCase)
  published
    procedure Test30_IsBusinessDay;
    procedure Test31_NextBusinessDay;
    procedure Test32_PreviousBusinessDay;
    procedure Test33_AddBusinessDays;
    procedure Test131_DefaultBusinessCalendarCompatibility;
    procedure Test132_BusinessCalendarHolidays;
    procedure Test133_AlternativeWorkingWeek;
    procedure Test134_ConfiguredBusinessDayNavigation;
    procedure Test135_InvalidBusinessCalendar;
    procedure Test136_LeapDayHolidayBoundary;
    procedure Test137_MonthEndBusinessDayBoundary;
    procedure Test138_WeekStartBusinessDayBoundary;
    procedure Test139_ZeroBusinessDaysPreservesInput;
    procedure Test168_BusinessDaysBetweenCountsInclusiveDates;
  end;

implementation

procedure TBusinessCalendarTests.Test30_IsBusinessDay;
var
  Monday, Saturday: TDateTime;
begin
  WriteLn('Test30_IsBusinessDay:Starting');
  Monday := EncodeDate(2024, 1, 15);    // Monday
  Saturday := EncodeDate(2024, 1, 20);  // Saturday
  AssertTrue('Monday should be a business day',
    TChronoKit.IsBusinessDay(Monday));
  AssertFalse('Saturday should not be a business day',
    TChronoKit.IsBusinessDay(Saturday));
  WriteLn('Test30_IsBusinessDay:Finished');
end;

procedure TBusinessCalendarTests.Test31_NextBusinessDay;
var
  Friday, Monday: TDateTime;
begin
  WriteLn('Test31_NextBusinessDay:Starting');
  Friday := EncodeDate(2024, 1, 19);    // Friday
  Monday := EncodeDate(2024, 1, 22);    // Next Monday
  AssertEquals('Next business day after Friday should be Monday',
    Monday, TChronoKit.NextBusinessDay(Friday));
  WriteLn('Test31_NextBusinessDay:Finished');
end;

procedure TBusinessCalendarTests.Test32_PreviousBusinessDay;
var
  Monday, Friday: TDateTime;
begin
  WriteLn('Test32_PreviousBusinessDay:Starting');
  Monday := EncodeDate(2024, 1, 22);    // Monday
  Friday := EncodeDate(2024, 1, 19);    // Previous Friday
  AssertEquals('Previous business day before Monday should be Friday',
    Friday, TChronoKit.PreviousBusinessDay(Monday));
  WriteLn('Test32_PreviousBusinessDay:Finished');
end;

procedure TBusinessCalendarTests.Test33_AddBusinessDays;
var
  StartDate, Expected: TDateTime;
begin
  WriteLn('Test33_AddBusinessDays:Starting');
  StartDate := EncodeDate(2024, 1, 15); // Monday
  Expected := EncodeDate(2024, 1, 19);  // Friday (4 business days later)
  AssertEquals('AddBusinessDays should skip weekends',
    Expected, TChronoKit.AddBusinessDays(StartDate, 4));
  WriteLn('Test33_AddBusinessDays:Finished');
end;

procedure TBusinessCalendarTests.Test131_DefaultBusinessCalendarCompatibility;
var
  Friday: TDateTime;
begin
  WriteLn('Test131_DefaultBusinessCalendarCompatibility:Starting');
  Friday := EncodeDate(2026, 12, 25);

  AssertTrue('Legacy IsBusinessDay should still treat Friday as a business day',
    TChronoKit.IsBusinessDay(Friday));
  AssertEquals('Legacy AddBusinessDays should still use Monday through Friday',
    Friday, TChronoKit.AddBusinessDays(EncodeDate(2026, 12, 24), 1));
  WriteLn('Test131_DefaultBusinessCalendarCompatibility:Finished');
end;

procedure TBusinessCalendarTests.Test132_BusinessCalendarHolidays;
var
  Calendar: TBusinessCalendar;
  Holiday, Tuesday: TDateTime;
begin
  WriteLn('Test132_BusinessCalendarHolidays:Starting');
  Holiday := EncodeDateTime(2024, 1, 1, 12, 30, 0, 0);
  Tuesday := EncodeDate(2024, 1, 2);
  Calendar := TChronoKit.CreateBusinessCalendar([Holiday]);

  AssertFalse('Configured holiday should not be a business day',
    TChronoKit.IsBusinessDay(EncodeDate(2024, 1, 1), Calendar));
  AssertTrue('Non-holiday weekday should remain a business day',
    TChronoKit.IsBusinessDay(Tuesday, Calendar));
  WriteLn('Test132_BusinessCalendarHolidays:Finished');
end;

procedure TBusinessCalendarTests.Test133_AlternativeWorkingWeek;
var
  Calendar: TBusinessCalendar;
  Sunday, Friday: TDateTime;
begin
  WriteLn('Test133_AlternativeWorkingWeek:Starting');
  Calendar := TChronoKit.CreateBusinessCalendar(
    [bwdSunday, bwdMonday, bwdTuesday, bwdWednesday, bwdThursday], []);
  Sunday := EncodeDate(2024, 1, 7);
  Friday := EncodeDate(2024, 1, 5);

  AssertTrue('Sunday should be configurable as a working day',
    TChronoKit.IsBusinessDay(Sunday, Calendar));
  AssertFalse('Friday should be configurable as a non-working day',
    TChronoKit.IsBusinessDay(Friday, Calendar));
  WriteLn('Test133_AlternativeWorkingWeek:Finished');
end;

procedure TBusinessCalendarTests.Test134_ConfiguredBusinessDayNavigation;
var
  Calendar: TBusinessCalendar;
  Friday, MondayHoliday, Tuesday: TDateTime;
begin
  WriteLn('Test134_ConfiguredBusinessDayNavigation:Starting');
  Friday := EncodeDate(2023, 12, 29);
  MondayHoliday := EncodeDate(2024, 1, 1);
  Tuesday := EncodeDate(2024, 1, 2);
  Calendar := TChronoKit.CreateBusinessCalendar([MondayHoliday]);

  AssertEquals('NextBusinessDay should skip weekends and holidays',
    Tuesday, TChronoKit.NextBusinessDay(Friday, Calendar));
  AssertEquals('PreviousBusinessDay should skip weekends and holidays',
    Friday, TChronoKit.PreviousBusinessDay(Tuesday, Calendar));
  AssertEquals('AddBusinessDays should skip holidays when moving forward',
    Tuesday, TChronoKit.AddBusinessDays(Friday, 1, Calendar));
  AssertEquals('AddBusinessDays should skip holidays when moving backward',
    Friday, TChronoKit.AddBusinessDays(Tuesday, -1, Calendar));
  WriteLn('Test134_ConfiguredBusinessDayNavigation:Finished');
end;

procedure TBusinessCalendarTests.Test135_InvalidBusinessCalendar;
var
  Calendar: TBusinessCalendar;
begin
  WriteLn('Test135_InvalidBusinessCalendar:Starting');
  try
    TChronoKit.CreateBusinessCalendar([], []);
    Fail('CreateBusinessCalendar should reject an empty working week');
  except
    on E: EBusinessCalendarError do
      AssertTrue('Calendar validation should explain the working-day requirement',
        Pos('working day', LowerCase(E.Message)) > 0);
  end;

  Calendar.WorkingDays := [];
  SetLength(Calendar.Holidays, 0);
  try
    TChronoKit.NextBusinessDay(EncodeDate(2024, 1, 1), Calendar);
    Fail('Business-day operations should reject directly assigned invalid calendars');
  except
    on E: EBusinessCalendarError do
      AssertTrue('Operation validation should explain the working-day requirement',
        Pos('working day', LowerCase(E.Message)) > 0);
  end;
  WriteLn('Test135_InvalidBusinessCalendar:Finished');
end;

procedure TBusinessCalendarTests.Test136_LeapDayHolidayBoundary;
var
  Calendar: TBusinessCalendar;
  StartDate, Expected: TDateTime;
begin
  WriteLn('Test136_LeapDayHolidayBoundary:Starting');
  Calendar := TChronoKit.CreateBusinessCalendar([EncodeDate(2024, 2, 29)]);
  StartDate := EncodeDateTime(2024, 2, 28, 15, 45, 30, 125);
  Expected := EncodeDateTime(2024, 3, 1, 15, 45, 30, 125);

  AssertEquals('A leap-day holiday should be skipped and preserve the time',
    Expected, TChronoKit.AddBusinessDays(StartDate, 1, Calendar));
  WriteLn('Test136_LeapDayHolidayBoundary:Finished');
end;

procedure TBusinessCalendarTests.Test137_MonthEndBusinessDayBoundary;
var
  Calendar: TBusinessCalendar;
  January30, January31Holiday, February1: TDateTime;
begin
  WriteLn('Test137_MonthEndBusinessDayBoundary:Starting');
  January30 := EncodeDate(2024, 1, 30);
  January31Holiday := EncodeDate(2024, 1, 31);
  February1 := EncodeDate(2024, 2, 1);
  Calendar := TChronoKit.CreateBusinessCalendar([January31Holiday]);

  AssertEquals('Forward calculation should cross month end after a holiday',
    February1, TChronoKit.AddBusinessDays(January30, 1, Calendar));
  AssertEquals('Backward calculation should cross month end after a holiday',
    January30, TChronoKit.AddBusinessDays(February1, -1, Calendar));
  WriteLn('Test137_MonthEndBusinessDayBoundary:Finished');
end;

procedure TBusinessCalendarTests.Test138_WeekStartBusinessDayBoundary;
var
  Calendar: TBusinessCalendar;
  Thursday, Sunday: TDateTime;
begin
  WriteLn('Test138_WeekStartBusinessDayBoundary:Starting');
  Calendar := TChronoKit.CreateBusinessCalendar(
    [bwdSunday, bwdMonday, bwdTuesday, bwdWednesday, bwdThursday], []);
  Thursday := EncodeDate(2024, 1, 4);
  Sunday := EncodeDate(2024, 1, 7);

  AssertEquals('Next business day should honor a Sunday week start',
    Sunday, TChronoKit.NextBusinessDay(Thursday, Calendar));
  AssertEquals('Previous business day should remain strict at the week start',
    Thursday, TChronoKit.PreviousBusinessDay(Sunday, Calendar));
  WriteLn('Test138_WeekStartBusinessDayBoundary:Finished');
end;

procedure TBusinessCalendarTests.Test139_ZeroBusinessDaysPreservesInput;
var
  Calendar: TBusinessCalendar;
  Saturday: TDateTime;
begin
  WriteLn('Test139_ZeroBusinessDaysPreservesInput:Starting');
  Calendar := TChronoKit.CreateBusinessCalendar([]);
  Saturday := EncodeDateTime(2024, 1, 6, 9, 15, 30, 250);

  AssertEquals('Zero business days should return the exact input value',
    Saturday, TChronoKit.AddBusinessDays(Saturday, 0, Calendar));
  WriteLn('Test139_ZeroBusinessDaysPreservesInput:Finished');
end;

procedure TBusinessCalendarTests.Test168_BusinessDaysBetweenCountsInclusiveDates;
var
  Calendar: TBusinessCalendar;
  InvalidCalendar: TBusinessCalendar;
  Raised: Boolean;
begin
  AssertEquals('The default calendar counts both business-date endpoints', 5,
    TChronoKit.BusinessDaysBetween(
      EncodeDateTime(2024, 7, 1, 20, 0, 0, 0),
      EncodeDateTime(2024, 7, 7, 1, 0, 0, 0)));
  AssertEquals('Reverse input order reverses the signed count', -5,
    TChronoKit.BusinessDaysBetween(EncodeDate(2024, 7, 7),
      EncodeDate(2024, 7, 1)));
  AssertEquals('A same-day weekend range has no business dates', 0,
    TChronoKit.BusinessDaysBetween(EncodeDate(2024, 7, 7),
      EncodeDate(2024, 7, 7)));

  Calendar := TChronoKit.CreateBusinessCalendar(
    [bwdSunday, bwdMonday, bwdTuesday, bwdWednesday, bwdThursday],
    [EncodeDateTime(2024, 7, 4, 15, 0, 0, 0)]);
  AssertEquals('Custom weeks and date-only holidays affect the count', 4,
    TChronoKit.BusinessDaysBetween(EncodeDate(2024, 7, 1),
      EncodeDate(2024, 7, 7), Calendar));

  InvalidCalendar.WorkingDays := [];
  SetLength(InvalidCalendar.Holidays, 0);
  Raised := False;
  try
    TChronoKit.BusinessDaysBetween(EncodeDate(2024, 7, 1),
      EncodeDate(2024, 7, 2), InvalidCalendar);
  except
    on E: EBusinessCalendarError do
      Raised := Pos('working day', LowerCase(E.Message)) > 0;
  end;
  AssertTrue('Business-day counting validates custom calendars', Raised);
end;

initialization
  RegisterTest(TBusinessCalendarTests);

end.
