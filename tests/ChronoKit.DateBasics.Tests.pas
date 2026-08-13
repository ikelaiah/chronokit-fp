unit ChronoKit.DateBasics.Tests;

{$mode objfpc}{$H+}{$J-}
{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  Classes, SysUtils, DateUtils, fpcunit, testregistry,
  ChronoKit;

type
  TDateBasicsTests = class(TTestCase)
  published
    procedure Test01_Now;
    procedure Test02_Today;
    procedure Test04_Year;
    procedure Test05_Month;
    procedure Test06_Day;
    procedure Test07_Hour;
    procedure Test08_Minute;
    procedure Test09_Second;
    procedure Test10_Millisecond;
    procedure Test11_AddYears;
    procedure Test12_AddMonths;
    procedure Test13_AddDays;
    procedure Test14_AddHours;
    procedure Test15_AddMinutes;
    procedure Test16_AddSeconds;
    procedure Test17_StartOfYear;
    procedure Test18_StartOfMonth;
    procedure Test19_StartOfDay;
    procedure Test20_EndOfYear;
    procedure Test21_EndOfMonth;
    procedure Test22_EndOfDay;
    procedure Test23_IsBefore;
    procedure Test24_IsAfter;
    procedure Test25_IsSameDay;
    procedure Test26_IsSameMonth;
    procedure Test27_IsSameYear;
  end;

implementation

procedure TDateBasicsTests.Test01_Now;
var
  CurrentTime: TDateTime;
begin
  WriteLn('Test01_Now:Starting');
  CurrentTime := Now;
  AssertTrue('Now should return current time',
    Abs(CurrentTime - TChronoKit.GetNow) < 1/86400); // Within 1 second
  WriteLn('Test01_Now:Finished');
end;

procedure TDateBasicsTests.Test02_Today;
begin
  WriteLn('Test02_Today:Starting');
  AssertEquals('Today should return current date at midnight',
    Trunc(Date), Trunc(TChronoKit.GetToday));
  WriteLn('Test02_Today:Finished');
end;

procedure TDateBasicsTests.Test04_Year;
var
  TestYear: Integer;
  TestDate: TDateTime;
begin
  WriteLn('Test04_Year:Starting');
  TestYear := 2024;
  TestDate := Now;
  AssertEquals('Year getter should return current year',
    YearOf(TestDate), TChronoKit.GetYear(TestDate));
  AssertEquals('Year setter should set specified year',
    TestYear, TChronoKit.GetYear(TChronoKit.SetYear(TestDate, TestYear)));
  WriteLn('Test04_Year:Finished');
end;

procedure TDateBasicsTests.Test05_Month;
var
  TestMonth: Integer;
  TestDate: TDateTime;
begin
  WriteLn('Test05_Month:Starting');
  TestMonth := 6;
  TestDate := Now;
  AssertEquals('Month getter should return current month',
    MonthOf(TestDate), TChronoKit.GetMonth(TestDate));
  AssertEquals('Month setter should set specified month',
    TestMonth, TChronoKit.GetMonth(TChronoKit.SetMonth(TestDate, TestMonth)));
  WriteLn('Test05_Month:Finished');
end;

procedure TDateBasicsTests.Test06_Day;
var
  TestDay: Integer;
  TestDate: TDateTime;
begin
  WriteLn('Test06_Day:Starting');
  TestDay := 15;
  TestDate := Now;
  AssertEquals('Day getter should return current day',
    DayOf(TestDate), TChronoKit.GetDay(TestDate));
  AssertEquals('Day setter should set specified day',
    TestDay, TChronoKit.GetDay(TChronoKit.SetDay(TestDate, TestDay)));
  WriteLn('Test06_Day:Finished');
end;

procedure TDateBasicsTests.Test07_Hour;
var
  TestHour: Integer;
  TestDate: TDateTime;
begin
  WriteLn('Test07_Hour:Starting');
  TestHour := 14;
  TestDate := Now;
  AssertEquals('Hour getter should return current hour',
    HourOf(TestDate), TChronoKit.GetHour(TestDate));
  AssertEquals('Hour setter should set specified hour',
    TestHour, TChronoKit.GetHour(TChronoKit.SetHour(TestDate, TestHour)));
  WriteLn('Test07_Hour:Finished');
end;

procedure TDateBasicsTests.Test08_Minute;
var
  TestMinute: Integer;
  TestDate: TDateTime;
begin
  WriteLn('Test08_Minute:Starting');
  TestMinute := 30;
  TestDate := Now;
  AssertEquals('Minute getter should return current minute',
    MinuteOf(TestDate), TChronoKit.GetMinute(TestDate));
  AssertEquals('Minute setter should set specified minute',
    TestMinute, TChronoKit.GetMinute(TChronoKit.SetMinute(TestDate, TestMinute)));
  WriteLn('Test08_Minute:Finished');
end;

procedure TDateBasicsTests.Test09_Second;
var
  TestSecond: Integer;
  TestDate: TDateTime;
begin
  WriteLn('Test09_Second:Starting');
  TestSecond := 45;
  TestDate := Now;
  AssertEquals('Second getter should return current second',
    SecondOf(TestDate), TChronoKit.GetSecond(TestDate));
  AssertEquals('Second setter should set specified second',
    TestSecond, TChronoKit.GetSecond(TChronoKit.SetSecond(TestDate, TestSecond)));
  WriteLn('Test09_Second:Finished');
end;

procedure TDateBasicsTests.Test10_Millisecond;
var
  TestMillisecond: Integer;
  TestDate: TDateTime;
begin
  WriteLn('Test10_Millisecond:Starting');
  TestMillisecond := 500;
  TestDate := Now;
  AssertEquals('Millisecond getter should return current millisecond',
    MilliSecondOf(TestDate), TChronoKit.GetMillisecond(TestDate));
  AssertEquals('Millisecond setter should set specified millisecond',
    TestMillisecond, TChronoKit.GetMillisecond(TChronoKit.SetMilliSecond(TestDate, TestMillisecond)));
  WriteLn('Test10_Millisecond:Finished');
end;

procedure TDateBasicsTests.Test11_AddYears;
var
  StartDate, Expected: TDateTime;
begin
  WriteLn('Test11_AddYears:Starting');
  // Regular year transition
  StartDate := EncodeDate(2024, 1, 15);
  Expected := EncodeDate(2028, 1, 15);
  AssertEquals('AddYears should add specified years',
    Expected, TChronoKit.AddYears(StartDate, 4));

  // Feb 29 leap year to non-leap year
  StartDate := EncodeDate(2024, 2, 29);  // 2024 is leap year
  Expected := EncodeDate(2025, 2, 28);   // 2025 is not
  AssertEquals('AddYears should handle Feb 29 to non-leap year',
    Expected, TChronoKit.AddYears(StartDate, 1));

  // Feb 29 leap year to leap year
  StartDate := EncodeDate(2024, 2, 29);  // 2024 is leap year
  Expected := EncodeDate(2028, 2, 29);   // 2028 is also leap year
  AssertEquals('AddYears should preserve Feb 29 in leap year',
    Expected, TChronoKit.AddYears(StartDate, 4));
  WriteLn('Test11_AddYears:Finished');
end;

procedure TDateBasicsTests.Test12_AddMonths;
var
  StartDate, Expected: TDateTime;
begin
  WriteLn('Test12_AddMonths:Starting');
  StartDate := EncodeDate(2024, 1, 15);
  Expected := EncodeDate(2024, 3, 15);
  AssertEquals('AddMonths should add specified months',
    Expected, TChronoKit.AddMonths(StartDate, 2));
  WriteLn('Test12_AddMonths:Finished');
end;

procedure TDateBasicsTests.Test13_AddDays;
var
  StartDate, Expected: TDateTime;
begin
  WriteLn('Test13_AddDays:Starting');
  StartDate := EncodeDate(2024, 1, 15);
  Expected := EncodeDate(2024, 1, 17);
  AssertEquals('AddDays should add specified days',
    Expected, TChronoKit.AddDays(StartDate, 2));
  WriteLn('Test13_AddDays:Finished');
end;

procedure TDateBasicsTests.Test14_AddHours;
var
  StartDate, Expected: TDateTime;
begin
  WriteLn('Test14_AddHours:Starting');
  // Regular hour addition
  StartDate := EncodeDate(2024, 1, 15) + EncodeTime(12, 0, 0, 0);
  Expected := EncodeDate(2024, 1, 15) + EncodeTime(14, 0, 0, 0);
  AssertEquals('AddHours should add specified hours',
    Expected, TChronoKit.AddHours(StartDate, 2));

  // Cross day boundary
  StartDate := EncodeDate(2024, 1, 15) + EncodeTime(23, 0, 0, 0);
  Expected := EncodeDate(2024, 1, 16) + EncodeTime(1, 0, 0, 0);
  AssertEquals('AddHours should handle day boundary',
    Expected, TChronoKit.AddHours(StartDate, 2));

  // Cross month boundary
  StartDate := EncodeDate(2024, 1, 31) + EncodeTime(23, 0, 0, 0);
  Expected := EncodeDate(2024, 2, 1) + EncodeTime(1, 0, 0, 0);
  AssertEquals('AddHours should handle month boundary',
    Expected, TChronoKit.AddHours(StartDate, 2));

  // Cross year and handle leap year
  StartDate := EncodeDate(2024, 2, 28) + EncodeTime(23, 0, 0, 0);
  Expected := EncodeDate(2024, 2, 29) + EncodeTime(1, 0, 0, 0);
  AssertEquals('AddHours should handle leap year boundary',
    Expected, TChronoKit.AddHours(StartDate, 2));
  WriteLn('Test14_AddHours:Finished');
end;

procedure TDateBasicsTests.Test15_AddMinutes;
var
  StartDate, Expected: TDateTime;
begin
  WriteLn('Test15_AddMinutes:Starting');
  StartDate := EncodeDate(2024, 1, 15) + EncodeTime(12, 0, 0, 0);
  Expected := EncodeDate(2024, 1, 15) + EncodeTime(12, 30, 0, 0);
  AssertEquals('AddMinutes should add specified minutes',
    Expected, TChronoKit.AddMinutes(StartDate, 30));
  WriteLn('Test15_AddMinutes:Finished');
end;

procedure TDateBasicsTests.Test16_AddSeconds;
var
  StartDate, Expected: TDateTime;
begin
  WriteLn('Test16_AddSeconds:Starting');
  StartDate := EncodeDate(2024, 1, 15) + EncodeTime(12, 0, 0, 0);
  Expected := EncodeDate(2024, 1, 15) + EncodeTime(12, 0, 30, 0);
  AssertEquals('AddSeconds should add specified seconds',
    Expected, TChronoKit.AddSeconds(StartDate, 30));
  WriteLn('Test16_AddSeconds:Finished');
end;

procedure TDateBasicsTests.Test17_StartOfYear;
var
  TestDate, Expected: TDateTime;
begin
  WriteLn('Test17_StartOfYear:Starting');
  TestDate := EncodeDate(2024, 6, 15) + EncodeTime(12, 30, 45, 500);
  Expected := EncodeDate(2024, 1, 1);
  AssertEquals('StartOfYear should set to start of year',
    Expected, TChronoKit.StartOfYear(TestDate));
  WriteLn('Test17_StartOfYear:Finished');
end;

procedure TDateBasicsTests.Test18_StartOfMonth;
var
  TestDate, Expected: TDateTime;
begin
  WriteLn('Test18_StartOfMonth:Starting');
  TestDate := EncodeDate(2024, 6, 15) + EncodeTime(12, 30, 45, 500);
  Expected := EncodeDate(2024, 6, 1);
  AssertEquals('StartOfMonth should set to start of month',
    Expected, TChronoKit.StartOfMonth(TestDate));
  WriteLn('Test18_StartOfMonth:Finished');
end;

procedure TDateBasicsTests.Test19_StartOfDay;
var
  TestDate, Expected: TDateTime;
begin
  WriteLn('Test19_StartOfDay:Starting');
  TestDate := EncodeDate(2024, 6, 15) + EncodeTime(12, 30, 45, 500);
  Expected := EncodeDate(2024, 6, 15);
  AssertEquals('StartOfDay should set to start of day',
    Expected, TChronoKit.StartOfDay(TestDate));
  WriteLn('Test19_StartOfDay:Finished');
end;

procedure TDateBasicsTests.Test20_EndOfYear;
var
  TestDate, Expected: TDateTime;
begin
  WriteLn('Test20_EndOfYear:Starting');
  TestDate := EncodeDate(2024, 6, 15) + EncodeTime(12, 30, 45, 500);
  Expected := EncodeDate(2024, 12, 31) + EncodeTime(23, 59, 59, 999);
  AssertEquals('EndOfYear should set to end of year',
    Expected, TChronoKit.EndOfYear(TestDate));
  WriteLn('Test20_EndOfYear:Finished');
end;

procedure TDateBasicsTests.Test21_EndOfMonth;
var
  TestDate, Expected: TDateTime;
begin
  WriteLn('Test21_EndOfMonth:Starting');
  TestDate := EncodeDate(2024, 6, 15) + EncodeTime(12, 30, 45, 500);
  Expected := EncodeDate(2024, 6, 30) + EncodeTime(23, 59, 59, 999);
  AssertEquals('EndOfMonth should set to end of month',
    Expected, TChronoKit.EndOfMonth(TestDate));
  WriteLn('Test21_EndOfMonth:Finished');
end;

procedure TDateBasicsTests.Test22_EndOfDay;
var
  TestDate, Expected: TDateTime;
begin
  WriteLn('Test22_EndOfDay:Starting');
  TestDate := EncodeDate(2024, 6, 15) + EncodeTime(12, 30, 45, 500);
  Expected := EncodeDate(2024, 6, 15) + EncodeTime(23, 59, 59, 999);
  AssertEquals('EndOfDay should set to end of day',
    Expected, TChronoKit.EndOfDay(TestDate));
  WriteLn('Test22_EndOfDay:Finished');
end;

procedure TDateBasicsTests.Test23_IsBefore;
var
  Date1, Date2: TDateTime;
begin
  WriteLn('Test23_IsBefore:Starting');
  Date1 := EncodeDate(2024, 1, 15);
  Date2 := EncodeDate(2024, 1, 16);
  AssertTrue('IsBefore should work correctly',
    TChronoKit.IsBefore(Date1, Date2));
  WriteLn('Test23_IsBefore:Finished');
end;

procedure TDateBasicsTests.Test24_IsAfter;
var
  Date1, Date2: TDateTime;
begin
  WriteLn('Test24_IsAfter:Starting');
  Date1 := EncodeDate(2024, 1, 16);
  Date2 := EncodeDate(2024, 1, 15);
  AssertTrue('IsAfter should work correctly',
    TChronoKit.IsAfter(Date1, Date2));
  WriteLn('Test24_IsAfter:Finished');
end;

procedure TDateBasicsTests.Test25_IsSameDay;
var
  Date1, Date2: TDateTime;
begin
  WriteLn('Test25_IsSameDay:Starting');
  Date1 := EncodeDate(2024, 1, 15) + EncodeTime(10, 0, 0, 0);
  Date2 := EncodeDate(2024, 1, 15) + EncodeTime(14, 30, 0, 0);
  AssertTrue('IsSameDay should work correctly',
    TChronoKit.IsSameDay(Date1, Date2));
  WriteLn('Test25_IsSameDay:Finished');
end;

procedure TDateBasicsTests.Test26_IsSameMonth;
var
  Date1, Date2: TDateTime;
begin
  WriteLn('Test26_IsSameMonth:Starting');
  Date1 := EncodeDate(2024, 1, 15);
  Date2 := EncodeDate(2024, 1, 20);
  AssertTrue('IsSameMonth should work correctly',
    TChronoKit.IsSameMonth(Date1, Date2));
  WriteLn('Test26_IsSameMonth:Finished');
end;

procedure TDateBasicsTests.Test27_IsSameYear;
var
  Date1, Date2: TDateTime;
begin
  WriteLn('Test27_IsSameYear:Starting');
  Date1 := EncodeDate(2024, 1, 15);
  Date2 := EncodeDate(2024, 6, 15);
  AssertTrue('IsSameYear should work correctly',
    TChronoKit.IsSameYear(Date1, Date2));
  WriteLn('Test27_IsSameYear:Finished');
end;

initialization
  RegisterTest(TDateBasicsTests);

end.
