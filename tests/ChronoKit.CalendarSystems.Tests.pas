unit ChronoKit.CalendarSystems.Tests;

{$mode objfpc}{$H+}{$J-}
{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  Classes, SysUtils, DateUtils, fpcunit, testregistry,
  ChronoKit;

type
  TCalendarSystemsTests = class(TTestCase)
  published
    procedure Test84_ISOYear;
    procedure Test85_ISOWeek;
    procedure Test88_Semester;
    procedure Test153_DecimalDateRoundTripPreservesTime;
    procedure Test157_DecimalYearReplacementRoundTrip;
    procedure Test165_StartOfQuarterValidatesInputs;
    procedure Test167_QuarterValueBoundaries;
  end;

implementation

procedure TCalendarSystemsTests.Test84_ISOYear;
var
  TestDate: TDateTime;
begin
  WriteLn('Test84_ISOYear:Starting');
  // Test regular date
  TestDate := EncodeDate(2024, 6, 15);
  AssertEquals('Regular date should have same ISO year',
    2024, TChronoKit.GetISOYear(TestDate));

  // Test year boundary (Dec 31, 2024 is in week 1 of 2025)
  TestDate := EncodeDate(2024, 12, 31);
  AssertEquals('Dec 31 can belong to next ISO year',
    2025, TChronoKit.GetISOYear(TestDate));

  // Test year boundary (Jan 1, 2024 is in week 52 of 2023)
  TestDate := EncodeDate(2024, 1, 1);
  AssertEquals('Jan 1 can belong to previous ISO year',
    2023, TChronoKit.GetISOYear(TestDate));
  WriteLn('Test84_ISOYear:Finished');
end;

procedure TCalendarSystemsTests.Test85_ISOWeek;
var
  TestDate: TDateTime;
begin
  WriteLn('Test85_ISOWeek:Starting');
  // Test regular week
  TestDate := EncodeDate(2024, 6, 15);
  AssertEquals('Mid-year date should have correct ISO week',
    24, TChronoKit.GetISOWeek(TestDate));

  // Test first week of year
  TestDate := EncodeDate(2024, 1, 4);
  AssertEquals('January 4th is always in week 1',
    1, TChronoKit.GetISOWeek(TestDate));

  // Test last week of year
  TestDate := EncodeDate(2024, 12, 28);
  AssertEquals('December 28th should be in week 52',
    52, TChronoKit.GetISOWeek(TestDate));
  WriteLn('Test85_ISOWeek:Finished');
end;

procedure TCalendarSystemsTests.Test88_Semester;
var
  TestDate: TDateTime;
begin
  WriteLn('Test88_Semester:Starting');
  // Test first semester
  TestDate := EncodeDate(2024, 1, 1);
  AssertEquals('January should be semester 1',
    1, TChronoKit.GetSemester(TestDate));

  TestDate := EncodeDate(2024, 6, 30);
  AssertEquals('June should be semester 1',
    1, TChronoKit.GetSemester(TestDate));

  // Test second semester
  TestDate := EncodeDate(2024, 7, 1);
  AssertEquals('July should be semester 2',
    2, TChronoKit.GetSemester(TestDate));

  TestDate := EncodeDate(2024, 12, 31);
  AssertEquals('December should be semester 2',
    2, TChronoKit.GetSemester(TestDate));
  WriteLn('Test88_Semester:Finished');
end;

procedure TCalendarSystemsTests.Test153_DecimalDateRoundTripPreservesTime;
var
  OriginalValue, RoundTrippedValue: TDateTime;
begin
  OriginalValue := EncodeDateTime(2024, 7, 2, 12, 34, 56, 789);
  RoundTrippedValue := TChronoKit.DateDecimal(
    TChronoKit.GetDecimalDate(OriginalValue));
  AssertEquals('Decimal-year legacy pair must round-trip time of day',
    OriginalValue, RoundTrippedValue, OneMillisecond);
end;

procedure TCalendarSystemsTests.Test157_DecimalYearReplacementRoundTrip;
var
  CommonValue, LeapValue: TDateTime;
begin
  CommonValue := EncodeDateTime(2025, 4, 2, 6, 7, 8, 901);
  AssertEquals('Common-year replacement pair must round-trip',
    CommonValue,
    TChronoKit.DecimalYearToDateTime(
      TChronoKit.DateTimeToDecimalYear(CommonValue)), OneMillisecond);

  LeapValue := EncodeDateTime(2024, 2, 29, 23, 59, 59, 999);
  AssertEquals('Leap-year replacement pair must round-trip',
    LeapValue,
    TChronoKit.DecimalYearToDateTime(
      TChronoKit.DateTimeToDecimalYear(LeapValue)), OneMillisecond);
end;

procedure TCalendarSystemsTests.Test165_StartOfQuarterValidatesInputs;
var
  Raised: Boolean;
begin
  AssertEquals('Quarter 3 starts on July 1', EncodeDate(2024, 7, 1),
    TChronoKit.StartOfQuarter(2024, 3));

  Raised := False;
  try
    TChronoKit.StartOfQuarter(0, 1);
  except
    on E: EArgumentException do
      Raised := Pos('year', LowerCase(E.Message)) > 0;
  end;
  AssertTrue('StartOfQuarter must reject year zero', Raised);

  Raised := False;
  try
    TChronoKit.StartOfQuarter(2024, 5);
  except
    on E: EArgumentException do
      Raised := Pos('quarter', LowerCase(E.Message)) > 0;
  end;
  AssertTrue('StartOfQuarter must reject quarter five', Raised);
end;

procedure TCalendarSystemsTests.Test167_QuarterValueBoundaries;
var
  LeapDay, FourthQuarterValue: TDateTime;
begin
  LeapDay := EncodeDateTime(2024, 2, 29, 12, 34, 56, 789);
  AssertEquals('A value in Q1 starts at the first day of the year',
    EncodeDate(2024, 1, 1), TChronoKit.StartOfQuarter(LeapDay));
  AssertEquals('A leap-year Q1 ends at its last millisecond',
    EncodeDateTime(2024, 3, 31, 23, 59, 59, 999),
    TChronoKit.EndOfQuarter(LeapDay), OneMillisecond);

  FourthQuarterValue := EncodeDateTime(2024, 12, 31, 0, 0, 0, 1);
  AssertEquals('A Q4 value starts on October 1', EncodeDate(2024, 10, 1),
    TChronoKit.StartOfQuarter(FourthQuarterValue));
  AssertEquals('A Q4 value ends in the same calendar year',
    EncodeDateTime(2024, 12, 31, 23, 59, 59, 999),
    TChronoKit.EndOfQuarter(FourthQuarterValue), OneMillisecond);
  AssertEquals('The millisecond after Q4 end starts the next calendar year',
    EncodeDate(2025, 1, 1), TChronoKit.AddDuration(
      TChronoKit.EndOfQuarter(FourthQuarterValue),
      TChronoKit.DurationFromParts(0, 0, 0, 0, 1)), OneMillisecond);
end;

initialization
  RegisterTest(TCalendarSystemsTests);

end.
