unit ChronoKit.LegacyBehavior.Tests;

{$mode objfpc}{$H+}{$J-}
{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  Classes, SysUtils, DateUtils, fpcunit, testregistry,
  ChronoKit;

type
  TLegacyBehaviorTests = class(TTestCase)
  published
    procedure Test03_From;
    procedure Test28_ToDateTime;
    procedure Test29_ToString;
    procedure Test144_FromStringValidationMessage;
    procedure Test146_FormatDateTimeAlias;
    procedure Test147_ParseDateTimeAlias;
    procedure Test34_CreatePeriod;
    procedure Test35_CreateDuration;
    procedure Test36_CreateInterval;
    procedure Test37_AddSpan;
    procedure Test38_SubtractSpan;
    procedure Test39_SpanBetween;
    procedure Test40_IsWithinInterval;
    procedure Test41_IntervalsOverlap;
    procedure Test42_IntervalLength;
    procedure Test43_PeriodNormalization;
    procedure Test44_DurationCalculation;
    procedure Test45_SpanCornerCases;
    procedure Test76_YMD_Valid;
    procedure Test77_YMD_Invalid;
    procedure Test78_MDY_Valid;
    procedure Test79_MDY_Invalid;
    procedure Test80_DMY_Valid;
    procedure Test81_DMY_Invalid;
    procedure Test82_YQ_Valid;
    procedure Test83_YQ_Invalid;
    procedure Test86_EpiYear;
    procedure Test87_EpiWeek;
    procedure Test89_DateDecimal;
    procedure Test90_GetDecimalDate;
    procedure Test91_RollbackMonth;
    procedure Test92_RollForwardMonth;
    procedure Test93_PeriodToSeconds;
    procedure Test94_SecondsToPeriod;
    procedure Test95_StandardizePeriod;
    procedure Test96_IntervalAlign;
    procedure Test97_IntervalGap;
    procedure Test98_IntervalSetdiff;
    procedure Test99_IntervalUnion;
    procedure Test100_IntervalIntersection;
    procedure Test87a_EpiWeek_MidYear;
    procedure Test87b_EpiWeek_FirstWeek;
    procedure Test87c_EpiWeek_YearEnd;
    procedure Test95a_StandardizePeriod_Milliseconds;
    procedure Test95b_StandardizePeriod_Seconds;
    procedure Test95c_StandardizePeriod_Minutes;
    procedure Test95d_StandardizePeriod_Hours;
    procedure Test95e_StandardizePeriod_Months;
    procedure Test95f_StandardizePeriod_Complex;
    procedure Test97a_IntervalGap_NoOverlap;
    procedure Test97b_IntervalGap_Overlapping;
    procedure Test116_YMD;
    procedure Test117_MDY;
    procedure Test118_DMY;
    procedure Test151_DurationSpanDoesNotDoubleCountMilliseconds;
    procedure Test152_IntervalGapPreservesSubDayPrecision;
    procedure Test154_CreateIntervalRejectsReverseOrder;
    procedure Test155_SeasonRoundingRaises;
    procedure Test156_MonthRollingMatchesAddMonths;
  end;

implementation

procedure TLegacyBehaviorTests.Test03_From;
var
  TestDate: TDateTime;
begin
  WriteLn('Test03_From:Starting');
  TestDate := EncodeDate(2024, 1, 15);
  AssertEquals('GetDateTime should return the correct date',
    TestDate, TChronoKit.GetDateTime(TestDate));
  WriteLn('Test03_From:Finished');
end;

procedure TLegacyBehaviorTests.Test28_ToDateTime;
var
  TestDate: TDateTime;
begin
  WriteLn('Test28_ToDateTime:Starting');
  TestDate := EncodeDate(2024, 1, 15);
  AssertEquals('GetDateTime should return the correct date',
    TestDate, TChronoKit.GetDateTime(TestDate));
  WriteLn('Test28_ToDateTime:Finished');
end;

procedure TLegacyBehaviorTests.Test29_ToString;
var
  TestDate: TDateTime;
begin
  WriteLn('Test29_ToString:Starting');
  TestDate := EncodeDate(2024, 1, 15);
  AssertEquals('GetAsString should return the correct string',
    FormatDateTime('dd/mm/yyyy', TestDate),
    TChronoKit.GetAsString(TestDate, 'dd/mm/yyyy'));
  WriteLn('Test29_ToString:Finished');
end;

// Add new tests for business day functions

procedure TLegacyBehaviorTests.Test144_FromStringValidationMessage;
begin
  WriteLn('Test144_FromStringValidationMessage:Starting');
  try
    TChronoKit.FromString('not-a-date');
    Fail('FromString should reject invalid date/time input');
  except
    on E: EConvertError do
    begin
      AssertTrue('FromString error should include the rejected input',
        Pos('not-a-date', E.Message) > 0);
      AssertTrue('FromString error should explain the expected input',
        Pos('system date/time format', E.Message) > 0);
    end;
  end;
  WriteLn('Test144_FromStringValidationMessage:Finished');
end;

procedure TLegacyBehaviorTests.Test146_FormatDateTimeAlias;
var
  TestDate: TDateTime;
begin
  WriteLn('Test146_FormatDateTimeAlias:Starting');
  TestDate := EncodeDateTime(2026, 8, 11, 14, 5, 9, 0);
  AssertEquals('FormatDateTime should produce the requested representation',
    '2026-08-11 14:05:09',
    TChronoKit.FormatDateTime(TestDate, 'yyyy-mm-dd hh:nn:ss'));
  AssertEquals('FormatDateTime should preserve the compatibility behavior',
    TChronoKit.GetAsString(TestDate, 'yyyy-mm-dd hh:nn:ss'),
    TChronoKit.FormatDateTime(TestDate, 'yyyy-mm-dd hh:nn:ss'));
  WriteLn('Test146_FormatDateTimeAlias:Finished');
end;

procedure TLegacyBehaviorTests.Test147_ParseDateTimeAlias;
const
  InputValue = '2026-08-11 14:05:09';
  InputFormat = 'yyyy-mm-dd hh:nn:ss';
begin
  WriteLn('Test147_ParseDateTimeAlias:Starting');
  AssertEquals('ParseDateTime should produce the requested date/time',
    EncodeDateTime(2026, 8, 11, 14, 5, 9, 0),
    TChronoKit.ParseDateTime(InputValue, InputFormat));
  AssertEquals('ParseDateTime should preserve the compatibility behavior',
    TChronoKit.FromString(InputValue, InputFormat),
    TChronoKit.ParseDateTime(InputValue, InputFormat));
  WriteLn('Test147_ParseDateTimeAlias:Finished');
end;

procedure TLegacyBehaviorTests.Test34_CreatePeriod;
var
  Period: TDateSpan;
begin
  WriteLn('Test34_CreatePeriod:Starting');
  // Test creating a period with various components
  Period := TChronoKit.CreatePeriod(1, 2, 3, 4, 5, 6, 7);

  AssertEquals('Period kind should be dskPeriod', Ord(TDateSpanKind.dskPeriod), Ord(Period.Kind));
  AssertEquals('Years should match', 1, Period.Years);
  AssertEquals('Months should match', 2, Period.Months);
  AssertEquals('Days should match', 3, Period.Days);
  AssertEquals('Hours should match', 4, Period.Hours);
  AssertEquals('Minutes should match', 5, Period.Minutes);
  AssertEquals('Seconds should match', 6, Period.Seconds);
  AssertEquals('Milliseconds should match', 7, Period.Milliseconds);
  WriteLn('Test34_CreatePeriod:Finished');
end;

procedure TLegacyBehaviorTests.Test35_CreateDuration;
var
  Duration: TDateSpan;
begin
  WriteLn('Test35_CreateDuration:Starting');
  // Test creating a duration (converts to total seconds)
  Duration := TChronoKit.CreateDuration(0, 0, 1, 2, 30, 0, 0);  // 1 day, 2 hours, 30 minutes

  AssertEquals('Duration kind should be dskDuration', Ord(TDateSpanKind.dskDuration), Ord(Duration.Kind));
  AssertEquals('Total seconds should be calculated correctly',
    ((24 + 2) * 60 + 30) * 60,  // (26 hours + 30 minutes) in seconds
    Duration.Seconds);
  WriteLn('Test35_CreateDuration:Finished');
end;

procedure TLegacyBehaviorTests.Test36_CreateInterval;
var
  StartDate, EndDate: TDateTime;
  Interval: TInterval;
begin
  WriteLn('Test36_CreateInterval:Starting');
  StartDate := EncodeDate(2024, 1, 1);
  EndDate := EncodeDate(2024, 12, 31);

  Interval := TChronoKit.CreateInterval(StartDate, EndDate);

  AssertEquals('Interval start date should match', StartDate, Interval.StartDate);
  AssertEquals('Interval end date should match', EndDate, Interval.EndDate);
  WriteLn('Test36_CreateInterval:Finished');
end;

procedure TLegacyBehaviorTests.Test37_AddSpan;
var
  StartDate, ResultDate: TDateTime;
  Period: TDateSpan;
begin
  WriteLn('Test37_AddSpan:Starting');
  StartDate := EncodeDate(2024, 1, 1);

  // Test adding a period
  Period := TChronoKit.CreatePeriod(1, 2, 3);  // 1 year, 2 months, 3 days
  ResultDate := TChronoKit.AddSpan(StartDate, Period);

  AssertEquals('Year should be incremented', 2025, TChronoKit.GetYear(ResultDate));
  AssertEquals('Month should be March', 3, TChronoKit.GetMonth(ResultDate));
  AssertEquals('Day should be 4th', 4, TChronoKit.GetDay(ResultDate));
  WriteLn('Test37_AddSpan:Finished');
end;

procedure TLegacyBehaviorTests.Test38_SubtractSpan;
var
  StartDate, ResultDate: TDateTime;
  Period: TDateSpan;
begin
  WriteLn('Test38_SubtractSpan:Starting');
  StartDate := EncodeDate(2024, 3, 15);

  // Test subtracting a period
  Period := TChronoKit.CreatePeriod(0, 1, 10);  // 1 month, 10 days
  ResultDate := TChronoKit.SubtractSpan(StartDate, Period);

  AssertEquals('Month should be February', 2, TChronoKit.GetMonth(ResultDate));
  AssertEquals('Day should be 5th', 5, TChronoKit.GetDay(ResultDate));
  WriteLn('Test38_SubtractSpan:Finished');
end;

procedure TLegacyBehaviorTests.Test39_SpanBetween;
var
  StartDate, EndDate: TDateTime;
  Span: TDateSpan;
begin
  WriteLn('Test39_SpanBetween:Starting');
  StartDate := EncodeDate(2024, 1, 1);
  EndDate := EncodeDate(2025, 2, 15);

  // Test calculating period between dates
  Span := TChronoKit.SpanBetween(StartDate, EndDate);

  AssertEquals('Span kind should be period', Ord(TDateSpanKind.dskPeriod), Ord(Span.Kind));
  AssertEquals('Years should be 1', 1, Span.Years);
  AssertEquals('Months should be 1', 1, Span.Months);
  AssertEquals('Days should be 14', 14, Span.Days);
  WriteLn('Test39_SpanBetween:Finished');
end;

procedure TLegacyBehaviorTests.Test40_IsWithinInterval;
var
  StartDate, EndDate, TestDate: TDateTime;
  Interval: TInterval;
begin
  WriteLn('Test40_IsWithinInterval:Starting');
  StartDate := EncodeDate(2024, 1, 1);
  EndDate := EncodeDate(2024, 12, 31);
  TestDate := EncodeDate(2024, 6, 15);

  Interval := TChronoKit.CreateInterval(StartDate, EndDate);

  AssertTrue('Date should be within interval',
    TChronoKit.IsWithinInterval(TestDate, Interval));
  AssertFalse('Date before interval should not be within interval',
    TChronoKit.IsWithinInterval(EncodeDate(2023, 12, 31), Interval));
  AssertFalse('Date after interval should not be within interval',
    TChronoKit.IsWithinInterval(EncodeDate(2025, 1, 1), Interval));
  WriteLn('Test40_IsWithinInterval:Finished');
end;

procedure TLegacyBehaviorTests.Test41_IntervalsOverlap;
var
  Interval1, Interval2: TInterval;
begin
  WriteLn('Test41_IntervalsOverlap:Starting');
  // Create two overlapping intervals
  Interval1 := TChronoKit.CreateInterval(
    EncodeDate(2024, 1, 1),
    EncodeDate(2024, 6, 30));
  Interval2 := TChronoKit.CreateInterval(
    EncodeDate(2024, 6, 1),
    EncodeDate(2024, 12, 31));

  AssertTrue('Overlapping intervals should be detected',
    TChronoKit.IntervalsOverlap(Interval1, Interval2));

  // Create non-overlapping intervals
  Interval2.StartDate := EncodeDate(2024, 7, 1);
  AssertFalse('Non-overlapping intervals should be detected',
    TChronoKit.IntervalsOverlap(Interval1, Interval2));
  WriteLn('Test41_IntervalsOverlap:Finished');
end;

procedure TLegacyBehaviorTests.Test42_IntervalLength;
var
  Interval: TInterval;
  Span: TDateSpan;
begin
  WriteLn('Test42_IntervalLength:Starting');
  // Create an interval for exactly one year
  Interval.StartDate := EncodeDate(2024, 1, 1);  // 2024-01-01 00:00:00.000
  Interval.EndDate := EncodeDate(2025, 1, 1);    // 2025-01-01 00:00:00.000

  // Test period length
  Span := TChronoKit.IntervalLength(Interval, dskPeriod);
  AssertEquals('Interval length should be 1 year', 1, Span.Years);
  AssertEquals('No remaining months', 0, Span.Months);
  AssertEquals('No remaining days', 0, Span.Days);

  // Test duration length (366 days for leap year 2024)
  Span := TChronoKit.IntervalLength(Interval, dskDuration);
  AssertEquals('Duration should be calculated in seconds',
    366 * 24 * 60 * 60,  // Full leap year 2024
    Span.Seconds);
  WriteLn('Test42_IntervalLength:Finished');
end;

procedure TLegacyBehaviorTests.Test43_PeriodNormalization;
var
  Period: TDateSpan;
  StartDate, ResultDate: TDateTime;
begin
  WriteLn('Test43_PeriodNormalization:Starting');
  // Test period normalization (13 months should become 1 year 1 month)
  Period := TChronoKit.CreatePeriod(0, 13, 0);
  StartDate := EncodeDate(2024, 1, 1);
  ResultDate := TChronoKit.AddSpan(StartDate, Period);

  AssertEquals('Year should be incremented', 2025, TChronoKit.GetYear(ResultDate));
  AssertEquals('Month should be February', 2, TChronoKit.GetMonth(ResultDate));
  WriteLn('Test43_PeriodNormalization:Finished');
end;

procedure TLegacyBehaviorTests.Test44_DurationCalculation;
var
  Duration: TDateSpan;
  StartDate, ResultDate: TDateTime;
begin
  WriteLn('Test44_DurationCalculation:Starting');
  // Test precise duration calculations
  Duration := TChronoKit.CreateDuration(0, 0, 0, 25, 0, 0, 0);  // 25 hours
  StartDate := EncodeDate(2024, 1, 1) + EncodeTime(12, 0, 0, 0);  // Noon
  ResultDate := TChronoKit.AddSpan(StartDate, Duration);

  AssertEquals('Day should be incremented', 2, TChronoKit.GetDay(ResultDate));
  AssertEquals('Hour should be 13', 13, TChronoKit.GetHour(ResultDate));
  WriteLn('Test44_DurationCalculation:Finished');
end;

procedure TLegacyBehaviorTests.Test45_SpanCornerCases;
var
  Period: TDateSpan;
  StartDate, ResultDate: TDateTime;
begin
  WriteLn('Test45_SpanCornerCases:Starting');
  // Test adding one month to January 31st (should go to last day of February)
  Period := TChronoKit.CreatePeriod(0, 1, 0);
  StartDate := EncodeDate(2024, 1, 31);
  ResultDate := TChronoKit.AddSpan(StartDate, Period);

  AssertEquals('Month should be February', 2, TChronoKit.GetMonth(ResultDate));
  AssertEquals('Day should be adjusted to 29 (leap year)',
    29, TChronoKit.GetDay(ResultDate));

  // Test adding one month to January 31st in non-leap year (should go to February 28)
  StartDate := EncodeDate(2025, 1, 31);
  ResultDate := TChronoKit.AddSpan(StartDate, Period);

  AssertEquals('Month should be February', 2, TChronoKit.GetMonth(ResultDate));
  AssertEquals('Day should be adjusted to 28 (non-leap year)',
    28, TChronoKit.GetDay(ResultDate));

  // Test adding one year to February 29th in leap year (should go to February 28)
  Period := TChronoKit.CreatePeriod(1, 0, 0);
  StartDate := EncodeDate(2024, 2, 29);
  ResultDate := TChronoKit.AddSpan(StartDate, Period);

  AssertEquals('Year should be incremented', 2025, TChronoKit.GetYear(ResultDate));
  AssertEquals('Day should be adjusted to 28 (non-leap year)',
    28, TChronoKit.GetDay(ResultDate));

  // Test adding two years to February 29th (should go back to February 29)
  Period := TChronoKit.CreatePeriod(2, 0, 0);
  StartDate := EncodeDate(2024, 2, 29);
  ResultDate := TChronoKit.AddSpan(StartDate, Period);

  AssertEquals('Year should be incremented by 2', 2026, TChronoKit.GetYear(ResultDate));
  AssertEquals('Day should be adjusted to 28 (non-leap year)',
    28, TChronoKit.GetDay(ResultDate));

  // Test adding one month to March 31st (should go to April 30)
  Period := TChronoKit.CreatePeriod(0, 1, 0);
  StartDate := EncodeDate(2024, 3, 31);
  ResultDate := TChronoKit.AddSpan(StartDate, Period);

  AssertEquals('Month should be April', 4, TChronoKit.GetMonth(ResultDate));
  AssertEquals('Day should be adjusted to 30',
    30, TChronoKit.GetDay(ResultDate));
  WriteLn('Test45_SpanCornerCases:Finished');
end;

// Implementation of new test cases

procedure TLegacyBehaviorTests.Test76_YMD_Valid;
var
  TestDate: TDateTime;
begin
  WriteLn('Test76_YMD_Valid:Starting');
  // Test YYYY-MM-DD format
  TestDate := TChronoKit.YMD('2024-03-15');
  AssertEquals('YMD should parse year correctly', 2024, TChronoKit.GetYear(TestDate));
  AssertEquals('YMD should parse month correctly', 3, TChronoKit.GetMonth(TestDate));
  AssertEquals('YMD should parse day correctly', 15, TChronoKit.GetDay(TestDate));

  // Test YYYY/MM/DD format
  TestDate := TChronoKit.YMD('2024/03/15');
  AssertEquals('YMD should parse year correctly with slash', 2024, TChronoKit.GetYear(TestDate));
  AssertEquals('YMD should parse month correctly with slash', 3, TChronoKit.GetMonth(TestDate));
  AssertEquals('YMD should parse day correctly with slash', 15, TChronoKit.GetDay(TestDate));
  WriteLn('Test76_YMD_Valid:Finished');
end;

procedure TLegacyBehaviorTests.Test77_YMD_Invalid;
begin
  WriteLn('Test77_YMD_Invalid:Starting');
  try
    TChronoKit.YMD('invalid');
    Fail('YMD should raise exception for invalid format');
  except
    on E: EConvertError do
      ; // Expected exception
  end;

  try
    TChronoKit.YMD('2024-13-15');
    Fail('YMD should raise exception for invalid month');
  except
    on E: EConvertError do
      ; // Expected exception
  end;
  WriteLn('Test77_YMD_Invalid:Finished');
end;

procedure TLegacyBehaviorTests.Test78_MDY_Valid;
var
  TestDate: TDateTime;
begin
  WriteLn('Test78_MDY_Valid:Starting');
  // Test MM-DD-YYYY format
  TestDate := TChronoKit.MDY('03-15-2024');
  AssertEquals('MDY should parse year correctly', 2024, TChronoKit.GetYear(TestDate));
  AssertEquals('MDY should parse month correctly', 3, TChronoKit.GetMonth(TestDate));
  AssertEquals('MDY should parse day correctly', 15, TChronoKit.GetDay(TestDate));

  // Test MM/DD/YY format with 2-digit year
  TestDate := TChronoKit.MDY('03/15/24');
  AssertEquals('MDY should parse 2-digit year correctly', 2024, TChronoKit.GetYear(TestDate));
  AssertEquals('MDY should parse month correctly with slash', 3, TChronoKit.GetMonth(TestDate));
  AssertEquals('MDY should parse day correctly with slash', 15, TChronoKit.GetDay(TestDate));
  WriteLn('Test78_MDY_Valid:Finished');
end;

procedure TLegacyBehaviorTests.Test79_MDY_Invalid;
begin
  WriteLn('Test79_MDY_Invalid:Starting');
  try
    TChronoKit.MDY('invalid');
    Fail('MDY should raise exception for invalid format');
  except
    on E: EConvertError do
      ; // Expected exception
  end;

  try
    TChronoKit.MDY('13-15-2024');
    Fail('MDY should raise exception for invalid month');
  except
    on E: EConvertError do
      ; // Expected exception
  end;
  WriteLn('Test79_MDY_Invalid:Finished');
end;

procedure TLegacyBehaviorTests.Test80_DMY_Valid;
var
  TestDate: TDateTime;
begin
  WriteLn('Test80_DMY_Valid:Starting');
  // Test DD-MM-YYYY format
  TestDate := TChronoKit.DMY('15-03-2024');
  AssertEquals('DMY should parse year correctly', 2024, TChronoKit.GetYear(TestDate));
  AssertEquals('DMY should parse month correctly', 3, TChronoKit.GetMonth(TestDate));
  AssertEquals('DMY should parse day correctly', 15, TChronoKit.GetDay(TestDate));

  // Test DD/MM/YY format with 2-digit year
  TestDate := TChronoKit.DMY('15/03/24');
  AssertEquals('DMY should parse 2-digit year correctly', 2024, TChronoKit.GetYear(TestDate));
  AssertEquals('DMY should parse month correctly with slash', 3, TChronoKit.GetMonth(TestDate));
  AssertEquals('DMY should parse day correctly with slash', 15, TChronoKit.GetDay(TestDate));
  WriteLn('Test80_DMY_Valid:Finished');
end;

procedure TLegacyBehaviorTests.Test81_DMY_Invalid;
begin
  WriteLn('Test81_DMY_Invalid:Starting');
  try
    TChronoKit.DMY('invalid');
    Fail('DMY should raise exception for invalid format');
  except
    on E: EConvertError do
      ; // Expected exception
  end;

  try
    TChronoKit.DMY('15-13-2024');
    Fail('DMY should raise exception for invalid month');
  except
    on E: EConvertError do
      ; // Expected exception
  end;
  WriteLn('Test81_DMY_Invalid:Finished');
end;

procedure TLegacyBehaviorTests.Test82_YQ_Valid;
var
  TestDate: TDateTime;
begin
  WriteLn('Test82_YQ_Valid:Starting');
  // Test YYYY-Q format
  TestDate := TChronoKit.YQ('2024-1');
  AssertEquals('YQ should parse year correctly', 2024, TChronoKit.GetYear(TestDate));
  AssertEquals('YQ should set month to start of Q1', 1, TChronoKit.GetMonth(TestDate));
  AssertEquals('YQ should set day to first of month', 1, TChronoKit.GetDay(TestDate));

  // Test YYYY/Q format for Q2
  TestDate := TChronoKit.YQ('2024/2');
  AssertEquals('YQ should parse year correctly', 2024, TChronoKit.GetYear(TestDate));
  AssertEquals('YQ should set month to start of Q2', 4, TChronoKit.GetMonth(TestDate));
  AssertEquals('YQ should set day to first of month', 1, TChronoKit.GetDay(TestDate));

  // Test Q3
  TestDate := TChronoKit.YQ('2024-3');
  AssertEquals('YQ should set month to start of Q3', 7, TChronoKit.GetMonth(TestDate));

  // Test Q4
  TestDate := TChronoKit.YQ('2024-4');
  AssertEquals('YQ should set month to start of Q4', 10, TChronoKit.GetMonth(TestDate));
  WriteLn('Test82_YQ_Valid:Finished');
end;

procedure TLegacyBehaviorTests.Test83_YQ_Invalid;
begin
  WriteLn('Test83_YQ_Invalid:Starting');
  try
    TChronoKit.YQ('invalid');
    Fail('YQ should raise exception for invalid format');
  except
    on E: EConvertError do
      ; // Expected exception
  end;

  try
    TChronoKit.YQ('2024-5');
    Fail('YQ should raise exception for invalid quarter');
  except
    on E: EConvertError do
      ; // Expected exception
  end;
  WriteLn('Test83_YQ_Invalid:Finished');
end;

procedure TLegacyBehaviorTests.Test86_EpiYear;
var
  TestDate: TDateTime;
begin
  WriteLn('Test86_EpiYear:Starting');
  // Test regular date
  TestDate := EncodeDate(2024, 6, 15);
  AssertEquals('Regular date should have same epi year',
    2024, TChronoKit.GetEpiYear(TestDate));

  // Test year boundary (Dec 29, 2024 might be week 1 of 2025)
  TestDate := EncodeDate(2024, 12, 29);
  AssertEquals('Late December can belong to next epi year',
    2025, TChronoKit.GetEpiYear(TestDate));

  // Test year boundary (Jan 1 might be week 52 of previous year)
  TestDate := EncodeDate(2024, 1, 1);
  AssertEquals('Early January can belong to previous epi year',
    2023, TChronoKit.GetEpiYear(TestDate));
  WriteLn('Test86_EpiYear:Finished');
end;

procedure TLegacyBehaviorTests.Test87_EpiWeek;
var
  TestDate: TDateTime;
begin
  WriteLn('Test87_EpiWeek:Starting');
  // Test regular week
  TestDate := EncodeDate(2024, 6, 15);
  AssertEquals('Mid-year date should have correct epi week',
    24, TChronoKit.GetEpiWeek(TestDate));

  // Test first week of year
  TestDate := EncodeDate(2024, 1, 4);
  AssertEquals('First full week should be week 1',
    1, TChronoKit.GetEpiWeek(TestDate));

  // Test week spanning year boundary
  TestDate := EncodeDate(2024, 12, 31);
  AssertEquals('Year-end week number should be correct',
    53, TChronoKit.GetEpiWeek(TestDate));
  WriteLn('Test87_EpiWeek:Finished');
end;

procedure TLegacyBehaviorTests.Test89_DateDecimal;
var
  TestDate: TDateTime;
begin
  WriteLn('Test89_DateDecimal:Starting');
  // Test regular date
  TestDate := TChronoKit.DateDecimal(2024.5); // Mid-year
  AssertEquals('Year should be 2024', 2024, TChronoKit.GetYear(TestDate));
  AssertEquals('Halfway through a leap year should be July 2nd',
    184, TChronoKit.GetDayOfYear(TestDate));

  // Test leap year handling
  TestDate := TChronoKit.DateDecimal(2024.25); // Quarter year
  AssertEquals('Should be around April 1st in leap year',
    92, TChronoKit.GetDayOfYear(TestDate));

  // Test non-leap year
  TestDate := TChronoKit.DateDecimal(2025.25); // Quarter year
  AssertEquals('Quarter-way through a non-leap year should be April 2nd',
    92, TChronoKit.GetDayOfYear(TestDate));
  WriteLn('Test89_DateDecimal:Finished');
end;

procedure TLegacyBehaviorTests.Test90_GetDecimalDate;
var
  TestDate: TDateTime;
  DecimalDate: Double;
begin
  WriteLn('Test90_GetDecimalDate:Starting');
  // Test mid-year in leap year
  TestDate := EncodeDate(2024, 7, 2);  // Day 183 of 366
  DecimalDate := TChronoKit.GetDecimalDate(TestDate);
  AssertEquals('Mid-year 2024 should be approximately 2024.5',
    2024.5, DecimalDate, 0.01);

  // Test quarter-year in non-leap year
  TestDate := EncodeDate(2025, 4, 1);  // Day 91 of 365
  DecimalDate := TChronoKit.GetDecimalDate(TestDate);
  AssertEquals('Quarter-year 2025 should be approximately 2025.25',
    2025.25, DecimalDate, 0.01);
  WriteLn('Test90_GetDecimalDate:Finished');
end;

procedure TLegacyBehaviorTests.Test91_RollbackMonth;
var
  TestDate, RolledDate: TDateTime;
begin
  WriteLn('Test91_RollbackMonth:Starting');
  // Test regular case
  TestDate := EncodeDate(2024, 3, 15);
  RolledDate := TChronoKit.RollbackMonth(TestDate);
  AssertEquals('Month should be February', 2, TChronoKit.GetMonth(RolledDate));
  AssertEquals('Day should remain 15', 15, TChronoKit.GetDay(RolledDate));

  // Test year boundary
  TestDate := EncodeDate(2024, 1, 15);
  RolledDate := TChronoKit.RollbackMonth(TestDate);
  AssertEquals('Year should be previous', 2023, TChronoKit.GetYear(RolledDate));
  AssertEquals('Month should be December', 12, TChronoKit.GetMonth(RolledDate));

  // Test day adjustment (31 -> 29 in leap year)
  TestDate := EncodeDate(2024, 3, 31);
  RolledDate := TChronoKit.RollbackMonth(TestDate);
  AssertEquals('Day should adjust to February 29 in leap year',
    29, TChronoKit.GetDay(RolledDate));
  WriteLn('Test91_RollbackMonth:Finished');
end;

procedure TLegacyBehaviorTests.Test92_RollForwardMonth;
var
  TestDate, RolledDate: TDateTime;
begin
  WriteLn('Test92_RollForwardMonth:Starting');
  // Test regular case
  TestDate := EncodeDate(2024, 3, 15);
  RolledDate := TChronoKit.RollForwardMonth(TestDate);
  AssertEquals('Month should be April', 4, TChronoKit.GetMonth(RolledDate));
  AssertEquals('Day should remain 15', 15, TChronoKit.GetDay(RolledDate));

  // Test year boundary
  TestDate := EncodeDate(2024, 12, 15);
  RolledDate := TChronoKit.RollForwardMonth(TestDate);
  AssertEquals('Year should be next', 2025, TChronoKit.GetYear(RolledDate));
  AssertEquals('Month should be January', 1, TChronoKit.GetMonth(RolledDate));

  // Test day adjustment (31 -> 30)
  TestDate := EncodeDate(2024, 3, 31);
  RolledDate := TChronoKit.RollForwardMonth(TestDate);
  AssertEquals('Day should adjust to April 30',
    30, TChronoKit.GetDay(RolledDate));
  WriteLn('Test92_RollForwardMonth:Finished');
end;

procedure TLegacyBehaviorTests.Test93_PeriodToSeconds;
var
  Period: TDateSpan;
  Seconds: Int64;
begin
  WriteLn('Test93_PeriodToSeconds:Starting');
  // Test simple period
  Period := TChronoKit.CreatePeriod(0, 0, 1, 2, 3, 4, 500);  // 1d 2h 3m 4.5s
  Seconds := TChronoKit.PeriodToSeconds(Period);
  AssertEquals('Period should convert to correct seconds',
    93784, Seconds);  // 1*86400 + 2*3600 + 3*60 + 4

  // Test larger period
  Period := TChronoKit.CreatePeriod(1, 1, 0, 0, 0, 0, 0);  // 1y 1m
  Seconds := TChronoKit.PeriodToSeconds(Period);
  AssertEquals('Larger period should convert approximately',
    34128000, Seconds);  // ~1y 1m in seconds
  WriteLn('Test93_PeriodToSeconds:Finished');
end;

procedure TLegacyBehaviorTests.Test94_SecondsToPeriod;
var
  Period: TDateSpan;
begin
  WriteLn('Test94_SecondsToPeriod:Starting');
  // Test simple conversion
  Period := TChronoKit.SecondsToPeriod(93784);  // 1d 2h 3m 4s
  AssertEquals('Should get correct days', 1, Period.Days);
  AssertEquals('Should get correct hours', 2, Period.Hours);
  AssertEquals('Should get correct minutes', 3, Period.Minutes);
  AssertEquals('Should get correct seconds', 4, Period.Seconds);

  // Test large number of seconds
  Period := TChronoKit.SecondsToPeriod(34128000);  // ~1y 1m
  AssertEquals('Should get approximate years', 1, Period.Years);
  AssertEquals('Should get approximate months', 1, Period.Months);
  WriteLn('Test94_SecondsToPeriod:Finished');
end;

procedure TLegacyBehaviorTests.Test95_StandardizePeriod;
var
  Period, Standardized: TDateSpan;
begin
  WriteLn('Test95_StandardizePeriod:Starting');
  // Test overflow values
  Period := TChronoKit.CreatePeriod(1, 13, 32, 25, 61, 61, 1001);
  Standardized := TChronoKit.StandardizePeriod(Period);

  AssertEquals('Years should include extra months', 2, Standardized.Years);
  AssertEquals('Months should be normalized', 1, Standardized.Months);
  AssertEquals('Days should include extra hours', 33, Standardized.Days);
  AssertEquals('Hours should be normalized', 2, Standardized.Hours);
  AssertEquals('Minutes should be normalized', 2, Standardized.Minutes);
  AssertEquals('Seconds should be normalized', 2, Standardized.Seconds);
  AssertEquals('Milliseconds should be normalized', 1, Standardized.Milliseconds);
  WriteLn('Test95_StandardizePeriod:Finished');
end;

procedure TLegacyBehaviorTests.Test96_IntervalAlign;
var
  Interval1, Interval2: TInterval;
begin
  WriteLn('Test96_IntervalAlign:Starting');
  // Test adjacent intervals
  Interval1 := TChronoKit.CreateInterval(
    EncodeDate(2024, 1, 1),
    EncodeDate(2024, 1, 15));
  Interval2 := TChronoKit.CreateInterval(
    EncodeDate(2024, 1, 15),
    EncodeDate(2024, 1, 31));
  AssertTrue('Adjacent intervals should align',
    TChronoKit.IntervalAlign(Interval1, Interval2));

  // Test non-adjacent intervals
  Interval2.StartDate := EncodeDate(2024, 1, 16);
  AssertFalse('Non-adjacent intervals should not align',
    TChronoKit.IntervalAlign(Interval1, Interval2));
  WriteLn('Test96_IntervalAlign:Finished');
end;

procedure TLegacyBehaviorTests.Test97_IntervalGap;
var
  Interval1, Interval2: TInterval;
  Gap: TDateSpan;
begin
  WriteLn('Test97_IntervalGap:Starting');
  // Test intervals with gap
  Interval1 := TChronoKit.CreateInterval(
    EncodeDate(2024, 1, 1),
    EncodeDate(2024, 1, 15));
  Interval2 := TChronoKit.CreateInterval(
    EncodeDate(2024, 1, 20),
    EncodeDate(2024, 1, 31));
  Gap := TChronoKit.IntervalGap(Interval1, Interval2);

  AssertEquals('Gap should be 5 days',
    5, Gap.Days);

  // Test overlapping intervals
  Interval2.StartDate := EncodeDate(2024, 1, 10);
  Gap := TChronoKit.IntervalGap(Interval1, Interval2);
  AssertEquals('Overlapping intervals should have no gap',
    0, Gap.Days);
  WriteLn('Test97_IntervalGap:Finished');
end;

procedure TLegacyBehaviorTests.Test98_IntervalSetdiff;
var
  Interval1, Interval2, Result: TInterval;
begin
  WriteLn('Test98_IntervalSetdiff:Starting');
  // Test partial overlap
  Interval1 := TChronoKit.CreateInterval(
    EncodeDate(2024, 1, 1),
    EncodeDate(2024, 1, 31));
  Interval2 := TChronoKit.CreateInterval(
    EncodeDate(2024, 1, 15),
    EncodeDate(2024, 2, 15));
  Result := TChronoKit.IntervalSetdiff(Interval1, Interval2);

  AssertEquals('Difference should start at original start',
    EncodeDate(2024, 1, 1), Result.StartDate);
  AssertEquals('Difference should end at overlap start',
    EncodeDate(2024, 1, 15), Result.EndDate);
  WriteLn('Test98_IntervalSetdiff:Finished');
end;

procedure TLegacyBehaviorTests.Test99_IntervalUnion;
var
  Interval1, Interval2, Result: TInterval;
begin
  WriteLn('Test99_IntervalUnion:Starting');
  // Test overlapping intervals
  Interval1 := TChronoKit.CreateInterval(
    EncodeDate(2024, 1, 1),
    EncodeDate(2024, 1, 15));
  Interval2 := TChronoKit.CreateInterval(
    EncodeDate(2024, 1, 10),
    EncodeDate(2024, 1, 31));
  Result := TChronoKit.IntervalUnion(Interval1, Interval2);

  AssertEquals('Union should start at earliest date',
    EncodeDate(2024, 1, 1), Result.StartDate);
  AssertEquals('Union should end at latest date',
    EncodeDate(2024, 1, 31), Result.EndDate);
  WriteLn('Test99_IntervalUnion:Finished');
end;

procedure TLegacyBehaviorTests.Test100_IntervalIntersection;
var
  Interval1, Interval2, Result: TInterval;
begin
  WriteLn('Test100_IntervalIntersection:Starting');
  // Test overlapping intervals
  Interval1 := TChronoKit.CreateInterval(
    EncodeDate(2024, 1, 1),
    EncodeDate(2024, 1, 15));
  Interval2 := TChronoKit.CreateInterval(
    EncodeDate(2024, 1, 10),
    EncodeDate(2024, 1, 31));
  Result := TChronoKit.IntervalIntersection(Interval1, Interval2);

  AssertEquals('Intersection should start at later start date',
    EncodeDate(2024, 1, 10), Result.StartDate);
  AssertEquals('Intersection should end at earlier end date',
    EncodeDate(2024, 1, 15), Result.EndDate);

  // Test non-overlapping intervals
  Interval2.StartDate := EncodeDate(2024, 1, 16);
  Result := TChronoKit.IntervalIntersection(Interval1, Interval2);
  AssertEquals('Non-overlapping intervals should have empty intersection',
    0, Result.StartDate);
  WriteLn('Test100_IntervalIntersection:Finished');
end;

{ EpiWeek Tests }

procedure TLegacyBehaviorTests.Test87a_EpiWeek_MidYear;
var
  TestDate: TDateTime;
begin
  WriteLn('Test87a_EpiWeek_MidYear:Starting');
  TestDate := EncodeDate(2024, 6, 15);
  AssertEquals('Mid-year date should have correct epi week',
    24, TChronoKit.GetEpiWeek(TestDate));
  WriteLn('Test87a_EpiWeek_MidYear:Finished');
end;

procedure TLegacyBehaviorTests.Test87b_EpiWeek_FirstWeek;
var
  TestDate: TDateTime;
begin
  WriteLn('Test87b_EpiWeek_FirstWeek:Starting');
  TestDate := EncodeDate(2024, 1, 4);
  AssertEquals('First full week should be week 1',
    1, TChronoKit.GetEpiWeek(TestDate));
  WriteLn('Test87b_EpiWeek_FirstWeek:Finished');
end;

procedure TLegacyBehaviorTests.Test87c_EpiWeek_YearEnd;
var
  TestDate: TDateTime;
begin
  WriteLn('Test87c_EpiWeek_YearEnd:Starting');
  TestDate := EncodeDate(2024, 12, 31);
  AssertEquals('Year-end week number should be correct',
    53, TChronoKit.GetEpiWeek(TestDate));
  WriteLn('Test87c_EpiWeek_YearEnd:Finished');
end;

{ StandardizePeriod Tests }

procedure TLegacyBehaviorTests.Test95a_StandardizePeriod_Milliseconds;
var
  Period, Standardized: TDateSpan;
begin
  WriteLn('Test95a_StandardizePeriod_Milliseconds:Starting');
  Period := TChronoKit.CreatePeriod(0, 0, 0, 0, 0, 0, 1001);
  Standardized := TChronoKit.StandardizePeriod(Period);
  AssertEquals('Milliseconds should be normalized', 1, Standardized.Milliseconds);
  AssertEquals('Extra milliseconds should carry to seconds', 1, Standardized.Seconds);
  WriteLn('Test95a_StandardizePeriod_Milliseconds:Finished');
end;

procedure TLegacyBehaviorTests.Test95b_StandardizePeriod_Seconds;
var
  Period, Standardized: TDateSpan;
begin
  WriteLn('Test95b_StandardizePeriod_Seconds:Starting');
  Period := TChronoKit.CreatePeriod(0, 0, 0, 0, 0, 61, 0);
  Standardized := TChronoKit.StandardizePeriod(Period);
  AssertEquals('Seconds should be normalized', 1, Standardized.Seconds);
  AssertEquals('Extra seconds should carry to minutes', 1, Standardized.Minutes);
  WriteLn('Test95b_StandardizePeriod_Seconds:Finished');
end;

procedure TLegacyBehaviorTests.Test95c_StandardizePeriod_Minutes;
var
  Period, Standardized: TDateSpan;
begin
  WriteLn('Test95c_StandardizePeriod_Minutes:Starting');
  Period := TChronoKit.CreatePeriod(0, 0, 0, 0, 61, 0, 0);
  Standardized := TChronoKit.StandardizePeriod(Period);
  AssertEquals('Minutes should be normalized', 1, Standardized.Minutes);
  AssertEquals('Extra minutes should carry to hours', 1, Standardized.Hours);
  WriteLn('Test95c_StandardizePeriod_Minutes:Finished');
end;

procedure TLegacyBehaviorTests.Test95d_StandardizePeriod_Hours;
var
  Period, Standardized: TDateSpan;
begin
  WriteLn('Test95d_StandardizePeriod_Hours:Starting');
  Period := TChronoKit.CreatePeriod(0, 0, 0, 25, 0, 0, 0);
  Standardized := TChronoKit.StandardizePeriod(Period);
  AssertEquals('Hours should be normalized', 1, Standardized.Hours);
  AssertEquals('Extra hours should carry to days', 1, Standardized.Days);
  WriteLn('Test95d_StandardizePeriod_Hours:Finished');
end;

procedure TLegacyBehaviorTests.Test95e_StandardizePeriod_Months;
var
  Period, Standardized: TDateSpan;
begin
  WriteLn('Test95e_StandardizePeriod_Months:Starting');
  Period := TChronoKit.CreatePeriod(0, 13, 0, 0, 0, 0, 0);
  Standardized := TChronoKit.StandardizePeriod(Period);
  AssertEquals('Months should be normalized', 1, Standardized.Months);
  AssertEquals('Extra months should carry to years', 1, Standardized.Years);
  WriteLn('Test95e_StandardizePeriod_Months:Finished');
end;

procedure TLegacyBehaviorTests.Test95f_StandardizePeriod_Complex;
var
  Period, Standardized: TDateSpan;
begin
  WriteLn('Test95f_StandardizePeriod_Complex:Starting');
  Period := TChronoKit.CreatePeriod(1, 13, 32, 25, 61, 61, 1001);
  Standardized := TChronoKit.StandardizePeriod(Period);

  AssertEquals('Years should include extra months', 2, Standardized.Years);
  AssertEquals('Months should be normalized', 1, Standardized.Months);
  AssertEquals('Days should include extra hours', 33, Standardized.Days);
  AssertEquals('Hours should be normalized', 2, Standardized.Hours);
  AssertEquals('Minutes should be normalized', 2, Standardized.Minutes);
  AssertEquals('Seconds should be normalized', 2, Standardized.Seconds);
  AssertEquals('Milliseconds should be normalized', 1, Standardized.Milliseconds);
  WriteLn('Test95f_StandardizePeriod_Complex:Finished');
end;

{ IntervalGap Tests }

procedure TLegacyBehaviorTests.Test97a_IntervalGap_NoOverlap;
var
  Interval1, Interval2: TInterval;
  Gap: TDateSpan;
begin
  WriteLn('Test97a_IntervalGap_NoOverlap:Starting');
  Interval1 := TChronoKit.CreateInterval(
    EncodeDate(2024, 1, 1),
    EncodeDate(2024, 1, 15));
  Interval2 := TChronoKit.CreateInterval(
    EncodeDate(2024, 1, 20),
    EncodeDate(2024, 1, 31));
  Gap := TChronoKit.IntervalGap(Interval1, Interval2);

  AssertEquals('Gap should be 5 days', 5, Gap.Days);
  WriteLn('Test97a_IntervalGap_NoOverlap:Finished');
end;

procedure TLegacyBehaviorTests.Test97b_IntervalGap_Overlapping;
var
  Interval1, Interval2: TInterval;
  Gap: TDateSpan;
begin
  WriteLn('Test97b_IntervalGap_Overlapping:Starting');
  Interval1 := TChronoKit.CreateInterval(
    EncodeDate(2024, 1, 1),
    EncodeDate(2024, 1, 15));
  Interval2 := TChronoKit.CreateInterval(
    EncodeDate(2024, 1, 10),
    EncodeDate(2024, 1, 31));
  Gap := TChronoKit.IntervalGap(Interval1, Interval2);

  AssertEquals('Overlapping intervals should have no gap', 0, Gap.Days);
  WriteLn('Test97b_IntervalGap_Overlapping:Finished');
end;

procedure TLegacyBehaviorTests.Test116_YMD;
var
  Expected: TDateTime;
begin
  Expected := EncodeDate(2024, 3, 15);
  WriteLn('Test116_YMD:Starting');
  AssertEquals('YMD with hyphen', Expected, TChronoKit.YMD('2024-03-15'));
  AssertEquals('YMD with slash', Expected, TChronoKit.YMD('2024/03/15'));
  WriteLn('Test116_YMD:Finished');
end;

procedure TLegacyBehaviorTests.Test117_MDY;
var
  Expected: TDateTime;
begin
  Expected := EncodeDate(2024, 3, 15);
  WriteLn('Test117_MDY:Starting');
  AssertEquals('MDY with hyphen', Expected, TChronoKit.MDY('03-15-2024'));
  AssertEquals('MDY with slash', Expected, TChronoKit.MDY('03/15/2024'));
  WriteLn('Test117_MDY:Finished');
end;

procedure TLegacyBehaviorTests.Test118_DMY;
var
  Expected: TDateTime;
begin
  Expected := EncodeDate(2024, 3, 15);
  WriteLn('Test118_DMY:Starting');
  AssertEquals('DMY with hyphen', Expected, TChronoKit.DMY('15-03-2024'));
  AssertEquals('DMY with slash', Expected, TChronoKit.DMY('15/03/2024'));
  WriteLn('Test118_DMY:Finished');
end;

procedure TLegacyBehaviorTests.Test151_DurationSpanDoesNotDoubleCountMilliseconds;
var
  Span: TDateSpan;
  StartValue: TDateTime;
begin
  StartValue := EncodeDate(2024, 1, 1);
  Span := TChronoKit.SpanBetween(StartValue,
    IncMilliSecond(StartValue, 1500), dskDuration);
  AssertEquals('Duration seconds must contain only whole seconds',
    1, Span.Seconds);
  AssertEquals('Duration milliseconds must contain the remainder',
    500, Span.Milliseconds);
end;

procedure TLegacyBehaviorTests.Test152_IntervalGapPreservesSubDayPrecision;
var
  FirstInterval, SecondInterval: TInterval;
  Gap: TDateSpan;
begin
  FirstInterval := TChronoKit.CreateInterval(
    EncodeDateTime(2024, 1, 1, 9, 0, 0, 0),
    EncodeDateTime(2024, 1, 1, 10, 0, 0, 0));
  SecondInterval := TChronoKit.CreateInterval(
    EncodeDateTime(2024, 1, 1, 11, 30, 0, 0),
    EncodeDateTime(2024, 1, 1, 12, 0, 0, 0));
  Gap := TChronoKit.IntervalGap(FirstInterval, SecondInterval);
  AssertEquals('A 90-minute gap must retain all elapsed seconds',
    90 * 60, Gap.Seconds);
  AssertEquals('A whole-second gap has no millisecond remainder',
    0, Gap.Milliseconds);
end;

procedure TLegacyBehaviorTests.Test154_CreateIntervalRejectsReverseOrder;
var
  Raised: Boolean;
begin
  Raised := False;
  try
    TChronoKit.CreateInterval(EncodeDate(2024, 1, 2),
      EncodeDate(2024, 1, 1));
  except
    on E: EArgumentException do
    begin
      Raised := True;
      AssertTrue('Interval validation must identify endpoint order',
        Pos('start', LowerCase(E.Message)) > 0);
    end;
  end;
  AssertTrue('CreateInterval must reject a start after its end', Raised);
end;

procedure TLegacyBehaviorTests.Test155_SeasonRoundingRaises;
var
  Raised: Boolean;
  Value: TDateTime;
begin
  Value := EncodeDate(2024, 3, 15);

  Raised := False;
  try
    TChronoKit.FloorDate(Value, duSeason);
  except
    on E: EArgumentException do
      Raised := Pos('season', LowerCase(E.Message)) > 0;
  end;
  AssertTrue('FloorDate must reject duSeason clearly', Raised);

  Raised := False;
  try
    TChronoKit.CeilingDate(Value, duSeason);
  except
    on E: EArgumentException do
      Raised := Pos('season', LowerCase(E.Message)) > 0;
  end;
  AssertTrue('CeilingDate must reject duSeason clearly', Raised);

  Raised := False;
  try
    TChronoKit.RoundDate(Value, duSeason);
  except
    on E: EArgumentException do
      Raised := Pos('season', LowerCase(E.Message)) > 0;
  end;
  AssertTrue('RoundDate must reject duSeason clearly', Raised);
end;

procedure TLegacyBehaviorTests.Test156_MonthRollingMatchesAddMonths;
var
  Value: TDateTime;
begin
  Value := EncodeDateTime(2024, 3, 31, 12, 34, 56, 789);
  AssertEquals('RollbackMonth must match AddMonths at month end',
    TChronoKit.AddMonths(Value, -1), TChronoKit.RollbackMonth(Value),
    OneMillisecond);

  Value := EncodeDateTime(2024, 1, 31, 12, 34, 56, 789);
  AssertEquals('RollForwardMonth must match AddMonths at month end',
    TChronoKit.AddMonths(Value, 1), TChronoKit.RollForwardMonth(Value),
    OneMillisecond);
end;

initialization
  RegisterTest(TLegacyBehaviorTests);

end.
