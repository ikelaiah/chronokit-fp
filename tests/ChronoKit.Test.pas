unit ChronoKit.Test;

{$mode objfpc}{$H+}{$J-}
{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  Classes, SysUtils, DateUtils, fpcunit, testregistry,
  ChronoKit;

type
  TStringArray = array of string;

type
  TDateTimeTests = class(TTestCase)
  private
    FDateTime: TChronoKit;
    function FixtureName(const AVariable: string): string;
    function FixtureDateTime(const AVariable: string): TDateTime;
    function NamedWallClockToUTC(const AValue: TDateTime;
      const ATimeZone: string): TDateTime;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Basic operations
    procedure Test01_Now;
    procedure Test02_Today;
    procedure Test03_From;
    // Date parts - getters and setters
    procedure Test04_Year;
    procedure Test05_Month;
    procedure Test06_Day;
    procedure Test07_Hour;
    procedure Test08_Minute;
    procedure Test09_Second;
    procedure Test10_Millisecond;
    // Date manipulations
    procedure Test11_AddYears;
    procedure Test12_AddMonths;
    procedure Test13_AddDays;
    procedure Test14_AddHours;
    procedure Test15_AddMinutes;
    procedure Test16_AddSeconds;
    // Date truncations
    procedure Test17_StartOfYear;
    procedure Test18_StartOfMonth;
    procedure Test19_StartOfDay;
    procedure Test20_EndOfYear;
    procedure Test21_EndOfMonth;
    procedure Test22_EndOfDay;
    // Date comparisons
    procedure Test23_IsBefore;
    procedure Test24_IsAfter;
    procedure Test25_IsSameDay;
    procedure Test26_IsSameMonth;
    procedure Test27_IsSameYear;
    // Conversions
    procedure Test28_ToDateTime;
    procedure Test29_ToString;
    // Business day functions
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
    procedure Test140_YMDValidationMessage;
    procedure Test141_MDYValidationMessage;
    procedure Test142_DMYValidationMessage;
    procedure Test143_YQValidationMessage;
    procedure Test144_FromStringValidationMessage;
    procedure Test145_YQYearValidationMessage;
    procedure Test146_FormatDateTimeAlias;
    procedure Test147_ParseDateTimeAlias;
    procedure Test148_ParseDateTimeValidationMessage;
    // Time Span Tests
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
    // Date Unit Tests
    procedure Test46_FloorDateSecond;
    procedure Test47_FloorDateMinute;
    procedure Test48_FloorDateHour;
    procedure Test49_FloorDateDay;
    procedure Test50_FloorDateWeek;
    procedure Test51_FloorDateMonth;
    procedure Test52_FloorDateBiMonth;
    procedure Test53_FloorDateQuarter;
    procedure Test54_FloorDateHalfYear;
    procedure Test55_FloorDateYear;
    procedure Test56_CeilingDateSecond;
    procedure Test57_CeilingDateMinute;
    procedure Test58_CeilingDateHour;
    procedure Test59_CeilingDateDay;
    procedure Test60_CeilingDateWeek;
    procedure Test61_CeilingDateMonth;
    procedure Test62_CeilingDateBiMonth;
    procedure Test63_CeilingDateQuarter;
    procedure Test64_CeilingDateHalfYear;
    procedure Test65_CeilingDateYear;
    procedure Test66_RoundDateSecond;
    procedure Test67_RoundDateMinute;
    procedure Test68_RoundDateHour;
    procedure Test69_RoundDateDay;
    procedure Test70_RoundDateWeek;
    procedure Test71_RoundDateMonth;
    procedure Test72_RoundDateBiMonth;
    procedure Test73_RoundDateQuarter;
    procedure Test74_RoundDateHalfYear;
    procedure Test75_RoundDateYear;
    
    // Date parsing tests
    procedure Test76_YMD_Valid;
    procedure Test77_YMD_Invalid;
    procedure Test78_MDY_Valid;
    procedure Test79_MDY_Invalid;
    procedure Test80_DMY_Valid;
    procedure Test81_DMY_Invalid;
    procedure Test82_YQ_Valid;
    procedure Test83_YQ_Invalid;
    
    // ISO calendar tests
    procedure Test84_ISOYear;
    procedure Test85_ISOWeek;
    // Epidemiological calendar tests
    procedure Test86_EpiYear;
    procedure Test87_EpiWeek;
    // Academic calendar tests
    procedure Test88_Semester;
    
    // Decimal date tests
    procedure Test89_DateDecimal;
    procedure Test90_GetDecimalDate;
    // Month rolling tests
    procedure Test91_RollbackMonth;
    procedure Test92_RollForwardMonth;
    
    // Period/Duration conversion tests
    procedure Test93_PeriodToSeconds;
    procedure Test94_SecondsToPeriod;
    procedure Test95_StandardizePeriod;
    // Interval operation tests
    procedure Test96_IntervalAlign;
    procedure Test97_IntervalGap;
    procedure Test98_IntervalSetdiff;
    procedure Test99_IntervalUnion;
    procedure Test100_IntervalIntersection;
    
    // EpiWeek Tests
    procedure Test87a_EpiWeek_MidYear;
    procedure Test87b_EpiWeek_FirstWeek;
    procedure Test87c_EpiWeek_YearEnd;
    
    // StandardizePeriod Tests
    procedure Test95a_StandardizePeriod_Milliseconds;
    procedure Test95b_StandardizePeriod_Seconds;
    procedure Test95c_StandardizePeriod_Minutes;
    procedure Test95d_StandardizePeriod_Hours;
    procedure Test95e_StandardizePeriod_Months;
    procedure Test95f_StandardizePeriod_Complex;
    
    // IntervalGap Tests
    procedure Test97a_IntervalGap_NoOverlap;
    procedure Test97b_IntervalGap_Overlapping;
    
    // Timezone Tests
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
    
    // More Date parsing tests
    procedure Test116_YMD;
    procedure Test117_MDY;
    procedure Test118_DMY;
    
    // Shared logical-zone matrix
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
    // v1.6.0 correctness regressions
    procedure Test149_CeilingDateRollsAcrossTimeBoundaries;
    procedure Test150_EndBoundariesContainTheirStartingBoundary;
    procedure Test151_DurationSpanDoesNotDoubleCountMilliseconds;
    procedure Test152_IntervalGapPreservesSubDayPrecision;
    procedure Test153_DecimalDateRoundTripPreservesTime;
    procedure Test154_CreateIntervalRejectsReverseOrder;
    procedure Test155_SeasonRoundingRaises;
    procedure Test156_MonthRollingMatchesAddMonths;
    procedure Test157_DecimalYearReplacementRoundTrip;
    procedure Test158_CalendarPeriodArithmetic;
    procedure Test159_ExactDurationArithmetic;
    procedure Test160_DurationConstructionRejectsOverflow;
    procedure Test161_HalfOpenRangeValidationAndContainment;
    procedure Test162_HalfOpenRangeRelationsAndGap;
    procedure Test163_SubtractRangeReturnsEveryRemainder;
    procedure Test164_RangeTryOperationsAvoidSentinels;
    procedure Test165_StartOfQuarterValidatesInputs;
    procedure Test166_ExplicitTimezoneNamesPreserveSemantics;
    procedure Test167_QuarterValueBoundaries;
    procedure Test168_BusinessDaysBetweenCountsInclusiveDates;
    procedure Test169_ConvertBetweenTimeZonesPreservesInstant;
  end;

implementation

{$IFDEF WINDOWS}
uses Windows;
{$ENDIF}

{$IFDEF UNIX}
{$LINKLIB c}
function setenv(name, value: PChar; overwrite: LongInt): LongInt; cdecl; external 'c';
{$ENDIF}

{ SetEnvironmentVariableCrossPlatform - Sets environment variable in a platform-independent way }
procedure SetEnvironmentVariableCrossPlatform(const Name, Value: string);
begin
  {$IFDEF WINDOWS}
  Windows.SetEnvironmentVariable(PChar(Name), PChar(Value));
  {$ELSE}
  {$IFDEF UNIX}
  setenv(PChar(Name), PChar(Value), 1);
  {$ENDIF}
  {$ENDIF}
end;

{ Helper function for cross-platform environment variable handling }
function GetEnvVar(const Name: string): string;
begin
  Result := SysUtils.GetEnvironmentVariable(Name);
end;

{ Helper function for cross-platform environment variable setting }
procedure SetEnvVar(const Name, Value: string);
begin
  {$IFDEF WINDOWS}
  SetEnvironmentVariable(PChar(Name), PChar(Value));
  {$ELSE}
  SetEnvironmentVariableCrossPlatform(Name, Value);
  {$ENDIF}
end;

{ TDateTimeTests }

procedure TDateTimeTests.SetUp;
begin
  // No setup needed for static functions
end;

procedure TDateTimeTests.TearDown;
begin
  // No teardown needed for static functions
end;

function TDateTimeTests.FixtureName(const AVariable: string): string;
begin
  Result := GetEnvVar(AVariable);
  AssertTrue(AVariable + ' must name a platform-native timezone fixture',
    Result <> '');
end;

function TDateTimeTests.FixtureDateTime(const AVariable: string): TDateTime;
var
  FixtureText: string;
begin
  FixtureText := GetEnvVar(AVariable);
  AssertTrue(AVariable + ' must contain a system-local fixture',
    FixtureText <> '');
  Result := ScanDateTime('yyyy-mm-dd hh:nn:ss', FixtureText);
end;

function TDateTimeTests.NamedWallClockToUTC(const AValue: TDateTime;
  const ATimeZone: string): TDateTime;
var
  SystemLocal: TDateTime;
begin
  SystemLocal := TChronoKit.ForceTimeZone(AValue, ATimeZone);
  Result := TChronoKit.WithTimeZone(SystemLocal, 'UTC');
end;

procedure TDateTimeTests.Test01_Now;
var
  CurrentTime: TDateTime;
begin
  WriteLn('Test01_Now:Starting');
  CurrentTime := Now;
  AssertTrue('Now should return current time',
    Abs(CurrentTime - TChronoKit.GetNow) < 1/86400); // Within 1 second
  WriteLn('Test01_Now:Finished');
end;

procedure TDateTimeTests.Test02_Today;
begin
  WriteLn('Test02_Today:Starting');
  AssertEquals('Today should return current date at midnight',
    Trunc(Date), Trunc(TChronoKit.GetToday));
  WriteLn('Test02_Today:Finished');
end;

procedure TDateTimeTests.Test03_From;
var
  TestDate: TDateTime;
begin
  WriteLn('Test03_From:Starting');
  TestDate := EncodeDate(2024, 1, 15);
  AssertEquals('GetDateTime should return the correct date',
    TestDate, TChronoKit.GetDateTime(TestDate));
  WriteLn('Test03_From:Finished');
end;

procedure TDateTimeTests.Test04_Year;
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

procedure TDateTimeTests.Test05_Month;
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

procedure TDateTimeTests.Test06_Day;
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

procedure TDateTimeTests.Test07_Hour;
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

procedure TDateTimeTests.Test08_Minute;
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

procedure TDateTimeTests.Test09_Second;
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

procedure TDateTimeTests.Test10_Millisecond;
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

procedure TDateTimeTests.Test11_AddYears;
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

procedure TDateTimeTests.Test12_AddMonths;
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

procedure TDateTimeTests.Test13_AddDays;
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

procedure TDateTimeTests.Test14_AddHours;
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

procedure TDateTimeTests.Test15_AddMinutes;
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

procedure TDateTimeTests.Test16_AddSeconds;
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

procedure TDateTimeTests.Test17_StartOfYear;
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

procedure TDateTimeTests.Test18_StartOfMonth;
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

procedure TDateTimeTests.Test19_StartOfDay;
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

procedure TDateTimeTests.Test20_EndOfYear;
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

procedure TDateTimeTests.Test21_EndOfMonth;
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

procedure TDateTimeTests.Test22_EndOfDay;
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

procedure TDateTimeTests.Test23_IsBefore;
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

procedure TDateTimeTests.Test24_IsAfter;
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

procedure TDateTimeTests.Test25_IsSameDay;
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

procedure TDateTimeTests.Test26_IsSameMonth;
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

procedure TDateTimeTests.Test27_IsSameYear;
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

procedure TDateTimeTests.Test28_ToDateTime;
var
  TestDate: TDateTime;
begin
  WriteLn('Test28_ToDateTime:Starting');
  TestDate := EncodeDate(2024, 1, 15);
  AssertEquals('GetDateTime should return the correct date',
    TestDate, TChronoKit.GetDateTime(TestDate));
  WriteLn('Test28_ToDateTime:Finished');
end;

procedure TDateTimeTests.Test29_ToString;
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
procedure TDateTimeTests.Test30_IsBusinessDay;
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

procedure TDateTimeTests.Test31_NextBusinessDay;
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

procedure TDateTimeTests.Test32_PreviousBusinessDay;
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

procedure TDateTimeTests.Test33_AddBusinessDays;
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

procedure TDateTimeTests.Test131_DefaultBusinessCalendarCompatibility;
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

procedure TDateTimeTests.Test132_BusinessCalendarHolidays;
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

procedure TDateTimeTests.Test133_AlternativeWorkingWeek;
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

procedure TDateTimeTests.Test134_ConfiguredBusinessDayNavigation;
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

procedure TDateTimeTests.Test135_InvalidBusinessCalendar;
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

procedure TDateTimeTests.Test136_LeapDayHolidayBoundary;
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

procedure TDateTimeTests.Test137_MonthEndBusinessDayBoundary;
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

procedure TDateTimeTests.Test138_WeekStartBusinessDayBoundary;
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

procedure TDateTimeTests.Test139_ZeroBusinessDaysPreservesInput;
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

procedure TDateTimeTests.Test140_YMDValidationMessage;
begin
  WriteLn('Test140_YMDValidationMessage:Starting');
  try
    TChronoKit.YMD('2024-02-30');
    Fail('YMD should reject a day outside the calendar month');
  except
    on E: EConvertError do
    begin
      AssertTrue('YMD error should include the rejected input',
        Pos('2024-02-30', E.Message) > 0);
      AssertTrue('YMD error should show the accepted shape',
        Pos('YYYY-MM-DD', E.Message) > 0);
    end;
  end;
  WriteLn('Test140_YMDValidationMessage:Finished');
end;

procedure TDateTimeTests.Test141_MDYValidationMessage;
begin
  WriteLn('Test141_MDYValidationMessage:Starting');
  try
    TChronoKit.MDY('02-30-2024');
    Fail('MDY should reject a day outside the calendar month');
  except
    on E: EConvertError do
    begin
      AssertTrue('MDY error should include the rejected input',
        Pos('02-30-2024', E.Message) > 0);
      AssertTrue('MDY error should show the accepted shape',
        Pos('MM-DD-YYYY', E.Message) > 0);
    end;
  end;
  WriteLn('Test141_MDYValidationMessage:Finished');
end;

procedure TDateTimeTests.Test142_DMYValidationMessage;
begin
  WriteLn('Test142_DMYValidationMessage:Starting');
  try
    TChronoKit.DMY('30-02-2024');
    Fail('DMY should reject a day outside the calendar month');
  except
    on E: EConvertError do
    begin
      AssertTrue('DMY error should include the rejected input',
        Pos('30-02-2024', E.Message) > 0);
      AssertTrue('DMY error should show the accepted shape',
        Pos('DD-MM-YYYY', E.Message) > 0);
    end;
  end;
  WriteLn('Test142_DMYValidationMessage:Finished');
end;

procedure TDateTimeTests.Test143_YQValidationMessage;
begin
  WriteLn('Test143_YQValidationMessage:Starting');
  try
    TChronoKit.YQ('2024-5');
    Fail('YQ should reject a quarter outside 1 through 4');
  except
    on E: EConvertError do
    begin
      AssertTrue('YQ error should include the rejected input',
        Pos('2024-5', E.Message) > 0);
      AssertTrue('YQ error should explain the valid quarter range',
        Pos('between 1 and 4', E.Message) > 0);
    end;
  end;
  WriteLn('Test143_YQValidationMessage:Finished');
end;

procedure TDateTimeTests.Test144_FromStringValidationMessage;
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

procedure TDateTimeTests.Test145_YQYearValidationMessage;
begin
  WriteLn('Test145_YQYearValidationMessage:Starting');
  try
    TChronoKit.YQ('0-1');
    Fail('YQ should reject a year outside the TDateTime range');
  except
    on E: EConvertError do
    begin
      AssertTrue('YQ year error should include the rejected input',
        Pos('0-1', E.Message) > 0);
      AssertTrue('YQ year error should explain the valid year range',
        Pos('between 1 and 9999', E.Message) > 0);
    end;
  end;
  WriteLn('Test145_YQYearValidationMessage:Finished');
end;

procedure TDateTimeTests.Test146_FormatDateTimeAlias;
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

procedure TDateTimeTests.Test147_ParseDateTimeAlias;
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

procedure TDateTimeTests.Test148_ParseDateTimeValidationMessage;
begin
  WriteLn('Test148_ParseDateTimeValidationMessage:Starting');
  try
    TChronoKit.ParseDateTime('not-a-date');
    Fail('ParseDateTime should reject invalid date/time input');
  except
    on E: EConvertError do
    begin
      AssertTrue('ParseDateTime error should include the rejected input',
        Pos('not-a-date', E.Message) > 0);
      AssertTrue('ParseDateTime error should explain the expected input',
        Pos('system date/time format', E.Message) > 0);
    end;
  end;
  WriteLn('Test148_ParseDateTimeValidationMessage:Finished');
end;

procedure TDateTimeTests.Test34_CreatePeriod;
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

procedure TDateTimeTests.Test35_CreateDuration;
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

procedure TDateTimeTests.Test36_CreateInterval;
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

procedure TDateTimeTests.Test37_AddSpan;
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

procedure TDateTimeTests.Test38_SubtractSpan;
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

procedure TDateTimeTests.Test39_SpanBetween;
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

procedure TDateTimeTests.Test40_IsWithinInterval;
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

procedure TDateTimeTests.Test41_IntervalsOverlap;
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

procedure TDateTimeTests.Test42_IntervalLength;
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

procedure TDateTimeTests.Test43_PeriodNormalization;
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

procedure TDateTimeTests.Test44_DurationCalculation;
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

procedure TDateTimeTests.Test45_SpanCornerCases;
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

procedure TDateTimeTests.Test46_FloorDateSecond;
var
  TestDate: TDateTime;
begin
  WriteLn('Test46_FloorDateSecond:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Floor to second should clear milliseconds',
    EncodeDateTime(2024, 3, 15, 14, 30, 45, 0),
    TChronoKit.FloorDate(TestDate, duSecond));
  WriteLn('Test46_FloorDateSecond:Finished');
end;

procedure TDateTimeTests.Test47_FloorDateMinute;
var
  TestDate: TDateTime;
begin
  WriteLn('Test47_FloorDateMinute:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Floor to minute should clear seconds and milliseconds',
    EncodeDateTime(2024, 3, 15, 14, 30, 0, 0),
    TChronoKit.FloorDate(TestDate, duMinute));
  WriteLn('Test47_FloorDateMinute:Finished');
end;

procedure TDateTimeTests.Test48_FloorDateHour;
var
  TestDate: TDateTime;
begin
  WriteLn('Test48_FloorDateHour:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Floor to hour should clear minutes, seconds and milliseconds',
    EncodeDateTime(2024, 3, 15, 14, 0, 0, 0),
    TChronoKit.FloorDate(TestDate, duHour));
  WriteLn('Test48_FloorDateHour:Finished');
end;

procedure TDateTimeTests.Test49_FloorDateDay;
var
  TestDate: TDateTime;
begin
  WriteLn('Test49_FloorDateDay:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Floor to day should clear time portion',
    EncodeDate(2024, 3, 15),
    TChronoKit.FloorDate(TestDate, duDay));
  WriteLn('Test49_FloorDateDay:Finished');
end;

procedure TDateTimeTests.Test50_FloorDateWeek;
var
  TestDate: TDateTime;
begin
  WriteLn('Test50_FloorDateWeek:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500); // March 15, 2024 is a Friday
  AssertEquals('Floor to week should go to Sunday',
    EncodeDate(2024, 3, 10), // Should go to Sunday, March 10
    TChronoKit.FloorDate(TestDate, duWeek));
  WriteLn('Test50_FloorDateWeek:Finished');
end;

procedure TDateTimeTests.Test51_FloorDateMonth;
var
  TestDate: TDateTime;
begin
  WriteLn('Test51_FloorDateMonth:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Floor to month should go to first day of month',
    EncodeDate(2024, 3, 1),
    TChronoKit.FloorDate(TestDate, duMonth));
  WriteLn('Test51_FloorDateMonth:Finished');
end;

procedure TDateTimeTests.Test52_FloorDateBiMonth;
var
  TestDate: TDateTime;
begin
  WriteLn('Test52_FloorDateBiMonth:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Floor to bi-month should go to first day of even month',
    EncodeDate(2024, 3, 1),
    TChronoKit.FloorDate(TestDate, duBiMonth));
  WriteLn('Test52_FloorDateBiMonth:Finished');
end;

procedure TDateTimeTests.Test53_FloorDateQuarter;
var
  TestDate: TDateTime;
begin
  WriteLn('Test53_FloorDateQuarter:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Floor to quarter should go to first day of quarter',
    EncodeDate(2024, 1, 1),
    TChronoKit.FloorDate(TestDate, duQuarter));
  WriteLn('Test53_FloorDateQuarter:Finished');
end;

procedure TDateTimeTests.Test54_FloorDateHalfYear;
var
  TestDate: TDateTime;
begin
  WriteLn('Test54_FloorDateHalfYear:Starting');
  TestDate := EncodeDateTime(2024, 8, 15, 14, 30, 45, 500);
  AssertEquals('Floor to half year should go to July 1 or January 1',
    EncodeDate(2024, 7, 1),
    TChronoKit.FloorDate(TestDate, duHalfYear));
  WriteLn('Test54_FloorDateHalfYear:Finished');
end;

procedure TDateTimeTests.Test55_FloorDateYear;
var
  TestDate: TDateTime;
begin
  WriteLn('Test55_FloorDateYear:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Floor to year should go to January 1',
    EncodeDate(2024, 1, 1),
    TChronoKit.FloorDate(TestDate, duYear));
  WriteLn('Test55_FloorDateYear:Finished');
end;

procedure TDateTimeTests.Test56_CeilingDateSecond;
var
  TestDate: TDateTime;
begin
  WriteLn('Test56_CeilingDateSecond:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to second should round up to next second',
    EncodeDateTime(2024, 3, 15, 14, 30, 46, 0),
    TChronoKit.CeilingDate(TestDate, duSecond));
  WriteLn('Test56_CeilingDateSecond:Finished');
end;

procedure TDateTimeTests.Test57_CeilingDateMinute;
var
  TestDate: TDateTime;
begin
  WriteLn('Test57_CeilingDateMinute:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to minute should round up to next minute',
    EncodeDateTime(2024, 3, 15, 14, 31, 0, 0),
    TChronoKit.CeilingDate(TestDate, duMinute));
  WriteLn('Test57_CeilingDateMinute:Finished');
end;

procedure TDateTimeTests.Test58_CeilingDateHour;
var
  TestDate: TDateTime;
begin
  WriteLn('Test58_CeilingDateHour:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to hour should round up to next hour',
    EncodeDateTime(2024, 3, 15, 15, 0, 0, 0),
    TChronoKit.CeilingDate(TestDate, duHour));
  WriteLn('Test58_CeilingDateHour:Finished');
end;

procedure TDateTimeTests.Test59_CeilingDateDay;
var
  TestDate: TDateTime;
begin
  WriteLn('Test59_CeilingDateDay:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to day should round up to next day',
    EncodeDate(2024, 3, 16),
    TChronoKit.CeilingDate(TestDate, duDay));
  WriteLn('Test59_CeilingDateDay:Finished');
end;

procedure TDateTimeTests.Test60_CeilingDateWeek;
var
  TestDate: TDateTime;
begin
  WriteLn('Test60_CeilingDateWeek:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500); // March 15, 2024 is a Friday
  AssertEquals('Ceiling to week should go to next Sunday',
    EncodeDate(2024, 3, 17), // Should go to next Sunday, March 17
    TChronoKit.CeilingDate(TestDate, duWeek));
  WriteLn('Test60_CeilingDateWeek:Finished');
end;

procedure TDateTimeTests.Test61_CeilingDateMonth;
var
  TestDate: TDateTime;
begin
  WriteLn('Test61_CeilingDateMonth:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to month should go to first day of next month',
    EncodeDate(2024, 4, 1),
    TChronoKit.CeilingDate(TestDate, duMonth));
  WriteLn('Test61_CeilingDateMonth:Finished');
end;

procedure TDateTimeTests.Test62_CeilingDateBiMonth;
var
  TestDate: TDateTime;
begin
  WriteLn('Test62_CeilingDateBiMonth:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to bi-month should go to first day of next even month',
    EncodeDate(2024, 5, 1),
    TChronoKit.CeilingDate(TestDate, duBiMonth));
  WriteLn('Test62_CeilingDateBiMonth:Finished');
end;

procedure TDateTimeTests.Test63_CeilingDateQuarter;
var
  TestDate: TDateTime;
begin
  WriteLn('Test63_CeilingDateQuarter:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to quarter should go to first day of next quarter',
    EncodeDate(2024, 4, 1),
    TChronoKit.CeilingDate(TestDate, duQuarter));
  WriteLn('Test63_CeilingDateQuarter:Finished');
end;

procedure TDateTimeTests.Test64_CeilingDateHalfYear;
var
  TestDate: TDateTime;
begin
  WriteLn('Test64_CeilingDateHalfYear:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to half year should go to July 1',
    EncodeDate(2024, 7, 1),
    TChronoKit.CeilingDate(TestDate, duHalfYear));
  WriteLn('Test64_CeilingDateHalfYear:Finished');
end;

procedure TDateTimeTests.Test65_CeilingDateYear;
var
  TestDate: TDateTime;
begin
  WriteLn('Test65_CeilingDateYear:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Ceiling to year should go to January 1 of next year',
    EncodeDate(2025, 1, 1),
    TChronoKit.CeilingDate(TestDate, duYear));
  WriteLn('Test65_CeilingDateYear:Finished');
end;

procedure TDateTimeTests.Test66_RoundDateSecond;
var
  TestDate: TDateTime;
begin
  WriteLn('Test66_RoundDateSecond:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to second should round to nearest second',
    EncodeDateTime(2024, 3, 15, 14, 30, 46, 0),
    TChronoKit.RoundDate(TestDate, duSecond));
  WriteLn('Test66_RoundDateSecond:Finished');
end;

procedure TDateTimeTests.Test67_RoundDateMinute;
var
  TestDate: TDateTime;
begin
  WriteLn('Test67_RoundDateMinute:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to minute should round to nearest minute',
    EncodeDateTime(2024, 3, 15, 14, 31, 0, 0),
    TChronoKit.RoundDate(TestDate, duMinute));
  WriteLn('Test67_RoundDateMinute:Finished');
end;

procedure TDateTimeTests.Test68_RoundDateHour;
var
  TestDate: TDateTime;
begin
  WriteLn('Test68_RoundDateHour:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to hour should round to nearest hour',
    EncodeDateTime(2024, 3, 15, 15, 0, 0, 0),
    TChronoKit.RoundDate(TestDate, duHour));
  WriteLn('Test68_RoundDateHour:Finished');
end;

procedure TDateTimeTests.Test69_RoundDateDay;
var
  TestDate: TDateTime;
begin
  WriteLn('Test69_RoundDateDay:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to day should round to nearest day',
    EncodeDate(2024, 3, 16),
    TChronoKit.RoundDate(TestDate, duDay));
  WriteLn('Test69_RoundDateDay:Finished');
end;

procedure TDateTimeTests.Test70_RoundDateWeek;
var
  TestDate: TDateTime;
begin
  WriteLn('Test70_RoundDateWeek:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500); // March 15, 2024 is a Friday
  AssertEquals('Round to week should round to nearest Sunday',
    EncodeDate(2024, 3, 17), // Should round to next Sunday, March 17
    TChronoKit.RoundDate(TestDate, duWeek));
  WriteLn('Test70_RoundDateWeek:Finished');
end;

procedure TDateTimeTests.Test71_RoundDateMonth;
var
  TestDate: TDateTime;
begin
  WriteLn('Test71_RoundDateMonth:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to month should round to nearest month start',
    EncodeDate(2024, 4, 1),
    TChronoKit.RoundDate(TestDate, duMonth));
  WriteLn('Test71_RoundDateMonth:Finished');
end;

procedure TDateTimeTests.Test72_RoundDateBiMonth;
var
  TestDate: TDateTime;
begin
  WriteLn('Test72_RoundDateBiMonth:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to bi-month should round to nearest even month start',
    EncodeDate(2024, 3, 1),
    TChronoKit.RoundDate(TestDate, duBiMonth));
  WriteLn('Test72_RoundDateBiMonth:Finished');
end;

procedure TDateTimeTests.Test73_RoundDateQuarter;
var
  TestDate: TDateTime;
begin
  WriteLn('Test73_RoundDateQuarter:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to quarter should round to nearest quarter start',
    EncodeDate(2024, 4, 1),
    TChronoKit.RoundDate(TestDate, duQuarter));
  WriteLn('Test73_RoundDateQuarter:Finished');
end;

procedure TDateTimeTests.Test74_RoundDateHalfYear;
var
  TestDate: TDateTime;
begin
  WriteLn('Test74_RoundDateHalfYear:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to half year should round to nearest half year start',
    EncodeDate(2024, 7, 1),
    TChronoKit.RoundDate(TestDate, duHalfYear));
  WriteLn('Test74_RoundDateHalfYear:Finished');
end;

procedure TDateTimeTests.Test75_RoundDateYear;
var
  TestDate: TDateTime;
begin
  WriteLn('Test75_RoundDateYear:Starting');
  TestDate := EncodeDateTime(2024, 3, 15, 14, 30, 45, 500);
  AssertEquals('Round to year should round to nearest year start',
    EncodeDate(2024, 1, 1),
    TChronoKit.RoundDate(TestDate, duYear));
  WriteLn('Test75_RoundDateYear:Finished');
end;

procedure TDateTimeTests.Test76_YMD_Valid;
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

procedure TDateTimeTests.Test77_YMD_Invalid;
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

procedure TDateTimeTests.Test78_MDY_Valid;
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

procedure TDateTimeTests.Test79_MDY_Invalid;
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

procedure TDateTimeTests.Test80_DMY_Valid;
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

procedure TDateTimeTests.Test81_DMY_Invalid;
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

procedure TDateTimeTests.Test82_YQ_Valid;
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

procedure TDateTimeTests.Test83_YQ_Invalid;
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

procedure TDateTimeTests.Test84_ISOYear;
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

procedure TDateTimeTests.Test85_ISOWeek;
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

procedure TDateTimeTests.Test86_EpiYear;
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

procedure TDateTimeTests.Test87_EpiWeek;
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

procedure TDateTimeTests.Test88_Semester;
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

procedure TDateTimeTests.Test89_DateDecimal;
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

procedure TDateTimeTests.Test90_GetDecimalDate;
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

procedure TDateTimeTests.Test91_RollbackMonth;
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

procedure TDateTimeTests.Test92_RollForwardMonth;
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

procedure TDateTimeTests.Test93_PeriodToSeconds;
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

procedure TDateTimeTests.Test94_SecondsToPeriod;
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

procedure TDateTimeTests.Test95_StandardizePeriod;
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


procedure TDateTimeTests.Test96_IntervalAlign;
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

procedure TDateTimeTests.Test97_IntervalGap;
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

procedure TDateTimeTests.Test98_IntervalSetdiff;
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

procedure TDateTimeTests.Test99_IntervalUnion;
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

procedure TDateTimeTests.Test100_IntervalIntersection;
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
procedure TDateTimeTests.Test87a_EpiWeek_MidYear;
var
  TestDate: TDateTime;
begin
  WriteLn('Test87a_EpiWeek_MidYear:Starting');
  TestDate := EncodeDate(2024, 6, 15);
  AssertEquals('Mid-year date should have correct epi week', 
    24, TChronoKit.GetEpiWeek(TestDate));
  WriteLn('Test87a_EpiWeek_MidYear:Finished');
end;

procedure TDateTimeTests.Test87b_EpiWeek_FirstWeek;
var
  TestDate: TDateTime;
begin
  WriteLn('Test87b_EpiWeek_FirstWeek:Starting');
  TestDate := EncodeDate(2024, 1, 4);
  AssertEquals('First full week should be week 1', 
    1, TChronoKit.GetEpiWeek(TestDate));
  WriteLn('Test87b_EpiWeek_FirstWeek:Finished');
end;

procedure TDateTimeTests.Test87c_EpiWeek_YearEnd;
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
procedure TDateTimeTests.Test95a_StandardizePeriod_Milliseconds;
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

procedure TDateTimeTests.Test95b_StandardizePeriod_Seconds;
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

procedure TDateTimeTests.Test95c_StandardizePeriod_Minutes;
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

procedure TDateTimeTests.Test95d_StandardizePeriod_Hours;
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

procedure TDateTimeTests.Test95e_StandardizePeriod_Months;
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

procedure TDateTimeTests.Test95f_StandardizePeriod_Complex;
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
procedure TDateTimeTests.Test97a_IntervalGap_NoOverlap;
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

procedure TDateTimeTests.Test97b_IntervalGap_Overlapping;
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


procedure TDateTimeTests.Test101_TimeZoneInfoIsBounded;
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

procedure TDateTimeTests.Test102_SystemTimeZoneIsListed;
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

procedure TDateTimeTests.Test103_PortableUTCIsListed;
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

procedure TDateTimeTests.Test104_SameZoneConversionIsIdentity;
var
  ConvertedDate, TestDate: TDateTime;
begin
  TestDate := EncodeDateTime(2024, 6, 1, 12, 0, 0, 0);
  ConvertedDate := TChronoKit.WithTimeZone(
    TestDate, TChronoKit.GetSystemTimeZone);

  AssertEquals('Converting to the system timezone must be an identity',
    TestDate, ConvertedDate, OneMillisecond);
end;

procedure TDateTimeTests.Test105_LocalToUTCUsesSourceOffset;
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

procedure TDateTimeTests.Test106_UTCInterpretationUsesSystemOffset;
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

procedure TDateTimeTests.Test107_UTCRoundTripPreservesClock;
var
  LocalDate, RoundTrip, UTCDate: TDateTime;
begin
  LocalDate := EncodeDateTime(2024, 6, 1, 12, 0, 0, 0);
  UTCDate := TChronoKit.WithTimeZone(LocalDate, 'UTC');
  RoundTrip := TChronoKit.ForceTimeZone(UTCDate, 'UTC');

  AssertEquals('Local-to-UTC round trip must preserve the local clock',
    LocalDate, RoundTrip, OneMillisecond);
end;

procedure TDateTimeTests.Test108_UnsupportedTimeZonesRaise;
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

procedure TDateTimeTests.Test109_UTCOffsetBounds;
begin
  AssertEquals('Minimum UTC offset must be accepted',
    -12 * 60, TChronoKit.ValidateTimeZoneOffset(-12 * 60));
  AssertEquals('Maximum UTC offset must be accepted',
    14 * 60, TChronoKit.ValidateTimeZoneOffset(14 * 60));
end;

procedure TDateTimeTests.Test110_DSTStartMatrix;
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

procedure TDateTimeTests.Test111_DSTEndMatrix;
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

procedure TDateTimeTests.Test112_LeapYearDSTMatrix;
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

procedure TDateTimeTests.Test113_MalformedTimeZonesRaise;
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

procedure TDateTimeTests.Test114_UTCOffsetOutOfRangeRaises;
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

procedure TDateTimeTests.Test115_DateBoundaryConversion;
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

procedure TDateTimeTests.Test116_YMD;
var
  Expected: TDateTime;
begin
  Expected := EncodeDate(2024, 3, 15);
  WriteLn('Test116_YMD:Starting');
  AssertEquals('YMD with hyphen', Expected, FDateTime.YMD('2024-03-15'));
  AssertEquals('YMD with slash', Expected, FDateTime.YMD('2024/03/15'));
  WriteLn('Test116_YMD:Finished');
end;

procedure TDateTimeTests.Test117_MDY;
var
  Expected: TDateTime;
begin
  Expected := EncodeDate(2024, 3, 15);
  WriteLn('Test117_MDY:Starting');
  AssertEquals('MDY with hyphen', Expected, FDateTime.MDY('03-15-2024'));
  AssertEquals('MDY with slash', Expected, FDateTime.MDY('03/15/2024'));
  WriteLn('Test117_MDY:Finished');
end;

procedure TDateTimeTests.Test118_DMY;
var
  Expected: TDateTime;
begin
  Expected := EncodeDate(2024, 3, 15);
  WriteLn('Test118_DMY:Starting');
  AssertEquals('DMY with hyphen', Expected, FDateTime.DMY('15-03-2024'));
  AssertEquals('DMY with slash', Expected, FDateTime.DMY('15/03/2024'));
  WriteLn('Test118_DMY:Finished');
end;

procedure TDateTimeTests.Test119_SeasonalOffsetMatrix;
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

procedure TDateTimeTests.Test120_NamedTargetConversion;
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

procedure TDateTimeTests.Test121_SouthernHemisphereSeasonalRules;
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

procedure TDateTimeTests.Test122_NamedNonexistentTimeRaises;
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

procedure TDateTimeTests.Test123_NamedAmbiguousTimeRaises;
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

procedure TDateTimeTests.Test124_SystemNonexistentTimeRaises;
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

procedure TDateTimeTests.Test125_SystemAmbiguousTimeRaises;
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

procedure TDateTimeTests.Test126_LogicalZoneFixturesAreDiscoverable;
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

procedure TDateTimeTests.Test127_TargetZoneUsesDateSpecificOffset;
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

procedure TDateTimeTests.Test128_FutureRecurringRules;
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

procedure TDateTimeTests.Test149_CeilingDateRollsAcrossTimeBoundaries;
begin
  AssertEquals('Second ceiling must carry into the next minute',
    EncodeDateTime(2024, 3, 15, 14, 31, 0, 0),
    TChronoKit.CeilingDate(
      EncodeDateTime(2024, 3, 15, 14, 30, 59, 500), duSecond));
  AssertEquals('Minute ceiling must carry into the next hour',
    EncodeDateTime(2024, 3, 15, 15, 0, 0, 0),
    TChronoKit.CeilingDate(
      EncodeDateTime(2024, 3, 15, 14, 59, 30, 0), duMinute));
  AssertEquals('Hour ceiling must carry into the next day',
    EncodeDate(2024, 3, 16),
    TChronoKit.CeilingDate(
      EncodeDateTime(2024, 3, 15, 23, 30, 0, 0), duHour));
end;

procedure TDateTimeTests.Test150_EndBoundariesContainTheirStartingBoundary;
begin
  AssertEquals('EndOfYear at January 1 must end the containing year',
    EncodeDateTime(2024, 12, 31, 23, 59, 59, 999),
    TChronoKit.EndOfYear(EncodeDate(2024, 1, 1)), OneMillisecond);
  AssertEquals('EndOfWeek at Sunday midnight must end the containing week',
    EncodeDateTime(2024, 3, 9, 23, 59, 59, 999),
    TChronoKit.EndOfWeek(EncodeDate(2024, 3, 3)), OneMillisecond);
end;

procedure TDateTimeTests.Test151_DurationSpanDoesNotDoubleCountMilliseconds;
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

procedure TDateTimeTests.Test152_IntervalGapPreservesSubDayPrecision;
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

procedure TDateTimeTests.Test153_DecimalDateRoundTripPreservesTime;
var
  OriginalValue, RoundTrippedValue: TDateTime;
begin
  OriginalValue := EncodeDateTime(2024, 7, 2, 12, 34, 56, 789);
  RoundTrippedValue := TChronoKit.DateDecimal(
    TChronoKit.GetDecimalDate(OriginalValue));
  AssertEquals('Decimal-year legacy pair must round-trip time of day',
    OriginalValue, RoundTrippedValue, OneMillisecond);
end;

procedure TDateTimeTests.Test154_CreateIntervalRejectsReverseOrder;
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

procedure TDateTimeTests.Test155_SeasonRoundingRaises;
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

procedure TDateTimeTests.Test156_MonthRollingMatchesAddMonths;
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

procedure TDateTimeTests.Test157_DecimalYearReplacementRoundTrip;
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

procedure TDateTimeTests.Test158_CalendarPeriodArithmetic;
var
  Period, Normalized: TCalendarPeriod;
  StartValue: TDateTime;
  Raised: Boolean;
begin
  Period := TChronoKit.CreateCalendarPeriod(1, 13, 0, 25, 61, 61, 1001);
  Normalized := TChronoKit.NormalizeCalendarPeriod(Period);
  AssertEquals('Months must carry into years', 2, Normalized.Years);
  AssertEquals('Normalized month remainder', 1, Normalized.Months);
  AssertEquals('Hours must carry into days', 1, Normalized.Days);
  AssertEquals('Normalized hour remainder', 2, Normalized.Hours);
  AssertEquals('Normalized minute remainder', 2, Normalized.Minutes);
  AssertEquals('Normalized second remainder', 2, Normalized.Seconds);
  AssertEquals('Normalized millisecond remainder',
    1, Normalized.Milliseconds);

  StartValue := EncodeDateTime(2024, 1, 31, 10, 0, 0, 0);
  Period := TChronoKit.CreateCalendarPeriod(0, 1, 1, 2);
  AssertEquals('Periods apply months before days and time',
    EncodeDateTime(2024, 3, 1, 12, 0, 0, 0),
    TChronoKit.AddPeriod(StartValue, Period), OneMillisecond);
  AssertEquals('SubtractPeriod uses calendar month-end rules',
    EncodeDateTime(2024, 2, 29, 10, 0, 0, 0),
    TChronoKit.SubtractPeriod(
      EncodeDateTime(2024, 3, 31, 10, 0, 0, 0),
      TChronoKit.CreateCalendarPeriod(0, 1)), OneMillisecond);

  Raised := False;
  try
    TChronoKit.SubtractPeriod(StartValue,
      TChronoKit.CreateCalendarPeriod(Low(Integer)));
  except
    on E: ERangeError do
      Raised := True;
  end;
  AssertTrue('SubtractPeriod must reject an unrepresentable negation', Raised);
end;

procedure TDateTimeTests.Test159_ExactDurationArithmetic;
var
  Duration, Difference: TDuration;
  StartValue, EndValue: TDateTime;
begin
  Duration := TChronoKit.DurationFromParts(1, 2, 3, 4, 5);
  AssertEquals('Duration parts must produce exact elapsed milliseconds',
    Int64(93784005), Duration.Milliseconds);
  AssertEquals('Negative durations must remain exact',
    Int64(-3600000),
    TChronoKit.DurationFromParts(0, -1).Milliseconds);

  StartValue := EncodeDateTime(2024, 1, 1, 12, 0, 0, 0);
  Duration := TChronoKit.DurationFromSeconds(1);
  Duration.Milliseconds := Duration.Milliseconds + 500;
  EndValue := TChronoKit.AddDuration(StartValue, Duration);
  AssertEquals('AddDuration must add exact milliseconds',
    IncMilliSecond(StartValue, 1500), EndValue, OneMillisecond);
  AssertEquals('SubtractDuration must reverse exact elapsed addition',
    StartValue, TChronoKit.SubtractDuration(EndValue, Duration),
    OneMillisecond);

  Difference := TChronoKit.DurationBetween(StartValue, EndValue);
  AssertEquals('DurationBetween must round once to milliseconds',
    Int64(1500), Difference.Milliseconds);
  Difference := TChronoKit.DurationBetween(EndValue, StartValue);
  AssertEquals('DurationBetween must preserve direction',
    Int64(-1500), Difference.Milliseconds);
end;

procedure TDateTimeTests.Test160_DurationConstructionRejectsOverflow;
var
  Raised: Boolean;
begin
  Raised := False;
  try
    TChronoKit.DurationFromParts(High(Int64));
  except
    on E: ERangeError do
      Raised := True;
  end;
  AssertTrue('Day conversion overflow must raise ERangeError', Raised);

  Raised := False;
  try
    TChronoKit.DurationFromSeconds(High(Int64));
  except
    on E: ERangeError do
      Raised := True;
  end;
  AssertTrue('Second conversion overflow must raise ERangeError', Raised);
end;

procedure TDateTimeTests.Test161_HalfOpenRangeValidationAndContainment;
var
  RangeValue, EmptyRange: TDateTimeRange;
  Raised: Boolean;
begin
  RangeValue := TChronoKit.CreateRange(EncodeDate(2024, 1, 1),
    EncodeDate(2024, 1, 2));
  AssertTrue('A half-open range contains its start',
    TChronoKit.RangeContains(RangeValue, EncodeDate(2024, 1, 1)));
  AssertFalse('A half-open range excludes its end',
    TChronoKit.RangeContains(RangeValue, EncodeDate(2024, 1, 2)));

  EmptyRange := TChronoKit.CreateRange(EncodeDate(2024, 1, 1),
    EncodeDate(2024, 1, 1));
  AssertFalse('An empty range contains no values',
    TChronoKit.RangeContains(EmptyRange, EncodeDate(2024, 1, 1)));

  Raised := False;
  try
    TChronoKit.CreateRange(EncodeDate(2024, 1, 2),
      EncodeDate(2024, 1, 1));
  except
    on E: EArgumentException do
      Raised := Pos('start', LowerCase(E.Message)) > 0;
  end;
  AssertTrue('CreateRange must reject reversed endpoints', Raised);
end;

procedure TDateTimeTests.Test162_HalfOpenRangeRelationsAndGap;
var
  FirstRange, TouchingRange, OverlappingRange, DistantRange: TDateTimeRange;
  Gap: TDuration;
begin
  FirstRange := TChronoKit.CreateRange(
    EncodeDateTime(2024, 1, 1, 9, 0, 0, 0),
    EncodeDateTime(2024, 1, 1, 10, 0, 0, 0));
  TouchingRange := TChronoKit.CreateRange(
    EncodeDateTime(2024, 1, 1, 10, 0, 0, 0),
    EncodeDateTime(2024, 1, 1, 11, 0, 0, 0));
  OverlappingRange := TChronoKit.CreateRange(
    EncodeDateTime(2024, 1, 1, 9, 30, 0, 0),
    EncodeDateTime(2024, 1, 1, 10, 30, 0, 0));
  DistantRange := TChronoKit.CreateRange(
    EncodeDateTime(2024, 1, 1, 10, 0, 0, 1),
    EncodeDateTime(2024, 1, 1, 11, 0, 0, 0));

  AssertTrue('Ranges with equal adjacent endpoints touch',
    TChronoKit.RangesTouch(FirstRange, TouchingRange));
  AssertFalse('Touching half-open ranges do not overlap',
    TChronoKit.RangesOverlap(FirstRange, TouchingRange));
  AssertTrue('Ranges sharing included values overlap',
    TChronoKit.RangesOverlap(FirstRange, OverlappingRange));
  AssertEquals('RangeDuration returns exact elapsed milliseconds',
    Int64(60 * 60 * 1000),
    TChronoKit.RangeDuration(FirstRange).Milliseconds);

  Gap := TChronoKit.RangeGap(FirstRange, DistantRange);
  AssertEquals('RangeGap preserves a one-millisecond gap',
    Int64(1), Gap.Milliseconds);
  AssertEquals('Touching ranges have a zero gap', Int64(0),
    TChronoKit.RangeGap(FirstRange, TouchingRange).Milliseconds);
end;

procedure TDateTimeTests.Test163_SubtractRangeReturnsEveryRemainder;
var
  ValueRange, RemoveRange: TDateTimeRange;
  Results: TDateTimeRangeArray;
begin
  ValueRange := TChronoKit.CreateRange(EncodeDate(2024, 1, 1),
    EncodeDate(2024, 1, 10));
  RemoveRange := TChronoKit.CreateRange(EncodeDate(2024, 1, 4),
    EncodeDate(2024, 1, 6));
  Results := TChronoKit.SubtractRange(ValueRange, RemoveRange);
  AssertEquals('A middle removal must produce two ranges', 2, Length(Results));
  AssertEquals('Left remainder starts at original start',
    EncodeDate(2024, 1, 1), Results[0].StartValue);
  AssertEquals('Left remainder ends at removal start',
    EncodeDate(2024, 1, 4), Results[0].EndValue);
  AssertEquals('Right remainder starts at removal end',
    EncodeDate(2024, 1, 6), Results[1].StartValue);
  AssertEquals('Right remainder ends at original end',
    EncodeDate(2024, 1, 10), Results[1].EndValue);

  Results := TChronoKit.SubtractRange(ValueRange,
    TChronoKit.CreateRange(EncodeDate(2023, 12, 1),
      EncodeDate(2024, 2, 1)));
  AssertEquals('Complete removal returns no ranges', 0, Length(Results));

  Results := TChronoKit.SubtractRange(ValueRange,
    TChronoKit.CreateRange(EncodeDate(2024, 1, 10),
      EncodeDate(2024, 1, 12)));
  AssertEquals('A touching removal leaves the value unchanged',
    1, Length(Results));
  AssertEquals('Unchanged result preserves the end',
    ValueRange.EndValue, Results[0].EndValue);
end;

procedure TDateTimeTests.Test164_RangeTryOperationsAvoidSentinels;
var
  FirstRange, SecondRange, ResultRange: TDateTimeRange;
begin
  FirstRange := TChronoKit.CreateRange(0, 1);
  SecondRange := TChronoKit.CreateRange(0, 0.5);
  AssertTrue('A valid intersection may start at TDateTime zero',
    TChronoKit.TryIntersectRanges(FirstRange, SecondRange, ResultRange));
  AssertEquals('Intersection preserves the valid zero start',
    TDateTime(0), ResultRange.StartValue);
  AssertEquals('Intersection chooses the earlier end',
    TDateTime(0.5), ResultRange.EndValue);

  SecondRange := TChronoKit.CreateRange(1, 2);
  AssertTrue('Touching ranges can be merged',
    TChronoKit.TryMergeRanges(FirstRange, SecondRange, ResultRange));
  AssertEquals('Merged range spans both inputs', TDateTime(2),
    ResultRange.EndValue);

  SecondRange := TChronoKit.CreateRange(2, 3);
  AssertFalse('Disjoint ranges cannot be represented by one merge result',
    TChronoKit.TryMergeRanges(FirstRange, SecondRange, ResultRange));
  AssertFalse('Disjoint ranges have no intersection',
    TChronoKit.TryIntersectRanges(FirstRange, SecondRange, ResultRange));
end;

procedure TDateTimeTests.Test165_StartOfQuarterValidatesInputs;
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

procedure TDateTimeTests.Test166_ExplicitTimezoneNamesPreserveSemantics;
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

procedure TDateTimeTests.Test167_QuarterValueBoundaries;
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
end;

procedure TDateTimeTests.Test168_BusinessDaysBetweenCountsInclusiveDates;
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

procedure TDateTimeTests.Test169_ConvertBetweenTimeZonesPreservesInstant;
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
  RegisterTest(TDateTimeTests);
end.
