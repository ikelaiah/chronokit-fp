program ChronoKitExample;

{$mode objfpc}{$H+}{$J-}

{ This example demonstrates the date/time handling capabilities of ChronoKit's TChronoKit class.
  It shows various date and time operations including:
  - Basic operations (get current time, format dates, parse dates)
  - Component access and modification (year, month, day, etc)
  - Date arithmetic (add/subtract time units)
  - Period and interval handling
  - Period boundaries (start/end of year, month, etc)
  - Timezone operations
  - Date comparisons and rounding
  - Special date operations }

uses
  SysUtils,
  ChronoKit;

procedure DemonstrateBasicOperations;
var
  CurrentDateTime, TodayAtMidnight, ParsedDate: TDateTime;
  FormattedDate: string;
begin
  WriteLn('=== Basic Operations ===');
  
  // Get current date/time using TChronoKit helpers
  CurrentDateTime := TChronoKit.GetNow;      // Returns current date and time
  TodayAtMidnight := TChronoKit.GetToday;    // Returns today's date at midnight
  
  // Format dates using custom format strings
  // Format: yyyy=year, mm=month, dd=day, hh=hour, nn=minute, ss=second
  FormattedDate := TChronoKit.GetAsString(CurrentDateTime, 'yyyy-mm-dd hh:nn:ss');
  WriteLn('Current time (custom format): ', FormattedDate);
  WriteLn('Today at midnight (default format): ', TChronoKit.GetAsString(TodayAtMidnight, 'yyyy-mm-dd hh:nn:ss'));
  
  // Parse dates from strings using custom format patterns
  ParsedDate := TChronoKit.FromString('2024-03-15 14:30:00', 'yyyy-mm-dd hh:nn:ss');
  WriteLn('Parsed from string "2024-03-15 14:30:00": ', TChronoKit.GetAsString(ParsedDate, 'yyyy-mm-dd hh:nn:ss'));
  
  WriteLn;
  WriteLn('Common date format parsers:');
  WriteLn('  YMD "2024-03-15": ', TChronoKit.GetAsString(TChronoKit.YMD('2024-03-15'), 'yyyy-mm-dd'));
  WriteLn('  MDY "03-15-2024": ', TChronoKit.GetAsString(TChronoKit.MDY('03-15-2024'), 'yyyy-mm-dd'));
  WriteLn('  DMY "15-03-2024": ', TChronoKit.GetAsString(TChronoKit.DMY('15-03-2024'), 'yyyy-mm-dd'));
  WriteLn('  YQ "2024-1" (Q1): ', TChronoKit.GetAsString(TChronoKit.YQ('2024-1'), 'yyyy-mm-dd'));
end;

procedure DemonstrateComponentAccess;
var
  CurrentDateTime: TDateTime;
begin
  WriteLn('=== Component Access ===');
  CurrentDateTime := TChronoKit.GetNow;
  
  WriteLn('Current date/time: ', TChronoKit.GetAsString(CurrentDateTime, 'yyyy-mm-dd hh:nn:ss'));
  WriteLn;
  
  // Extract basic date/time components
  WriteLn('Basic components:');
  WriteLn('  Year: ', TChronoKit.GetYear(CurrentDateTime));           // 4-digit year
  WriteLn('  Month: ', TChronoKit.GetMonth(CurrentDateTime));         // 1-12
  WriteLn('  Day: ', TChronoKit.GetDay(CurrentDateTime));            // 1-31
  WriteLn('  Hour: ', TChronoKit.GetHour(CurrentDateTime));          // 0-23
  WriteLn('  Minute: ', TChronoKit.GetMinute(CurrentDateTime));      // 0-59
  WriteLn('  Second: ', TChronoKit.GetSecond(CurrentDateTime));      // 0-59
  WriteLn('  Millisecond: ', TChronoKit.GetMillisecond(CurrentDateTime)); // 0-999
  WriteLn;
  
  // Get additional calendar components
  WriteLn('Calendar components:');
  WriteLn('  Day of Week: ', TChronoKit.GetDayOfWeek(CurrentDateTime), ' (1=Sunday, 7=Saturday)');
  WriteLn('  Day of Year: ', TChronoKit.GetDayOfYear(CurrentDateTime), ' (1-366)');
  WriteLn('  Quarter: ', TChronoKit.GetQuarter(CurrentDateTime), ' (1-4)');
  WriteLn('  Semester: ', TChronoKit.GetSemester(CurrentDateTime), ' (1-2)');
  WriteLn;
  
  // Get ISO calendar components (international standard)
  WriteLn('ISO standard components:');
  WriteLn('  ISO Year: ', TChronoKit.GetISOYear(CurrentDateTime), ' (ISO week-numbering year)');
  WriteLn('  ISO Week: ', TChronoKit.GetISOWeek(CurrentDateTime), ' (1-53)');
  WriteLn;
  
  // Get epidemiological calendar components (used in healthcare)
  WriteLn('Epidemiological components:');
  WriteLn('  Epi Year: ', TChronoKit.GetEpiYear(CurrentDateTime), ' (epidemiological year)');
  WriteLn('  Epi Week: ', TChronoKit.GetEpiWeek(CurrentDateTime), ' (1-53)');
end;

procedure DemonstrateComponentModification;
var
  OriginalDateTime, ModifiedDateTime: TDateTime;
begin
  WriteLn('=== Component Modification ===');
  OriginalDateTime := TChronoKit.GetNow;
  WriteLn('Original: ', TChronoKit.GetAsString(OriginalDateTime, 'yyyy-mm-dd hh:nn:ss'));
  
  // Modify individual components while preserving others
  // Start with the original and apply changes step by step
  ModifiedDateTime := OriginalDateTime;
  ModifiedDateTime := TChronoKit.SetYear(ModifiedDateTime, 2025);           // Set year to 2025
  ModifiedDateTime := TChronoKit.SetMonth(ModifiedDateTime, 6);             // Set month to June
  ModifiedDateTime := TChronoKit.SetDay(ModifiedDateTime, 15);             // Set day to 15th
  ModifiedDateTime := TChronoKit.SetHour(ModifiedDateTime, 14);            // Set hour to 14 (2 PM)
  ModifiedDateTime := TChronoKit.SetMinute(ModifiedDateTime, 30);          // Set minute to 30
  ModifiedDateTime := TChronoKit.SetSecond(ModifiedDateTime, 45);          // Set second to 45
  ModifiedDateTime := TChronoKit.SetMilliSecond(ModifiedDateTime, 500);    // Set millisecond to 500
  
  WriteLn('Modified: ', TChronoKit.GetAsString(ModifiedDateTime, 'yyyy-mm-dd hh:nn:ss'));
  WriteLn('Target:   2025-06-15 14:30:45 (for comparison)');
end;

procedure DemonstrateDateArithmetic;
var
  StartDateTime, ResultDateTime: TDateTime;
begin
  WriteLn('=== Date Arithmetic ===');
  StartDateTime := TChronoKit.GetNow;
  WriteLn('Starting date: ', TChronoKit.GetAsString(StartDateTime, 'yyyy-mm-dd hh:nn:ss'));
  
  // Demonstrate various ways to add/subtract time units
  // Chain multiple operations to show cumulative effect
  ResultDateTime := StartDateTime;
  ResultDateTime := TChronoKit.AddYears(ResultDateTime, 1);        // Add 1 year
  ResultDateTime := TChronoKit.AddMonths(ResultDateTime, -2);      // Subtract 2 months  
  ResultDateTime := TChronoKit.AddDays(ResultDateTime, 7);         // Add 7 days
  ResultDateTime := TChronoKit.AddHours(ResultDateTime, 12);       // Add 12 hours
  ResultDateTime := TChronoKit.AddMinutes(ResultDateTime, 30);     // Add 30 minutes
  ResultDateTime := TChronoKit.AddSeconds(ResultDateTime, -15);    // Subtract 15 seconds
  
  WriteLn('After +1yr -2mo +7d +12h +30m -15s: ', TChronoKit.GetAsString(ResultDateTime, 'yyyy-mm-dd hh:nn:ss'));
  WriteLn;
  
  // Business day calculations (skipping weekends)
  WriteLn('Business day operations:');
  WriteLn('  Next business day: ', TChronoKit.GetAsString(TChronoKit.NextBusinessDay(StartDateTime), 'yyyy-mm-dd hh:nn:ss'));
  WriteLn('  Previous business day: ', TChronoKit.GetAsString(TChronoKit.PreviousBusinessDay(StartDateTime), 'yyyy-mm-dd hh:nn:ss'));
  WriteLn('  5 business days later: ', TChronoKit.GetAsString(TChronoKit.AddBusinessDays(StartDateTime, 5), 'yyyy-mm-dd hh:nn:ss'));
end;

procedure DemonstratePeriodOperations;
var
  StartDateTime: TDateTime;
  Period: TDateSpan;      // Represents a period of time (years, months, days)
  Duration: TDateSpan;    // Represents a duration in days
  Future: TDateTime;
  Span: TDateSpan;
  Seconds: Int64;
begin
  WriteLn('=== Period Operations ===');
  StartDateTime := TChronoKit.GetNow;
  WriteLn('Starting date: ', TChronoKit.GetAsString(StartDateTime, 'yyyy-mm-dd hh:nn:ss'));
  WriteLn;
  
  // Create different types of time spans
  Period := TChronoKit.CreatePeriod(1, 2, 3);      // 1 year, 2 months, 3 days
  Duration := TChronoKit.CreateDuration(0, 0, 1);  // 1 day duration
  
  WriteLn('Period operations:');
  // Add a period to a date
  Future := TChronoKit.AddSpan(StartDateTime, Period);
  WriteLn('  After adding period (1y 2m 3d): ', TChronoKit.GetAsString(Future, 'yyyy-mm-dd hh:nn:ss'));
  
  // Subtract a duration from a date
  Future := TChronoKit.SubtractSpan(Future, Duration);
  WriteLn('  After subtracting duration (1d): ', TChronoKit.GetAsString(Future, 'yyyy-mm-dd hh:nn:ss'));
  WriteLn;
  
  // Calculate the period between two dates
  Span := TChronoKit.SpanBetween(StartDateTime, Future, TDateSpanKind.dskPeriod);
  WriteLn('Period between start and final dates:');
  WriteLn('  Years: ', Span.Years);
  WriteLn('  Months: ', Span.Months);
  WriteLn('  Days: ', Span.Days);
  WriteLn;
  
  // Convert periods to/from seconds
  WriteLn('Period conversion:');
  Seconds := TChronoKit.PeriodToSeconds(Period);
  WriteLn('  Original period (1y 2m 3d) in seconds: ', Seconds);
  
  // Convert seconds back to a standardized period
  Period := TChronoKit.SecondsToPeriod(Seconds);
  Period := TChronoKit.StandardizePeriod(Period);  // Normalize the period
  WriteLn('  Standardized period from seconds:');
  WriteLn('    Years: ', Period.Years);
  WriteLn('    Months: ', Period.Months);
  WriteLn('    Days: ', Period.Days);
end;

procedure DemonstrateIntervalOperations;
var
  StartDateTime, LaterDateTime: TDateTime;
  Interval1, Interval2: TInterval;  // Represents a time interval with start/end dates
begin
  WriteLn('=== Interval Operations ===');
  StartDateTime := TChronoKit.GetNow;
  LaterDateTime := TChronoKit.AddDays(StartDateTime, 5);
  
  WriteLn('Base dates:');
  WriteLn('  Start: ', TChronoKit.GetAsString(StartDateTime, 'yyyy-mm-dd hh:nn:ss'));
  WriteLn('  Later: ', TChronoKit.GetAsString(LaterDateTime, 'yyyy-mm-dd hh:nn:ss'));
  WriteLn;
  
  // Create time intervals
  Interval1 := TChronoKit.CreateInterval(StartDateTime, LaterDateTime);                    // 5-day interval
  Interval2 := TChronoKit.CreateInterval(
    TChronoKit.AddDays(StartDateTime, 3),                                         // Starts 3 days from now
    TChronoKit.AddDays(StartDateTime, 8)                                          // Ends 8 days from now
  );
  
  WriteLn('Interval definitions:');
  WriteLn('  Interval1: 5-day span from start date');
  WriteLn('  Interval2: 5-day span starting 3 days from start date');
  WriteLn;
  
  // Perform interval checks
  WriteLn('Interval checks:');
  WriteLn('  Start date is within interval1: ',
    TChronoKit.IsWithinInterval(StartDateTime, Interval1));                       // Check if date is in interval
  WriteLn('  Intervals overlap: ',
    TChronoKit.IntervalsOverlap(Interval1, Interval2));                // Check if intervals overlap
  WriteLn('  Intervals align (adjacent): ',
    TChronoKit.IntervalAlign(Interval1, Interval2));                   // Check if intervals are adjacent
end;

procedure DemonstratePeriodBoundaries;
var
  CurrentDateTime, StartDate, EndDate: TDateTime;
begin
  WriteLn('=== Period Boundaries ===');
  CurrentDateTime := TChronoKit.GetNow;
  WriteLn('Current date/time: ', TChronoKit.GetAsString(CurrentDateTime, 'yyyy-mm-dd hh:nn:ss'));
  WriteLn;
  
  // Get start of various time periods
  WriteLn('Start of periods:');
  StartDate := TChronoKit.StartOfYear(CurrentDateTime);        // First moment of the year
  WriteLn('  Start of year: ', TChronoKit.GetAsString(StartDate, 'yyyy-mm-dd hh:nn:ss'));
  
  StartDate := TChronoKit.StartOfMonth(CurrentDateTime);       // First moment of the month
  WriteLn('  Start of month: ', TChronoKit.GetAsString(StartDate, 'yyyy-mm-dd hh:nn:ss'));
  
  StartDate := TChronoKit.StartOfWeek(CurrentDateTime);        // First moment of the week (Sunday)
  WriteLn('  Start of week: ', TChronoKit.GetAsString(StartDate, 'yyyy-mm-dd hh:nn:ss'));
  
  StartDate := TChronoKit.StartOfDay(CurrentDateTime);         // Midnight (00:00:00)
  WriteLn('  Start of day: ', TChronoKit.GetAsString(StartDate, 'yyyy-mm-dd hh:nn:ss'));
  WriteLn;
  
  // Get end of various time periods
  WriteLn('End of periods:');
  EndDate := TChronoKit.EndOfYear(CurrentDateTime);           // Last moment of the year
  WriteLn('  End of year: ', TChronoKit.GetAsString(EndDate, 'yyyy-mm-dd hh:nn:ss'));
  
  EndDate := TChronoKit.EndOfMonth(CurrentDateTime);          // Last moment of the month
  WriteLn('  End of month: ', TChronoKit.GetAsString(EndDate, 'yyyy-mm-dd hh:nn:ss'));
  
  EndDate := TChronoKit.EndOfWeek(CurrentDateTime);           // Last moment of the week (Saturday)
  WriteLn('  End of week: ', TChronoKit.GetAsString(EndDate, 'yyyy-mm-dd hh:nn:ss'));
  
  EndDate := TChronoKit.EndOfDay(CurrentDateTime);            // Last moment of day (23:59:59.999)
  WriteLn('  End of day: ', TChronoKit.GetAsString(EndDate, 'yyyy-mm-dd hh:nn:ss'));
end;

procedure DemonstrateTimezoneOperations;
var
  CurrentDateTime: TDateTime;
  TZInfo: TTimeZoneInfo;     // Contains timezone details (name, offset, DST status)
  SystemTZ: string;
  TZNames: TStringArray;
  UTC, Local: TDateTime;
  I: Integer;
begin
  WriteLn('=== Timezone Operations ===');
  CurrentDateTime := TChronoKit.GetNow;
  WriteLn('Current local time: ', TChronoKit.GetAsString(CurrentDateTime, 'yyyy-mm-dd hh:nn:ss'));
  WriteLn;
  
  // Get detailed timezone information
  TZInfo := TChronoKit.GetTimeZone(CurrentDateTime);
  WriteLn('Current timezone details:');
  WriteLn('  Name: ', TZInfo.Name);
  WriteLn('  Offset from UTC: ', TZInfo.Offset, ' minutes');    // Minutes from UTC
  WriteLn('  Is Daylight Saving: ', TZInfo.IsDST);              // Daylight Saving Time status
  WriteLn;
  
  // Get system timezone name
  SystemTZ := TChronoKit.GetSystemTimeZone;
  WriteLn('System timezone: ', SystemTZ);
  WriteLn;
  
  // List all available timezone names
  TZNames := TChronoKit.GetTimeZoneNames;
  WriteLn('Available timezones (', Length(TZNames), ' total):');
  for I := Low(TZNames) to High(TZNames) do
    WriteLn('  ', TZNames[I]);
  WriteLn;
  
  // Convert between timezones
  WriteLn('Timezone conversions:');
  UTC := TChronoKit.WithTimeZone(CurrentDateTime, 'UTC');      // Convert to UTC
  WriteLn('  UTC time: ', TChronoKit.GetAsString(UTC, 'yyyy-mm-dd hh:nn:ss'));
  
  Local := TChronoKit.WithTimeZone(UTC, SystemTZ); // Convert back to local time
  WriteLn('  Back to local: ', TChronoKit.GetAsString(Local, 'yyyy-mm-dd hh:nn:ss'));
end;

procedure DemonstrateDateComparisons;
var
  Date1, Date2: TDateTime;
begin
  WriteLn('=== Date Comparisons ===');
  Date1 := TChronoKit.GetNow;
  Date2 := TChronoKit.AddDays(Date1, 1);  // One day later
  
  WriteLn('Comparing dates:');
  WriteLn('  Date1: ', TChronoKit.GetAsString(Date1, 'yyyy-mm-dd hh:nn:ss'));
  WriteLn('  Date2: ', TChronoKit.GetAsString(Date2, 'yyyy-mm-dd hh:nn:ss'), ' (+1 day)');
  WriteLn;
  
  // Compare dates in various ways
  WriteLn('Comparison results:');
  WriteLn('  Date1 is before Date2: ',
    TChronoKit.IsBefore(Date1, Date2));            // True if Date1 < Date2
  WriteLn('  Date2 is after Date1: ',
    TChronoKit.IsAfter(Date2, Date1));             // True if Date2 > Date1
  WriteLn('  Same calendar day: ',
    TChronoKit.IsSameDay(Date1, Date2));           // True if same calendar day
  WriteLn('  Same calendar month: ',
    TChronoKit.IsSameMonth(Date1, Date2));         // True if same calendar month
  WriteLn('  Same calendar year: ',
    TChronoKit.IsSameYear(Date1, Date2));          // True if same calendar year
  WriteLn;
  
  // Check time of day
  WriteLn('Time of day checks for Date1:');
  WriteLn('  Is AM (before noon): ', TChronoKit.IsAM(Date1)); // Before noon
  WriteLn('  Is PM (after noon): ', TChronoKit.IsPM(Date1)); // After noon
end;

procedure DemonstrateDateRounding;
var
  OriginalDateTime, RoundedDateTime: TDateTime;
begin
  WriteLn('=== Date Rounding ===');
  OriginalDateTime := TChronoKit.GetNow;
  WriteLn('Original: ', TChronoKit.GetAsString(OriginalDateTime, 'yyyy-mm-dd hh:nn:ss'));
  WriteLn;
  
  // Round to nearest time unit
  WriteLn('Rounding to nearest unit:');
  RoundedDateTime := TChronoKit.RoundDate(OriginalDateTime, TDateUnit.duHour);    // Round to nearest hour
  WriteLn('  To nearest hour: ', TChronoKit.GetAsString(RoundedDateTime, 'yyyy-mm-dd hh:nn:ss'));
  
  RoundedDateTime := TChronoKit.RoundDate(OriginalDateTime, TDateUnit.duDay);     // Round to nearest day
  WriteLn('  To nearest day: ', TChronoKit.GetAsString(RoundedDateTime, 'yyyy-mm-dd hh:nn:ss'));
  
  RoundedDateTime := TChronoKit.RoundDate(OriginalDateTime, TDateUnit.duMonth);   // Round to nearest month
  WriteLn('  To nearest month: ', TChronoKit.GetAsString(RoundedDateTime, 'yyyy-mm-dd hh:nn:ss'));
  WriteLn;
  
  // Floor (round down) to time unit
  WriteLn('Floor operations (round down):');
  RoundedDateTime := TChronoKit.FloorDate(OriginalDateTime, TDateUnit.duHour);    // Beginning of current hour
  WriteLn('  Floor to hour: ', TChronoKit.GetAsString(RoundedDateTime, 'yyyy-mm-dd hh:nn:ss'));
  WriteLn;
  
  // Ceiling (round up) to time unit
  WriteLn('Ceiling operations (round up):');
  RoundedDateTime := TChronoKit.CeilingDate(OriginalDateTime, TDateUnit.duHour);  // Beginning of next hour
  WriteLn('  Ceiling to hour: ', TChronoKit.GetAsString(RoundedDateTime, 'yyyy-mm-dd hh:nn:ss'));
end;

procedure DemonstrateSpecialOperations;
var
  CurrentDateTime, PrevMonth, NextMonth: TDateTime;
  DecimalDate: Double;
  DateFromDecimal: TDateTime;
begin
  WriteLn('=== Special Operations ===');
  CurrentDateTime := TChronoKit.GetNow;
  WriteLn('Current date: ', TChronoKit.GetAsString(CurrentDateTime, 'yyyy-mm-dd hh:nn:ss'));
  WriteLn;
  
  // Month boundary operations with clearer explanations
  PrevMonth := TChronoKit.RollbackMonth(CurrentDateTime);      // Last day of previous month
  WriteLn('Month boundary operations:');
  WriteLn('  Last day of previous month: ', TChronoKit.GetAsString(PrevMonth, 'yyyy-mm-dd hh:nn:ss'));
  
  NextMonth := TChronoKit.RollForwardMonth(CurrentDateTime);   // First day of next month
  WriteLn('  First day of next month: ', TChronoKit.GetAsString(NextMonth, 'yyyy-mm-dd hh:nn:ss'));
  WriteLn;
  
  // Convert between decimal years and dates
  WriteLn('Decimal date conversion:');
  DecimalDate := TChronoKit.GetDecimalDate(CurrentDateTime);   // Convert to decimal year (e.g., 2024.45)
  WriteLn('  Date as decimal year: ', DecimalDate:0:4, ' (represents progress through the year)');
  
  DateFromDecimal := TChronoKit.DateDecimal(DecimalDate);      // Convert back to date
  WriteLn('  Back from decimal: ', TChronoKit.GetAsString(DateFromDecimal, 'yyyy-mm-dd hh:nn:ss'));
  WriteLn('  Note: Some precision may be lost in conversion');
end;

begin
  try
    // Run all demonstrations
    DemonstrateBasicOperations;
    WriteLn;
    DemonstrateComponentAccess;
    WriteLn;
    DemonstrateComponentModification;
    WriteLn;
    DemonstrateDateArithmetic;
    WriteLn;
    DemonstratePeriodOperations;
    WriteLn;
    DemonstrateIntervalOperations;
    WriteLn;
    DemonstratePeriodBoundaries;
    WriteLn;
    DemonstrateTimezoneOperations;
    WriteLn;
    DemonstrateDateComparisons;
    WriteLn;
    DemonstrateDateRounding;
    WriteLn;
    DemonstrateSpecialOperations;
    
    WriteLn;
    WriteLn('Press Enter to exit...');
    ReadLn;
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      ReadLn;
    end;
  end;
end. 
