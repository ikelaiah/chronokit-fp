# 📋 ChronoKit-FP Cheat Sheet

A comprehensive reference for ChronoKit-FP DateTime operations and timezone handling.

## 🕙 DateTime Operations

### Basic Operations
```pascal
Now := TChronoKit.GetNow;                         // Current date and time
Today := TChronoKit.GetToday;                     // Current date (time = 00:00:00)
DateTime := TChronoKit.GetDateTime(Now);          // Validate/convert TDateTime
FormattedDate := TChronoKit.GetAsString(Now, 'yyyy-mm-dd hh:nn:ss');  // Format date
ParsedDate := TChronoKit.FromString('2024-07-15 10:30', 'yyyy-mm-dd hh:nn'); // Parse string
```

### Component Access
```pascal
Year := TChronoKit.GetYear(Now);                  // Extract year
Month := TChronoKit.GetMonth(Now);                // Extract month
Day := TChronoKit.GetDay(Now);                    // Extract day
DayOfWeek := TChronoKit.GetDayOfWeek(Now);        // Get day of week (1=Sunday)
DayOfYear := TChronoKit.GetDayOfYear(Now);        // Get day of year (1-366)
Hour := TChronoKit.GetHour(Now);                  // Extract hour
Minute := TChronoKit.GetMinute(Now);              // Extract minute
Second := TChronoKit.GetSecond(Now);              // Extract second
Millisecond := TChronoKit.GetMillisecond(Now);    // Extract millisecond
Quarter := TChronoKit.GetQuarter(Now);            // Get quarter (1-4)
IsAM := TChronoKit.IsAM(Now);                     // Check if time is AM
IsPM := TChronoKit.IsPM(Now);                     // Check if time is PM
```

### Component Modification
```pascal
// Set individual components
NewDate := TChronoKit.SetYear(Now, 2024);
NewDate := TChronoKit.SetMonth(Now, 5);
NewDate := TChronoKit.SetDay(Now, 15);
NewDate := TChronoKit.SetHour(Now, 10);
NewDate := TChronoKit.SetMinute(Now, 30);
NewDate := TChronoKit.SetSecond(Now, 45);
NewDate := TChronoKit.SetMillisecond(Now, 500);
```

### Date Arithmetic
```pascal
// Add or subtract time units
Tomorrow := TChronoKit.AddDays(Now, 1);
Yesterday := TChronoKit.AddDays(Now, -1);
NextMonth := TChronoKit.AddMonths(Now, 1);
NextYear := TChronoKit.AddYears(Now, 1);
OneHourLater := TChronoKit.AddHours(Now, 1);
ThirtyMinutesAgo := TChronoKit.AddMinutes(Now, -30);
OneMinuteLater := TChronoKit.AddSeconds(Now, 60);

// Business day calculations
IsWorkday := TChronoKit.IsBusinessDay(Now);
NextWorkday := TChronoKit.NextBusinessDay(Now);
PrevWorkday := TChronoKit.PreviousBusinessDay(Now);
FiveDaysLater := TChronoKit.AddBusinessDays(Now, 5);
```

### Period Operations
```pascal
// Create time spans
Period := TChronoKit.CreatePeriod(1, 2, 15);        // 1 year, 2 months, 15 days
Duration := TChronoKit.CreateDuration(0, 0, 0, 5, 30, 0, 0);  // 5h 30m

// Add/subtract time spans
NewDate := TChronoKit.AddSpan(Now, Period);
PastDate := TChronoKit.SubtractSpan(Now, Period);

// Calculate spans between dates
PeriodSpan := TChronoKit.SpanBetween(Date1, Date2, dskPeriod);
DurationSpan := TChronoKit.SpanBetween(Date1, Date2, dskDuration);
```

### Interval Operations
```pascal
// Create intervals
Interval := TChronoKit.CreateInterval(StartDate, EndDate);

// Check if date is within interval
if TChronoKit.IsWithinInterval(TestDate, Interval) then ...

// Check if intervals overlap
if TChronoKit.IntervalsOverlap(Interval1, Interval2) then ...

// Get interval duration
Duration := TChronoKit.IntervalLength(Interval, dskDuration);
```

### Date Comparison
```pascal
// Compare dates
if TChronoKit.IsBefore(Date1, Date2) then ...     // Date1 < Date2
if TChronoKit.IsAfter(Date1, Date2) then ...      // Date1 > Date2
if TChronoKit.IsSameDay(Date1, Date2) then ...    // Same date (ignore time)
if TChronoKit.IsSameMonth(Date1, Date2) then ...  // Same month and year
if TChronoKit.IsSameYear(Date1, Date2) then ...   // Same year
```

### Period Boundaries
```pascal
// Start of time period
StartOfDay := TChronoKit.StartOfDay(Now);
StartOfWeek := TChronoKit.StartOfWeek(Now);
StartOfMonth := TChronoKit.StartOfMonth(Now);
StartOfQuarter := TChronoKit.StartOfQuarter(Now);
StartOfYear := TChronoKit.StartOfYear(Now);

// End of time period
EndOfDay := TChronoKit.EndOfDay(Now);
EndOfWeek := TChronoKit.EndOfWeek(Now);
EndOfMonth := TChronoKit.EndOfMonth(Now);
EndOfQuarter := TChronoKit.EndOfQuarter(Now);
EndOfYear := TChronoKit.EndOfYear(Now);
```

### Date Rounding
```pascal
// Round down (floor) to time units
RoundToSecond := TChronoKit.FloorDate(Now, duSecond);  // 12:34:56.789 -> 12:34:56.000
RoundToMinute := TChronoKit.FloorDate(Now, duMinute);  // 12:34:56.789 -> 12:34:00.000
RoundToHour := TChronoKit.FloorDate(Now, duHour);      // 12:34:56.789 -> 12:00:00.000
RoundToDay := TChronoKit.FloorDate(Now, duDay);        // 2023-04-15 12:34 -> 2023-04-15 00:00
RoundToMonth := TChronoKit.FloorDate(Now, duMonth);    // 2023-04-15 -> 2023-04-01
RoundToYear := TChronoKit.FloorDate(Now, duYear);      // 2023-04-15 -> 2023-01-01

// Round up (ceiling) to time units
RoundToSecond := TChronoKit.CeilingDate(Now, duSecond); // 12:34:56.789 -> 12:34:57.000
RoundToMinute := TChronoKit.CeilingDate(Now, duMinute); // 12:34:56.789 -> 12:35:00.000
RoundToHour := TChronoKit.CeilingDate(Now, duHour);     // 12:34:56.789 -> 13:00:00.000
RoundToDay := TChronoKit.CeilingDate(Now, duDay);       // 2023-04-15 12:34 -> 2023-04-16 00:00
RoundToMonth := TChronoKit.CeilingDate(Now, duMonth);   // 2023-04-15 -> 2023-05-01
RoundToYear := TChronoKit.CeilingDate(Now, duYear);     // 2023-04-15 -> 2024-01-01
```

### Timezone Operations
```pascal
// Timezone information
TZInfo := TChronoKit.GetTimeZone(Now);
WriteLn('Timezone: ', TZInfo.Name);
WriteLn('Offset: ', TZInfo.Offset, ' minutes');
WriteLn('DST: ', BoolToStr(TZInfo.IsDST, True));

// Get available timezones
TZNames := TChronoKit.GetTimeZoneNames;
for I := Low(TZNames) to High(TZNames) do
  WriteLn(TZNames[I]);

// Cross-platform environment variable handling
// Save original timezone
OriginalTZ := GetEnvVar('TZ');
try
  // Set timezone for testing
  SetEnvVar('TZ', 'America/New_York');
  // Now timezone operations will use this setting...
  
  // Special date checks for DST transitions
  DSTDate := EncodeDateTime(2024, 10, 6, 2, 0, 0, 0); // First Sunday in October
  TZInfo := TChronoKit.GetTimeZone(DSTDate);
  if TZInfo.IsDST then
    WriteLn('Australian DST is in effect');
finally
  // Restore original timezone
  SetEnvVar('TZ', OriginalTZ);
end;

// DST transition examples for different regions
// Create date values near DST transition points
AustralianDate := EncodeDateTime(2024, 10, 6, 2, 0, 0, 0); // First Sunday in October
USDate := EncodeDateTime(2024, 3, 10, 2, 0, 0, 0);        // Second Sunday in March
EUDate := EncodeDateTime(2024, 3, 31, 1, 0, 0, 0);        // Last Sunday in March

// Check if each date is in DST period using GetTimeZone
AUInfo := TChronoKit.GetTimeZone(AustralianDate);
WriteLn('Australia DST active: ', BoolToStr(AUInfo.IsDST, True));

USInfo := TChronoKit.GetTimeZone(USDate);
WriteLn('US DST active: ', BoolToStr(USInfo.IsDST, True));

EUInfo := TChronoKit.GetTimeZone(EUDate);
WriteLn('EU DST active: ', BoolToStr(EUInfo.IsDST, True));
```

### Specialized Date Parsing
```pascal
// Parse various date formats
Date1 := TChronoKit.YMD(2024, 12, 25);              // Year-Month-Day
Date2 := TChronoKit.MDY(12, 25, 2024);              // Month-Day-Year  
Date3 := TChronoKit.DMY(25, 12, 2024);              // Day-Month-Year
Date4 := TChronoKit.YQ(2024, 4);                    // Year-Quarter

// Decimal date
DecimalDate := TChronoKit.DateDecimal(2024.5);      // Mid-year 2024
```

### ISO Date Functions
```pascal
// ISO year and week
ISOYear := TChronoKit.GetISOYear(SomeDate);
ISOWeek := TChronoKit.GetISOWeek(SomeDate);

// Epidemiological year and week
EpiYear := TChronoKit.GetEpiYear(SomeDate);
EpiWeek := TChronoKit.GetEpiWeek(SomeDate);
```

### Additional Date Operations
```pascal
// Round to nearest date unit
RoundedDate := TChronoKit.RoundDate(SomeDate, drsDay);
RoundedDate := TChronoKit.RoundDate(SomeDate, drsWeek);
RoundedDate := TChronoKit.RoundDate(SomeDate, drsMonth);
RoundedDate := TChronoKit.RoundDate(SomeDate, drsYear);

// Set specific time components
NewDate := TChronoKit.SetMillisecond(SomeDate, 500);
WeekStart := TChronoKit.StartOfWeek(SomeDate);
WeekEnd := TChronoKit.EndOfWeek(SomeDate);
```