# ChronoKit-FP searchable cheat sheet

Use your browser or editor's find command and search for what you want to do:
`parse`, `format`, `today`, `add`, `subtract`, `difference`, `between`,
`start`, `end`, `round`, `business`, `holiday`, `range`, `interval`, `week`,
`timezone`, `UTC`, or `DST`.

Examples assume:

```pascal
uses
  SysUtils,
  ChronoKit;
```

## Find the operation by task

| I want to… | Search words | Start with |
|---|---|---|
| create a date or date/time | create, encode, construct | Free Pascal `EncodeDate`, `EncodeDateTime` |
| get today or the current local time | today, now, current | `GetToday`, `GetNow` |
| parse text as a date/time | parse, read, input, string | `ParseDateTime` |
| format a date/time for display | format, display, output, string | `FormatDateTime` |
| add or subtract calendar units | add, subtract, tomorrow, next | `AddDays`, `AddMonths`, other `Add*` methods |
| add a calendar period or fixed duration | period, duration, span | `CreateCalendarPeriod`, `DurationFromParts`, `AddPeriod`, `AddDuration` |
| measure the difference between two values | difference, between, elapsed | `DurationBetween` |
| get or replace one component | year, month, day, hour, part | `GetYear`, `SetYear`, and the other component methods |
| find the start or end of a period | start, end, boundary | `StartOfDay`, `EndOfMonth`, and the other boundary methods |
| floor, ceiling, or round a value | floor, ceiling, truncate, round | `FloorDate`, `CeilingDate`, `RoundDate` |
| compare two dates | before, after, same, compare | `IsBefore`, `IsAfter`, `IsSameDay` |
| calculate weekdays or holidays | business, workday, holiday, deadline | `AddBusinessDays`, `CreateBusinessCalendar` |
| count working dates in a period | business, count, between, reporting | `BusinessDaysBetween` |
| test, measure, or combine ranges | range, overlap, gap, union | `CreateRange` and the range methods |
| get ISO week values | ISO, week | `GetISOYear`, `GetISOWeek` |
| convert the same instant to another timezone | timezone, convert, target, UTC | `SystemLocalToTimeZone` |
| interpret a clock from a named timezone | timezone, source, assign | `TimeZoneToSystemLocal` |
| convert one named timezone directly to another | timezone, source, target, convert | `ConvertBetweenTimeZones` |
| validate a timezone name or offset | timezone, valid, offset | `ValidateTimeZone`, `ValidateTimeZoneOffset` |

## Create, parse, and format

```pascal
var
  CreatedDate, CreatedDateTime, ParsedDate: TDateTime;
  DisplayText: string;
begin
  CreatedDate := EncodeDate(2026, 8, 11);
  CreatedDateTime := EncodeDateTime(2026, 8, 11, 14, 5, 0, 0);

  ParsedDate := TChronoKit.ParseDateTime(
    '2026-08-11 14:05', 'yyyy-mm-dd hh:nn');
  DisplayText := TChronoKit.FormatDateTime(
    ParsedDate, 'dd mmm yyyy, hh:nn');
end;
```

Pass an explicit format for data from users, files, or APIs. In Free Pascal
format strings, `mm` is the month and `nn` is the minute. Parsing failures
raise `EConvertError`.

Use an explicit format for fixed-order input:

```pascal
Date1 := TChronoKit.ParseDateTime('2024-08-11', 'yyyy-mm-dd');
Date2 := TChronoKit.ParseDateTime('08-11-2024', 'mm-dd-yyyy');
Date3 := TChronoKit.ParseDateTime('11-08-2024', 'dd-mm-yyyy');
QuarterStart := TChronoKit.StartOfQuarter(2024, 3);
QuarterEnd := TChronoKit.EndOfQuarter(Value);
```

## Current values and components

```pascal
LocalNow := TChronoKit.GetNow;
TodayAtMidnight := TChronoKit.GetToday;

YearNumber := TChronoKit.GetYear(LocalNow);
MonthNumber := TChronoKit.GetMonth(LocalNow);
DayNumber := TChronoKit.GetDay(LocalNow);
HourNumber := TChronoKit.GetHour(LocalNow);

Changed := TChronoKit.SetYear(LocalNow, 2030);
Changed := TChronoKit.SetMonth(Changed, 12);
Changed := TChronoKit.SetDay(Changed, 25);
```

Setter methods return a new `TDateTime`; they do not mutate the input.

## Add, subtract, and measure

Use the direct methods for a single unit. A negative amount subtracts:

```pascal
Tomorrow := TChronoKit.AddDays(StartDate, 1);
LastWeek := TChronoKit.AddDays(StartDate, -7);
NextMonth := TChronoKit.AddMonths(StartDate, 1);
InNinetyMinutes := TChronoKit.AddMinutes(StartDate, 90);
```

Use a period for calendar concepts and a duration for fixed elapsed time:

```pascal
var
  OneMonth: TCalendarPeriod;
  NinetyMinutes, Difference: TDuration;
begin
  OneMonth := TChronoKit.CreateCalendarPeriod(0, 1);
  NinetyMinutes := TChronoKit.DurationFromParts(0, 0, 90);

  CalendarResult := TChronoKit.AddPeriod(StartDate, OneMonth);
  ElapsedResult := TChronoKit.AddDuration(StartDate, NinetyMinutes);
  Earlier := TChronoKit.SubtractDuration(StartDate, NinetyMinutes);

  Difference := TChronoKit.DurationBetween(StartDate, EndDate);
end;
```

Calendar periods apply components in order and may contain years or months.
Durations contain exact elapsed milliseconds and never approximate calendar
units.

## Boundaries and rounding

```pascal
DayStart := TChronoKit.StartOfDay(Value);
DayEnd := TChronoKit.EndOfDay(Value);
MonthStart := TChronoKit.StartOfMonth(Value);
MonthEnd := TChronoKit.EndOfMonth(Value);

HourFloor := TChronoKit.FloorDate(Value, duHour);
NextHour := TChronoKit.CeilingDate(Value, duHour);
NearestDay := TChronoKit.RoundDate(Value, duDay);
```

`CeilingDate` returns an upper boundary rather than the last representable
instant from an `EndOf*` method. Exact year and week boundaries remain
unchanged; the other implemented units advance to their next boundary. The
Supported `TDateUnit` values are `duSecond`, `duMinute`, `duHour`, `duDay`,
`duWeek`, `duMonth`, `duBiMonth`, `duQuarter`, `duHalfYear`, and `duYear`.
Seasonal rounding is deprecated because it needs a hemisphere and definition;
passing `duSeason` raises `EArgumentException`.

## Compare dates and times

```pascal
if TChronoKit.IsBefore(FirstDate, SecondDate) then ...
if TChronoKit.IsAfter(FirstDate, SecondDate) then ...
if TChronoKit.IsSameDay(FirstDate, SecondDate) then ...
if TChronoKit.IsSameMonth(FirstDate, SecondDate) then ...
if TChronoKit.IsSameYear(FirstDate, SecondDate) then ...
```

The `IsSame*` methods ignore smaller units: `IsSameDay` ignores time, while
`IsSameMonth` compares month and year.

## Business days, workdays, and holidays

Calls without a calendar use Monday to Friday:

```pascal
IsWorkday := TChronoKit.IsBusinessDay(Value);
NextWorkday := TChronoKit.NextBusinessDay(Value);
PreviousWorkday := TChronoKit.PreviousBusinessDay(Value);
DueDate := TChronoKit.AddBusinessDays(StartDate, 5);
```

Exclude holidays while keeping that working week:

```pascal
var
  Calendar: TBusinessCalendar;
begin
  Calendar := TChronoKit.CreateBusinessCalendar([
    EncodeDate(2026, 1, 1),
    EncodeDate(2026, 12, 25)
  ]);
  DueDate := TChronoKit.AddBusinessDays(StartDate, 5, Calendar);
  WorkingDates := TChronoKit.BusinessDaysBetween(StartDate, DueDate, Calendar);
end;
```

See [Business calendars](Business-Calendars.md) for alternative working weeks,
boundary rules, and deadline recipes. A calendar with no working days raises
`EBusinessCalendarError`. `BusinessDaysBetween` includes qualifying endpoints,
ignores input times, and returns a negative count for reversed dates.

## Half-open ranges

```pascal
Workday := TChronoKit.CreateRange(
  EncodeDateTime(2026, 8, 11, 9, 0, 0, 0),
  EncodeDateTime(2026, 8, 11, 17, 0, 0, 0));

if TChronoKit.RangeContains(Workday, Value) then ...
if TChronoKit.RangesOverlap(FirstRange, SecondRange) then ...

Length := TChronoKit.RangeDuration(Workday);
Gap := TChronoKit.RangeGap(FirstRange, SecondRange);
if TChronoKit.TryIntersectRanges(FirstRange, SecondRange, CommonRange) then ...
if TChronoKit.TryMergeRanges(FirstRange, SecondRange, CombinedRange) then ...
RemainingRanges := TChronoKit.SubtractRange(FirstRange, SecondRange);
```

Ranges include their start and exclude their end. Equal endpoints represent an
empty range. The `Try*` Boolean reports disjoint results, and subtraction can
return zero, one, or two ranges.

## Calendar reporting

```pascal
ISOYear := TChronoKit.GetISOYear(Value);
ISOWeek := TChronoKit.GetISOWeek(Value);
Quarter := TChronoKit.GetQuarter(Value);
Semester := TChronoKit.GetSemester(Value);
QuarterStart := TChronoKit.StartOfQuarter(Value);
QuarterEnd := TChronoKit.EndOfQuarter(Value);

DecimalValue := TChronoKit.DateTimeToDecimalYear(Value);
RestoredDate := TChronoKit.DecimalYearToDateTime(DecimalValue);
```

ISO week years can differ from the calendar year near a year boundary; read
the ISO year and week as a pair.

## Timezone conversion, UTC, and DST

`TDateTime` does not store a timezone name. Keep the intended name beside the
value when it matters.

```pascal
// Interpret Value in the computer's system timezone and preserve the instant.
UTCValue := TChronoKit.SystemLocalToTimeZone(Value, 'UTC');

// Interpret UTCValue as a UTC wall clock and return a system-local wall clock.
SystemValue := TChronoKit.TimeZoneToSystemLocal(UTCValue, 'UTC');

// Interpret a named source wall clock and represent the same instant in target.
TargetValue := TChronoKit.ConvertBetweenTimeZones(
  SourceValue, SourceTimeZone, TargetTimeZone);

Info := TChronoKit.GetSystemTimeZoneInfo(Value);
SystemZone := TChronoKit.GetSystemTimeZone;
Names := TChronoKit.GetTimeZoneNames;
```

For a named source, use its platform-native identifier and handle DST gaps and
overlaps:

```pascal
try
  SystemValue := TChronoKit.TimeZoneToSystemLocal(InputValue, SourceTimeZone);
except
  on E: ETimeZoneError do
    WriteLn('The local clock cannot identify one instant: ', E.Message);
end;
```

`UTC` is the only portable identifier. Linux uses IANA names such as
`America/New_York`; Windows uses names such as `Eastern Standard Time`. See the
[timezone contract](Timezone-Contract.md) for mappings and exact failure
rules.

## Preferred public method index

This index includes the preferred v1.7 `TChronoKit` methods. Deprecated 1.x
methods are indexed in the [migration guide](MIGRATION-v1.6-to-v2.0.md).

| Task group | Methods |
|---|---|
| Current values and text | `GetNow`, `GetToday`, `FormatDateTime`, `ParseDateTime` |
| Date/time components | `GetYear`, `GetMonth`, `GetDay`, `GetDayOfWeek`, `GetDayOfYear`, `GetHour`, `GetMinute`, `GetSecond`, `GetMillisecond`, `GetQuarter`, `GetSemester`, `IsAM`, `IsPM` |
| Replace components | `SetYear`, `SetMonth`, `SetDay`, `SetHour`, `SetMinute`, `SetSecond`, `SetMilliSecond` |
| Direct arithmetic | `AddYears`, `AddMonths`, `AddDays`, `AddHours`, `AddMinutes`, `AddSeconds` |
| Boundaries and rounding | `StartOfYear`, `StartOfQuarter`, `StartOfMonth`, `StartOfWeek`, `StartOfDay`, `StartOfHour`, `EndOfYear`, `EndOfQuarter`, `EndOfMonth`, `EndOfWeek`, `EndOfDay`, `EndOfHour`, `FloorDate`, `CeilingDate`, `RoundDate` |
| Comparisons | `IsBefore`, `IsAfter`, `IsSameDay`, `IsSameMonth`, `IsSameYear` |
| Business calendars | `CreateBusinessCalendar`, `IsBusinessDay`, `NextBusinessDay`, `PreviousBusinessDay`, `AddBusinessDays`, `BusinessDaysBetween` |
| Calendar periods and durations | `CreateCalendarPeriod`, `NormalizeCalendarPeriod`, `DurationFromParts`, `DurationFromSeconds`, `AddPeriod`, `SubtractPeriod`, `AddDuration`, `SubtractDuration`, `DurationBetween` |
| Half-open ranges | `CreateRange`, `RangeContains`, `RangesOverlap`, `RangeDuration`, `RangesTouch`, `RangeGap`, `SubtractRange`, `TryMergeRanges`, `TryIntersectRanges` |
| Calendar reporting | `DecimalYearToDateTime`, `DateTimeToDecimalYear`, `GetISOYear`, `GetISOWeek` |
| Timezones | `GetSystemTimeZoneInfo`, `GetSystemTimeZone`, `GetTimeZoneNames`, `IsValidTimeZoneName`, `IsValidUTCOffset`, `ValidateTimeZone`, `ValidateTimeZoneOffset`, `SystemLocalToTimeZone`, `TimeZoneToSystemLocal`, `ConvertBetweenTimeZones` |

## Public types and errors

| Type | Use |
|---|---|
| `TCalendarPeriod` | Calendar-relative years through milliseconds |
| `TDuration` | Exact elapsed milliseconds |
| `TDateUnit` | Units accepted by floor, ceiling, and round operations; do not use `duSeason` |
| `TDateTimeRange`, `TDateTimeRangeArray` | Validated half-open ranges and split results |
| `TBusinessCalendar`, `TBusinessWeek`, `TBusinessWeekday` | Working-week and holiday rules |
| `TTimeZoneInfo` | Platform name, offset in minutes east of UTC, and DST state |
| `EConvertError` | Invalid parsing input |
| `EBusinessCalendarError` | Invalid business-calendar rules |
| `ETimeZoneError` | Unsupported timezone, missing rules, DST gap, or DST overlap |
