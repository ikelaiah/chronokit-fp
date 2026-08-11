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
| add a calendar period or fixed duration | period, duration, span | `CreatePeriod`, `CreateDuration`, `AddSpan` |
| measure the difference between two values | difference, between, elapsed | `SpanBetween` |
| get or replace one component | year, month, day, hour, part | `GetYear`, `SetYear`, and the other component methods |
| find the start or end of a period | start, end, boundary | `StartOfDay`, `EndOfMonth`, and the other boundary methods |
| floor, ceiling, or round a value | floor, ceiling, truncate, round | `FloorDate`, `CeilingDate`, `RoundDate` |
| compare two dates | before, after, same, compare | `IsBefore`, `IsAfter`, `IsSameDay` |
| calculate weekdays or holidays | business, workday, holiday, deadline | `AddBusinessDays`, `CreateBusinessCalendar` |
| test, measure, or combine ranges | range, interval, overlap, gap, union | `CreateInterval` and the interval methods |
| get ISO or epidemiological week values | ISO, epidemiological, epi, week | `GetISOWeek`, `GetEpiWeek` |
| convert the same instant to another timezone | timezone, convert, target, UTC | `WithTimeZone` |
| interpret a clock from a named timezone | timezone, source, assign, force | `ForceTimeZone` |
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

`GetAsString` and `FromString` are the original 1.x names for the same
behavior. They remain supported; new code should prefer the task-oriented
`FormatDateTime` and `ParseDateTime` names.

For fixed-order input with `-` or `/` separators, use:

```pascal
Date1 := TChronoKit.YMD('2024-08-11');
Date2 := TChronoKit.MDY('08-11-2024');
Date3 := TChronoKit.DMY('11-08-2024');
QuarterStart := TChronoKit.YQ('2024-3');
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
  OneMonth, NinetyMinutes, Difference: TDateSpan;
begin
  OneMonth := TChronoKit.CreatePeriod(0, 1);
  NinetyMinutes := TChronoKit.CreateDuration(0, 0, 0, 0, 90);

  CalendarResult := TChronoKit.AddSpan(StartDate, OneMonth);
  ElapsedResult := TChronoKit.AddSpan(StartDate, NinetyMinutes);
  Earlier := TChronoKit.SubtractSpan(StartDate, NinetyMinutes);

  Difference := TChronoKit.SpanBetween(
    StartDate, EndDate, dskDuration);
end;
```

`dskPeriod` expresses calendar components. `dskDuration` expresses fixed
elapsed time. Avoid duration years and months when exact elapsed length
matters because those fields use fixed approximations.

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
`TDateUnit` values are `duSecond`, `duMinute`, `duHour`,
`duDay`, `duWeek`, `duMonth`, `duBiMonth`, `duQuarter`, `duSeason`,
`duHalfYear`, and `duYear`. `duSeason` is declared but not implemented in
v1.5.0; floor, ceiling, and round return the input unchanged for that unit.

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
end;
```

See [Business calendars](Business-Calendars.md) for alternative working weeks,
boundary rules, and deadline recipes. A calendar with no working days raises
`EBusinessCalendarError`.

## Ranges and intervals

```pascal
Workday := TChronoKit.CreateInterval(
  EncodeDateTime(2026, 8, 11, 9, 0, 0, 0),
  EncodeDateTime(2026, 8, 11, 17, 0, 0, 0));

if TChronoKit.IsWithinInterval(Value, Workday) then ...
if TChronoKit.IntervalsOverlap(FirstRange, SecondRange) then ...

Length := TChronoKit.IntervalLength(Workday, dskDuration);
Gap := TChronoKit.IntervalGap(FirstRange, SecondRange);
CommonRange := TChronoKit.IntervalIntersection(FirstRange, SecondRange);
CombinedRange := TChronoKit.IntervalUnion(FirstRange, SecondRange);
```

Intervals are inclusive. `IntervalUnion` returns an empty `0..0` interval when
the inputs have a gap. `IntervalIntersection` returns `0..0` when there is no
overlap. If subtraction would split an interval, `IntervalSetdiff` can return
only the first remaining interval; use a collection in application code when
both pieces are required.

## Calendar reporting

```pascal
ISOYear := TChronoKit.GetISOYear(Value);
ISOWeek := TChronoKit.GetISOWeek(Value);
EpiYear := TChronoKit.GetEpiYear(Value);
EpiWeek := TChronoKit.GetEpiWeek(Value);
Quarter := TChronoKit.GetQuarter(Value);
Semester := TChronoKit.GetSemester(Value);

DecimalValue := TChronoKit.GetDecimalDate(Value);
RestoredDate := TChronoKit.DateDecimal(DecimalValue);
```

ISO and epidemiological week years can differ from the calendar year near a
year boundary; read the year and week as a pair.

## Timezone conversion, UTC, and DST

`TDateTime` does not store a timezone name. Keep the intended name beside the
value when it matters.

```pascal
// Interpret Value in the computer's system timezone and preserve the instant.
UTCValue := TChronoKit.WithTimeZone(Value, 'UTC');

// Interpret UTCValue as a UTC wall clock and return a system-local wall clock.
SystemValue := TChronoKit.ForceTimeZone(UTCValue, 'UTC');

Info := TChronoKit.GetTimeZone(Value);
SystemZone := TChronoKit.GetSystemTimeZone;
Names := TChronoKit.GetTimeZoneNames;
```

For a named source, use its platform-native identifier and handle DST gaps and
overlaps:

```pascal
try
  SystemValue := TChronoKit.ForceTimeZone(InputValue, SourceTimeZone);
except
  on E: ETimeZoneError do
    WriteLn('The local clock cannot identify one instant: ', E.Message);
end;
```

`UTC` is the only portable identifier. Linux uses IANA names such as
`America/New_York`; Windows uses names such as `Eastern Standard Time`. See the
[timezone contract](Timezone-Contract.md) for mappings and exact failure
rules.

## Complete public method index

This index includes every public `TChronoKit` method. Overloads appear once.

| Task group | Methods |
|---|---|
| Current values and text | `GetNow`, `GetToday`, `GetDateTime`, `FormatDateTime`, `ParseDateTime`, `GetAsString`, `FromString` |
| Date/time components | `GetYear`, `GetMonth`, `GetDay`, `GetDayOfWeek`, `GetDayOfYear`, `GetHour`, `GetMinute`, `GetSecond`, `GetMillisecond`, `GetQuarter`, `GetSemester`, `IsAM`, `IsPM` |
| Replace components | `SetYear`, `SetMonth`, `SetDay`, `SetHour`, `SetMinute`, `SetSecond`, `SetMilliSecond` |
| Direct arithmetic | `AddYears`, `AddMonths`, `AddDays`, `AddHours`, `AddMinutes`, `AddSeconds`, `RollbackMonth`, `RollForwardMonth` |
| Boundaries and rounding | `StartOfYear`, `StartOfMonth`, `StartOfWeek`, `StartOfDay`, `StartOfHour`, `EndOfYear`, `EndOfMonth`, `EndOfWeek`, `EndOfDay`, `EndOfHour`, `FloorDate`, `CeilingDate`, `RoundDate` |
| Comparisons | `IsBefore`, `IsAfter`, `IsSameDay`, `IsSameMonth`, `IsSameYear` |
| Business calendars | `CreateBusinessCalendar`, `IsBusinessDay`, `NextBusinessDay`, `PreviousBusinessDay`, `AddBusinessDays` |
| Spans and durations | `CreatePeriod`, `CreateDuration`, `AddSpan`, `SubtractSpan`, `SpanBetween`, `PeriodToSeconds`, `SecondsToPeriod`, `StandardizePeriod` |
| Intervals and ranges | `CreateInterval`, `IsWithinInterval`, `IntervalsOverlap`, `IntervalLength`, `IntervalAlign`, `IntervalGap`, `IntervalSetdiff`, `IntervalUnion`, `IntervalIntersection` |
| Fixed input formats and reporting | `YMD`, `MDY`, `DMY`, `YQ`, `DateDecimal`, `GetDecimalDate`, `GetISOYear`, `GetISOWeek`, `GetEpiYear`, `GetEpiWeek` |
| Timezones | `GetTimeZone`, `GetSystemTimeZone`, `GetTimeZoneNames`, `IsValidTimeZoneName`, `IsValidUTCOffset`, `ValidateTimeZone`, `ValidateTimeZoneOffset`, `WithTimeZone`, `ForceTimeZone` |

## Public types and errors

| Type | Use |
|---|---|
| `TDateSpan`, `TDateSpanKind` | Calendar periods (`dskPeriod`) and fixed durations (`dskDuration`) |
| `TDateUnit` | Units accepted by floor, ceiling, and round operations |
| `TInterval` | Inclusive start/end range |
| `TBusinessCalendar`, `TBusinessWeek`, `TBusinessWeekday` | Working-week and holiday rules |
| `TTimeZoneInfo` | Platform name, offset in minutes east of UTC, and DST state |
| `EConvertError` | Invalid parsing input |
| `EBusinessCalendarError` | Invalid business-calendar rules |
| `ETimeZoneError` | Unsupported timezone, missing rules, DST gap, or DST overlap |
