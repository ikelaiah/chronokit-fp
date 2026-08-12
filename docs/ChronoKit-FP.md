# ChronoKit-FP v1.6.0 task guide

ChronoKit-FP is a dependency-free date/time toolkit for Free Pascal 3.2.2+ on
Windows and Linux. This guide is organized by what you need to accomplish. For
a compact exhaustive index, use the [searchable cheat sheet](Cheat-Sheet.md).

## Choose a task

| Need | Preferred operation | Important choice |
|---|---|---|
| Create a date or date/time | `EncodeDate`, `EncodeDateTime` | These are Free Pascal RTL functions returning `TDateTime`. |
| Get today or the current time | `GetToday`, `GetNow` | Both use the computer's local clock. |
| Parse input text | `ParseDateTime` | Pass an explicit format for portable input. |
| Format for display or output | `FormatDateTime` | `nn` means minute; `mm` means month. |
| Add one unit | `AddDays`, `AddMonths`, other `Add*` methods | Negative values subtract. |
| Add a multi-part value | `CreateCalendarPeriod` or `DurationFromParts` | Choose calendar meaning or exact elapsed time. |
| Find a boundary | `StartOf*`, `EndOf*`, or rounding methods | A ceiling is the next boundary, not the end of a unit. |
| Compare or measure | `IsBefore`, `IsSameDay`, `DurationBetween` | Duration differences are exact milliseconds. |
| Apply working-day rules | Business-calendar methods | Calls without a calendar use Monday to Friday. |
| Work with ranges | `CreateRange` and range methods | Ranges are half-open: `[start, end)`. |
| Convert a timezone | `SystemLocalToTimeZone` or `TimeZoneToSystemLocal` | Decide whether the input is system-local or belongs to a named source zone. |

## Understand the value type

ChronoKit uses Free Pascal's `TDateTime`:

- a date is conventionally a `TDateTime` at midnight;
- a local date/time is a wall-clock value, such as `GetNow`; and
- the value does not store a timezone name or distinguish repeated DST clocks.

Operations return new values; they do not modify their inputs. Keep a timezone
name beside a value in your application when that context must be retained.

## Create, parse, and format values

```pascal
var
  CreatedDate, ParsedDate: TDateTime;
  OutputText: string;
begin
  CreatedDate := EncodeDate(2026, 8, 11);
  ParsedDate := TChronoKit.ParseDateTime(
    '2026-08-11 09:30', 'yyyy-mm-dd hh:nn');
  OutputText := TChronoKit.FormatDateTime(
    ParsedDate, 'dd mmm yyyy, hh:nn');
end;
```

Parsing failures raise `EConvertError`. Omitting the format uses the system
date/time format and accepts `-` or `/` as the date separator, which is useful
for local input but not a portable storage contract.

The original 1.x aliases remain source compatible but are deprecated. Use
`ParseDateTime` with an explicit layout for fixed-order input and
`StartOfQuarter(Year, Quarter)` for a quarter boundary. The
[migration guide](MIGRATION-v1.6-to-v2.0.md) indexes every legacy name.

## Read or replace components

Component readers cover year, month, day, day of week, day of year, hour,
minute, second, millisecond, quarter, and semester:

```pascal
YearNumber := TChronoKit.GetYear(Value);
DayNumber := TChronoKit.GetDay(Value);
QuarterNumber := TChronoKit.GetQuarter(Value);
SemesterNumber := TChronoKit.GetSemester(Value);
```

Setters return a new value:

```pascal
Changed := TChronoKit.SetYear(Value, 2030);
Changed := TChronoKit.SetHour(Changed, 9);
```

Use `IsAM` and `IsPM` when only the time-of-day half matters.

## Add and subtract dates

The direct arithmetic methods cover years through seconds:

```pascal
Tomorrow := TChronoKit.AddDays(Value, 1);
LastMonth := TChronoKit.AddMonths(Value, -1);
InTwoHours := TChronoKit.AddHours(Value, 2);
```

`AddMonths` applies Free Pascal's calendar-aware month-end rules. Use an amount
of `-1` or `1` for previous or next month.

### Period or duration?

A period expresses calendar units such as one month. A duration expresses
fixed elapsed units such as 90 minutes:

```pascal
var
  BillingPeriod: TCalendarPeriod;
  Timeout, Difference: TDuration;
begin
  BillingPeriod := TChronoKit.CreateCalendarPeriod(0, 1);
  Timeout := TChronoKit.DurationFromParts(0, 0, 90);

  NextBillingDate := TChronoKit.AddPeriod(StartDate, BillingPeriod);
  Expiry := TChronoKit.AddDuration(StartDate, Timeout);
  Previous := TChronoKit.SubtractDuration(StartDate, Timeout);

  Difference := TChronoKit.DurationBetween(StartDate, EndDate);
end;
```

Normalize calendar components with `NormalizeCalendarPeriod`. Construct exact
durations with `DurationFromParts` or `DurationFromSeconds`; checked `Int64`
arithmetic raises `ERangeError` on overflow and never approximates months or
years.

## Find boundaries and round values

Use the named boundary methods when the unit is known in code:

```pascal
DayStart := TChronoKit.StartOfDay(Value);
WeekStart := TChronoKit.StartOfWeek(Value);
MonthEnd := TChronoKit.EndOfMonth(Value);
HourEnd := TChronoKit.EndOfHour(Value);
```

Use an enum for dynamic precision:

```pascal
HourFloor := TChronoKit.FloorDate(Value, duHour);
NextMonthBoundary := TChronoKit.CeilingDate(Value, duMonth);
NearestDay := TChronoKit.RoundDate(Value, duDay);
```

`CeilingDate` returns an upper boundary rather than an `EndOf*` value. Exact
year and week boundaries remain unchanged; the other implemented units
advance to their next boundary. Seasonal rounding needs a hemisphere and
definition, so passing deprecated `duSeason` raises `EArgumentException`.
`StartOfWeek` follows the Sunday-based week boundary; ISO week reporting is a
separate operation.

## Compare and measure values

```pascal
if TChronoKit.IsBefore(FirstDate, SecondDate) then ...
if TChronoKit.IsAfter(FirstDate, SecondDate) then ...
if TChronoKit.IsSameDay(FirstDate, SecondDate) then ...
if TChronoKit.IsSameMonth(FirstDate, SecondDate) then ...
if TChronoKit.IsSameYear(FirstDate, SecondDate) then ...
```

Use `DurationBetween` for an exact elapsed-millisecond difference. Calendar
component differences are domain-specific because month-end rules prevent one
context-free period from being the inverse of addition in both directions.

## Apply business-calendar rules

Without an explicit calendar, ChronoKit uses Monday through Friday:

```pascal
DueDate := TChronoKit.AddBusinessDays(StartDate, 5);
```

Add holidays or change the working week with `CreateBusinessCalendar`:

```pascal
var
  Calendar: TBusinessCalendar;
begin
  Calendar := TChronoKit.CreateBusinessCalendar([
    EncodeDate(2026, 1, 1),
    EncodeDate(2026, 12, 25)
  ]);

  IsWorking := TChronoKit.IsBusinessDay(Value, Calendar);
  NextWorking := TChronoKit.NextBusinessDay(Value, Calendar);
  PreviousWorking := TChronoKit.PreviousBusinessDay(Value, Calendar);
  DueDate := TChronoKit.AddBusinessDays(StartDate, 5, Calendar);
end;
```

The navigation methods are strict: they do not return the input day.
`AddBusinessDays(..., 0, ...)` returns the input unchanged. Invalid calendars
with no working days raise `EBusinessCalendarError`. See
[Business calendars](Business-Calendars.md) for full recipes.

## Work with half-open ranges

```pascal
Range := TChronoKit.CreateRange(StartDate, EndDate);
Contains := TChronoKit.RangeContains(Range, Value);
Overlaps := TChronoKit.RangesOverlap(FirstRange, SecondRange);
Length := TChronoKit.RangeDuration(Range);
```

Ranges include their start and exclude their end. Equal endpoints form an
empty range. Use `RangesTouch`, `RangeGap`, `SubtractRange`, `TryMergeRanges`,
and `TryIntersectRanges` for algebra without sentinel dates; subtraction
returns both pieces when a removal splits a range.

## Produce calendar reports

Use year/week pairs near year boundaries:

```pascal
ISOYear := TChronoKit.GetISOYear(Value);
ISOWeek := TChronoKit.GetISOWeek(Value);
```

`GetQuarter` and `GetSemester` report larger calendar groupings, and
`StartOfQuarter` constructs a validated quarter boundary.
`DateTimeToDecimalYear` converts a date/time to a decimal year and
`DecimalYearToDateTime` converts it back to within one millisecond.

## Convert timezones safely

ChronoKit's timezone functions operate on unzoned wall clocks:

- `GetSystemTimeZoneInfo(Value)` interprets `Value` in the system zone and returns its
  name, minutes east of UTC, and DST state for that date.
- `SystemLocalToTimeZone(Value, TargetZone)` interprets `Value` as system-local,
  preserves its instant, and returns the target-zone wall clock.
- `TimeZoneToSystemLocal(Value, SourceZone)` interprets `Value` in the named source
  zone and returns the equivalent system-local wall clock.

```pascal
LocalValue := TChronoKit.GetNow;
UTCValue := TChronoKit.SystemLocalToTimeZone(LocalValue, 'UTC');
SystemValue := TChronoKit.TimeZoneToSystemLocal(UTCValue, 'UTC');
```

`UTC` is the only portable identifier. Get exact platform-native values from
`GetTimeZoneNames`, inspect the current name with `GetSystemTimeZone`, and use
`IsValidTimeZoneName` or `ValidateTimeZone` before accepting a configured
name. Offset validation uses `IsValidUTCOffset` or `ValidateTimeZoneOffset` and
minutes east of UTC.

Ambiguous and nonexistent source clocks raise `ETimeZoneError`; ChronoKit does
not silently choose an occurrence. The [timezone contract](Timezone-Contract.md)
defines platform mappings and exact boundary behavior.

## Continue from here

- [Searchable cheat sheet](Cheat-Sheet.md): question index and every public
  method.
- [Getting Started](Getting-Started.md): installation and first program.
- [Business calendars](Business-Calendars.md): holidays, working weeks, and
  reporting recipes.
- [Timezone contract](Timezone-Contract.md): identifiers, conversions, and DST
  failures.
- [Troubleshooting](Troubleshooting.md): build paths, formats, and platforms.
- [v1.6 migration guide](MIGRATION-v1.6-to-v2.0.md): every deprecated name and
  its 2.0-safe replacement.
- [v1.6 release notes](RELEASE-NOTES-v1.6.0.md): additions, fixes, and
  compatibility guarantees.
