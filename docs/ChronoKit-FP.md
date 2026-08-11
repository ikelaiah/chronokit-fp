# ChronoKit-FP v1.5.0 task guide

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
| Add a multi-part span | `CreatePeriod` or `CreateDuration`, then `AddSpan` | Choose calendar meaning or fixed elapsed time. |
| Find a boundary | `StartOf*`, `EndOf*`, or rounding methods | A ceiling is the next boundary, not the end of a unit. |
| Compare or measure | `IsBefore`, `IsSameDay`, `SpanBetween` | Choose period or duration for a span. |
| Apply working-day rules | Business-calendar methods | Calls without a calendar use Monday to Friday. |
| Work with ranges | Interval methods | Intervals include both endpoints. |
| Convert a timezone | `WithTimeZone` or `ForceTimeZone` | Decide whether the input is system-local or belongs to a named source zone. |

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

`GetAsString` and `FromString` are the original 1.x names for the same
behavior. They remain supported for compatibility; new code should prefer the
names that say `Format` and `Parse` directly.

For fixed-order layouts, `YMD`, `MDY`, and `DMY` parse values with `-` or `/`
separators, and `YQ` parses a year and quarter such as `2026-3`.

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

`RollbackMonth` and `RollForwardMonth` move one month while handling a day that
does not exist in the target month. Use the direct `AddMonths` path unless that
specialized rolling operation is specifically needed.

### Period or duration?

A period expresses calendar units such as one month. A duration expresses
fixed elapsed units such as 90 minutes:

```pascal
var
  BillingPeriod, Timeout, Difference: TDateSpan;
begin
  BillingPeriod := TChronoKit.CreatePeriod(0, 1);
  Timeout := TChronoKit.CreateDuration(0, 0, 0, 0, 90);

  NextBillingDate := TChronoKit.AddSpan(StartDate, BillingPeriod);
  Expiry := TChronoKit.AddSpan(StartDate, Timeout);
  Previous := TChronoKit.SubtractSpan(StartDate, Timeout);

  Difference := TChronoKit.SpanBetween(
    StartDate, EndDate, dskDuration);
end;
```

`PeriodToSeconds`, `SecondsToPeriod`, and `StandardizePeriod` support explicit
span conversion and normalization. Duration months and years use fixed
approximations, so prefer days and smaller units when exact elapsed time
matters.

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
advance to their next boundary. `duSeason` is declared but not implemented in
v1.5.0 and returns the input unchanged. `StartOfWeek` follows the Sunday-based
week boundary; ISO week reporting is a separate operation.

## Compare and measure values

```pascal
if TChronoKit.IsBefore(FirstDate, SecondDate) then ...
if TChronoKit.IsAfter(FirstDate, SecondDate) then ...
if TChronoKit.IsSameDay(FirstDate, SecondDate) then ...
if TChronoKit.IsSameMonth(FirstDate, SecondDate) then ...
if TChronoKit.IsSameYear(FirstDate, SecondDate) then ...
```

Use `SpanBetween` for a structured difference. `dskPeriod` reports calendar
components; `dskDuration` reports fixed elapsed time.

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

## Work with intervals and ranges

```pascal
Range := TChronoKit.CreateInterval(StartDate, EndDate);
Contains := TChronoKit.IsWithinInterval(Value, Range);
Overlaps := TChronoKit.IntervalsOverlap(FirstRange, SecondRange);
Length := TChronoKit.IntervalLength(Range, dskDuration);
```

The extended operations are `IntervalAlign`, `IntervalGap`,
`IntervalSetdiff`, `IntervalUnion`, and `IntervalIntersection`. Empty results
use `StartDate = 0` and `EndDate = 0`. A set difference that splits one range
can return only the first remaining part, so model a list of intervals in
application code when both pieces are required.

## Produce calendar reports

Use year/week pairs near year boundaries:

```pascal
ISOYear := TChronoKit.GetISOYear(Value);
ISOWeek := TChronoKit.GetISOWeek(Value);
EpiYear := TChronoKit.GetEpiYear(Value);
EpiWeek := TChronoKit.GetEpiWeek(Value);
```

`GetQuarter` and `GetSemester` report larger calendar groupings.
`GetDecimalDate` converts a date to a decimal year and `DateDecimal` converts a
decimal year back to `TDateTime`.

## Convert timezones safely

ChronoKit's timezone functions operate on unzoned wall clocks:

- `GetTimeZone(Value)` interprets `Value` in the system zone and returns its
  name, minutes east of UTC, and DST state for that date.
- `WithTimeZone(Value, TargetZone)` interprets `Value` as system-local,
  preserves its instant, and returns the target-zone wall clock.
- `ForceTimeZone(Value, SourceZone)` interprets `Value` in the named source
  zone and returns the equivalent system-local wall clock.

```pascal
LocalValue := TChronoKit.GetNow;
UTCValue := TChronoKit.WithTimeZone(LocalValue, 'UTC');
SystemValue := TChronoKit.ForceTimeZone(UTCValue, 'UTC');
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
- [v1.5.0 API audit](API-Audit-v1.5.0.md): why the discovery changes were made.
- [2.0 decision](V2-DECISION.md): why the project is or is not proposing a
  major-version change.
