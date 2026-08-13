# ChronoKit-FP decision guides

## Choose a value type

| If you mean… | Use | Avoid |
|---|---|---|
| A calendar date | `TDateTime` at midnight via `EncodeDate` | Adding a timezone that the task does not need |
| A local wall clock | `TDateTime` via `EncodeDateTime` or `GetNow` | Assuming it keeps a timezone name |
| “One month” or “two calendar days” | `TCalendarPeriod` | Converting months or years into seconds |
| Exact elapsed time | `TDuration` | A calendar period when elapsed milliseconds matter |
| A scheduled window | `TDateTimeRange` | An inclusive end boundary when adjacent windows must touch safely |
| Holiday and work-week rules | `TBusinessCalendar` | Repeating weekday checks throughout an application |

## Choose an operation

| Need | Preferred operation | Key rule |
|---|---|---|
| Add one calendar unit | `AddDays`, `AddMonths`, other `Add*` | A negative amount subtracts |
| Add several calendar components | `CreateCalendarPeriod` then `AddPeriod` | Months remain calendar-aware |
| Add elapsed time or measure it | `AddDuration` or `DurationBetween` | Durations are exact milliseconds |
| Find a period boundary | `StartOf*` or `EndOf*` | An `EndOf*` result is the final millisecond |
| Test an interval | `CreateRange` and range operations | Ranges are `[start, end)` |
| Find working days | `AddBusinessDays` or `BusinessDaysBetween` | Counting includes qualifying endpoints; adding does not count its start |
| Convert system-local to a named target | `SystemLocalToTimeZone` | Input belongs to the computer's system zone |
| Convert named source to system-local | `TimeZoneToSystemLocal` | Input belongs to the named source zone |
| Convert a named source to a named target | `ConvertBetweenTimeZones` | Input is a source-zone wall clock; output is the same instant in target-zone fields |

## Understand validation and timezone errors

| Situation | Result | What to do |
|---|---|---|
| Text cannot match its explicit parse format | `EConvertError` | Show the expected format and let the user correct the input |
| A custom calendar has no working days | `EBusinessCalendarError` | Select at least one `TBusinessWeekday` |
| A range ends before it starts | `EArgumentException` | Correct the ordering; equal endpoints are a valid empty range |
| Timezone name is empty, unsupported, or lacks data | `ETimeZoneError` | Use `UTC` or a name from `GetTimeZoneNames` on that platform |
| Source clock is in a DST forward gap | `ETimeZoneError` | Ask for a real local time |
| Source clock is in a DST backward overlap | `ETimeZoneError` | Ask the user to choose an unambiguous time; `TDateTime` cannot carry which occurrence they meant |

Catch error classes, not message text. See the [timezone contract](Timezone-Contract.md)
for supported names and exact DST rules.
