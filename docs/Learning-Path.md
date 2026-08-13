# Learn ChronoKit-FP one concept at a time

This path uses the preferred v1.7 API only. Each step is a complete program
under `examples/LearningPath/`; the documentation links to its source instead
of copying a second version. Compile a program with the source-based command
from the README, or add the `ChronoKit` unit through the Lazarus package.

## 1. Dates and wall clocks

Run [01-DatesAndWallClocks.lpr](../examples/LearningPath/01-DatesAndWallClocks.lpr).

A `TDateTime` can represent a calendar **date** at midnight or an unzoned
**wall clock** with a time of day. Start here for ordinary dates. The program
also shows `StartOfQuarter(Value)` and `EndOfQuarter(Value)`: starts are at
midnight and ends are the last millisecond of the quarter.

## 2. Calendar periods and exact durations

Run [02-PeriodsAndDurations.lpr](../examples/LearningPath/02-PeriodsAndDurations.lpr).

Use `TCalendarPeriod` for calendar-relative language such as “one month”. Use
`TDuration` for exact elapsed time such as “24 hours”. Months and years are
never silently converted into seconds.

## 3. Half-open ranges

Run [03-HalfOpenRanges.lpr](../examples/LearningPath/03-HalfOpenRanges.lpr).

`TDateTimeRange` includes its start and excludes its end: `[start, end)`. This
makes adjoining bookings unambiguous—09:00 belongs to a 09:00–17:00 workday,
while 17:00 does not.

## 4. Business calendars

Run [04-BusinessCalendars.lpr](../examples/LearningPath/04-BusinessCalendars.lpr).

The default is Monday through Friday. Create a `TBusinessCalendar` when a
holiday or different work week matters. `BusinessDaysBetween` counts business
**dates** inclusively and ignores input times; reverse the dates to obtain a
negative count.

## 5. Named timezones and DST

Run [05-NamedTimeZones.lpr](../examples/LearningPath/05-NamedTimeZones.lpr).

Timezone conversion begins only when an application has a named source or
target zone. `ConvertBetweenTimeZones` interprets the input clock in the
source zone and returns the target clock for the same instant. A `TDateTime`
does not remember a timezone, so retain the target name beside it. `UTC` works
on Windows and Linux; other names are platform-native. A DST gap or overlap in
the **source** clock raises `ETimeZoneError` rather than guessing.

## Choose quickly

- Need a value type, operation, or error rule? Read the
  [decision guides](Decision-Guides.md).
- Need a task or method name? Search the generated
  [API reference](API-Reference.md) or the [cheat sheet](Cheat-Sheet.md).
- Need more detail about a named-zone error? Read the
  [timezone contract](Timezone-Contract.md).
