# ChronoKit-FP documentation

![ChronoKit-FP banner with a clock, calendar, and timezone paths](assets/chronokit-fp-banner-alternate.svg)

ChronoKit-FP is a practical, dependency-free toolkit for date, time,
business-calendar, range, and timezone work in Free Pascal and Lazarus.
It keeps ordinary `TDateTime` work simple while making the important choices
around business rules and named timezones explicit.

## Start with the task

- New to the library? Follow [Getting Started](Getting-Started.md), then the
  five runnable programs in the [Learning Path](Learning-Path.md).
- Need a working pattern? Open [Common recipes](guides/recipes.md) or the
  [Cheat Sheet](Cheat-Sheet.md).
- Choosing an operation or error-handling approach? Use the
  [Decision Guides](Decision-Guides.md).
- Looking up a declaration? Go straight to the generated
  [API Reference](API-Reference.md).

## What ChronoKit helps with

ChronoKit covers calendar arithmetic, parsing and formatting, exact durations,
half-open date/time ranges, working days and holidays, reporting boundaries,
and conversions between named timezones. The preferred public API is the
`ChronoKit` unit; all examples in the learning path compile against that
surface.

> [!IMPORTANT]
> `TDateTime` does not retain a timezone name. Keep the intended zone beside a
> value in your application, and handle `ETimeZoneError` when a source wall
> clock is ambiguous or does not exist at a DST transition.

## Platform support

ChronoKit-FP supports Free Pascal 3.2.2+ on Windows and Linux. `UTC` is the
only timezone identifier guaranteed on both platforms. Other names use the
operating system's native naming scheme; see the
[Timezone Contract](Timezone-Contract.md) before storing or accepting a
named zone.
