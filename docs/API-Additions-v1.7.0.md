# v1.7.0 focused workflow contract

**Status:** Accepted for implementation

**Scope:** This document defines the three and only three additive runtime
operations in v1.7.0. It was reviewed against the executable learning tasks
in `examples/LearningPath/` before implementation. Other audit observations
belong in `docs/API-Audit-v1.7.0.md` as post-2.0 design input; they do not
authorize more v1.x additions.

## Named source-to-named target conversion

```pascal
class function ConvertBetweenTimeZones(const AValue: TDateTime;
  const ASourceTimeZone, ATargetTimeZone: string): TDateTime; static;
```

`AValue` is a wall-clock value in `ASourceTimeZone`. The operation resolves
that clock to one UTC instant using the source zone's rules, then returns the
wall-clock representation of that instant in `ATargetTimeZone`.

- The result preserves the instant, not the clock fields.
- The returned `TDateTime` has no retained timezone identity. Applications
  keep `ATargetTimeZone` with the value when they need that context later.
- Both names use the existing platform-native identifier contract; `UTC` is
  the only portable identifier.
- An empty or unsupported source or target name, missing rule data, a source
  clock in a DST gap, or a source clock in a DST overlap raises
  `ETimeZoneError`. The target is derived from a unique instant and is never
  rejected merely because its displayed clock is in an overlap.
- The implementation resolves the source directly to UTC; it must not compose
  the two existing system-local operations, because an unrelated system-zone
  overlap could then reject a valid named-zone conversion.

## Quarter boundaries for a value

```pascal
class function StartOfQuarter(const AValue: TDateTime): TDateTime; overload; static;
class function EndOfQuarter(const AValue: TDateTime): TDateTime; static;
```

`StartOfQuarter(AValue)` returns midnight on 1 January, 1 April, 1 July, or
1 October in `AValue`'s calendar year. It ignores the input time portion.
`EndOfQuarter(AValue)` returns the last millisecond of the corresponding
quarter: 31 March, 30 June, 30 September, or 31 December at
`23:59:59.999`.

- They follow the existing `StartOf*` and `EndOf*` conventions.
- Leap years affect the first quarter's final day naturally; the fourth
  quarter ends in the input year even when its input contains a fractional
  time.
- `StartOfQuarter(Year, Quarter)` remains the explicit constructor and keeps
  its existing validation: `Year` is 1 through 9999 and `Quarter` is 1 through
  4. The value overload uses the valid calendar date already held by
  `TDateTime` and adds no new error rule.

## Business-day counting

```pascal
class function BusinessDaysBetween(const AStartDate,
  AEndDate: TDateTime): Integer; overload; static;
class function BusinessDaysBetween(const AStartDate, AEndDate: TDateTime;
  const ACalendar: TBusinessCalendar): Integer; overload; static;
```

The operation counts business **calendar dates** in the inclusive interval
between its endpoints.

- Both endpoints are included when they are business dates. A same-day range
  therefore returns `1` for a business date and `0` for a non-business date.
- If `AStartDate > AEndDate` by calendar date, the result is the negative of
  the equivalent forward count. Reversing the inputs always reverses the
  sign; equal dates are not negative.
- Time portions are accepted and ignored. This matches holiday and
  `IsBusinessDay` comparison semantics and lets callers pass values received
  from forms without stripping a time first.
- The no-calendar overload uses Monday through Friday and no holidays. The
  calendar overload applies its `WorkingDays` and holidays, including
  alternative weeks and date-only holiday matching.
- The calendar overload validates `ACalendar` exactly as the existing
  business-day helpers do. An empty working week raises
  `EBusinessCalendarError` with the established actionable error.

## Review result

The contracts give each workflow one explicit path and reuse existing
validation and timezone semantics. They require no aliases, RTL wrappers,
recurrence model, instant type, or zoned value type.
