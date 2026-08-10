# Business-calendar API design for v1.2.0

**Status:** Accepted

**Reviewed:** 2026-08-10

## Context

The existing business-day helpers treat Monday through Friday as working days
and do not support holidays. v1.2.0 must make those rules configurable without
changing the result of any existing call.

The API needs to be readable to developers who are new to Free Pascal, work on
Windows and Linux without platform calendar data, and remain safe for static,
thread-friendly `TChronoKit` operations.

## Public model

```pascal
type
  TBusinessWeekday = (
    bwdSunday,
    bwdMonday,
    bwdTuesday,
    bwdWednesday,
    bwdThursday,
    bwdFriday,
    bwdSaturday
  );
  TBusinessWeek = set of TBusinessWeekday;
  TBusinessHolidayArray = array of TDateTime;

  TBusinessCalendar = record
    WorkingDays: TBusinessWeek;
    Holidays: TBusinessHolidayArray;
  end;

  EBusinessCalendarError = class(Exception);
```

Calendars are created with either a default Monday-to-Friday week or an
explicit working week:

```pascal
Calendar := TChronoKit.CreateBusinessCalendar([
  EncodeDate(2026, 1, 1),
  EncodeDate(2026, 12, 25)
]);

Calendar := TChronoKit.CreateBusinessCalendar(
  [bwdSunday, bwdMonday, bwdTuesday, bwdWednesday, bwdThursday],
  [EncodeDate(2026, 1, 1)]
);
```

Each existing business-day helper gains an overload whose final parameter is
the calendar:

```pascal
TChronoKit.IsBusinessDay(AValue, Calendar);
TChronoKit.NextBusinessDay(AValue, Calendar);
TChronoKit.PreviousBusinessDay(AValue, Calendar);
TChronoKit.AddBusinessDays(AValue, ADays, Calendar);
```

## Behavior contract

- Existing calls without a calendar retain Monday-to-Friday behavior and do
  not exclude holidays.
- A holiday is matched by date; its time component is ignored.
- `IsBusinessDay` returns true only when the weekday is in `WorkingDays` and
  the date is not a holiday.
- `NextBusinessDay` and `PreviousBusinessDay` are strict: they never return the
  input date.
- `AddBusinessDays` counts neither the starting date nor non-working dates.
  Passing zero returns the original value unchanged, even if it is not a
  business day.
- Date arithmetic preserves the input time component, including when crossing
  a weekend, holiday, month end, or leap day.
- A calendar with no working days raises `EBusinessCalendarError` with an
  actionable message instead of entering an unbounded search.
- Duplicate holidays and holidays on non-working weekdays are harmless.

## Alternatives considered

### Callback-based rules

A callback could express arbitrary rules, but it would make the common case
harder to discover, complicate examples, and introduce function-reference
compatibility concerns across supported Free Pascal modes. The v1.2.0 scope
only requires holidays and alternative weeks.

### Mutable calendar class

A class could hide its fields and validate once, but callers would need to
manage object lifetime for a small value. A record matches the library's
functional style. Public operations validate the working week because record
fields can be assigned directly.

### Replacing the current signatures

Default parameters or changed signatures could make existing source ambiguous
and would weaken the backwards-compatibility guarantee. Additive overloads are
clearer and leave current call sites untouched.

## Review outcome

The design was checked against the v1.2.0 roadmap and the existing public API.
It directly expresses both required rule types, has a bounded failure mode for
invalid calendars, preserves legacy defaults, and requires no dependency or
platform service. Implementation may proceed with tests written first.
