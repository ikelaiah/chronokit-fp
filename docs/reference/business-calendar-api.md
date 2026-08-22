# Business calendar API

`TBusinessCalendar` supplies a working week and holiday dates to the
business-day overloads. Without an explicit calendar, ChronoKit uses Monday
through Friday and does not exclude holidays.

## Create a calendar

Create a default Monday-to-Friday calendar by supplying holidays, or provide
an explicit `TBusinessWeek` first:

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

The public record exposes `WorkingDays` and `Holidays`. Calls validate the
working week; a calendar with no working days raises `EBusinessCalendarError`.
Holidays are matched by calendar date, so their time portion is ignored.

## Apply rules

Pass the calendar as the final argument to `IsBusinessDay`,
`NextBusinessDay`, `PreviousBusinessDay`, `AddBusinessDays`, or
`BusinessDaysBetween`.

| Operation | Contract |
|---|---|
| `IsBusinessDay` | True only for a configured weekday that is not a holiday. |
| `NextBusinessDay` / `PreviousBusinessDay` | Strict navigation; never returns the input date. |
| `AddBusinessDays` | Does not count the starting date. Zero returns the exact input unchanged. |
| `BusinessDaysBetween` | Counts qualifying endpoints inclusively, ignores input times, and reverses sign when endpoints are reversed. |

Business-day operations preserve an input's time component when they return a
date. Duplicate holidays and holidays on non-working weekdays have no extra
effect.

For practical deadlines, reporting periods, and date-list recipes, see
[Business calendars](../Business-Calendars.md). The historic design record is
also retained at [Business-calendar API design](../Business-Calendar-API.md)
for links that predate this reference page.
