# ChronoKit-FP v1.2.0 release notes

Released 2026-08-10

## Business calendars

v1.2.0 adds explicit holiday and working-week rules without changing existing
business-day calls.

```pascal
Calendar := TChronoKit.CreateBusinessCalendar([
  EncodeDate(2026, 1, 1),
  EncodeDate(2026, 12, 25)
]);

DueDate := TChronoKit.AddBusinessDays(StartDate, 5, Calendar);
```

Use the second `CreateBusinessCalendar` overload for an alternative week such
as Sunday through Thursday. The calendar can be passed to `IsBusinessDay`,
`NextBusinessDay`, `PreviousBusinessDay`, and `AddBusinessDays`.

The [Business calendars guide](Business-Calendars.md) contains complete and
focused recipes for deadlines, reporting periods, and inclusive date ranges.
The accepted public contract and alternatives are recorded in the
[API design](Business-Calendar-API.md).

## Defined boundaries

- Existing calls still use Monday through Friday and do not infer holidays.
- Holiday time portions are ignored; calculation results preserve input time.
- Next and previous operations remain strict.
- Adding zero business days returns the exact input.
- Leap days, month ends, and Sunday-start working weeks have deterministic
  regression coverage.
- Calendars with no working day raise `EBusinessCalendarError` instead of
  searching indefinitely.

## Validation

Invalid `FromString`, `YMD`, `MDY`, `DMY`, and `YQ` inputs now produce
actionable `EConvertError` messages. Messages identify the rejected value and
state the accepted format, valid-calendar requirement, or quarter range.

## Compatibility

This is an additive, backwards-compatible 1.x release. No timezone contract or
timezone implementation changes from later milestones are included.
