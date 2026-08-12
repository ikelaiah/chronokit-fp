# Business calendars

ChronoKit-FP uses Monday through Friday when you call a business-day helper
without a calendar. Create a `TBusinessCalendar` when you also need holidays or
a different working week.

```pascal
var
  Calendar: TBusinessCalendar;
begin
  Calendar := TChronoKit.CreateBusinessCalendar([
    EncodeDate(2026, 1, 1),
    EncodeDate(2026, 12, 25)
  ]);
end;
```

For a Sunday-to-Thursday week, pass the working days explicitly:

```pascal
Calendar := TChronoKit.CreateBusinessCalendar(
  [bwdSunday, bwdMonday, bwdTuesday, bwdWednesday, bwdThursday],
  [EncodeDate(2026, 1, 1)]
);
```

Pass the calendar as the final argument to `IsBusinessDay`,
`NextBusinessDay`, `PreviousBusinessDay`, or `AddBusinessDays`.

## Deadline recipe

This complete program calculates a five-business-day deadline while excluding
a public holiday:

```pascal
program BusinessDeadline;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils,
  ChronoKit;

var
  Calendar: TBusinessCalendar;
  Submitted, DueDate: TDateTime;
begin
  Calendar := TChronoKit.CreateBusinessCalendar([
    EncodeDate(2026, 8, 10)
  ]);
  Submitted := EncodeDate(2026, 8, 7);
  DueDate := TChronoKit.AddBusinessDays(Submitted, 5, Calendar);

  WriteLn(TChronoKit.FormatDateTime(DueDate, 'yyyy-mm-dd'));
end.
```

The result is `2026-08-17`. The starting date is not counted.

## Reporting-period recipe

Count the working days in a calendar month by checking each date in the
inclusive reporting range:

```pascal
function CountBusinessDays(const AStartDate, AEndDate: TDateTime;
  const ACalendar: TBusinessCalendar): Integer;
var
  CurrentDate: TDateTime;
begin
  Result := 0;
  CurrentDate := DateOf(AStartDate);
  while CurrentDate <= DateOf(AEndDate) do
  begin
    if TChronoKit.IsBusinessDay(CurrentDate, ACalendar) then
      Inc(Result);
    CurrentDate := TChronoKit.AddDays(CurrentDate, 1);
  end;
end;

WorkingDaysInAugust := CountBusinessDays(
  EncodeDate(2026, 8, 1),
  TChronoKit.EndOfMonth(EncodeDate(2026, 8, 1)),
  Calendar
);
```

Add `SysUtils`, `DateUtils`, and `ChronoKit` to the program's `uses` clause.

## Date-range recipe

Build an inclusive list by iterating over calendar dates and keeping only
those accepted by the calendar:

```pascal
CurrentDate := EncodeDate(2026, 8, 7);
EndDate := EncodeDate(2026, 8, 14);

while CurrentDate <= EndDate do
begin
  if TChronoKit.IsBusinessDay(CurrentDate, Calendar) then
    WriteLn(TChronoKit.FormatDateTime(CurrentDate, 'yyyy-mm-dd'));
  CurrentDate := TChronoKit.AddDays(CurrentDate, 1);
end;
```

This pattern makes both endpoints explicit. It is useful for report rows,
booking availability, and working-date exports.

## Boundary behavior

- Holidays are compared by date; a time stored in a holiday value is ignored.
- Returned dates preserve the input time.
- `NextBusinessDay` and `PreviousBusinessDay` are strict and never return the
  starting date.
- `AddBusinessDays` does not count the starting date. Zero returns the exact
  input, including when it falls on a non-working day.
- Leap days, month ends, and year ends follow normal calendar arithmetic.
- An empty working week raises `EBusinessCalendarError`; select at least one
  `TBusinessWeekday`.
- Duplicate holidays and holidays already outside the working week have no
  additional effect.

The accepted API contract and alternatives are recorded in
[Business-calendar API design](Business-Calendar-API.md).
