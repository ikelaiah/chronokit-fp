# Date and time arithmetic

ChronoKit uses Free Pascal's `TDateTime`. Create ordinary dates with
`EncodeDate` and date/times with `EncodeDateTime`; a date is conventionally a
value at midnight. Operations return a new value and do not change the input.

## Add one calendar unit

Use `AddYears`, `AddMonths`, `AddDays`, `AddHours`, `AddMinutes`, or
`AddSeconds` when one unit expresses the intent. A negative amount subtracts:

```pascal
Tomorrow := TChronoKit.AddDays(Value, 1);
LastWeek := TChronoKit.AddDays(Value, -7);
NextMonth := TChronoKit.AddMonths(Value, 1);
InNinetyMinutes := TChronoKit.AddMinutes(Value, 90);
```

`AddMonths` follows Free Pascal's calendar-aware month-end rules. Use a
[calendar period or exact duration](durations-and-ranges.md) when a single
unit is not enough.

## Read and replace components

Use the `Get*` methods to read a component and `Set*` methods to return an
adjusted value:

```pascal
YearNumber := TChronoKit.GetYear(Value);
QuarterNumber := TChronoKit.GetQuarter(Value);
Changed := TChronoKit.SetYear(Value, 2030);
Changed := TChronoKit.SetHour(Changed, 9);
```

The API also provides `GetDayOfWeek`, `GetDayOfYear`, `GetMillisecond`,
`GetSemester`, `IsAM`, and `IsPM`. See the [API Reference](../API-Reference.md)
for the complete preferred method list.

## Boundaries and rounding

Use named boundaries when the unit is known in code:

```pascal
DayStart := TChronoKit.StartOfDay(Value);
WeekStart := TChronoKit.StartOfWeek(Value);
MonthEnd := TChronoKit.EndOfMonth(Value);
QuarterEnd := TChronoKit.EndOfQuarter(Value);
```

For a run-time precision, use `FloorDate`, `CeilingDate`, or `RoundDate` with
a supported `TDateUnit`. A ceiling is an upper boundary, not the final
representable instant from an `EndOf*` method. `duSeason` is deprecated and
raises `EArgumentException`; seasons need an application-specific hemisphere
and definition.

The executable [dates and wall clocks example](../../examples/LearningPath/01-DatesAndWallClocks.lpr)
demonstrates quarter boundaries with fixed input data.
