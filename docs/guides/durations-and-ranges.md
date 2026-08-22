# Durations, differences, and ranges

ChronoKit separates calendar-relative values from exact elapsed time. This
prevents a month from being silently treated as a fixed number of seconds.

## Choose a calendar period or an exact duration

Use `TCalendarPeriod` for a calendar concept such as “one month”. Use
`TDuration` for an exact elapsed amount such as 90 minutes or 24 hours:

```pascal
OneMonth := TChronoKit.CreateCalendarPeriod(0, 1);
NinetyMinutes := TChronoKit.DurationFromParts(0, 0, 90);

BillingDate := TChronoKit.AddPeriod(StartDate, OneMonth);
Expiry := TChronoKit.AddDuration(StartDate, NinetyMinutes);
Elapsed := TChronoKit.DurationBetween(StartDate, EndDate);
```

`DurationFromParts` and `DurationFromSeconds` use checked `Int64` arithmetic
and raise `ERangeError` on overflow. Durations never approximate months or
years. `NormalizeCalendarPeriod` is available when a calendar period needs
normalized components.

The runnable [periods and durations example](../../examples/LearningPath/02-PeriodsAndDurations.lpr)
shows why a calendar month and exactly 24 hours are different operations.

## Use half-open ranges for schedules

`TDateTimeRange` is a validated half-open range: it includes its start and
excludes its end. Equal endpoints represent an empty range.

```pascal
Workday := TChronoKit.CreateRange(
  EncodeDateTime(2026, 8, 17, 9, 0, 0, 0),
  EncodeDateTime(2026, 8, 17, 17, 0, 0, 0));

Contains := TChronoKit.RangeContains(Workday, Value);
Length := TChronoKit.RangeDuration(Workday);
Overlaps := TChronoKit.RangesOverlap(FirstRange, SecondRange);
```

Use `RangesTouch`, `RangeGap`, `SubtractRange`, `TryMergeRanges`, and
`TryIntersectRanges` for range algebra. The two `Try*` methods report a
disjoint result with `False`; subtraction may return zero, one, or two ranges.
The [half-open range example](../../examples/LearningPath/03-HalfOpenRanges.lpr)
prints the start/end boundary behaviour.
