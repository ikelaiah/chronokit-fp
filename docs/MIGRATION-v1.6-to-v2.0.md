# Migrating from ChronoKit-FP 1.6 to 2.0

ChronoKit-FP 1.6 keeps every 1.x declaration source compatible, but marks the
APIs scheduled for removal in 2.0 as deprecated. Migrate while still on 1.6 so
the compiler can identify remaining work. No deprecated declaration is removed
in the 1.6 line.

## Direct replacements

| Deprecated in 1.6 | Preferred 1.6 API or action |
|---|---|
| `GetDateTime(Value)` | Use `Value` directly. |
| `GetAsString` | `FormatDateTime` |
| `FromString` | `ParseDateTime` |
| `RollbackMonth(Value)` | `AddMonths(Value, -1)` |
| `RollForwardMonth(Value)` | `AddMonths(Value, 1)` |
| `YMD(Value)` | `ParseDateTime(Value, 'yyyy-mm-dd')` |
| `MDY(Value)` | `ParseDateTime(Value, 'mm-dd-yyyy')` |
| `DMY(Value)` | `ParseDateTime(Value, 'dd-mm-yyyy')` |
| `YQ(Value)` | Parse the year and quarter, then use `StartOfQuarter`. |
| `DateDecimal` | `DecimalYearToDateTime` |
| `GetDecimalDate` | `DateTimeToDecimalYear` |
| `GetTimeZone` | `GetSystemTimeZoneInfo` |
| `WithTimeZone` | `SystemLocalToTimeZone` |
| `ForceTimeZone` | `TimeZoneToSystemLocal` |
| `GetEpiYear` | `GetISOYear` if ISO weeks are intended; otherwise use a domain calendar. |
| `GetEpiWeek` | `GetISOWeek` if ISO weeks are intended; otherwise use a domain calendar. |

```pascal
Text := TChronoKit.FormatDateTime(Value, 'yyyy-mm-dd');
Value := TChronoKit.ParseDateTime(Text, 'yyyy-mm-dd');
QuarterStart := TChronoKit.StartOfQuarter(2026, 3);
PreviousMonth := TChronoKit.AddMonths(Value, -1);

DecimalValue := TChronoKit.DateTimeToDecimalYear(Value);
Value := TChronoKit.DecimalYearToDateTime(DecimalValue);

Info := TChronoKit.GetSystemTimeZoneInfo(LocalValue);
UTCValue := TChronoKit.SystemLocalToTimeZone(LocalValue, 'UTC');
LocalValue := TChronoKit.TimeZoneToSystemLocal(UTCValue, 'UTC');
```

## Split calendar periods from exact durations

`TDateSpanKind` and `TDateSpan` are replaced by two representations. Use
`TCalendarPeriod` for calendar-relative values containing years or months. Use
`TDuration` for an exact number of elapsed milliseconds.

| Deprecated in 1.6 | Preferred 1.6 API or action |
|---|---|
| `TDateSpanKind` | Choose `TCalendarPeriod` or `TDuration`; no tag is needed. |
| `TDateSpan` | `TCalendarPeriod` or `TDuration` |
| `CreatePeriod` | `CreateCalendarPeriod` |
| `CreateDuration` | `DurationFromParts` |
| `AddSpan` | `AddPeriod` or `AddDuration` |
| `SubtractSpan` | `SubtractPeriod` or `SubtractDuration` |
| `SpanBetween` | `DurationBetween`, or construct a domain-specific calendar period. |
| `PeriodToSeconds` | Use an exact duration and read `Milliseconds div 1000`. |
| `SecondsToPeriod` | `DurationFromSeconds` |
| `StandardizePeriod` | `NormalizeCalendarPeriod`; durations need no normalization. |

```pascal
Period := TChronoKit.CreateCalendarPeriod(0, 1);
NextMonth := TChronoKit.AddPeriod(Value, Period);

Duration := TChronoKit.DurationFromParts(0, 2, 30);
Later := TChronoKit.AddDuration(Value, Duration);
Elapsed := TChronoKit.DurationBetween(Value, Later);
WriteLn(Elapsed.Milliseconds);
```

Duration constructors use checked `Int64` arithmetic and raise `ERangeError`
on overflow. They never approximate a month or year as seconds.

## Move from inclusive intervals to half-open ranges

`TDateTimeRange` uses `[StartValue, EndValue)`: the start is included and the
end is excluded. Equal endpoints form an empty range. `CreateRange` rejects a
start later than its end.

| Deprecated in 1.6 | Preferred 1.6 API |
|---|---|
| `TInterval` | `TDateTimeRange` |
| `CreateInterval` | `CreateRange` |
| `IsWithinInterval` | `RangeContains` |
| `IntervalsOverlap` | `RangesOverlap` |
| `IntervalLength` | `RangeDuration` |
| `IntervalAlign` | `RangesTouch` |
| `IntervalGap` | `RangeGap` |
| `IntervalSetdiff` | `SubtractRange` |
| `IntervalUnion` | `TryMergeRanges` |
| `IntervalIntersection` | `TryIntersectRanges` |

```pascal
Workday := TChronoKit.CreateRange(StartValue, EndValue);
if TChronoKit.RangeContains(Workday, Candidate) then
  UseValue(Candidate);

if TChronoKit.TryIntersectRanges(First, Second, Common) then
  UseRange(Common);

Remaining := TChronoKit.SubtractRange(First, Second);
for I := Low(Remaining) to High(Remaining) do
  UseRange(Remaining[I]);
```

To preserve an old inclusive interval whose precision is milliseconds, create
the range with `OldInterval.EndDate + OneMillisecond` as its exclusive end.
For finer application precision, choose and document the appropriate smallest
unit instead.

## Replace seasonal rounding

The `duSeason` enum value cannot identify a hemisphere or a meteorological
versus astronomical definition. It cannot carry a Free Pascal deprecation
directive, so 1.6 documents it as deprecated and raises `EArgumentException`
when it is passed to `FloorDate`, `CeilingDate`, or `RoundDate`.

Use explicit domain dates. If calendar quarters are intended, use `duQuarter`
or `StartOfQuarter`.

## Removal list for 2.0

ChronoKit-FP 2.0 is expected to remove every declaration named in the tables
above, plus `duSeason`. The complete rationale and behavioural contract remain
in the [v1.6 API transition specification](API-Deprecations-v1.6.0.md).
