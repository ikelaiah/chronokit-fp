# ChronoKit-FP v1.6.0 API transition specification

**Status:** Accepted and implemented

**Target:** v1.6.0

**Removal target:** v2.0.0

**Decision date:** 2026-08-12

## Objective

v1.6.0 is the transition from the broad historical API to a smaller, explicit
2.0 surface. It adds replacements, marks superseded declarations as deprecated,
and fixes correctness defects, but removes no 1.x declaration.

This decision is based on repository evidence and maintainer experience:

- aliases and pass-through methods create multiple names for one operation;
- `TDateSpan` mixes calendar periods and fixed durations in one record whose
  fields have different meanings according to a tag;
- several interval operations cannot represent empty or split results with a
  single inclusive `TInterval` value;
- some names conceal important source, target, or conversion semantics; and
- several existing implementations lose precision or silently return a value
  that does not represent the requested operation.

User feedback can reprioritise this work, but is not a prerequisite or release
gate. The acceptance criteria below are the gate.

## Release contract

- v1.6.0 adds every replacement before deprecating its predecessor.
- Existing 1.x declarations remain source-compatible and continue to compile.
- Deprecated declarations emit a compiler warning with the replacement or
  migration direction in the message where Free Pascal supports it.
- A deprecated method delegates to the canonical replacement when their
  contracts are equivalent.
- A deprecated method with incompatible historical semantics remains isolated
  as legacy code until 2.0; new implementation code must not depend on it.
- v2.0.0 removes the declarations listed in this document after the v1.6.0
  tests, examples, and migration guide are complete.
- No elapsed-time API approximates a month or year as a fixed number of
  seconds.

## Replacement types

### Calendar periods and fixed durations

Replace the tagged `TDateSpan` representation with two types:

```pascal
type
  TCalendarPeriod = record
    Years: Integer;
    Months: Integer;
    Days: Integer;
    Hours: Integer;
    Minutes: Integer;
    Seconds: Integer;
    Milliseconds: Integer;
  end;

  TDuration = record
    { Exact elapsed milliseconds. }
    Milliseconds: Int64;
  end;
```

A calendar period is applied component by component and can contain years and
months. A duration is a fixed elapsed quantity and cannot contain approximate
calendar units.

The replacement operations are:

```pascal
class function CreateCalendarPeriod(const AYears: Integer = 0;
  const AMonths: Integer = 0; const ADays: Integer = 0;
  const AHours: Integer = 0; const AMinutes: Integer = 0;
  const ASeconds: Integer = 0;
  const AMilliseconds: Integer = 0): TCalendarPeriod; static;
class function NormalizeCalendarPeriod(
  const AValue: TCalendarPeriod): TCalendarPeriod; static;
class function DurationFromParts(const ADays: Int64 = 0;
  const AHours: Int64 = 0; const AMinutes: Int64 = 0;
  const ASeconds: Int64 = 0;
  const AMilliseconds: Int64 = 0): TDuration; static;
class function DurationFromSeconds(const ASeconds: Int64): TDuration; static;
class function AddPeriod(const AValue: TDateTime;
  const APeriod: TCalendarPeriod): TDateTime; static;
class function SubtractPeriod(const AValue: TDateTime;
  const APeriod: TCalendarPeriod): TDateTime; static;
class function AddDuration(const AValue: TDateTime;
  const ADuration: TDuration): TDateTime; static;
class function SubtractDuration(const AValue: TDateTime;
  const ADuration: TDuration): TDateTime; static;
class function DurationBetween(const AStart,
  AEnd: TDateTime): TDuration; static;
```

Duration constructors use checked `Int64` arithmetic and raise `ERangeError`
on overflow. `DurationBetween` rounds once to the nearest millisecond.
Calendar-component differences remain application-defined because month-end
rules prevent one context-free period from being the inverse of addition in
both directions.

### Half-open date/time ranges

Replace inclusive `TInterval` algebra with validated half-open ranges. A range
contains its start and excludes its end: `[StartValue, EndValue)`. Equal
endpoints represent an empty range without a sentinel date.

```pascal
type
  TDateTimeRange = record
    StartValue: TDateTime;
    EndValue: TDateTime;
  end;

  TDateTimeRangeArray = array of TDateTimeRange;
```

The replacement operations are:

```pascal
class function CreateRange(const AStart,
  AEnd: TDateTime): TDateTimeRange; static;
class function RangeContains(const ARange: TDateTimeRange;
  const AValue: TDateTime): Boolean; static;
class function RangesOverlap(const AFirst,
  ASecond: TDateTimeRange): Boolean; static;
class function RangeDuration(
  const ARange: TDateTimeRange): TDuration; static;
class function RangesTouch(const AFirst,
  ASecond: TDateTimeRange): Boolean; static;
class function RangeGap(const AFirst,
  ASecond: TDateTimeRange): TDuration; static;
class function SubtractRange(const AValue,
  ARemove: TDateTimeRange): TDateTimeRangeArray; static;
class function TryMergeRanges(const AFirst, ASecond: TDateTimeRange;
  out AMerged: TDateTimeRange): Boolean; static;
class function TryIntersectRanges(const AFirst, ASecond: TDateTimeRange;
  out AIntersection: TDateTimeRange): Boolean; static;
```

`CreateRange` raises `EArgumentException` when `AStart > AEnd`.
`SubtractRange` returns zero, one, or two ranges without discarding a valid
remainder. The `Try*` methods use their Boolean result instead of the valid
date `0` as an empty-result marker.

## Deprecation matrix

### Redundant and compatibility names

| Deprecated declaration | Replacement | Reason |
|---|---|---|
| `GetDateTime` | Use the input value directly | The method is an identity operation and performs no validation. |
| `GetAsString` | `FormatDateTime` | Two names expose the same formatting contract. |
| `FromString` | `ParseDateTime` | Two names expose the same parsing contract. |
| `RollbackMonth` | `AddMonths(Value, -1)` | Duplicates single-unit month arithmetic. |
| `RollForwardMonth` | `AddMonths(Value, 1)` | Duplicates single-unit month arithmetic. |

Month-end equivalence tests must pass before the rolling methods receive their
deprecation annotations.

### Fixed-format and specialist parsing

| Deprecated declaration | Replacement | Reason |
|---|---|---|
| `YMD` | `ParseDateTime(Value, 'yyyy-mm-dd')` or the slash equivalent | A terse format-specific parser duplicates the general explicit-format path. |
| `MDY` | `ParseDateTime(Value, 'mm-dd-yyyy')` or the slash equivalent | Same. |
| `DMY` | `ParseDateTime(Value, 'dd-mm-yyyy')` or the slash equivalent | Same. |
| `YQ` | Parse and validate the two integers, then call `StartOfQuarter(Year, Quarter)` | The name does not reveal that parsing returns the quarter's first date. |

v1.6.0 adds `StartOfQuarter(const AYear, AQuarter: Integer): TDateTime`,
which validates year `1..9999` and quarter `1..4`. It does not add another
string-specific quarter parser.

### Calendar periods and durations

| Deprecated declaration | Replacement | Reason |
|---|---|---|
| `TDateSpanKind` | Separate types; no tag required | A tag permits invalid combinations of kind and populated fields. |
| `TDateSpan` | `TCalendarPeriod` or `TDuration` | Calendar and elapsed time require different representations. |
| `CreatePeriod` | `CreateCalendarPeriod` | Returns the explicit calendar type. |
| `CreateDuration` | `DurationFromParts` | The old signature approximates months and years as fixed seconds. |
| `AddSpan` | `AddPeriod` or `AddDuration` | Removes runtime branching on span kind. |
| `SubtractSpan` | `SubtractPeriod` or `SubtractDuration` | Removes runtime branching on span kind. |
| `SpanBetween` | `DurationBetween` for elapsed time; construct a domain-specific `TCalendarPeriod` when calendar components are required | Month-end rules prevent one universal calendar-difference contract. |
| `PeriodToSeconds` | Use an exact `TDuration` and read `Milliseconds div 1000` | Calendar periods do not have a context-free second count. |
| `SecondsToPeriod` | `DurationFromSeconds` | Seconds describe a duration, not a calendar period. |
| `StandardizePeriod` | `NormalizeCalendarPeriod`; durations need no normalization | The old method can make a duration incompatible with `AddSpan`. |

### Intervals and ranges

| Deprecated declaration | Replacement | Reason |
|---|---|---|
| `TInterval` | `TDateTimeRange` | Half-open ranges can represent empty and adjacent ranges consistently. |
| `CreateInterval` | `CreateRange` | Adds ordering validation and explicit boundary semantics. |
| `IsWithinInterval` | `RangeContains` | Uses the new boundary contract. |
| `IntervalsOverlap` | `RangesOverlap` | Uses the new boundary contract. |
| `IntervalLength` | `RangeDuration` | Returns an exact duration rather than a tagged span. |
| `IntervalAlign` | `RangesTouch` | “Touch” states the boundary relation directly. |
| `IntervalGap` | `RangeGap` | Preserves sub-day and millisecond precision. |
| `IntervalSetdiff` | `SubtractRange` | Can return both pieces when removal splits a range. |
| `IntervalUnion` | `TryMergeRanges` | A Boolean represents a disjoint result without a sentinel interval. |
| `IntervalIntersection` | `TryIntersectRanges` | A Boolean represents no intersection without a sentinel interval. |

### Decimal-year conversion

| Deprecated declaration | Replacement | Reason |
|---|---|---|
| `DateDecimal` | `DecimalYearToDateTime` | The old name does not state the conversion direction. |
| `GetDecimalDate` | `DateTimeToDecimalYear` | The old name does not state the conversion direction. |

The new pair includes the time-of-day fraction and round-trips to within one
millisecond. The fractional part is measured over the actual length of the
selected year.

### Timezone direction

| Deprecated declaration | Replacement | Reason |
|---|---|---|
| `GetTimeZone` | `GetSystemTimeZoneInfo` | The input is interpreted specifically in the system timezone. |
| `WithTimeZone` | `SystemLocalToTimeZone` | The replacement names both the implicit source and explicit target. |
| `ForceTimeZone` | `TimeZoneToSystemLocal` | The replacement names both the explicit source and implicit target. |

The replacements retain the v1.3/v1.4 timezone contract, including errors for
ambiguous and nonexistent local times. These are naming migrations, not new
conversion semantics.

### Epidemiological week helpers

| Deprecated declaration | Replacement | Reason |
|---|---|---|
| `GetEpiYear` | `GetISOYear` when ISO weeks are intended; otherwise a domain-specific calendar | “Epidemiological week” does not identify one universal week convention. |
| `GetEpiWeek` | `GetISOWeek` when ISO weeks are intended; otherwise a domain-specific calendar | The current API has no parameter with which to select a convention. |

ChronoKit 2.0 will not guess a regional epidemiological convention.

### Rounding enum

`duSeason` is deprecated even though it is an enum value rather than a method.
Seasonal boundaries require a hemisphere and a definition such as
meteorological or astronomical seasons, neither of which `TDateUnit` can
express. Applications should use explicit domain dates; `duQuarter` remains
available when quarter boundaries are the intended rule.

## Migration examples

### Remove compatibility wrappers

```pascal
// Before
Text := TChronoKit.GetAsString(Value, 'yyyy-mm-dd');
Value := TChronoKit.FromString(Text, 'yyyy-mm-dd');
Previous := TChronoKit.RollbackMonth(Value);

// v1.6 and 2.0 path
Text := TChronoKit.FormatDateTime(Value, 'yyyy-mm-dd');
Value := TChronoKit.ParseDateTime(Text, 'yyyy-mm-dd');
Previous := TChronoKit.AddMonths(Value, -1);
```

### Separate a calendar period from a duration

```pascal
// Before: meaning depends on TDateSpan.Kind
Span := TChronoKit.CreatePeriod(0, 1);
NextMonth := TChronoKit.AddSpan(Value, Span);
Elapsed := TChronoKit.SpanBetween(StartValue, EndValue, dskDuration);

// v1.6 and 2.0 path
Period := TChronoKit.CreateCalendarPeriod(0, 1);
NextMonth := TChronoKit.AddPeriod(Value, Period);
Elapsed := TChronoKit.DurationBetween(StartValue, EndValue);
```

### Replace interval sentinels and truncated set difference

```pascal
// Before: 0..0 means no result and a split can lose its second piece
Common := TChronoKit.IntervalIntersection(First, Second);
Remaining := TChronoKit.IntervalSetdiff(First, Second);

// v1.6 and 2.0 path
if TChronoKit.TryIntersectRanges(FirstRange, SecondRange, CommonRange) then
  UseRange(CommonRange);

RemainingRanges := TChronoKit.SubtractRange(FirstRange, SecondRange);
for I := Low(RemainingRanges) to High(RemainingRanges) do
  UseRange(RemainingRanges[I]);
```

When migrating an inclusive interval whose end has millisecond precision, use
`EndValue := OldInterval.EndDate + OneMillisecond` to obtain the equivalent
half-open range. Callers using finer precision must choose and document their
own smallest unit instead of relying on an implicit adjustment.

### Make timezone direction visible

```pascal
// Before
UTCValue := TChronoKit.WithTimeZone(LocalValue, 'UTC');
LocalValue := TChronoKit.ForceTimeZone(UTCValue, 'UTC');

// v1.6 and 2.0 path
UTCValue := TChronoKit.SystemLocalToTimeZone(LocalValue, 'UTC');
LocalValue := TChronoKit.TimeZoneToSystemLocal(UTCValue, 'UTC');
```

## Correctness work separate from deprecation

The following are implementation defects, not reasons to remove otherwise
useful APIs:

1. `CeilingDate` must cross day, month, and year boundaries without passing
   hour `24`, minute `60`, or second `60` to `EncodeTime`.
2. `EndOfYear` and `EndOfWeek` must return the end of the containing unit when
   the input is already at its first exact boundary.
3. Legacy `SpanBetween(..., dskDuration)` must not double-count fractional
   seconds while it remains available in 1.6.
4. Legacy `IntervalGap` must preserve sub-day precision while it remains
   available in 1.6.
5. `DateDecimal` and `GetDecimalDate` must delegate to the corrected decimal-
   year conversion pair so their documented inverse relationship is true.
6. `CreateInterval` must reject a start later than its end while it remains
   available in 1.6.
7. Passing deprecated `duSeason` to a rounding operation must raise a clear
   `EArgumentException` instead of silently returning the input.

Each correction requires a regression test that fails against v1.5.0 before
the implementation changes.

## Reviewed and retained

The following public groups remain part of the planned 2.0 surface:

- `GetNow`, `GetToday`, and the preferred parsing and formatting methods;
- component getters and setters;
- direct `Add*`, `StartOf*`, and `EndOf*` operations;
- `FloorDate`, `CeilingDate`, and `RoundDate` after their boundary fixes;
- comparisons and business-calendar operations;
- `GetQuarter`, `GetSemester`, `IsAM`, and `IsPM`;
- ISO year/week reporting; and
- timezone discovery and validation methods other than the three directional
  names replaced above.

These methods are direct, discoverable, or domain-relevant, and removing them
would not eliminate a conflicting representation or implementation path.

## Project structure and implementation boundaries

- Public types and methods remain in `src/ChronoKit.pas` for v1.6 source
  compatibility.
- Tests remain behavior-focused and must be added before each implementation
  slice. Since v1.7, they live in the matching `tests/ChronoKit.*.Tests.pas`
  domain suite.
- User migration documentation belongs in `docs/` and must use the preferred
  APIs in every new example.
- No new runtime dependency is allowed.
- Do not remove a deprecated declaration, change timezone rules, or weaken a
  validation error during v1.6 implementation.
- Keep deprecated implementations out of new canonical code paths.

## Code style

- Follow the repository's Object Pascal conventions: PascalCase declarations,
  `A`-prefixed parameters, two-space indentation, and public API comments that
  explain contracts and failure behaviour.
- Keep replacement functions static and side-effect free, matching the
  existing functional `TChronoKit` surface.
- Prefer separate, direct implementations over branching on a mode tag.
- Use checked conversions and explicit exceptions instead of sentinel values
  or silent fallback behaviour.

## Testing strategy and commands

Tests must cover replacement behavior, legacy compatibility, migration
equivalence where promised, overflow, negative durations, leap days, month
ends, sub-day ranges, empty ranges, split subtraction, timezone DST gaps and
overlaps, and compiler deprecation diagnostics.

Windows verification from `tests/`:

```powershell
fpc "-FU." "-Fu..\src" TestRunner.lpr
.\TestRunner.exe -a --format=plain
```

Linux verification from `tests/`:

```bash
fpc -FU. -Fu../src TestRunner.lpr
./TestRunner -a --format=plain
```

The release gate also compiles every shipped example, builds the Lazarus
package, checks documentation links and public-method coverage, and runs the
existing named-timezone matrix on Windows and Linux.

## Success criteria

- Every declaration in the matrix has a compiler annotation where Free Pascal
  supports one, a documentation marker otherwise, and a migration example.
- Every replacement is public, documented, and covered by focused tests.
- No v1.5 call is removed; compatibility fixtures compile under Free Pascal
  3.2.2.
- The new period/duration API contains no approximate month/year conversion.
- The new range API represents empty, disjoint, intersecting, touching, and
  split results without sentinel dates or discarded ranges.
- All listed correctness regressions pass.
- The full Windows and Linux release matrices pass.
- Documentation teaches only the preferred v1.6 path while keeping one
  discoverable migration index for deprecated names.

## Implementation record

The accepted work was converted into ordered tasks in `tasks/plan.md` and
`tasks/todo.md`. The v1.6.0 implementation follows this specification; the
release verification record is in `RELEASE-NOTES-v1.6.0.md` and
`PR-v1.6.0.md`.

## Open questions

None at the scope level. The implementation plan must verify which method,
type, and enum-value deprecation directive forms are supported by Free Pascal
3.2.2; unsupported declaration kinds use the documentation marker required by
the release contract.
