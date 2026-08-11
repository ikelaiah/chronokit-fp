# ChronoKit-FP v1.5.0 release notes

Completed 2026-08-11

## Find parsing and formatting directly

The beginner API audit found that the documentation consistently said “parse”
and “format,” while the public method names were `FromString` and
`GetAsString`. v1.5.0 adds two direct names for those first-five-minutes tasks:

```pascal
Parsed := TChronoKit.ParseDateTime(
  '2026-08-11 09:30', 'yyyy-mm-dd hh:nn');
DisplayText := TChronoKit.FormatDateTime(
  Parsed, 'dd mmm yyyy, hh:nn');
```

These are aliases, not alternate implementations. `ParseDateTime` delegates
to `FromString`, and `FormatDateTime` delegates to `GetAsString`, so established
locale behavior, explicit formats, results, and `EConvertError` diagnostics
remain unchanged.

## Search by the question you have

The cheat sheet now begins with questions and synonyms such as parse, format,
difference, elapsed, business, holiday, interval, UTC, and DST. It then gives
task recipes and a compact index containing every public `TChronoKit` method.

The complete guide, README, Getting Started guide, and troubleshooting guide
use the same task vocabulary. Missing advanced interval, span, boundary,
calendar-reporting, and timezone-validation operations are now represented,
and invalid `drs*` rounding examples have been corrected to the public `du*`
values.

## 2.0 remains conditional

The audit found discoverability and documentation issues that can be fixed
compatibly. It did not find an existing deprecation with a proven replacement
or user evidence that migration would be valuable. The project will therefore
continue compatible 1.x releases and is not publishing a proposed 2.0 change
list with v1.5.0.

The [2.0 decision](V2-DECISION.md) records the evidence and the conditions that
would justify reconsideration.

## Compatibility

This is an additive 1.x release. `GetAsString` and `FromString` remain fully
supported, no public API is deprecated, and no existing signature or behavior
changed.

## Verification

- The new methods were specified by tests that failed before implementation.
- The Windows FPCUnit suite passes 157 tests with 0 errors and 0 failures on
  Free Pascal 3.2.2 using the shared named-zone matrix and local Sydney DST
  fixtures.
- Every shipped example compiles on Windows with Free Pascal 3.2.2.
- The Lazarus package builds with v1.5.0 metadata.
- Pull requests repeat the suite and example compilation on Windows and Linux.
