# ChronoKit-FP 1.6.0 release notes

ChronoKit-FP 1.6.0 is the final planned 1.x API-consolidation release. It adds
the preferred 2.0-facing APIs and deprecates their predecessors without
removing any 1.x declaration.

## Highlights

- Separate `TCalendarPeriod` and exact-millisecond `TDuration` types replace
  the ambiguous tagged span model.
- Validated half-open `TDateTimeRange` operations represent empty, touching,
  disjoint, intersecting, merged, and split results without sentinel dates.
- `StartOfQuarter`, explicit decimal-year conversions, and directionally named
  timezone conversions make intent visible at the call site.
- Every supported superseded type and method emits a compiler deprecation
  warning with its migration direction. `duSeason` is documented because Free
  Pascal 3.2.2 cannot annotate individual enum values.
- The [migration guide](MIGRATION-v1.6-to-v2.0.md) covers every API scheduled
  for removal in 2.0.

## Correctness fixes

- Ceiling rounding now crosses hour, day, month, and year boundaries safely.
- Week and year end calculations work at exact start boundaries.
- Legacy duration differences no longer double-count fractional seconds.
- Legacy interval gaps retain sub-day and millisecond precision.
- Decimal-year conversions use the actual year length and include time of day.
- Reversed legacy intervals are rejected.
- Seasonal rounding raises a clear error instead of silently returning input.

## Compatibility

All 1.5 public declarations remain available in 1.6. Existing callers compile
unchanged, with warnings for declarations scheduled for 2.0 removal. There are
no new runtime dependencies.
