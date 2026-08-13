# ChronoKit-FP 1.7.0 release notes

ChronoKit-FP 1.7.0 turns the preferred v1.6 API into a small, executable
learning path and makes three focused workflows direct.

## Highlights

- The [learning path](Learning-Path.md) teaches dates and wall clocks,
  calendar periods versus exact durations, half-open ranges, business
  calendars, and named timezones with DST through five runnable programs.
- `ConvertBetweenTimeZones(Value, SourceTimeZone, TargetTimeZone)` converts a
  named source wall clock directly to a named target wall clock for the same
  instant. DST gaps and overlaps in the source raise `ETimeZoneError`.
- `StartOfQuarter(Value)` and `EndOfQuarter(Value)` make quarter boundaries
  discoverable when a caller already has a date/time.
- `BusinessDaysBetween(StartDate, EndDate[, Calendar])` counts qualifying
  business dates inclusively, returns a signed result for reverse order, and
  ignores input time components.

## Learning and maintenance

- The [decision guides](Decision-Guides.md) explain type, operation, and error
  choices without requiring source-code reading.
- The [v1.7 beginner audit](API-Audit-v1.7.0.md) records the only additions
  justified by the learning tasks; further API expansion is post-2.0 input.
- [API-Reference.md](API-Reference.md) is generated from public declaration
  comments. Its check fails when the committed reference is stale or a
  preferred declaration lacks useful contract documentation.
- Clean consumer fixtures exercise both documented installation paths, and CI
  compiles and runs every learning program on Windows and Linux.

## Compatibility

This is an additive 1.x release. No existing declaration is removed or newly
deprecated, and no runtime dependency is introduced. The preferred v1.7 public
surface is now frozen through v1.9.
