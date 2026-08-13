# ADR-001: Domain-focused internals behind the ChronoKit facade

## Status

Proposed

## Date

2026-08-13

## Context

ChronoKit 1.7 exposes one stable `ChronoKit` unit, 95 preferred methods, and 33
deprecated compatibility methods. The facade implementation contains calendar,
duration, range, business-calendar, parsing, legacy, and timezone logic. Its
tests likewise live in one 3,000-line unit. This makes unrelated changes hard
to navigate and review.

The v1.7 public and behavioral contract is frozen. Internal simplification must
not require consumers to learn new units or change existing source.

## Decision

Keep `ChronoKit.pas` as the public facade and move non-trivial implementations
to domain-focused units. Shared records and exceptions live in a dependency-
free types unit and are re-exported by compatible facade aliases. Dependency
direction is:

```text
ChronoKit facade
  -> calendar -> rounding, business calendars
  -> durations -> ranges
  -> parsing
  -> timezone engine
  -> legacy compatibility

domain units -> ChronoKitTypes + Free Pascal RTL
preferred domain units -X-> legacy compatibility
```

Equivalent deprecated names stay as small delegates to preferred operations.
Only incompatible historical algorithms move to the legacy unit. Every move is
preceded by domain test separation and guarded by platform API manifests.

## Alternatives considered

### Keep one implementation unit

Rejected because unrelated domains and compatibility code remain coupled in
navigation, review, and tests.

### Split the public API into new domain classes

Rejected because it would change the frozen v1.7 user model and create a new
learning burden.

### Use include files instead of units

Rejected because include files reduce file length but do not establish compiler
dependency boundaries or independently testable ownership.

### Move every facade wrapper into a domain unit

Rejected because one-line RTL and compatibility delegates are clearer in the
facade than behind an additional pass-through abstraction.

## Consequences

- Consumers continue to use `ChronoKit` and existing names.
- Contributors can locate tests and implementations by domain.
- The package contains additional internal units.
- Type aliases and package integration require explicit FPC 3.2.2 verification.
- Large moves must be delivered one domain at a time.
