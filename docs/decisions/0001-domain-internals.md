# ADR-001: Domain-focused internals behind the ChronoKit facade

## Status

Accepted

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
to domain-focused units. Shared internal records live in a dependency-free
types unit and the facade maps them explicitly to the unchanged public records.
Dependency direction is:

```text
ChronoKit facade
  -> calendar (including rounding)
  -> business calendars
  -> durations <- ranges
  -> parsing
  -> timezone engine
  -> legacy compatibility

domain units -> ChronoKitInternalTypes + Free Pascal RTL
legacy compatibility -> calendar + ChronoKitInternalTypes + RTL
preferred domain units -X-> legacy compatibility
```

Equivalent deprecated names stay as small delegates to preferred operations.
Only incompatible historical algorithms move to the legacy unit. Every move is
preceded by domain test separation and guarded by platform API manifests.

FPC 3.2.2 does not re-export enumeration identifiers through a type alias. A
probe using an aliased enum compiled the record alias but rejected an existing
consumer expression using the enum value. Public type aliases were therefore
rejected: they would make names such as `duDay`, `bwdMonday`, and `dskPeriod`
unavailable to consumers that correctly use only `ChronoKit`.

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

### Move public records to a types unit and re-export aliases

Rejected after an FPC 3.2.2 compiler probe. Record aliases remain compatible,
but enumeration values are not re-exported with their aliased type. Repeating
every enum value as a facade constant would change the frozen declaration
surface and make the manifest describe an implementation workaround rather
than the original API.

### Move every facade wrapper into a domain unit

Rejected because one-line RTL and compatibility delegates are clearer in the
facade than behind an additional pass-through abstraction.

### Split the Windows and Linux timezone backends now

Deferred. The timezone engine is already an internal boundary with one small,
platform-neutral interface. Its Windows and Unix implementations are selected
at compile time, share UTC normalization and façade dispatch, and cannot be
loaded together. Splitting the conditional implementation would add two more
units and a backend protocol without reducing runtime coupling or the public
surface. Revisit this only when a backend needs independent reuse or when a
change can be verified on both platforms in the same delivery.

## Consequences

- Consumers continue to use `ChronoKit` and existing names.
- Contributors can locate tests and implementations by domain.
- The package contains additional internal units.
- Facade mapping is deliberate boundary code and must remain small and direct.
- Large moves must be delivered one domain at a time.
- The timezone engine remains one conditional unit; this is an explicit
  cohesion decision rather than unfinished extraction work.
