# v1.6.0 implementation summary

## Scope

Implements only the v1.6.0 milestone in `ROADMAP.md` and the accepted API
transition contract in `API-Deprecations-v1.6.0.md`.

## Changes

- adds explicit calendar-period, duration, half-open range, quarter, decimal,
  and timezone APIs;
- preserves and deprecates the complete superseded 1.x surface;
- fixes the seven correctness defects listed in the transition specification;
- migrates current examples and documentation to preferred APIs; and
- adds focused regressions plus a standalone legacy compatibility fixture.

## Verification

- Free Pascal 3.2.2 unit suite: 175 tests, 0 errors, 0 failures on Windows.
- Legacy fixture: compiles while emitting deprecation diagnostics.
- Shipped examples and Lazarus package: included in the release verification.
- Linux execution remains part of the project CI release matrix.

## Compatibility and risk

No 1.x declaration is removed. The main behavioural addition is half-open
range semantics; the migration guide calls out the end-boundary conversion.
Legacy incompatible semantics remain isolated from the new canonical paths.
