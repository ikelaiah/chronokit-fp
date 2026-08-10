# ChronoKit-FP v1.3.0 release notes

Released 2026-08-11

## Timezone contract

v1.3.0 defines the meaning of the existing timezone API without changing its
public signatures. The [timezone contract](Timezone-Contract.md) explains:

- why `TDateTime` is a wall-clock value rather than a zoned instant;
- which operations preserve an instant and which assign timezone meaning;
- why `UTC` is the only portable identifier;
- how Windows and IANA identifiers map for equivalent logical zones; and
- why ambiguous and nonexistent local inputs must raise `ETimeZoneError`
  instead of silently choosing an occurrence.

The broader named-zone implementation and full DST-discontinuity enforcement
remain the v1.4.0 conformance release.

## Shared regression suite

The timezone tests now use the same assertions on Windows and Linux. The old
suite's unconditional passes, diagnostic-only checks, OS-specific skips, and
broad conversion tolerances have been removed. Coverage includes:

- supported UTC-offset bounds;
- exact New York DST offsets immediately before and after transitions;
- local-to-UTC and UTC-to-local offset direction;
- same-zone identity and UTC round trips;
- date-boundary conversions; and
- exact `ETimeZoneError` handling for unsupported inputs.

CI configures `Eastern Standard Time` on Windows and `America/New_York` on
Linux, then runs the same FPCUnit test runner.

## Correctness fixes

- `GetTimeZoneNames` includes `UTC` on Windows.
- UTC conversion now follows `local = UTC + offset`.
- `ForceTimeZone(..., 'UTC')` applies the inverse conversion.
- Linux queries evaluate the supplied `TDateTime` instead of the current
  system clock.

## Compatibility

This is a backwards-compatible 1.x release: no public type, method, parameter,
or return type changed. Applications should store platform-native timezone
identifiers only when they control the platform, keep timezone context beside
unzoned `TDateTime` values, and catch `ETimeZoneError` rather than matching its
message.
