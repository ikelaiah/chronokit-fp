# ChronoKit-FP v1.4.0 release notes

Released 2026-08-11

## Named timezones now use their own rules

`WithTimeZone` and `ForceTimeZone` now use both the supplied date/time and the
requested timezone. A conversion to Tokyo, Sydney, or New York no longer
reuses the computer's current-zone offset.

ChronoKit uses platform-native data:

- Windows reads the registered timezone catalog and the per-year `Dynamic DST`
  rules for Windows identifiers such as `Eastern Standard Time`.
- Linux reads the installed IANA TZif database for identifiers such as
  `America/New_York`, including its recurring future-rule footer after the
  final explicit transition.

This removes the old hard-coded United States transition calculation and
supports northern and southern hemisphere rules through the same public API.

## DST gaps and overlaps are explicit

A local clock in a forward jump does not exist. A clock in a backward overlap
identifies two instants. Because a plain `TDateTime` cannot select an
occurrence, ChronoKit now raises `ETimeZoneError` for both cases.

The diagnostic identifies the rejected wall clock, timezone, and whether it
is `nonexistent` or `ambiguous`. Applications should catch the exception class
rather than matching its message.

```pascal
try
  SystemValue := TChronoKit.ForceTimeZone(InputValue, SourceTimeZone);
except
  on E: ETimeZoneError do
    WriteLn('Choose another local time: ', E.Message);
end;
```

## Discovery and failures

`GetTimeZoneNames` now returns the platform catalog used for validation. `UTC`
is still the only portable identifier; Windows and IANA names are not aliases
for one another. Missing rule data, unsupported names, conversion failures,
gaps, and overlaps all raise `ETimeZoneError`. A failed lookup is never
silently replaced with UTC.

## Cross-platform regression gate

The shared suite now contains 154 tests. The same logical assertions cover
UTC, New York, London, Sydney, Tokyo, and Auckland with native identifiers on
each platform, including 2050 northern- and southern-hemisphere rules. Free
Pascal 3.2.2 completed the suite with 0 errors and 0 failures on both Windows
and Linux. The Linux run used installed IANA tzdata; the pull-request workflow
repeats the gate on Ubuntu.

## Compatibility

This is a backwards-compatible 1.x release. No public type, method, parameter,
or return type changed. Code that previously relied on a guessed result for an
ambiguous/nonexistent local clock must now handle `ETimeZoneError`, as required
by the v1.3.0 contract.
