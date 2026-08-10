# Timezone contract for v1.3.0

## Status and scope

This document is the normative contract for ChronoKit-FP's existing timezone
API. It defines the meaning of the v1.2.0 public signatures without adding a
new type, overload, or function. The contract separates three ideas that a
plain Free Pascal `TDateTime` cannot carry by itself:

- a **wall-clock value**, such as `2026-08-11 09:30`;
- a **timezone**, whose rules give that value a UTC offset; and
- an **instant**, which is one unique point on the UTC timeline.

The v1.3.0 regression suite is the executable specification for this contract.
The wider named-zone implementation work remains the v1.4.0 milestone.

## Supported identifiers

### Portable identifier

`UTC` is the only identifier that application code may assume is accepted on
every supported platform. It is ASCII, case-sensitive, and has an offset of
zero with `IsDST = False` for every date.

Linux also accepts `Etc/UTC` and `/Etc/UTC` as backwards-compatible aliases.
They normalize to `UTC` in returned information. These aliases are not
portable and should not be stored in cross-platform data.

### Platform-native identifiers

Other names are platform-native and case-sensitive:

- Linux uses IANA identifiers from the installed timezone database, such as
  `America/New_York` and `Australia/Sydney`.
- Windows uses Windows timezone identifiers, such as
  `Eastern Standard Time` and `AUS Eastern Standard Time`.

`GetTimeZoneNames` is the authority for the exact non-UTC strings accepted by
the current process. Its result is an availability list, not a cross-platform
serialization format. An empty string or a name absent from that list is
unsupported and timezone operations raise `ETimeZoneError`.

The regression suite uses these equivalent logical zones:

| Logical zone | Windows identifier | Linux identifier |
|---|---|---|
| UTC | `UTC` | `UTC` |
| New York | `Eastern Standard Time` | `America/New_York` |
| London | `GMT Standard Time` | `Europe/London` |
| Sydney | `AUS Eastern Standard Time` | `Australia/Sydney` |
| Tokyo | `Tokyo Standard Time` | `Asia/Tokyo` |
| Auckland | `New Zealand Standard Time` | `Pacific/Auckland` |

The table is a platform mapping, not an alias promise: a Windows identifier is
not required to work on Linux, and an IANA identifier is not required to work
on Windows.

## Operation semantics

All returned `TDateTime` values remain unzoned. Applications must keep the
associated timezone separately whenever later code needs to know it.

| Operation | Meaning of input | Result | Preservation rule |
|---|---|---|---|
| `GetTimeZone(AValue)` | `AValue` is a wall-clock value in the system timezone | System-zone name, offset in minutes east of UTC, and DST state at that local value | Query only; clock fields are unchanged |
| `GetSystemTimeZone` | No date input | The current platform-native system-zone name | Query only |
| `GetTimeZoneNames` | No date input | Exact identifiers accepted on this platform, including `UTC` | Query only |
| `WithTimeZone(AValue, ATimeZone)` | `AValue` is a wall-clock value in the system timezone; `ATimeZone` is the destination | The destination-zone wall-clock representation of the same instant | Preserves the instant |
| `ForceTimeZone(AValue, ATimeZone)` | `AValue` is a wall-clock value that should be interpreted in `ATimeZone` | The system-zone wall-clock representation of that instant | Preserves the input clock fields while assigning their source-zone meaning; the returned clock fields may differ |

Offsets use `local = UTC + offset`. For example, Sydney standard time has
offset `+600` minutes. Therefore a system-local Sydney value of
`2026-06-01 12:00` represents `2026-06-01 02:00 UTC`.

Converting to the system timezone is an identity operation. Converting to UTC
and interpreting a UTC wall clock with `ForceTimeZone(..., 'UTC')` are inverse
operations when neither value is at a DST discontinuity.

## Ambiguous and nonexistent local times

A forward DST transition creates nonexistent wall-clock values. A backward
transition creates ambiguous wall-clock values that correspond to two
instants. A plain `TDateTime` has no field with which the caller can select an
occurrence.

ChronoKit therefore does not guess:

- `GetTimeZone` raises `ETimeZoneError` when its system-local input is
  ambiguous or nonexistent.
- `WithTimeZone` raises `ETimeZoneError` when its system-local source is
  ambiguous or nonexistent. Its target cannot be ambiguous because an instant
  maps to one offset and occurrence.
- `ForceTimeZone` raises `ETimeZoneError` when the wall-clock input is
  ambiguous or nonexistent in `ATimeZone`.

The exception message identifies the rejected local value, timezone, and
whether the value is ambiguous or nonexistent. Applications that need to
choose the earlier or later occurrence require a future API that can represent
that choice; silently selecting one is outside this 1.x contract.

For the New York 2024 rules used by the shared regression matrix:

| Local wall clock | Classification | Required behavior |
|---|---|---|
| `2024-03-10 01:59:59` | Valid, standard time (`UTC-05:00`) | Succeeds |
| `2024-03-10 02:00:00` through `02:59:59` | Nonexistent | Raises `ETimeZoneError` |
| `2024-03-10 03:00:00` | Valid, daylight time (`UTC-04:00`) | Succeeds |
| `2024-11-03 00:59:59` | Valid, daylight time (`UTC-04:00`) | Succeeds |
| `2024-11-03 01:00:00` through `01:59:59` | Ambiguous | Raises `ETimeZoneError` |
| `2024-11-03 02:00:00` | Valid, standard time (`UTC-05:00`) | Succeeds |

## Failures and fallbacks

The following failures use `ETimeZoneError`:

- empty or unsupported timezone identifier;
- offset outside `-12:00` through `+14:00`;
- ambiguous or nonexistent local input;
- missing timezone data or an operating-system lookup/conversion failure.

Timezone operations must not silently replace a failed lookup with UTC. UTC
is a valid explicit zone, not an error fallback. The exception message is for
diagnosis; callers should catch `ETimeZoneError` rather than matching its text.

## Shared regression matrix

Windows and Linux run the same Pascal assertions. Platform setup maps a
logical fixture to its native identifier; assertion bodies do not use
`IFDEF`, unconditional success, diagnostic-only output, or OS-specific
tolerances.

The matrix covers:

| Area | Required cases |
|---|---|
| UTC | Canonical name, zero offset, no DST, stable in winter and summer |
| Offset bounds | `-12:00` and `+14:00` accepted; values outside rejected |
| DST start | Last valid standard second, nonexistent interval, first valid daylight second |
| DST end | Last unambiguous daylight second, ambiguous interval, first valid standard second |
| Conversion | Same-zone identity, local-to-UTC instant preservation, UTC-to-local interpretation, date-boundary crossing, round trip |
| Validation | Empty, malformed, unsupported, and platform-mismatched identifiers raise `ETimeZoneError` |

The v1.4.0 release gate is full conformance of named-zone conversions and DST
failure detection to this matrix on both supported operating systems.

## Compatibility

The public API is unchanged. Existing source continues to compile. Code that
depended on a guessed result at an ambiguous/nonexistent time or on silent UTC
fallback did not have a portable guarantee and must handle `ETimeZoneError`.
