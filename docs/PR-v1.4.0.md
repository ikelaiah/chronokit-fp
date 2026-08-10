# fix(timezone): implement trustworthy cross-platform conversions

## Description

Implements the v1.4.0 “Time zones users can trust” milestone. Named conversion
now uses the requested timezone and date-specific platform rules, local DST
gaps and overlaps raise `ETimeZoneError`, the shared matrix covers equivalent
logical zones on Windows and Linux, and all timezone/release documentation is
updated without a public signature change.

Closes # (not applicable; release milestone)

## Type of change

- [x] Bug fix
- [x] Documentation update
- [x] Tests or CI update
- [x] Release or maintenance

## Checklist

- [x] I have self-reviewed this change.
- [x] The change follows this project's style and naming conventions.
- [x] Named conversions use the supplied date/time and requested timezone.
- [x] Platform-native data replaces hard-coded regional transition rules.
- [x] Ambiguous and nonexistent local clocks are visible to callers.
- [x] No public type or function signature changed.
- [x] Version metadata, roadmap, changelog, and release notes are updated.

## Testing

- [x] FPCUnit suite passed — 154 tests, 0 errors, 0 failures on Windows with
  Free Pascal 3.2.2.
- [x] FPCUnit suite passed — 154 tests, 0 errors, 0 failures on Linux with
  Free Pascal 3.2.2 and installed IANA tzdata.
- [x] `ChronoKitQuickStart`, `AddBusinessDays`, and `ChronoKitExample` compile
  successfully on Windows and Linux.
- [x] The Lazarus package builds with v1.4.0 metadata.

### Windows (PowerShell)

```powershell
cd tests
fpc "-FU." "-Fu..\src" TestRunner.lpr
.\TestRunner.exe -a --format=plain
```

### Linux

```bash
cd tests
export TZ=America/New_York
fpc "-FU." "-Fu../src" TestRunner.lpr
./TestRunner -a --format=plain
```

The CI matrix additionally supplies the platform-native names for New York,
London, Sydney, Tokyo, and Auckland.

## Notes for reviewers

- Review [Timezone-Contract.md](Timezone-Contract.md) first.
- `ChronoKitTimeZones.pas` is internal; the public surface remains in
  `ChronoKit.pas`.
- Windows uses registered per-year rules; Linux parses installed TZif
  transitions and recurring future rules.
- Local-clock resolution succeeds only when exactly one UTC instant maps back
  to the supplied clock fields.
