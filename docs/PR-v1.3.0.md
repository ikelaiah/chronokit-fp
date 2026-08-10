# test(timezone): define the v1.3.0 contract and regression matrix

## Description

Implements the v1.3.0 “Timezone contract and regression suite” milestone. The
change publishes the normative contract, removes platform-specific skipped
assertions, runs one New York regression fixture on Windows and Linux, fixes
UTC offset direction, and updates all release documentation without changing
the public API.

Closes # (not applicable; release milestone)

## Type of change

- [x] Bug fix
- [x] Documentation update
- [x] Tests or CI update
- [x] Release or maintenance

## Checklist

- [x] I have self-reviewed this change.
- [x] The change follows this project's style and naming conventions.
- [x] The public timezone contract is documented.
- [x] No public type or function signature changed.
- [x] No timezone test uses a platform-specific skip or unconditional pass.
- [x] Version metadata, roadmap, changelog, and release notes are updated.

## Testing

- [x] FPCUnit suite passed — 145 tests, 0 errors, 0 failures on Windows with
  Free Pascal 3.2.2.
- [x] `ChronoKitQuickStart`, `AddBusinessDays`, and `ChronoKitExample` compile
  successfully on Windows.
- [x] The Lazarus package builds with v1.3.0 metadata.

### Windows (PowerShell)

```powershell
cd tests
fpc "-FU." "-Fu..\src" TestRunner.lpr
.\TestRunner.exe -a --format=plain
```

### Linux

The pull-request workflow sets `TZ=America/New_York`, runs the same FPCUnit
assertions, and compiles every shipped example. Linux was not available for a
local run on this branch.

## Notes for reviewers

- Review [Timezone-Contract.md](Timezone-Contract.md) first.
- `UTC` is portable; all other accepted names remain platform-native.
- v1.4.0 remains responsible for full named-zone conversion and for enforcing
  ambiguous/nonexistent-time errors across platform timezone engines.
