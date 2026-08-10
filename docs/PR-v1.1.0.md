# release(repo): add v1.1.0 first-five-minutes onboarding release

## Description

Implements the v1.1.0 “First five minutes” roadmap milestone. This release
improves installation, onboarding, troubleshooting, example consistency, and
the pull-request release gate without changing ChronoKit's public API.

Closes # (not applicable; release milestone)

## Type of change

- [x] Bug fix
- [x] Documentation update
- [x] Tests or CI update
- [x] Release or maintenance

## Checklist

- [x] I have self-reviewed this change.
- [x] The change follows this project's style and naming conventions.
- [x] I updated documentation where needed.
- [x] The change introduces no new compiler warnings.
- [x] New and existing tests pass locally, where applicable.
- [x] Updated or added examples compile, where applicable.
- [x] I updated the changelog, where applicable.

## Testing

- [x] FPCUnit suite passed — 130 tests, 0 errors, 0 failures on Windows with
  Free Pascal 3.2.2.
- [x] Shipped examples compiled — `ChronoKitQuickStart`, `AddBusinessDays`,
  and `ChronoKitExample` compiled successfully on Windows.
- [x] Lazarus package compiled — verified from a clean source copy with
  Lazarus 4.0 and Free Pascal 3.2.2.
- [x] Ran the quick-start and business-day examples; their output is
  deterministic.
- [x] Fixed the Linux Free Pascal 3.2.2 compilation error in timezone-offset
  parsing and Windows Chocolatey compiler discovery.

### Windows (PowerShell)

```powershell
cd tests
fpc "-Fu..\src" TestRunner.lpr
.\TestRunner.exe -a --format=plain
```

### Linux

The pull-request workflow runs the FPCUnit suite and compiles every shipped
example on Linux. It was not run locally for this PR.

## Notes for reviewers

This PR is limited to v1.1.0. It intentionally excludes v1.2.0 business
calendar configuration and all later roadmap work.
