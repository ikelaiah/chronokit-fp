# feat(calendar): add configurable business calendars for v1.2.0

## Description

Implements the v1.2.0 “Business calendars” roadmap milestone. The change adds
explicit holiday and working-week rules through overloads, preserves legacy
Monday-to-Friday defaults, documents calendar boundaries, improves invalid
date messages, and publishes focused recipes.

Closes # (not applicable; release milestone)

## Type of change

- [x] New functionality
- [x] Bug fix
- [x] Documentation update
- [x] Tests update
- [x] Release or maintenance

## Checklist

- [x] I have self-reviewed this change.
- [x] The change follows this project's style and naming conventions.
- [x] Public API additions have inline and guide documentation.
- [x] Existing business-day calls retain their previous behavior.
- [x] New and existing tests pass locally.
- [x] Updated and existing examples compile locally.
- [x] Version metadata, roadmap, changelog, and release notes are updated.

## Testing

- [x] FPCUnit suite passed — 145 tests, 0 errors, 0 failures on Windows with
  Free Pascal 3.2.2.
- [x] `ChronoKitQuickStart`, `AddBusinessDays`, and `ChronoKitExample` compile
  successfully on Windows.
- [x] The Lazarus package builds with the release version metadata.
- [x] The configured business-day example runs with deterministic output.

### Windows (PowerShell)

```powershell
cd tests
fpc "-Fu..\src" TestRunner.lpr
.\TestRunner.exe -a --format=plain
```

### Linux

The pull-request workflow runs the same FPCUnit suite and compiles every
shipped example on Linux. Linux was not run locally for this branch.

## Notes for reviewers

- Review the public contract first in
  [Business-Calendar-API.md](Business-Calendar-API.md).
- Calendar records are validated at each public operation boundary because
  their fields can be directly assigned after construction.
- This PR is limited to v1.2.0. It intentionally excludes timezone-contract
  and implementation work scheduled for v1.3.0 and v1.4.0.
