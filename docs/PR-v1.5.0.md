# feat(api): deliver task-oriented API discovery

## Description

Implements the v1.5.0 “Discoverable API and 2.0 decision” milestone. The change
adds two audit-justified, compatibility-preserving text helpers, reorganizes
the documentation around common questions, makes the cheat sheet searchable
and exhaustive, and records why current evidence supports continued 1.x
development rather than a speculative breaking release.

Closes # (not applicable; release milestone)

## Type of change

- [x] Additive feature
- [x] Documentation update
- [x] Tests update
- [x] Release or maintenance

## Checklist

- [x] Beginner-focused audit covers the common date/time task set.
- [x] Every public addition is tied to a reproducible discovery problem.
- [x] `FormatDateTime` and `ParseDateTime` have tests and a compiled example.
- [x] `GetAsString` and `FromString` remain source-compatible.
- [x] The cheat sheet contains every public `TChronoKit` method.
- [x] The README and guides use one task-oriented vocabulary.
- [x] The 2.0 decision follows the roadmap's evidence threshold.
- [x] Version metadata, roadmap, changelog, and release notes are updated.

## Testing

- [x] RED check: the three new tests failed to compile before the aliases
  existed.
- [x] FPCUnit suite passed — 157 tests, 0 errors, 0 failures on Windows with
  Free Pascal 3.2.2 and the complete local timezone fixture matrix.
- [x] `ChronoKitQuickStart`, `AddBusinessDays`, and `ChronoKitExample` compile
  successfully on Windows.
- [x] The Lazarus package builds with v1.5.0 metadata.
- [x] Pull-request CI is configured to repeat the suite and compile examples on
  Windows and Linux.

### Windows (PowerShell)

```powershell
cd tests
$env:CHRONOKIT_TEST_NEW_YORK='Eastern Standard Time'
$env:CHRONOKIT_TEST_LONDON='GMT Standard Time'
$env:CHRONOKIT_TEST_SYDNEY='AUS Eastern Standard Time'
$env:CHRONOKIT_TEST_TOKYO='Tokyo Standard Time'
$env:CHRONOKIT_TEST_AUCKLAND='New Zealand Standard Time'
$env:CHRONOKIT_TEST_SYSTEM_GAP='2024-10-06 02:30:00'
$env:CHRONOKIT_TEST_SYSTEM_OVERLAP='2024-04-07 02:30:00'
fpc "-FU." "-Fu..\src" TestRunner.lpr
.\TestRunner.exe -a --format=plain
```

The system-local gap/overlap fixtures above match a Sydney Windows host. CI
sets its system timezone to New York and uses the corresponding March/November
fixtures from `.github/workflows/test.yml`.

## Notes for reviewers

- Review [API-Audit-v1.5.0.md](API-Audit-v1.5.0.md) before the aliases.
- The aliases contain no independent parsing or formatting logic.
- Review the [searchable cheat sheet](Cheat-Sheet.md) against the public
  declarations; its compact index should contain all method names.
- [V2-DECISION.md](V2-DECISION.md) intentionally publishes no change list
  because the roadmap's deprecation evidence threshold is not met.
