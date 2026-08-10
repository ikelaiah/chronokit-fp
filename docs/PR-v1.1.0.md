# Pull request: release v1.1.0

## Summary

Implements the v1.1.0 “First five minutes” roadmap milestone. The release
improves installation, onboarding, troubleshooting, example consistency, and
the pull-request release gate without changing ChronoKit's public API.

## Changes

- Documented one verified installation path each for Lazarus and source-based
  Free Pascal projects.
- Added the first-five-minutes guide and troubleshooting guide.
- Updated the README quick start to demonstrate creating, formatting, parsing,
  and adding dates.
- Standardised shipped console examples and removed interactive pauses.
- Added Windows/Linux CI compilation checks for every shipped example.
- Added v1.1.0 changelog and release notes.

## Verification

- `fpc "-FuC:\\path\\to\\chronokit-fp\\src" TestRunner.lpr`
- `./TestRunner.exe -a --format=plain`
- Compiled `ChronoKitQuickStart`, `AddBusinessDays`, and `ChronoKitExample`.
- Ran the quick-start and business-day examples; their output is deterministic.

## Scope

This PR is limited to v1.1.0. It intentionally excludes v1.2.0 business
calendar configuration and all later roadmap work.
