# Implementation Plan: v1.1.0 first-five-minutes release

## Overview

Deliver the v1.1.0 onboarding milestone without changing the public API: provide
one verified installation path for Lazarus and source-based projects, a focused
quick start, troubleshooting guidance, compilable examples, and release/PR
notes.

## Task List

### Phase 1: Onboarding documentation

- [x] Document the verified Lazarus and source-based installation paths.
- [x] Add a first-five-minutes tutorial covering date creation, formatting,
  parsing, and date addition.
- [x] Add concise troubleshooting guidance and explain date, local date/time,
  and timezone conversion.

### Phase 2: Examples and release gate

- [x] Standardise the shipped examples on the concise `TChronoKit` console
  style and remove interactive pauses.
- [x] Compile each example in the Windows and Linux pull-request workflow.

### Phase 3: Release material

- [x] Update the version and changelog for 1.1.0.
- [x] Add the v1.1.0 release notes and pull-request description under `docs/`.

## Verification

- [x] Compile and run the complete FPCUnit suite.
- [x] Compile every shipped example.
- [x] Review the documentation links and Markdown files.

## Scope boundary

This plan intentionally does not add business-calendar configuration, timezone
contract changes, or any work scheduled for v1.2.0 and later.
