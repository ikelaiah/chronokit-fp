# ChronoKit-FP v1.1.0 release notes

Released 2026-08-10

## First five minutes

v1.1.0 makes the first ChronoKit-FP experience more direct for new Free Pascal
and Lazarus users. It does not change the public API.

- Follow one verified installation path for [Lazarus](../README.md#-installation-lazarus-ide)
  or [source-based projects](../README.md#-installation-source-based-projects).
- Start with the [Getting Started guide](Getting-Started.md), which shows how
  to create, format, parse, and add calendar dates.
- Use [Troubleshooting](Troubleshooting.md) for compiler search-path, format,
  and platform guidance.

## Examples and release confidence

- The quick-start and business-day examples are concise, deterministic console
  programs that do not wait for interactive input.
- Every shipped example is compiled by the Windows and Linux pull-request
  workflow, alongside the existing FPCUnit suite.
- The release gate supports Free Pascal 3.2.2 on Linux and discovers the
  Chocolatey-installed compiler on Windows.

## Compatibility

This is a backwards-compatible 1.x release. There are no new business-calendar
configuration options, no timezone-contract changes, and no work from v1.2.0
or later included in this release.
