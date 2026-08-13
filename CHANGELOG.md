# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.7.0] - 2026-08-13

### Added

- `ConvertBetweenTimeZones` for direct named-source-to-named-target conversion
  that resolves the source clock once and preserves its instant.
- Value-based `StartOfQuarter` and `EndOfQuarter` boundaries.
- Inclusive, signed `BusinessDaysBetween` overloads, with custom-calendar
  support and date-only endpoint semantics.
- Five executable learning programs, decision guides, a repeated beginner
  audit, and a generated searchable preferred API reference.
- Clean source and Lazarus-package consumer fixtures plus documentation and
  executable-example checks in the Windows/Linux workflow.

### Changed

- README and task-oriented guides lead new users through the preferred v1.7
  concepts before introducing business calendars or named timezones.
- The preferred public surface is closed to further additions through v1.9.

### Compatibility

- No 1.x declaration was removed, deprecated, or given a new runtime
  dependency.

## [1.6.0] - 2026-08-13

### Added

- Separate `TCalendarPeriod` and exact-millisecond `TDuration` types with
  constructors, arithmetic, normalization, and elapsed-time measurement.
- Validated half-open `TDateTimeRange` algebra, including empty, touching,
  merge, intersection, gap, and two-part subtraction results.
- `StartOfQuarter`, explicit decimal-year conversion names, and directional
  timezone conversion names.
- A complete 1.6-to-2.0 migration guide and legacy compatibility fixture.

### Changed

- Superseded 1.x types and methods are deprecated with actionable compiler
  messages but remain source compatible until 2.0.
- Current documentation and examples teach only the preferred v1.6 API.

### Fixed

- Boundary-safe ceiling rounding and correct exact week/year end boundaries.
- Fractional legacy duration differences and precise legacy interval gaps.
- Millisecond decimal-year round trips and reversed interval validation.
- Seasonal rounding now rejects an undefined season instead of returning its
  input silently.

### Compatibility

- No 1.x declaration was removed and no runtime dependency was added.

## [1.5.0] - 2026-08-11

### Added

- `TChronoKit.FormatDateTime`, a discoverable alias for the established
  `GetAsString` formatting behavior.
- `TChronoKit.ParseDateTime`, a discoverable alias for the established
  `FromString` parsing and validation behavior.
- A beginner-focused API audit covering common date/time tasks and the
  evidence required for public-surface changes.
- A searchable question-and-synonym index plus an exhaustive task-grouped
  index of every public `TChronoKit` method.
- An evidence-based 2.0 decision with explicit reconsideration criteria.

### Changed

- README, Getting Started, troubleshooting, and complete API guidance now use
  one task-oriented vocabulary and teach `FormatDateTime` and `ParseDateTime`
  as the preferred first path.
- The shipped quick-start example now demonstrates both additive helpers.
- The cheat sheet now documents omitted boundary, span, reporting, interval,
  and timezone-validation operations.

### Fixed

- Cheat-sheet rounding examples now use the public `TDateUnit` values (`du*`)
  instead of nonexistent `drs*` values.

### Compatibility

- `GetAsString` and `FromString` remain supported with unchanged signatures,
  results, and errors throughout 1.x.
- No existing public type, method, parameter, or return type changed.
- No API is deprecated, and current usage evidence does not justify a proposed
  2.0 breaking-change list.

## [1.4.0] - 2026-08-11

### Added

- A platform-native timezone engine that reads Windows per-year dynamic rules
  and installed Linux IANA TZif data for the requested identifier and date.
- Shared named-zone regression fixtures for New York, London, Sydney, Tokyo,
  and Auckland on Windows and Linux.
- Regression coverage for named target conversion, northern and southern
  seasonal rules, recurring future rules, and ambiguous/nonexistent system
  and named local clocks.

### Changed

- `GetTimeZoneNames` now returns the platform's discoverable timezone catalog
  rather than a short system/current-zone list.
- Timezone documentation now gives task-based guidance for `WithTimeZone` and
  `ForceTimeZone`, including copyable DST-error handling.
- The Windows and Linux CI jobs supply equivalent logical-zone identifiers to
  the same 154-test FPCUnit suite.

### Fixed

- `WithTimeZone` now uses the requested target timezone and the offset in
  effect at the resolved instant.
- `ForceTimeZone` now interprets its input in the requested source timezone
  rather than reusing the system-zone offset.
- Windows no longer applies hard-coded United States transition dates to
  other regions; Linux no longer depends on a process command that ignores the
  requested zone.
- Ambiguous and nonexistent local clocks now raise `ETimeZoneError` with the
  rejected value, timezone, and classification instead of silently guessing.
- Timezone lookup and conversion failures no longer fall back silently to UTC.

### Compatibility

- All existing public types and function signatures are unchanged.
- `UTC` remains the only portable identifier; Windows and IANA names remain
  platform-native.

## [1.3.0] - 2026-08-11

### Added

- A normative timezone contract covering portable and platform-native
  identifiers, instant and wall-clock semantics, and DST-discontinuity errors.
- One shared Windows/Linux regression matrix for offset bounds, New York DST
  transitions, conversions, round trips, date boundaries, and invalid inputs.
- Documented mappings between representative Windows and IANA timezone names.

### Changed

- `GetTimeZoneNames` now always advertises the portable `UTC` identifier on
  Windows.
- Local-to-UTC conversion now applies offsets according to
  `local = UTC + offset`; UTC interpretation uses the inverse operation.
- Unix timezone lookup now evaluates the supplied `TDateTime` rather than the
  current clock.
- Pull-request tests use equivalent New York timezone fixtures and identical
  assertions on Windows and Linux.

### Compatibility

- All existing public types and function signatures are unchanged.
- `UTC` is the only cross-platform identifier guarantee. Other names remain
  platform-native, and full named-zone/DST-discontinuity conformance is the
  v1.4.0 release gate.

## [1.2.0] - 2026-08-10

### Added

- `TBusinessCalendar`, `TBusinessWeek`, and `TBusinessWeekday` for explicit
  working-week and holiday rules.
- `CreateBusinessCalendar` factories and calendar-aware overloads for
  `IsBusinessDay`, `NextBusinessDay`, `PreviousBusinessDay`, and
  `AddBusinessDays`.
- Business-calendar recipes for deadlines, reporting periods, and inclusive
  date ranges.
- Deterministic coverage for holidays, alternative working weeks, leap days,
  month ends, week starts, zero-day additions, and preserved time values.

### Changed

- The `AddBusinessDays` example now demonstrates excluding a holiday.
- Invalid `FromString`, `YMD`, `MDY`, `DMY`, and `YQ` input errors now identify
  the rejected value and explain the expected format or range.

### Compatibility

- Existing business-day calls retain their Monday-to-Friday behavior and do
  not exclude holidays unless a `TBusinessCalendar` is passed.
- `NextBusinessDay` and `PreviousBusinessDay` remain strict, and adding zero
  business days continues to return the original value unchanged.

## [1.1.0] - 2026-08-10

### Added

- Verified installation instructions for Lazarus and source-based Free Pascal
  projects.
- A first-five-minutes guide covering date creation, formatting, parsing, and
  date addition.
- A troubleshooting guide for compiler search paths, date formats, and
  Windows/Linux platform setup.
- Pull-request checks that compile every shipped example on Windows and Linux.

### Changed

- Refreshed the quick-start example to use the same focused, non-interactive
  console style as the other shipped examples.
- Updated the business-day example to use a deterministic input and output.

### Fixed

- Linux compilation with Free Pascal 3.2.2 by avoiding a Windows-only numeric
  `IfThen` overload in timezone-offset parsing.
- Windows CI compiler discovery and FPCUnit unit lookup after Chocolatey
  installation.

## [1.0.0] - 2025-07-20

### Added

- **Cross-Platform Timezone Support**: Comprehensive timezone handling for both Windows and Linux
  - Windows: Uses native Windows API (`GetTimeZoneInformation`, `TzSpecificLocalTimeToSystemTime`)
  - Linux: Simplified implementation using system `date` command for reliable timezone detection
- **DST Detection**: Accurate daylight saving time detection for multiple global regions
  - North America (US/Canada): Second Sunday in March to First Sunday in November
  - Europe (EU): Last Sunday in March to Last Sunday in October
  - Australia (Southern States): First Sunday in October to First Sunday in April
  - New Zealand: Last Sunday in September to First Sunday in April
  - Brazil: First Sunday in November to Third Sunday in February
- **Complete DateTime API**: Over 50 functions for comprehensive date/time manipulation
  - Basic operations: `GetNow`, `GetToday`, `GetAsString`, `FromString`
  - Component access: `GetYear`, `GetMonth`, `GetDay`, `GetHour`, `GetMinute`, `GetSecond`, `GetMillisecond`
  - Component modification: `SetYear`, `SetMonth`, `SetDay`, `SetHour`, `SetMinute`, `SetSecond`, `SetMillisecond`
  - Date arithmetic: `AddYears`, `AddMonths`, `AddDays`, `AddHours`, `AddMinutes`, `AddSeconds`
  - Period boundaries: `StartOfYear`, `StartOfMonth`, `StartOfWeek`, `StartOfDay`, `StartOfHour`
  - Period endings: `EndOfYear`, `EndOfMonth`, `EndOfWeek`, `EndOfDay`, `EndOfHour`
  - Date comparisons: `IsBefore`, `IsAfter`, `IsSameDay`, `IsSameMonth`, `IsSameYear`
  - Business day functions: `IsBusinessDay`, `NextBusinessDay`, `PreviousBusinessDay`, `AddBusinessDays`
  - Utility functions: `GetQuarter`, `IsAM`, `IsPM`, `GetDayOfWeek`, `GetDayOfYear`
  - Date rounding: `FloorDate`, `CeilingDate` with support for multiple time units
- **Timezone Functions**:
  - `GetTimeZone`: Returns timezone information including DST status
  - `GetSystemTimeZone`: Get current system timezone name
  - `GetTimeZoneNames`: List available timezone names
  - `WithTimeZone`: Convert between timezones preserving point in time
  - `ForceTimeZone`: Force timezone interpretation without changing time
- **Cross-Platform Environment Variables**: Helper functions for testing and configuration
  - `GetEnvVar`: Cross-platform environment variable retrieval
  - `SetEnvVar`: Cross-platform environment variable setting
- **Time Span Operations**:
  - `TDateSpan`: Flexible time span representation with calendar vs. duration support
  - `TInterval`: Time interval management with start/end points
  - `CreatePeriod`, `CreateDuration`, `CreateInterval`: Factory functions for time spans
- **Exception Handling**: `ETimeZoneError` for robust timezone error handling
- **Comprehensive Documentation**:
  - Complete API reference with examples
  - Cross-platform usage guidelines
  - DST handling best practices
  - Timezone conversion examples

### Changed

- **Simplified Timezone Implementation**: Replaced complex manual timezone file parsing with reliable system-based approach
- **Improved Error Handling**: Better exception handling for timezone operations
- **Enhanced Cross-Platform Compatibility**: Consistent behavior across Windows and Linux

### Removed

- Complex manual DST calculation logic (replaced with system-based approach)
- Hardcoded timezone region mappings (now uses system timezone data)
- Error-prone timezone file parsing (simplified to use system commands)

### Fixed

- **Compilation Errors**: Resolved all syntax errors from previous timezone implementation
- **DST Transition Handling**: Accurate DST detection for all supported regions
- **Cross-Platform Consistency**: Unified behavior between Windows and Linux platforms
- **Memory Management**: Proper cleanup of timezone resources

### Technical Details

- **Platform Support**: Tested on Windows 11 and Ubuntu 24.04
- **Dependencies**: Uses only standard Free Pascal RTL units (Classes, SysUtils, DateUtils, StrUtils, Types, Math, Process, Unix, streamex)
- **Compiler Support**: Free Pascal 3.2.2+ and Lazarus 3.6+
- **Architecture**: Object-oriented design with static methods for easy usage
- **Testing**: Comprehensive test suite covering all major functionality

### Performance

- **Optimized Timezone Lookups**: Fast timezone detection using system facilities
- **Efficient Date Calculations**: Leverages Free Pascal's optimized DateUtils functions
- **Minimal Memory Footprint**: No heavy external dependencies or large timezone databases

### Security

- **Safe Type Conversions**: Proper error handling for date/time parsing
- **Input Validation**: Robust validation for timezone names and date components
- **Exception Safety**: All functions handle edge cases gracefully
