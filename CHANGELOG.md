# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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
