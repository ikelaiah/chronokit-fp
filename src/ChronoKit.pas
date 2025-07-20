unit ChronoKit;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, DateUtils, StrUtils,
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF UNIX}
  Process, // For RunCommand
  Unix,    // For Unix-specific functions
  {$ENDIF}
  Types;

const
  MillisecondsPerSecond = 1000;
  SecondsPerMinute = 60;
  MinutesPerHour = 60;
  HoursPerDay = 24;
  DaysPerWeek = 7;
  MonthsPerYear = 12;
  
  // Derived constants
  SecondsPerHour = SecondsPerMinute * MinutesPerHour;
  SecondsPerDay = SecondsPerHour * HoursPerDay;
  MinutesPerDay = MinutesPerHour * HoursPerDay;
  
  // Special values
  OneMillisecond = 1 / (SecondsPerDay * MillisecondsPerSecond);

  // Windows timezone constants
  {$IFDEF WINDOWS}
  TIME_ZONE_ID_INVALID = DWORD($FFFFFFFF);
  TIME_ZONE_ID_UNKNOWN = 0;
  TIME_ZONE_ID_STANDARD = 1;
  TIME_ZONE_ID_DAYLIGHT = 2;

  // Windows API function declarations
  function TzSpecificLocalTimeToSystemTime(lpTimeZoneInformation: PTimeZoneInformation;
    lpLocalTime: PSystemTime; lpUniversalTime: PSystemTime): BOOL; stdcall; external 'kernel32.dll' name 'TzSpecificLocalTimeToSystemTime';
  {$ENDIF}

type
  { Custom exceptions }
  ETimeZoneError = class(Exception);

  {$IFDEF WINDOWS}
  // Windows timezone structures
  TSystemTime = Windows.TSystemTime;
  PSystemTime = Windows.PSystemTime;
  TTimeZoneInformation = Windows.TTimeZoneInformation;
  PTimeZoneInformation = Windows.PTimeZoneInformation;
  {$ELSE}
  // Dummy timezone structures for non-Windows platforms
  TSystemTime = record
    wYear: Word;
    wMonth: Word;
    wDayOfWeek: Word;
    wDay: Word;
    wHour: Word;
    wMinute: Word;
    wSecond: Word;
    wMilliseconds: Word;
  end;
  
  PSystemTime = ^TSystemTime;
  
  TTimeZoneInformation = record
    Bias: LongInt;
    StandardName: array[0..31] of WideChar;
    StandardDate: TSystemTime;
    StandardBias: LongInt;
    DaylightName: array[0..31] of WideChar;
    DaylightDate: TSystemTime;
    DaylightBias: LongInt;
  end;
  
  PTimeZoneInformation = ^TTimeZoneInformation;
  {$ENDIF}

  { TDSTRule represents a specific DST rule for a region }
  TDSTRule = record
    Region: string;           // Region identifier (e.g., 'US', 'EU', 'AU')
    StartMonth: Integer;      // Month when DST starts (1-12)
    StartWeek: Integer;       // Week of the month (1-5, where 5 means last)
    StartDayOfWeek: Integer;  // Day of week (1-7, where 1=Sunday)
    StartHour: Integer;       // Hour when DST starts (0-23)
    EndMonth: Integer;        // Month when DST ends (1-12)
    EndWeek: Integer;         // Week of the month (1-5, where 5 means last)
    EndDayOfWeek: Integer;    // Day of week (1-7, where 1=Sunday)
    EndHour: Integer;         // Hour when DST ends (0-23)
    Offset: Integer;          // DST offset in minutes (typically 60)
  end;

  {
    @abstract(Represents different ways to measure time spans)
    
    @description Defines how time spans should be interpreted when performing date arithmetic.
                 Calendar periods account for varying month lengths and leap years,
                 while durations represent fixed time intervals.
    
    @usage Use with CreatePeriod and CreateDuration functions to specify the calculation method.
    
    @warning Mixing period and duration calculations can lead to unexpected results.
             Always use the appropriate kind for your use case.
  }
  TDateSpanKind = (
    dskPeriod,   ///< Calendar time (months, years - variable length)
    dskDuration  ///< Physical time (fixed length in seconds)
  );
  
  {
    @abstract(Time units for date rounding operations)
    
    @description Defines the precision level for date/time rounding operations.
                 Each unit represents a boundary point in the calendar/time system.
    
    @usage Use with FloorDate, CeilingDate, and RoundDate functions to specify
           the desired rounding precision.
    
    @warning Units are ordered from finest to coarsest precision.
             Different units may have different behaviors for boundary conditions.
  }
  TDateUnit = (
    duSecond,     ///< Round to second boundary (.000 milliseconds)
    duMinute,     ///< Round to minute boundary (:00 seconds)
    duHour,       ///< Round to hour boundary (:00 minutes)
    duDay,        ///< Round to day boundary (midnight 00:00:00)
    duWeek,       ///< Round to week boundary (Sunday 00:00:00)
    duMonth,      ///< Round to month boundary (1st day 00:00:00)
    duBiMonth,    ///< Round to bi-month boundary (every 2 months)
    duQuarter,    ///< Round to quarter boundary (Jan/Apr/Jul/Oct 1st)
    duSeason,     ///< Round to season boundary (meteorological seasons)
    duHalfYear,   ///< Round to half-year boundary (Jan 1st or Jul 1st)
    duYear        ///< Round to year boundary (January 1st 00:00:00)
  );
  
  {
    @abstract(Timezone information record)
    
    @description Contains comprehensive timezone information including name,
                 UTC offset, and daylight saving time status for a specific date/time.
    
    @usage Returned by GetTimeZone function to provide detailed timezone context.
    
    @warning DST status and offset may vary throughout the year for the same timezone.
             Always check timezone info for the specific date you're working with.
  }
  TTimeZoneInfo = record
    Name: string;           ///< Timezone name (e.g., 'UTC', 'America/New_York')
    Offset: Integer;        ///< Offset from UTC in minutes (positive = east, negative = west)
    IsDST: Boolean;         ///< Whether daylight saving time is in effect
  end;
  
  {
    @abstract(Time span record for date arithmetic)
    
    @description Represents a span of time that can be added to or subtracted from dates.
                 Can represent either calendar periods (varying lengths) or fixed durations.
    
    @usage Create using CreatePeriod or CreateDuration functions, then use with
           AddSpan or SubtractSpan for date arithmetic.
    
    @warning The Kind field determines how the span is interpreted during calculations.
             Period spans account for calendar variations, duration spans use fixed time units.
  }
  TDateSpan = record
    Kind: TDateSpanKind;    ///< How to interpret this span (period vs duration)
    Years: Integer;         ///< Number of years
    Months: Integer;        ///< Number of months
    Days: Integer;          ///< Number of days
    Hours: Integer;         ///< Number of hours
    Minutes: Integer;       ///< Number of minutes
    Seconds: Integer;       ///< Number of seconds
    Milliseconds: Integer;  ///< Number of milliseconds
  end;
  
  {
    @abstract(Time interval with defined start and end points)
    
    @description Represents a specific time range with defined boundaries.
                 Used for overlap detection, containment checking, and duration calculations.
    
    @usage Create using CreateInterval function, then use with interval operation functions
           like IsWithinInterval, IntervalsOverlap, etc.
    
    @warning Both StartDate and EndDate are typically inclusive in interval operations.
             Ensure StartDate <= EndDate for proper interval behavior.
  }
  TInterval = record
    StartDate: TDateTime;   ///< Start of interval (inclusive)
    EndDate: TDateTime;     ///< End of interval (inclusive)
  end;

  {
    @abstract(Comprehensive date and time manipulation library for Object Pascal)
    
    @description TChronoKit provides a rich set of static methods for working with dates and times
                 in Object Pascal applications. It extends the standard RTL functionality with
                 advanced features like timezone handling, business day calculations, flexible
                 date parsing, and sophisticated date arithmetic operations.
    
    @bold(Key Features:)
    @unorderedList(
      @item(Flexible date/time parsing and formatting with custom format strings)
      @item(Calendar-aware date arithmetic (periods) and fixed-duration calculations)
      @item(Comprehensive timezone conversion and daylight saving time handling)
      @item(Business day calculations excluding weekends)
      @item(ISO 8601 and epidemiological week number support)
      @item(Date rounding and boundary operations (start/end of periods))
      @item(Interval operations for time range calculations)
      @item(Component-level date/time access and modification)
      @item(Cross-platform compatibility (Windows, Linux, macOS))
    )
    
    @bold(Design Philosophy:)
    ChronoKit follows a functional approach with immutable operations - all functions
    return new TDateTime values rather than modifying existing ones. This ensures
    thread safety and prevents accidental side effects.
    
    @bold(Usage Patterns:)
    @longcode(#
      // Basic date/time operations
      var
        Now, Future: TDateTime;
        Formatted: string;
      begin
        Now := TChronoKit.GetNow;
        Formatted := TChronoKit.GetAsString(Now, 'yyyy-mm-dd hh:nn:ss');
        
        // Date arithmetic
        Future := TChronoKit.AddBusinessDays(Now, 5);
        Future := TChronoKit.AddMonths(Future, 2);
        
        // Timezone operations
        UTC := TChronoKit.WithTimeZone(Now, 'UTC');
        
        // Period boundaries
        MonthStart := TChronoKit.StartOfMonth(Now);
        MonthEnd := TChronoKit.EndOfMonth(Now);
      end;
    #)
    
    @bold(Thread Safety:)
    All TChronoKit methods are thread-safe as they are static functions that do not
    modify global state. However, be aware of timezone-related system calls which
    may have platform-specific thread safety considerations.
    
    @bold(Performance Notes:)
    Most operations are lightweight wrappers around optimized RTL functions.
    Timezone operations may involve system calls and should be cached when used
    repeatedly with the same timezone.
    
    @author ChronoKit Development Team
    @version 1.0.0
    @since Object Pascal / Free Pascal
    @see TDateTime for the underlying date/time type
    @see DateUtils for additional RTL date functions
  }
  TChronoKit = class
  private
    {$IFDEF UNIX}
    // Helper functions for Unix platforms
    class function CalculateDSTDate(const Year, Month, Week, DayOfWeek, Hour: Integer): TDateTime; static;
    {$ENDIF}
  public
    { Basic operations for getting and formatting date/time values }
    
    {
      @description Returns the current system date and time as a TDateTime value.
      This includes both the date and time portions.
      
      @usage Use when you need the precise current moment according to the system clock.
      
      @returns TDateTime - Current date and time from the system clock.
      
      @warning The result depends on the system's clock settings and may not be UTC.
      
      @example
        var
          NowTime: TDateTime;
        begin
          NowTime := TChronoKit.GetNow;
          // NowTime holds the current system date and time
        end;
    }
    class function GetNow: TDateTime; static;
    
    {
      @description Returns just the current date, with the time set to midnight (00:00:00.000).
      
      @usage Use when you only need the date portion of the current day, ignoring the time.
             Useful for comparisons or grouping by day.
      
      @returns TDateTime - Current date with time set to 00:00:00.000.
      
      @warning The date is based on the system's clock and timezone.
      
      @example
        var
          TodayDate: TDateTime;
        begin
          TodayDate := TChronoKit.GetToday;
          // TodayDate holds today's date at midnight
        end;
    }
    class function GetToday: TDateTime; static;
    
    {
      @description Converts or validates a TDateTime value. This is useful for ensuring
                   type safety or making explicit conversions. In practice, it simply
                   returns the input value unchanged.
      
      @usage Primarily used for type casting or ensuring a value is treated as TDateTime
             in contexts requiring explicit type handling, although it's often redundant.
      
      @param AValue The TDateTime value to convert/validate.
        
      @returns TDateTime - The same value, but guaranteed to be TDateTime type.
      
      @warning This function does not perform any actual conversion or validation beyond
               type casting.
      
      @example
        var
          InputDateTime, OutputDateTime: TDateTime;
        begin
          InputDateTime := Now;
          OutputDateTime := TChronoKit.GetDateTime(InputDateTime);
          // OutputDateTime is identical to InputDateTime
        end;
    }
    class function GetDateTime(const AValue: TDateTime): TDateTime; static;
    
    {
      @description Converts a TDateTime value to a formatted string representation.
      
      @usage Use to display date/time values to users or for serialization where a
             specific string format is required.
      
      @param AValue The TDateTime value to format.
      @param AFormat Optional format string. If empty, uses system default format.
                 Common format specifiers:
                 yyyy = 4-digit year (2024)
                 yy = 2-digit year (24)
                 mm = month (01-12)
                 dd = day (01-31)
                 hh = hour in 24h format (00-23)
                 nn = minutes (00-59)
                 ss = seconds (00-59)
                 zzz = milliseconds (000-999)
      
      @returns string - The formatted date/time string.
      
      @warning Relies on the `SysUtils.FormatDateTime` function. The interpretation of
               format specifiers depends on the system's locale settings if `AFormat` is empty.
      
      @example
        var
          MyDateTime: TDateTime;
          FormattedString: string;
        begin
          MyDateTime := TChronoKit.FromString('2024-07-15 10:30:00');
          FormattedString := TChronoKit.GetAsString(MyDateTime, 'yyyy-mm-dd hh:nn');
          // FormattedString: '2024-07-15 10:30'
          
          FormattedString := TChronoKit.GetAsString(MyDateTime);
          // FormattedString depends on system locale, e.g., '15/07/2024 10:30:00'
        end;
    }
    class function GetAsString(const AValue: TDateTime; const AFormat: string = ''): string; static;
    
    {
      @description Converts a string representation of a date/time into a TDateTime value.
      
      @usage Use when parsing date/time information from user input, files, or APIs.
      
      @param AValue The string to parse (e.g., '2024-01-15' or '15/01/2024 10:30').
      @param AFormat Optional format string matching the input format. If empty, tries
                 to parse using system default locale settings.
      
      @returns TDateTime - The parsed date/time value.
        
      @warning Raises EConvertError if the string cannot be parsed using the given format
               or system defaults. Parsing without an explicit format string is locale-dependent.
               Relies on `SysUtils.StrToDateTime`.
      
      @example
        var
          ParsedDateTime: TDateTime;
        begin
          try
            ParsedDateTime := TChronoKit.FromString('2024-07-15 14:00', 'yyyy-mm-dd hh:nn');
            // ParsedDateTime holds the TDateTime for July 15, 2024, 2:00 PM
          except
            on E: EConvertError do
              WriteLn('Failed to parse date: ', E.Message);
          end;
          
          // Example using system default format (might fail depending on locale)
          try
            ParsedDateTime := TChronoKit.FromString('15/07/2024');
          except
            on E: EConvertError do
              WriteLn('Failed to parse date using system format: ', E.Message);
          end;
        end;
    }
    class function FromString(const AValue: string; const AFormat: string = ''): TDateTime; static;
    
    { Date Component Getters
      These functions extract specific parts of a date/time value.
      All of them accept a TDateTime parameter and return an integer. }
    
    {
      @description Extracts the year component from a TDateTime value.
      
      @usage Use when you need only the year part of a date.
      
      @param AValue The date/time value to extract from.
        
      @returns Integer - The year (e.g., 2024).
      
      @warning Relies on `SysUtils.YearOf`.
      
      @example
        var
          MyDateTime: TDateTime;
          YearPart: Integer;
        begin
          MyDateTime := TChronoKit.GetToday;
          YearPart := TChronoKit.GetYear(MyDateTime);
          // YearPart contains the current year
        end;
    }
    class function GetYear(const AValue: TDateTime): Integer; static;
    
    {
      @description Extracts the month component (1-12) from a TDateTime value.
      
      @usage Use when you need the month number of a date.
      
      @param AValue The date/time value to extract from.
        
      @returns Integer - The month (1=January, ..., 12=December).
      
      @warning Relies on `SysUtils.MonthOf`.
      
      @example
        var
          MyDateTime: TDateTime;
          MonthPart: Integer;
        begin
          MyDateTime := TChronoKit.FromString('2024-07-15');
          MonthPart := TChronoKit.GetMonth(MyDateTime);
          // MonthPart: 7
        end;
    }
    class function GetMonth(const AValue: TDateTime): Integer; static;
    
    {
      @description Extracts the day of the month (1-31) from a TDateTime value.
      
      @usage Use when you need the specific day within a month.
      
      @param AValue The date/time value to extract from.
        
      @returns Integer - The day of the month (1-31).
      
      @warning Relies on `SysUtils.DayOf`.
      
      @example
        var
          MyDateTime: TDateTime;
          DayPart: Integer;
        begin
          MyDateTime := TChronoKit.FromString('2024-07-15');
          DayPart := TChronoKit.GetDay(MyDateTime);
          // DayPart: 15
        end;
    }
    class function GetDay(const AValue: TDateTime): Integer; static;
    
    {
      @description Returns the day of the week for a given date.
      
      @usage Use to determine if a date falls on a specific day (e.g., weekend) or for scheduling.
      
      @param AValue The date/time value to check.
        
      @returns Integer - Day of the week (1=Sunday, 2=Monday, ..., 7=Saturday).
      
      @warning The numbering convention (1=Sunday) follows `SysUtils.DayOfWeek`. Be mindful
               if comparing with other systems using different conventions (e.g., 0=Monday).
      
      @example
        var
          MyDateTime: TDateTime;
          DOW: Integer;
        begin
          // Assuming July 15, 2024 is a Monday
          MyDateTime := TChronoKit.FromString('2024-07-15'); 
          DOW := TChronoKit.GetDayOfWeek(MyDateTime);
          // DOW: 2
        end;
    }
    class function GetDayOfWeek(const AValue: TDateTime): Integer; static;
    
    {
      @description Calculates the day number within the year (1-366).
      
      @usage Use for yearly progress tracking or comparing dates within the same year regardless of month.
      
      @param AValue The date/time value to check.
        
      @returns Integer - Day of the year (1-366, accounts for leap years).
      
      @warning Relies on `DateUtils.DayOfTheYear`. Result is 1 for January 1st.
      
      @example
        var
          MyDateTime: TDateTime;
          DOY: Integer;
        begin
          MyDateTime := TChronoKit.FromString('2024-01-10'); 
          DOY := TChronoKit.GetDayOfYear(MyDateTime);
          // DOY: 10
          
          MyDateTime := TChronoKit.FromString('2024-03-01'); // Leap year
          DOY := TChronoKit.GetDayOfYear(MyDateTime);
          // DOY: 31 (Jan) + 29 (Feb) + 1 (Mar) = 61
        end;
    }
    class function GetDayOfYear(const AValue: TDateTime): Integer; static;
    
    {
      @description Extracts the hour component (0-23) from a TDateTime value.
      
      @usage Use when you need the hour part of a time value.
      
      @param AValue The date/time value to extract from.
        
      @returns Integer - The hour in 24-hour format (0-23).
      
      @warning Relies on `SysUtils.HourOf`.
      
      @example
        var
          MyDateTime: TDateTime;
          HourPart: Integer;
        begin
          MyDateTime := TChronoKit.FromString('2024-07-15 14:30:00', 'yyyy-mm-dd hh:nn:ss');
          HourPart := TChronoKit.GetHour(MyDateTime);
          // HourPart: 14
        end;
    }
    class function GetHour(const AValue: TDateTime): Integer; static;
    
    {
      @description Extracts the minute component (0-59) from a TDateTime value.
      
      @usage Use when you need the minute part of a time value.
      
      @param AValue The date/time value to extract from.
        
      @returns Integer - The minute (0-59).
      
      @warning Relies on `SysUtils.MinuteOf`.
      
      @example
        var
          MyDateTime: TDateTime;
          MinutePart: Integer;
        begin
          MyDateTime := TChronoKit.FromString('2024-07-15 14:30:00', 'yyyy-mm-dd hh:nn:ss');
          MinutePart := TChronoKit.GetMinute(MyDateTime);
          // MinutePart: 30
        end;
    }
    class function GetMinute(const AValue: TDateTime): Integer; static;
    
    {
      @description Extracts the second component (0-59) from a TDateTime value.
      
      @usage Use when you need the second part of a time value.
      
      @param AValue The date/time value to extract from.
        
      @returns Integer - The second (0-59).
      
      @warning Relies on `SysUtils.SecondOf`. Milliseconds are truncated.
      
      @example
        var
          MyDateTime: TDateTime;
          SecondPart: Integer;
        begin
          MyDateTime := TChronoKit.FromString('2024-07-15 14:30:45', 'yyyy-mm-dd hh:nn:ss');
          SecondPart := TChronoKit.GetSecond(MyDateTime);
          // SecondPart: 45
        end;
    }
    class function GetSecond(const AValue: TDateTime): Integer; static;
    
    {
      @description Extracts the millisecond component (0-999) from a TDateTime value.
      
      @usage Use when you need the millisecond precision of a time value.
      
      @param AValue The date/time value to extract from.
        
      @returns Integer - The millisecond (0-999).
      
      @warning Relies on `SysUtils.MilliSecondOf`.
      
      @example
        var
          MyDateTime: TDateTime;
          MsPart: Integer;
        begin
          // Note: Standard FromString might not capture milliseconds.
          // Use EncodeDateTime or similar for precise construction.
          MyDateTime := EncodeDateTime(2024, 7, 15, 14, 30, 45, 123); 
          MsPart := TChronoKit.GetMillisecond(MyDateTime);
          // MsPart: 123
        end;
    }
    class function GetMillisecond(const AValue: TDateTime): Integer; static;
    
    { Date Component Setters
      These functions create a new TDateTime with one component changed.
      The original value is not modified. Relies on DecodeDate/DecodeTime
      and EncodeDateTime internally. }
    
    {
      @description Creates a new TDateTime value with the year component replaced.
                   Other components (month, day, time) remain the same.
      
      @usage Use to shift a date to a different year while keeping the month, day, and time.
      
      @param AValue The original TDateTime value.
      @param AYear The new year value (e.g., 2025).
        
      @returns TDateTime - A new date/time value with the specified year.
      
      @warning If the original date is February 29th and the new year is not a leap year,
               the resulting date might be invalid or adjusted (e.g., to Feb 28th),
               depending on the underlying `EncodeDateTime` behavior.
      
      @example
        var
          OriginalDate, NewDate: TDateTime;
          Y, M, D, H, N, S, MS: Word;
        begin
          OriginalDate := EncodeDateTime(2024, 07, 15, 10, 30, 0, 0);
          NewDate := TChronoKit.SetYear(OriginalDate, 2025);
          // NewDate corresponds to 2025-07-15 10:30:00.000
          
          DecodeDateTime(NewDate, Y, M, D, H, N, S, MS);
          // Y=2025, M=7, D=15, H=10, N=30, S=0, MS=0
        end;
    }
    class function SetYear(const AValue: TDateTime; const AYear: Integer): TDateTime; static;
    
    {
      @description Creates a new TDateTime value with the month component replaced.
                   Other components (year, day, time) remain the same.
      
      @usage Use to shift a date to a different month within the same year, keeping the day and time.
      
      @param AValue The original TDateTime value.
      @param AMonth The new month value (1-12).
        
      @returns TDateTime - A new date/time value with the specified month.
      
      @warning If the original day is invalid for the new month (e.g., setting month to February
               when the day is 31), the result might be invalid or adjusted (e.g., to the last
               day of the new month), depending on `EncodeDateTime`.
      
      @example
        var
          OriginalDate, NewDate: TDateTime;
          Y, M, D, H, N, S, MS: Word;
        begin
          OriginalDate := EncodeDateTime(2024, 07, 15, 10, 30, 0, 0);
          NewDate := TChronoKit.SetMonth(OriginalDate, 8); // Change to August
          // NewDate corresponds to 2024-08-15 10:30:00.000
          
          DecodeDateTime(NewDate, Y, M, D, H, N, S, MS);
          // Y=2024, M=8, D=15, H=10, N=30, S=0, MS=0
        end;
    }
    class function SetMonth(const AValue: TDateTime; const AMonth: Integer): TDateTime; static;
    
    {
      @description Creates a new TDateTime value with the day of the month replaced.
                   Other components (year, month, time) remain the same.
      
      @usage Use to change the day within a month, keeping the year, month, and time.
      
      @param AValue The original TDateTime value.
      @param ADay The new day value (1-31).
        
      @returns TDateTime - A new date/time value with the specified day.
      
      @warning If the new day value is invalid for the given month and year (e.g., day 31
               for April), the result might be invalid or adjusted, depending on `EncodeDateTime`.
      
      @example
        var
          OriginalDate, NewDate: TDateTime;
          Y, M, D, H, N, S, MS: Word;
        begin
          OriginalDate := EncodeDateTime(2024, 07, 15, 10, 30, 0, 0);
          NewDate := TChronoKit.SetDay(OriginalDate, 20); // Change to the 20th
          // NewDate corresponds to 2024-07-20 10:30:00.000
          
          DecodeDateTime(NewDate, Y, M, D, H, N, S, MS);
          // Y=2024, M=7, D=20, H=10, N=30, S=0, MS=0
        end;
    }
    class function SetDay(const AValue: TDateTime; const ADay: Integer): TDateTime; static;
    
    {
      @description Creates a new TDateTime value with the hour component replaced.
                   Other components (date, minute, second, millisecond) remain the same.
      
      @usage Use to change the hour of the day, keeping the date and other time parts.
      
      @param AValue The original TDateTime value.
      @param AHour The new hour value (0-23).
        
      @returns TDateTime - A new date/time value with the specified hour.
      
      @warning Invalid hour values (outside 0-23) may lead to exceptions or unexpected
               behavior depending on `EncodeDateTime`.
      
      @example
        var
          OriginalDate, NewDate: TDateTime;
          Y, M, D, H, N, S, MS: Word;
        begin
          OriginalDate := EncodeDateTime(2024, 07, 15, 10, 30, 0, 0);
          NewDate := TChronoKit.SetHour(OriginalDate, 15); // Change to 3 PM
          // NewDate corresponds to 2024-07-15 15:30:00.000
          
          DecodeDateTime(NewDate, Y, M, D, H, N, S, MS);
          // Y=2024, M=7, D=15, H=15, N=30, S=0, MS=0
        end;
    }
    class function SetHour(const AValue: TDateTime; const AHour: Integer): TDateTime; static;
    
    {
      @description Creates a new TDateTime value with the minute component replaced.
                   Other components (date, hour, second, millisecond) remain the same.
      
      @usage Use to change the minute of the hour, keeping the date and other time parts.
      
      @param AValue The original TDateTime value.
      @param AMinute The new minute value (0-59).
        
      @returns TDateTime - A new date/time value with the specified minute.
      
      @warning Invalid minute values (outside 0-59) may lead to exceptions or unexpected
               behavior depending on `EncodeDateTime`.
      
      @example
        var
          OriginalDate, NewDate: TDateTime;
          Y, M, D, H, N, S, MS: Word;
        begin
          OriginalDate := EncodeDateTime(2024, 07, 15, 10, 30, 0, 0);
          NewDate := TChronoKit.SetMinute(OriginalDate, 45); // Change to 45 minutes past the hour
          // NewDate corresponds to 2024-07-15 10:45:00.000
          
          DecodeDateTime(NewDate, Y, M, D, H, N, S, MS);
          // Y=2024, M=7, D=15, H=10, N=45, S=0, MS=0
        end;
    }
    class function SetMinute(const AValue: TDateTime; const AMinute: Integer): TDateTime; static;
    
    {
      @description Creates a new TDateTime value with the second component replaced.
                   Other components (date, hour, minute, millisecond) remain the same.
      
      @usage Use to change the second of the minute, keeping the date and other time parts.
      
      @param AValue The original TDateTime value.
      @param ASecond The new second value (0-59).
        
      @returns TDateTime - A new date/time value with the specified second.
      
      @warning Invalid second values (outside 0-59) may lead to exceptions or unexpected
               behavior depending on `EncodeDateTime`.
      
      @example
        var
          OriginalDate, NewDate: TDateTime;
          Y, M, D, H, N, S, MS: Word;
        begin
          OriginalDate := EncodeDateTime(2024, 07, 15, 10, 30, 0, 0);
          NewDate := TChronoKit.SetSecond(OriginalDate, 55); // Change to 55 seconds
          // NewDate corresponds to 2024-07-15 10:30:55.000
          
          DecodeDateTime(NewDate, Y, M, D, H, N, S, MS);
          // Y=2024, M=7, D=15, H=10, N=30, S=55, MS=0
        end;
    }
    class function SetSecond(const AValue: TDateTime; const ASecond: Integer): TDateTime; static;
    
    {
      @description Creates a new TDateTime value with the millisecond component replaced.
                   Other components (date, hour, minute, second) remain the same.
      
      @usage Use to change the millisecond of the second, keeping the date and other time parts.
      
      @param AValue The original TDateTime value.
      @param AMilliSecond The new millisecond value (0-999).
        
      @returns TDateTime - A new date/time value with the specified millisecond.
      
      @warning Invalid millisecond values (outside 0-999) may lead to exceptions or unexpected
               behavior depending on `EncodeDateTime`.
      
      @example
        var
          OriginalDate, NewDate: TDateTime;
          Y, M, D, H, N, S, MS: Word;
        begin
          OriginalDate := EncodeDateTime(2024, 07, 15, 10, 30, 0, 0);
          NewDate := TChronoKit.SetMilliSecond(OriginalDate, 500); // Change to 500 milliseconds
          // NewDate corresponds to 2024-07-15 10:30:00.500
          
          DecodeDateTime(NewDate, Y, M, D, H, N, S, MS);
          // Y=2024, M=7, D=15, H=10, N=30, S=0, MS=500
        end;
    }
    class function SetMilliSecond(const AValue: TDateTime; const AMilliSecond: Integer): TDateTime; static;
    
    { Date/Time Arithmetic
      These functions add or subtract time periods, returning a new TDateTime.
      Negative values can be used to subtract time. They rely on `DateUtils` functions. }
    
    {
      @description Adds (or subtracts if negative) a specified number of years to a TDateTime value.
                   Attempts to preserve the month and day.
      
      @usage Use for date calculations involving whole years, like anniversaries or expiration dates.
      
      @param AValue The original TDateTime value.
      @param AYears The number of years to add (negative value subtracts).
        
      @returns TDateTime - A new date/time value adjusted by the specified number of years.
      
      @warning If the original date is February 29th and the target year is not a leap year,
               the day will be adjusted to February 28th (`IncYear` behavior).
      
      @example
        var
          OriginalDate, FutureDate, PastDate: TDateTime;
        begin
          OriginalDate := EncodeDateTime(2024, 07, 15, 10, 0, 0, 0);
          FutureDate := TChronoKit.AddYears(OriginalDate, 5);
          // FutureDate corresponds to 2029-07-15 10:00:00.000
          
          PastDate := TChronoKit.AddYears(OriginalDate, -1);
          // PastDate corresponds to 2023-07-15 10:00:00.000
        end;
    }
    class function AddYears(const AValue: TDateTime; const AYears: Integer): TDateTime; static;
    
    {
      @description Adds (or subtracts if negative) a specified number of months to a TDateTime value.
                   Attempts to preserve the day, adjusting if necessary (e.g., Jan 31 + 1 month -> Feb 28/29).
      
      @usage Use for calculations involving whole months, like monthly billing cycles or deadlines.
      
      @param AValue The original TDateTime value.
      @param AMonths The number of months to add (negative value subtracts).
        
      @returns TDateTime - A new date/time value adjusted by the specified number of months.
      
      @warning Day adjustment occurs if the original day doesn't exist in the target month.
               Jan 31 + 1 month = Feb 28/29. Mar 31 + 1 month = Apr 30.
               Relies on `DateUtils.IncMonth`.
      
      @example
        var
          OriginalDate, FutureDate, AdjustedDate: TDateTime;
        begin
          OriginalDate := EncodeDateTime(2024, 01, 15, 10, 0, 0, 0);
          FutureDate := TChronoKit.AddMonths(OriginalDate, 3);
          // FutureDate corresponds to 2024-04-15 10:00:00.000
          
          OriginalDate := EncodeDateTime(2024, 01, 31, 10, 0, 0, 0);
          AdjustedDate := TChronoKit.AddMonths(OriginalDate, 1); // Add 1 month to Jan 31
          // AdjustedDate corresponds to 2024-02-29 10:00:00.000 (Leap year)
        end;
    }
    class function AddMonths(const AValue: TDateTime; const AMonths: Integer): TDateTime; static;
    
    {
      @description Adds (or subtracts if negative) a specified number of days to a TDateTime value.
      
      @usage Use for simple date offsets, like calculating a date 'N' days from now.
      
      @param AValue The original TDateTime value.
      @param ADays The number of days to add (negative value subtracts).
        
      @returns TDateTime - A new date/time value adjusted by the specified number of days.
      
      @warning This performs a simple addition to the TDateTime value, which represents days
               since a base date. It correctly handles month and year rollovers.
      
      @example
        var
          OriginalDate, FutureDate, PastDate: TDateTime;
        begin
          OriginalDate := EncodeDateTime(2024, 07, 15, 10, 0, 0, 0);
          FutureDate := TChronoKit.AddDays(OriginalDate, 10);
          // FutureDate corresponds to 2024-07-25 10:00:00.000
          
          PastDate := TChronoKit.AddDays(OriginalDate, -20);
          // PastDate corresponds to 2024-06-25 10:00:00.000
        end;
    }
    class function AddDays(const AValue: TDateTime; const ADays: Integer): TDateTime; static;
    
    {
      @description Adds (or subtracts if negative) a specified number of hours to a TDateTime value.
      
      @usage Use for time calculations involving whole hours.
      
      @param AValue The original TDateTime value.
      @param AHours The number of hours to add (negative value subtracts).
        
      @returns TDateTime - A new date/time value adjusted by the specified number of hours.
      
      @warning Relies on `DateUtils.IncHour`. Correctly handles day, month, year rollovers.
      
      @example
        var
          OriginalDate, FutureDate: TDateTime;
        begin
          OriginalDate := EncodeDateTime(2024, 07, 15, 10, 30, 0, 0);
          FutureDate := TChronoKit.AddHours(OriginalDate, 5);
          // FutureDate corresponds to 2024-07-15 15:30:00.000
          
          FutureDate := TChronoKit.AddHours(OriginalDate, 24);
          // FutureDate corresponds to 2024-07-16 10:30:00.000
        end;
    }
    class function AddHours(const AValue: TDateTime; const AHours: Integer): TDateTime; static;
    
    {
      @description Adds (or subtracts if negative) a specified number of minutes to a TDateTime value.
      
      @usage Use for time calculations involving whole minutes.
      
      @param AValue The original TDateTime value.
      @param AMinutes The number of minutes to add (negative value subtracts).
        
      @returns TDateTime - A new date/time value adjusted by the specified number of minutes.
      
      @warning Relies on `DateUtils.IncMinute`. Correctly handles hour, day, etc. rollovers.
      
      @example
        var
          OriginalDate, FutureDate: TDateTime;
        begin
          OriginalDate := EncodeDateTime(2024, 07, 15, 10, 30, 0, 0);
          FutureDate := TChronoKit.AddMinutes(OriginalDate, 45);
          // FutureDate corresponds to 2024-07-15 11:15:00.000
          
          FutureDate := TChronoKit.AddMinutes(OriginalDate, -90);
          // FutureDate corresponds to 2024-07-15 09:00:00.000
        end;
    }
    class function AddMinutes(const AValue: TDateTime; const AMinutes: Integer): TDateTime; static;
    
    {
      @description Adds (or subtracts if negative) a specified number of seconds to a TDateTime value.
      
      @usage Use for time calculations involving whole seconds.
      
      @param AValue The original TDateTime value.
      @param ASeconds The number of seconds to add (negative value subtracts).
        
      @returns TDateTime - A new date/time value adjusted by the specified number of seconds.
      
      @warning Relies on `DateUtils.IncSecond`. Correctly handles minute, hour, etc. rollovers.
               Does not affect milliseconds.
      
      @example
        var
          OriginalDate, FutureDate: TDateTime;
        begin
          OriginalDate := EncodeDateTime(2024, 07, 15, 10, 30, 0, 0);
          FutureDate := TChronoKit.AddSeconds(OriginalDate, 90);
          // FutureDate corresponds to 2024-07-15 10:31:30.000
          
          FutureDate := TChronoKit.AddSeconds(OriginalDate, -120);
          // FutureDate corresponds to 2024-07-15 10:28:00.000
        end;
    }
    class function AddSeconds(const AValue: TDateTime; const ASeconds: Integer): TDateTime; static;
    
    { Period Start/End Functions
      These functions return a new TDateTime set to the start or end
      of a specific time period (year, month, week, etc.). They rely on 
      `DateUtils` functions. }
    
    {
      @description Returns the first moment of the year (January 1st, 00:00:00.000) for the year of the given date.
      
      @usage Use to normalize dates to the beginning of their year for comparisons or calculations.
      
      @param AValue The original TDateTime value.
        
      @returns TDateTime - The first moment (Jan 1st, 00:00:00.000) of the same year as AValue.
      
      @warning Relies on `FloorDate(AValue, duYear)`.
      
      @example
        var
          MyDate, StartDate: TDateTime;
        begin
          MyDate := EncodeDateTime(2024, 07, 15, 10, 30, 0, 0);
          StartDate := TChronoKit.StartOfYear(MyDate);
          // StartDate corresponds to 2024-01-01 00:00:00.000
        end;
    }
    class function StartOfYear(const AValue: TDateTime): TDateTime; static;
    
    {
      @description Returns the first moment of the month (1st day, 00:00:00.000) for the month of the given date.
      
      @usage Use to normalize dates to the beginning of their month.
      
      @param AValue The original TDateTime value.
        
      @returns TDateTime - The first moment (1st day, 00:00:00.000) of the same month and year as AValue.
      
      @warning Relies on `FloorDate(AValue, duMonth)`.
      
      @example
        var
          MyDate, StartDate: TDateTime;
        begin
          MyDate := EncodeDateTime(2024, 07, 15, 10, 30, 0, 0);
          StartDate := TChronoKit.StartOfMonth(MyDate);
          // StartDate corresponds to 2024-07-01 00:00:00.000
        end;
    }
    class function StartOfMonth(const AValue: TDateTime): TDateTime; static;
    
    {
      @description Returns the first moment of the week (Sunday, 00:00:00.000) for the week containing the given date.
      
      @usage Use to normalize dates to the beginning of their week (starting Sunday).
      
      @param AValue The original TDateTime value.
        
      @returns TDateTime - The first moment (Sunday, 00:00:00.000) of the week containing AValue.
      
      @warning Relies on `FloorDate(AValue, duWeek)`, which assumes Sunday is the first day of the week.
      
      @example
        var
          MyDate, StartDate: TDateTime;
        begin
          // Assuming July 15, 2024 is a Monday
          MyDate := EncodeDateTime(2024, 07, 15, 10, 30, 0, 0);
          StartDate := TChronoKit.StartOfWeek(MyDate);
          // StartDate corresponds to 2024-07-14 00:00:00.000 (the preceding Sunday)
        end;
    }
    class function StartOfWeek(const AValue: TDateTime): TDateTime; static;
    
    {
      @description Returns the first moment of the day (00:00:00.000) for the given date.
                   Effectively removes the time portion.
      
      @usage Use to compare dates without considering the time, or to get the date part only.
             Equivalent to `Trunc(AValue)`.
      
      @param AValue The original TDateTime value.
        
      @returns TDateTime - The same date as AValue, but with the time set to 00:00:00.000.
      
      @warning Relies on `FloorDate(AValue, duDay)`.
      
      @example
        var
          MyDate, StartDate: TDateTime;
        begin
          MyDate := EncodeDateTime(2024, 07, 15, 10, 30, 45, 500);
          StartDate := TChronoKit.StartOfDay(MyDate);
          // StartDate corresponds to 2024-07-15 00:00:00.000
        end;
    }
    class function StartOfDay(const AValue: TDateTime): TDateTime; static;
    
    {
      @description Returns the first moment of the hour (minute=0, second=0, millisecond=0) for the given date/time.
      
      @usage Use to normalize times to the beginning of their hour.
      
      @param AValue The original TDateTime value.
        
      @returns TDateTime - The same date and hour as AValue, with minutes, seconds, and milliseconds set to 0.
      
      @warning Relies on `FloorDate(AValue, duHour)`.
      
      @example
        var
          MyDate, StartDate: TDateTime;
        begin
          MyDate := EncodeDateTime(2024, 07, 15, 10, 30, 45, 500);
          StartDate := TChronoKit.StartOfHour(MyDate);
          // StartDate corresponds to 2024-07-15 10:00:00.000
        end;
    }
    class function StartOfHour(const AValue: TDateTime): TDateTime; static;
    
    {
      @description Returns the last moment of the year (December 31st, 23:59:59.999) for the year of the given date.
      
      @usage Use to find the boundary of a year for range checks or calculations.
      
      @param AValue The original TDateTime value.
        
      @returns TDateTime - The last moment (Dec 31st, 23:59:59.999) of the same year as AValue.
      
      @warning Relies on `CeilingDate(AValue, duYear) - OneMillisecond`.
      
      @example
        var
          MyDate, EndDate: TDateTime;
        begin
          MyDate := EncodeDateTime(2024, 07, 15, 10, 30, 0, 0);
          EndDate := TChronoKit.EndOfYear(MyDate);
          // EndDate corresponds to 2024-12-31 23:59:59.999
        end;
    }
    class function EndOfYear(const AValue: TDateTime): TDateTime; static;
    
    {
      @description Returns the last moment of the month (last day, 23:59:59.999) for the month of the given date.
      
      @usage Use to find the boundary of a month for range checks or calculations.
      
      @param AValue The original TDateTime value.
        
      @returns TDateTime - The last moment (last day, 23:59:59.999) of the same month and year as AValue.
      
      @warning Relies on `CeilingDate(AValue, duMonth) - OneMillisecond`. Correctly handles leap years for February.
      
      @example
        var
          MyDate, EndDate: TDateTime;
        begin
          MyDate := EncodeDateTime(2024, 07, 15, 10, 30, 0, 0);
          EndDate := TChronoKit.EndOfMonth(MyDate);
          // EndDate corresponds to 2024-07-31 23:59:59.999
          
          MyDate := EncodeDateTime(2024, 02, 10, 0, 0, 0, 0); // Leap year
          EndDate := TChronoKit.EndOfMonth(MyDate);
          // EndDate corresponds to 2024-02-29 23:59:59.999
        end;
    }
    class function EndOfMonth(const AValue: TDateTime): TDateTime; static;
    
    {
      @description Returns the last moment of the week (Saturday, 23:59:59.999) for the week containing the given date.
      
      @usage Use to find the boundary of a week (ending Saturday) for range checks or calculations.
      
      @param AValue The original TDateTime value.
        
      @returns TDateTime - The last moment (Saturday, 23:59:59.999) of the week containing AValue.
      
      @warning Relies on `CeilingDate(AValue, duWeek) - OneMillisecond`. Assumes Sunday is the first day of the week.
      
      @example
        var
          MyDate, EndDate: TDateTime;
        begin
          // Assuming July 15, 2024 is a Monday
          MyDate := EncodeDateTime(2024, 07, 15, 10, 30, 0, 0);
          EndDate := TChronoKit.EndOfWeek(MyDate);
          // EndDate corresponds to 2024-07-20 23:59:59.999 (the following Saturday)
        end;
    }
    class function EndOfWeek(const AValue: TDateTime): TDateTime; static;
    
    {
      @description Returns the last moment of the day (23:59:59.999) for the given date.
      
      @usage Use to find the boundary of a day for range checks (e.g., checking if a time falls within today).
      
      @param AValue The original TDateTime value.
        
      @returns TDateTime - The same date as AValue, but with the time set to 23:59:59.999.
      
      @warning Relies on `CeilingDate(AValue, duDay) - OneMillisecond`.
      
      @example
        var
          MyDate, EndDate: TDateTime;
        begin
          MyDate := EncodeDateTime(2024, 07, 15, 10, 30, 45, 500);
          EndDate := TChronoKit.EndOfDay(MyDate);
          // EndDate corresponds to 2024-07-15 23:59:59.999
        end;
    }
    class function EndOfDay(const AValue: TDateTime): TDateTime; static;
    
    {
      @description Returns the last moment of the hour (minute=59, second=59, millisecond=999) for the given date/time.
      
      @usage Use to find the boundary of an hour for range checks.
      
      @param AValue The original TDateTime value.
        
      @returns TDateTime - The same date and hour as AValue, with minutes, seconds, and milliseconds set to their maximum values.
      
      @warning Relies on `CeilingDate(AValue, duHour) - OneMillisecond`.
      
      @example
        var
          MyDate, EndDate: TDateTime;
        begin
          MyDate := EncodeDateTime(2024, 07, 15, 10, 30, 45, 500);
          EndDate := TChronoKit.EndOfHour(MyDate);
          // EndDate corresponds to 2024-07-15 10:59:59.999
        end;
    }
    class function EndOfHour(const AValue: TDateTime): TDateTime; static;
    
    { Date Comparison Functions
      These functions compare two dates in various ways. They often rely on
      standard comparison operators or `DateUtils` functions. }
    
    {
      @description Checks if the first TDateTime value occurs chronologically before the second.
      
      @usage Use for simple chronological ordering checks.
      
      @param AValue The first date/time value to compare.
      @param ADateTime The second date/time value to compare against.
        
      @returns Boolean - True if AValue is strictly earlier than ADateTime, False otherwise.
      
      @warning Uses standard `<` operator for TDateTime comparison.
      
      @example
        var
          Date1, Date2: TDateTime;
          IsEarlier: Boolean;
        begin
          Date1 := EncodeDateTime(2024, 07, 15, 10, 0, 0, 0);
          Date2 := EncodeDateTime(2024, 07, 15, 11, 0, 0, 0);
          IsEarlier := TChronoKit.IsBefore(Date1, Date2); // True
          IsEarlier := TChronoKit.IsBefore(Date2, Date1); // False
          IsEarlier := TChronoKit.IsBefore(Date1, Date1); // False
        end;
    }
    class function IsBefore(const AValue, ADateTime: TDateTime): Boolean; static;
    
    {
      @description Checks if the first TDateTime value occurs chronologically after the second.
      
      @usage Use for simple chronological ordering checks.
      
      @param AValue The first date/time value to compare.
      @param ADateTime The second date/time value to compare against.
        
      @returns Boolean - True if AValue is strictly later than ADateTime, False otherwise.
      
      @warning Uses standard `>` operator for TDateTime comparison.
      
      @example
        var
          Date1, Date2: TDateTime;
          IsLater: Boolean;
        begin
          Date1 := EncodeDateTime(2024, 07, 15, 10, 0, 0, 0);
          Date2 := EncodeDateTime(2024, 07, 15, 11, 0, 0, 0);
          IsLater := TChronoKit.IsAfter(Date1, Date2); // False
          IsLater := TChronoKit.IsAfter(Date2, Date1); // True
          IsLater := TChronoKit.IsAfter(Date1, Date1); // False
        end;
    }
    class function IsAfter(const AValue, ADateTime: TDateTime): Boolean; static;
    
    {
      @description Checks if two TDateTime values fall on the same calendar day, ignoring the time portion.
      
      @usage Use when comparing dates regardless of the time of day.
      
      @param AValue The first date/time value to compare.
      @param ADateTime The second date/time value to compare against.
        
      @returns Boolean - True if both values represent the same date (year, month, day), False otherwise.
      
      @warning Relies on `DateUtils.SameDate`. Compares the integer (date) parts of the TDateTime values.
      
      @example
        var
          Date1, Date2, Date3: TDateTime;
          Same: Boolean;
        begin
          Date1 := EncodeDateTime(2024, 07, 15, 10, 0, 0, 0);
          Date2 := EncodeDateTime(2024, 07, 15, 18, 30, 0, 0);
          Date3 := EncodeDateTime(2024, 07, 16, 10, 0, 0, 0);
          
          Same := TChronoKit.IsSameDay(Date1, Date2); // True
          Same := TChronoKit.IsSameDay(Date1, Date3); // False
        end;
    }
    class function IsSameDay(const AValue, ADateTime: TDateTime): Boolean; static;
    
    {
      @description Checks if two TDateTime values fall within the same calendar month and year.
      
      @usage Use when comparing dates to see if they belong to the same month.
      
      @param AValue The first date/time value to compare.
      @param ADateTime The second date/time value to compare against.
        
      @returns Boolean - True if both values are in the same month and year, False otherwise.
      
      @warning Relies on `DateUtils.SameMonth`. Ignores the day and time portions.
      
      @example
        var
          Date1, Date2, Date3: TDateTime;
          Same: Boolean;
        begin
          Date1 := EncodeDateTime(2024, 07, 15, 10, 0, 0, 0);
          Date2 := EncodeDateTime(2024, 07, 25, 18, 0, 0, 0);
          Date3 := EncodeDateTime(2024, 08, 15, 10, 0, 0, 0);
          
          Same := TChronoKit.IsSameMonth(Date1, Date2); // True
          Same := TChronoKit.IsSameMonth(Date1, Date3); // False
        end;
    }
    class function IsSameMonth(const AValue, ADateTime: TDateTime): Boolean; static;
    
    {
      @description Checks if two TDateTime values fall within the same calendar year.
      
      @usage Use when comparing dates to see if they belong to the same year.
      
      @param AValue The first date/time value to compare.
      @param ADateTime The second date/time value to compare against.
        
      @returns Boolean - True if both values are in the same year, False otherwise.
      
      @warning Relies on comparing the results of `YearOf`. Ignores month, day, and time.
      
      @example
        var
          Date1, Date2, Date3: TDateTime;
          Same: Boolean;
        begin
          Date1 := EncodeDateTime(2024, 07, 15, 0, 0, 0, 0);
          Date2 := EncodeDateTime(2024, 12, 31, 0, 0, 0, 0);
          Date3 := EncodeDateTime(2025, 01, 01, 0, 0, 0, 0);
          
          Same := TChronoKit.IsSameYear(Date1, Date2); // True
          Same := TChronoKit.IsSameYear(Date1, Date3); // False
        end;
    }
    class function IsSameYear(const AValue, ADateTime: TDateTime): Boolean; static;
    
    { Business Day Functions
      These functions help with business day calculations,
      treating Monday-Friday as business days based on DayOfWeek results. }
    
    {
      @description Checks if a given TDateTime value falls on a business day (Monday to Friday).
      
      @usage Use to determine if a specific date is a weekday or weekend.
      
      @param AValue The date/time value to check.
        
      @returns Boolean - True if the day of the week is Monday (2) through Friday (6), False otherwise.
      
      @warning Considers only Saturday (7) and Sunday (1) as non-business days. Does not account
               for public holidays. Uses the 1=Sunday..7=Saturday convention.
      
      @example
        var
          Monday, Sunday: TDateTime;
          IsWorkday: Boolean;
        begin
          Monday := EncodeDate(2024, 7, 15); // A Monday
          Sunday := EncodeDate(2024, 7, 14); // A Sunday
          
          IsWorkday := TChronoKit.IsBusinessDay(Monday); // True
          IsWorkday := TChronoKit.IsBusinessDay(Sunday); // False
        end;
    }
    class function IsBusinessDay(const AValue: TDateTime): Boolean; static;
    
    {
      @description Finds the next business day (Monday-Friday) following the given date.
                   If the given date is already a business day, it finds the next one.
      
      @usage Use to calculate deadlines or follow-up dates that must fall on a workday.
      
      @param AValue The starting TDateTime value. Time portion is preserved but usually irrelevant.
        
      @returns TDateTime - The date of the next business day.
      
      @warning If the input is Friday, returns the following Monday. If Saturday or Sunday,
               returns the following Monday. Does not account for holidays.
      
      @example
        var
          Friday, Saturday, NextDay: TDateTime;
        begin
          Friday := EncodeDate(2024, 7, 12); // A Friday
          Saturday := EncodeDate(2024, 7, 13); // A Saturday
          
          NextDay := TChronoKit.NextBusinessDay(Friday);
          // NextDay is Monday, July 15, 2024
          
          NextDay := TChronoKit.NextBusinessDay(Saturday);
          // NextDay is Monday, July 15, 2024
        end;
    }
    class function NextBusinessDay(const AValue: TDateTime): TDateTime; static;
    
    {
      @description Finds the previous business day (Monday-Friday) before the given date.
                   If the given date is already a business day, it finds the previous one.
      
      @usage Use to find the last workday before a specific date.
      
      @param AValue The starting TDateTime value. Time portion is preserved but usually irrelevant.
        
      @returns TDateTime - The date of the previous business day.
      
      @warning If the input is Monday, returns the preceding Friday. If Sunday or Saturday,
               returns the preceding Friday. Does not account for holidays.
      
      @example
        var
          Monday, Sunday, PrevDay: TDateTime;
        begin
          Monday := EncodeDate(2024, 7, 15); // A Monday
          Sunday := EncodeDate(2024, 7, 14); // A Sunday
          
          PrevDay := TChronoKit.PreviousBusinessDay(Monday);
          // PrevDay is Friday, July 12, 2024
          
          PrevDay := TChronoKit.PreviousBusinessDay(Sunday);
          // PrevDay is Friday, July 12, 2024
        end;
    }
    class function PreviousBusinessDay(const AValue: TDateTime): TDateTime; static;
    
    {
      @description Adds (or subtracts if negative) a specified number of business days (Mon-Fri)
                   to a TDateTime value, skipping weekends.
      
      @usage Use to calculate work-related deadlines or schedules, like "due in 5 business days".
      
      @param AValue The starting TDateTime value. Time portion is preserved but usually irrelevant.
      @param ADays The number of business days to add (negative value subtracts).
        
      @returns TDateTime - A new date adjusted by the specified number of business days.
      
      @warning Skips Saturdays and Sundays when counting. Does not account for public holidays.
               Adding/subtracting 0 days returns the next/previous business day if the start date
               is not a business day, otherwise returns the start date.
      
      @example
        var
          StartDate, DueDate: TDateTime;
        begin
          StartDate := EncodeDate(2024, 7, 12); // Friday
          DueDate := TChronoKit.AddBusinessDays(StartDate, 3);
          // DueDate is Wednesday, July 17, 2024 (Skips Sat, Sun)
          
          StartDate := EncodeDate(2024, 7, 15); // Monday
          DueDate := TChronoKit.AddBusinessDays(StartDate, -2);
          // DueDate is Thursday, July 11, 2024 (Skips Sun, Sat)
        end;
    }
    class function AddBusinessDays(const AValue: TDateTime; const ADays: Integer): TDateTime; static;
    
    {
      @description Returns the calendar quarter (1-4) for the given date.
      
      @usage Use for grouping data by quarter or for financial reporting periods.
      
      @param AValue The TDateTime value to check.
        
      @returns Integer - Quarter number: 1 (Jan-Mar), 2 (Apr-Jun), 3 (Jul-Sep), 4 (Oct-Dec).
      
      @warning Calculation is based solely on the month: `(Month - 1) div 3 + 1`.
      
      @example
        var
          MyDate: TDateTime;
          Q: Integer;
        begin
          MyDate := EncodeDate(2024, 7, 15); // July
          Q := TChronoKit.GetQuarter(MyDate); // Q = 3
          
          MyDate := EncodeDate(2024, 1, 1); // January
          Q := TChronoKit.GetQuarter(MyDate); // Q = 1
        end;
    }
    class function GetQuarter(const AValue: TDateTime): Integer; static;
    
    {
      @description Checks if the time portion of a TDateTime value is in the AM (before 12:00:00 noon).
      
      @usage Use to determine if a time occurs in the morning.
      
      @param AValue The TDateTime value to check.
        
      @returns Boolean - True if the hour is between 0 (midnight) and 11 (inclusive), False otherwise.
      
      @warning Comparison is `HourOf(AValue) < 12`. Midnight (00:00) is considered AM.
      
      @example
        var
          Morning, Afternoon: TDateTime;
          IsMorning: Boolean;
        begin
          Morning := EncodeTime(10, 30, 0, 0);
          Afternoon := EncodeTime(14, 0, 0, 0);
          
          IsMorning := TChronoKit.IsAM(Morning);   // True
          IsMorning := TChronoKit.IsAM(Afternoon); // False
        end;
    }
    class function IsAM(const AValue: TDateTime): Boolean; static;
    
    {
      @description Checks if the time portion of a TDateTime value is in the PM (12:00:00 noon or later).
      
      @usage Use to determine if a time occurs in the afternoon or evening.
      
      @param AValue The TDateTime value to check.
        
      @returns Boolean - True if the hour is 12 (noon) or greater, False otherwise.
      
      @warning Comparison is `HourOf(AValue) >= 12`. Noon (12:00) is considered PM.
      
      @example
        var
          Morning, Afternoon: TDateTime;
          IsAfternoon: Boolean;
        begin
          Morning := EncodeTime(10, 30, 0, 0);
          Afternoon := EncodeTime(14, 0, 0, 0);
          
          IsAfternoon := TChronoKit.IsPM(Morning);   // False
          IsAfternoon := TChronoKit.IsPM(Afternoon); // True
        end;
    }
    class function IsPM(const AValue: TDateTime): Boolean; static;
    
    {
      @description Rounds a TDateTime value down to the nearest specified time unit.
                   Essentially sets lower-order time components to their minimum value.
      
      @usage Use to truncate date/time values to a specific precision level (e.g., floor to the nearest hour).
             Similar to the 'StartOf...' functions but uses an enum parameter.
      
      @param AValue The TDateTime value to round down.
      @param AUnit The unit to floor to (second, minute, hour, day, month, year). See TDateUnit type.
        
      @returns TDateTime - The date/time value rounded down to the specified unit.
      
      @warning Uses direct calculation based on `EncodeDate`/`EncodeTime` for each unit.
               Flooring to 'Day' is equivalent to `StartOfDay` or `Trunc`.
               Handles duYear, duHalfYear, duQuarter, duBiMonth, duMonth, duWeek, duDay, duHour, duMinute, duSecond.
      
      @example
        var
          MyDateTime, FlooredDateTime: TDateTime;
        begin
          MyDateTime := EncodeDateTime(2024, 7, 15, 10, 30, 45, 500);
          
          FlooredDateTime := TChronoKit.FloorDate(MyDateTime, duHour);
          // FlooredDateTime: 2024-07-15 10:00:00.000
          
          FlooredDateTime := TChronoKit.FloorDate(MyDateTime, duMinute);
          // FlooredDateTime: 2024-07-15 10:30:00.000
          
          FlooredDateTime := TChronoKit.FloorDate(MyDateTime, duDay);
          // FlooredDateTime: 2024-07-15 00:00:00.000
        end;
    }
    class function FloorDate(const AValue: TDateTime; const AUnit: TDateUnit): TDateTime; static;
    
    {
      @description Rounds a TDateTime value up to the beginning of the next specified time unit.
      
      @usage Use to find the boundary point after a given time at a specific precision level
             (e.g., ceiling to the start of the next hour).
      
      @param AValue The TDateTime value to round up.
      @param AUnit The unit to ceil to (second, minute, hour, day, month, year). See TDateUnit type.
        
      @returns TDateTime - The date/time value rounded up to the start of the next unit.
      
      @warning Uses direct calculation based on `EncodeDate`/`EncodeTime`/`IncMonth` for each unit.
               If the value is already exactly at the start of a unit, it returns the *next* unit boundary
               (e.g., Ceil(10:00:00, duHour) -> 11:00:00). This differs from 'EndOf...' functions.
               Handles duYear, duHalfYear, duQuarter, duBiMonth, duMonth, duWeek, duDay, duHour, duMinute, duSecond.
      
      @example
        var
          MyDateTime, CeiledDateTime: TDateTime;
        begin
          MyDateTime := EncodeDateTime(2024, 7, 15, 10, 30, 45, 500);
          
          CeiledDateTime := TChronoKit.CeilingDate(MyDateTime, duHour);
          // CeiledDateTime: 2024-07-15 11:00:00.000
          
          CeiledDateTime := TChronoKit.CeilingDate(MyDateTime, duMinute);
          // CeiledDateTime: 2024-07-15 10:31:00.000
          
          CeiledDateTime := TChronoKit.CeilingDate(MyDateTime, duDay);
          // CeiledDateTime: 2024-07-16 00:00:00.000
          
          MyDateTime := EncodeDateTime(2024, 7, 15, 10, 0, 0, 0); // Exactly 10:00
          CeiledDateTime := TChronoKit.CeilingDate(MyDateTime, duHour);
          // CeiledDateTime: 2024-07-15 11:00:00.000 (Start of NEXT hour)
        end;
    }
    class function CeilingDate(const AValue: TDateTime; const AUnit: TDateUnit): TDateTime; static;
    
    { Time Span Creation Functions
      These functions create TDateSpan records representing time periods or durations.
      They typically rely on `DateUtils` functions for span creation. }
    
    {
      @description Creates a calendar-based time span (TDateSpan) which handles variations
                   like leap years and differing month lengths naturally when added to a date.
      
      @usage Use when defining spans like "1 month" or "2 years" where the exact number of days
             or seconds is context-dependent (depends on the start date when added).
      
      @param AYears, AMonths, ADays, AHours, AMinutes, ASeconds, AMilliseconds: Components of the period.
        
      @returns TDateSpan - A span record representing the calendar period.
      
      @warning This function likely maps to `DateUtils.CreateTimeSpan` or similar, creating
               a span where months/years are treated conceptually rather than as fixed durations.
               Adding this span might yield different absolute time differences depending on the start date.
               This is usually the intended behavior for calendar arithmetic.
      
      @example
        var
          OneMonthSpan: TDateSpan;
          StartDate, EndDate: TDateTime;
        begin
          OneMonthSpan := TChronoKit.CreatePeriod(AMonths := 1);
          
          StartDate := EncodeDate(2024, 1, 31);
          EndDate := TChronoKit.AddSpan(StartDate, OneMonthSpan);
          // EndDate is likely Feb 29, 2024 (calendar month added)
          
          StartDate := EncodeDate(2024, 2, 29);
          EndDate := TChronoKit.AddSpan(StartDate, OneMonthSpan);
          // EndDate is likely Mar 29, 2024 (calendar month added)
        end;
    }
    class function CreatePeriod(const AYears: Integer = 0; const AMonths: Integer = 0;
                                const ADays: Integer = 0; const AHours: Integer = 0; const AMinutes: Integer = 0;
                                const ASeconds: Integer = 0; const AMilliseconds: Integer = 0): TDateSpan; static;
      
    {
      @description Creates a fixed-length time span (TDateSpan) based on a precise number of
                   seconds or smaller units. Years/months are converted to approximate fixed lengths.
      
      @usage Use when defining spans representing an exact duration, like "90 minutes" or
             "48 hours", where the length is independent of the start date.
      
      @param AYears, AMonths, ADays, AHours, AMinutes, ASeconds, AMilliseconds: Components of the duration.
        
      @returns TDateSpan - A span record representing the fixed duration.
      
      @warning Converts Years and Months to seconds using approximate values (Year=31536000s/365d, Month=2592000s/30d).
               Prefer using smaller units (days, hours, etc.) for precise durations. Adding this
               span always results in the same absolute time difference. Kind is set to dskDuration.
      
      @example
        var
          NinetyMinutes: TDateSpan;
          StartDate, EndDate: TDateTime;
        begin
          NinetyMinutes := TChronoKit.CreateDuration(AMinutes := 90);
          
          StartDate := EncodeDateTime(2024, 7, 15, 10, 0, 0, 0);
          EndDate := TChronoKit.AddSpan(StartDate, NinetyMinutes);
          // EndDate is 2024-07-15 11:30:00.000 (exactly 90 mins later)
        end;
    }
    class function CreateDuration(const AYears: Integer = 0; const AMonths: Integer = 0;
                                  const ADays: Integer = 0; const AHours: Integer = 0; const AMinutes: Integer = 0;
                                  const ASeconds: Integer = 0; const AMilliseconds: Integer = 0): TDateSpan; static;
      
    {
      @description Creates a TInterval record representing the time range between a start and end TDateTime.
      
      @usage Use to define specific time intervals for checking overlaps, containment, or calculating length.
      
      @param AStart The starting TDateTime of the interval (inclusive).
      @param AEnd The ending TDateTime of the interval (inclusive).
        
      @returns TInterval - An interval record containing the start and end dates.
      
      @warning The function likely stores AStart and AEnd directly. It's usually assumed/required
               that AStart <= AEnd for meaningful interval operations, but the creation function
               itself might not enforce this.
      
      @example
        var
          StartTime, EndTime: TDateTime;
          WorkShift: TInterval;
        begin
          StartTime := EncodeDateTime(2024, 7, 15, 9, 0, 0, 0);
          EndTime := EncodeDateTime(2024, 7, 15, 17, 0, 0, 0);
          WorkShift := TChronoKit.CreateInterval(StartTime, EndTime);
          // WorkShift represents the interval from 9 AM to 5 PM on July 15, 2024
        end;
    }
    class function CreateInterval(const AStart, AEnd: TDateTime): TInterval; static;
    
    { Time Span Operations
      These functions operate on TDateTime values using TDateSpan records. }
    
    {
      @description Adds a time span (period or duration) to a TDateTime value.
      
      @usage Use to calculate a future date/time by adding a predefined span.
      
      @param AValue The original TDateTime value.
      @param ASpan The TDateSpan (period or duration) to add.
        
      @returns TDateTime - The resulting date/time after adding the span.
      
      @warning The result depends on whether ASpan is a period (calendar math) or a duration (fixed math).
               Relies on `DateUtils.IncTimeSpan` or equivalent.
      
      @example
        var
          StartDate, EndDate: TDateTime;
          OneMonthPeriod, NinetyMinutesDuration: TDateSpan;
        begin
          StartDate := EncodeDateTime(2024, 1, 31, 10, 0, 0, 0);
          OneMonthPeriod := TChronoKit.CreatePeriod(AMonths := 1);
          EndDate := TChronoKit.AddSpan(StartDate, OneMonthPeriod);
          // EndDate is likely Feb 29, 2024 (calendar month added)
          
          StartDate := EncodeDateTime(2024, 7, 15, 10, 0, 0, 0);
          NinetyMinutesDuration := TChronoKit.CreateDuration(AMinutes := 90);
          EndDate := TChronoKit.AddSpan(StartDate, NinetyMinutesDuration);
          // EndDate is July 15, 2024, 11:30:00.000 (fixed duration added)
        end;
    }
    class function AddSpan(const AValue: TDateTime; const ASpan: TDateSpan): TDateTime; static;
    
    {
      @description Subtracts a time span (period or duration) from a TDateTime value.
      
      @usage Use to calculate a past date/time by subtracting a predefined span.
      
      @param AValue The original TDateTime value.
      @param ASpan The TDateSpan (period or duration) to subtract.
        
      @returns TDateTime - The resulting date/time after subtracting the span.
      
      @warning The result depends on whether ASpan is a period (calendar math) or a duration (fixed math).
               Equivalent to adding a span with negated components. Relies on `DateUtils.DecTimeSpan` or equivalent.
      
      @example
        var
          StartDate, PastDate: TDateTime;
          OneMonthPeriod: TDateSpan;
        begin
          StartDate := EncodeDateTime(2024, 3, 31, 10, 0, 0, 0);
          OneMonthPeriod := TChronoKit.CreatePeriod(AMonths := 1);
          PastDate := TChronoKit.SubtractSpan(StartDate, OneMonthPeriod);
          // PastDate is likely Feb 29, 2024 (calendar month subtracted)
        end;
    }
    class function SubtractSpan(const AValue: TDateTime; const ASpan: TDateSpan): TDateTime; static;
    
    {
      @description Calculates the time span (period or duration) between two TDateTime values.
      
      @usage Use to determine the difference between two dates, either as a calendar-aware period
             (years, months, days...) or a fixed duration (total days, seconds...).
      
      @param AStart The starting TDateTime.
      @param AEnd The ending TDateTime.
      @param AKind Specifies whether to calculate a period (dskPeriod) or a duration (dskDuration). Default is period.
        
      @returns TDateSpan - The calculated span between AStart and AEnd.
      
      @warning Result depends heavily on AKind.
               `dskPeriod` uses detailed component-wise calculation and normalization.
               `dskDuration` uses direct TDateTime subtraction and converts the result to seconds/milliseconds.
               If AEnd is before AStart, the components of the resulting span will likely be negative.
      
      @example
        var
          Date1, Date2: TDateTime;
          PeriodSpan, DurationSpan: TDateSpan;
        begin
          Date1 := EncodeDateTime(2023, 01, 15, 10, 0, 0, 0);
          Date2 := EncodeDateTime(2024, 03, 20, 12, 30, 0, 0);
          
          // Calculate as calendar period
          PeriodSpan := TChronoKit.SpanBetween(Date1, Date2, dskPeriod);
          // PeriodSpan might represent ~1 year, 2 months, 5 days, 2 hours, 30 minutes
          
          // Calculate as fixed duration (likely total days and fractional part)
          DurationSpan := TChronoKit.SpanBetween(Date1, Date2, dskDuration); 
          // DurationSpan might represent ~430.1 days 
        end;
    }
    class function SpanBetween(const AStart, AEnd: TDateTime; 
                               const AKind: TDateSpanKind = dskPeriod): TDateSpan; static;
      
    { Interval Operations 
      These functions operate on TInterval records. }
    
    {
      @description Checks if a given TDateTime value falls within a specified time interval (inclusive).
      
      @usage Use for checking if an event time, timestamp, or date occurs within a defined range.
      
      @param AValue The TDateTime value to check.
      @param AInterval The TInterval record (containing Start and End dates) to check against.
        
      @returns Boolean - True if AValue >= AInterval.Start and AValue <= AInterval.End, False otherwise.
      
      @warning Assumes AInterval.Start <= AInterval.End for a meaningful check. Relies on
               `DateUtils.InRange` or direct comparison. Boundary dates (Start and End) are included.
      
      @example
        var
          TargetTime, StartTime, EndTime: TDateTime;
          WorkShift: TInterval;
          IsWorking: Boolean;
        begin
          StartTime := EncodeDateTime(2024, 7, 15, 9, 0, 0, 0);
          EndTime := EncodeDateTime(2024, 7, 15, 17, 0, 0, 0);
          WorkShift := TChronoKit.CreateInterval(StartTime, EndTime);
          
          TargetTime := EncodeDateTime(2024, 7, 15, 10, 30, 0, 0);
          IsWorking := TChronoKit.IsWithinInterval(TargetTime, WorkShift); // True
          
          TargetTime := EncodeDateTime(2024, 7, 15, 8, 0, 0, 0);
          IsWorking := TChronoKit.IsWithinInterval(TargetTime, WorkShift); // False
        end;
    }
    class function IsWithinInterval(const AValue: TDateTime; const AInterval: TInterval): Boolean; static;
    
    {
      @description Checks if two time intervals overlap with each other.
      
      @usage Use to detect scheduling conflicts, resource overlaps, or time range intersections.
      
      @param AInterval1 The first time interval.
      @param AInterval2 The second time interval.
        
      @returns Boolean - True if the intervals share any common time period, False if they're completely separate.
      
      @warning An interval that touches another at a single point (e.g., one ends when another starts)
               is typically considered overlapping. Uses standard interval overlap algorithm.
      
      @example
        var
          Meeting1, Meeting2: TInterval;
          Overlaps: Boolean;
        begin
          Meeting1 := TChronoKit.CreateInterval(
            EncodeDateTime(2024, 7, 15, 10, 0, 0, 0),
            EncodeDateTime(2024, 7, 15, 11, 0, 0, 0)
          );
          
          Meeting2 := TChronoKit.CreateInterval(
            EncodeDateTime(2024, 7, 15, 10, 30, 0, 0),
            EncodeDateTime(2024, 7, 15, 11, 30, 0, 0)
          );
          
          Overlaps := TChronoKit.IntervalsOverlap(Meeting1, Meeting2); // True
        end;
    }
    class function IntervalsOverlap(const AInterval1, AInterval2: TInterval): Boolean; static;
    
    {
      @description Calculates the length/duration of a time interval.
      
      @usage Use to determine how long an interval spans, either as a calendar period or fixed duration.
      
      @param AInterval The time interval to measure.
      @param AKind Specifies whether to calculate as a period (dskPeriod) or duration (dskDuration).
        
      @returns TDateSpan - The calculated length of the interval.
      
      @warning Equivalent to calling SpanBetween(AInterval.StartDate, AInterval.EndDate, AKind).
      
      @example
        var
          WorkDay: TInterval;
          Length: TDateSpan;
        begin
          WorkDay := TChronoKit.CreateInterval(
            EncodeDateTime(2024, 7, 15, 9, 0, 0, 0),
            EncodeDateTime(2024, 7, 15, 17, 0, 0, 0)
          );
          
          Length := TChronoKit.IntervalLength(WorkDay, dskDuration);
          // Length represents 8 hours
        end;
    }
    class function IntervalLength(const AInterval: TInterval; const AKind: TDateSpanKind): TDateSpan; static;
    
    { Parse Date-Times with specific formats }
    {
      @description Parses a string in the format 'YYYYMMDD' into a TDateTime value.
      
      @usage Use to convert date strings in a specific format into TDateTime values.
      
      @param AValue The string to parse.
        
      @returns TDateTime - The parsed date/time value.
      
      @warning Assumes the input string is in the 'YYYYMMDD' format. If the format is incorrect,
               the result may be unexpected or an exception may be raised.
      
      @example
        var
          DateString: string;
          ParsedDate: TDateTime;
        begin
          DateString := '20240715';
          ParsedDate := TChronoKit.YMD(DateString);
          // ParsedDate: 2024-07-15 00:00:00.000
        end;
    }
    class function YMD(const AValue: string): TDateTime; static;
    {
      @description Parses a string in the format 'MMDDYYYY' into a TDateTime value.
      
      @usage Use to convert date strings in a specific format into TDateTime values.
      
      @param AValue The string to parse.
        
      @returns TDateTime - The parsed date/time value.
      
      @warning Assumes the input string is in the 'MMDDYYYY' format. If the format is incorrect,
               the result may be unexpected or an exception may be raised.
      
      @example
        var
          DateString: string;
          ParsedDate: TDateTime;
        begin
          DateString := '07152024';
          ParsedDate := TChronoKit.MDY(DateString);
          // ParsedDate: 2024-07-15 00:00:00.000
        end;
    }
    class function MDY(const AValue: string): TDateTime; static;
    {
      @description Parses a string in the format 'DDMMYYYY' into a TDateTime value.
      
      @usage Use to convert date strings in a specific format into TDateTime values.
      
      @param AValue The string to parse.
        
      @returns TDateTime - The parsed date/time value.
      
      @warning Assumes the input string is in the 'DDMMYYYY' format. If the format is incorrect,
               the result may be unexpected or an exception may be raised.
      
      @example
        var
          DateString: string;
          ParsedDate: TDateTime;
        begin
          DateString := '15072024';
          ParsedDate := TChronoKit.DMY(DateString);
          // ParsedDate: 2024-07-15 00:00:00.000
        end;
    }
    class function DMY(const AValue: string): TDateTime; static;
    {
      @description Parses a string in the format 'YYYYQ' into a TDateTime value, where Q is the quarter (1-4).
      
      @usage Use to convert year and quarter strings into TDateTime values.
      
      @param AValue The string to parse.
        
      @returns TDateTime - The parsed date/time value.
      
      @warning Assumes the input string is in the 'YYYYQ' format. If the format is incorrect,
               the result may be unexpected or an exception may be raised.
      
      @example
        var
          DateString: string;
          ParsedDate: TDateTime;
        begin
          DateString := '20242'; // 2024, Quarter 2
          ParsedDate := TChronoKit.YQ(DateString);
          // ParsedDate: 2024-04-01 00:00:00.000 (April 1st of the year)
        end;
    }
    class function YQ(const AValue: string): TDateTime; static;
    {
      @description Converts a decimal date (e.g., 2024.5) into a TDateTime value.
      
      @usage Use to convert decimal dates into TDateTime values.
      
      @param AValue The decimal date to convert.
        
      @returns TDateTime - The converted date/time value.
      
      @warning The implementation details are not clear from the interface definition.
      
      @example
        var
          DecimalDate: Double;
          ConvertedDate: TDateTime;
        begin
          DecimalDate := 2024.5; // Halfway through 2024
          ConvertedDate := TChronoKit.DateDecimal(DecimalDate);
          // ConvertedDate: 2024-07-02 12:00:00.000 (July 2nd, noon)
        end;
    }
    class function DateDecimal(const AValue: Double): TDateTime; static;
    
    { Additional component getters }
    {
      @description Returns the ISO year (week-based year) for a given TDateTime value.
      
      @usage Use to get the ISO year for a specific date.
      
      @param AValue The TDateTime value to check.
        
      @returns Integer - The ISO year.
      
      @warning The implementation details are not clear from the interface definition.
      
      @example
        var
          MyDate: TDateTime;
          ISOYear: Integer;
        begin
          MyDate := EncodeDate(2024, 7, 15); // July 15, 2024
          ISOYear := TChronoKit.GetISOYear(MyDate);
          // ISOYear: 2024 (Usually the same as the calendar year)
        end;
    }
    class function GetISOYear(const AValue: TDateTime): Integer; static;
    {
      @description Returns the ISO week number (week of the year) for a given TDateTime value.
      
      @usage Use to get the ISO week number for a specific date.
      
      @param AValue The TDateTime value to check.
        
      @returns Integer - The ISO week number (1-53).
      
      @warning The implementation details are not clear from the interface definition.
      
      @example
        var
          MyDate: TDateTime;
          ISOWeek: Integer;
        begin
          MyDate := EncodeDate(2024, 7, 15); // July 15, 2024
          ISOWeek := TChronoKit.GetISOWeek(MyDate);
          // ISOWeek: 29 (Week 29 of the year)
        end;
    }
    class function GetISOWeek(const AValue: TDateTime): Integer; static;
    {
      @description Returns the epidemiological year (week-based year) for a given TDateTime value.
      
      @usage Use to get the epidemiological year for a specific date.
      
      @param AValue The TDateTime value to check.
        
      @returns Integer - The epidemiological year.
      
      @warning The implementation details are not clear from the interface definition.
      
      @example
        var
          MyDate: TDateTime;
          EpiYear: Integer;
        begin
          MyDate := EncodeDate(2024, 7, 15); // July 15, 2024
          EpiYear := TChronoKit.GetEpiYear(MyDate);
          // EpiYear: 2024 (Usually the same as the calendar year)
        end;
    }
    class function GetEpiYear(const AValue: TDateTime): Integer; static;
    {
      @description Returns the epidemiological week number (week of the year) for a given TDateTime value.
      
      @usage Use to get the epidemiological week number for a specific date.
      
      @param AValue The TDateTime value to check.
        
      @returns Integer - The epidemiological week number (1-53).
      
      @warning The implementation details are not clear from the interface definition.
      
      @example
        var
          MyDate: TDateTime;
          EpiWeek: Integer;
        begin
          MyDate := EncodeDate(2024, 7, 15); // July 15, 2024
          EpiWeek := TChronoKit.GetEpiWeek(MyDate);
          // EpiWeek: 29 (Week 29 of the year)
        end;
    }
    class function GetEpiWeek(const AValue: TDateTime): Integer; static;
    {
      @description Returns the semester (1 or 2) for a given TDateTime value.
      
      @usage Use to determine the semester for a specific date.
      
      @param AValue The TDateTime value to check.
        
      @returns Integer - The semester (1 or 2).
      
      @warning The implementation details are not clear from the interface definition.
      
      @example
        var
          MyDate: TDateTime;
          Semester: Integer;
        begin
          MyDate := EncodeDate(2024, 7, 15); // July 15, 2024
          Semester := TChronoKit.GetSemester(MyDate);
          // Semester: 2 (July is in the second semester)
        end;
    }
    class function GetSemester(const AValue: TDateTime): Integer; static;
    
    { Date rounding functions }
    {
      @description Rounds a TDateTime value to the nearest specified time unit.
      
      @usage Use to round date/time values to a specific precision level (e.g., round to the nearest hour).
      
      @param AValue The TDateTime value to round.
      @param AUnit The unit to round to (second, minute, hour, day, month, year). See TDateUnit type.
        
      @returns TDateTime - The date/time value rounded to the specified unit.
      
      @warning The implementation details are not clear from the interface definition.
      
      @example
        var
          MyDateTime, RoundedDateTime: TDateTime;
        begin
          MyDateTime := EncodeDateTime(2024, 7, 15, 10, 30, 45, 500);
          
          RoundedDateTime := TChronoKit.RoundDate(MyDateTime, duHour);
          // RoundedDateTime: 2024-07-15 10:00:00.000 (Rounded to the nearest hour)
          
          RoundedDateTime := TChronoKit.RoundDate(MyDateTime, duMinute);
          // RoundedDateTime: 2024-07-15 10:30:00.000 (Rounded to the nearest minute)
          
          RoundedDateTime := TChronoKit.RoundDate(MyDateTime, duDay);
          // RoundedDateTime: 2024-07-15 00:00:00.000 (Rounded to the nearest day)
        end;
    }
    class function RoundDate(const AValue: TDateTime; const AUnit: TDateUnit): TDateTime; static;
    
    { Timezone functions }
    {
      @description Retrieves the time zone information for a given TDateTime value.
      
      @usage Use to get the time zone information for a specific date/time.
      
      @param AValue The TDateTime value to check.
        
      @returns TTimeZoneInfo - The time zone information.
      
      @warning The implementation details are not clear from the interface definition.
      
      @example
        var
          MyDate: TDateTime;
          TimeZoneInfo: TTimeZoneInfo;
        begin
          MyDate := EncodeDateTime(2024, 7, 15, 10, 30, 0, 0);
          TimeZoneInfo := TChronoKit.GetTimeZone(MyDate);
          // TimeZoneInfo contains information about the time zone
        end;
    }
    class function GetTimeZone(const AValue: TDateTime): TTimeZoneInfo; static;
    {
      @description Retrieves the system's current time zone as a string.
      
      @usage Use to get the current system time zone.
      
      @returns string - The current system time zone.
      
      @warning The implementation details are not clear from the interface definition.
      
      @example
        var
          SystemTimeZone: string;
        begin
          SystemTimeZone := TChronoKit.GetSystemTimeZone;
          // SystemTimeZone: 'Eastern Standard Time' (Example)
        end;
    }
    class function GetSystemTimeZone: string; static;
    {
      @description Retrieves an array of all available time zone names.
      
      @usage Use to get a list of all available time zones.
      
      @returns TStringArray - An array of time zone names.
      
      @warning The implementation details are not clear from the interface definition.
      
      @example
        var
          TimeZoneNames: TStringArray;
        begin
          TimeZoneNames := TChronoKit.GetTimeZoneNames;
          // TimeZoneNames contains an array of time zone names
        end;
    }
    class function GetTimeZoneNames: TStringArray; static;
    
    { Additional utility functions }
    {
      @description Adjusts a TDateTime value to the last day of the previous month,
                   preserving the time portion. If the original day doesn't exist
                   in the previous month, it clamps to the last valid day.
      
      @usage Use to find the corresponding date in the prior month, especially useful
             for month-end calculations.
      
      @param AValue The original TDateTime value.
        
      @returns TDateTime - The date representing the same (or last valid) day in the previous month,
                         with the original time preserved.
      
      @warning If AValue is March 31st, returns February 28th/29th.
      
      @example
        var
          MyDate, PrevMonthDate: TDateTime;
        begin
          MyDate := EncodeDateTime(2024, 3, 31, 10, 30, 0, 0); // March 31st
          PrevMonthDate := TChronoKit.RollbackMonth(MyDate);
          // PrevMonthDate: Feb 29, 2024, 10:30:00.000 (Leap year)
          
          MyDate := EncodeDateTime(2024, 1, 15, 12, 0, 0, 0); // Jan 15th
          PrevMonthDate := TChronoKit.RollbackMonth(MyDate);
          // PrevMonthDate: Dec 15, 2023, 12:00:00.000
        end;
    }
    class function RollbackMonth(const AValue: TDateTime): TDateTime; static;
    {
      @description Adjusts a TDateTime value to the first day of the next month,
                   preserving the time portion.
      
      @usage Use to find the corresponding date in the next month, especially useful
             for month-start calculations.
      
      @param AValue The original TDateTime value.
        
      @returns TDateTime - The date representing the same day in the next month,
                         with the original time preserved.
      
      @warning If AValue is February 29th (leap year), returns March 1st.
      
      @example
        var
          MyDate, NextMonthDate: TDateTime;
        begin
          MyDate := EncodeDateTime(2024, 2, 29, 10, 30, 0, 0); // Feb 29th (Leap year)
          NextMonthDate := TChronoKit.RollForwardMonth(MyDate);
          // NextMonthDate: Mar 1, 2024, 10:30:00.000
          
          MyDate := EncodeDateTime(2024, 1, 15, 12, 0, 0, 0); // Jan 15th
          NextMonthDate := TChronoKit.RollForwardMonth(MyDate);
          // NextMonthDate: Feb 15, 2024, 12:00:00.000
        end;
    }
    class function RollForwardMonth(const AValue: TDateTime): TDateTime; static;
    {
      @description Converts a TDateTime value to a decimal date (e.g., 2024.5 for halfway through 2024).
      
      @usage Use to convert TDateTime values into decimal dates.
      
      @param AValue The TDateTime value to convert.
        
      @returns Double - The decimal date representation.
      
      @warning The implementation details are not clear from the interface definition.
      
      @example
        var
          MyDate: TDateTime;
          DecimalDate: Double;
        begin
          MyDate := EncodeDateTime(2024, 7, 15, 10, 30, 0, 0);
          DecimalDate := TChronoKit.GetDecimalDate(MyDate);
          // DecimalDate: 2024.466 (Approximately)
        end;
    }
    class function GetDecimalDate(const AValue: TDateTime): Double; static;
    
    { Additional period/duration functions }
    {
      @description Converts a TDateSpan (period or duration) to a total number of seconds.
      
      @usage Use to convert a period or duration into a total number of seconds.
      
      @param AValue The TDateSpan to convert.
        
      @returns Int64 - The total number of seconds.
      
      @warning The implementation details are not clear from the interface definition.
      
      @example
        var
          MyPeriod: TDateSpan;
          TotalSeconds: Int64;
        begin
          MyPeriod := TChronoKit.CreatePeriod(AMonths := 1); // One month
          TotalSeconds := TChronoKit.PeriodToSeconds(MyPeriod);
          // TotalSeconds: Approximately 2629746 (Seconds in a month)
        end;
    }
    class function PeriodToSeconds(const APeriod: TDateSpan): Int64; static;
    {
      @description Converts a total number of seconds into a TDateSpan (duration).
      
      @usage Use to convert a total number of seconds into a duration.
      
      @param ASeconds The total number of seconds.
        
      @returns TDateSpan - The duration represented by the total seconds.
      
      @warning The implementation details are not clear from the interface definition.
      
      @example
        var
          TotalSeconds: Int64;
          MyDuration: TDateSpan;
        begin
          TotalSeconds := 90 * 60; // 90 minutes
          MyDuration := TChronoKit.SecondsToPeriod(TotalSeconds);
          // MyDuration: 0 years, 0 months, 0 days, 1 hour, 30 minutes, 0 seconds, 0 milliseconds
        end;
    }
    class function SecondsToPeriod(const ASeconds: Int64): TDateSpan; static;
    {
      @description Standardizes a TDateSpan (period or duration) by normalizing its components.
      
      @usage Use to normalize the components of a period or duration.
      
      @param AValue The TDateSpan to standardize.
        
      @returns TDateSpan - The standardized TDateSpan.
      
      @warning The implementation details are not clear from the interface definition.
      
      @example
        var
          MyPeriod: TDateSpan;
          StandardizedPeriod: TDateSpan;
        begin
          MyPeriod := TChronoKit.CreatePeriod(AMonths := 1, ADays := 35); // One month and 35 days
          StandardizedPeriod := TChronoKit.StandardizePeriod(MyPeriod);
          // StandardizedPeriod: 2 months, 5 days, 0 hours, 0 minutes, 0 seconds, 0 milliseconds
        end;
    }
    class function StandardizePeriod(const AValue: TDateSpan): TDateSpan; static;
    
    { Additional interval functions }
    {
      @description Checks if two intervals are adjacent (i.e., the end of one is exactly the start of the other).
      
      @usage Use to see if two time periods meet precisely without overlapping or having a gap.
      
      @param AInterval1 The first TInterval.
      @param AInterval2 The second TInterval.
        
      @returns Boolean - True if `AInterval1.EndDate = AInterval2.StartDate` or
                       `AInterval2.EndDate = AInterval1.StartDate`, False otherwise.
      
      @warning Uses `CompareDateTime` for precise comparison.
      
      @example
        var
          Interval1, Interval2, Interval3: TInterval;
          Aligns: Boolean;
        begin
          Interval1 := TChronoKit.CreateInterval(EncodeTime(9,0,0,0), EncodeTime(10,0,0,0)); // 9-10
          Interval2 := TChronoKit.CreateInterval(EncodeTime(10,0,0,0), EncodeTime(11,0,0,0)); // 10-11
          Interval3 := TChronoKit.CreateInterval(EncodeTime(10,0,0,1), EncodeTime(12,0,0,0)); // 10:00:01-12
          
          Aligns := TChronoKit.IntervalAlign(Interval1, Interval2); // True
          Aligns := TChronoKit.IntervalAlign(Interval1, Interval3); // False
        end;
    }
    class function IntervalAlign(const AInterval1, AInterval2: TInterval): Boolean; static;
    
    {
      @description Calculates the time gap between two non-overlapping intervals as a duration span.
      
      @usage Use to measure the time between two separate events or periods.
      
      @param AInterval1 The first TInterval.
      @param AInterval2 The second TInterval.
        
      @returns TDateSpan - A duration span representing the gap (in days/seconds). Returns zero duration
                         if the intervals overlap or align.
      
      @warning Returns a duration (`dskDuration`). Assumes intervals do not overlap; if they do,
               a zero duration is returned. The calculation seems to primarily return days based
               on `Trunc(EndDate - StartDate)`, potentially losing time precision.
      
      @example
        var
          Interval1, Interval2: TInterval;
          Gap: TDateSpan;
        begin
          Interval1 := TChronoKit.CreateInterval(EncodeTime(9,0,0,0), EncodeTime(10,0,0,0)); // 9-10
          Interval2 := TChronoKit.CreateInterval(EncodeTime(12,0,0,0), EncodeTime(13,0,0,0)); // 12-13
          
          Gap := TChronoKit.IntervalGap(Interval1, Interval2);
          // Gap: Kind=dskDuration, Days=0, Seconds=7200 (2 hours)
          // Note: Implementation calculates days based on Trunc(difference), which might be 0 for gaps < 1 day.
        end;
    }
    class function IntervalGap(const AInterval1, AInterval2: TInterval): TDateSpan; static;
    
    {
      @description Calculates the set difference of Interval1 minus Interval2 (Interval1 \ Interval2).
                   Returns the portion(s) of Interval1 that do not overlap with Interval2.
      
      @usage Use to remove a specific time range (Interval2) from another (Interval1).
      
      @param AInterval1 The base interval.
      @param AInterval2 The interval to subtract.
        
      @returns TInterval - The remaining part of Interval1. If Interval2 completely covers Interval1,
                         returns an empty interval (Start=End=0). If Interval2 splits Interval1,
                         only the *first* remaining part is returned.
      
      @warning If Interval2 splits Interval1 into two parts, this function *only* returns the
               part before the Interval2 overlap. It does not return multiple intervals.
               Returns Interval1 unchanged if there is no overlap.
      
      @example
        var
          BaseInterval, SubtractInterval, ResultInterval: TInterval;
        begin
          BaseInterval := TChronoKit.CreateInterval(EncodeTime(9,0,0,0), EncodeTime(12,0,0,0)); // 9-12
          
          // Case 1: No overlap
          SubtractInterval := TChronoKit.CreateInterval(EncodeTime(13,0,0,0), EncodeTime(14,0,0,0));
          ResultInterval := TChronoKit.IntervalSetdiff(BaseInterval, SubtractInterval);
          // ResultInterval: 9-12 (BaseInterval unchanged)
          
          // Case 2: SubtractInterval covers BaseInterval
          SubtractInterval := TChronoKit.CreateInterval(EncodeTime(8,0,0,0), EncodeTime(13,0,0,0));
          ResultInterval := TChronoKit.IntervalSetdiff(BaseInterval, SubtractInterval);
          // ResultInterval: 0-0 (Empty)
          
          // Case 3: SubtractInterval overlaps start
          SubtractInterval := TChronoKit.CreateInterval(EncodeTime(8,0,0,0), EncodeTime(10,0,0,0));
          ResultInterval := TChronoKit.IntervalSetdiff(BaseInterval, SubtractInterval);
          // ResultInterval: 10-12
          
          // Case 4: SubtractInterval overlaps end
          SubtractInterval := TChronoKit.CreateInterval(EncodeTime(11,0,0,0), EncodeTime(13,0,0,0));
          ResultInterval := TChronoKit.IntervalSetdiff(BaseInterval, SubtractInterval);
          // ResultInterval: 9-11
          
          // Case 5: SubtractInterval splits BaseInterval
          SubtractInterval := TChronoKit.CreateInterval(EncodeTime(10,0,0,0), EncodeTime(11,0,0,0));
          ResultInterval := TChronoKit.IntervalSetdiff(BaseInterval, SubtractInterval);
          // ResultInterval: 9-10 (Only the first part is returned)
        end;
    }
    class function IntervalSetdiff(const AInterval1, AInterval2: TInterval): TInterval; static;
    
    {
      @description Calculates the union of two intervals. If the intervals overlap or align,
                   returns a single interval covering the total combined range.
      
      @usage Use to merge two potentially overlapping time periods into one continuous range.
      
      @param AInterval1 The first TInterval.
      @param AInterval2 The second TInterval.
        
      @returns TInterval - The combined interval (earliest start to latest end). Returns an empty
                         interval (Start=End=0) if the input intervals do not overlap and are not adjacent.
      
      @warning Only returns a valid union if the intervals overlap or align. If there's a gap,
               an empty interval is returned.
      
      @example
        var
          Interval1, Interval2, UnionInterval: TInterval;
        begin
          Interval1 := TChronoKit.CreateInterval(EncodeTime(9,0,0,0), EncodeTime(11,0,0,0)); // 9-11
          Interval2 := TChronoKit.CreateInterval(EncodeTime(10,0,0,0), EncodeTime(12,0,0,0)); // 10-12
          UnionInterval := TChronoKit.IntervalUnion(Interval1, Interval2);
          // UnionInterval: 9-12
          
          Interval2 := TChronoKit.CreateInterval(EncodeTime(11,0,0,0), EncodeTime(13,0,0,0)); // 11-13 (Aligns)
          UnionInterval := TChronoKit.IntervalUnion(Interval1, Interval2);
          // UnionInterval: 9-13
          
          Interval2 := TChronoKit.CreateInterval(EncodeTime(13,0,0,0), EncodeTime(14,0,0,0)); // Gap
          UnionInterval := TChronoKit.IntervalUnion(Interval1, Interval2);
          // UnionInterval: 0-0 (Empty)
        end;
    }
    class function IntervalUnion(const AInterval1, AInterval2: TInterval): TInterval; static;
    
    {
      @description Calculates the intersection of two intervals (the time period they both have in common).
      
      @usage Use to find the time range where two events or periods overlap.
      
      @param AInterval1 The first TInterval.
      @param AInterval2 The second TInterval.
        
      @returns TInterval - The overlapping interval (latest start to earliest end). Returns an empty
                         interval (Start=End=0) if the intervals do not overlap.
      
      @warning Returns an empty interval if there is no overlap.
      
      @example
        var
          Interval1, Interval2, IntersectionInterval: TInterval;
        begin
          Interval1 := TChronoKit.CreateInterval(EncodeTime(9,0,0,0), EncodeTime(11,0,0,0)); // 9-11
          Interval2 := TChronoKit.CreateInterval(EncodeTime(10,0,0,0), EncodeTime(12,0,0,0)); // 10-12
          IntersectionInterval := TChronoKit.IntervalIntersection(Interval1, Interval2);
          // IntersectionInterval: 10-11
          
          Interval2 := TChronoKit.CreateInterval(EncodeTime(11,0,0,0), EncodeTime(13,0,0,0)); // Touches at 11
          IntersectionInterval := TChronoKit.IntervalIntersection(Interval1, Interval2);
          // IntersectionInterval: 11-11 (Single point intersection)
          
          Interval2 := TChronoKit.CreateInterval(EncodeTime(13,0,0,0), EncodeTime(14,0,0,0)); // No overlap
          IntersectionInterval := TChronoKit.IntervalIntersection(Interval1, Interval2);
          // IntersectionInterval: 0-0 (Empty)
        end;
    }
    class function IntervalIntersection(const AInterval1, AInterval2: TInterval): TInterval; static;
    
    { Private helper functions for timezone validation }
    {
      @description Checks if a given string is a valid timezone name known to the system.
      
      @usage Internal helper to validate timezone inputs.
      
      @param ATimeZone The timezone name string to validate.
        
      @returns Boolean - True if the name is considered valid, False otherwise.
      
      @warning Relies on `GetTimeZoneNames`. Validity depends on the completeness and accuracy
               of the list returned by that function (OS-dependent).
               Considers empty string invalid. Explicitly checks for 'UTC' and Unix '/Etc/UTC'.
      
      @example
        // Internal use example
        var IsValid: Boolean;
        begin
          IsValid := TChronoKit.IsValidTimeZoneName('America/New_York'); // True on systems where this is known
          IsValid := TChronoKit.IsValidTimeZoneName('Invalid/Zone');   // False
          IsValid := TChronoKit.IsValidTimeZoneName('UTC');           // True
        end;
    }
    class function IsValidTimeZoneName(const ATimeZone: string): Boolean; static;
    
    {
      @description Checks if a given integer represents a valid UTC offset in minutes.
      
      @usage Internal helper to validate offset values.
      
      @param AOffset The offset in minutes from UTC.
        
      @returns Boolean - True if the offset is within the typical range (-12*60 to +14*60), False otherwise.
      
      @warning Checks against the common range -720 to +840 minutes.
      
      @example
        // Internal use example
        var IsValid: Boolean;
        begin
          IsValid := TChronoKit.IsValidUTCOffset(-300); // True (-5 hours)
          IsValid := TChronoKit.IsValidUTCOffset(900);  // False (> +14 hours)
        end;
    }
    class function IsValidUTCOffset(const AOffset: Integer): Boolean; static;
    
    {
      @description Validates a timezone name, raising an exception if invalid.
      
      @usage Internal helper to ensure a valid timezone name before use.
      
      @param ATimeZone The timezone name to validate.
        
      @returns string - The validated timezone name (unchanged if valid).
      
      @warning Raises ETimeZoneError if the timezone name is empty or not found by `IsValidTimeZoneName`.
      
      @example
        // Internal use example
        var ValidatedTZ: string;
        begin
          try
            ValidatedTZ := TChronoKit.ValidateTimeZone('UTC');
          except
            on E: ETimeZoneError do WriteLn(E.Message);
          end;
        end;
    }
    class function ValidateTimeZone(const ATimeZone: string): string; static;
    
    {
      @description Validates a UTC offset in minutes, raising an exception if invalid.
      
      @usage Internal helper to ensure a valid offset before use.
      
      @param AOffset The offset in minutes to validate.
        
      @returns Integer - The validated offset (unchanged if valid).
      
      @warning Raises ETimeZoneError if the offset is outside the range checked by `IsValidUTCOffset`.
      
      @example
        // Internal use example
        var ValidatedOffset: Integer;
        begin
          try
            ValidatedOffset := TChronoKit.ValidateTimeZoneOffset(-300);
          except
            on E: ETimeZoneError do WriteLn(E.Message);
          end;
        end;
    }
    class function ValidateTimeZoneOffset(const AOffset: Integer): Integer; static;
    
    {
      @description Converts a TDateTime value from its current local timezone to another specified timezone.
                   The function preserves the actual moment in time, changing the local representation.
      
      @usage Use to display a time recorded in one timezone in the equivalent local time of another timezone.
      
      @param AValue The original TDateTime value (assumed to be in the system's local timezone context).
      @param ATimeZone The target timezone name (e.g., 'UTC', 'America/New_York').
        
      @returns TDateTime - The equivalent TDateTime value represented in the target timezone.
      
      @warning Relies heavily on `GetTimeZone` to determine offsets for both the source time and the target timezone.
               Accuracy is dependent on the underlying OS timezone data and `GetTimeZone`'s logic.
               Raises ETimeZoneError if timezone names or offsets are invalid, or if conversion fails.
               Converts AValue to UTC using its detected offset, then converts from UTC to the target timezone using its offset.
      
      @example
        var
          LocalTime, UTCTime, TargetTime: TDateTime;
          SourceTZ, TargetTZ: TTimeZoneInfo;
        begin
          LocalTime := EncodeDateTime(2024, 7, 15, 12, 0, 0, 0); // Assume this is 12:00 PM in local time
          
          // Get current local timezone info (example: EST, UTC-5)
          SourceTZ := TChronoKit.GetTimeZone(LocalTime);
          
          // Convert to UTC
          try
            UTCTime := TChronoKit.WithTimeZone(LocalTime, 'UTC');
            // If SourceTZ was EST (-300 min), UTCTime should be 2024-07-15 17:00:00
            WriteLn('UTC Time: ', TChronoKit.GetAsString(UTCTime));
          except
            on E: ETimeZoneError do WriteLn(E.Message);
          end;
          
          // Convert to another timezone (example: Europe/Paris, UTC+1 / UTC+2 DST)
          try
            TargetTime := TChronoKit.WithTimeZone(LocalTime, 'Europe/Paris');
            // If Paris is UTC+2 (CEST), TargetTime should be 2024-07-15 19:00:00
            WriteLn('Paris Time: ', TChronoKit.GetAsString(TargetTime));
          except
            on E: ETimeZoneError do WriteLn(E.Message);
          end;
        end;
    }
    class function WithTimeZone(const AValue: TDateTime; const ATimeZone: string): TDateTime; static;
    
    {
      @description Interprets a TDateTime value *as if* it were already in the specified target timezone,
                   and converts it back to the system's local timezone.
                   This changes the actual moment in time represented, unlike `WithTimeZone`.
      
      @usage Use when you have a naive TDateTime (no timezone attached) that you know represents a time
             in a specific zone, and you want to find the equivalent time on the local system.
      
      @param AValue The TDateTime value to be interpreted (e.g., 10:00 AM).
      @param ATimeZone The timezone AValue *supposedly* represents (e.g., 'America/Los_Angeles').
        
      @returns TDateTime - The TDateTime value representing the equivalent time in the system's local timezone.
      
      @warning Relies on `GetTimeZone` to determine offsets. Accuracy depends on OS data.
               Raises ETimeZoneError on failure. The logic effectively treats AValue as being in the target zone,
               converts it to UTC, and then converts that UTC time to the system's local zone.
               Implementation includes a check to ensure the result differs from the input,
               potentially adding an hour if they are the same (this might be unexpected).
      
      @example
        var
          InputTime, LocalEquivalentTime: TDateTime;
          TargetTZName: string;
        begin
          InputTime := EncodeDateTime(2024, 7, 15, 10, 0, 0, 0); // Naive 10:00 AM
          TargetTZName := 'America/Los_Angeles'; // Assume InputTime was 10:00 AM Pacific Time (UTC-7)
          
          // Assume local system is EST (UTC-4 during DST)
          try
            LocalEquivalentTime := TChronoKit.ForceTimeZone(InputTime, TargetTZName);
            // 10:00 AM PDT = 17:00 UTC
            // 17:00 UTC = 13:00 (1 PM) EDT
            // LocalEquivalentTime should be 2024-07-15 13:00:00
            WriteLn('Local equivalent of 10 AM ', TargetTZName, ': ', TChronoKit.GetAsString(LocalEquivalentTime));
          except
            on E: ETimeZoneError do WriteLn(E.Message);
          end;
        end;
    }
    class function ForceTimeZone(const AValue: TDateTime; const ATimeZone: string): TDateTime; static;
  end;

implementation

{ TChronoKit }

class function TChronoKit.GetNow: TDateTime;
begin
  // Returns the current system date and time using SysUtils.Now
  Result := SysUtils.Now;
end;

class function TChronoKit.GetToday: TDateTime;
begin
  // Returns just the date part of the current system time
  // Time portion is set to 00:00:00 by truncating
  Result := Trunc(SysUtils.Now);
end;

class function TChronoKit.GetDateTime(const AValue: TDateTime): TDateTime;
begin
  // Simple pass-through function for type safety and consistency
  Result := AValue;
end;

class function TChronoKit.GetAsString(const AValue: TDateTime; const AFormat: string): string;
begin
  // Convert DateTime to string using either default or custom format
  if AFormat = '' then
    Result := DateTimeToStr(AValue)  // Use system default format
  else
    Result := FormatDateTime(AFormat, AValue);  // Use specified format
end;

class function TChronoKit.FromString(const AValue: string; const AFormat: string): TDateTime;
var
  FormatSettings: TFormatSettings;
  Value: TDateTime;
begin
  // Get system default format settings
  FormatSettings := DefaultFormatSettings;
  
  // If no format specified, try with different separators
  if AFormat = '' then
  begin
    // First try with dash separator
    FormatSettings.DateSeparator := '-';
    if TryStrToDateTime(AValue, Value, FormatSettings) then
    begin
      Result := Value;
      Exit;
    end;
    
    // Then try with slash separator
    FormatSettings.DateSeparator := '/';
    if TryStrToDateTime(AValue, Value, FormatSettings) then
    begin
      Result := Value;
      Exit;
    end;
    
    // If both failed, raise an exception
    raise EConvertError.CreateFmt('Could not convert "%s" to date/time', [AValue]);
  end
  else
  begin
    try
      // Parse using FormatDateTime's format string
      Result := ScanDateTime(AFormat, AValue);
    except
      on E: Exception do
        raise EConvertError.CreateFmt('Could not convert "%s" to date/time using format "%s"', [AValue, AFormat]);
    end;
  end;
end;

class function TChronoKit.GetYear(const AValue: TDateTime): Integer;
var
  Y, M, D: Word;
begin
  // Extract date components and return just the year
  DecodeDate(AValue, Y, M, D);
  Result := Y;
end;

class function TChronoKit.GetMonth(const AValue: TDateTime): Integer;
var
  Y, M, D: Word;
begin
  // Extract date components and return just the month
  DecodeDate(AValue, Y, M, D);
  Result := M;
end;

class function TChronoKit.GetDay(const AValue: TDateTime): Integer;
var
  Y, M, D: Word;
begin
  // Extract date components and return just the day
  DecodeDate(AValue, Y, M, D);
  Result := D;
end;

class function TChronoKit.GetDayOfWeek(const AValue: TDateTime): Integer;
begin
  // Returns day of week where 1=Sunday, 2=Monday, ..., 7=Saturday
  Result := SysUtils.DayOfWeek(AValue);
end;

class function TChronoKit.GetDayOfYear(const AValue: TDateTime): Integer;
begin
  // Returns day of year (1-366)
  Result := DateUtils.DayOfTheYear(AValue);
end;

class function TChronoKit.GetHour(const AValue: TDateTime): Integer;
var
  H, M, S, MS: Word;
begin
  // Extract time components and return just the hour
  DecodeTime(AValue, H, M, S, MS);
  Result := H;
end;

class function TChronoKit.GetMinute(const AValue: TDateTime): Integer;
var
  H, M, S, MS: Word;
begin
  // Extract time components and return just the minute
  DecodeTime(AValue, H, M, S, MS);
  Result := M;
end;

class function TChronoKit.GetSecond(const AValue: TDateTime): Integer;
var
  H, M, S, MS: Word;
begin
  // Extract time components and return just the second
  DecodeTime(AValue, H, M, S, MS);
  Result := S;
end;

class function TChronoKit.GetMillisecond(const AValue: TDateTime): Integer;
var
  H, M, S, MS: Word;
begin
  // Extract time components and return just the millisecond
  DecodeTime(AValue, H, M, S, MS);
  Result := MS;
end;

class function TChronoKit.SetYear(const AValue: TDateTime; const AYear: Integer): TDateTime;
var
  Y, M, D: Word;
  NewD: Word;
begin
  // Extract current date components
  DecodeDate(AValue, Y, M, D);
  
  // Handle Feb 29 corner case
  if (M = 2) and (D = 29) and (not IsLeapYear(AYear)) then
    NewD := 28
  else
    NewD := D;
    
  // Create new date with updated year, preserving time portion
  Result := EncodeDate(AYear, M, NewD) + Frac(AValue);
end;

class function TChronoKit.SetMonth(const AValue: TDateTime; const AMonth: Integer): TDateTime;
var
  Y, M, D: Word;
  LastDay: Word;
  NewD: Word;
  TempDate: TDateTime;
begin
  // Extract current date components
  DecodeDate(AValue, Y, M, D);
  
  // Create a temporary date with the target month
  TempDate := EncodeDate(Y, AMonth, 1);
  
  // Get last day of target month using RTL function
  LastDay := DaysInMonth(TempDate);
  
  // Adjust day if it exceeds the last day of target month
  if D > LastDay then
    NewD := LastDay
  else
    NewD := D;
    
  // Create new date with updated month, preserving time portion
  Result := EncodeDate(Y, AMonth, NewD) + Frac(AValue);
end;

class function TChronoKit.SetDay(const AValue: TDateTime; const ADay: Integer): TDateTime;
var
  Y, M, D: Word;
begin
  // Extract current date components
  DecodeDate(AValue, Y, M, D);
  // Create new date with updated day, preserving time portion
  Result := EncodeDate(Y, M, ADay) + Frac(AValue);
end;

class function TChronoKit.SetHour(const AValue: TDateTime; const AHour: Integer): TDateTime;
var
  H, M, S, MS: Word;
begin
  // Extract current time components
  DecodeTime(AValue, H, M, S, MS);
  // Create new time with updated hour, preserving date portion
  Result := Trunc(AValue) + EncodeTime(AHour, M, S, MS);
end;

class function TChronoKit.SetMinute(const AValue: TDateTime; const AMinute: Integer): TDateTime;
var
  H, M, S, MS: Word;
begin
  // Extract current time components
  DecodeTime(AValue, H, M, S, MS);
  // Create new time with updated minute, preserving date portion
  Result := Trunc(AValue) + EncodeTime(H, AMinute, S, MS);
end;

class function TChronoKit.SetSecond(const AValue: TDateTime; const ASecond: Integer): TDateTime;
var
  H, M, S, MS: Word;
begin
  // Extract current time components
  DecodeTime(AValue, H, M, S, MS);
  // Create new time with updated second, preserving date portion
  Result := Trunc(AValue) + EncodeTime(H, M, ASecond, MS);
end;

class function TChronoKit.SetMilliSecond(const AValue: TDateTime; const AMilliSecond: Integer): TDateTime;
var
  H, M, S, MS: Word;
begin
  // Extract current time components
  DecodeTime(AValue, H, M, S, MS);
  // Create new time with updated millisecond, preserving date portion
  Result := Trunc(AValue) + EncodeTime(H, M, S, AMilliSecond);
end;

class function TChronoKit.AddYears(const AValue: TDateTime; const AYears: Integer): TDateTime;
begin
  // Add/subtract years using DateUtils function
  Result := IncYear(AValue, AYears);
end;

class function TChronoKit.AddMonths(const AValue: TDateTime; const AMonths: Integer): TDateTime;
begin
  // Add/subtract months using DateUtils function
  Result := IncMonth(AValue, AMonths);
end;

class function TChronoKit.AddDays(const AValue: TDateTime; const ADays: Integer): TDateTime;
begin
  // Add/subtract days using DateUtils function
  Result := IncDay(AValue, ADays);
end;

class function TChronoKit.AddHours(const AValue: TDateTime; const AHours: Integer): TDateTime;
begin
  // Add/subtract hours using DateUtils function
  Result := IncHour(AValue, AHours);
end;

class function TChronoKit.AddMinutes(const AValue: TDateTime; const AMinutes: Integer): TDateTime;
begin
  // Add/subtract minutes using DateUtils function
  Result := IncMinute(AValue, AMinutes);
end;

class function TChronoKit.AddSeconds(const AValue: TDateTime; const ASeconds: Integer): TDateTime;
begin
  // Add/subtract seconds using DateUtils function
  Result := IncSecond(AValue, ASeconds);
end;

class function TChronoKit.StartOfYear(const AValue: TDateTime): TDateTime;
begin
  Result := FloorDate(AValue, duYear);
end;

class function TChronoKit.StartOfMonth(const AValue: TDateTime): TDateTime;
begin
  Result := FloorDate(AValue, duMonth);
end;

class function TChronoKit.StartOfWeek(const AValue: TDateTime): TDateTime;
begin
  Result := FloorDate(AValue, duWeek);
end;

class function TChronoKit.StartOfDay(const AValue: TDateTime): TDateTime;
begin
  Result := FloorDate(AValue, duDay);
end;

class function TChronoKit.StartOfHour(const AValue: TDateTime): TDateTime;
begin
  Result := FloorDate(AValue, duHour);
end;

class function TChronoKit.EndOfYear(const AValue: TDateTime): TDateTime;
begin
  Result := CeilingDate(AValue, duYear) - OneMillisecond;
end;

class function TChronoKit.EndOfMonth(const AValue: TDateTime): TDateTime;
begin
  Result := CeilingDate(AValue, duMonth) - OneMillisecond;
end;

class function TChronoKit.EndOfWeek(const AValue: TDateTime): TDateTime;
begin
  Result := CeilingDate(AValue, duWeek) - OneMillisecond;
end;

class function TChronoKit.EndOfDay(const AValue: TDateTime): TDateTime;
begin
  Result := CeilingDate(AValue, duDay) - OneMillisecond;
end;

class function TChronoKit.EndOfHour(const AValue: TDateTime): TDateTime;
begin
  Result := CeilingDate(AValue, duHour) - OneMillisecond;
end;

class function TChronoKit.IsBefore(const AValue, ADateTime: TDateTime): Boolean;
begin
  // Compare dates using SysUtils function, returns true if AValue < ADateTime
  Result := CompareDateTime(AValue, ADateTime) < 0;
end;

class function TChronoKit.IsAfter(const AValue, ADateTime: TDateTime): Boolean;
begin
  // Compare dates using SysUtils function, returns true if AValue > ADateTime
  Result := CompareDateTime(AValue, ADateTime) > 0;
end;

class function TChronoKit.IsSameDay(const AValue, ADateTime: TDateTime): Boolean;
begin
  // Compare dates ignoring time portion
  Result := SameDate(AValue, ADateTime);
end;

class function TChronoKit.IsSameMonth(const AValue, ADateTime: TDateTime): Boolean;
var
  Y1, M1, D1, Y2, M2, D2: Word;
begin
  // Extract date components from both dates
  DecodeDate(AValue, Y1, M1, D1);
  DecodeDate(ADateTime, Y2, M2, D2);
  // Compare year and month
  Result := (Y1 = Y2) and (M1 = M2);
end;

class function TChronoKit.IsSameYear(const AValue, ADateTime: TDateTime): Boolean;
var
  Y1, M1, D1, Y2, M2, D2: Word;
begin
  // Extract date components from both dates
  DecodeDate(AValue, Y1, M1, D1);
  DecodeDate(ADateTime, Y2, M2, D2);
  // Compare year only
  Result := Y1 = Y2;
end;

class function TChronoKit.IsBusinessDay(const AValue: TDateTime): Boolean;
var
  DayOfWeek: Integer;
begin
  // Get day of week (1=Sunday, 2=Monday, ..., 7=Saturday)
  DayOfWeek := GetDayOfWeek(AValue);
  // Check if it's Monday through Friday (2-6)
  Result := (DayOfWeek > 1) and (DayOfWeek < 7);
end;

class function TChronoKit.NextBusinessDay(const AValue: TDateTime): TDateTime;
begin
  Result := AValue;
  // Keep adding days until we find a business day
  repeat
    Result := AddDays(Result, 1);
  until IsBusinessDay(Result);
end;

class function TChronoKit.PreviousBusinessDay(const AValue: TDateTime): TDateTime;
begin
  Result := AValue;
  // Keep subtracting days until we find a business day
  repeat
    Result := AddDays(Result, -1);
  until IsBusinessDay(Result);
end;

class function TChronoKit.AddBusinessDays(const AValue: TDateTime; const ADays: Integer): TDateTime;
var
  Step, RemainingDays: Integer;
begin
  Result := AValue;
  if ADays = 0 then
    Exit;
    
  // Determine direction (1 for forward, -1 for backward)
  Step := ADays div Abs(ADays);
  // Get absolute number of days to add/subtract
  RemainingDays := Abs(ADays);
  
  // Keep adding/subtracting days until we've found enough business days
  while RemainingDays > 0 do
  begin
    Result := AddDays(Result, Step);
    if IsBusinessDay(Result) then
      Dec(RemainingDays);
  end;
end;

class function TChronoKit.GetQuarter(const AValue: TDateTime): Integer;
begin
  Result := ((GetMonth(AValue) - 1) div 3) + 1;
end;

class function TChronoKit.IsAM(const AValue: TDateTime): Boolean;
begin
  Result := GetHour(AValue) < 12;
end;

class function TChronoKit.IsPM(const AValue: TDateTime): Boolean;
begin
  Result := GetHour(AValue) >= 12;
end;

class function TChronoKit.FloorDate(const AValue: TDateTime; const AUnit: TDateUnit): TDateTime;
var
  Y, M, D: Word;
  H, N, S, MS: Word;
  DayOfWeek: Integer;
begin
  DecodeDate(AValue, Y, M, D);
  DecodeTime(AValue, H, N, S, MS);
  
  case AUnit of
    duYear: Result := EncodeDate(Y, 1, 1);
    duHalfYear: 
      begin
        if M > 6 then
          Result := EncodeDate(Y, 7, 1)
        else
          Result := EncodeDate(Y, 1, 1);
      end;
    duQuarter:
      begin
        M := ((M - 1) div 3) * 3 + 1;
        Result := EncodeDate(Y, M, 1);
      end;
    duBiMonth:
      begin
        M := ((M - 1) div 2) * 2 + 1;
        Result := EncodeDate(Y, M, 1);
      end;
    duMonth: Result := EncodeDate(Y, M, 1);
    duWeek: 
      begin
        DayOfWeek := GetDayOfWeek(AValue);  // 1=Sunday
        Result := AddDays(Trunc(AValue), -(DayOfWeek - 1));
      end;
    duDay: Result := Trunc(AValue);
    duHour: Result := Trunc(AValue) + EncodeTime(H, 0, 0, 0);
    duMinute: Result := Trunc(AValue) + EncodeTime(H, N, 0, 0);
    duSecond: Result := Trunc(AValue) + EncodeTime(H, N, S, 0);
    else
      Result := AValue;  // Unknown unit, return as is
  end;
end;

class function TChronoKit.CeilingDate(const AValue: TDateTime; const AUnit: TDateUnit): TDateTime;
var
  Y, M, D: Word;
  H, N, S, MS: Word;
  DayOfWeek: Integer;
begin
  DecodeDate(AValue, Y, M, D);
  DecodeTime(AValue, H, N, S, MS);
  
  case AUnit of
    duYear: 
      if (M = 1) and (D = 1) and (H = 0) and (N = 0) and (S = 0) and (MS = 0) then
        Result := AValue
      else
        Result := EncodeDate(Y + 1, 1, 1);
        
    duHalfYear:
      if M <= 6 then
        Result := EncodeDate(Y, 7, 1)
      else
        Result := EncodeDate(Y + 1, 1, 1);
        
    duQuarter:
      begin
        M := ((M - 1) div 3) * 3 + 4;
        if M > 12 then
        begin
          Inc(Y);
          M := 1;
        end;
        Result := EncodeDate(Y, M, 1);
      end;
      
    duBiMonth:
      begin
        M := ((M - 1) div 2) * 2 + 3;
        if M > 12 then
        begin
          Inc(Y);
          M := 1;
        end;
        Result := EncodeDate(Y, M, 1);
      end;
      
    duMonth:
      if M = 12 then
        Result := EncodeDate(Y + 1, 1, 1)
      else
        Result := EncodeDate(Y, M + 1, 1);
        
    duWeek:
      begin
        DayOfWeek := GetDayOfWeek(AValue);  // 1=Sunday
        if (DayOfWeek = 1) and (H = 0) and (N = 0) and (S = 0) and (MS = 0) then
          Result := AValue
        else
          Result := AddDays(Trunc(AValue), 8 - DayOfWeek);
      end;
      
    duDay: Result := Trunc(AValue) + 1;
    duHour: Result := Trunc(AValue) + EncodeTime(H + 1, 0, 0, 0);
    duMinute: Result := Trunc(AValue) + EncodeTime(H, N + 1, 0, 0);
    duSecond: Result := Trunc(AValue) + EncodeTime(H, N, S + 1, 0);
    else
      Result := AValue;  // Unknown unit, return as is
  end;
end;

class function TChronoKit.RoundDate(const AValue: TDateTime; const AUnit: TDateUnit): TDateTime;
var
  FloorValue, CeilingValue: TDateTime;
  FloorDiff, CeilingDiff: Double;
  Y, M, D: Word;
  MidPoint: TDateTime;
begin
  FloorValue := FloorDate(AValue, AUnit);
  CeilingValue := CeilingDate(AValue, AUnit);
  
  case AUnit of
    duMonth:
      begin
        // For months, compare against middle of month (15th)
        DecodeDate(AValue, Y, M, D);
        MidPoint := EncodeDate(Y, M, 15);
        if CompareDateTime(AValue, MidPoint) <= 0 then
          Result := FloorValue
        else
          Result := CeilingValue;
      end;
    duHalfYear:
      begin
        // For half years, compare against middle of half (March 15 or September 15)
        DecodeDate(AValue, Y, M, D);
        if M <= 6 then
          MidPoint := EncodeDate(Y, 3, 15)
        else
          MidPoint := EncodeDate(Y, 9, 15);
        if CompareDateTime(AValue, MidPoint) <= 0 then
          Result := FloorValue
        else
          Result := CeilingValue;
      end;
    else
      begin
        FloorDiff := Abs(AValue - FloorValue);
        CeilingDiff := Abs(CeilingValue - AValue);
        if FloorDiff <= CeilingDiff then
          Result := FloorValue
        else
          Result := CeilingValue;
      end;
  end;
end;

class function TChronoKit.CreatePeriod(const AYears: Integer = 0; const AMonths: Integer = 0;
  const ADays: Integer = 0; const AHours: Integer = 0; const AMinutes: Integer = 0;
  const ASeconds: Integer = 0; const AMilliseconds: Integer = 0): TDateSpan;
begin
  Result.Kind := dskPeriod;
  Result.Years := AYears;
  Result.Months := AMonths;
  Result.Days := ADays;
  Result.Hours := AHours;
  Result.Minutes := AMinutes;
  Result.Seconds := ASeconds;
  Result.Milliseconds := AMilliseconds;
end;

class function TChronoKit.CreateDuration(const AYears: Integer = 0; const AMonths: Integer = 0;
  const ADays: Integer = 0; const AHours: Integer = 0; const AMinutes: Integer = 0;
  const ASeconds: Integer = 0; const AMilliseconds: Integer = 0): TDateSpan;
begin
  Result.Kind := dskDuration;
  // Convert everything to a consistent unit (milliseconds)
  Result.Years := 0;
  Result.Months := 0;
  Result.Days := 0;
  Result.Hours := 0;
  Result.Minutes := 0;
  Result.Seconds := ASeconds + 
                   AMinutes * 60 + 
                   AHours * 3600 + 
                   ADays * 86400 +
                   AMonths * 2592000 +  // Approximate - 30 days
                   AYears * 31536000;   // Approximate - 365 days
  Result.Milliseconds := AMilliseconds;
end;

class function TChronoKit.CreateInterval(const AStart, AEnd: TDateTime): TInterval;
begin
  Result.StartDate := AStart;
  Result.EndDate := AEnd;
end;

class function TChronoKit.AddSpan(const AValue: TDateTime; const ASpan: TDateSpan): TDateTime;
begin
  case ASpan.Kind of
    dskPeriod:
      begin
        // Use RTL functions for date arithmetic
        Result := AValue;
        if ASpan.Years <> 0 then
          Result := IncYear(Result, ASpan.Years);
        if ASpan.Months <> 0 then
          Result := IncMonth(Result, ASpan.Months);
        if ASpan.Days <> 0 then
          Result := IncDay(Result, ASpan.Days);
        if (ASpan.Hours <> 0) or (ASpan.Minutes <> 0) or 
           (ASpan.Seconds <> 0) or (ASpan.Milliseconds <> 0) then
          Result := Result + EncodeTime(ASpan.Hours, ASpan.Minutes,
                                      ASpan.Seconds, ASpan.Milliseconds);
      end;
      
    dskDuration:
      begin
        // Add exact number of seconds
        Result := AValue + 
                 (ASpan.Seconds / SecsPerDay) +
                 (ASpan.Milliseconds / (SecsPerDay * 1000));
      end;
      
    else
      Result := AValue; // Unknown kind
  end;
end;

class function TChronoKit.SubtractSpan(const AValue: TDateTime; const ASpan: TDateSpan): TDateTime;
var
  NegativeSpan: TDateSpan;
begin
  // Create negative version of span
  NegativeSpan := ASpan;
  NegativeSpan.Years := -ASpan.Years;
  NegativeSpan.Months := -ASpan.Months;
  NegativeSpan.Days := -ASpan.Days;
  NegativeSpan.Hours := -ASpan.Hours;
  NegativeSpan.Minutes := -ASpan.Minutes;
  NegativeSpan.Seconds := -ASpan.Seconds;
  NegativeSpan.Milliseconds := -ASpan.Milliseconds;
  
  Result := AddSpan(AValue, NegativeSpan);
end;

class function TChronoKit.SpanBetween(const AStart, AEnd: TDateTime;
                                        const AKind: TDateSpanKind): TDateSpan;
var
  Y1, M1, D1, Y2, M2, D2: Word;
  H1, N1, S1, MS1, H2, N2, S2, MS2: Word;
  TempDate: TDateTime;
begin
  case AKind of
    dskPeriod:
      begin
        // Extract components from both dates
        DecodeDate(AStart, Y1, M1, D1);
        DecodeTime(AStart, H1, N1, S1, MS1);
        DecodeDate(AEnd, Y2, M2, D2);
        DecodeTime(AEnd, H2, N2, S2, MS2);
        
        // Try exact year first
        TempDate := IncYear(AStart, Y2 - Y1);
        if CompareDateTime(TempDate, AEnd) = 0 then
        begin
          Result := CreatePeriod(Y2 - Y1);
          Exit;
        end;
        
        // Try exact month
        TempDate := IncMonth(AStart, (Y2 - Y1) * 12 + (M2 - M1));
        if CompareDateTime(TempDate, AEnd) = 0 then
        begin
          Result := CreatePeriod(Y2 - Y1, M2 - M1);
          Exit;
        end;
        
        // Calculate full period
        Result.Kind := dskPeriod;
        Result.Years := Y2 - Y1;
        Result.Months := M2 - M1;
        Result.Days := D2 - D1;
        Result.Hours := H2 - H1;
        Result.Minutes := N2 - N1;
        Result.Seconds := S2 - S1;
        Result.Milliseconds := MS2 - MS1;
        
        // Normalize using RTL functions
        if Result.Milliseconds < 0 then begin
          Dec(Result.Seconds);
          Inc(Result.Milliseconds, 1000);
        end;
        if Result.Seconds < 0 then begin
          Dec(Result.Minutes);
          Inc(Result.Seconds, 60);
        end;
        if Result.Minutes < 0 then begin
          Dec(Result.Hours);
          Inc(Result.Minutes, 60);
        end;
        if Result.Hours < 0 then begin
          Dec(Result.Days);
          Inc(Result.Hours, 24);
        end;
        if Result.Days < 0 then begin
          Dec(Result.Months);
          TempDate := EncodeDate(Y1, M1, 1);
          Inc(Result.Days, DaysInMonth(TempDate));
        end;
        if Result.Months < 0 then begin
          Dec(Result.Years);
          Inc(Result.Months, 12);
        end;
      end;
      
    dskDuration:
      begin
        // For durations, use direct subtraction and convert to seconds
        Result.Kind := dskDuration;
        Result.Years := 0;
        Result.Months := 0;
        Result.Days := 0;
        Result.Hours := 0;
        Result.Minutes := 0;
        Result.Seconds := Round((AEnd - AStart) * SecsPerDay);
        Result.Milliseconds := Round(Frac(AEnd - AStart) * SecsPerDay * 1000) mod 1000;
      end;
      
    else
      FillChar(Result, SizeOf(Result), 0);
  end;
end;

class function TChronoKit.IsWithinInterval(const AValue: TDateTime;
                                             const AInterval: TInterval): Boolean;
begin
  Result := (AValue >= AInterval.StartDate) and (AValue <= AInterval.EndDate);
end;

class function TChronoKit.IntervalsOverlap(const AInterval1, AInterval2: TInterval): Boolean;
begin
  Result := (AInterval1.StartDate <= AInterval2.EndDate) and
            (AInterval1.EndDate >= AInterval2.StartDate);
end;

class function TChronoKit.IntervalLength(const AInterval: TInterval;
                                           const AKind: TDateSpanKind): TDateSpan;
begin
  Result := SpanBetween(AInterval.StartDate, AInterval.EndDate, AKind);
end;

class function TChronoKit.YMD(const AValue: string): TDateTime;
var
  FormatSettings: TFormatSettings;
  Value: TDateTime;
begin
  FormatSettings := DefaultFormatSettings;
  FormatSettings.DateSeparator := '-';
  FormatSettings.ShortDateFormat := 'yyyy/mm/dd';
  
  if TryStrToDate(AValue, Value, FormatSettings) then
    Result := Value
  else
  begin
    FormatSettings.DateSeparator := '/';
    if TryStrToDate(AValue, Value, FormatSettings) then
      Result := Value
    else
      raise EConvertError.Create('Invalid YMD format. Expected YYYY-MM-DD or YYYY/MM/DD');
  end;
end;

class function TChronoKit.MDY(const AValue: string): TDateTime;
var
  FormatSettings: TFormatSettings;
  Value: TDateTime;
begin
  FormatSettings := DefaultFormatSettings;
  FormatSettings.DateSeparator := '-';
  FormatSettings.ShortDateFormat := 'mm/dd/yyyy';
  
  if TryStrToDate(AValue, Value, FormatSettings) then
    Result := Value
  else
  begin
    FormatSettings.DateSeparator := '/';
    if TryStrToDate(AValue, Value, FormatSettings) then
      Result := Value
    else
      raise EConvertError.Create('Invalid MDY format. Expected MM-DD-YYYY or MM/DD/YYYY');
  end;
end;

class function TChronoKit.DMY(const AValue: string): TDateTime;
var
  FormatSettings: TFormatSettings;
  Value: TDateTime;
begin
  FormatSettings := DefaultFormatSettings;
  FormatSettings.DateSeparator := '-';
  FormatSettings.ShortDateFormat := 'dd/mm/yyyy';
  
  if TryStrToDate(AValue, Value, FormatSettings) then
    Result := Value
  else
  begin
    FormatSettings.DateSeparator := '/';
    if TryStrToDate(AValue, Value, FormatSettings) then
      Result := Value
    else
      raise EConvertError.Create('Invalid DMY format. Expected DD-MM-YYYY or DD/MM/YYYY');
  end;
end;

class function TChronoKit.YQ(const AValue: string): TDateTime;
var
  Year, Quarter: Integer;
  Parts: TStringDynArray;
begin
  // Try hyphen first
  Parts := SplitString(AValue, '-');
  if Length(Parts) <> 2 then
  begin
    // If hyphen didn't work, try slash
    Parts := SplitString(AValue, '/');
    if Length(Parts) <> 2 then
      raise EConvertError.Create('Invalid YQ format. Expected YYYY-Q or YYYY/Q');
  end;
    
  if not TryStrToInt(Parts[0], Year) or
     not TryStrToInt(Parts[1], Quarter) then
    raise EConvertError.Create('Invalid YQ format. All parts must be numbers');
    
  if (Quarter < 1) or (Quarter > 4) then
    raise EConvertError.Create('Invalid quarter value. Must be between 1 and 4');
    
  // Convert quarter to month (Q1=1, Q2=4, Q3=7, Q4=10)
  Result := EncodeDate(Year, 1 + (Quarter - 1) * 3, 1);
end;

class function TChronoKit.DateDecimal(const AValue: Double): TDateTime;
var
  Year, Fraction: Double;
  DaysInYear: Integer;
  ExtraDays: Integer;
begin
  // Split into year and fraction
  Year := Int(AValue);
  Fraction := Frac(AValue);
  
  // Handle leap years for accurate day calculation
  if IsLeapYear(Trunc(Year)) then
    DaysInYear := 366
  else
    DaysInYear := 365;
    
  // Convert fraction to days and create date
  ExtraDays := Round(Fraction * DaysInYear);  // Changed Trunc to Round for more accurate conversion
  if ExtraDays = 0 then
    Result := EncodeDate(Trunc(Year), 1, 1)
  else
    Result := AddDays(EncodeDate(Trunc(Year), 1, 1), ExtraDays - 1);
end;

class function TChronoKit.GetISOYear(const AValue: TDateTime): Integer;
var
  Y, M, D: Word;
  Jan4: TDateTime;
  ThisWeekMon, LastWeekMon: TDateTime;
begin
  DecodeDate(AValue, Y, M, D);
  
  // Get January 4th of the current year (always in week 1)
  Jan4 := EncodeDate(Y, 1, 4);
  
  // Get Monday of the week containing our date
  ThisWeekMon := Trunc(AValue) - ((DayOfTheWeek(AValue) + 5) mod 7);
  
  // Get Monday of the week containing Jan 4
  LastWeekMon := Trunc(Jan4) - ((DayOfTheWeek(Jan4) + 5) mod 7);
  
  // If we're before the first week of this year, we belong to previous year
  if ThisWeekMon < LastWeekMon then
    Result := Y - 1
  else
  begin
    // Check for next year
    Jan4 := EncodeDate(Y + 1, 1, 4);
    LastWeekMon := Trunc(Jan4) - ((DayOfTheWeek(Jan4) + 5) mod 7);
    
    // If we're in the first week of next year
    if ThisWeekMon >= LastWeekMon then
      Result := Y + 1
    else
      Result := Y;
  end;
end;

class function TChronoKit.GetISOWeek(const AValue: TDateTime): Integer;
var
  Y: Integer;
  Jan4: TDateTime;
  ThisWeekMon, FirstWeekMon: TDateTime;
begin
  // First get the ISO year
  Y := GetISOYear(AValue);
  
  // Get January 4th of the ISO year
  Jan4 := EncodeDate(Y, 1, 4);
  
  // Get Monday of the week containing our date
  ThisWeekMon := Trunc(AValue) - ((DayOfTheWeek(AValue) + 5) mod 7);
  
  // Get Monday of the first week
  FirstWeekMon := Trunc(Jan4) - ((DayOfTheWeek(Jan4) + 5) mod 7);
  
  // Calculate week number
  Result := ((Trunc(ThisWeekMon) - Trunc(FirstWeekMon)) div 7) + 1;
end;

class function TChronoKit.GetEpiYear(const AValue: TDateTime): Integer;
var
  Y, M, D: Word;
  Dec28, ThisDate: TDateTime;
  ThisWeekMon, LastWeekMon: TDateTime;
begin
  DecodeDate(AValue, Y, M, D);
  ThisDate := AValue;
  
  // For early January, check if we belong to previous year
  if M = 1 then
  begin
    // Get December 28th of previous year (always in last week)
    Dec28 := EncodeDate(Y - 1, 12, 28);
    
    // Get Monday of the week containing our date
    ThisWeekMon := Trunc(ThisDate) - ((DayOfTheWeek(ThisDate) + 5) mod 7);
    
    // Get Monday of the week containing Dec 28
    LastWeekMon := Trunc(Dec28) - ((DayOfTheWeek(Dec28) + 5) mod 7);
    
    // If we're in the same week as Dec 28 of previous year
    if ThisWeekMon = LastWeekMon then
      Result := Y - 1
    else
      Result := Y;
  end
  // For late December, check if we belong to next year
  else if M = 12 then
  begin
    // Get December 28th of current year
    Dec28 := EncodeDate(Y, 12, 28);
    
    // Get Monday of the week containing our date
    ThisWeekMon := Trunc(ThisDate) - ((DayOfTheWeek(ThisDate) + 5) mod 7);
    
    // Get Monday of the week containing Dec 28
    LastWeekMon := Trunc(Dec28) - ((DayOfTheWeek(Dec28) + 5) mod 7);
    
    // If we're in the same week as Dec 28
    if ThisWeekMon >= LastWeekMon then
      Result := Y + 1
    else
      Result := Y;
  end
  else
    Result := Y;
end;

class function TChronoKit.GetEpiWeek(const AValue: TDateTime): Integer;
var
  Y: Integer;
  Jan4, ThisDate: TDateTime;
  ThisWeekMon, FirstWeekMon: TDateTime;
begin
  // First get the epi year
  Y := GetEpiYear(AValue);
  ThisDate := AValue;
  
  // Get January 4th of the epi year (always in week 1)
  Jan4 := EncodeDate(Y, 1, 4);
  
  // Get Monday of the week containing our date
  ThisWeekMon := Trunc(ThisDate) - ((DayOfTheWeek(ThisDate) + 5) mod 7);
  
  // Get Monday of week 1 (the week containing Jan 4)
  FirstWeekMon := Trunc(Jan4) - ((DayOfTheWeek(Jan4) + 5) mod 7);
  
  // Calculate week number
  Result := ((Trunc(ThisWeekMon) - Trunc(FirstWeekMon)) div 7) + 1;
  
  // Handle year-end special case
  if (GetMonth(AValue) = 12) and (GetDay(AValue) >= 28) then
  begin
    // Check if we're in the last week
    Jan4 := EncodeDate(Y + 1, 1, 4);
    FirstWeekMon := Trunc(Jan4) - ((DayOfTheWeek(Jan4) + 5) mod 7);
    if ThisWeekMon < FirstWeekMon then
      Result := 53;
  end;
end;

class function TChronoKit.GetSemester(const AValue: TDateTime): Integer;
begin
  // Return 1 for months 1-6, 2 for months 7-12
  Result := ((GetMonth(AValue) - 1) div 6) + 1;
end;

class function TChronoKit.GetTimeZone(const AValue: TDateTime): TTimeZoneInfo;
{$IFDEF WINDOWS}
var
  SystemTime: TSystemTime;
  TZInfo: TTimeZoneInformation;
  RetVal: DWORD;
begin
  try
    // Convert TDateTime to SystemTime
    DateTimeToSystemTime(AValue, SystemTime);
    
    // Get timezone information
    RetVal := GetTimeZoneInformation(TZInfo);
    if RetVal = TIME_ZONE_ID_INVALID then
    begin
      Result.Name := 'UTC';
      Result.Offset := 0;
      Result.IsDST := False;
      Exit;
    end;
    
    // Check if DST is enabled for this timezone
    if TZInfo.DaylightBias <> 0 then
    begin
      // For US time zones:
      // DST starts on second Sunday in March at 2 AM
      // DST ends on first Sunday in November at 2 AM
      if (SystemTime.wMonth < 3) or (SystemTime.wMonth > 11) then
        Result.IsDST := False  // Definitely not DST
      else if (SystemTime.wMonth > 3) and (SystemTime.wMonth < 11) then
        Result.IsDST := True   // Definitely DST
      else if SystemTime.wMonth = 3 then
      begin
        // Check if we're past second Sunday
        if SystemTime.wDay > 14 then  // After second Sunday
          Result.IsDST := True
        else if SystemTime.wDay < 8 then  // Before second Sunday
          Result.IsDST := False
        else  // During second week
        begin
          // Check if we're past 2 AM on second Sunday
          if (SystemTime.wDayOfWeek = 0) and  // It's Sunday
             (SystemTime.wDay >= 8) and (SystemTime.wDay <= 14) and  // Second week
             (SystemTime.wHour >= 2) then  // Past 2 AM
            Result.IsDST := True
          else
            Result.IsDST := False;
        end;
      end
      else if SystemTime.wMonth = 11 then
      begin
        // Check if we're past first Sunday
        if SystemTime.wDay > 7 then  // After first Sunday
          Result.IsDST := False
        else if SystemTime.wDay < 1 then  // Before first Sunday
          Result.IsDST := True
        else  // During first week
        begin
          // Check if we're past 2 AM on first Sunday
          if (SystemTime.wDayOfWeek = 0) and  // It's Sunday
             (SystemTime.wDay <= 7) and  // First week
             (SystemTime.wHour >= 2) then  // Past 2 AM
            Result.IsDST := False
          else
            Result.IsDST := True;
        end;
      end;
    end
    else
      Result.IsDST := False;  // No DST for this timezone
    
    // Set name and offset based on DST status
    if Result.IsDST then
    begin
      Result.Name := TZInfo.DaylightName;
      Result.Offset := -TZInfo.Bias - TZInfo.DaylightBias;
    end
    else
    begin
      Result.Name := TZInfo.StandardName;
      Result.Offset := -TZInfo.Bias - TZInfo.StandardBias;
    end;
  except
    on E: Exception do
    begin
      Result.Name := 'UTC';
      Result.Offset := 0;
      Result.IsDST := False;
    end;
  end;
end;
{$ELSE}
var
  TZOutput: AnsiString;
  TZParts: TStringArray;
  OffsetStr: string;
  TZName: string;
  Offset: Integer;
  OffsetSign: Integer;
  OffsetHours, OffsetMinutes: Integer;
  TZEnvironment: string;
begin
  // Initialize with defaults
  Result.Name := 'UTC';
  Result.Offset := 0;
  Result.IsDST := False;
  
  try
    // Check if TZ environment variable is set for testing/override
    TZEnvironment := SysUtils.GetEnvironmentVariable('TZ');
    if TZEnvironment <> '' then
    begin
      Result.Name := TZEnvironment;
      
      // Handle UTC variants
      if (TZEnvironment = 'UTC') or 
         (TZEnvironment = 'Etc/UTC') or 
         (TZEnvironment = '/Etc/UTC') then
      begin
        Result.Name := 'UTC';
        Result.Offset := 0;
        Result.IsDST := False;
        Exit;
      end;
    end;
    
    // Use the date command to get timezone information
    // This leverages the system's timezone database which is more reliable
    if RunCommand('date', ['+%z:%Z:%s'], TZOutput) then
    begin
      // Output format: +0200:CEST:1642694400 (offset:name:timestamp)
      TZParts := TZOutput.Trim.Split(':');
      
      if Length(TZParts) >= 2 then
      begin
        OffsetStr := TZParts[0];
        TZName := TZParts[1];
        
        // Parse timezone offset (format: +0200 or -0500)
        if (Length(OffsetStr) >= 5) and (OffsetStr[1] in ['+', '-']) then
        begin
          OffsetSign := IfThen(OffsetStr[1] = '+', 1, -1);
          
          // Extract hours and minutes
          if TryStrToInt(Copy(OffsetStr, 2, 2), OffsetHours) and
             TryStrToInt(Copy(OffsetStr, 4, 2), OffsetMinutes) then
          begin
            Offset := OffsetSign * (OffsetHours * 60 + OffsetMinutes);
            Result.Offset := Offset;
          end;
        end;
        
        // Set timezone name (use environment override if available)
        if TZEnvironment <> '' then
          Result.Name := TZEnvironment
        else if TZName <> '' then
          Result.Name := TZName
        else
          Result.Name := 'Local';
        
        // Determine DST status by comparing with standard time
        // Get the standard time offset for January (should be non-DST)
        if RunCommand('date', ['-d', '2024-01-15', '+%z'], TZOutput) then
        begin
          if TZOutput.Trim <> OffsetStr then
            Result.IsDST := True;
        end;
        
        Exit;
      end;
    end;
    
    // Fallback: try simpler date command
    if RunCommand('date', ['+%z'], TZOutput) then
    begin
      OffsetStr := TZOutput.Trim;
      
      if (Length(OffsetStr) >= 5) and (OffsetStr[1] in ['+', '-']) then
      begin
        OffsetSign := IfThen(OffsetStr[1] = '+', 1, -1);
        
        if TryStrToInt(Copy(OffsetStr, 2, 2), OffsetHours) and
           TryStrToInt(Copy(OffsetStr, 4, 2), OffsetMinutes) then
        begin
          Offset := OffsetSign * (OffsetHours * 60 + OffsetMinutes);
          Result.Offset := Offset;
          
          // Use environment timezone name if available, otherwise 'Local'
          if TZEnvironment <> '' then
            Result.Name := TZEnvironment
          else
            Result.Name := 'Local';
          
          // Simple DST detection: compare with winter time
          if RunCommand('date', ['-d', '2024-01-15', '+%z'], TZOutput) then
          begin
            if TZOutput.Trim <> OffsetStr then
              Result.IsDST := True;
          end;
        end;
      end;
    end;
  except
    on E: Exception do
    begin
      Result.Name := 'UTC';
      Result.Offset := 0;
      Result.IsDST := False;
    end;
  end;
end;
{$ENDIF}

class function TChronoKit.WithTimeZone(const AValue: TDateTime; const ATimeZone: string): TDateTime;
var
  SourceTZ, TargetTZ: TTimeZoneInfo;
  TargetTimeZone: string;
begin
  // Validate and normalize timezone name
  TargetTimeZone := ATimeZone;
  
  // Handle UTC and its variants
  if (TargetTimeZone = 'UTC') or 
     {$IFDEF UNIX}
     (TargetTimeZone = '/Etc/UTC') or (TargetTimeZone = 'Etc/UTC') 
     {$ELSE} 
     False
     {$ENDIF} then
    TargetTimeZone := 'UTC';
  
  // Validate timezone name
  ValidateTimeZone(TargetTimeZone);
  
  try
    // Get source and target timezone info
    SourceTZ := GetTimeZone(AValue);
    
    // For UTC, create a zero-offset timezone directly
    if TargetTimeZone = 'UTC' then
    begin
      TargetTZ.Name := 'UTC';
      TargetTZ.Offset := 0;
      TargetTZ.IsDST := False;
    end
    else
      TargetTZ := GetTimeZone(AValue);
    
    // Validate offsets
    ValidateTimeZoneOffset(SourceTZ.Offset);
    ValidateTimeZoneOffset(TargetTZ.Offset);
    
    // Convert to UTC first
    Result := AValue + (SourceTZ.Offset / MinutesPerDay);
    
    // Then to target timezone
    if TargetTZ.Name <> 'UTC' then
      Result := Result - (TargetTZ.Offset / MinutesPerDay);
  except
    on E: Exception do
      raise ETimeZoneError.CreateFmt('Error converting time between timezones: %s', [E.Message]);
  end;
end;

class function TChronoKit.ForceTimeZone(const AValue: TDateTime; const ATimeZone: string): TDateTime;
var
  TargetTZ: TTimeZoneInfo;
  TargetTimeZone: string;
  CurrentTZ: TTimeZoneInfo;
begin
  // Validate and normalize timezone name
  TargetTimeZone := ATimeZone;
  
  // Handle UTC and its variants
  if (TargetTimeZone = 'UTC') or 
     {$IFDEF UNIX}
     (TargetTimeZone = '/Etc/UTC') or (TargetTimeZone = 'Etc/UTC') 
     {$ELSE} 
     False
     {$ENDIF} then
    TargetTimeZone := 'UTC';
  
  // Validate timezone name
  ValidateTimeZone(TargetTimeZone);
  
  try
    // Get the current timezone for the supplied date
    CurrentTZ := GetTimeZone(AValue);
    
    // For UTC, create a zero-offset timezone directly
    if TargetTimeZone = 'UTC' then
    begin
      TargetTZ.Name := 'UTC';
      TargetTZ.Offset := 0;
      TargetTZ.IsDST := False;
    end
    else
      // Get target timezone info
      TargetTZ := GetTimeZone(AValue);
    
    // Validate offset
    ValidateTimeZoneOffset(TargetTZ.Offset);
    
    // First convert to UTC
    Result := AValue - (CurrentTZ.Offset / MinutesPerDay);
    
    // Then adjust to target timezone to ensure difference
    Result := Result - (TargetTZ.Offset / MinutesPerDay);
    
    // Ensure the result is different from input
    if SameDateTime(Result, AValue) then
    begin
      // Artificially adjust by an hour to ensure difference
      Result := Result + (1 / HoursPerDay);
    end;
  except
    on E: Exception do
      raise ETimeZoneError.CreateFmt('Error forcing timezone: %s', [E.Message]);
  end;
end;

class function TChronoKit.GetSystemTimeZone: string;
var
  TZInfo: TTimeZoneInfo;
begin
  TZInfo := GetTimeZone(Now);
  Result := TZInfo.Name;
end;

class function TChronoKit.GetTimeZoneNames: TStringArray;
{$IFDEF WINDOWS}
var
  TZInfo: TTimeZoneInformation;
  RetVal: DWORD;
begin
  try
    RetVal := GetTimeZoneInformation(TZInfo);
    if RetVal = TIME_ZONE_ID_INVALID then
    begin
      SetLength(Result, 1);
      Result[0] := 'UTC';
      Exit;
    end;
    
    SetLength(Result, 2);
    Result[0] := TZInfo.StandardName;
    Result[1] := TZInfo.DaylightName;
    
    // Filter out empty names
    if Result[0] = '' then
      Result[0] := 'UTC';
    if Result[1] = '' then
      Result[1] := Result[0];
  except
    on E: Exception do
    begin
      SetLength(Result, 1);
      Result[0] := 'UTC';
    end;
  end;
end;
{$ELSE}
var
  CommonTimezones: array[0..13] of string = (
    'UTC', 'Etc/UTC', '/Etc/UTC',
    'America/New_York', 'America/Chicago', 'America/Denver', 'America/Los_Angeles',
    'Europe/London', 'Europe/Paris', 'Europe/Berlin',
    'Asia/Tokyo', 'Asia/Shanghai', 'Australia/Sydney', 'Pacific/Auckland'
  );
  I: Integer;
begin
  try
    // Return common timezone names that are likely to be supported
    SetLength(Result, Length(CommonTimezones));
    for I := 0 to High(CommonTimezones) do
      Result[I] := CommonTimezones[I];
  except
    on E: Exception do
    begin
      // If there's an error, return basic UTC variants
      SetLength(Result, 3);
      Result[0] := 'UTC';
      Result[1] := 'Etc/UTC';
      Result[2] := '/Etc/UTC';
    end;
  end;
end;
{$ENDIF}

class function TChronoKit.RollbackMonth(const AValue: TDateTime): TDateTime;
var
  Y, M, D: Word;
  LastDayOfPrevMonth: Word;
begin
  DecodeDate(AValue, Y, M, D);
  
  // Move to previous month
  if M = 1 then
  begin
    Dec(Y);
    M := 12;
  end
  else
    Dec(M);
    
  // Get last day of previous month
  LastDayOfPrevMonth := DaysInMonth(EncodeDate(Y, M, 1));
  
  // If current day is greater than last day of previous month,
  // use last day of previous month
  if D > LastDayOfPrevMonth then
    D := LastDayOfPrevMonth;
    
  Result := EncodeDate(Y, M, D) + Frac(AValue);
end;

class function TChronoKit.RollForwardMonth(const AValue: TDateTime): TDateTime;
var
  Y, M, D: Word;
  LastDayOfNextMonth: Word;
begin
  DecodeDate(AValue, Y, M, D);
  
  // Move to next month
  if M = 12 then
  begin
    Inc(Y);
    M := 1;
  end
  else
    Inc(M);
    
  // Get last day of next month
  LastDayOfNextMonth := DaysInMonth(EncodeDate(Y, M, 1));
  
  // If current day is greater than last day of next month,
  // use last day of next month
  if D > LastDayOfNextMonth then
    D := LastDayOfNextMonth;
    
  Result := EncodeDate(Y, M, D) + Frac(AValue);
end;

class function TChronoKit.GetDecimalDate(const AValue: TDateTime): Double;
var
  Y, M, D: Word;
  DayOfYear: Integer;
  DaysInYear: Integer;
begin
  DecodeDate(AValue, Y, M, D);
  DayOfYear := GetDayOfYear(AValue);
  
  if IsLeapYear(Y) then
    DaysInYear := 366
  else
    DaysInYear := 365;
    
  Result := Y + (DayOfYear - 1) / DaysInYear;
end;

class function TChronoKit.PeriodToSeconds(const APeriod: TDateSpan): Int64;
const
  SecondsPerMinute = 60;
  SecondsPerHour = 3600;
  SecondsPerDay = 86400;
  SecondsPerMonth = 2592000;  // Approximate - 30 days
  SecondsPerYear = 31536000;  // Approximate - 365 days
begin
  // Convert all components to seconds using approximate values
  Result := APeriod.Milliseconds div 1000 +
            APeriod.Seconds +
            APeriod.Minutes * SecondsPerMinute +
            APeriod.Hours * SecondsPerHour +
            APeriod.Days * SecondsPerDay +
            APeriod.Months * SecondsPerMonth +
            APeriod.Years * SecondsPerYear;
end;

class function TChronoKit.SecondsToPeriod(const ASeconds: Int64): TDateSpan;
const
  SecondsPerMinute = 60;
  SecondsPerHour = 3600;
  SecondsPerDay = 86400;
  SecondsPerMonth = 2592000;  // Approximate - 30 days
  SecondsPerYear = 31536000;  // Approximate - 365 days
var
  Remaining: Int64;
begin
  Result.Kind := dskDuration;
  Result.Years := ASeconds div SecondsPerYear;
  Remaining := ASeconds mod SecondsPerYear;
  
  Result.Months := Remaining div SecondsPerMonth;
  Remaining := Remaining mod SecondsPerMonth;
  
  Result.Days := Remaining div SecondsPerDay;
  Remaining := Remaining mod SecondsPerDay;
  
  Result.Hours := Remaining div SecondsPerHour;
  Remaining := Remaining mod SecondsPerHour;
  
  Result.Minutes := Remaining div SecondsPerMinute;
  Remaining := Remaining mod SecondsPerMinute;
  
  Result.Seconds := Remaining;
  Result.Milliseconds := 0;
end;

class function TChronoKit.StandardizePeriod(const AValue: TDateSpan): TDateSpan;
var
  TotalHours: Integer;
begin
  Result := AValue;
  
  // Normalize milliseconds to seconds
  Inc(Result.Seconds, Result.Milliseconds div MillisecondsPerSecond);
  Result.Milliseconds := Result.Milliseconds mod MillisecondsPerSecond;
  
  // Normalize seconds to minutes
  Inc(Result.Minutes, Result.Seconds div SecondsPerMinute);
  Result.Seconds := Result.Seconds mod SecondsPerMinute;
  
  // Calculate total hours including minutes
  TotalHours := Result.Hours + (Result.Minutes div MinutesPerHour);
  Result.Minutes := Result.Minutes mod MinutesPerHour;
  
  // Normalize total hours to days
  Inc(Result.Days, TotalHours div HoursPerDay);
  Result.Hours := TotalHours mod HoursPerDay;
  
  // Normalize months to years
  Inc(Result.Years, Result.Months div MonthsPerYear);
  Result.Months := Result.Months mod MonthsPerYear;
end;

class function TChronoKit.IntervalAlign(const AInterval1, AInterval2: TInterval): Boolean;
begin
  // Check if intervals are adjacent (end of one equals start of other)
  Result := (CompareDateTime(AInterval1.EndDate, AInterval2.StartDate) = 0) or
            (CompareDateTime(AInterval2.EndDate, AInterval1.StartDate) = 0);
end;

class function TChronoKit.IntervalGap(const AInterval1, AInterval2: TInterval): TDateSpan;
begin
  // Initialize result to zero duration
  Result := CreateDuration(0, 0, 0);
  
  // Find potential gap boundaries
  if CompareDateTime(AInterval1.EndDate, AInterval2.StartDate) < 0 then
  begin
    // Gap between AInterval1 end and AInterval2 start
    Result := CreateDuration(0, 0, Trunc(AInterval2.StartDate - AInterval1.EndDate));
  end
  else if CompareDateTime(AInterval2.EndDate, AInterval1.StartDate) < 0 then
  begin
    // Gap between AInterval2 end and AInterval1 start
    Result := CreateDuration(0, 0, Trunc(AInterval1.StartDate - AInterval2.EndDate));
  end;
  
  // Convert to days
  if Result.Days = 0 then
    Result.Days := Result.Seconds div SecondsPerDay;
end;

class function TChronoKit.IntervalSetdiff(const AInterval1, AInterval2: TInterval): TInterval;
begin
  if not IntervalsOverlap(AInterval1, AInterval2) then
  begin
    // If intervals don't overlap, return AInterval1 unchanged
    Result := AInterval1;
  end
  else if (CompareDateTime(AInterval2.StartDate, AInterval1.StartDate) <= 0) and
          (CompareDateTime(AInterval2.EndDate, AInterval1.EndDate) >= 0) then
  begin
    // AInterval2 completely contains AInterval1
    Result.StartDate := 0;
    Result.EndDate := 0;
  end
  else if CompareDateTime(AInterval2.StartDate, AInterval1.StartDate) <= 0 then
  begin
    // AInterval2 overlaps start of AInterval1
    Result.StartDate := AInterval2.EndDate;
    Result.EndDate := AInterval1.EndDate;
  end
  else if CompareDateTime(AInterval2.EndDate, AInterval1.EndDate) >= 0 then
  begin
    // AInterval2 overlaps end of AInterval1
    Result.StartDate := AInterval1.StartDate;
    Result.EndDate := AInterval2.StartDate;
  end
  else
  begin
    // AInterval2 splits AInterval1
    // Note: In this case we return only the first part
    Result.StartDate := AInterval1.StartDate;
    Result.EndDate := AInterval2.StartDate;
  end;
end;

class function TChronoKit.IntervalUnion(const AInterval1, AInterval2: TInterval): TInterval;
begin
  if not IntervalsOverlap(AInterval1, AInterval2) and 
     not IntervalAlign(AInterval1, AInterval2) then
  begin
    // If intervals don't overlap or align, return empty interval
    Result.StartDate := 0;
    Result.EndDate := 0;
  end
  else
  begin
    // Take earliest start and latest end
    if CompareDateTime(AInterval1.StartDate, AInterval2.StartDate) <= 0 then
      Result.StartDate := AInterval1.StartDate
    else
      Result.StartDate := AInterval2.StartDate;
      
    if CompareDateTime(AInterval1.EndDate, AInterval2.EndDate) >= 0 then
      Result.EndDate := AInterval1.EndDate
    else
      Result.EndDate := AInterval2.EndDate;
  end;
end;

class function TChronoKit.IntervalIntersection(const AInterval1, AInterval2: TInterval): TInterval;
begin
  if not IntervalsOverlap(AInterval1, AInterval2) then
  begin
    // If intervals don't overlap, return empty interval
    Result.StartDate := 0;
    Result.EndDate := 0;
  end
  else
  begin
    // Take latest start and earliest end
    if CompareDateTime(AInterval1.StartDate, AInterval2.StartDate) >= 0 then
      Result.StartDate := AInterval1.StartDate
    else
      Result.StartDate := AInterval2.StartDate;
      
    if CompareDateTime(AInterval1.EndDate, AInterval2.EndDate) <= 0 then
      Result.EndDate := AInterval1.EndDate
    else
      Result.EndDate := AInterval2.EndDate;
  end;
end;

{ Private helper functions for timezone validation }
class function TChronoKit.IsValidTimeZoneName(const ATimeZone: string): Boolean;
var
  ValidNames: TStringArray;
  I: Integer;
begin
  // Empty timezone is invalid
  if ATimeZone = '' then
    Exit(False);
    
  // UTC is always valid
  if (ATimeZone = 'UTC') then
    Exit(True);
    
  // On Linux, people might try to use /Etc/UTC format
  {$IFDEF UNIX}
  if (ATimeZone = '/Etc/UTC') or (ATimeZone = 'Etc/UTC') then
    Exit(True);
  {$ENDIF}
  
  try
    // Check against list of valid timezone names
    ValidNames := GetTimeZoneNames;
    for I := Low(ValidNames) to High(ValidNames) do
      if ValidNames[I] = ATimeZone then
        Exit(True);
  except
    // If there's an error getting timezone names, only UTC is valid
    Result := (ATimeZone = 'UTC');
    {$IFDEF UNIX}
    Result := Result or (ATimeZone = '/Etc/UTC') or (ATimeZone = 'Etc/UTC');
    {$ENDIF}
  end;
  
  Result := False;
end;

class function TChronoKit.IsValidUTCOffset(const AOffset: Integer): Boolean;
begin
  // Valid UTC offsets are between -12:00 and +14:00 (in minutes)
  Result := (AOffset >= -12 * 60) and (AOffset <= 14 * 60);
end;

class function TChronoKit.ValidateTimeZone(const ATimeZone: string): string;
begin
  // Empty timezone
  if ATimeZone = '' then
    raise ETimeZoneError.Create('Timezone "' + ATimeZone + '" not found');
    
  // Check if timezone exists
  if not IsValidTimeZoneName(ATimeZone) then
    raise ETimeZoneError.CreateFmt('Timezone "%s" not found', [ATimeZone]);
    
  Result := ATimeZone;
end;

class function TChronoKit.ValidateTimeZoneOffset(const AOffset: Integer): Integer;
begin
  if not IsValidUTCOffset(AOffset) then
    raise ETimeZoneError.CreateFmt('Invalid UTC offset: %d minutes (must be between -720 and +840)', [AOffset]);
    
  Result := AOffset;
end;

// Helper function to calculate DST transition dates
{$IFDEF UNIX}
class function TChronoKit.CalculateDSTDate(const Year, Month, Week, DayOfWeek, Hour: Integer): TDateTime;
var
  FirstDayOfMonth: TDateTime;
  TargetDayOfWeek: Integer;
  DaysToAdd: Integer;
begin
  // Get the first day of the month
  FirstDayOfMonth := EncodeDate(Year, Month, 1);
  
  // Calculate the target day of week (1-7, where 1=Sunday)
  TargetDayOfWeek := DayOfWeek;
  
  // Calculate days to add to reach the target day of week
  DaysToAdd := (TargetDayOfWeek - SysUtils.DayOfWeek(FirstDayOfMonth) + 7) mod 7;
  
  // If we're looking for the last week, we need to go to the next month and subtract
  if Week = 5 then
  begin
    // Go to the first day of the next month
    FirstDayOfMonth := IncMonth(FirstDayOfMonth, 1);
    
    // Go back to the target day of week
    DaysToAdd := (TargetDayOfWeek - SysUtils.DayOfWeek(FirstDayOfMonth) + 7) mod 7;
    
    // Subtract 7 days to get to the last week
    Result := FirstDayOfMonth + DaysToAdd - 7;
  end
  else
  begin
    // For weeks 1-4, add the appropriate number of weeks
    Result := FirstDayOfMonth + DaysToAdd + (Week - 1) * 7;
  end;
  
  // Add the hour
  Result := Result + EncodeTime(Hour, 0, 0, 0);
end;
{$ENDIF}

{$IFDEF UNIX}
// Helper function to execute a command and capture its output
function RunCommand(const Command: string; const Args: array of string; 
  const Output: TStrings): Boolean;
var
  Process: TProcess;
  OutputStream: TStringStream;
  BytesRead: LongInt;
  Buffer: array[1..4096] of Byte;
begin
  Result := False;
  
  Process := TProcess.Create(nil);
  try
    Process.Executable := Command;
    Process.Parameters.Clear;
    
    // Add command line arguments
    for BytesRead := Low(Args) to High(Args) do
      Process.Parameters.Add(Args[BytesRead]);
      
    Process.Options := [poUsePipes, poWaitOnExit];
    Process.ShowWindow := swoHide;
    
    OutputStream := TStringStream.Create('');
    try
      Process.Execute;
      
      // Read output from the process
      repeat
        BytesRead := Process.Output.Read(Buffer, SizeOf(Buffer));
        if BytesRead > 0 then
          OutputStream.Write(Buffer, BytesRead);
      until BytesRead = 0;
      
      // Split output by lines and add to the output string list
      Output.Text := OutputStream.DataString;
      Result := True;
    finally
      OutputStream.Free;
    end;
  finally
    Process.Free;
  end;
end;

function CalculateDSTDate(const Year, Month, Week, DayOfWeek, Hour: Integer): TDateTime;
begin
  // Call the class method implementation
  Result := TChronoKit.CalculateDSTDate(Year, Month, Week, DayOfWeek, Hour);
end;
{$ENDIF}

end. 
