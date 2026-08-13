unit ChronoKitInternalTypes;

{$mode objfpc}{$H+}{$J-}

interface

type
  TCKBusinessWeekday = (
    ckbwdSunday,
    ckbwdMonday,
    ckbwdTuesday,
    ckbwdWednesday,
    ckbwdThursday,
    ckbwdFriday,
    ckbwdSaturday
  );
  TCKBusinessWeek = set of TCKBusinessWeekday;
  TCKBusinessHolidayArray = array of TDateTime;

  TCKBusinessCalendar = record
    WorkingDays: TCKBusinessWeek;
    Holidays: TCKBusinessHolidayArray;
  end;

  TCKCalendarPeriod = record
    Years: Integer;
    Months: Integer;
    Days: Integer;
    Hours: Integer;
    Minutes: Integer;
    Seconds: Integer;
    Milliseconds: Integer;
  end;

  TCKDuration = record
    Milliseconds: Int64;
  end;

  TCKDateTimeRange = record
    StartValue: TDateTime;
    EndValue: TDateTime;
  end;
  TCKDateTimeRangeArray = array of TCKDateTimeRange;

  TCKDateUnit = (
    ckduSecond,
    ckduMinute,
    ckduHour,
    ckduDay,
    ckduWeek,
    ckduMonth,
    ckduBiMonth,
    ckduQuarter,
    ckduSeason,
    ckduHalfYear,
    ckduYear
  );

implementation

end.
