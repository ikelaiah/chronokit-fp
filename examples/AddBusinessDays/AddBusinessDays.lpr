program AddBusinessDays;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils,
  ChronoKit;

var
  Calendar: TBusinessCalendar;
  StartDate, DueDate: TDateTime;
begin
  Calendar := TChronoKit.CreateBusinessCalendar([
    EncodeDate(2026, 8, 10)
  ]);
  StartDate := EncodeDate(2026, 8, 7);
  DueDate := TChronoKit.AddBusinessDays(StartDate, 5, Calendar);

  WriteLn('Start date: ', TChronoKit.FormatDateTime(StartDate, 'yyyy-mm-dd'));
  WriteLn('Excluded holiday: 2026-08-10');
  WriteLn('Five business days later: ',
    TChronoKit.FormatDateTime(DueDate, 'yyyy-mm-dd'));
end.
