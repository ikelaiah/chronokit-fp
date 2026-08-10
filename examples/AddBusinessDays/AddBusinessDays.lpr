program AddBusinessDays;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils,
  ChronoKit;

var
  StartDate, DueDate: TDateTime;
begin
  StartDate := EncodeDate(2026, 8, 7);
  DueDate := TChronoKit.AddBusinessDays(StartDate, 5);

  WriteLn('Start date: ', TChronoKit.GetAsString(StartDate, 'yyyy-mm-dd'));
  WriteLn('Five business days later: ',
    TChronoKit.GetAsString(DueDate, 'yyyy-mm-dd'));
end.
