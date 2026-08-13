program LearningPeriodsAndDurations;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils,
  DateUtils,
  ChronoKit;

var
  OneMonth: TCalendarPeriod;
  TwentyFourHours: TDuration;
  StartValue: TDateTime;
begin
  StartValue := EncodeDateTime(2024, 1, 31, 10, 0, 0, 0);
  OneMonth := TChronoKit.CreateCalendarPeriod(0, 1);
  TwentyFourHours := TChronoKit.DurationFromParts(0, 24);

  WriteLn('Start: ', TChronoKit.FormatDateTime(StartValue, 'yyyy-mm-dd hh:nn'));
  WriteLn('One calendar month: ', TChronoKit.FormatDateTime(
    TChronoKit.AddPeriod(StartValue, OneMonth), 'yyyy-mm-dd hh:nn'));
  WriteLn('Exactly 24 hours: ', TChronoKit.FormatDateTime(
    TChronoKit.AddDuration(StartValue, TwentyFourHours), 'yyyy-mm-dd hh:nn'));
end.
