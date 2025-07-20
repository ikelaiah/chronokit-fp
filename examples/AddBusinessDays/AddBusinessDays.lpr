program AddBusinessDays;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, ChronoKit;


  // Get today's date and add 5 business days
var
  Today, NextWeek: TDateTime;
begin
  Today := TChronoKit.GetToday;
  NextWeek := TChronoKit.AddBusinessDays(Today, 5);
  WriteLn('Next workday: ', TChronoKit.GetAsString(NextWeek, 'yyyy-mm-dd'));

  // Pause
  WriteLn('Press enter key to quit ...');
  ReadLn;
end.
