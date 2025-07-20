program ChronoKitQuickStart;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils,
  ChronoKit;

var
  CurrentTime, NextWorkday: TDateTime;
  TZInfo: TTimeZoneInfo;
  BusinessHours: TInterval;
  OffsetSign: string;

begin
  // Basic date/time operations
  CurrentTime := TChronoKit.GetNow;
  NextWorkday := TChronoKit.NextBusinessDay(CurrentTime);

  WriteLn('Current time: ', TChronoKit.GetAsString(CurrentTime, 'yyyy-mm-dd hh:nn:ss'));
  WriteLn('Next workday: ', TChronoKit.GetAsString(NextWorkday, 'yyyy-mm-dd'));

  // Business hours check (9 AM - 5 PM)
  BusinessHours := TChronoKit.CreateInterval(
    TChronoKit.StartOfDay(CurrentTime) + EncodeTime(9, 0, 0, 0),
    TChronoKit.StartOfDay(CurrentTime) + EncodeTime(17, 0, 0, 0)
  );

  if TChronoKit.IsWithinInterval(CurrentTime, BusinessHours) then
    WriteLn('✅ Within business hours')
  else
    WriteLn('❌ Outside business hours');

  // Timezone information
  TZInfo := TChronoKit.GetTimeZone(CurrentTime);
  if TZInfo.Offset >= 0 then
    OffsetSign := '+'
  else
    OffsetSign := '-';

  WriteLn('Timezone: ', TZInfo.Name, ' (UTC', OffsetSign, IntToStr(Abs(TZInfo.Offset) div 60), ')');
  WriteLn('DST active: ', BoolToStr(TZInfo.IsDST, 'Yes', 'No'));

  // Pause console
  WriteLn('Press enter to quit ...');
  ReadLn;
end.

