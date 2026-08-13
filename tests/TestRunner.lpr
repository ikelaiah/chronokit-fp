program TestRunner;

{$mode objfpc}{$H+}{$J-}

uses
  Classes
  , consoletestrunner
  , ChronoKit.DateBasics.Tests
  , ChronoKit.Parsing.Tests
  , ChronoKit.BusinessCalendars.Tests
  , ChronoKit.PeriodsDurations.Tests
  , ChronoKit.Ranges.Tests
  , ChronoKit.Rounding.Tests
  , ChronoKit.CalendarSystems.Tests
  , ChronoKit.TimeZones.Tests
  , ChronoKit.LegacyBehavior.Tests;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'FPCUnit Console test runner';
  Application.Run;
  Application.Free;
end.
