unit ChronoKit.Parsing.Tests;

{$mode objfpc}{$H+}{$J-}
{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  Classes, SysUtils, DateUtils, fpcunit, testregistry,
  ChronoKit;

type
  TParsingTests = class(TTestCase)
  published
    procedure Test140_YMDValidationMessage;
    procedure Test141_MDYValidationMessage;
    procedure Test142_DMYValidationMessage;
    procedure Test143_YQValidationMessage;
    procedure Test145_YQYearValidationMessage;
    procedure Test148_ParseDateTimeValidationMessage;
  end;

implementation

procedure TParsingTests.Test140_YMDValidationMessage;
begin
  WriteLn('Test140_YMDValidationMessage:Starting');
  try
    TChronoKit.YMD('2024-02-30');
    Fail('YMD should reject a day outside the calendar month');
  except
    on E: EConvertError do
    begin
      AssertTrue('YMD error should include the rejected input',
        Pos('2024-02-30', E.Message) > 0);
      AssertTrue('YMD error should show the accepted shape',
        Pos('YYYY-MM-DD', E.Message) > 0);
    end;
  end;
  WriteLn('Test140_YMDValidationMessage:Finished');
end;

procedure TParsingTests.Test141_MDYValidationMessage;
begin
  WriteLn('Test141_MDYValidationMessage:Starting');
  try
    TChronoKit.MDY('02-30-2024');
    Fail('MDY should reject a day outside the calendar month');
  except
    on E: EConvertError do
    begin
      AssertTrue('MDY error should include the rejected input',
        Pos('02-30-2024', E.Message) > 0);
      AssertTrue('MDY error should show the accepted shape',
        Pos('MM-DD-YYYY', E.Message) > 0);
    end;
  end;
  WriteLn('Test141_MDYValidationMessage:Finished');
end;

procedure TParsingTests.Test142_DMYValidationMessage;
begin
  WriteLn('Test142_DMYValidationMessage:Starting');
  try
    TChronoKit.DMY('30-02-2024');
    Fail('DMY should reject a day outside the calendar month');
  except
    on E: EConvertError do
    begin
      AssertTrue('DMY error should include the rejected input',
        Pos('30-02-2024', E.Message) > 0);
      AssertTrue('DMY error should show the accepted shape',
        Pos('DD-MM-YYYY', E.Message) > 0);
    end;
  end;
  WriteLn('Test142_DMYValidationMessage:Finished');
end;

procedure TParsingTests.Test143_YQValidationMessage;
begin
  WriteLn('Test143_YQValidationMessage:Starting');
  try
    TChronoKit.YQ('2024-5');
    Fail('YQ should reject a quarter outside 1 through 4');
  except
    on E: EConvertError do
    begin
      AssertTrue('YQ error should include the rejected input',
        Pos('2024-5', E.Message) > 0);
      AssertTrue('YQ error should explain the valid quarter range',
        Pos('between 1 and 4', E.Message) > 0);
    end;
  end;
  WriteLn('Test143_YQValidationMessage:Finished');
end;

procedure TParsingTests.Test145_YQYearValidationMessage;
begin
  WriteLn('Test145_YQYearValidationMessage:Starting');
  try
    TChronoKit.YQ('0-1');
    Fail('YQ should reject a year outside the TDateTime range');
  except
    on E: EConvertError do
    begin
      AssertTrue('YQ year error should include the rejected input',
        Pos('0-1', E.Message) > 0);
      AssertTrue('YQ year error should explain the valid year range',
        Pos('between 1 and 9999', E.Message) > 0);
    end;
  end;
  WriteLn('Test145_YQYearValidationMessage:Finished');
end;

procedure TParsingTests.Test148_ParseDateTimeValidationMessage;
begin
  WriteLn('Test148_ParseDateTimeValidationMessage:Starting');
  try
    TChronoKit.ParseDateTime('not-a-date');
    Fail('ParseDateTime should reject invalid date/time input');
  except
    on E: EConvertError do
    begin
      AssertTrue('ParseDateTime error should include the rejected input',
        Pos('not-a-date', E.Message) > 0);
      AssertTrue('ParseDateTime error should explain the expected input',
        Pos('system date/time format', E.Message) > 0);
    end;
  end;
  WriteLn('Test148_ParseDateTimeValidationMessage:Finished');
end;

initialization
  RegisterTest(TParsingTests);

end.
