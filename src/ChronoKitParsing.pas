unit ChronoKitParsing;

{$mode objfpc}{$H+}{$J-}

interface

function CKFormatDateTime(const AValue: TDateTime;
  const AFormat: string): string;
function CKParseDateTime(const AValue: string;
  const AFormat: string): TDateTime;

implementation

uses
  SysUtils, DateUtils;

function CKFormatDateTime(const AValue: TDateTime;
  const AFormat: string): string;
begin
  if AFormat = '' then
    Result := DateTimeToStr(AValue)
  else
    Result := SysUtils.FormatDateTime(AFormat, AValue);
end;

function CKParseDateTime(const AValue: string;
  const AFormat: string): TDateTime;
var
  FormatSettings: TFormatSettings;
  Value: TDateTime;
begin
  FormatSettings := DefaultFormatSettings;
  if AFormat = '' then
  begin
    FormatSettings.DateSeparator := '-';
    if TryStrToDateTime(AValue, Value, FormatSettings) then
      Exit(Value);

    FormatSettings.DateSeparator := '/';
    if TryStrToDateTime(AValue, Value, FormatSettings) then
      Exit(Value);

    raise EConvertError.CreateFmt(
      'Invalid date/time input "%s". Expected a valid value in the system ' +
      'date/time format using "-" or "/" as the date separator',
      [AValue]);
  end;

  try
    Result := ScanDateTime(AFormat, AValue);
  except
    on E: Exception do
      raise EConvertError.CreateFmt(
        'Invalid date/time input "%s". Expected format "%s" with valid calendar values',
        [AValue, AFormat]);
  end;
end;

end.
