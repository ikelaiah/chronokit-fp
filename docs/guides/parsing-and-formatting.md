# Parsing and formatting

Use `ParseDateTime` to turn text into a `TDateTime` and `FormatDateTime` to
render one. These are the preferred v1.7 names; the older aliases remain
source-compatible but are deprecated.

## Use explicit formats for portable input

Pass a Free Pascal format whenever the input comes from a file, protocol, or
cross-machine configuration:

```pascal
Parsed := TChronoKit.ParseDateTime(
  '2026-08-11 14:05', 'yyyy-mm-dd hh:nn');
OutputText := TChronoKit.FormatDateTime(
  Parsed, 'dd mmm yyyy, hh:nn');
```

The common tokens are `yyyy` for a four-digit year, `mm` for month, `dd` for
day, `hh` for hour, `nn` for minutes, and `ss` for seconds. The difference
between `mm` and `nn` matters: minutes use `nn`.

## Handle invalid input

Invalid input raises `EConvertError`. Treat it as an input error at the
boundary of your application:

```pascal
try
  Value := TChronoKit.ParseDateTime(InputText, 'yyyy-mm-dd');
except
  on E: EConvertError do
    WriteLn('Enter a date as YYYY-MM-DD: ', E.Message);
end;
```

An empty format uses the system date/time format and accepts `-` or `/` as a
date separator. That can suit local interactive input, but it is not a stable
storage or interchange format.

For a first complete example, see [Getting Started](../Getting-Started.md).
