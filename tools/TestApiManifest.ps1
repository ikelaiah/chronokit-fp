[CmdletBinding()]
param()

$ErrorActionPreference = 'Stop'

$generatorPath = Join-Path $PSScriptRoot 'GenerateApiManifest.ps1'
foreach ($platform in @('windows', 'linux')) {
  & $generatorPath -Platform $platform -Check
}

$repositoryRoot = Split-Path -Parent $PSScriptRoot
$windowsManifest = Get-Content -LiteralPath (
  Join-Path $repositoryRoot 'api\ChronoKit-v1.7.0-windows.txt') -Raw
$linuxManifest = Get-Content -LiteralPath (
  Join-Path $repositoryRoot 'api\ChronoKit-v1.7.0-linux.txt') -Raw

foreach ($required in @(
  'MillisecondsPerSecond = 1000;',
  'TCalendarPeriod = record',
  'TDateSpanKind = (',
  "deprecated 'Use TCalendarPeriod or TDuration; no tag is required';",
  'TChronoKit = class',
  'private',
  'public',
  'class function CreateBusinessCalendar(',
  'overload; static;',
  "deprecated 'Use TimeZoneToSystemLocal';"
)) {
  if (($windowsManifest -notmatch [regex]::Escape($required)) -or
      ($linuxManifest -notmatch [regex]::Escape($required))) {
    throw "API manifests do not capture required declaration: $required"
  }
}

if ($windowsManifest -notmatch
    [regex]::Escape('TSystemTime = Windows.TSystemTime;')) {
  throw 'Windows API manifest does not contain the Windows system-time alias.'
}
if ($linuxManifest -notmatch [regex]::Escape('TSystemTime = record')) {
  throw 'Linux API manifest does not contain the portable system-time record.'
}
if ($linuxManifest -match [regex]::Escape('external ''kernel32.dll''')) {
  throw 'Linux API manifest contains a Windows-only declaration.'
}

Write-Host 'Windows and Linux API manifests cover the complete v1.7 facade.'
