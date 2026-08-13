[CmdletBinding()]
param()

$ErrorActionPreference = 'Stop'

$repositoryRoot = Split-Path -Parent $PSScriptRoot
$generatorPath = Join-Path $PSScriptRoot 'GenerateApiReference.ps1'
& $generatorPath -Check

$chronokitPath = Join-Path $repositoryRoot 'src\ChronoKit.pas'
$lineEndingTestPath = Join-Path $repositoryRoot (
  'build-temp\documentation-line-endings-{0}' -f
    [guid]::NewGuid().ToString('N'))
$windowsSourcePath = Join-Path $lineEndingTestPath 'ChronoKit.pas'
New-Item -ItemType Directory -Path $lineEndingTestPath -Force | Out-Null
try {
  $source = Get-Content -LiteralPath $chronokitPath -Raw
  $windowsSource = [regex]::Replace($source, '\r?\n', "`r`n")
  [IO.File]::WriteAllText($windowsSourcePath, $windowsSource,
    [Text.UTF8Encoding]::new($false))
  & $generatorPath -Check -SourceFile $windowsSourcePath
} finally {
  Remove-Item -LiteralPath $lineEndingTestPath -Recurse -Force
}

$learningPathPath = Join-Path $repositoryRoot 'docs\Learning-Path.md'
$examplesPath = Join-Path $repositoryRoot 'examples\LearningPath'
$learningPath = Get-Content -LiteralPath $learningPathPath -Raw
$examples = Get-ChildItem -LiteralPath $examplesPath -Filter '*.lpr' -File
foreach ($example in $examples) {
  $link = "../examples/LearningPath/$($example.Name)"
  if ($learningPath.IndexOf($link, [System.StringComparison]::Ordinal) -lt 0) {
    throw "Learning path does not link to executable example $($example.Name)."
  }
}

@{
  '01-DatesAndWallClocks.lpr' = @('StartOfQuarter', 'EndOfQuarter')
  '04-BusinessCalendars.lpr' = @('BusinessDaysBetween')
  '05-NamedTimeZones.lpr' = @('ConvertBetweenTimeZones')
}.GetEnumerator() | ForEach-Object {
  $sourcePath = Join-Path $examplesPath $_.Key
  $exampleSource = Get-Content -LiteralPath $sourcePath -Raw
  foreach ($method in $_.Value) {
    if ($exampleSource -notmatch "TChronoKit\.$method\b") {
      throw "$($_.Key) does not demonstrate $method."
    }
  }
}

$publicSource = Get-Content -LiteralPath $chronokitPath -Raw
foreach ($exampleName in @(
  '01-DatesAndWallClocks.lpr',
  '04-BusinessCalendars.lpr',
  '05-NamedTimeZones.lpr'
)) {
  if ($publicSource -notmatch [regex]::Escape(
    "examples/LearningPath/$exampleName")) {
    throw "The public declarations do not link to $exampleName."
  }
}

Write-Host "Documentation checks passed for $($examples.Count) executable learning examples."
