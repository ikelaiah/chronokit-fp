[CmdletBinding()]
param(
  [switch]$KeepArtifacts
)

$ErrorActionPreference = 'Stop'

$repositoryRoot = Split-Path -Parent $PSScriptRoot
$stagingRoot = Join-Path $repositoryRoot 'build-temp\clean-consumers'
if (Test-Path -LiteralPath $stagingRoot) {
  Remove-Item -LiteralPath $stagingRoot -Recurse -Force
}
New-Item -ItemType Directory -Path $stagingRoot | Out-Null

function Invoke-CheckedCommand {
  param([string]$FileName, [string[]]$Arguments)

  & $FileName @Arguments
  if ($LASTEXITCODE -ne 0) {
    throw "$FileName failed with exit code $LASTEXITCODE."
  }
}

function Invoke-ConsumerProgram {
  param([string]$ProgramPath)

  & $ProgramPath
  if ($LASTEXITCODE -ne 0) {
    throw "$ProgramPath failed with exit code $LASTEXITCODE."
  }
}

try {
$fpc = (Get-Command fpc -ErrorAction Stop).Source
$sourceFixture = Join-Path $repositoryRoot 'tests\consumer\source\SourceConsumer.lpr'
$sourceStaging = Join-Path $stagingRoot 'source'
$sourceLibrary = Join-Path $sourceStaging 'chronokit-fp'
$sourceOutput = Join-Path $sourceStaging 'out'
New-Item -ItemType Directory -Path $sourceLibrary, $sourceOutput | Out-Null
Copy-Item -LiteralPath (Join-Path $repositoryRoot 'src') -Destination $sourceLibrary -Recurse
Copy-Item -LiteralPath $sourceFixture -Destination $sourceStaging
Invoke-CheckedCommand $fpc @(
  "-FU$sourceOutput",
  "-FE$sourceOutput",
  "-Fu$(Join-Path $sourceLibrary 'src')",
  (Join-Path $sourceStaging 'SourceConsumer.lpr')
)
$sourceExecutable = Join-Path $sourceOutput 'SourceConsumer'
if ($IsWindows) {
  $sourceExecutable += '.exe'
}
Invoke-ConsumerProgram $sourceExecutable

$lazarusFixture = Join-Path $repositoryRoot 'tests\consumer\lazarus\LazarusConsumer.lpr'
$lazarusProjectFixture = Join-Path $repositoryRoot 'tests\consumer\lazarus\LazarusConsumer.lpi'
$lazarusStaging = Join-Path $stagingRoot 'lazarus'
$lazarusLibrary = Join-Path $lazarusStaging 'chronokit-fp'
$lazarusOutput = Join-Path $lazarusStaging 'out'
New-Item -ItemType Directory -Path $lazarusLibrary, $lazarusOutput | Out-Null
Copy-Item -LiteralPath (Join-Path $repositoryRoot 'src') -Destination $lazarusLibrary -Recurse
Copy-Item -LiteralPath (Join-Path $repositoryRoot 'packages') -Destination $lazarusLibrary -Recurse
Copy-Item -LiteralPath $lazarusFixture -Destination $lazarusStaging
Copy-Item -LiteralPath $lazarusProjectFixture -Destination $lazarusStaging
$packageOutput = Join-Path $lazarusLibrary 'packages\lazarus\lib\consumer'
New-Item -ItemType Directory -Path $packageOutput | Out-Null
Invoke-CheckedCommand $fpc @(
  "-FU$packageOutput",
  "-Fu$(Join-Path $lazarusLibrary 'src')",
  (Join-Path $lazarusLibrary 'packages\lazarus\chronokit_fp.pas')
)
if (Get-Command lazbuild -ErrorAction SilentlyContinue) {
  Invoke-CheckedCommand 'lazbuild' @('-B',
    (Join-Path $lazarusStaging 'LazarusConsumer.lpi'))
}
else {
  Invoke-CheckedCommand $fpc @(
    "-FU$lazarusOutput",
    "-FE$lazarusOutput",
    "-Fu$packageOutput",
    (Join-Path $lazarusStaging 'LazarusConsumer.lpr')
  )
}
$lazarusExecutable = Join-Path $lazarusOutput 'LazarusConsumer'
if ($IsWindows) {
  $lazarusExecutable += '.exe'
}
Invoke-ConsumerProgram $lazarusExecutable

Write-Host 'Clean source and Lazarus consumer fixtures passed.'
}
finally {
  if (-not $KeepArtifacts -and (Test-Path -LiteralPath $stagingRoot)) {
    Remove-Item -LiteralPath $stagingRoot -Recurse -Force
  }
  if ((Test-Path -LiteralPath $stagingRoot) -and
    (Get-ChildItem -LiteralPath $stagingRoot -Force | Measure-Object).Count -eq 0) {
    Remove-Item -LiteralPath $stagingRoot -Force
  }
  $buildRoot = Split-Path -Parent $stagingRoot
  if ((Test-Path -LiteralPath $buildRoot) -and
    (Get-ChildItem -LiteralPath $buildRoot -Force | Measure-Object).Count -eq 0) {
    Remove-Item -LiteralPath $buildRoot -Force
  }
}
