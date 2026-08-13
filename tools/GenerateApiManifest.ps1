[CmdletBinding()]
param(
  [ValidateSet('windows', 'linux')]
  [string]$Platform,
  [switch]$Check
)

$ErrorActionPreference = 'Stop'

$repositoryRoot = Split-Path -Parent $PSScriptRoot
$sourcePath = Join-Path $repositoryRoot 'src\ChronoKit.pas'
$manifestPath = Join-Path $repositoryRoot (
  "api\ChronoKit-v1.7.0-$Platform.txt")
$definitions = [Collections.Generic.HashSet[string]]::new(
  [StringComparer]::OrdinalIgnoreCase)
if ($Platform -eq 'windows') {
  [void]$definitions.Add('WINDOWS')
} else {
  [void]$definitions.Add('UNIX')
}

$source = (Get-Content -LiteralPath $sourcePath -Raw) -replace '\r\n?', "`n"
$interfaceEnd = $source.IndexOf("`nimplementation")
if ($interfaceEnd -lt 0) {
  throw 'Could not locate the ChronoKit interface.'
}
$interfaceSource = $source.Substring(0, $interfaceEnd)
$declarationStart = $interfaceSource.IndexOf("`nconst")
if ($declarationStart -lt 0) {
  throw 'Could not locate the ChronoKit public declarations.'
}
$interfaceSource = $interfaceSource.Substring($declarationStart + 1)

$conditionalStack = [Collections.Generic.Stack[object]]::new()
$active = $true
$selectedLines = [Collections.Generic.List[string]]::new()
foreach ($line in ($interfaceSource -split "`n")) {
  if ($line -match '^\s*\{\$IFDEF\s+(?<name>[A-Za-z0-9_]+)\s*\}\s*$') {
    $condition = $definitions.Contains($Matches.name)
    $conditionalStack.Push([pscustomobject]@{
      ParentActive = $active
      Condition = $condition
    })
    $active = $active -and $condition
    continue
  }
  if ($line -match '^\s*\{\$IFNDEF\s+(?<name>[A-Za-z0-9_]+)\s*\}\s*$') {
    $condition = -not $definitions.Contains($Matches.name)
    $conditionalStack.Push([pscustomobject]@{
      ParentActive = $active
      Condition = $condition
    })
    $active = $active -and $condition
    continue
  }
  if ($line -match '^\s*\{\$ELSE\}\s*$') {
    if ($conditionalStack.Count -eq 0) {
      throw 'Found {$ELSE} without a matching conditional.'
    }
    $frame = $conditionalStack.Peek()
    $active = $frame.ParentActive -and -not $frame.Condition
    continue
  }
  if ($line -match '^\s*\{\$ENDIF\}\s*$') {
    if ($conditionalStack.Count -eq 0) {
      throw 'Found {$ENDIF} without a matching conditional.'
    }
    $frame = $conditionalStack.Pop()
    $active = $frame.ParentActive
    continue
  }
  if ($active) {
    $selectedLines.Add($line)
  }
}
if ($conditionalStack.Count -ne 0) {
  throw 'ChronoKit interface contains an unterminated conditional.'
}

$selected = $selectedLines -join "`n"
$selected = [regex]::Replace($selected, '(?ms)\{(?!\$).*?\}', '')
$selected = [regex]::Replace($selected, '(?m)//.*$', '')
$manifestLines = [Collections.Generic.List[string]]::new()
$manifestLines.Add('# ChronoKit-FP v1.7.0 public API manifest')
$manifestLines.Add("platform=$Platform")
$manifestLines.Add('')
foreach ($line in ($selected -split "`n")) {
  $normalized = [regex]::Replace($line.Trim(), '\s+', ' ')
  if ($normalized -ne '') {
    $manifestLines.Add($normalized)
  }
}
$content = ($manifestLines -join "`n") + "`n"

if ($Check) {
  if (-not (Test-Path -LiteralPath $manifestPath)) {
    throw "API manifest is missing: $manifestPath"
  }
  $existing = (Get-Content -LiteralPath $manifestPath -Raw) -replace
    '\r\n?', "`n"
  if ($existing -cne $content) {
    throw "API manifest is stale for $Platform. Run tools/GenerateApiManifest.ps1 -Platform $Platform."
  }
  Write-Host "API manifest is current for $Platform."
  exit 0
}

$manifestDirectory = Split-Path -Parent $manifestPath
New-Item -ItemType Directory -Path $manifestDirectory -Force | Out-Null
[IO.File]::WriteAllText($manifestPath, $content,
  [Text.UTF8Encoding]::new($false))
Write-Host "Generated API manifest for $Platform."
