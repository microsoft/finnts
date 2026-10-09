# Prints the full path to Rscript.exe, or exits 1 with guidance.
# Usage: powershell -NoProfile -ExecutionPolicy Bypass -File find_rscript.ps1
$ErrorActionPreference = 'SilentlyContinue'

$cmd = Get-Command Rscript.exe
if ($cmd) { Write-Output $cmd.Source; exit 0 }

$candidates = @()
foreach ($key in 'HKLM:\SOFTWARE\R-core\R', 'HKCU:\SOFTWARE\R-core\R') {
  $p = (Get-ItemProperty $key).InstallPath
  if ($p) { $candidates += (Join-Path $p 'bin\Rscript.exe') }
}
foreach ($base in @("$env:ProgramFiles\R", "$env:LOCALAPPDATA\Programs\R")) {
  if (Test-Path $base) {
    Get-ChildItem $base -Directory | Sort-Object Name -Descending | ForEach-Object {
      $candidates += (Join-Path $_.FullName 'bin\Rscript.exe')
    }
  }
}

foreach ($c in $candidates) {
  if (Test-Path $c) { Write-Output $c; exit 0 }
}

Write-Error 'R was not found. Ask the user before installing R (for example: winget install --id RProject.R -e).'
exit 1
