[CmdletBinding(DefaultParameterSetName = "Expression")]
param(
    [Parameter(Mandatory = $true, ParameterSetName = "Expression")]
    [string]$Expression,

    [Parameter(Mandatory = $true, ParameterSetName = "File")]
    [string]$File,

    [Parameter(ParameterSetName = "File")]
    [string[]]$Arguments = @()
)

$rCommand = Get-Command R.exe -ErrorAction SilentlyContinue | Select-Object -First 1
$rExecutable = if ($null -ne $rCommand) {
    $rCommand.Source
} else {
    $roots = @($env:ProgramFiles, ${env:ProgramFiles(x86)}) |
        Where-Object { -not [string]::IsNullOrWhiteSpace($_) -and (Test-Path $_) }
    $candidates = foreach ($root in $roots) {
        Get-ChildItem -Path (Join-Path $root "R") -Filter R.exe -Recurse -ErrorAction SilentlyContinue
    }
    $selected = $candidates |
        Where-Object { $_.FullName -match "[\\/]bin([\\/]x64)?[\\/]R\.exe$" } |
        Sort-Object LastWriteTimeUtc -Descending |
        Select-Object -First 1
    if ($null -eq $selected) {
        throw "R.exe was not found on PATH or under a standard Program Files R installation."
    }
    $selected.FullName
}

if ($PSCmdlet.ParameterSetName -eq "Expression") {
    & $rExecutable --vanilla -q -e $Expression
} else {
    & $rExecutable --vanilla --slave -f $File --args @Arguments
}

exit $LASTEXITCODE