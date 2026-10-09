# One-line installer for the Finn skill (Windows PowerShell 5.1 or PowerShell 7).
#
#   irm https://raw.githubusercontent.com/microsoft/finnts/main/skills/finn/install_skill.ps1 | iex
#
# Downloads only the skills/finn folder from GitHub and copies it into your agent's
# personal skills folder. It does not install R or finnts; the skill does that later,
# after asking you. It never deletes or overwrites anything: if the skill is already
# installed it stops and tells you how to update.
#
# Optional environment variables (set them before running the line above):
#   FINN_SKILL_AGENT  copilot (default), claude, or agents
#   FINN_SKILL_DEST   exact folder to install into (overrides FINN_SKILL_AGENT)
#   FINN_SKILL_REPO   GitHub owner/repo (default microsoft/finnts)
#   FINN_SKILL_REF    branch, tag, or commit (default main)

& {
  $ErrorActionPreference = "Stop"
  $ProgressPreference = "SilentlyContinue"
  try {
    [Net.ServicePointManager]::SecurityProtocol = [Net.ServicePointManager]::SecurityProtocol -bor [Net.SecurityProtocolType]::Tls12
  } catch {
  }

  $repo = if ($env:FINN_SKILL_REPO) { $env:FINN_SKILL_REPO } else { "microsoft/finnts" }
  $ref = if ($env:FINN_SKILL_REF) { $env:FINN_SKILL_REF } else { "main" }
  $agent = if ($env:FINN_SKILL_AGENT) { $env:FINN_SKILL_AGENT.ToLower() } else { "copilot" }
  $folders = @{ copilot = ".copilot"; claude = ".claude"; agents = ".agents" }

  if ($env:FINN_SKILL_DEST) {
    $dest = $env:FINN_SKILL_DEST
  } elseif ($folders.ContainsKey($agent)) {
    $dest = Join-Path (Join-Path (Join-Path $HOME $folders[$agent]) "skills") "finn"
  } else {
    Write-Host "Unknown FINN_SKILL_AGENT '$agent'. Use copilot, claude, or agents." -ForegroundColor Red
    return
  }

  if (Test-Path -LiteralPath $dest) {
    Write-Host "The Finn skill is already installed at $dest." -ForegroundColor Yellow
    Write-Host "To update it, ask your agent: 'Update the Finn skill.'"
    Write-Host "(It runs scripts/update_skill.R --action=update, which keeps a backup you can roll back to.)"
    return
  }

  $headers = @{ "User-Agent" = "finn-skill"; "Accept" = "application/vnd.github+json" }
  Write-Host "Finding the Finn skill files in $repo ($ref)..."
  try {
    $tree = Invoke-RestMethod -UseBasicParsing -Headers $headers -Uri "https://api.github.com/repos/$repo/git/trees/$([uri]::EscapeDataString($ref))?recursive=1"
  } catch {
    Write-Host "Could not reach GitHub: $($_.Exception.Message)" -ForegroundColor Red
    Write-Host "Check your internet connection or proxy, or install manually (see skills/finn/README.md)."
    return
  }
  if ($tree.truncated) {
    Write-Host "GitHub returned a partial file list. Install manually (see skills/finn/README.md)." -ForegroundColor Red
    return
  }
  $prefix = "skills/finn/"
  $files = @($tree.tree | Where-Object { $_.type -eq "blob" -and $_.path.StartsWith($prefix) } | ForEach-Object { $_.path.Substring($prefix.Length) })
  if ($files.Count -eq 0 -or -not ($files -contains "SKILL.md")) {
    Write-Host "No Finn skill found in $repo at $ref." -ForegroundColor Red
    return
  }

  $stage = Join-Path ([IO.Path]::GetTempPath()) ("finn-skill-" + [guid]::NewGuid().ToString("N"))
  $i = 0
  foreach ($f in $files) {
    $i++
    Write-Host ("  [{0}/{1}] {2}" -f $i, $files.Count, $f)
    $target = Join-Path $stage ($f -replace "/", [IO.Path]::DirectorySeparatorChar)
    New-Item -ItemType Directory -Force -Path (Split-Path $target -Parent) | Out-Null
    $url = "https://raw.githubusercontent.com/$repo/$ref/$prefix$f"
    try {
      Invoke-WebRequest -UseBasicParsing -Headers @{ "User-Agent" = "finn-skill" } -Uri $url -OutFile $target
    } catch {
      Write-Host "Download failed for $f : $($_.Exception.Message)" -ForegroundColor Red
      Write-Host "Nothing was installed. Try again later."
      return
    }
  }

  foreach ($required in @("SKILL.md", "scripts/finn_common.R", "scripts/update_skill.R")) {
    if (-not (Test-Path -LiteralPath (Join-Path $stage $required))) {
      Write-Host "The download is incomplete ($required is missing). Nothing was installed." -ForegroundColor Red
      return
    }
  }

  New-Item -ItemType Directory -Force -Path (Split-Path $dest -Parent) | Out-Null
  Copy-Item -Recurse -LiteralPath $stage -Destination $dest
  if (-not (Test-Path -LiteralPath (Join-Path $dest "SKILL.md"))) {
    Write-Host "Copying into $dest failed. Nothing was installed." -ForegroundColor Red
    return
  }

  Write-Host ""
  Write-Host "Installed the Finn skill at $dest" -ForegroundColor Green
  Write-Host "Next steps:"
  Write-Host "  1. Start a new chat (or reload your agent) so it finds the skill."
  Write-Host "  2. Say: 'Use the Finn skill to give me the Finn tutorial.'"
  Write-Host "     or:  'Use the Finn skill to forecast <your file>.'"
  Write-Host "The agent checks your setup and asks before installing R or finnts."
}
