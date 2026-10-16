#!/bin/sh
# One-line installer for the Finn skill (macOS and Linux).
#
#   curl -fsSL https://raw.githubusercontent.com/microsoft/finnts/main/skills/finn/install_skill.sh | sh
#
# Downloads only the skills/finn folder from GitHub and copies it into your agent's
# personal skills folder. It does not install R or finnts; the skill does that later,
# after asking you. It never deletes or overwrites anything: if the skill is already
# installed it stops and tells you how to update.
#
# Optional environment variables:
#   FINN_SKILL_AGENT  copilot (default), claude, or agents
#   FINN_SKILL_DEST   exact folder to install into (overrides FINN_SKILL_AGENT)
#   FINN_SKILL_REPO   GitHub owner/repo (default microsoft/finnts)
#   FINN_SKILL_REF    branch, tag, or commit (default main)

set -u

repo="${FINN_SKILL_REPO:-microsoft/finnts}"
ref="${FINN_SKILL_REF:-main}"
agent="${FINN_SKILL_AGENT:-copilot}"

if [ -n "${FINN_SKILL_DEST:-}" ]; then
  dest="$FINN_SKILL_DEST"
else
  case "$agent" in
    copilot) dest="$HOME/.copilot/skills/finn" ;;
    claude) dest="$HOME/.claude/skills/finn" ;;
    agents) dest="$HOME/.agents/skills/finn" ;;
    *)
      echo "Unknown FINN_SKILL_AGENT '$agent'. Use copilot, claude, or agents." >&2
      exit 1
      ;;
  esac
fi

if [ -e "$dest" ]; then
  echo "The Finn skill is already installed at $dest."
  echo "To update it, ask your agent: 'Update the Finn skill.'"
  echo "(It runs scripts/update_skill.R --action=update, which keeps a backup you can roll back to.)"
  exit 0
fi

if command -v curl >/dev/null 2>&1; then
  fetch() { curl -fsSL -H "User-Agent: finn-skill" "$1" -o "$2"; }
elif command -v wget >/dev/null 2>&1; then
  fetch() { wget -q --header="User-Agent: finn-skill" "$1" -O "$2"; }
else
  echo "Neither curl nor wget is available. Install one, or install manually (see skills/finn/README.md)." >&2
  exit 1
fi

stage="$(mktemp -d 2>/dev/null || mktemp -d -t finn-skill)"
tree="$stage.tree.json"

echo "Finding the Finn skill files in $repo ($ref)..."
if ! fetch "https://api.github.com/repos/$repo/git/trees/$ref?recursive=1" "$tree"; then
  echo "Could not reach GitHub. Check your internet connection or proxy, or install manually (see skills/finn/README.md)." >&2
  exit 1
fi

if grep -q '"truncated": *true' "$tree"; then
  echo "GitHub returned a partial file list. Install manually (see skills/finn/README.md)." >&2
  exit 1
fi

# Put each tree entry on its own line (works for pretty or compact JSON), keep
# blobs, and strip the skills/finn/ prefix. Skill file names contain no spaces.
files="$(tr -d '\n\r' < "$tree" | tr '{' '\n' | grep '"type": *"blob"' |
  sed -n 's/.*"path": *"skills\/finn\/\([^"]*\)".*/\1/p')"

case "$files" in
  *SKILL.md*) ;;
  *)
    echo "No Finn skill found in $repo at $ref." >&2
    exit 1
    ;;
esac

total="$(printf '%s\n' "$files" | wc -l | tr -d ' ')"
i=0
skill="$stage/finn"
for f in $files; do
  i=$((i + 1))
  echo "  [$i/$total] $f"
  mkdir -p "$skill/$(dirname "$f")"
  if ! fetch "https://raw.githubusercontent.com/$repo/$ref/skills/finn/$f" "$skill/$f"; then
    echo "Download failed for $f. Nothing was installed. Try again later." >&2
    exit 1
  fi
done

for required in SKILL.md scripts/finn_common.R scripts/update_skill.R; do
  if [ ! -f "$skill/$required" ]; then
    echo "The download is incomplete ($required is missing). Nothing was installed." >&2
    exit 1
  fi
done

mkdir -p "$(dirname "$dest")"
if ! cp -R "$skill" "$dest" || [ ! -f "$dest/SKILL.md" ]; then
  echo "Copying into $dest failed. Nothing was installed." >&2
  exit 1
fi

echo ""
echo "Installed the Finn skill at $dest"
echo "Next steps:"
echo "  1. Start a new chat (or reload your agent) so it finds the skill."
echo "  2. Say: 'Use the Finn skill to give me the Finn tutorial.'"
echo "     or:  'Use the Finn skill to forecast <your file>.'"
echo "The agent checks your setup and asks before installing R or finnts."
