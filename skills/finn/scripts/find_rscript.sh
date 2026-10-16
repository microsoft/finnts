#!/usr/bin/env sh
# Prints the full path to Rscript, or exits 1 with guidance.
if command -v Rscript >/dev/null 2>&1; then
  command -v Rscript
  exit 0
fi
# Common install locations: Homebrew, CRAN macOS framework, distro packages,
# Posit/rig versioned installs (/opt/R/<version>), and user-local installs.
for c in /usr/local/bin/Rscript /opt/homebrew/bin/Rscript /usr/bin/Rscript \
  /Library/Frameworks/R.framework/Resources/bin/Rscript \
  "$HOME/.local/bin/Rscript" /opt/R/*/bin/Rscript; do
  if [ -x "$c" ]; then
    echo "$c"
    exit 0
  fi
done
echo "R was not found. Ask the user before installing R (see references/setup.md)." >&2
exit 1