#!/bin/bash
# Print (or run) the git command that removes package/version directories that are no longer needed.
#
# A <package>/<version> directory is kept if
#   1) that package/version is built in the HEPrpms or HEPrpmsSUSE COPR project, or
#   2) its version is newer than the newest version of the package built in those projects (a pending update), or
#   3) the package is protected with --keep (default: HEPrpms, the meta package).
# Everything else is removed, including packages that are not in COPR at all.
# The COPR package list is read with copr-cli (no login needed): the version of a package is taken from the
# "srpmsbuild.sh <package> <version>" line of its COPR custom-source script.
#
# Usage: scripts/git-rm-unused.sh [options]
#   --run              execute the git rm command (default: only print it)
#   --keep NAME        additionally protect package NAME (all its versions); can be repeated
#   --owner NAME       COPR owner (default: averbyts)
#   --projects LIST    comma-separated COPR projects (default: HEPrpms,HEPrpmsSUSE)
#   -h, --help         show this help
#
# Only tracked files are removed; untracked leftovers (e.g. rpmbuild/ directories) stay.

set -euo pipefail

OWNER=averbyts
PROJECTS=HEPrpms,HEPrpmsSUSE
KEEP_PACKAGES=(HEPrpms)
RUN=0
# Versions that look newer than the COPR version by number, but are older upstream
# (rapgap 3.303 ... 3.310 mean 3.3.03 ... 3.3.10, which precede 3.4.0).
OLDER_OVERRIDE=(rapgap/3.303 rapgap/3.308 rapgap/3.310)

usage() { sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'; }
die() { echo "ERROR: $*" >&2; exit 1; }

while [ $# -gt 0 ]; do
  case "$1" in
    --run)      RUN=1; shift ;;
    --keep)     [ $# -ge 2 ] || die "--keep needs a value"; KEEP_PACKAGES+=("$2"); shift 2 ;;
    --owner)    [ $# -ge 2 ] || die "--owner needs a value"; OWNER=$2; shift 2 ;;
    --projects) [ $# -ge 2 ] || die "--projects needs a value"; PROJECTS=$2; shift 2 ;;
    -h|--help)  usage; exit 0 ;;
    *)          die "unknown argument $1" ;;
  esac
done

cd "$(dirname "$0")/.."
# shellcheck source=scripts/lib-copr.sh
. scripts/lib-copr.sh
copr_used_versions "$OWNER" "$PROJECTS" || exit 1

REMOVE=()
KEPT_NEWER=()
for pdir in */; do
  pkg=${pdir%/}
  [ -n "$(find "$pkg" -mindepth 2 -maxdepth 2 -name "$pkg.spec" -print -quit)" ] || continue
  for vdir in "$pkg"/*/; do
    ver=$(basename "$vdir")
    [ -f "$pkg/$ver/$pkg.spec" ] || continue
    if in_list "$pkg" "${KEEP_PACKAGES[@]}" || [ -n "${USED[$pkg/$ver]:-}" ]; then continue; fi
    if [ -n "${NEWEST[$pkg]:-}" ] && ! in_list "$pkg/$ver" "${OLDER_OVERRIDE[@]}" && is_newer "$ver" "${NEWEST[$pkg]}"; then
      KEPT_NEWER+=("$pkg/$ver (COPR: ${NEWEST[$pkg]})")
      continue
    fi
    # git rm fails on paths without tracked files
    [ -n "$(git ls-files -- "$pkg/$ver" | head -1)" ] && REMOVE+=("$pkg/$ver")
  done
done

echo "Kept (protected): ${KEEP_PACKAGES[*]}" >&2
echo "Kept because newer than the newest version in COPR: ${#KEPT_NEWER[@]}" >&2
printf '  %s\n' "${KEPT_NEWER[@]}" >&2
echo "To remove: ${#REMOVE[@]} directories" >&2
[ "${#REMOVE[@]}" -gt 0 ] || exit 0

if [ "$RUN" = 1 ]; then
  git rm -r -q -- "${REMOVE[@]}"
  echo "Removed ${#REMOVE[@]} directories; review with 'git status' before committing." >&2
else
  echo >&2
  echo "git rm -r -q -- ${REMOVE[*]}"
fi
