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

command -v copr-cli >/dev/null || die "copr-cli is not installed"
command -v jq >/dev/null || die "jq is not installed"
cd "$(dirname "$0")/.."

in_list() { local x=$1; shift; local y; for y in "$@"; do [ "$x" = "$y" ] && return 0; done; return 1; }
# pre-release tags (3.5.1pre12, 3.0.01-beta01) are older than the release: sort -V puts '~' first
norm() { sed -E 's/([0-9])-?(pre|alpha|beta|rc)/\1~\2/' <<< "$1"; }
is_newer() {  # is_newer A B: A is a newer version than B
  [ "$1" != "$2" ] && [ "$(printf '%s\n%s\n' "$(norm "$1")" "$(norm "$2")" | sort -V | tail -1)" = "$(norm "$1")" ]
}

# package/version pairs built in COPR
declare -A NEWEST   # package -> newest version built in COPR
declare -A USED     # package/version -> 1
IFS=',' read -r -a PROJ <<< "$PROJECTS"
for proj in "${PROJ[@]}"; do
  json=$(copr-cli list-packages "$OWNER/$proj" --output-format json) || die "cannot list packages of $OWNER/$proj"
  n=0
  while read -r pkg ver; do
    [ -n "$pkg" ] || continue
    n=$((n + 1))
    USED["$pkg/$ver"]=1
    if [ -z "${NEWEST[$pkg]:-}" ] || is_newer "$ver" "${NEWEST[$pkg]}"; then NEWEST[$pkg]=$ver; fi
  done < <(jq -r '.[].source_dict.script // empty' <<< "$json" |
           sed -nE 's/.*srpmsbuild\.sh[[:space:]]+([^[:space:]]+)[[:space:]]+([^[:space:]]+).*/\1 \2/p')
  [ "$n" -gt 0 ] || die "found no packages in $OWNER/$proj, refusing to continue"
  echo "$OWNER/$proj: $n packages" >&2
done

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
