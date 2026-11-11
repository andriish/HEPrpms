#!/bin/bash
# List the packages of this repository for which an upstream version newer than the one built in the HEPrpms or
# HEPrpmsSUSE COPR projects exists.
#
# For every package directory the upstream location is derived from the Source0 URL of the spec file:
#   - github.com, gitlab.* and bitbucket.org archive URLs: the tags of the git repository (git ls-remote)
#   - HepForge "downloader?f=" URLs and other URLs: the file listing of the download directory, matched against
#     the Source0 file name with the version replaced by a wildcard
# Packages whose upstream cannot be checked this way have an entry in OVERRIDE below (another location, or a reason
# why they are skipped). Some sites are behind the Anubis bot protection; they are reported as blocked.
# Sites that do not answer within 60 seconds are unavailable: they are not checked and are listed as skipped. Pre-release versions (alpha, beta, rc, pre, ...) are ignored unless the current one is.
# The COPR version of a package is the newest one found in the COPR projects (see lib-copr.sh).
#
# Usage: scripts/check-new-versions.sh [options]
#   --package NAME     check only package NAME; can be repeated
#   --jobs N           number of parallel checks (default: 8)
#   --owner NAME       COPR owner (default: averbyts)
#   --projects LIST    comma-separated COPR projects (default: HEPrpms,HEPrpmsSUSE)
#   -v, --verbose      also list the packages that are up to date
#   -h, --help         show this help
#
# Output: a table of the packages with a newer upstream version on stdout; the packages that could not be checked
# are listed on stderr. Needs copr-cli, jq, git, curl, spectool.

set -uo pipefail

OWNER=averbyts
PROJECTS=HEPrpms,HEPrpmsSUSE
JOBS=8
TIMEOUT=60   # seconds, for every network request
VERBOSE=0
ONLY=()
PRERELEASE_RE='(alpha|beta|rc|pre|dev|snap|nightly|[0-9][abc])[0-9]*$'

# Upstream location of packages that cannot be derived from Source0. Values:
#   git <repository URL> [<regex the version has to match>]
#   page <URL of a file listing> <file name with @V@ in place of the version>
#   skip <reason>
declare -A OVERRIDE=(
  [HEPrpms]="skip meta package"
  [lhapdf-sets-Herwig]="skip PDF set data, no upstream version"
  [lhapdf-sets-whizard]="skip PDF set data, no upstream version"
  [COCOA]="skip built from a git commit, no releases"
  [gosam-contrib]="skip date-based version, no releases"
  [qqvvamp]="skip built from the master branch, no releases"
  [f2c]="skip date-based version (netlib)"
  [topdrawer]="skip unversioned source"
  [ugs]="skip unversioned source"
  [qgraf]="skip source is not downloadable"
  [pythia6]="skip frozen upstream"
  [xbae]="skip site blocks scripted access, unmaintained"
  [qcdnum]="skip site blocks scripted access"
  [MCFM]="skip site blocks scripted access"
  [cernlib]="skip site has no machine-readable release list"
  [ginac]="page https://www.ginac.de/Download.html ginac-@V@.tar.bz2"
  [JetVHeto]="page https://jetvheto.hepforge.org/downloads/ JetVHeto-@V@.tgz"
  [njet]="git https://bitbucket.org/njet/njet.git"
  [nlox]="skip tags are not numeric (1.2.2.atlas7)"
  [ariadne]="skip no machine-readable file listing"
  [FeynHiggs]="skip no machine-readable file listing"
  [geant4]="git https://gitlab.cern.ch/geant4/geant4.git"
  [PHOTOS]="git https://gitlab.cern.ch/photospp/photospp.git"
  [TAUOLA]="git https://gitlab.cern.ch/tauolapp/tauolapp.git"
  [rapgap]="git https://gitlab.cern.ch/jung/rapgap.git ^[0-9]+\\.[0-9]{1,2}\\.[0-9]+"
  [python-uproot4]="git https://github.com/scikit-hep/uproot5.git ^4\\."
  [EvtGen]="page https://evtgen.hepforge.org/downloads/ EvtGen-@V@.tar.gz"
  [fastnlo]="page https://fastnlo.hepforge.org/code/v25/ fastnlo_toolkit-@V@.tar.gz"
  [MG5_aMC]="page https://launchpad.net/mg5amcnlo/+download MG5_aMC_v@V@.tar.gz"
)

usage() { sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'; }
die() { echo "ERROR: $*" >&2; exit 1; }

while [ $# -gt 0 ]; do
  case "$1" in
    --package)  [ $# -ge 2 ] || die "--package needs a value"; ONLY+=("$2"); shift 2 ;;
    --jobs)     [ $# -ge 2 ] || die "--jobs needs a value"; JOBS=$2; shift 2 ;;
    --owner)    [ $# -ge 2 ] || die "--owner needs a value"; OWNER=$2; shift 2 ;;
    --projects) [ $# -ge 2 ] || die "--projects needs a value"; PROJECTS=$2; shift 2 ;;
    -v|--verbose) VERBOSE=1; shift ;;
    -h|--help)  usage; exit 0 ;;
    *)          die "unknown argument $1" ;;
  esac
done
for c in git curl spectool jq; do command -v "$c" >/dev/null || die "$c is not installed"; done

cd "$(dirname "$0")/.."
# shellcheck source=scripts/lib-copr.sh
. scripts/lib-copr.sh
copr_used_versions "$OWNER" "$PROJECTS" || exit 1

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

# newest_of: read versions (one per line) and print the newest one
newest_of() { while read -r v; do printf '%s\t%s\n' "$(norm "$v")" "$v"; done | sort -V -k1,1 | tail -1 | cut -f2; }

# filter_candidates CURRENT: drop pre-releases unless the current version is one, and drop date-like versions
# (first component with 4 or more digits, e.g. 20060912 or 2010.08.27) unless the current version is date-like too
filter_candidates() {
  local out
  out=$(if [[ $1 =~ $PRERELEASE_RE ]]; then cat; else grep -Ev "$PRERELEASE_RE"; fi)
  if [[ $1 =~ ^[0-9]{4} ]]; then echo "$out"; else grep -Ev '^[0-9]{4}' <<< "$out"; fi
}

# git_versions URL [REGEX]: versions found in the tags of a git repository
git_versions() {
  local tags
  tags=$(GIT_TERMINAL_PROMPT=0 timeout "$TIMEOUT" git ls-remote --tags --refs "$1" 2>/dev/null) || return 2
  sed -E 's#^.*refs/tags/##' <<< "$tags" |
    sed -nE 's/^[^0-9]*([0-9].*)$/\1/p' | tr '_-' '..' |
    grep -E '^[0-9]+(\.[0-9]+)*([a-z]+[0-9]*)?$' | grep -E "${2:-.}"
}

# page_versions URL PATTERN: versions of the files in a listing page; PATTERN has @V@ in place of the version.
# Returns 2 if the page cannot be fetched (unavailable or timeout) and 3 if it is a bot-protection (Anubis) challenge page.
page_versions() {
  local prefix=${2%%@V@*} suffix=${2#*@V@} re html
  re="$(sed 's/[.[\*^$+?(){}|]/\\&/g' <<< "$prefix")[0-9][0-9A-Za-z._-]*$(sed 's/[.[\*^$+?(){}|]/\\&/g' <<< "$suffix")"
  html=$(curl -fsSL --connect-timeout "$TIMEOUT" -m "$TIMEOUT" "$1" 2>/dev/null) || return 2
  if grep -qiE "making sure you.?re not a bot|within\.website/x/xess|anubis" <<< "$html"; then return 3; fi
  grep -oE "$re" <<< "$html" | sort -u |
    while read -r m; do m=${m#"$prefix"}; echo "${m%"$suffix"}"; done | tr '_-' '..'
}

# check_one PACKAGE: writes one result line into $TMP/PACKAGE.res
check_one() {
  local pkg=$1 cur spec src base method arg1 arg2 vers up repo_has dirs rc
  local res="$TMP/$pkg.res"
  cur=${NEWEST[$pkg]:-}
  if [ -z "$cur" ]; then echo "SKIP|$pkg||not built in COPR" > "$res"; return; fi
  spec="$pkg/$cur/$pkg.spec"
  [ -f "$spec" ] || { echo "FAIL|$pkg|$cur|missing $spec" > "$res"; return; }

  read -r method arg1 arg2 <<< "${OVERRIDE[$pkg]:-}"
  if [ "$method" = skip ]; then echo "SKIP|$pkg|$cur|${OVERRIDE[$pkg]#skip }" > "$res"; return; fi
  if [ -z "$method" ]; then
    src=$(spectool -S "$spec" 2>/dev/null | sed -n 's/^Source0: *//p' | head -1)
    case "$src" in
      https://github.com/*/*/archive/*|https://github.com/*/*/releases/*)
        method=git; arg1=$(sed -E 's#^(https://github.com/[^/]+/[^/]+)/.*#\1.git#' <<< "$src") ;;
      https://*/-/archive/*)
        method=git; arg1=$(sed -E 's#^(https://[^/]+/.+)/-/archive/.*#\1.git#' <<< "$src") ;;
      https://bitbucket.org/*/*/get/*)
        method=git; arg1=$(sed -E 's#^(https://bitbucket.org/[^/]+/[^/]+)/get/.*#\1.git#' <<< "$src") ;;
      https://*.hepforge.org/downloader\?f=*)
        method=page; arg1=$(sed -E 's#^(https://[^/]+)/.*#\1/downloads/#' <<< "$src"); base=${src##*f=} ;;
      http*://*)
        method=page; arg1=$(sed -E 's#/[^/]*$#/#' <<< "$src"); base=${src##*/} ;;
      *) echo "SKIP|$pkg|$cur|no download URL in Source0" > "$res"; return ;;
    esac
    if [ "$method" = page ]; then
      if [[ $base != *"$cur"* ]]; then echo "FAIL|$pkg|$cur|version $cur not in file name $base" > "$res"; return; fi
      arg2=${base/"$cur"/@V@}
    fi
  fi

  case "$method" in
    git)  vers=$(git_versions "$arg1" "$arg2"); rc=$?
          if [ "$rc" = 2 ]; then echo "SKIP|$pkg|$cur|unavailable: $arg1 (no answer within ${TIMEOUT}s)" > "$res"; return; fi ;;
    page) vers=$(page_versions "$arg1" "$arg2"); rc=$?
          case $rc in
            2) echo "SKIP|$pkg|$cur|unavailable: $arg1 (no answer within ${TIMEOUT}s)" > "$res"; return ;;
            3) echo "SKIP|$pkg|$cur|blocked by Anubis bot protection: $arg1" > "$res"; return ;;
          esac ;;
    *)    echo "FAIL|$pkg|$cur|bad override" > "$res"; return ;;
  esac
  vers=$(filter_candidates "$cur" <<< "$vers")
  if [ -z "$vers" ]; then echo "FAIL|$pkg|$cur|no versions found at $arg1" > "$res"; return; fi
  up=$(newest_of <<< "$vers")
  if is_newer "$up" "$cur"; then
    # does the repository already have a directory for it?
    repo_has=no
    for dirs in "$pkg"/*/; do [ "$(norm "$(basename "$dirs")")" = "$(norm "$up")" ] && repo_has=yes; done
    echo "NEWER|$pkg|$cur|$up|$repo_has" > "$res"
  else
    echo "OK|$pkg|$cur|$up" > "$res"
  fi
}

PACKAGES=()
for d in */; do
  d=${d%/}
  [ -n "$(find "$d" -mindepth 2 -maxdepth 2 -name "$d.spec" -print -quit 2>/dev/null)" ] || continue
  if [ "${#ONLY[@]}" -gt 0 ]; then in_list "$d" "${ONLY[@]}" || continue; fi
  PACKAGES+=("$d")
done
[ "${#PACKAGES[@]}" -gt 0 ] || die "no packages to check"

echo "Checking ${#PACKAGES[@]} packages ..." >&2
for pkg in "${PACKAGES[@]}"; do
  while [ "$(jobs -rp | wc -l)" -ge "$JOBS" ]; do wait -n; done
  check_one "$pkg" &
done
wait

n_newer=0
{
  echo "| Package | In COPR | Newest upstream | Already in repository |"
  echo "|---------|---------|-----------------|-----------------------|"
} > "$TMP/table"
for pkg in "${PACKAGES[@]}"; do
  IFS='|' read -r status _ cur up repo_has < "$TMP/$pkg.res"
  case "$status" in
    NEWER) n_newer=$((n_newer + 1)); echo "| $pkg | $cur | $up | $repo_has |" >> "$TMP/table" ;;
    OK)    [ "$VERBOSE" = 1 ] && echo "| $pkg | $cur | $up (up to date) | |" >> "$TMP/table" ;;
  esac
done
if [ "$n_newer" -gt 0 ] || [ "$VERBOSE" = 1 ]; then cat "$TMP/table"; else echo "No package has a newer upstream version."; fi

echo >&2
echo "$n_newer of ${#PACKAGES[@]} packages have a newer upstream version." >&2
echo "Not checked (skipped or unavailable):" >&2
for pkg in "${PACKAGES[@]}"; do
  IFS='|' read -r status _ cur reason < "$TMP/$pkg.res"
  case "$status" in SKIP|FAIL) printf '  %-22s %-6s %s\n' "$pkg" "$status" "$reason" >&2 ;; esac
done
