#!/bin/bash
# Create an openSUSE Build Service (OBS) project and fill it with the packages of a COPR project.
#
# By default the OBS project home:averbyts:HEPrpmsSUSE mirrors averbyts/HEPrpmsSUSE on COPR: the same packages, in
# the versions that COPR builds, for openSUSE Tumbleweed (x86_64). The list is read from the COPR custom-source
# scripts, as scripts/git-rm-unused.sh does. Every package is set up with scripts/obs-add-package.sh, which only
# creates a _service file in OBS (git clone of this repository + download of the sources); nothing is built or
# uploaded from this machine, the packages have to be committed and pushed to GitHub first.
#
# Usage: scripts/obs-sync.sh [options] [<package> ...]
#   <package> ...        only these packages (default: all packages of the COPR project)
#   --project NAME       OBS project (default: home:averbyts:HEPrpmsSUSE)
#   --copr OWNER/PROJ    COPR project to mirror (default: averbyts/HEPrpmsSUSE)
#   --jobs N             set up N packages in parallel (default: 4)
#   --skip-existing      do not touch packages that already exist in the OBS project
#   --dry-run            only print what would be done
#   -h, --help           show this help
#
# The project is created if it does not exist; its repository is openSUSE_Tumbleweed (x86_64), based on
# the science project and openSUSE:Tumbleweed/standard. Failed packages are listed at the end and do not stop the run.
#
# Requires osc, copr-cli and jq.

set -uo pipefail

PROJECT=home:averbyts:HEPrpmsSUSE
COPR=averbyts/HEPrpmsSUSE
JOBS=4
SKIP=0
DRYRUN=0
ONLY=()

usage() { sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'; }
die() { echo "ERROR: $*" >&2; exit 1; }

while [ $# -gt 0 ]; do
  case "$1" in
    --project)       [ $# -ge 2 ] || die "--project needs a value"; PROJECT=$2; shift 2 ;;
    --copr)          [ $# -ge 2 ] || die "--copr needs a value"; COPR=$2; shift 2 ;;
    --jobs)          [ $# -ge 2 ] || die "--jobs needs a value"; JOBS=$2; shift 2 ;;
    --skip-existing) SKIP=1; shift ;;
    --dry-run)       DRYRUN=1; shift ;;
    -h|--help)       usage; exit 0 ;;
    -*)              die "unknown option $1" ;;
    *)               ONLY+=("$1"); shift ;;
  esac
done

HERE=$(cd "$(dirname "$0")" && pwd)
TOPDIR=$(dirname "$HERE")
# shellcheck source=lib-copr.sh
. "$HERE/lib-copr.sh"
command -v osc >/dev/null || die "osc is not installed"

copr_used_versions "${COPR%%/*}" "${COPR#*/}" || exit 1

# Project with the Tumbleweed repository.
if ! osc meta prj "$PROJECT" >/dev/null 2>&1; then
  META=$(mktemp)
  cat > "$META" <<EOM
<project name="$PROJECT">
  <title>${PROJECT##*:}</title>
  <description>Clone of https://copr.fedorainfracloud.org/coprs/$COPR/ for Tumbleweed. HEP software packaged from https://github.com/andriish/HEPrpms</description>
  <person userid="${PROJECT#home:}" role="maintainer"/>
  <repository name="openSUSE_Tumbleweed">
    <path project="science" repository="openSUSE_Tumbleweed"/>
    <path project="openSUSE:Tumbleweed" repository="standard"/>
    <arch>x86_64</arch>
  </repository>
</project>
EOM
  sed -i "s|<person userid=\"[^\"]*\"|<person userid=\"$(sed -n 's/^user *= *//p' ~/.config/osc/oscrc | head -1)\"|" "$META"
  if [ "$DRYRUN" = 1 ]; then echo "+ osc meta prj $PROJECT -F <meta>"; cat "$META"
  else osc meta prj "$PROJECT" -F "$META" || die "cannot create $PROJECT"; fi
  rm -f "$META"
fi

LIST=()
for key in "${!USED[@]}"; do
  pkg=${key%%/*}; ver=${key#*/}
  if [ ${#ONLY[@]} -gt 0 ] && ! in_list "$pkg" "${ONLY[@]}"; then continue; fi
  if [ "$SKIP" = 1 ] && osc ls "$PROJECT" "$pkg" >/dev/null 2>&1; then echo "skip $pkg (exists)"; continue; fi
  [ -f "$TOPDIR/$pkg/$ver/$pkg.spec" ] || { echo "WARNING: no $pkg/$ver in the repository, skipped" >&2; continue; }
  LIST+=("$pkg $ver")
done
IFS=$'\n' LIST=($(sort <<< "${LIST[*]}")); unset IFS
echo "${#LIST[@]} packages to import into $PROJECT"

OPTS=(--project "$PROJECT")
[ "$DRYRUN" = 1 ] && OPTS+=(--dry-run)
mkdir -p "$TOPDIR/logs"
STATUS=$(mktemp)
export STATUS_FILE=$STATUS ADD="$HERE/obs-add-package.sh" LOGDIR="$TOPDIR/logs"
export OPTS_STR="$(printf '%q ' "${OPTS[@]}")"
if [ ${#LIST[@]} -gt 0 ]; then
  printf '%s\n' "${LIST[@]}" | xargs -P "$JOBS" -L1 bash -c '
    eval "set -- $OPTS_STR \"\$0\" \"\$1\""
    if "$ADD" "$@" > "$LOGDIR/obs-add-${@: -2:1}.log" 2>&1
    then echo "OK ${@: -2:1}" >> "$STATUS_FILE"; else echo "FAILED ${@: -2:1}" >> "$STATUS_FILE"; fi'
fi
echo "imported: $(grep -c '^OK' "$STATUS" || true)"
if grep -q '^FAILED' "$STATUS"; then
  echo "failed (see logs/obs-add-<package>.log):"; grep '^FAILED' "$STATUS" | sed 's/^FAILED /  /'
fi
rm -f "$STATUS"
