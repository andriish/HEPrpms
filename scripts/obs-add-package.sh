#!/bin/bash
# Add (or update) a package in an openSUSE Build Service (OBS) project.
#
# OBS cannot run a custom source script as COPR does, so the package is built locally into a source RPM with the
# unchanged srpmsbuild.sh (it downloads the sources, checks their checksums, runs do.sh if present and produces the
# SRPM) and the SRPM is imported with "osc importsrcpkg"; the spec file, patches and sources of the SRPM are
# committed to the OBS package, which triggers the build.
#
# Usage: scripts/obs-add-package.sh [options] <package> [<version>]
#   <version>            version directory to import; the newest directory under <package>/ is used by default
#   --project NAME       OBS project (default: home:averbyts:HEPrpmsSUSE)
#   --reuse-srpm         use the SRPM already present in <package>/<version>/rpmbuild/SRPMS instead of rebuilding it
#   --dry-run            only print what would be done
#   -h, --help           show this help
#
# Examples:
#   scripts/obs-add-package.sh OpenPHIGS
#   scripts/obs-add-package.sh --reuse-srpm fastjet 3.5.3
#
# Requires osc with valid credentials in ~/.config/osc/oscrc and the project to exist (see scripts/obs-sync.sh).

set -euo pipefail

PROJECT=home:averbyts:HEPrpmsSUSE
REUSE=0
DRYRUN=0
PACKAGE=
VERSION=

usage() { sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'; }
die() { echo "ERROR: $*" >&2; exit 1; }

while [ $# -gt 0 ]; do
  case "$1" in
    --project)    [ $# -ge 2 ] || die "--project needs a value"; PROJECT=$2; shift 2 ;;
    --reuse-srpm) REUSE=1; shift ;;
    --dry-run)    DRYRUN=1; shift ;;
    -h|--help)    usage; exit 0 ;;
    -*)           die "unknown option $1" ;;
    *)            if [ -z "$PACKAGE" ]; then PACKAGE=$1
                  elif [ -z "$VERSION" ]; then VERSION=$1
                  else die "too many arguments"; fi
                  shift ;;
  esac
done
[ -n "$PACKAGE" ] || { usage; exit 1; }
command -v osc >/dev/null || die "osc is not installed"

TOPDIR=$(cd "$(dirname "$0")/.." && pwd)
[ -d "$TOPDIR/$PACKAGE" ] || die "no directory $PACKAGE in $TOPDIR"
if [ -z "$VERSION" ]; then
  VERSION=$(ls -1 "$TOPDIR/$PACKAGE" | grep -vx rpmbuild | sort -V | tail -1)
  [ -n "$VERSION" ] || die "no version directory found in $PACKAGE"
  echo "Using the newest version directory: $VERSION"
fi
[ -f "$TOPDIR/$PACKAGE/$VERSION/$PACKAGE.spec" ] || die "missing $TOPDIR/$PACKAGE/$VERSION/$PACKAGE.spec"

run() {
  if [ "$DRYRUN" = 1 ]; then printf '+'; printf ' %q' "$@"; echo; else "$@"; fi
}

SRPMDIR="$TOPDIR/$PACKAGE/$VERSION/rpmbuild/SRPMS"
if [ "$REUSE" = 1 ] && ls "$SRPMDIR"/*.src.rpm >/dev/null 2>&1; then
  echo "== $PACKAGE $VERSION: reusing the existing SRPM"
else
  echo "== $PACKAGE $VERSION: building the SRPM"
  mkdir -p "$TOPDIR/logs"
  (cd "$TOPDIR" && run sh srpmsbuild.sh "$PACKAGE" "$VERSION" > "$TOPDIR/logs/obs-$PACKAGE.log" 2>&1) ||
    die "srpmsbuild.sh failed, see logs/obs-$PACKAGE.log"
fi
SRPM=$(ls -1 "$SRPMDIR"/*.src.rpm 2>/dev/null | head -1 || true)
if [ -z "$SRPM" ]; then
  [ "$DRYRUN" = 1 ] && SRPM="$SRPMDIR/$PACKAGE.src.rpm" || die "no SRPM in $SRPMDIR, see logs/obs-$PACKAGE.log"
fi

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
echo "== $PROJECT: importing $PACKAGE $VERSION"
if [ "$DRYRUN" = 1 ]; then
  echo "+ (in a temporary checkout of $PROJECT)"
  echo "+ osc importsrcpkg -c --delete-old-files $SRPM"
  exit 0
fi
cd "$WORK"
# Check out the package if it exists, so that the import updates it (and removes obsolete files) instead of failing.
if osc ls "$PROJECT" "$PACKAGE" >/dev/null 2>&1; then
  osc co "$PROJECT" "$PACKAGE" >/dev/null
else
  osc co "$PROJECT" >/dev/null
fi
cd "$PROJECT"
osc importsrcpkg -c --delete-old-files "$SRPM"
