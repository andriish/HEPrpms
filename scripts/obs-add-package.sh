#!/bin/bash
# Add (or update) a package in an openSUSE Build Service (OBS) project.
#
# Nothing is built or uploaded from this machine. The package gets a _service file and a <package>-rpmlintrc file,
# which exist only in OBS and are generated here. The _service file makes OBS
#   1. clone https://github.com/andriish/HEPrpms.git and extract the files of <package>/<version>/ (the spec file and
#      the patches; the files are taken from the git tree, so they have to be committed and pushed), and
#   2. download the sources listed in the Source lines of the spec file (download_files, or download_url with an
#      explicit file name for sources with a query string in the URL, e.g. HepForge downloader?f=...).
# Sources that are files and not tracked by git (qgraf-3.6.7.tgz, made by do.sh) are uploaded to OBS as they are.
# OBS then builds the package. The rpmlintrc file makes the OBS checks ignore the rpmlint errors listed in
# RPMLINT_FILTERS below. The repository itself needs no OBS-specific files.
#
# Usage: scripts/obs-add-package.sh [options] <package> [<version>]
#   <version>            version directory to import; the newest directory under <package>/ is used by default
#   --project NAME       OBS project (default: home:averbyts:HEPrpmsSUSE)
#   --revision REV       git branch, tag or commit OBS clones (default: master)
#   --force              also touch a package whose last OBS build succeeded and whose version is unchanged
#                        (by default such packages are left alone, so that they are not rebuilt)
#   --dry-run            only print the generated _service file
#   -h, --help           show this help
#
# Examples:
#   scripts/obs-add-package.sh OpenPHIGS
#   scripts/obs-add-package.sh --project home:averbyts:HEPrpmsSUSE fastjet 3.5.3
#
# Requires osc with valid credentials in ~/.config/osc/oscrc and the project to exist (see scripts/obs-sync.sh).

set -euo pipefail

PROJECT=home:averbyts:HEPrpmsSUSE
GIT_URL=https://github.com/andriish/HEPrpms.git
REVISION=master
# rpmlint messages that OBS must ignore (they fail or spoil a build although the packages are fine for us).
RPMLINT_FILTERS=(
  devel-file-in-non-devel-package   # static libraries and headers in the main package, as in the Fedora/EPEL layout
  lto-no-text-in-archive            # static libraries built with LTO but without fat objects
  static-library-without-debuginfo  # static libraries without debug info
  explicit-lib-dependency           # explicit library Requires: kept as in the Fedora/EPEL spec files
  arch-dependent-file-in-usr-share  # binaries in the documentation directories
  backup-file-in-package            # upstream sources contain *.orig files
  env-script-interpreter            # upstream scripts use #!/usr/bin/env
)
DRYRUN=0
FORCE=0
PACKAGE=
VERSION=

usage() { sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'; }
die() { echo "ERROR: $*" >&2; exit 1; }

while [ $# -gt 0 ]; do
  case "$1" in
    --project)  [ $# -ge 2 ] || die "--project needs a value"; PROJECT=$2; shift 2 ;;
    --revision) [ $# -ge 2 ] || die "--revision needs a value"; REVISION=$2; shift 2 ;;
    --force)    FORCE=1; shift ;;
    --dry-run)  DRYRUN=1; shift ;;
    -h|--help)  usage; exit 0 ;;
    -*)         die "unknown option $1" ;;
    *)          if [ -z "$PACKAGE" ]; then PACKAGE=$1
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
SPEC="$PACKAGE/$VERSION/$PACKAGE.spec"
git -C "$TOPDIR" ls-files --error-unmatch "$SPEC" >/dev/null 2>&1 ||
  die "$SPEC is not tracked by git; commit and push it, OBS clones $GIT_URL"

# The files of the version directory itself (no ORIG/PATCHED trees, no helper scripts).
FILES=$(git -C "$TOPDIR" ls-files "$PACKAGE/$VERSION" | sed "s|^$PACKAGE/$VERSION/||" |
        grep -v '/' | grep -vE '^(do\.sh|_service)$' || true)

# Sources whose URL has a query string (e.g. https://yoda.hepforge.org/downloader?f=YODA-2.1.4.tar.gz) would be saved
# by download_files under a name like "downloader?f=YODA-2.1.4.tar.gz", which the spec file does not expect. If the
# spec has such a source, every URL source is fetched by download_url with an explicit file name instead (the URLs
# are taken from the spec file with rpmspec, which only parses it).
SRC_URLS=$(rpmspec -P "$TOPDIR/$SPEC" 2>/dev/null | awk '/^Source[0-9]*:/{print $2}' | grep -E '^(https?|ftp)://' || true)
download_services() {
  if ! grep -q '?' <<< "$SRC_URLS"; then echo '  <service name="download_files"/>'; return; fi
  local u proto rest host path name
  for u in $SRC_URLS; do
    case "$u" in *'#/'*) name=${u##*'#/'}; u=${u%%'#/'*} ;; *'?'*) name=${u##*[=/]} ;; *) name=${u##*/} ;; esac
    proto=${u%%://*}; rest=${u#*://}; host=${rest%%/*}; path=/${rest#*/}
    echo '  <service name="download_url">'
    echo "    <param name=\"protocol\">$proto</param>"
    echo "    <param name=\"host\">$host</param>"
    echo "    <param name=\"path\">$path</param>"
    echo "    <param name=\"filename\">$name</param>"
    echo '  </service>'
  done
}

SERVICE=$(
  echo '<services>'
  echo '  <service name="obs_scm">'
  echo '    <param name="scm">git</param>'
  echo "    <param name=\"url\">$GIT_URL</param>"
  echo "    <param name=\"revision\">$REVISION</param>"
  echo "    <param name=\"subdir\">$PACKAGE/$VERSION</param>"
  for f in $FILES; do echo "    <param name=\"extract\">$f</param>"; done
  echo '  </service>'
  download_services
  echo '</services>'
)

RPMLINTRC=$(
  echo "# generated by scripts/obs-add-package.sh"
  for f in "${RPMLINT_FILTERS[@]}"; do
    case "$f" in \#*) continue ;; esac
    echo "addFilter(\"$f\")"
  done
)
# (the comments after the names in RPMLINT_FILTERS are bash comments, not array elements)

if [ "$DRYRUN" = 1 ]; then echo "== $PROJECT/$PACKAGE ($VERSION)"; echo "$SERVICE"; echo "-- $PACKAGE-rpmlintrc"; echo "$RPMLINTRC"; exit 0; fi

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
cd "$WORK"

if ! osc ls "$PROJECT" "$PACKAGE" >/dev/null 2>&1; then
  echo "== $PROJECT: creating $PACKAGE"
  cat > meta.xml <<EOM
<package name="$PACKAGE" project="$PROJECT">
  <title>$PACKAGE</title>
  <description>$(sed -n 's/^Summary: *//p' "$TOPDIR/$SPEC" | head -1 | sed 's/&/\&amp;/g; s/</\&lt;/g') (packaging from https://github.com/andriish/HEPrpms)</description>
</package>
EOM
  osc meta pkg "$PROJECT" "$PACKAGE" -F meta.xml >/dev/null
fi

osc co "$PROJECT" "$PACKAGE" >/dev/null
cd "$PROJECT/$PACKAGE"
echo "$SERVICE" > _service.new
echo "$RPMLINTRC" > "$PACKAGE-rpmlintrc.new"
changed=0
# A package that was built successfully is not resubmitted: only a changed version (_service) updates it.
if [ "$FORCE" = 0 ] && [ -f _service ] && cmp -s _service _service.new &&
   osc results "$PROJECT" "$PACKAGE" 2>/dev/null | grep -qE 'openSUSE_Tumbleweed +x86_64 .* succeeded'; then
  echo "== $PROJECT: $PACKAGE $VERSION was built successfully, left alone"
  exit 0
fi
if [ ! -f _service ] || ! cmp -s _service _service.new; then mv _service.new _service; changed=1; else rm _service.new; fi
if [ ! -f "$PACKAGE-rpmlintrc" ] || ! cmp -s "$PACKAGE-rpmlintrc" "$PACKAGE-rpmlintrc.new"; then
  mv "$PACKAGE-rpmlintrc.new" "$PACKAGE-rpmlintrc"; changed=1
else rm "$PACKAGE-rpmlintrc.new"; fi
# Sources that are files, not URLs, and are not tracked by git (e.g. qgraf-3.6.7.tgz, which do.sh makes from the
# committed .f08 file) are uploaded to the OBS package as they are; do.sh is run if the file does not exist yet.
LOCAL_SRC=$(rpmspec -P "$TOPDIR/$SPEC" 2>/dev/null | awk '/^Source[0-9]*:/{print $2}' | grep -vE '^(https?|ftp)://' || true)
for f in $LOCAL_SRC; do
  f=${f##*/}
  git -C "$TOPDIR" ls-files --error-unmatch "$PACKAGE/$VERSION/$f" >/dev/null 2>&1 && continue
  if [ ! -f "$TOPDIR/$PACKAGE/$VERSION/$f" ] && [ -f "$TOPDIR/$PACKAGE/$VERSION/do.sh" ]; then
    (cd "$TOPDIR/$PACKAGE/$VERSION" && sh do.sh >/dev/null 2>&1) || true
  fi
  [ -f "$TOPDIR/$PACKAGE/$VERSION/$f" ] || die "source $f is neither tracked by git nor made by do.sh"
  if [ ! -f "$f" ] || ! cmp -s "$f" "$TOPDIR/$PACKAGE/$VERSION/$f"; then
    isnew=0; [ -f "$f" ] || isnew=1
    cp "$TOPDIR/$PACKAGE/$VERSION/$f" "$f"
    [ "$isnew" = 1 ] && osc add "$f" >/dev/null 2>&1
    changed=1
  fi
done
if [ "$changed" = 0 ]; then
  echo "== $PROJECT: $PACKAGE $VERSION is up to date, refreshing the sources from git"
  osc service remoterun "$PROJECT" "$PACKAGE" >/dev/null
else
  osc add _service "$PACKAGE-rpmlintrc" >/dev/null 2>&1 || true
  echo "== $PROJECT: updating $PACKAGE to $VERSION"
  osc commit -m "$PACKAGE $VERSION from $GIT_URL" >/dev/null
fi
