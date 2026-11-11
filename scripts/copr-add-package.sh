#!/bin/bash
# Add (or update) a package in the HEPrpms COPR projects.
#
# The package is registered as a COPR "custom" source, in the same way as the existing packages: COPR clones
# https://github.com/andriish/HEPrpms.git and runs srpmsbuild.sh, which downloads the sources, runs do.sh if present
# and produces the SRPM. The package directory therefore has to be pushed to GitHub before COPR can build it.
#
# Usage: scripts/copr-add-package.sh [options] <package> [<version>]
#   <version>            version directory to build; the newest directory under <package>/ is used by default
#   --projects LIST      comma-separated COPR projects (default: HEPrpms,HEPrpmsSUSE)
#   --owner NAME         COPR owner (default: averbyts)
#   --build              start a build after the package has been added
#   --dry-run            only print the copr-cli commands
#   -h, --help           show this help
#
# Examples:
#   scripts/copr-add-package.sh OpenPHIGS
#   scripts/copr-add-package.sh --projects HEPrpms --build ginac 1.8.10
#
# Requires copr-cli with a valid ~/.config/copr token.

set -euo pipefail

OWNER=averbyts
PROJECTS=HEPrpms,HEPrpmsSUSE
GIT_URL=https://github.com/andriish/HEPrpms.git
BUILD=0
DRYRUN=0
PACKAGE=
VERSION=

usage() { sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'; }
die() { echo "ERROR: $*" >&2; exit 1; }

while [ $# -gt 0 ]; do
  case "$1" in
    --projects) [ $# -ge 2 ] || die "--projects needs a value"; PROJECTS=$2; shift 2 ;;
    --owner)    [ $# -ge 2 ] || die "--owner needs a value"; OWNER=$2; shift 2 ;;
    --build)    BUILD=1; shift ;;
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

TOPDIR=$(cd "$(dirname "$0")/.." && pwd)
[ -d "$TOPDIR/$PACKAGE" ] || die "no directory $PACKAGE in $TOPDIR"
if [ -z "$VERSION" ]; then
  VERSION=$(ls -1 "$TOPDIR/$PACKAGE" | grep -vx rpmbuild | sort -V | tail -1)
  [ -n "$VERSION" ] || die "no version directory found in $PACKAGE"
  echo "Using the newest version directory: $VERSION"
fi
SPEC="$TOPDIR/$PACKAGE/$VERSION/$PACKAGE.spec"
[ -f "$SPEC" ] || die "missing $SPEC"
grep -Fq "$PACKAGE/$VERSION/rpmbuild/SOURCES/" "$TOPDIR/md5sums.txt" ||
  echo "WARNING: no source checksum for $PACKAGE/$VERSION in md5sums.txt; srpmsbuild.sh will refuse to build it" >&2
if ! git -C "$TOPDIR" ls-files --error-unmatch "$PACKAGE/$VERSION/$PACKAGE.spec" >/dev/null 2>&1; then
  echo "WARNING: $PACKAGE/$VERSION/$PACKAGE.spec is not tracked by git; commit and push it, COPR clones $GIT_URL" >&2
fi

run() {
  if [ "$DRYRUN" = 1 ]; then printf '+'; printf ' %q' "$@"; echo; else "$@"; fi
}

SCRIPT=$(mktemp)
trap 'rm -f "$SCRIPT"' EXIT
cat > "$SCRIPT" <<EOS
#!/bin/bash
git clone --depth 1 $GIT_URL
cd HEPrpms
sh srpmsbuild.sh  $PACKAGE $VERSION
EOS

IFS=',' read -r -a PROJ <<< "$PROJECTS"
for proj in "${PROJ[@]}"; do
  # Same settings as the packages that are already in the projects.
  case "$proj" in
    HEPrpmsSUSE) BUILDDEPS="git rpmdevtools wget"; CHROOT=fedora-latest-x86_64 ;;
    *)           BUILDDEPS="git spectool wget";    CHROOT=epel-8-x86_64 ;;
  esac
  if copr-cli get-package --name "$PACKAGE" "$OWNER/$proj" >/dev/null 2>&1; then
    action=edit-package-custom
    echo "== $OWNER/$proj: $PACKAGE exists, updating it to $VERSION"
  else
    action=add-package-custom
    echo "== $OWNER/$proj: adding $PACKAGE $VERSION"
  fi
  run copr-cli "$action" \
      --name "$PACKAGE" \
      --script "$SCRIPT" \
      --script-chroot "$CHROOT" \
      --script-builddeps "$BUILDDEPS" \
      --script-resultdir "HEPrpms/$PACKAGE/$VERSION/rpmbuild/SOURCES/" \
      "$OWNER/$proj"
  if [ "$BUILD" = 1 ]; then
    run copr-cli build-package --name "$PACKAGE" --nowait "$OWNER/$proj"
  fi
done
