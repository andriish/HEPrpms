#!/bin/bash
# Helpers shared by the scripts in this directory. Source this file, do not run it.

in_list() { local x=$1; shift; local y; for y in "$@"; do [ "$x" = "$y" ] && return 0; done; return 1; }

# Normalise a version so that sort -V orders it sensibly:
# pre-release tags (3.5.1pre12, 3.0.01-beta01) are older than the release ('~' sorts first),
# and leading zeros of a component are dropped (02.22.02 = 2.22.2).
norm() { sed -E 's/([0-9])-?(pre|alpha|beta|rc)/\1~\2/; s/(^|\.)0+([0-9])/\1\2/g' <<< "$1"; }

# is_newer A B: A is a newer version than B
is_newer() {
  [ "$(norm "$1")" != "$(norm "$2")" ] &&
    [ "$(printf '%s\n%s\n' "$(norm "$1")" "$(norm "$2")" | sort -V | tail -1)" = "$(norm "$1")" ]
}

# copr_used_versions OWNER PROJECTS
# Fills USED["pkg/version"]=1 and NEWEST[pkg]=<newest version> with the package versions that are built in the
# given COPR projects (comma-separated). The version is taken from the "srpmsbuild.sh <package> <version>" line
# of the COPR custom-source script. Reading the package list needs no COPR login.
declare -gA USED NEWEST
copr_used_versions() {
  local owner=$1 projects=$2 proj json n pkg ver
  local -a proj_list
  command -v copr-cli >/dev/null || { echo "ERROR: copr-cli is not installed" >&2; return 1; }
  command -v jq >/dev/null || { echo "ERROR: jq is not installed" >&2; return 1; }
  IFS=',' read -r -a proj_list <<< "$projects"
  for proj in "${proj_list[@]}"; do
    json=$(copr-cli list-packages "$owner/$proj" --output-format json) ||
      { echo "ERROR: cannot list packages of $owner/$proj" >&2; return 1; }
    n=0
    while read -r pkg ver; do
      [ -n "$pkg" ] || continue
      n=$((n + 1))
      USED["$pkg/$ver"]=1
      if [ -z "${NEWEST[$pkg]:-}" ] || is_newer "$ver" "${NEWEST[$pkg]}"; then NEWEST[$pkg]=$ver; fi
    done < <(jq -r '.[].source_dict.script // empty' <<< "$json" |
             sed -nE 's/.*srpmsbuild\.sh[[:space:]]+([^[:space:]]+)[[:space:]]+([^[:space:]]+).*/\1 \2/p')
    [ "$n" -gt 0 ] || { echo "ERROR: found no packages in $owner/$proj, refusing to continue" >&2; return 1; }
    echo "$owner/$proj: $n packages" >&2
  done
}
