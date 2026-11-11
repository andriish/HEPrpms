# HEPrpms Agent Instructions

## Purpose
This file provides repository-specific guidance for AI assistants working on `HEPrpms`.

## Repository scope
- `HEPrpms` is a collection of RPM packaging recipes for High Energy Physics (HEP) software.
- The repository contains `.spec` files, patches, and build scripts for many HEP packages.
- Binary packages are built and published on COPR:
  - `https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/`
  - `https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpmsSUSE/`
- The root `README.md` documents supported distros, repository use, and build instructions.

## What to focus on
When helping with this repo, prioritize tasks around:
- RPM spec file maintenance and updates
- patch creation/fixes for package sources
- ensuring package build compatibility with Fedora, CentOS/EPEL, and openSUSE targets
- understanding package metadata, dependencies, and build requirements
- fixing packaging mistakes such as invalid macros, wrong source URLs, missing build requirements, or bad patch application
- improving documentation for package usage or build instructions

## How to work with the codebase
- Inspect package directories under the root for the relevant package version and patch files.
- Use `README.md` and the COPR repo pages as the authoritative high-level repository documentation.
- If a change touches packaging, check for related `.patch` files, `srpmsbuild.sh`, `alllocal.sh`, and `spectool` usage.
- If asked to add or change packages, prefer minimal, semantic edits rather than broad refactors.
- Consult `DEPENDENCIES.md` in the repository root to understand the dependencies between the packages in this repository (direct dependencies, reverse dependencies, build order, and the distribution packages they rely on). Use it to decide which packages must be built first and which packages are affected by a change. When a package's `BuildRequires:`/`Requires:` on another repository package changes, or a package is added, update `DEPENDENCIES.md` too.

## New version packaging workflow
When creating a spec file for a new package version, follow these steps:
- Identify the package directory and latest version subfolder under the package name.
- Copy the existing `.spec` file and any relevant `.patch` files from the current version directory as a starting point.
- Update version-related fields in the spec file: `Version:`, `Release:`, source URL(s), checksums, and any version-specific build logic.
- Preserve existing changelog history; keep the old changelog entries and append the new version note to the top of `%changelog`.
- Verify changelog entry dates are valid and use the correct weekday; RPM warns on bogus changelog dates.
- Verify the upstream source tarball or archive location and update `Source0` / `Source1` accordingly.
- Add or update patch references only if the new version still requires the same fixes; remove obsolete patches and add new ones when necessary.
- Check `BuildRequires:` and runtime `Requires:` for new or changed dependencies introduced by the new upstream release.
- Run local package tools like `spectool` or `rpmbuild -bp` if available to validate sources and syntax.
- Keep the new version directory layout consistent with the repository convention and include any package-specific build helper scripts if needed.
- Provide reasonable changelog entries explaining the version update and any packaging changes.

## Adding a new package
When adding a package that does not yet exist in the repository (for example `OpenPHIGS`):
- Create `<package>/<version>/<package>.spec`, plus any patches and a `do.sh` if patches are generated from `ORIG/` and `PATCHED/` trees.
- RPM `Version:` cannot contain `-`. If the upstream tag does, e.g. `v0.6-5`, use `Version: 0.6.5` and a `%global upstream_ver 0.6-5` macro for `Source0` and `%setup -n`. The downloaded file keeps the upstream name (`v0.6-5.tar.gz`), and that name is what `srpmsbuild.sh` stores in `rpmbuild/SOURCES`.
- Run `sh srpmsbuild.sh <package> <version>` once; it fails on an unknown checksum and writes it to `md5sums.txt.rej`. After checking the source, add the line `<md5>  <package>/<version>/rpmbuild/SOURCES/<file>` to `md5sums.txt` and rerun.
- Add a row for the package to the table in `README.md`, keeping the alphabetical order.
- Once the package is committed and pushed, register it in COPR with `scripts/copr-add-package.sh <package> <version>`.
- Add the package to `DEPENDENCIES.md` (dependency table, build-order layer, reverse dependencies, graph, and distribution dependencies). Add it to the `HEPrpms` meta package only if it is meant to be part of the full collection.
- If the build needs distribution packages that are not installed locally (such as `glew-devel`), verify the binary build in a clean chroot, e.g. `mock -r fedora-rawhide-x86_64 <package>.src.rpm`, instead of relying on `--nodeps`.
- Check whether upstream installs files relative to the prefix (for example `lib/` or `tests/`) rather than to `%{_libdir}`; relocate them in `%install` so that the package passes the unpackaged-files check.

## Local build workflow
This repository supports local package builds for verification.
- The root script `srpmsbuild.sh` is the primary local build helper.
- From the repository root, run:
  - `sh srpmsbuild.sh <package> <version> --build`
  - this downloads sources referenced by the `.spec`, checks MD5 checksums against `md5sums.txt`, executes `do.sh` if present, builds the SRPM, and then rebuilds the binary RPMs.
- Omitting `--build` creates only the SRPM.
- Build artifacts are written under `<package>/<version>/rpmbuild`, so multiple packages can be built in parallel without interfering.
- `alllocal.sh` is a wrapper that defines a `BUILDLIST` array of `package:version` entries, then runs `srpmsbuild.sh` for each item in parallel and saves logs under `logs/`.
- Local build output and failure details are stored in `logs/`, and the AI agent should read and analyze those logs when diagnosing build issues.
- Use `alllocal.sh` as a template: uncomment or add the desired `package:version` lines, then run `sh alllocal.sh` from the repo root.
- Local builds require `rpmdevtools`, `wget`, and network access to download upstream source archives.

## Scripts and COPR project maintenance
- All helper scripts for repository maintenance live in the `scripts/` directory (`srpmsbuild.sh` and `alllocal.sh` remain in the repository root because the COPR package definitions call `srpmsbuild.sh` from there).
- `scripts/copr-add-package.sh [--projects LIST] [--build] [--dry-run] <package> [<version>]` registers a package in the COPR project(s) as a custom source that clones `https://github.com/andriish/HEPrpms.git` and runs `srpmsbuild.sh <package> <version>`; if the package already exists in COPR it is updated instead. The default project is `HEPrpms`; use `--projects HEPrpms,HEPrpmsSUSE` for both. The package must be committed and pushed to GitHub before COPR can build it. It needs a valid `~/.config/copr` token; use `--dry-run` to only print the `copr-cli` commands.
- `scripts/git-rm-unused.sh [--run] [--keep NAME]` prints (or with `--run` executes) the `git rm` command that removes unused package/version directories. The COPR package list is read with `copr-cli list-packages`, and the version of each package is taken from the `srpmsbuild.sh <package> <version>` line of its COPR script.
- Retention rules: a `<package>/<version>` directory stays in the repository if (1) that version is built in the `HEPrpms` or `HEPrpmsSUSE` COPR project, or (2) its version is newer than the newest version of that package built in those projects (a pending update), or (3) the package is protected, which by default is the `HEPrpms` meta package. All other directories, including packages that are not built in COPR at all, are removed. Do not delete package directories by hand; use the script, and do not add a version directory for an old version that is not built in COPR.
- Packages that were removed under these rules (`CGAL`, `DD4hep`, `imake`, `motif`, `pythia8`, `transfig`) are taken from the distribution by their dependents; see `DEPENDENCIES.md`. If one of them has to be built again, add it back with the "Adding a new package" steps and register it in COPR.
- After running `scripts/git-rm-unused.sh --run`, update `DEPENDENCIES.md` (and the table in `README.md`) for the removed packages, and review `git status` before committing.

## Best practices
- Keep changes aligned with RPM packaging conventions used in this repository.
- Do not assume an external build system beyond COPR; the repo is primarily about packaging recipes.
- Mention the supported platforms and the COPR build service when relevant.
- If a task requires a missing detail, ask for the target distro/version or the specific package name.

## Non-goals
- Do not invent new package build platforms that are not documented here.
- Do not make broad source code changes unrelated to RPM packaging.
- Do not assume there is a full application source tree for every package; many directories contain only packaging metadata.

## Useful references
- `README.md` in the repository root
- `https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/`
- `https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpmsSUSE/`
