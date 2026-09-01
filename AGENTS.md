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

## New version packaging workflow
When creating a spec file for a new package version, follow these steps:
- Identify the package directory and latest version subfolder under the package name.
- Copy the existing `.spec` file and any relevant `.patch` files from the current version directory as a starting point.
- Update version-related fields in the spec file: `Version:`, `Release:`, source URL(s), checksums, and any version-specific build logic.
- Verify the upstream source tarball or archive location and update `Source0` / `Source1` accordingly.
- Add or update patch references only if the new version still requires the same fixes; remove obsolete patches and add new ones when necessary.
- Check `BuildRequires:` and runtime `Requires:` for new or changed dependencies introduced by the new upstream release.
- Run local package tools like `spectool` or `rpmbuild -bp` if available to validate sources and syntax.
- Keep the new version directory layout consistent with the repository convention and include any package-specific build helper scripts if needed.
- Provide reasonable changelog entries explaining the version update and any packaging changes.

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
