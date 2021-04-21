%if %{?rhel}%{!?rhel:0} >= 8
%global optflags -std=legacy -DL_cuserid=512
%endif

%if 0%{?suse_version}
%global optflags -std=legacy -DL_cuserid=512 -fallow-argument-mismatch -fallow-invalid-boz -fcommon   -Ddgeqpf=dgeqp3 -Ddgeqpf_=dgeqp3_  -DDGEQPF=dgeqp3
%endif


%if %{?fedora}%{!?fedora:0} 
#global optflags -std=legacy -DL_cuserid=512 -fallow-argument-mismatch -fallow-invalid-boz -fcommon
#For Fedora it makes sense to check the compiler version
# replace gcc optimization flags
# for gcc < 8 must be -O (no -O2 or -O3)
#
# gcc -dumpversion => older 4.8.5 => newer 8
%define gcc_dump_ver %(gcc -dumpversion | sed 's/[.].*$//')

%if 0%{?gcc_dump_ver} < 8
%global optflags %(echo %{optflags} | sed 's/-O[0-3]/-O/')
%endif

# 2020-05-23
%if 0%{?gcc_dump_ver} >= 10
%global optflags %(echo %{optflags} -fcommon)
%endif
%endif

%if 0%{?fedora}
%if 0%{?fedora} > 6
%bcond_without gfortran
%else
%bcond_with gfortran
# this is set to 1 if g77 is used for the cernlib package without
# suffix.
%define g77_cernlib_compiler 1
%endif

%if 0%{?fedora} <= 3
%define old_lapack_name 1
%endif
%if 0%{?fedora} <= 4
%define monolithic_X 1
%endif
%endif

%if 0%{?rhel}
%bcond_without gfortran
%endif

%if 0%{?suse_version}
%bcond_without gfortran
%endif




# compiler is used to disambiguate package names and executables
%if %{with gfortran}

%define compiler_string -gfortran
%if 0%{?g77_cernlib_compiler}
%define compiler -gfortran
%endif

%else
# g77 is used to build the utilities that goes in the packages without
# suffix.
%define utils_compiler 1
%define compiler_string -g77
%if ! 0%{?g77_cernlib_compiler}
%define compiler -g77

# there is no --build-id in RHEL 5.
%if 0%{?rhel}
%if 0%{?rhel} <= 5
%define no_build_id 1
%endif
%endif

%endif
# no compat prefix, the utilities compiled with gfortran are non functionnal
# see Bug 241416
#%%define compat compat-
%endif
# verdir is the directory used for libraries and replaces the version 
# in some files and file names
%define verdir %{version}%{?compiler}
# data files should be the same and therefore parallel installable

Name:          %{?compat}cernlib%{?compiler}
Version:       2006
Release:       43%{?dist}
Summary:       General purpose CERN library
Group:         Development/Libraries
# As explained in the cernlib on debian FAQ, cfortran can be considered LGPL.
# http://borex.princeton.edu/~kmccarty/faq.html#44
License:       GPL+ and LGPLv2+
URL:           http://cernlib.web.cern.ch/cernlib/
# mandrake
#BuildRequires: libxorg-x11-devel lesstif-devel libblas3-devel liblapack3-devel
#BuildRequires: gcc-g77
#Requires:      libxorg-x11-devel lesstif-devel libblas3-devel liblapack3-devel
# fedora core

%if 0%{?old_lapack_name}
BuildRequires: lapack blas
%else
BuildRequires: lapack-devel blas-devel
%endif

%if 0%{?suse_version}
BuildRequires: lapacke-devel
%endif

%if 0%{?monolithic_X}
BuildRequires: xorg-x11-devel 
%else
BuildRequires: imake libXaw-devel
# workaround #173530
BuildRequires: libXau-devel
%endif

# indirectly requires lesstif or openmotif and X libs
BuildRequires: xbae-devel

# for patchy build scripts
BuildRequires: tcsh
BuildRequires: gawk

BuildRequires: desktop-file-utils

%if %{?fedora}%{!?fedora:0} >=28 || %{?rhel}%{!?rhel:0} >= 8
BuildRequires: libnsl libnsl2-devel make
%endif

%if %{with gfortran}
%if 0%{?rhel} || 0%{?fedora}
BuildRequires: gcc-gfortran
%endif
%if 0%{?suse_version}
BuildRequires:  gcc-fortran
%endif
%else
BuildRequires: /usr/bin/g77
%endif
BuildRoot:     %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)

# these sources are different from the upstream sources as files with 
# GPL incompatible licences are removed. You can use cernlib-remove-deadpool
# and cernlib-deadpool.txt to recreate them from the upstream
# In a directory with the cernlib sources, issue 
# sh cernlib-remove-deadpool 

# source is now monolithic
#Source0: http://wwwasd.web.cern.ch/wwwasd/cernlib/download/2006_source/tar/2006_src.tar.gz
Source0: 2006_src-free.tar.gz

# The patchy version 4 sources. Mattias Ellert gave it so me in a mail.
Source17: patchy.tar.gz
# this is modified with regard with what Mattias gave me. Indeed the file
# containing some other files (p4inceta) was in fortran unformatted format, 
# I believe it is what caused a segfault of rceta on the ppc platform.
# I have recreated the patchy.tar.gz from Mattias tarball by running
# sh patchy-unpack-rceta
Source203: patchy-unpack-rceta 

# Shell scripts that go in /etc/profile.d
# Currently they are not installed since they do more harm than good, when
# parallel cernlib versions are installed.
Source100: cernlib.sh.in
Source105: cernlib.csh.in
# m4 macros for autoconf
Source101: cernlib.m4
# README file for paw slightly modified from the debian
Source102: paw.README
# README file that lists the changes done in the package
Source103: cernlib.README
# mkdirhier is used to create directories. Taken from xorg-x11
Source104: mkdirhier
# Files that can be used in the SOURCE directory to remove files with
# GPL incompatible licences from upstream sources
Source200: cernlib-remove-deadpool
Source201: cernlib-deadpool.txt
# copyright.in from the non split debian patchset
Source204: cernlib-debian-copyright.in

# debian patchsets
Patch100001: cernlib_2006.dfsg.2-13.diff.gz
Patch100002: paw_2.14.04.dfsg.2-6.diff.gz
Patch100003: mclibs_2006.dfsg.2-5.diff.gz
Patch100004: geant321_3.21.14.dfsg-8.diff.gz
Patch100005: cernlib_2006.dfsg.2-14.diff.gz
# change file to directory to DATADIR
Patch1100: cernlib-enforce-FHS.diff
Patch1: geant321-001-fix-missing-fluka.dpatch
# mclibs debian split also remove other packages from LIBDIRS
Patch2: 002-fix-missing-mclibs.dpatch
Patch3: geant321-003-geant-dummy-functions.dpatch
Patch100: mclibs-100-fix-isajet-manual-corruption.dpatch
Patch101: mclibs-101-undefine-PPC.dpatch
Patch10201: cernlib-102-dont-optimize-some-code.dpatch
Patch10202: paw-102-dont-optimize-some-code.dpatch
Patch10203: mclibs-102-dont-optimize-some-code.dpatch
Patch103: cernlib-103-ignore-overly-long-macro-in-gen.h.dpatch
Patch104: cernlib-104-fix-undefined-insertchar-warning.dpatch
Patch10501: cernlib-105-fix-obsolete-xmfontlistcreate-warning.dpatch 
Patch10502: paw-105-fix-obsolete-xmfontlistcreate-warning.dpatch 
Patch106: cernlib-106-fix-paw++-menus-in-lesstif.dpatch
Patch107: cernlib-107-define-strdup-macro-safely.dpatch
Patch108: paw-108-quote-protect-comis-script.dpatch
# use xsneut95.dat from the archive
Patch109: geant321-109-fix-broken-xsneut95.dat-link.dpatch
Patch110: cernlib-110-ignore-included-lapack-rule.dpatch
Patch111: cernlib-111-fix-kuesvr-install-location.dpatch
Patch112: cernlib-112-remove-nonexistent-prototypes-from-gen.h.dpatch
# a workaround for cups. Applied in doubt.
# removed in 2006
#Patch113: cernlib-113-cups-postscript-fix.dpatch
# it is the same in all source packages...
Patch114: 114-install-scripts-properly.dpatch
Patch115: cernlib-115-rsrtnt64-goto-outer-block.dpatch
Patch116: cernlib-116-fix-fconc64-spaghetti-code.dpatch
Patch117: geant321-117-fix-optimizer-bug-in-gphot.dpatch
Patch118: cernlib-118-rename-mathlib-common-blocks.dpatch
Patch11901: cernlib-119-fix-compiler-warnings.dpatch
Patch11902: paw-119-fix-compiler-warnings.dpatch
Patch12001: cernlib-120-fix-gets-usage-in-kuipc.dpatch 
Patch12002: paw-120-fix-mlp-cdf-file.dpatch
Patch12101: cernlib-121-fix-mathlib-test-case-c209m.dpatch
Patch12102: paw-121-call-gfortran-in-cscrexec.dpatch
Patch122: cernlib-122-fix-cdf-file-syntax-errors.dpatch
Patch123: cernlib-123-extern-memmove-only-if-not-macro.dpatch
Patch124: cernlib-124-integrate-patchy-bootstrap.dpatch
Patch125: cernlib-125-fix-PLINAME-creation.dpatch
# correct build, use optflags and have fcasplit handle
# enough command line arguments (this fcasplit is shipped)
Patch126: cernlib-126-fix-patchy-compile-flags.dpatch
# in yexpand don't put temporary files in HOME, but in current directory
Patch127: cernlib-127-yexpand-makes-tmpfiles-in-pwd.dpatch
Patch200: paw-200-comis-allow-special-chars-in-path.dpatch
Patch201: cernlib-201-update-kuip-helper-apps.dpatch
Patch202: cernlib-202-fix-includes-in-minuit-example.dpatch
Patch203: geant321-203-compile-geant-with-ertrak.dpatch
Patch204: mclibs-204-compile-isajet-with-isasrt.dpatch
Patch205: cernlib-205-max-path-length-to-256.dpatch
Patch206: mclibs-206-herwig-uses-DBLE-not-REAL.dpatch
Patch207: paw-207-compile-temp-libs-with-fPIC.dpatch
# without that patch the binaries are linked with something like
# libzftplib.a /builddir/build/BUILD/cernlib-2005/2005/build/packlib/cspack/programs/zftp/libzftplib.a
Patch208: cernlib-208-fix-redundant-packlib-dependencies.dpatch
Patch209: cernlib-209-ignore-unneeded-headers-in-kmutil.c.dpatch
# in debian split the patches are specific for paw geant and mclibs but
# not cernlib. Here we keeep the cernlib one since it contains all the others
Patch210: 210-improve-cfortran-header-files.dpatch
# split in newer debian patchset
# in debian split there is a common part at the beginning of the patch
# 
# obsolete in 2006:
# 211-support-amd64-and-itanium corresponds with a merge of
# cernlib-211-support-amd64-and-itanium.dpatch
# paw-211-support-amd64-and-itanium.dpatch
# 
# in 2006
# paw-211-support-amd64-and-itanium.dpatch has the common part removed
Patch21101: cernlib-211-support-amd64-and-itanium.dpatch
Patch21102: paw-211-support-amd64-and-itanium.dpatch
Patch2111: cernlib-211-support-digital-alpha.dpatch
Patch212: cernlib-212-print-test-results.dpatch
Patch21301: cernlib-213-fix-test-suite-build.dpatch
Patch21302: geant321-213-fix-test-suite-build.dpatch
Patch21303: mclibs-213-fix-test-suite-build.dpatch
Patch214: cernlib-214-fix-kernnum-funcs-on-64-bit.dpatch
# not in latest debian cernlib and doesn't seems to be useful on fedora
#Patch215: cernlib-215-work-around-g77-bug-on-ia64.dpatch
Patch216: cernlib-216-use-cernlib-gamma-not-intrinsic.dpatch
Patch217: cernlib-217-abend-on-mathlib-test-failure.dpatch
Patch220: mclibs-220-compile-isajet-with-isarun.dpatch
Patch221: mclibs-221-use-double-in-hepevt.dpatch
Patch300: cernlib-300-skip-duplicate-lenocc.dpatch
# Use another approach, see cernlib-enforce-FHS
# Patch33: 301-datafiles-comply-with-FHS.dpatch
# use cernlib-gxint-script.diff instead and sed for paw and dzedit.script
# Patch34: 302-scripts-comply-with-FHS.dpatch
Patch303: cernlib-303-shadow-passwords-supported.dpatch
Patch304: cernlib-304-update-Imake-config-files.dpatch
Patch30501: paw-305-use-POWERPC-not-PPC-as-test.dpatch
Patch30502: mclibs-305-use-POWERPC-not-PPC-as-test.dpatch
# the bug in /usr/include/assert.h seems to be present in FC-4. So a local
# version is provided in that patch. Will have to look at newer glibc-headers
# packages
# it is the same in all packages in cernlib split
Patch306: 306-patch-assert.h-for-makedepend.dpatch
# it is the same in all packages in cernlib split
Patch307: 307-use-canonical-cfortran.dpatch
Patch30801: cernlib-308-use-canonical-cfortran-location.dpatch
Patch30802: paw-308-use-canonical-cfortran-location.dpatch
Patch309: mclibs-309-define-dummy-herwig-routines.dpatch
Patch310: mclibs-310-define-dummy-fowl-routines.dpatch
# The zebra qnexte is a fake, removing it remove an unneeded dependency.
# the other qnext don't seem to be the same code? They are duplicate symbols
# anyway so one must be removed
Patch311: cernlib-311-skip-duplicate-qnext.dpatch
Patch312: mclibs-312-skip-duplicate-gamma.dpatch
# It is a departure from upstream. Apply, but may revert if not agreed.  
Patch31301: cernlib-313-comis-preserves-filename-case.dpatch
Patch31302: paw-313-comis-preserves-filename-case.dpatch
# fixed in 2006
#Patch314: cernlib-314-permit-using-regcomp-for-re_comp.dpatch
# first chunk of the patches are the same in the split cernlib
Patch315: 315-fixes-for-MacOSX.dpatch

Patch318: cernlib-318-additional-gcc-3.4-fixes.dpatch
# certainly not needed, but who knows?
Patch319: cernlib-319-work-around-imake-segfaults.dpatch
Patch32001: cernlib-320-support-ifort.dpatch
Patch32002: paw-320-support-ifort-and-gfortran.dpatch
Patch32101: cernlib-321-support-gfortran.dpatch
Patch32102: mclibs-321-support-gfortran.dpatch

# use host.def for gfortran 
Patch600: cernlib-600-use-host.def-config-file.dpatch
# in cernlib debian split also remove other packages from LIBDIRS
Patch700: 700-remove-kernlib-from-packlib-Imakefile.dpatch
Patch70101: cernlib-701-patch-hbook-comis-Imakefiles.dpatch
Patch70102: paw-701-patch-hbook-comis-Imakefiles.dpatch
Patch702: 702-patch-Imakefiles-for-packlib-mathlib.dpatch
# I would have preferred not to move the motif code to toplevel...
# in cernlib debian split also remove other packages from LIBDIRS
Patch703: 703-patch-code_motif-packlib-Imakefiles.dpatch
Patch704: cernlib-704-patch-code_kuip-higzcc-Imakefiles.dpatch
# I would have preferred not to move the motif code to toplevel...
# in paw debian split also remove other packages from LIBDIRS
Patch705: 705-patch-paw_motif-paw-Imakefiles.dpatch
Patch706: paw-706-use-external-xbae-and-xaw.dpatch

Patch800: cernlib-800-implement-shared-library-rules-in-Imake.dpatch
Patch80101: cernlib-801-non-optimized-rule-uses-fPIC-g.dpatch
Patch80102: paw-801-non-optimized-rule-uses-fPIC-g.dpatch
# in cernlib debian split also remove other packages from LIBDIRS
# otherwise patches are cleanly split
Patch802: 802-create-shared-libraries.dpatch
# in the original cernlib kxterm is built with the C compiler, which cause
# a failure if compiled with the cernlib debian script as -lg2c isn't found. 
# It is corrected in
Patch80301: cernlib-803-link-binaries-dynamically.dpatch
Patch80302: paw-803-link-binaries-dynamically.dpatch
# 803 depends on 
# 208-fix-redundant-packlib-dependencies.dpatch

# no idea about what this does
Patch804: paw-804-workaround-for-comis-mdpool-struct-location.dpatch
Patch805: cernlib-805-expunge-missing-mathlib-kernlib-symbols.dpatch 
# not needed but keep sync with debian, that'll avoid bumping sonames
Patch80601: cernlib-806-bump-mathlib-and-dependents-sonames.dpatch
Patch80602: paw-806-bump-mathlib-and-dependents-sonames.dpatch
Patch80603: geant321-806-bump-mathlib-and-dependents-sonames.dpatch
Patch80604: mclibs-806-bump-mathlib-and-dependents-sonames.dpatch
Patch80701: cernlib-807-static-link-some-tests-on-64-bit.dpatch
Patch80702: geant321-807-static-link-some-tests-on-64-bit.dpatch
Patch80703: mclibs-807-static-link-some-tests-on-64-bit.dpatch

# change the cernlib script such that -llapack -lblas is used instead of 
# cernlib lapack
# depend explicitely on libkernlib now that it is out of packlib
# use lesstif-pawlib and lesstif-packlib
# use external Xbae and Xaw
# the modified script is renamed cernlib-static later and the debian cernlib
# script is used instead.
Patch1200: cernlib-script.patch
# don't stop if the CERN variable isn't defined
Patch1201: cernlib-gxint-script.diff
# modify the cernlib man page to fit with the distribution
Patch1206: cernlib-man_static.patch

# patchy 4
# not applied as it has allready been done by the sed one-liner in 
# patchy-unpack-rceta
##AV Patch1500: patchy-rceta.patch

Patch1501: patchy-insecure_tmp_use.diff
#Patch1502: patchy-fcasplit.patch
# build fixes
Patch1503: patchy-p4comp.patch
# use the flags in the the p4boot.sh script and have fcasplit handle
# enough command line arguments (this fcasplit is not shipped)
Patch1507: patchy-use_OPT.patch

%description
CERN program library is a large collection of general purpose libraries
and modules maintained and offered on the CERN. Most of these programs 
were developed at CERN and are therefore oriented towards the needs of a 
physics research laboratory that is general mathematics, data analysis, 
detectors simulation, data-handling etc... applicable to a wide range 
of problems.

The main and devel packages are parallel installable, but not the helper
scripts from the utils subpackage.

%package devel
Summary:       General purpose CERN library development package

%if 0%{?old_lapack_name}
Requires:       lapack blas
%else
Requires:       lapack-devel blas-devel
%endif

%if 0%{?fedora} >= 28 || %{?rhel}%{!?rhel:0} >= 8
Requires: libnsl2-devel
%endif
%if 0%{?suse_version}
BuildRequires: libnsl-devel
%endif
# Motif and X devel libs are indirectly required through xbae
Requires: xbae-devel

%if 0%{?monolithic_X}
Requires:      xorg-x11-devel
%else
# workaround #173530
Requires:      libXau-devel
Requires:      libXaw-devel
%endif
Requires:      %{name} = %{version}-%{release}
Group:         Development/Libraries
Provides:  cernlib(devel) = %{version}-%{release}

# for the m4 macro directory ownership
Requires: automake

%description devel
CERN program library is a large collection of general purpose libraries
and modules maintained and offered on the CERN. Most of these programs 
were developed at CERN and are therefore oriented towards the needs of a 
physics research laboratory that is general mathematics, data analysis, 
detectors simulation, data-handling etc... applicable to a wide range 
of problems.

The cernlib-devel package contains the header files and symlinks needed  
to develop programs that use the CERN library using dynamic libraries.

Static libraries are in %{name}-static.

%package static
Summary:       General purpose CERN library static libraries
Group:         Development/Libraries
Requires:  %{name}-devel = %{version}-%{release}
Provides:  cernlib(static) = %{version}-%{release}

%description static
CERN program library is a large collection of general purpose libraries
and modules maintained and offered on the CERN. Most of these programs 
were developed at CERN and are therefore oriented towards the needs of a 
physics research laboratory that is general mathematics, data analysis, 
detectors simulation, data-handling etc... applicable to a wide range 
of problems.

The %{name}-static package contains the static cernlib libraries.

%package utils
Summary:   CERN library compilation and environment setting scripts
Group:     Applications/System
Requires:  %{name}-devel = %{version}-%{release}
Requires:  %{name}-static = %{version}-%{release}
Provides:  cernlib(utils) = %{version}-%{release}

%description utils
CERN library compilation and environment setting scripts.

This package will conflict with other versions, therefore if you 
want to have different compile script and different environments for 
different versions of the library you have to set them by hand.

%package -n %{?compat}geant321%{?compiler}
Summary:  Particle detector description and simulation tool
Group:    Applications/Engineering
Requires: %{name}-devel = %{version}-%{release}
Requires: %{name}-utils = %{version}-%{release}

%description -n %{?compat}geant321%{?compiler}
Geant simulates the passage of subatomic particles through matter, for 
instance, particle detectors. For maximum flexibility, Geant simulations 
are performed by linking Fortran code supplied by the user with the Geant 
libraries, then running the resulting executable.

This package includes gxint, the script used to perform this linking step. 

%package -n %{?compat}kuipc%{?compiler}
Summary:  Cernlib's Kit for a User Interface Package (KUIP) compiler
Group:    Development/Languages
Requires: cernlib(devel)

%description -n %{?compat}kuipc%{?compiler}
KUIPC, the Kit for a User Interface Package Compiler, is a tool to simplify 
the writing of a program's user interface code. It takes as input a Command 
Definition File (CDF) that describes the commands to be understood by the 
program, and outputs C or FORTRAN code that makes the appropriate function 
calls to set up the user interface. This code can then be compiled and linked 
with the rest of the program. Since the generated code uses KUIP routines, 
the program must also be linked against the Packlib library that contains them.

# we want to have both g77 and gfortran suffixed utilities available.
%package -n %{?compat}paw%{?compiler_string}
Group: Applications/Engineering
Summary: A program for the analysis and presentation of data

%description -n %{?compat}paw%{?compiler_string}
PAW is conceived as an instrument to assist physicists in the analysis and 
presentation of their data. It provides interactive graphical presentation 
and statistical or mathematical analysis, working on objects familiar to 
physicists like histograms, event files (Ntuples), vectors, etc. PAW is 
based on several components of the CERN Program Library.

%package -n cernlib-packlib%{?compiler_string}
Group: Applications/Archiving
Summary: I/O, network and other utilities from the cernlib

%description -n cernlib-packlib%{?compiler_string}
I/O, network and miscalleneous utilities based on the CERN Program 
Library. 
According to the responsible of the cernlib debian package, some
of these utilities may have security flaws.

%package -n %{?compat}patchy%{?compiler_string}
Group: Applications/Archiving
Summary: The patchy utilities

%description -n %{?compat}patchy%{?compiler_string}
Utilities for extracting sources from patchy cards and cradles.

# the package that has utils_compiler set provides the utilities without
# suffix
%if 0%{?utils_compiler}
%package -n paw
Group: Applications/Engineering
Summary: A program for the analysis and presentation of data

%description -n paw
PAW is conceived as an instrument to assist physicists in the analysis and 
presentation of their data. It provides interactive graphical presentation 
and statistical or mathematical analysis, working on objects familiar to 
physicists like histograms, event files (Ntuples), vectors, etc. PAW is 
based on several components of the CERN Program Library.

%package -n cernlib-packlib
Group: Applications/Archiving
Summary: I/O, network and other utilities from the cernlib

%description -n cernlib-packlib
I/O, network and miscalleneous utilities based on the CERN Program 
Library. 
According to the responsible of the cernlib debian package, some
of these utilities may have security flaws.

%package -n patchy
Group: Applications/Archiving
Summary: The patchy utilities

%description -n patchy
Utilities for extracting sources from patchy cards and cradles.
%endif


%prep

echo 'Building cernlib %{verdir}'

%setup -q -c 
# patchy4
%setup -q -T -D -a 17

# patch patchy 4 installer fortran generator script
# actually it is unusefull, because the unpacking has been done
# offline, see comment above.
#  patch -P 1500

%patch -P 1501

# unpack the patchy version 4 sources is done offline,
# see comment above
#pushd patchy
#        ./rceta.sh
#popd

#   patch -P 1502 -b .use_OPT
%patch -P 1503
%patch -P 1507 -p1 -b .use_OPT

# adapt static script to modular X, use system libs and follow debian split
%patch -P 1200 -p1 -b .script
# patches for man page
%patch -P 1206 -p1 -b .man_static
touch -r %{version}/src/man/man1/cernlib.1.man_static %{version}/src/man/man1/cernlib.1

# debian patchesets
%patch -P 100001 -p0
%patch -P 100002 -p0
%patch -P 100003 -p0
%patch -P 100004 -p0 
%patch -P 100005 -p0 
# we should set the debian files timestamps to the debian patches timestamps
# but they are not the same for the packages corresponding with different
# compilers :(. So we use the src/log/tag file.
#find cernlib-*dfsg* -type f -exec touch -r {PATCH100001} {} \;
#find paw-*dfsg* -type f -exec touch -r {PATCH100002} {} \;
#find mclibs-*dfsg* -type f -exec touch -r {PATCH100003} {} \;
#find geant321-*dfsg* -type f -exec touch -r {PATCH100004} {} \;
timestamp=%{_builddir}/%{name}-%{version}/%{version}/src/log/tag
find cernlib-*dfsg* -type f -exec touch -r $timestamp {} \;
find paw-*dfsg* -type f -exec touch -r $timestamp {} \;
find mclibs-*dfsg* -type f -exec touch -r $timestamp {} \;
find geant321-*dfsg* -type f -exec touch -r $timestamp {} \;

cp -p cernlib-2006.dfsg.2/debian/add-ons/bin/cernlib.in .

cd %{version}
%patch -P 1100 -p2
%patch -P 1 -p1
%patch -P 2 -p1
%patch -P 3 -p1
%patch -P 100 -p1
%patch -P 101 -p1
%patch -P 10201 -p1
%patch -P 10202 -p1
%patch -P 10203 -p1
%patch -P 103 -p1
%patch -P 104 -p1
%patch -P 10501 -p1
%patch -P 10502 -p1
%patch -P 106 -p1
%patch -P 107 -p1
%patch -P 108 -p1
%patch -P 109 -p1
%patch -P 110 -p1
%patch -P 111 -p1
%patch -P 112 -p1
#patch -P 113 -p1
%patch -P 114 -p1
%patch -P 115 -p1
%patch -P 116 -p1
%patch -P 117 -p1
%patch -P 118 -p1
%patch -P 11901 -p1
%patch -P 11902 -p1
%patch -P 12001 -p1
%patch -P 12002 -p1
%patch -P 12101 -p1
%patch -P 12102 -p1
%patch -P 122 -p1
%patch -P 123 -p1
%patch -P 124 -p1
%patch -P 125 -p1
%patch -P 126 -p1
%patch -P 127 -p1
%patch -P 200 -p1
%patch -P 201 -p1
%patch -P 202 -p1
%patch -P 203 -p1
%patch -P 204 -p1
%patch -P 205 -p1
%patch -P 206 -p1
%patch -P 207 -p1
%patch -P 208 -p1
%patch -P 209 -p1
%patch -P 210 -p1
%patch -P 21101 -p1
%patch -P 21102 -p1
%patch -P 2111 -p1
%patch -P 212 -p1
%patch -P 21301 -p1
%patch -P 21302 -p1
%patch -P 21303 -p1
%patch -P 214 -p1
#patch -P 215 -p1
%patch -P 216 -p1
%patch -P 217 -p1
%patch -P 220 -p1
%patch -P 221 -p1
%patch -P 300 -p1
#patch -P 301 -p1
#patch -P 302 -p1
%patch -P 303 -p1
%patch -P 304 -p1
%patch -P 30501 -p1
%patch -P 30502 -p1
%patch -P 306 -p1
%patch -P 307 -p1
%patch -P 30801 -p1
%patch -P 30802 -p1
%patch -P 309 -p1
%patch -P 310 -p1
%patch -P 311 -p1
%patch -P 312 -p1
%patch -P 31301 -p1
%patch -P 31302 -p1
#patch -P 314 -p1
%patch -P 315 -p1

# copy a paw include file to include directory (debian
# 317-copy-converter.h-to-installed-headers-dir.sh.dpatch)
# not in 2006
#cp src/pawlib/paw/tree/converter.h src/pawlib/paw/paw/

%patch -P 318 -p1
%patch -P 319 -p1
%patch -P 32001 -p1
%patch -P 32002 -p1
%patch -P 32101 -p1
%patch -P 32102 -p1

%patch -P 600 -p1

# move kernlib out of packlib (debian 700-move-kernlib-to-top-level.sh.dpatch)
mv src/packlib/kernlib src/kernlib

%patch -P 700 -p1

# move hkf1q.F and hkfill.F out of packlib/hbook and into
# pawlib/comis where they obviously belong (it even says so in the files!)
# (debian 701-move-packlib-hkfill-to-comis.sh.dpatch)
pushd src
                mv packlib/hbook/code/hkf1q.F  pawlib/comis/code/
                mv packlib/hbook/code/hkfill.F pawlib/comis/code/
                # these files also need some headers to go with them:
                mkdir pawlib/comis/hbook
                cp -p packlib/hbook/hbook/pilot.h    pawlib/comis/hbook/
                cp -p packlib/hbook/hbook/hcbook.inc pawlib/comis/hbook/
                cp -p packlib/hbook/hbook/hcbits.inc pawlib/comis/hbook/
                cp -p packlib/hbook/hbook/hcfast.inc pawlib/comis/hbook/
popd

%patch -P 70101 -p1
%patch -P 70102 -p1

# Must create dirs before applying following patch. 
# Corresponds with 702-fix-packlib-mathlib-circular-mess.sh.dpatch

pushd src
                # Hdiff depends upon a bunch of mathlib files; move it into
                # mathlib.
                mkdir mathlib/hbook
                mv packlib/hbook/hdiff mathlib/hbook/
                cp -r packlib/hbook/hbook mathlib/hbook/

                # Meanwhile, other packlib files depend upon these mathlib
                # files which are easily moved:
                mkdir packlib/hbook/d
                mv mathlib/gen/d/rgmlt*.F packlib/hbook/d/
                mv mathlib/gen/n          packlib/hbook/
                cp -r mathlib/gen/gen     packlib/hbook/
popd

%patch -P 702 -p1

# Script to move packlib/kuip/code_motif to top level, splitting it out
# from packlib.  But keep kmutil.c in packlib, it's otherwise used.
# debian 703-move-code_motif-to-top-level.sh.dpatch

pushd src
                mv packlib/kuip/code_motif/kmutil.c packlib/kuip/code_kuip/
                mv packlib/kuip/code_motif          ./
                cp -r packlib/kuip/kuip             code_motif/
popd

%patch -P 703 -p1

# Script to move the file kuwhag.c from packlib/kuip/code_kuip/kuwhag.c
# into graflib/higz/higzcc.  It appears to be the only file in code_kuip
# that depends upon grafX11 and libX11, so it fits in better here.
# debian 704-move-kuwhag.c-to-grafX11.sh.dpatch

pushd src
                mv packlib/kuip/code_kuip/kuwhag.c graflib/higz/higzcc/
                cp -r packlib/kuip/kuip            graflib/higz/
popd

%patch -P 704 -p1

# Script to move Lesstif-dependent Paw code into its own library.
# debian 705-move-paw++-code-to-top-level.sh.dpatch

pushd src
                mkdir paw_motif
                mv pawlib/paw/?motif pawlib/paw/fpanels? paw_motif/
                mv pawlib/paw/tree pawlib/paw/uimx pawlib/paw/xbae* paw_motif/
                cp -p pawlib/paw/Imakefile paw_motif/
                
                mkdir paw_motif/code
                mv pawlib/paw/code/pawpp.F   paw_motif/code/
                mv pawlib/paw/code/pawintm.F paw_motif/code/

                mkdir paw_motif/cdf
                mv pawlib/paw/cdf/pamcdf.cdf paw_motif/cdf/

                cp -pr pawlib/paw/paw paw_motif/
                cp -pr pawlib/paw/hpaw paw_motif/
popd

%patch -P 705 -p1
%patch -P 706 -p1

%patch -P 800 -p1
%patch -P 80101 -p1
%patch -P 80102 -p1
%patch -P 802 -p1
%patch -P 80301 -p1
%patch -P 80302 -p1

# Shell script to link pawlib/comis/comis into the top-level include directory.
# debian 804-link-to-comis-includes.sh.dpatch
pushd src
                ln -s ../pawlib/comis/comis include/comis
popd

%patch -P 804 -p1
%patch -P 805 -p1
%patch -P 80601 -p1
%patch -P 80602 -p1
%patch -P 80603 -p1
%patch -P 80604 -p1
%patch -P 80701 -p1
%patch -P 80702 -p1
%patch -P 80703 -p1

#patch -P 1504 -p2 -b .np_flags
#patch -P 1505 -p1
#patch -P 1506 -p2 -b .curdir

%patch -P 1201

# remove CVS directories
find . -depth -type d -name CVS -exec rm -rf {} ';' 

# unset executable bit on source files
chmod a-x src/kernlib/kerngen/ccgencf/cfstati.c \
  src/cfortran/cfortran.*

# remove empty header file not needed anywhere to shut up rpmlint
rm src/pawlib/paw/ntuple/dbmalloc.h


sed -i 's/ArCmdBase clq/ArCmdBase cq/g'  src/config/Imake.tmpl

%if %{?fedora}%{!?fedora:0} >= 34
sed -i 's/char    \*prompt\;/const char    \*prompt\;/g' src/packlib/cspack/tcpaw/tcpaw.c
%endif


%build

CERN=$RPM_BUILD_DIR/%{name}-%{version}
CERN_LEVEL=%{version}
CERN_ROOT=$CERN/$CERN_LEVEL
CVSCOSRC=$CERN/$CERN_LEVEL/src
PATH=$CERN_ROOT/bin:$PATH

export CERN
export CERN_LEVEL
export CERN_ROOT 
export CVSCOSRC
export PATH

LIB_SONAME=1
export LIB_SONAME

# add something in the soname to avoid binaries linked against g77-compiled
# library to be linked against gfortran-compiled libraries, as the ABI is 
# incompatible for functions.
%if %{with gfortran}
TOOL_SONAME=_gfortran
TOOL_NAME=_gfortran
export TOOL_SONAME
export TOOL_NAME
%endif

# set the CERN and CERN_LEVEL environment variables in shell scripts
# meant to go to /etc/profile.d
sed -e 's/==CERN_LEVEL==/%{verdir}/' -e 's:==CERN==:%{_libdir}/cernlib:' %{SOURCE100} > cernlib-%{verdir}.sh
sed -e 's/==CERN_LEVEL==/%{verdir}/' -e 's:==CERN==:%{_libdir}/cernlib:' %{SOURCE105} > cernlib-%{verdir}.csh

cp -p %{SOURCE101} .
cp -p %{SOURCE102} .
cp -p %{SOURCE103} .

# Regenerate the copyright file (from non split debian/rules)
grep -v DEADPOOL_LIST_GOES_HERE %{SOURCE203} > debian-copyright
sed -e 's/#.*//g' -e '/^[[:space:]]*$$/d' %{SOURCE201} | \
    sort | uniq >> debian-copyright

cd $CERN_ROOT

# substitude the right defaults in the scripts
sed -i.paths -e 's:"/cern":"%{_libdir}/cernlib/":' -e 's:"pro":"%{verdir}":' \
   src/scripts/paw src/scripts/cernlib src/graflib/dzdoc/dzedit/dzedit.script

sed -i -e 's:"/cern":"%{_libdir}/cernlib/":' -e 's:"pro":"%{verdir}":' \
   ../patchy/ylist ../patchy/yindex

%if %{with gfortran}
FC_COMPILER=gfortran
%else
FC_COMPILER=g77
%endif

# substitute version in gxint with the right version
# substitute includedir in gxint to conform to FHS, and gxint.o to gxint.f
# and substitue the name of the cernlib link script
sed -i -e 's/"pro"/%{version}/' -e 's:\${CERN}/\${ver}/lib/gxint\${gvs}\.\$_o:%{_includedir}/cernlib/\${ver}/gxint.f:' \
  -e 's/`cernlib /`cernlib%{?compiler} /' \
  -e 's/"f77"/"'$FC_COMPILER'"/' \
  src/scripts/gxint 

# substitute DATADIR in source files to conform to FHS
sed -i -e 's:DATADIR:%{_datadir}/cernlib/%{version}:' \
  src/geant321/miface/gmorin.F 
# don't correct datadir in test files or header used in tests
#src/mclibs/cojets/test/test.F src/mclibs/eurodec/eurodec/eufiles.inc src/mclibs/isajet/test/isajett.F

# substitute bindir in ylist and yindex to conform to FHS
sed -i -e 's:\$CERN/patchy/\$PATCHY_VERSION/bin:%{_bindir}:' ../patchy/ylist ../patchy/yindex

# Create the build directory structure
mkdir -p build bin lib shlib

# rename the cernlib script cernlib-static
mv src/scripts/cernlib src/scripts/cernlib-static
%{__install} -p -m755 src/scripts/cernlib-static bin/cernlib-static

# use the debian cernlib script for dynamic libraries support.
# remove -lg2c to the link commands, because libg2c.so isn't available, 
# it is found by g77/gfortran if needed, and similar with -lgfortran.
# don't add {_libdir} to the directory searched in for libraries, 
# since it is already in the list.
sed -e 's:@PREFIX@:%{_prefix}:g' \
  -e 's:@CERN@:%{_libdir}/cernlib:g' \
  -e 's:@VERSION@:%{verdir}:g' \
  -e 's:@LIBPREFIX@::g' \
  -e 's/-lg2c//' \
  -e 's/-lgfortran//' \
  ../cernlib.in > src/scripts/cernlib
# to remove reference to monolithic X directories
#  -e 's/-L\$XDIR\(64\)\? //' \
#  -e 's:-L/usr/X11R6/lib\(64\)\? ::' \
#  -e 's:/usr/X11R6/lib\(64\)\? ::g' \
#
chmod 0755 src/scripts/cernlib
touch -r ../cernlib.in src/scripts/cernlib

# install mkdirhier which is needed to make directories
%{__install} -p -m755 %{SOURCE104} bin/

# set FC_OPTFLAGS and FC_COMPILER based on compiler used
%if %{with gfortran}
%if %{?fedora}%{!?fedora:0} >= 32 
FC_OPTFLAGS="%{optflags} -fallow-argument-mismatch -fallow-invalid-boz"
%else
 FC_OPTFLAGS="%{optflags}"
%endif
FC_COMPILER=gfortran
%else
# optflags are different for g77, so we remove problematic flags
FC_OPTFLAGS=`echo "%optflags" | sed -e 's/-mtune=[^ ]\+//' -e 's/-fstack-protector//' -e 's/--param=ssp-buffer-size=[^ ]\+//'`
# this is needed (at least in F-8).
%if 0%{?no_build_id}
G_LDFLAGS=
%else
G_LDFLAGS='-Wl,--build-id'
%endif
FC_COMPILER=g77
%endif

PATHSAVE=$PATH
# Build patchy version 4
pushd ../patchy

if [ z"$G_LDFLAGS" != z ]; then
sed -i.ldflags -e "s/f77/f77 $G_LDFLAGS/" p4boot.sh
fi

%if %{with gfortran}
        sed -i.gfortran -e 's/f77/gfortran/' fcasplit.f p4boot.sh
%endif
        sed -i.optflags -e 's~FOPT \+=.*~FOPT = "'"$FC_OPTFLAGS"'"~' \
                 -e 's~COPT  \+=.*~COPT = "%{optflags} -D_GNU_SOURCE"~' p4boot.sh
        sed -i.mxleng -e 's/MXLENG=256/MXLENG=1024/' fcasplit.f
#        export PATH="$CERN/patchy:$CERN/patchy/p4sub:$PATH" 
        export PATH=".:..:$PATH" 
        p4boot.sh 0
popd
export PATH=$PATHSAVE

# pass informations to the build system through host.def
echo '#define DefaultCDebugFlags %{optflags} -D_GNU_SOURCE' >> ${CVSCOSRC}/config/host.def
%if %{?rhel}%{!?rhel:0} >= 8
echo '#define DefaultCDebugFlags %{optflags} -DIEC_60559_BFP_EXT=0 ' >> ${CVSCOSRC}/config/host.def
%else
echo '#define DefaultCDebugFlags %{optflags} -D_GNU_SOURCE' >> ${CVSCOSRC}/config/host.def
%endif


%if %{with gfortran}
echo '#define Hasgfortran YES' >> ${CVSCOSRC}/config/host.def
%endif
echo "#define FortranDebugFlags $FC_OPTFLAGS" >> ${CVSCOSRC}/config/host.def
# keep timestamps
echo "#define InstallCmd %{__install} -p" >> ${CVSCOSRC}/config/host.def
# don't strip executables
echo "#define InstPgmFlags -m 0755" >> ${CVSCOSRC}/config/host.def
# this is not used for libraries
#   echo "#define ExtraLoadOptions $G_LDFLAGS" >> ${CVSCOSRC}/config/host.def

# optflags are doubled for programs because they are in FortranDebugFlags
# and below, but they are not doubled for libs.
echo "#define FortranLinkCmd $FC_COMPILER $FC_OPTFLAGS $G_LDFLAGS" >> ${CVSCOSRC}/config/host.def

# glibc-2.25 (2017-02) added, among other things, function strfromd
# replace strfromd by strfromd9
cd $CERN_ROOT/src
sed -i 's/strfromd/strfromd9/g' graflib/higz/kuip/kstring.h
sed -i 's/strfromd/strfromd9/g' code_motif/mkdcmp.c
sed -i 's/strfromd/strfromd9/g' code_motif/kuip/kstring.h
sed -i 's/strfromd/strfromd9/g' packlib/kuip/kuip/kstring.h
sed -i 's/strfromd/strfromd9/g' packlib/kuip/ykuip/kmath.y
sed -i 's/strfromd/strfromd9/g' packlib/kuip/code_kuip/kmath.c
sed -i 's/strfromd/strfromd9/g' packlib/kuip/code_kuip/kuwhat.c
sed -i 's/strfromd/strfromd9/g' packlib/kuip/code_kuip/kexec.c
sed -i 's/strfromd/strfromd9/g' packlib/kuip/code_kuip/kkern.c
sed -i 's/strfromd/strfromd9/g' packlib/kuip/code_kuip/kmacro.c
sed -i 's/strfromd/strfromd9/g' packlib/kuip/code_kuip/kmenu.c
sed -i 's/strfromd/strfromd9/g' packlib/kuip/code_kuip/kvect.c

# glibc-2.32 (2020-08) deprecated symbols sys_errlist, sys_nerr are no longer available
# replace deprecated sys_errlist and sys_nerr by strerror
cd $CERN_ROOT/src
sed -i 's/sys_errlist\[errno\]/strerror\(errno\)/g' kernlib/kernbit/z268/systems.c # 2x
sed -i 's/n<sys_nerr/n<strerror\(\)/g'              packlib/cspack/sysreq/serror.c
sed -i 's/sys_errlist\[n\]/strerror\(n\)/g'         packlib/cspack/sysreq/serror.c
sed -i 's/sys_errlist\[errno\]/strerror\(errno\)/g' packlib/cspack/sysreq/socket.c

# fix printf with flis_name[i]
sed -i 's/printf (flis_name/printf ("\%s", flis_name/' packlib/kuip/code_kuip/kmenu.c

# Create the top level Makefile with imake
cd $CERN_ROOT/build
$CVSCOSRC/config/imake_boot

# Install kuipc and the scripts (cernlib, paw and gxint) in $CERN_ROOT/bin

#  %{?_smp_mflags} breaks the builds
make bin/kuipc
make scripts/Makefile
# make Makefiles that are not done automatically now
make p5boot/Makefile
make patchy/Makefile

cd scripts
make install.bin

# Install the libraries

cd $CERN_ROOT/build
make
make install.shlib
chmod a+x ../shlib/*.so.*

# Build dynamic paw
cd $CERN_ROOT/build/pawlib
make install.bin
cd $CERN_ROOT/
mv bin/pawX11 bin/pawX11.dynamic
mv bin/paw++ bin/paw++.dynamic

# Build static paw
$FC_COMPILER $FC_OPTFLAGS $G_LDFLAGS $CERN_ROOT/build/pawlib/paw/programs/0pamain.o \
  `cernlib -G X11 pawlib` -Wl,-E -o bin/pawX11
$FC_COMPILER $FC_OPTFLAGS $G_LDFLAGS $CERN_ROOT/build/pawlib/paw/programs/0pamainm.o \
  `cernlib -G Motif pawlib` -Wl,-E -o bin/paw++



# Build packlib
cd $CERN_ROOT/build/packlib
make install.bin

cd $CERN_ROOT/build/graflib
make install.bin

cd $CERN_ROOT/build/p5boot
make install.bin

# we cannot use %%{?_smp_mflags} because fcasplit is used after being
# built, and during the fcasplit installation (which is done in parallel 
# with the build) it is removed and replaced by a symlink, and if it 
# takes time the link may not be there on time. 
# At least that's my understanding of the failure.
cd $CERN_ROOT/build/patchy
make install.bin

# it is not completly obvious that it is better to use patchy 4 for 
# ypatchy, but that's what we do. In any case it should be replaced by a
# link to the final npatchy
rm $CERN_ROOT/bin/ypatchy

%install

rm -rf %{buildroot}

%{__install} -d -m755 %{buildroot}%{_sysconfdir}/profile.d

%{__install} -d -m755 cfortran/Examples
%{__install} -p -m644 %{version}/src/cfortran/Examples/*.c cfortran/Examples/

%{__install} -d -m755 %{buildroot}%{_datadir}/aclocal
%{__install} -p -m644 cernlib.m4 %{buildroot}%{_datadir}/aclocal/cernlib.m4

# copy patchy executables in bin. Keep the timestamps for the scripts.
%{__install} -d -m755 %{buildroot}%{_bindir}/
find patchy -name y* -a -perm -755 -exec %{__install} -p -m755 {} %{buildroot}%{_bindir} ';'

cd %{version}
# fix generated data file timestamps such that they are the same for all 
# compilers
touch -r src/mclibs/cojets/data/cojets.cpp lib/cojets.dat
touch -r src/mclibs/isajet/data/decay.cpp lib/isajet.dat
touch -r src/mclibs/eurodec/data/eurodec.dat lib/eurodec.dat

%{__install} -d -m755 %{buildroot}%{_libdir}/cernlib/%{verdir}/lib
%{__install} -d -m755 %{buildroot}%{_datadir}/cernlib/%{version}
%{__install} -d -m755 %{buildroot}%{_includedir}/cernlib/%{version}
%{__install} -d -m755 %{buildroot}%{_includedir}/cernlib/%{version}/cfortran

%{__install} -p -m644 lib/*.dat %{buildroot}%{_datadir}/cernlib/%{version}
 
%{__install} -p -m644 lib/gxint321.f %{buildroot}%{_includedir}/cernlib/%{version}
%{__install} -p -m644 src/cfortran/*.h %{buildroot}%{_includedir}/cernlib/%{version}/

%{__install} -p -m755 bin/* %{buildroot}%{_bindir}/


# avoid name conflicts for files in bin
# first move kuipc, cernlib and gxint scripts out of the way
for file in cernlib cernlib-static gxint kuipc; do
   mv %{buildroot}%{_bindir}/$file $file%{?compiler}
done

# always ship suffixed utilities
for file in %{buildroot}%{_bindir}/*; do
   cp -p $file ${file}%{compiler_string}
   # and not suffixed utilities only for one compiler 
   if [ 'z%{?utils_compiler}' != 'z1' ]; then
     rm $file
   fi
done

# move gxint and cernlib scripts back
mv cernlib%{?compiler} cernlib-static%{?compiler} gxint%{?compiler} kuipc%{?compiler}\
   %{buildroot}%{_bindir}/

# add a link to pawX11 and dzeX11 from {_libdir}/cernlib/{verdir}/bin
%{__install} -d -m755 %{buildroot}%{_libdir}/cernlib/%{verdir}/bin/
%{__ln_s} %{_bindir}/pawX11%{?compiler_string} %{buildroot}%{_libdir}/cernlib/%{verdir}/bin/pawX11%{?compiler_string}
%{__ln_s} %{_bindir}/dzeX11%{?compiler_string} %{buildroot}%{_libdir}/cernlib/%{verdir}/bin/dzeX11%{?compiler_string}

# fix utilities names in calling scripts
sed -i -e 's:$GDIR/paw$drv:$GDIR/paw$drv%{compiler_string}:' %{buildroot}%{_bindir}/paw%{compiler_string}
sed -i -e 's:$GDIR/dze$drv:$GDIR/dze$drv%{compiler_string}:' %{buildroot}%{_bindir}/dzedit%{compiler_string}

%if 0%{?utils_compiler}
   %{__ln_s} %{_bindir}/pawX11 %{buildroot}%{_libdir}/cernlib/%{verdir}/bin/pawX11
   %{__ln_s} %{_bindir}/dzeX11 %{buildroot}%{_libdir}/cernlib/%{verdir}/bin/dzeX11
%endif

# to preserve symlinks and timestamps
(cd lib && tar cf - *.a) | (cd %{buildroot}%{_libdir}/cernlib/%{verdir}/lib && tar xf -)
(cd shlib && tar cf - *.so*) | (cd %{buildroot}%{_libdir}/cernlib/%{verdir}/lib && tar xf -)

rm %{buildroot}%{_bindir}/mkdirhier*

# add links for cfortran header files in the top include directory
pushd %{buildroot}%{_includedir}/cernlib/%{version}
for file in *.h; do
   %{__ln_s} ../$file cfortran/$file
done
%{__ln_s} gxint321.f gxint.f
popd

cd src
# install include directories for the cernlib libraries
base_include=%{buildroot}%{_includedir}/cernlib/%{version}
for dir in `cat ../../*/debian/add-ons/includelist.txt`; do
   basedir=`basename $dir`
   rm -rf $base_include/$basedir
   cp -Rp $dir $base_include/
done

# patching of header files changed their timestamp. However, since it 
# is much more complicated, instead of finding the real timestamp, the 
# source file timestamp is used

pushd $base_include
for file in cfortran.h comis.h cspack.h geant315.h geant321.h gen.h graflib.h \
 gxint321.f hbook.h jetset74.h kernlib.h kuip.h  lapack.h lepto62.h minuit.h \
 packlib.h paw.h zebra.h \
 comis/cspar.inc comis/cstab64.inc comis/mdpool.* comis/mdsize.h \
 cspack/hcntpar.inc \
 geant321/gcnmec.inc \
 hbook/hcdire.inc hbook/hcntpar.inc \
 kuip/kstring.h \
 ntuple/cern_types.h ntuple/c_hcntpar.h \
 paw/pawsiz.inc; do
# source file timestamps are not the same for the packages corresponding 
# with different compilers. So we use the src/log/tag file.
   #touch -r {SOURCE0} $file
   touch -r %{_builddir}/%{name}-%{version}/%{version}/src/log/tag $file
done
popd

# substitute the path in installed eufiles.inc, not in in-source
# file, because in-source the relative path is required for test
touch -r %{buildroot}%{_includedir}/cernlib/%{version}/eurodec/eufiles.inc __eufiles_timestamp
sed -i -e 's:eurodec.dat:%{_datadir}/cernlib/%{version}/eurodec.dat:' \
  %{buildroot}%{_includedir}/cernlib/%{version}/eurodec/eufiles.inc
touch -r __eufiles_timestamp %{buildroot}%{_includedir}/cernlib/%{version}/eurodec/eufiles.inc
rm __eufiles_timestamp

# install the tree.h and converter.h include files redirecting to 
# system headers
%{__install} -p -m644 ../../paw-2.14.04.dfsg.2/debian/add-ons/paw/*.h %{buildroot}%{_includedir}/cernlib/%{version}/paw

%{__install} -d -m755 %{buildroot}/etc/ld.so.conf.d
echo %{_libdir}/cernlib/%{verdir}/lib > %{buildroot}/etc/ld.so.conf.d/cernlib-%{verdir}-%{_arch}.conf

%{__install} -d -m755 %{buildroot}/%{_mandir}/man1
%{__install} -p -m644 man/man1/cernlib.1 %{buildroot}/%{_mandir}/man1/cernlib-static.1
for cernlib_manpage in cernlib.1 kxterm.1 dzedit.1 dzeX11.1 kuipc.1 kuesvr.1 zftp.1 gxint.1 paw.1 paw++.1 pawX11.1 yexpand.1 ypatchy.1 ny*.1; do
%{__install} -p -m644 ../../*/debian/add-ons/manpages/$cernlib_manpage %{buildroot}/%{_mandir}/man1/
done

%{__install} -d -m755 %{buildroot}/%{_mandir}/man8
for cernlib_manpage in pawserv.8 zserv.8; do
%{__install} -p -m644 ../../*/debian/add-ons/manpages/$cernlib_manpage %{buildroot}/%{_mandir}/man8/
done

%{__install} -d -m755 %{buildroot}/%{_datadir}/X11/app-defaults
%{__install} -p -m644 ../../*/debian/add-ons/app-defaults/* %{buildroot}/%{_datadir}/X11/app-defaults/

sed -e 's/Exec=paw++/Exec=paw++%{?compiler_string}/' -e 's/Paw++/Paw++%{?compiler_string}/' \
 ../../paw*/debian/add-ons/misc/paw++.desktop > paw++%{?compiler_string}.desktop
desktop-file-install --vendor="fedora"               \
  --dir=%{buildroot}/%{_datadir}/applications         \
  --delete-original                                   \
  paw++%{?compiler_string}.desktop

%if 0%{?utils_compiler}
desktop-file-install --vendor="fedora"               \
  --dir=%{buildroot}/%{_datadir}/applications         \
  ../../paw*/debian/add-ons/misc/paw++.desktop
%endif

%{__install} -d -m755 %{buildroot}/%{_datadir}/pixmaps
%{__install} -d -m755 %{buildroot}/%{_datadir}/icons/hicolor/{48x48,32x32}/apps/
%{__install} -p -m644 ../../*/debian/add-ons/icons/*.xpm %{buildroot}/%{_datadir}/pixmaps/
%{__install} -p -m644 ../../paw*/debian/add-ons/icons/paw32x32.xpm %{buildroot}/%{_datadir}/icons/hicolor/32x32/apps/paw.xpm
%{__install} -p -m644 ../../paw*/debian/add-ons/icons/paw48x48.xpm %{buildroot}/%{_datadir}/icons/hicolor/48x48/apps/paw.xpm

find %{buildroot}%{_includedir}/cernlib/%{version} -name 'Imakefile*' -exec rm \{\} \;
rm %{buildroot}%{_includedir}/cernlib/%{version}/ntuple/*.c


%check
CERN=$RPM_BUILD_DIR/%{name}-%{version}
CERN_LEVEL=%{version}
CERN_ROOT=$CERN/$CERN_LEVEL
CVSCOSRC=$CERN/$CERN_LEVEL/src
PATH=$CERN_ROOT/bin:$PATH

export CERN
export CERN_LEVEL
export CERN_ROOT
export CVSCOSRC
export PATH

export LD_LIBRARY_PATH=$CERN_ROOT/shlib/

# cannot make out-of build test because of the data files
cd $CERN_ROOT/build

# no test in code_motif paw_motif scripts patchy pawlib
%if  %{?rhel}%{!?rhel:0} >= 7
test_dirs='graflib mclibs kernlib mathlib packlib phtools'
%else
test_dirs='graflib mclibs kernlib mathlib packlib phtools geant321'
%endif

rm -f __dist_failed_builds

for dir in $test_dirs; do
make -C $dir test || echo $dir >> __dist_failed_builds
done

if [ -f __dist_failed_builds ]; then
echo "DIST TESTS FAILED"
cat __dist_failed_builds
fi


%clean
rm -rf %{buildroot}

%post -p /sbin/ldconfig

%postun -p /sbin/ldconfig

%post -n %{?compat}paw%{?compiler_string}
touch --no-create %{_datadir}/icons/hicolor || :
%{_bindir}/gtk-update-icon-cache --quiet %{_datadir}/icons/hicolor || :

%postun -n %{?compat}paw%{?compiler_string}
touch --no-create %{_datadir}/icons/hicolor || :
%{_bindir}/gtk-update-icon-cache --quiet %{_datadir}/icons/hicolor || :

%if 0%{?utils_compiler}
%post -n paw
touch --no-create %{_datadir}/icons/hicolor || :
%{_bindir}/gtk-update-icon-cache --quiet %{_datadir}/icons/hicolor || :

%postun -n paw
touch --no-create %{_datadir}/icons/hicolor || :
%{_bindir}/gtk-update-icon-cache --quiet %{_datadir}/icons/hicolor || :
%endif

%files
%defattr(-,root,root,-)
%doc cernlib.README debian-copyright
%doc geant321-3.21.14.dfsg/debian/debhelper/geant321.README.debian 
%doc mclibs-2006.dfsg.2/debian/debhelper/libpdflib804-2-dev.README.debian
%doc mclibs-2006.dfsg.2/debian/debhelper/montecarlo-base.README.debian
%doc cernlib-2006.dfsg.2/debian/add-ons/vim/
/etc/ld.so.conf.d/*
%dir %{_libdir}/cernlib/
%dir %{_libdir}/cernlib/%{verdir}
%dir %{_libdir}/cernlib/%{verdir}/lib
%dir %{_libdir}/cernlib/%{verdir}/bin
%{_libdir}/cernlib/%{verdir}/lib/*.so.*
%{_datadir}/cernlib/

# the utils and devel are separated to have the possibility to install
# parallel versions of the library
%files devel
%defattr(-,root,root,-)
%doc cfortran paw-*/debian/add-ons/misc/comis-64bit-example.F
%{_libdir}/cernlib/%{verdir}/lib/*.so
%{_includedir}/cernlib/
%{_datadir}/aclocal/cernlib.m4

%files static
%defattr(-,root,root,-)
%{_libdir}/cernlib/%{verdir}/lib/*.a

%files utils
%defattr(-,root,root,-)
%doc cernlib-%{verdir}.csh cernlib-%{verdir}.sh
%{_bindir}/cernlib*%{?compiler}
# %%{_sysconfdir}/profile.d/cernlib-%%{verdir}.sh
# %%{_sysconfdir}/profile.d/cernlib-%%{verdir}.csh
%{_mandir}/man1/cernlib*.1*


%files -n %{?compat}geant321%{?compiler}
%defattr(-,root,root,-)
%{_bindir}/gxint%{?compiler}
%{_datadir}/X11/app-defaults/*Geant++
%{_mandir}/man1/gxint.1*

%files -n %{?compat}kuipc%{?compiler}
%defattr(-,root,root,-)
%{_bindir}/kuipc%{?compiler}
%{_mandir}/man1/kuipc.1*

%files -n %{?compat}paw%{?compiler_string}
%defattr(-,root,root,-)
%doc paw.README
%{_bindir}/paw++%{?compiler_string}
%{_bindir}/paw%{?compiler_string}
%{_bindir}/pawX11%{?compiler_string}
%{_bindir}/pawX11.dynamic%{?compiler_string}
%{_bindir}/paw++.dynamic%{?compiler_string}
# paw doesn't explicitly depend on the main package, so it owns the dirs
%dir %{_libdir}/cernlib/%{verdir}
%dir %{_libdir}/cernlib/%{verdir}/bin
%{_libdir}/cernlib/%{verdir}/bin/pawX11%{?compiler_string}
%{_datadir}/X11/app-defaults/*Paw++
%{_datadir}/icons/hicolor/
%{_mandir}/man1/paw*.1*
%{_datadir}/applications/*paw++%{?compiler_string}.desktop
%{_datadir}/pixmaps/paw*.xpm

%files -n cernlib-packlib%{?compiler_string}
%defattr(-,root,root,-)
%doc cernlib-*/debian/debhelper/zftp.README.debian
%{_bindir}/cdbackup%{?compiler_string}
%{_bindir}/cdserv%{?compiler_string}
%{_bindir}/dzedit%{?compiler_string}
# packlib doesn't explicitly depend on the main package, so it owns the dirs
%dir %{_libdir}/cernlib/%{verdir}
%dir %{_libdir}/cernlib/%{verdir}/bin
%{_libdir}/cernlib/%{verdir}/bin/dzeX11%{?compiler_string}
%{_bindir}/dzeX11%{?compiler_string}
%{_bindir}/fatmen%{?compiler_string}
%{_bindir}/fatsrv%{?compiler_string}
%{_bindir}/kuesvr%{?compiler_string}
%{_bindir}/zserv%{?compiler_string}
%{_bindir}/cdmake%{?compiler_string}
%{_bindir}/fatnew%{?compiler_string}
%{_bindir}/pawserv%{?compiler_string}
%{_bindir}/cdmove%{?compiler_string}
%{_bindir}/fatback%{?compiler_string}
%{_bindir}/fatsend%{?compiler_string}
%{_bindir}/hepdb%{?compiler_string}
%{_bindir}/kxterm%{?compiler_string}
%{_bindir}/zftp%{?compiler_string}
%{_datadir}/X11/app-defaults/KXterm
%{_datadir}/pixmaps/kxterm*.xpm
%{_mandir}/man1/kxterm.1* 
%{_mandir}/man1/dze*.1*
%{_mandir}/man1/kuesvr.1* 
%{_mandir}/man1/zftp.1*
%{_mandir}/man8/*.8*

%files -n %{?compat}patchy%{?compiler_string}
%defattr(-,root,root,-)
%{_bindir}/fcasplit%{?compiler_string}
%{_bindir}/nycheck%{?compiler_string}
%{_bindir}/nydiff%{?compiler_string}
%{_bindir}/nyindex%{?compiler_string}
%{_bindir}/nylist%{?compiler_string}
%{_bindir}/nymerge%{?compiler_string}
%{_bindir}/nypatchy%{?compiler_string}
%{_bindir}/nyshell%{?compiler_string}
%{_bindir}/nysynopt%{?compiler_string}
%{_bindir}/nytidy%{?compiler_string}
%{_bindir}/yexpand%{?compiler_string}
%{_bindir}/ycompar%{?compiler_string}
%{_bindir}/yedit%{?compiler_string}
%{_bindir}/yfrceta%{?compiler_string}
%{_bindir}/yindex%{?compiler_string}
%{_bindir}/yindexb%{?compiler_string}
%{_bindir}/ylist%{?compiler_string}
%{_bindir}/ylistb%{?compiler_string}
%{_bindir}/ypatchy%{?compiler_string}
%{_bindir}/ysearch%{?compiler_string}
%{_bindir}/yshift%{?compiler_string}
%{_bindir}/ytobcd%{?compiler_string}
%{_bindir}/ytobin%{?compiler_string}
%{_bindir}/ytoceta%{?compiler_string}
%{_mandir}/man1/ny*.1*
%{_mandir}/man1/yexpand.1* 
%{_mandir}/man1/ypatchy.1*

%if 0%{?utils_compiler}
%files -n paw
%defattr(-,root,root,-)
%doc paw.README
%{_bindir}/paw++
%{_bindir}/paw
%{_bindir}/pawX11
%{_bindir}/pawX11.dynamic
%{_bindir}/paw++.dynamic
# paw doesn't explicitly depend on the main package, so it owns the dirs
%dir %{_libdir}/cernlib/%{verdir}
%dir %{_libdir}/cernlib/%{verdir}/bin
%{_libdir}/cernlib/%{verdir}/bin/pawX11
%{_datadir}/X11/app-defaults/*Paw++
%{_datadir}/icons/hicolor/
%{_mandir}/man1/paw*.1*
%{_datadir}/applications/*paw++.desktop
%{_datadir}/pixmaps/paw*.xpm

%files -n cernlib-packlib
%defattr(-,root,root,-)
%doc cernlib-*/debian/debhelper/zftp.README.debian
%{_bindir}/cdbackup
%{_bindir}/cdserv
%{_bindir}/dzedit
# packlib doesn't explicitly depend on the main package, so it owns the dirs
%dir %{_libdir}/cernlib/%{verdir}
%dir %{_libdir}/cernlib/%{verdir}/bin
%{_libdir}/cernlib/%{verdir}/bin/dzeX11
%{_bindir}/dzeX11
%{_bindir}/fatmen
%{_bindir}/fatsrv
%{_bindir}/kuesvr
%{_bindir}/zserv
%{_bindir}/cdmake
%{_bindir}/fatnew
%{_bindir}/pawserv
%{_bindir}/cdmove
%{_bindir}/fatback
%{_bindir}/fatsend
%{_bindir}/hepdb
%{_bindir}/kxterm
%{_bindir}/zftp
%{_datadir}/X11/app-defaults/KXterm
%{_datadir}/pixmaps/kxterm*.xpm
%{_mandir}/man1/kxterm.1* 
%{_mandir}/man1/dze*.1*
%{_mandir}/man1/kuesvr.1* 
%{_mandir}/man1/zftp.1*
%{_mandir}/man8/*.8*

%files -n patchy
%defattr(-,root,root,-)
%{_bindir}/fcasplit
%{_bindir}/nycheck
%{_bindir}/nydiff
%{_bindir}/nyindex
%{_bindir}/nylist
%{_bindir}/nymerge
%{_bindir}/nypatchy
%{_bindir}/nyshell
%{_bindir}/nysynopt
%{_bindir}/nytidy
%{_bindir}/yexpand
%{_bindir}/ycompar
%{_bindir}/yedit
%{_bindir}/yfrceta
%{_bindir}/yindex
%{_bindir}/yindexb
%{_bindir}/ylist
%{_bindir}/ylistb
%{_bindir}/ypatchy
%{_bindir}/ysearch
%{_bindir}/yshift
%{_bindir}/ytobcd
%{_bindir}/ytobin
%{_bindir}/ytoceta
%{_mandir}/man1/ny*.1*
%{_mandir}/man1/yexpand.1* 
%{_mandir}/man1/ypatchy.1*
%endif

%changelog
* Tue Dec 1 2020 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> 2006-43
 - Adding CentOS7/CentOS8 support

* Wed Oct 14 2020 Jan Musinsky <musinsky@gmail.com> - 2006-42
- glibc-2.32 support, Fedora 33

* Sat May 23 2020 Jan Musinsky <musinsky@gmail.com> - 2006-41
- gcc10 support, Fedora 32

* Mon May 06 2019 Jan Musinsky <musinsky@gmail.com> - 2006-40
- unofficial release 40 after 9 years (previous last official release 35)
- many fixies after long time
- correct sed with optflags
- fix MXLENG
- replace strfromd/strfromd9 function name
- fix printf with flis_name
- add libnsl2-devel for newer distributions
- replace -O2 (or -O3) by -O gcc flags for older distributions
- cernlib package is now compiling (but not guaranteed to work properly)

* Mon Feb 08 2016 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> 2006-36
- Disabled tests for geant321

* Wed May 05 2010 Jon Ciesla <limb@jcomserv.net> 2006-35
- Apply debian cernlib 2006.dfsg.2-14 patchset.

* Thu Oct  1 2009 Hans de Goede <hdegoede@redhat.com> 2006-34
- Fix FTBFS

* Fri Jul 24 2009 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2006-33
- Rebuilt for https://fedoraproject.org/wiki/Fedora_12_Mass_Rebuild

* Mon Feb 23 2009 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2006-32
- Rebuilt for https://fedoraproject.org/wiki/Fedora_11_Mass_Rebuild

* Wed Oct  1 2008 Patrice Dumas <pertusus at free.fr> 2006-31
- correct 702-patch-Imakefiles-for-packlib-mathlib to apply with fuzz 0

* Thu Jun 26 2008 Patrice Dumas <pertusus at free.fr> 2006-30
- debian paw 2.14.04.dfsg.2-6 patcheset

* Sat Apr  5 2008 Patrice Dumas <pertusus at free.fr> 2006-29
- debian cernlib 2006.dfsg.2-13 patchset

* Tue Mar 25 2008 Patrice Dumas <pertusus at free.fr> 2006-28
- new cernlib, paw and geant321 debian patchesets
- use regular p5boot and patchy build 

* Tue Feb 26 2008 Patrice Dumas <pertusus at free.fr> 2006-27
- new mclibs and geant321 patchsets

* Sun Feb 24 2008 Patrice Dumas <pertusus at free.fr> 2006-25
- new cernlib and paw patchsets

* Sun Jan 13 2008 Patrice Dumas <pertusus at free.fr> 2006-23
- new cernlib debian patcheset

* Tue Jan  8 2008 Patrice Dumas <pertusus at free.fr> 2006-22
- new debian patchesets

* Mon Dec 31 2007 Patrice Dumas <pertusus at free.fr> 2006-21
- no --build-id for EL-5

* Tue Oct 30 2007 Patrice Dumas <pertusus at free.fr> 2006-20
- don't use the same spec for epel4
- always ship the packages with compiler suffix. This is needed for 
  proper upgrade path as soon as such a package has been ever shipped
- fix timestamps

* Mon Aug 27 2007 Patrice Dumas <pertusus at free.fr> 2006-19
- pass --build-id when linking, this is needed for debuginfo generation
- use g77 binaries, paw with gfortran is broken on x86_64 (#241416)
- add virtual provides for devel, utils and static packages, packages 
  (like kuipc) may need the cernlib, but accept g77 or gfortran compiled 
  cernlib
- don't set the environment. It only hurts parallel installations.

* Mon Aug 27 2007 Patrice Dumas <pertusus at free.fr> 2006-18
- correct license
- use the right debian patch source

* Sun Aug  5 2007 Patrice Dumas <pertusus at free.fr> 2006-17
- remove old patches
- remove 'binaries' from the main lib summary

* Wed Jul 25 2007 Patrice Dumas <pertusus at free.fr> 2006-16
- add rhel conditionals
- bring back support for monolithic X
- don't build anything in parallel

* Sun May 27 2007 Patrice Dumas <pertusus at free.fr> 2006-15
- split out static libraries, as per FESCO decision

* Sat May 26 2007 Patrice Dumas <pertusus at free.fr> 2006-14
- add link to $CERN_ROOT/bin/dzeX11
- use %%{name} in the Requires
- Provides %%{name}-static
- allow utilities to have another suffix than libs. This allows to 
  have the utilities compiled with g77 and the default libraries compiled
  with gfortran

* Wed May 23 2007 Patrice Dumas <pertusus at free.fr> 2006-13
- remove reference to %%{_libdir} and X11R6 path in cernlib script
- apply cernlib-static script patch
- add packlib-lesstif only if Motif driver is selected. This driver
  is selected in the default case for geant321 and pawlib
- build and ship the graflib utilities

* Mon May 14 2007 Patrice Dumas <pertusus at free.fr> 2006-11
- disable the same tests on ppc64 than on x86_64

* Mon May 14 2007 Patrice Dumas <pertusus at free.fr> 2006-10.1
- packlib/{ffread,hbook} test segfaults on ppc64

* Sun May 13 2007 Patrice Dumas <pertusus at free.fr> 2006-9
- add a compat prefix when built with g77
- new debian patcheset

* Tue Apr 24 2007 Patrice Dumas <pertusus at free.fr> 2006-8
- change the soname with gfortran after coordination with debian maintainer

* Tue Apr 24 2007 Patrice Dumas <pertusus at free.fr> 2006-7
- use real cernlib lib location in cernlib debian script, don't assume
  that they are in %%_prefix, it is not the case on fedora

* Mon Apr 23 2007 Patrice Dumas <pertusus at free.fr> 2006-6
- package compiled with g77 is parallel installable with gfortran 
  compiled package

* Sun Apr 22 2007 Patrice Dumas <pertusus at free.fr> 2006-5.4
- packlib/ffread, packlib/hbook, packlib/kuip, packlib/zbook, packlib/zebra 
  tests fail on x86_64, exclude the tests on this arch
- kernbit/kernnum test fails on x86_64, exclude the test on this arch

* Sun Apr 22 2007 Patrice Dumas <pertusus at free.fr> 2006-3
- geant321 test fails on x86_64, exclude the test on this arch

* Sun Apr 22 2007 Patrice Dumas <pertusus at free.fr> 2006-2
- don't do a parallel build for npatchy

* Fri Apr 13 2007 Patrice Dumas <pertusus at free.fr> 2006-1
- update to cernlib 2006
- build with gfortran
- use system Xbae and Xaw
- ship man pages, app-defaults, icons and paw++ desktop file (from debian)
- run tests
- use optflags in patchy4
- bootstrap npatchy with p5boot (instead of using patchy4)
- fix npatchy build

* Tue Dec 19 2006 Patrice Dumas <pertusus at free.fr> 2005-27
- add support for gfortran (not enabled, lshift missing in fedora gfortran)
- use fedora compiler flags when possible
- use %%{optflags} and %%{buildroot}

* Mon Sep 11 2006 Patrice Dumas <pertusus at free.fr> 2005-26
- update to newer debian patchsets (paw and cernlib)
- remove gfortran related patches integrated in the debian 
  patchsets

* Fri Sep  1 2006 Patrice Dumas <pertusus at free.fr> 2005-25
- update to newer source split debian patchsets
- don't set the exec bits on file installed in /etc/profile.d/

* Thu Aug 31 2006 Patrice Dumas <pertusus at free.fr> - 2005-22
- add -q to %%setup
- rebuild against lesstif for FC6

* Wed May 24 2006 Patrice Dumas <pertusus at free.fr> - 2005-21
- compile paw statically against the cernlib (paw binaries dynamically 
  compiled against the cernlib are still shiped). Fix 192866

* Wed May 17 2006 Patrice Dumas <pertusus at free.fr> - 2005-20
- use new debian patchset. Fix 191631

* Thu Apr 13 2006 Patrice Dumas <pertusus at free.fr> - 2005-19
- add a patch to yexpand, to avoid using $HOME.

* Thu Apr 13 2006 Patrice Dumas <pertusus at free.fr> - 2005-17
- npatchy don't build on ppc.

* Wed Apr 12 2006 Patrice Dumas <pertusus at free.fr> - 2005-16
- unpack patchy offline because the files are within an unformatted
  fortran file which won't be right on all the arches.

* Tue Apr 11 2006 Patrice Dumas <pertusus at free.fr> - 2005-15.1
- add conditionals in spec to have only one for all fedora versions.

* Tue Apr 11 2006 Patrice Dumas <pertusus at free.fr> - 2005-14
- add patchy version 4 and build cernlib patchy. From Mattias Ellert.
- update to newer debian patchset

* Thu Feb 16 2006 Patrice Dumas <pertusus at free.fr> - 2005-13
- rebuild for fc5

* Tue Jan 17 2006 Patrice Dumas <pertusus at free.fr> - 2005-12.1
- attempt a rebuild against newer openmotif

* Tue Dec 20 2005 Patrice Dumas <pertusus at free.fr> - 2005-11.4
- add a symlink from /usr/lib/cernlib/2005/bin/pawX11 to /usr/bin/pawX11
- fix gxint

* Tue Dec 20 2005 Patrice Dumas <pertusus at free.fr> - 2005-10
- add file in /etc/ld.so.conf.d required for dynamic linking

* Wed Dec 14 2005 Patrice Dumas <pertusus at free.fr> - 2005-9.1
- use new debian patchset

* Fri Dec  9 2005 Patrice Dumas <pertusus at free.fr> - 2005-8.1
- use new debian patchset
- enable 64 bit fixes patch

* Fri Dec  2 2005 Patrice Dumas <pertusus at free.fr> - 2005-7
- use updated beta debian patchset
- remove the BSD in the licence because there is no library nor binary
  under a BSD licence and someone could get the idea that there is 
  some dual BSD/GPL licenced binaries or libraries. The LGPL is kept
  because of cfortran

* Tue Nov 29 2005 Patrice Dumas <pertusus at free.fr> - 2005-6.1
- update with newer debian patchset for cernlib, fix licence issues
- don't use the include.tar.gz source, instead get include files from
  the source files
- build shared libraries
- simplify the scripts modifications
- add BuildRequires: libXau-devel to workaround #173530

* Tue Nov 15 2005 Patrice Dumas <pertusus at free.fr> - 2005-4
- add a .csh file
- correct defaults in cernlib scripts

* Sun Sep 11 2005 Patrice Dumas <pertusus at free.fr> - 2005-3
- add rm -rf %%{buildroot}

* Thu Sep  1 2005 Patrice Dumas <pertusus at free.fr> - 2005-2
- minor update of cernlib.README
- modify the cernlib script patch to use /usr/X11R6/lib64 if existing

* Thu Jun 09 2005 Patrice Dumas <pertusus at free.fr> - 2005-1
- full rewrite of the spec file from Scientific Linux
- use some ideas and many patches from debian cernlib
