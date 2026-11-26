%global commit      54b9b5e2ca57c3eb465f0f0112fdc8fd590ac8d4
%global shortcommit %(c=%{commit}; echo ${c:0:7})
%global snapdate    20261126

# the libraries are static archives of old Fortran code: no LTO objects in them
%global _lto_cflags %{nil}

Name:           JADE
Version:        2026.11.0
Release:        0.1.%{snapdate}git%{shortcommit}%{?dist}
Summary:        Software of the JADE experiment
# no licence file upstream; individual files keep the licence notices they came with
License:        LicenseRef-Callaway-Freely-redistributable
URL:            https://github.com/andriish/JADE
Source0:        https://github.com/andriish/JADE/archive/%{commit}/JADE-%{commit}.tar.gz

BuildRequires:  cmake
BuildRequires:  make
BuildRequires:  gcc-c++
BuildRequires:  gcc-gfortran
BuildRequires:  blas-devel
BuildRequires:  lapack-devel
BuildRequires:  libX11-devel
BuildRequires:  libXt-devel
BuildRequires:  libXmu-devel
BuildRequires:  libXext-devel
BuildRequires:  libXaw-devel
BuildRequires:  libXau-devel
BuildRequires:  libxcb-devel
BuildRequires:  libXdmcp-devel
BuildRequires:  motif-devel
BuildRequires:  cernlib-devel
BuildRequires:  cernlib-static
BuildRequires:  HepMC3-devel
BuildRequires:  root-core
BuildRequires:  root-hist
BuildRequires:  root-tree
BuildRequires:  root-io
BuildRequires:  root-net
BuildRequires:  root-graf
BuildRequires:  root-graf-gpad
BuildRequires:  root-graf3d
BuildRequires:  root-matrix
BuildRequires:  root-mathcore

%description
The software of the JADE experiment at PETRA (DESY): the reconstruction,
simulation and analysis programs (jadesoft, including the event display
jadez and the old Monte Carlo generators), the converter of HepMC3 events
to the JADE format, jtuple, fptobos/unpjad and jade_h2root.

%package        devel
Summary:        Static libraries and CMake configuration of the JADE software
Requires:       %{name}%{?_isa} = %{version}-%{release}
Requires:       cernlib-devel
Requires:       cernlib-static
Requires:       motif-devel
Requires:       blas-devel
Requires:       lapack-devel
Provides:       %{name}-static = %{version}-%{release}

%description    devel
The static libraries of the JADE software and the CMake package
configuration files JADEConfig.cmake and JADESOFTConfig.cmake.

%prep
%autosetup -n JADE-%{commit}

%build
%cmake -DJADE_TOOLCHAIN=GNU \
       -DCMAKE_INSTALL_LIBDIR=%{_lib} \
       -DCMAKE_VERBOSE_MAKEFILE:BOOL=ON
%cmake_build

%install
%cmake_install

%files
%doc README.md
%{_bindir}/*
# unversioned shared libraries of jtuple, linked by jzread and jtjob
%{_libdir}/libjtlib.so
%{_libdir}/libjtzread.so
%{_libdir}/libntuple.so
%dir %{_datadir}/JADESOFT
%{_datadir}/JADESOFT/lumi.dat
%{_datadir}/JADESOFT/cal
%{_datadir}/JADESOFT/examples
%{_datadir}/JADESOFT/F22BOW.HELP.DATASET
%{_datadir}/JADESOFT/F11OLS.GRAPHICS.PROFILE.MACROS
%{_datadir}/JADESOFT/cj623inp.dat
%{_datadir}/JADESOFT/cj623tab.dat

%files devel
%{_libdir}/*.a
%{_datadir}/JADESOFT/cmake
%{_datadir}/JADE

%changelog
* Thu Nov 26 2026 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> - 2026.11.0-0.1.20261126git54b9b5e
- Initial package, snapshot of the master branch (commit 54b9b5e)
