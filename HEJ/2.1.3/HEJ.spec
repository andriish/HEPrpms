%define major       0
%define libname     HEJ
%define sharename   HEJ-common
%define openmpiname HEJ-openmpi
%define libnamedev  HEJ-devel
%define develnamestatic  HEJ-static-devel
%define _unpackaged_files_terminate_build 0
%global _missing_build_ids_terminate_build 0
# --enable-analysis  is not set


Name:           HEJ
Version:        2.1.3
Release:        1%{?dist}
License:        GPLv2
Url:              https://sherpa.hepforge.org
Source0:          https://hej.hepforge.org/downloads/%{name}_%{version}.tar.gz
#Source0:          {name}_{version}.tar.gz
Summary:          Multipurpose 
#Patch0:           patch-SHERPA-MC-0.txt

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} 
BuildRequires:    gcc-c++ root   Rivet Rivet-devel cmake
BuildRequires:    root-core root HepMC3-rootIO-devel HepMC3-devel HepMC3-search HepMC3-search-devel  HepMC3-rootIO HepMC3  HepMC HepMC-devel 
BuildRequires:    autoconf automake libtool root-physics libzip-devel 
BuildRequires:    lhapdf-devel fastjet fastjet-devel  YODA-devel Rivet-devel zlib zlib-devel
Requires:         HepMC lhapdf  HepMC3-rootIO HepMC3  root root-core root Rivet YODA fastjet  zlib
Requires:         HepMC3-search
BuildRequires:    texinfo git
BuildRequires:    qcdloop-devel boost-devel highfive-devel clhep-devel yaml-cpp-devel
%endif
%if 0%{?suse_version}
BuildRequires:    gcc-fortran gcc-c++ pythia-devel  Rivet Rivet-devel cmake
BuildRequires:    root6-libs root6-devel root6-config root6 HepMC3-devel  libHepMC4 HepMC2-devel 
BuildRequires:    autoconf automake libtool 
BuildRequires:    LHAPDF-devel fastjet fastjet-devel  YODA-devel Rivet-devel zlib zlib-devel
Requires:         libHepMC4 libLHAPDF libHepMC4  sqlite root6 root6-libs Rivet YODA lfastjet  zlib
BuildRequires:    texinfo git
BuildRequires:    qcdloop-devel boost-devel highfive-devel clhep-devel yaml-cpp-devel
%endif


%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} == 8
BuildRequires: python3-distutils-extra
%endif
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
BuildRequires:    python3 python3-devel 
Requires:         python3

%endif

%if 0%{?suse_version}
BuildRequires:    python3 python3-devel
Requires:         python3
%endif


Prefix: %{_prefix}

%description
HEJ Multipurpose Multipurpose Multipurpose Multipurpose 


%package  devel
Summary:        Libraries and headers for %{name}
Provides:       %{name}-devel = %{version}-%{release}

%description devel
%{libnamedev} contains the libraries and header files needed to
develop programs which make use of %{name}.
The library documentation is available on header files.

%package  common
Summary:        Libraries and headers for %{name}

Provides:       %{name}-common = %{version}-%{release}

%description common
Contains the common files and header files needed to
develop programs which make use of %{name}.
The library documentation is available on header files.






%prep

%setup -q -c HEJ-2.1.3

%build
%cmake -DEXCLUDE_ROOT=OFF -DEXCLUDE_HepMC=TRUE 


%cmake_build











%install
%cmake_install



%files 
%doc AUTHORS README COPYING
%{_libdir}/SHERPA-MC/*
%{_bindir}/*
%{_sysconfdir}/ld.so.conf.d/%{name}-%{_arch}.conf


%files  devel
%{_includedir}/SHERPA-MC/*




%changelog
* Fri Mar 10 2023 Andrii Verbytskyi 2.19.01
- Initial

