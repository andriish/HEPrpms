%undefine _debugsource_packages
#Manually set flags in CMake

Summary:  Automated generation and computation of EW and QCD amplitudes in Fortran95
Name: recola
Version: 1.5.0
Release: 1%{?dist}
License: GPLv3
Prefix: %{_prefix}
Source: https://recola.hepforge.org/downloader?f=recola-%{version}.tar.gz
URL:   https://recola.gitlab.io/recola2/
BuildRequires: collier  gcc-c++ 
Requires: collier
BuildRequires: cmake >= 3.4.3
%if 0%{?rhel} || 0%{?fedora}
BuildRequires:  gcc-gfortran
%endif
%if 0%{?suse_version}
BuildRequires:  gcc-fortran
%endif

%description
Recola is a Fortran95 computer program for the automated generation and 
numerical computation of EW and QCD amplitudes in the Standard Model at 
next-to-leading order. 
%prep 
%setup -q 

%build
%cmake   -DCMAKE_SKIP_RPATH=ON  -Dcollier_path=/usr/share/cmake -DSYSCONFIG_INSTALL_DIR=%{_prefix}/share/cmake/ -DCMAKE_POLICY_VERSION_MINIMUM=3.5
%cmake_build 

%install
%cmake_install

%files
%defattr(-,root,root)
%{_includedir}/*
%{_prefix}/%_lib/*
%{_prefix}/share/cmake/*
%post 
ldconfig 

%changelog
* Thu Nov 12 2026 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de>
- Update to 1.5.0
- Use -Dcollier_path instead of -DCOLLIER_LIB_PATH, the CMake files of 1.5.0 search collier only in the given path
* Thu Nov 23 2017 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - Initial
