%undefine _debugsource_packages
#Manually set flags in CMake

Summary:  A Fortran95 computer program for the automated generation and numerical computation of EW and QCD amplitudes
Name: recola
Version: 1.4.1
Release: 2%{?dist}
License: GPLv3
Prefix: %{_prefix}
Source: https://www.hepforge.org/archive/recola/recola-%{version}.tar.gz
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
%cmake    -DCOLLIER_LIB_PATH=/usr/share/cmake -DSYSCONFIG_INSTALL_DIR=%{_prefix}/share/cmake/
%cmake_build 

%install
%cmake_install

%files
%defattr(-,root,root)
%{_includedir}/*
%{_prefix}/%_lib/*
%{_prefix}/share/cmake/*
%clean
rm -rf %{buildroot}

%post 
ldconfig 

%changelog             
* Thu Nov 23 2017 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - Initial
