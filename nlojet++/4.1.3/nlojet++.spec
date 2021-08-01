Name:           nlojet++
Version:        4.1.3
Release:        2%{?dist}
License:        GPL
Url:            http://desy.de/~znagy
Source0:        http://desy.de/~znagy/hep-programs/nlojet++/%{name}-%{version}.tar.gz
Prefix: %{_prefix}
Summary:        A C++ program for calculating LO and NLO order cross sections
%if 0%{?rhel} || 0%{?fedora}
BuildRequires:  gcc-gfortran gcc gcc-c++
Requires: libgfortran
%endif
%if 0%{?suse_version}
BuildRequires:  gcc-fortran gcc gcc-c++
Requires: gcc-fortran
%endif


%description
NLOJET++ is a C++ program for calculating LO and NLO order cross section. 
The core library computes the Catani-Seymour dipole subtraction method 
(actually a slightly modified version has been coded and the original 
dipole scheme is its special case) at very abstract level and the 
various processes use this general framework. The current version of the
 program can deal with e+e- annihilation, deep inelastic scattering, 
photoproduction in electron proton scattering and with various processes
 in hadron-hadron collisions.


%prep
%setup -q
%build
%configure  --disable-rpath
make
%install
%make_install 
find $RPM_BUILD_ROOT -type f -name '*.la' -exec rm -f {} \;
export QA_RPATHS=3

%files 
%defattr(-,root,root)
%doc AUTHORS README COPYING
%{_bindir}/*
%{_includedir}/*
%{_libdir}/*
/usr/libexec/*

%changelog
* Sun Aug 01 2021 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
  - RPATH
* Thu Nov 23 2017 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - Initial

