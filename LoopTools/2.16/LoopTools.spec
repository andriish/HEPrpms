%undefine _debugsource_packages
#Scripts fail to find debug infos because the library is static

Summary: A package for evaluation of scalar and tensor one-loop integrals based on the FF package by G.J. van Oldenborgh
Name: LoopTools
Version: 2.16
Release: 7%{?dist}
License: LGPLv3
Source: https://www.feynarts.de/looptools/LoopTools-{version}.tar.gz
#Source:        https://github.com/andriish/HEPsources/raw/master/LoopTools-%{version}.tar.gz
Patch0:         patch-LoopTools-0.txt
URL:    http://www.feynarts.de/looptools/
Prefix: %{_prefix}

%if 0%{?rhel} || 0%{?fedora}
BuildRequires: gcc-gfortran gcc-c++
Requires:      libgfortran
%endif
%if 0%{?suse_version}
BuildRequires: gcc-fortran gcc-c++
Requires: libgfortran5
%endif

%description
LoopTools is a package for evaluation of scalar and tensor one-loop 
integrals based on the FF package by G.J. van Oldenborgh. 
It features an easy Fortran, C++, and Mathematica interface to the 
scalar one-loop functions of FF and in addition provides the 
2-, 3-, and 4-point tensor coefficient functions.

LoopTools has been published in Comput. Phys. Commun. 118 (1999) 153 [hep-ph/9807565]
FF has been published in Z. Phys. C46 (1990) 425 

%prep 
%setup -q
%patch0 -p1

%build
sed -i 's@clang++@@' configure
sed -i 's@clang@@' configure
export FFLAGS="%{optflags} -fPIC"
export CXXFLAGS="%{optflags} -fPIC"
export CFLAGS="%{optflags} -fPIC"
%configure
%make_build

%install
%make_install

%files
%defattr(-,root,root)
%{_bindir}/*
%{_includedir}/clooptools.h
%{_includedir}/looptools.h
%{_libdir}/libooptools.a


%post 
ldconfig 

%changelog
* Fri May 21 2021 Andrii Verbytskyi
- 2.16
* Fri Nov 29 2019 Andrii Verbytskyi
- 2.15        
* Tue Apr 8 2014 Ben Meekhof <bmeekhof@umich.edu> - 2.10-1
- initial packaging

