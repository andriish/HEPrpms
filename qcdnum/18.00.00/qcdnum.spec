Name:          qcdnum
Version:       18.00.0
Release:       1%{?dist}
License:       GPLv3
Prefix:        %{_prefix}
Summary:       A very fast QCD evolution program written in FORTRAN77
Source:        https://www.nikhef.nl/~h24/download/qcdnum180000.tar.gz
URL:           https://www.nikhef.nl/~h24/qcdnum/
%if 0%{?rhel} || 0%{?fedora}
BuildRequires: gcc-gfortran gcc-c++
%endif
%if 0%{?suse_version}
BuildRequires: gcc-fortran gcc-c++
%endif

%description
QCDNUM numerically solves the DGLAP evolution equations on a discrete
 grid in x and Q2. You can evolve unpolarised parton density functions
  in NNLO, and polarised pdfs or fragmentation functions in NLO. 


%package  devel
Summary:        Libraries and headers for %{name}
Provides:       %{name}-devel = %{version}-%{release}

%description  devel
Contains the libraries and header files needed to
develop programs which make use of %{name}.
The library documentation is available on header files.


%prep 
%setup -q -n qcdnum-18-00-00
%build 
%if %{?fedora}%{!?fedora:0} >= 31
export FFLAGS="%{optflags} -std=legacy  -fallow-argument-mismatch -Iinc"
export FCFLAGS="%{optflags} -std=legacy  -fallow-argument-mismatch -Iinc"
%else
export FFLAGS="%{optflags}  -std=legacy -Iinc"
export FCFLAGS="%{optflags} -std=legacy -Iinc"
%endif

%configure 
%make_build

%install 
%make_install
rm -f %{buildroot}/%_libdir/*.la

%files
%defattr(-,root,root)
%_bindir/*
%_libdir/*
/usr/share/doc/qcdnum/README

%files devel
%_includedir/*


%post 
ldconfig 

%changelog
* Fri Mar 10 2023 Andrii Verbytskyi 18.00.00
- Update to 18.00.00
* Mon Nov 15 2021 Andrii Verbytskyi 17.01.83
- Bump to 17.01.83    
*  Wed Feb 26 2020 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de>
 - 17.01.16
*  Fri Nov 29 2019 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de>
 - Initial
