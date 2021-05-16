Name:          qcdnum
Version:       17.01.16
Release:       2%{?dist}
License:       GPLv3
Prefix:        %{_prefix}
Summary:       A very fast QCD evolution program written in FORTRAN77
Source:        https://www.nikhef.nl/~h24/download/qcdnum170116.tar.gz
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
%setup -q -n qcdnum-17-01-16
%build 
%if %{?fedora}%{!?fedora:0} >= 31
export FFLAGS="%{optflags} -std=legacy  -fallow-argument-mismatch -Iinc"
export FCFLAGS="%{optflags} -std=legacy  -fallow-argument-mismatch -Iinc"
%else
export FFLAGS="%{optflags}  -std=legacy -Iinc"
export FCFLAGS="%{optflags} -std=legacy -Iinc"
%endif

%configure 
make %{?_smp_mflags}

%install 
%make_install


%files
%defattr(-,root,root)
/usr/bin/*
/usr/%_lib/*
/usr/share/doc/qcdnum/README

%files devel
%_includedir/*


%post 
ldconfig 

%changelog             
*  Wed Feb 26 2020 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de>
 - 17.01.16
*  Fri Nov 29 2019 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de>
 - Initial
