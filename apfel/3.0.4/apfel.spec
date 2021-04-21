
Name:           apfel
Version:        3.0.4
Release:        3%{?dist}
License:        GPL
Url:            https://github.com/scarrazza/apfel
Source:         https://github.com/scarrazza/apfel/archive/3.0.4.tar.gz
Summary:        APFEL: A PDF Evolution Library
Prefix: %{_prefix}
BuildRequires:  gcc-c++  gcc-gfortran
BuildRequires:  python%{python3_pkgversion}-devel

%description 
APFEL is a library able to perform DGLAP evolution up to NNLO in QCD 
and to NLO in QED, both with pole and MSbar masses. The coupled DGLAP 
QCD+QED evolution equations are solved in x-space by means of higher 
order interpolations and Runge-Kutta techniques.
 
%package devel
Summary: Development files for APFEL
Requires: %{name} = %{version}

%description devel
Install this package to develop software based on APFEL.

%package -n python%{python3_pkgversion}-%{name}
Summary:  %{name} Python 3 bindings
%{?python_provide:%python_provide python%{python3_pkgversion}-%{name}}
Requires:  %{name}%{?_isa} = %{version}-%{release}

%description -n python%{python3_pkgversion}-%{name}
This package provides the Python 3 bindings for HepMC3.
 
%prep
%setup  -q -n %{name}-%{version}


%build
%configure 
%make_build 

%install
%make_install



%files 
%{_bindir}/*
%{_docdir}/*
%{_libdir}/*
%{_datadir}/*


%files devel
%{_includedir}/*

%files -n python%{python3_pkgversion}-%{name}
%{python3_sitearch}/*

%changelog
* Tue Apr 20 2021 Andrii Verbytskyi <andrii.verbtskyi@mpp.mpg.de>
+ Preparation for release
* Mon Feb 3 2014 Andrii Verbytskyi 1.0.4
+ Initial spec file
