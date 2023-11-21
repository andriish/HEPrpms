

Name:           apfel
Version:        3.1.0
Release:        1%{?dist}
License:        GPL
Url:            https://github.com/scarrazza/apfel
Source:         https://github.com/scarrazza/apfel/archive/refs/tags/%{version}.tar.gz
Summary:        A PDF Evolution Library
Prefix: %{_prefix}
BuildRequires:  gcc-c++  gcc-gfortran cmake swig lhapdf-devel python3 python3-devel  
%if %{?rhel}%{!?rhel:0} >= 8
BuildRequires: platform-python-devel 
%endif

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
%if %{?fedora}%{!?fedora:0} >= 39
%py3_shebang_fix  bin/apfel.in
%else
pathfix.py -pn -i %{__python3}  bin/apfel.in
%endif

%cmake -DAPFEL_DOWNLOAD_PDFS=BOOL:OFF
%cmake_build 

%install
%cmake_install

%files 
%{_bindir}/apfel*
%{_docdir}/*
%{_libdir}/libA*
%{_datadir}/*


%files devel
%{_includedir}/APFEL/*

%files -n python%{python3_pkgversion}-%{name}
%{python3_sitearch}/*

%changelog
* Tue Nov 21 2023 Andrii Verbytskyi <andrii.verbtskyi@mpp.mpg.de>
+ 3.0.7-pre
