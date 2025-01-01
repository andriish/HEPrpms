Name:           LCIO
Version:        2.22.02
Release:        1%{?dist}
Summary:        LCIO (Linear Collider I/O) is a persistency framework and event data model for linear collider detector studies.

Group:          Development/Tools
License:        BSD 3-Clause
URL:            http://lcio.desy.de/
Source0:       https://github.com/iLCSoft/LCIO/archive/refs/tags/v02-22-02.tar.gz

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
Requires: root 
BuildRequires: root root-core root-tpython git
BuildRequires: zlib  ncurses-libs
BuildRequires: python3 python3-devel gcc-gfortran
%endif


%if 0%{?suse_version}
Requires: root6-config root6-libs  root6-utils nlohmann_json-devel git
BuildRequires: root6-config root6-libs root6-devel root6  root6-utils nlohmann_json-devel git
BuildRequires: pkgconfig(zlib) 
BuildRequires: python python-devel gcc-fortran
%endif

Requires: zlib
BuildRequires: gcc-c++ cmake  clhep clhep-devel 
BuildRequires: readline readline-devel ncurses-devel zlib-devel 

%description
LCIO (Linear Collider I/O) is a persistency framework and event data 
model for linear collider detector studies. It is intended to be used in
both simulation studies and analysis frameworks. Its light weight and 
portability makes it also suitable for use in detector R&D testbeam applications. 
It provides a C++ and a Java implementation with a common interface (API) -
a Fortran interface to the C++ implementation also exists.
Using a common persistency format and event data model allows to easily 
share results and compare reconstruction algorithms. LCIO is used by almost
all groups involved in linear collider detector studies and thus has become a de facto standard.

%package  devel
Summary:        Libraries and headers for %{name}
Requires:       %{name} = %{version}-%{release}
Provides:       %{name}-devel = %{version}-%{release}

%description  devel
%{libnamedev} contains the libraries and header files needed to
develop programs which make use of %{name}.
The library documentation is available on header files.

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
%package -n python3-%{name}
Summary:   %{name}  Python 3 bindings
%{?python_provide:%python_provide python3-%{name}}
%description -n python3-%{name}
This package provides the Python 3 bindings for %{name}
%endif
%if  0%{?suse_version}
%package -n python3-%{name}
Summary:   %{name}  Python  bindings
%{?python_provide:%python_provide python3-%{name}}
%description -n python3-%{name}
This package provides the Python 3 bindings for %{name}
%endif


%prep
%setup -q -n LCIO-02-22-02
#patch0 -p1
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
sed  -i 's/python -c/python3 -c/g' tests/CMakeLists.txt
%endif

%if  0%{?suse_version}
sed  -i 's/python -c/python3 -c/g' tests/CMakeLists.txt
%endif

%build
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
%if %{?fedora}%{!?fedora:0} >= 34 || %{?rhel}%{!?rhel:0} > 8
%cmake  -DBUILD_TESTING:BOOL=OFF -DBUILD_ROOTDICT:BOOL=ON -DCMAKE_SKIP_RPATH=ON -DCMAKE_CXX_STANDARD=17
%else
%cmake  -DBUILD_TESTING:BOOL=OFF -DBUILD_ROOTDICT:BOOL=ON -DCMAKE_SKIP_RPATH=ON 
%endif

%cmake_build
%endif
%if  0%{?suse_version}
%cmake  -DBUILD_TESTING:BOOL=OFF -DBUILD_ROOTDICT:BOOL=ON -DCMAKE_SKIP_RPATH=ON -DCMAKE_CXX_STANDARD=17
%cmake_build
%endif


%install

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
%cmake_install
mkdir -p $RPM_BUILD_ROOT/%{python3_sitearch}
mv $RPM_BUILD_ROOT/usr/python/* $RPM_BUILD_ROOT/%{python3_sitearch}
%endif

%if  0%{?suse_version}
%cmake_install
mkdir -p $RPM_BUILD_ROOT/%{python_sitearch}
mv $RPM_BUILD_ROOT/usr/python/* $RPM_BUILD_ROOT/%{python_sitearch}
%endif

%post -p /sbin/ldconfig

%postun -p /sbin/ldconfig

%files
%{_bindir}/*
%{_libdir}/lib*
%{_libdir}/*_rdict.pcm

%files  devel
%{_includedir}/*
%{_libdir}/cmake/*


%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
%files  -n python3-%{name}
%{python3_sitearch}/*
%endif

%if  0%{?suse_version}
%files  -n python3-%{name}
%{python_sitearch}/*
%endif

%changelog
* Mon Dec 30 2024 Andrii Verbytskyi 2.22.2
- Update to 2.22.2
* Thu Dec 21 2023 Andrii Verbytskyi <andrii.verbtskyi@mpp.mpg.de>
+ 02.20.02
* Thu Nov 2 2023 Andrii Verbytskyi 2.20.00
- Update to 2.20.00
* Fri Mar 10 2023 Andrii Verbytskyi 2.19.01
- Update to 2.19.01
* Tue Jul 12 2022 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
- 2.17.01
* Mon Nov 15 2021 Andrii Verbytskyi 2.17.0
- Bump to 2.17.0
* Tue May 04 2021 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> 
- Added tests. Updated to python3.
* Mon May 03 2021 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> 
- Better separation of packages. Patch for cmake. Added -DBUILD_ROOTDICT:BOOL=ON 
* Tue Apr 20 2021 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de>
- Preparation for release
