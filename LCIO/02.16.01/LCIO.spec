Name:           LCIO
Version:        2.16.01
Release:        1%{?dist}
Summary:        LCIO (Linear Collider I/O) is a persistency framework and event data model for linear collider detector studies.

Group:          Development/Tools
License:        BSD 3-Clause
URL:            http://lcio.desy.de/
Source0:        https://github.com/iLCSoft/LCIO/archive/v02-16-01.tar.gz

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
BuildRequires: zlib-devel zlib readline readline-devel ncurses-devel ncurses-libs clhep clhep-devel
Requires: zlib
BuildRequires: cmake gcc-c++ python2 python2-devel
%endif


%if 0%{?suse_version}
BuildRequires: zlib-devel pkgconfig(zlib) readline readline-devel ncurses-devel  clhep clhep-devel
Requires: zlib
BuildRequires: cmake gcc-c++ python python-devel
%endif

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
Provides:       %{name}-devel = %{version}-%{release}

%description  devel
%{libnamedev} contains the libraries and header files needed to
develop programs which make use of %{name}.
The library documentation is available on header files.

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
%package -n python2-%{name}
Summary:   %{name}  Python 2 bindings
%{?python_provide:%python_provide python2-%{name}}
%description -n python2-%{name}
This package provides the Python 2 bindings for %{name}
%endif
%if  0%{?suse_version}
%package -n python-%{name}
Summary:   %{name}  Python  bindings
%{?python_provide:%python_provide python2-%{name}}
%description -n python-%{name}
This package provides the Python 2 bindings for %{name}
%endif


%prep
%setup -q -n LCIO-02-16-01

%build

%if  %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
%cmake  -DBUILD_TESTING:BOOL=OFF
%cmake_build
%endif 
%if 0%{?suse_version}
%cmake  -DBUILD_TESTING:BOOL=OFF 
%cmake_build
%endif

%install

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
%cmake_install
mv $RPM_BUILD_ROOT/usr/lib $RPM_BUILD_ROOT/usr/%_lib
mkdir -p $RPM_BUILD_ROOT/%{python2_sitearch}
mv $RPM_BUILD_ROOT/usr/python/* $RPM_BUILD_ROOT/%{python2_sitearch}
%endif

%if  0%{?suse_version}
%cmake_install
mv $RPM_BUILD_ROOT/usr/lib $RPM_BUILD_ROOT/usr/%_lib
mkdir -p $RPM_BUILD_ROOT/%{python_sitearch}
mv $RPM_BUILD_ROOT/usr/python/* $RPM_BUILD_ROOT/%{python_sitearch}
%endif

mkdir -p $RPM_BUILD_ROOT/usr/share/cmake/
mv $RPM_BUILD_ROOT/usr/*cmake $RPM_BUILD_ROOT/usr/share/cmake/

%post -p /sbin/ldconfig

%postun -p /sbin/ldconfig

%files
%{_bindir}/*
%{_libdir}/*

%files  devel
%{_includedir}/*
/usr/share/cmake/*


%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
%files  -n python2-%{name}
%{python2_sitearch}/*
%endif

%if  0%{?suse_version}
%files  -n python-%{name}
%{python_sitearch}/*
%endif

%changelog
* Tue Apr 20 2021 Andrii Verbytskyi <andrii.verbtskyi@mpp.mpg.de>
- Preparation for release
