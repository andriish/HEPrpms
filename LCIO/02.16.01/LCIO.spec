Name:           LCIO
Version:        2.16.01
Release:        2%{?dist}
Summary:        LCIO (Linear Collider I/O) is a persistency framework and event data model for linear collider detector studies.

Group:          Development/Tools
License:        BSD 3-Clause
URL:            http://lcio.desy.de/
Source0:        https://github.com/iLCSoft/LCIO/archive/v02-16-01.tar.gz
Patch0:         patch-LCIO-0.txt

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
Requires: root 
BuildRequires: root root-devel
BuildRequires: zlib  ncurses-libs
BuildRequires: python2 python2-devel gcc-gfortran
%endif


%if 0%{?suse_version}
Requires: root6 root6-libs
BuildRequires: root6 root6-libs
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
%patch0 -p1

%build
%cmake  -DBUILD_TESTING:BOOL=OFF -DBUILD_ROOTDICT:BOOL=ON 
%cmake_build

%install

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
%cmake_install
mkdir -p $RPM_BUILD_ROOT/%{python2_sitearch}
mv $RPM_BUILD_ROOT/usr/python/* $RPM_BUILD_ROOT/%{python2_sitearch}
%endif

%if  0%{?suse_version}
%cmake_install
mkdir -p $RPM_BUILD_ROOT/%{python_sitearch}
mv $RPM_BUILD_ROOT/usr/python/* $RPM_BUILD_ROOT/%{python_sitearch}
%endif

mv $RPM_BUILD_ROOT/%{_prefix}/*.cmake $RPM_BUILD_ROOT/%{_libdir}/cmake/

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
%files  -n python2-%{name}
%{python2_sitearch}/*
%endif

%if  0%{?suse_version}
%files  -n python-%{name}
%{python_sitearch}/*
%endif

%changelog
* Mon May 03 2021 Andrii Verbytskyi <andrii.verbtskyi@mpp.mpg.de> 
- Better separation of packages. Patch for cmake. Added -DBUILD_ROOTDICT:BOOL=ON 
* Tue Apr 20 2021 Andrii Verbytskyi <andrii.verbtskyi@mpp.mpg.de>
- Preparation for release
