Name:           fastjet
Version:        3.5.1
Release:        1002%{?dist}
License:        GPLv2+
URL:            http://www.fastjet.fr
Source0:        https://www.fastjet.fr/repo/%{name}-%{version}.tar.gz
Prefix:         %{_prefix}
Summary:        Fast implementation of several recombination jet algorithms
BuildRequires:  gcc-c++ cmake
%if 0%{?rhel} || 0%{?fedora}
BuildRequires:  gcc-gfortran swig
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
BuildRequires:  CGAL-devel >= 5.0
Requires:       CGAL-devel >= 5.0
%endif

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
BuildRequires:  python3-devel
%endif
%if %{?rhel}%{!?rhel:0} == 8
BuildRequires:  python36-rpm-macros
%endif
%endif
%if 0%{?suse_version}
BuildRequires:  gcc-fortran
BuildRequires:  cgal-devel >= 5.0
Requires:       cgal-devel >= 5.0
BuildRequires:  python3-devel python-rpm-macros
BuildRequires:  swig
%endif


%description
A software package for jet finding in pp and e+e- collisions.
It includes fast native implementations of many sequential 
recombination clustering algorithms, plugins for access 
to a range of cone jet finders and tools for advanced jet manipulation. 


%package  devel
Summary:        Libraries and headers for %{name}
Provides:       %{name}-devel = %{version}-%{release}

%description devel
%{libnamedev} contains the libraries and header files needed to
develop programs which make use of %{name}.
The library documentation is available on header files.



%package -n python%{python3_pkgversion}-%{name}
Summary:        python bindings for %{name} - Python 3 module
%{?python_provide:%python_provide python%{python3_pkgversion}-%{name}}
Requires: %{name}%{?_isa} = %{version}-%{release}
%description -n python%{python3_pkgversion}-%{name}
This package contains python bindings for %{name}.

%prep
%setup -q -n %{name}-%{version}

%build
%cmake -DFASTJET_ENABLE_CGAL=ON -DFASTJET_ENABLE_PYTHON=ON -DFASTJET_ENABLE_ALLPLUGINS=ON
%cmake_build

%install
%cmake_install



%files -n fastjet
%doc AUTHORS README.md COPYING
%{_libdir}/libfastjet.so*
%{_libdir}/libfastjetplugins.so*
%{_libdir}/libfastjettools.so*
%{_libdir}/libsiscone.so*
%{_libdir}/libsiscone_spherical.so*

%files -n fastjet-devel 
%{_bindir}/%{name}-config
%{_includedir}/siscone/*.h
%{_includedir}/siscone/spherical/*.h
%{_includedir}/fastjet/*.h
%{_includedir}/fastjet/*.hh
%{_includedir}/fastjet/tools/*.hh
%{_includedir}/fastjet/internal/*.hh
%{_libdir}/cmake/siscone
%{_libdir}/cmake/fastjet


%files -n python%{python3_pkgversion}-%{name}
%{python3_sitearch}/fastjet/_%{name}*.so*
%{python3_sitearch}/fastjet/*.p*
%{python3_sitearch}/fastjet/__pycache__/*



%changelog
* Mon Dec 30 2024 Andrii Verbytskyi 3.4.3
- Update to 3.4.3
* Thu Nov 2 2023 Andrii Verbytskyi 3.4.2
- Update to 3.4.2
* Mon Mar 2 2020  Andrii Verbytskyi  <andrii.verbytskyi@mpp.mpg.de> 3.3.3-1
-  Initial version for EPEL
