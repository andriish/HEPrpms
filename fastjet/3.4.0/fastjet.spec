%if %{?fedora}%{!?fedora:0} >= 29 || %{?rhel}%{!?rhel:0} >= 8
%global py3default 1
%else
%global py3default 0
%endif
%if 0%{?suse_version}
%{!?python3_pkgversion:%global python3_pkgversion 3}
%endif

Name:           fastjet
Version:        3.4.0
Release:        1006%{?dist}
License:        GPLv2+
URL:            http://www.fastjet.fr
Source0:        https://www.fastjet.fr/repo/%{name}-%{version}.tar.gz
Prefix:         %{_prefix}
Summary:        Fast implementation of several recombination jet algorithms
BuildRequires:  gcc-c++ 
%if 0%{?rhel} || 0%{?fedora}
BuildRequires:  gcc-gfortran 
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
BuildRequires:  CGAL-devel >= 5.0
Requires:       CGAL-devel >= 5.0
%endif
%if ! %{py3default}
BuildRequires:  python2-devel
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

%if 0%{?rhel} || 0%{?fedora}
%if ! %{py3default}
%package        -n python2-%{name}
Summary:        python bindings for %{name} - Python 2 module
%{?python_provide:%python_provide python2-%{name}}
Requires: %{name}%{?_isa} = %{version}-%{release}
%description -n python2-%{name}
This package contains python bindings for %{name}.
%endif
%endif

%package -n python%{python3_pkgversion}-%{name}
Summary:        python bindings for %{name} - Python 3 module
%{?python_provide:%python_provide python%{python3_pkgversion}-%{name}}
Requires: %{name}%{?_isa} = %{version}-%{release}
%description -n python%{python3_pkgversion}-%{name}
This package contains python bindings for %{name}.


%if 0%{?suse_version}

%debug_package

%endif


%prep
%setup -q -n %{name}-%{version}
%build
%if 0%{?rhel} || 0%{?fedora}
%if %{py3default}
export PYTHON=%{__python3}
%else
export PYTHON=%{__python2}
%endif
%configure --enable-allplugins --enable-pyext --enable-cgal-header-only=yes  --with-cgal-dir=/usr/include
%make_build
%endif
%if 0%{?suse_version}
export PYTHON=%{__python3}
%configure --enable-allplugins --enable-pyext --enable-cgal-header-only=yes  --with-cgal-dir=/usr/include --enable-thread-safety
%make_build
%endif

%install
%if 0%{?rhel} || 0%{?fedora}
%if %{py3default}
%make_install pythondir=%{python3_sitearch}
rm -f %{buildroot}%{python3_sitearch}/*.a
rm -f %{buildroot}%{python3_sitearch}/*.la
%else
%make_install pythondir=%{python2_sitearch}
rm -f  %{buildroot}%{python2_sitearch}/*.a
rm -f  %{buildroot}%{python2_sitearch}/*.la
%endif
rm %{buildroot}%{_libdir}/*.a
rm %{buildroot}%{_libdir}/*.la

%if ! %{py3default}
  (
  export PYTHON=%{__python3}
  %configure --enable-allplugins --enable-pyext --enable-thread-safety
  cd pyinterface  
  make clean
  make  PYTHON=%{__python3}  pythondir=%{python3_sitearch}
  %make_install PYTHON=%{__python3}    pythondir=%{python3_sitearch}
  rm -f %{buildroot}%{python3_sitearch}/*.a
  rm -f %{buildroot}%{python3_sitearch}/*.la
  )
%endif
%endif


%if 0%{?suse_version}
%make_install pythondir=%{python3_sitearch}
rm -f %{buildroot}%{python3_sitearch}/*.a
rm -f %{buildroot}%{python3_sitearch}/*.la
rm %{buildroot}%{_libdir}/*.a
rm %{buildroot}%{_libdir}/*.la
%endif


%files -n fastjet
%doc AUTHORS README COPYING
%{_bindir}/%{name}-config
%{_libdir}/libfastjet.so
%{_libdir}/libfastjet.so.0
%{_libdir}/libfastjet.so.0.0.0
%{_libdir}/libfastjetplugins.so*
%{_libdir}/libfastjettools.so*
%{_libdir}/libsiscone.so*
%{_libdir}/libsiscone_spherical.so*

%files -n fastjet-devel 
%{_libdir}/libfastjet.so
%{_libdir}/libfastjettools.so
%{_libdir}/libsiscone.so
%{_libdir}/libsiscone_spherical.so
%{_libdir}/libfastjetplugins.so
%{_includedir}/siscone/*.h
%{_includedir}/siscone/spherical/*.h
%{_includedir}/fastjet/*.h
%{_includedir}/fastjet/*.hh
%{_includedir}/fastjet/tools/*.hh
%{_includedir}/fastjet/internal/*.hh
/usr/share/fastjet/pyinterface/*

%if 0%{?rhel} || 0%{?fedora}
%if ! %{py3default}
%files -n python2-%{name}
%{python2_sitearch}/_%{name}*.so*
%{python2_sitearch}/*.p*
%endif
%endif

%files -n python%{python3_pkgversion}-%{name}
%{python3_sitearch}/_%{name}*.so*
%{python3_sitearch}/*.p*
%{python3_sitearch}/__pycache__/*



%changelog
* Mon Mar 2 2020  Andrii Verbytskyi  <andrii.verbytskyi@mpp.mpg.de> 3.3.3-1
-  Initial version for EPEL
