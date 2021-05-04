Name:           DD4hep
Version:        01.16.01
Release:        1%{?dist}
Summary:        DD4hep (Detector Description for High Energy Physics)
Group:          Development/Tools
License:        Custom
URL:            dd4hep.cern.ch
Source0:        https://github.com/AIDASoft/DD4hep/archive/v01-16-01.tar.gz

BuildRequires: latex biber LCIO-devel doxygen
Requires: geant4 LCIO

%if 0%{?suse_version}
Requires:         root6 root6-libs libHepMC4 python tbb 
BuildRequires:    root6 root6-libs
BuildRequires:    HepMC3-devel  libHepMC4 python-devel boost-devel boost-filesystem tbb-devel libexpat-devel
%endif

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
Requires:         root HepMC3 python tbb 
BuildRequires:    root root-devel
BuildRequires:    HepMC3-devel  HepMC3  python3-devel boost-devel boost-filesystem tbb-devel expat-devel
%endif
BuildRequires: cmake>=3.4.3 gcc-c++  LCIO-devel xerces-c-devel xerces-c biber
Prefix: %{_prefix}

%description
DD4hep is a software framework for providing a complete solution for full 
detector description (geometry, materials, visualization, readout, alignment, 
calibration, etc.) for the full experiment life cycle (detector concept development, 
detector optimization, construction, operation). 
It offers a consistent description through a single source of detector information for 
simulation, reconstruction, analysis, etc. It distributed under the LGPLv3 

%package  devel
Summary:        Libraries and headers for %{name}
Provides:       %{name}-devel = %{version}-%{release}

%description  devel
Contains the libraries and header files needed to
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
%package -n python-%{name}
Summary:   %{name}  Python  bindings
%{?python_provide:%python_provide python3-%{name}}
%description -n python-%{name}
This package provides the Python 3 bindings for %{name}
%endif


%prep
%setup -q -n DD4hep-01-16-01
#patch0 -p1

%build
%if  %{?rhel}%{!?rhel:0} == 8
%cmake  -DBUILD_TESTING:BOOL=OFF -H. -B.  -DDD4HEP_USE_HEPMC3=ON -DDD4HEP_USE_LCIO=ON -DLCIO_DIR=%{_libdir}/cmake -DDD4HEP_USE_XERCESC=ON -DDD4HEP_USE_TBB=ON -DDD4HEP_USE_GEANT4=ON -DCLHEP_DIR=%{_libdir}/$(clhep-config --version| tr ' ' '-')
%else
%cmake  -DBUILD_TESTING:BOOL=OFF -S. -B. -DDD4HEP_USE_HEPMC3=ON -DDD4HEP_USE_LCIO=ON -DLCIO_DIR=%{_libdir}/cmake -DDD4HEP_USE_XERCESC=ON -DDD4HEP_USE_TBB=ON -DDD4HEP_USE_GEANT4=ON -DCLHEP_DIR=%{_libdir}/$(clhep-config --version| tr ' ' '-')
%endif 

%install
%make_install


mkdir -p $RPM_BUILD_ROOT/usr/share/cmake/
mv $RPM_BUILD_ROOT/usr/*cmake $RPM_BUILD_ROOT/usr/share/cmake/
mv $RPM_BUILD_ROOT/usr/lib $RPM_BUILD_ROOT/usr/%_lib
mkdir -p $RPM_BUILD_ROOT/usr/%_lib
mv $RPM_BUILD_ROOT/usr/lib/lib* $RPM_BUILD_ROOT/usr/%_lib
mv $RPM_BUILD_ROOT/usr/lib/G__* $RPM_BUILD_ROOT/usr/%_lib
mkdir -p $RPM_BUILD_ROOT/usr/share/DD4hep
mv $RPM_BUILD_ROOT/usr/DDDetectors $RPM_BUILD_ROOT/usr/share/DD4hep
mv $RPM_BUILD_ROOT/usr/examples $RPM_BUILD_ROOT/usr/share/DD4hep

%post -p /sbin/ldconfig

%postun -p /sbin/ldconfig

%files
%{_bindir}/*
%{_libdir}/*
%{_prefix}/DDDetectors
%{_prefix}/examples

%files  devel
%{_includedir}/*
%{_prefix}/share/cmake/*

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
%files  -n python3-%{name}
%{python3_sitearch}/*
%endif

%if  0%{?suse_version}
%files  -n python-%{name}
%{python_sitearch}/*
%endif

%changelog
* Mon May 03 2021 Andrii Verbytskyi <andrii.verbtskyi@mpp.mpg.de> 
- Try to have a first version for RHEL8
