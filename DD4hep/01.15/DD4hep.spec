Name:           DD4hep
Version:        1.15
Release:        1%{?dist}
Summary:        DD4hep (Detector Description for High Energy Physics)



Group:          Development/Tools
License:        Custom
URL:            dd4hep.cern.ch
Source0:        https://github.com/AIDASoft/DD4hep/archive/v01-15.tar.gz

BuildRequires: latex biber LCIO-devel
Requires: geant4 LCIO

%if 0%{?suse_version}
Requires:         root6 root6-libs libHepMC4 
BuildRequires:    root6 root6-libs
BuildRequires:    HepMC3-devel  libHepMC4 
%endif

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
Requires:         root HepMC3
BuildRequires:    root root-devel
BuildRequires:    HepMC3-devel  HepMC3 
%endif

BuildRequires: cmake gcc-c++  LCIO-devel xerces-c-devel xerces-c biber


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





%prep
%setup -q -n DD4hep-01-15

%build
%if  %{?rhel}%{!?rhel:0} == 8
%cmake  -DBUILD_TESTING:BOOL=OFF -H. -B.  -DDD4HEP_USE_HEPMC3=ON -DDD4HEP_USE_LCIO=ON -DDD4HEP_USE_XERCESC=ON
%else
%cmake  -DBUILD_TESTING:BOOL=OFF -S. -B. -DDD4HEP_USE_HEPMC3=ON -DDD4HEP_USE_LCIO=ON -DDD4HEP_USE_XERCESC=ON
%endif 

%install
%make_install


mkdir -p $RPM_BUILD_ROOT/usr/share/cmake/
mv $RPM_BUILD_ROOT/usr/*cmake $RPM_BUILD_ROOT/usr/share/cmake/

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



%changelog
* Mon May 03 2021 Andrii Verbytskyi <andrii.verbtskyi@mpp.mpg.de> 
- Try to have a first version for RHEL8
