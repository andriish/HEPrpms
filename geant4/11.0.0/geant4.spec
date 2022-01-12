%undefine __cmake_in_source_build
%undefine __cmake3_in_source_build
%if 0%{?suse_version}
%define python3_pkgversion 38
%endif
Name:		geant4
Version:	11.0.0
Release:	1%{?dist}
Summary:	A toolkit for the simulation of the passage of particles through matter

License:	Geant
URL:		https://geant4.web.cern.ch
Source0:	https://geant4-data.web.cern.ch/releases/geant4-v%{version}.tar.gz
#Patch0:         patch-geant4-0.txt

BuildRequires:	cmake >= 3.9
BuildRequires:	gcc-c++
BuildRequires:	doxygen
BuildRequires:	clhep-devel
BuildRequires:	zlib-devel
BuildRequires:	PTL-devel python3 python3-devel
%if 0%{?rhel} || 0%{?fedora}
BuildRequires:	boost-python3-devel boost-python3 expat-devel  xerces-c-devel xerces-c
%endif
%if 0%{?suse_version}
BuildRequires:	libboost_python3-devel libexpat-devel libxerces-c-devel xerces-c 
%endif



%description
Geant4: A Simulation Toolkit



%package devel
Summary:	C++ Event Record for Monte Carlo Generators - development files
Requires:	%{name}%{?_isa} = %{version}-%{release}

%description devel
This package provides development files for %{name}.


%package -n python%{python3_pkgversion}-%{name}
Summary:	%{name} Python 3 bindings
%{?python_provide:%python_provide python%{python3_pkgversion}-%{name}}
Requires:	%{name}%{?_isa} = %{version}-%{release}

%description -n python%{python3_pkgversion}-%{name}
This package provides the Python 3 bindings for %{name}.



%package doc
Summary:	C++ Event Record for Monte Carlo Generators - documentation
BuildArch:	noarch

%description doc
This package provides HepMC manuals and examples.


%package data
Summary:	C++ Event Record for Monte Carlo Generators - documentation
BuildArch:	noarch

%description data
This package provides HepMC manuals and examples.


%prep
%setup -q -n geant4-v11.0.0
#patch0 -p1
%if %{?rhel}%{!?rhel:0} == 8
sed -i 's|python${PYTHON_VERSION_MAJOR}${PYTHON_VERSION_MINOR}|python${PYTHON_VERSION_MAJOR}|g' environments/g4py/CMakeLists.txt
sed -i 's|python${PYTHON_VERSION_MAJOR}${PYTHON_VERSION_MINOR}|python${PYTHON_VERSION_MAJOR}|g' environments/g4py/G4PythonHelpers.cmake
%endif


%build

%cmake -DGEANT4_INSTALL_DATA:BOOL=ON \
	-DGEANT4_USE_SYSTEM_CLHEP:BOOL=ON \
	-DGEANT4_USE_SYSTEM_EXPAT:BOOL=ON \
	-DGEANT4_USE_SYSTEM_ZLIB:BOOL=ON \
	-DGEANT4_USE_SYSTEM_PTL:BOOL=ON \
	-DCMAKE_SKIP_INSTALL_RPATH:BOOL=ON \
	-DGEANT4_USE_GDML:BOOL=ON \
	-DGEANT4_BUILD_MULTITHREADED=ON -DGEANT4_USE_PYTHON:BOOL=ON  -DGEANT4_BUILD_TLS_MODEL=global-dynamic
%cmake_build


%install
%cmake_install


%ldconfig_scriptlets


%files
%{_bindir}/geant4*
%{_libdir}/lib*.so*
%{_datadir}/Geant4-11.0.0/geant4make
%{_libdir}/Geant4-11.0.0


%files devel
%{_includedir}/Geant4/


%files -n python%{python3_pkgversion}-%{name}
%{python3_sitearch}/*


%files doc
%{_datadir}/Geant4-11.0.0/examples
%license %{_datadir}/Geant4-11.0.0/tools.license

%files data
%{_datadir}/Geant4-11.0.0/data

%changelog
%changelog
* Mon Dec 12 2021  Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de>
- v11.0.0
* Mon Sep 06 2021  Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de>
- cmake fixes
* Tue Aug 03 2021  Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de>
- cmake fixes
* Fri Jul 16 2021 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> 
- Version bump
* Mon May 03 2021 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> 
- Added -DGEANT4_USE_GDML:BOOL=ON
* Fri May 29 2020 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> - 10.07-1
- Preparation

