%undefine __cmake_in_source_build
%undefine __cmake3_in_source_build

Name:		geant4
Version:	11.1.1
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
BuildRequires:	PTL-devel 
%if 0%{?rhel} || 0%{?fedora}
BuildRequires:	 expat-devel  xerces-c-devel xerces-c
%endif
%if 0%{?suse_version}
BuildRequires:	 libexpat-devel libxerces-c-devel xerces-c 
%endif



%description
Geant4: A Simulation Toolkit



%package devel
Summary:	C++ Event Record for Monte Carlo Generators - development files
Requires:	%{name}%{?_isa} = %{version}-%{release}

%description devel
This package provides development files for %{name}.



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
%setup -q -n geant4-v%{version}


%build

%cmake -DGEANT4_INSTALL_DATA:BOOL=ON \
	-DGEANT4_USE_SYSTEM_CLHEP:BOOL=ON \
	-DGEANT4_USE_SYSTEM_EXPAT:BOOL=ON \
	-DGEANT4_USE_SYSTEM_ZLIB:BOOL=ON \
	-DGEANT4_USE_SYSTEM_PTL:BOOL=ON \
	-DCMAKE_SKIP_INSTALL_RPATH:BOOL=ON \
	-DGEANT4_USE_GDML:BOOL=ON \
	-DGEANT4_BUILD_MULTITHREADED=ON  -DGEANT4_BUILD_TLS_MODEL=global-dynamic
%cmake_build


%install
%cmake_install


%ldconfig_scriptlets


%files
%{_bindir}/geant4*
%{_libdir}/lib*.so*
%{_datadir}/Geant4/geant4make
%{_libdir}/Geant4-%{version}


%files devel
%{_includedir}/Geant4/
%{_libdir}/cmake/Geant4



%files doc
%{_datadir}/Geant4/examples
%{_datadir}/Geant4/fonts
%license %{_datadir}/Geant4/tools.license

%files data
%{_datadir}/Geant4/data

%changelog
* Fri Mar 10 2023 Andrii Verbytskyi 11.1.1
- Update to 11.1.1
* Sun Dec 12 2021  Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de>
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

