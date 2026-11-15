%undefine __cmake_in_source_build
%undefine __cmake3_in_source_build

Name:        geant4
Version:    11.4.3
Release:    2%{?dist}
Summary:    A toolkit for the simulation of the passage of particles through matter

License:    Geant
URL:        https://geant4.web.cern.ch
Source0:    https://geant4-data.web.cern.ch/releases/geant4-v%{version}.tar.gz

%if 0%{?suse_version}
# the datasets are downloaded by the Geant4 CMake, which is not possible in OBS builds (no network): provide them as sources
Source1:    https://cern.ch/geant4-data/datasets/G4NDL.4.7.1.tar.gz
Source2:    https://cern.ch/geant4-data/datasets/G4EMLOW.8.8.tar.gz
Source3:    https://cern.ch/geant4-data/datasets/G4PhotonEvaporation.6.1.2.tar.gz
Source4:    https://cern.ch/geant4-data/datasets/G4RadioactiveDecay.6.1.2.tar.gz
Source5:    https://cern.ch/geant4-data/datasets/G4PARTICLEXS.4.2.tar.gz
Source6:    https://cern.ch/geant4-data/datasets/G4PII.1.3.tar.gz
Source7:    https://cern.ch/geant4-data/datasets/G4RealSurface.2.2.tar.gz
Source8:    https://cern.ch/geant4-data/datasets/G4SAIDDATA.2.0.tar.gz
Source9:    https://cern.ch/geant4-data/datasets/G4ABLA.3.3.tar.gz
Source10:    https://cern.ch/geant4-data/datasets/G4INCL.1.3.tar.gz
Source11:    https://cern.ch/geant4-data/datasets/G4ENSDFSTATE.3.0.tar.gz
Source12:    https://cern.ch/geant4-data/datasets/G4CHANNELING.2.0.tar.gz
%endif

BuildRequires:    cmake >= 3.9
BuildRequires:    gcc-c++
BuildRequires:    doxygen
BuildRequires:    clhep-devel
BuildRequires:    zlib-devel
BuildRequires:    PTL-devel 
%if 0%{?rhel} || 0%{?fedora}
BuildRequires:     expat-devel  xerces-c-devel xerces-c
%endif
%if 0%{?suse_version}
BuildRequires:     libexpat-devel libxerces-c-devel xerces-c 
%endif



%description
Geant4: A Simulation Toolkit



%package devel
Summary:    Geant4: A Simulation Toolkit - development files
Requires:    %{name}%{?_isa} = %{version}-%{release}

%description devel
This package provides development files for %{name}.



%package doc
Summary:    Geant4: A Simulation Toolkit - documentation
BuildArch:    noarch

%description doc
This package provides Geant4 manuals and examples.


%package data
Summary:    Geant4: A Simulation Toolkit - documentation
BuildArch:    noarch

%description data
This package provides Geant4 manuals and examples.


%prep
%setup -q -n geant4-v%{version}
%if 0%{?suse_version}
# ExternalProject accepts file:// URLs and checks the MD5 of the dataset files, named <NAME>.<version>.tar.gz as in the remote repository
sed -i 's|^set(GEANT4_DATASETS_URL .*|set(GEANT4_DATASETS_URL "file://%{_sourcedir}")|' cmake/Modules/G4InstallData.cmake
grep -q 'file://%{_sourcedir}' cmake/Modules/G4InstallData.cmake
%endif


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
* Sun Nov 15 2026 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> - 11.4.3-2
- openSUSE: provide the datasets as sources (no network in OBS builds)
* Fri Nov 13 2026 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> - 11.4.3-1
- Update to 11.4.3
* Mon Dec 30 2024 Andrii Verbytskyi 11.3.0
- Update to 11.3.0
* Thu Dec 21 2023 Andrii Verbytskyi <andrii.verbtskyi@mpp.mpg.de>
+ 11.2.1
* Thu Nov 16 2023 Andrii Verbytskyi 11.1.3
- Update to 11.1.3
* Thu Nov 2 2023 Andrii Verbytskyi 11.1.2
- Update to 11.1.2
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

