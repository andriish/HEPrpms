Name:           DD4hep
Version:        01.30.00
Release:        6%{?dist}
Summary:        DD4hep (Detector Description for High Energy Physics)
Group:          Development/Tools
License:        Custom
URL:            https://dd4hep.cern.ch
Source0:        https://github.com/AIDASoft/DD4hep/archive/v01-30.tar.gz
Patch0:         patch-DD4hep-0.txt

Requires: geant4 LCIO tbb PTL zlib-devel  clhep xerces-c  expat 
BuildRequires: cmake >= 3.4.3 
BuildRequires: gcc-c++  LCIO LCIO-devel xerces-c doxygen ImageMagick cups-filters clhep clhep-devel  PTL-devel zlib-devel

%if %{?rhel}%{!?rhel:0} >= 8
BuildRequires: tex(latex) platform-python-devel   texlive-tex4ht
%else
BuildRequires: tex(latex)  texlive-tex4ht
%endif
%if %{?fedora}%{!?fedora:0} 
BuildRequires: biber liburing liburing-devel
Requires: liburing
%endif

%if 0%{?suse_version}
Requires:         root6 root6-libs libHepMC4 python  
BuildRequires:    root6 root6-libs root6-devel python3-root6
BuildRequires:    geant4-devel >=   10.07.p01
BuildRequires:    HepMC3-devel  libHepMC4 python-devel boost-devel libboost_filesystem1_76_0 libboost_filesystem1_76_0-devel  libboost_system1_76_0 libboost_system1_76_0-devel tbb-devel libexpat-devel libxerces-c-devel
%endif

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
Requires:         root HepMC3 python tbb python3
BuildRequires:    geant4-devel
BuildRequires:    root root-core root-graf3d-eve root-graf3d-eve7 root-genvector root-geom root-gui root-mathcore root-mathmore root-tree root-physics root-gdml root-graf3d root-tpython  	root-gui-browserv7
BuildRequires:    HepMC3-devel  HepMC3  python3  python3-devel boost-devel boost-filesystem tbb-devel expat-devel xerces-c-devel
%endif

Prefix: %{_prefix}

%description
DD4hep is a software framework for providing a complete solution for full 
detector description (geometry, materials, visualization, readout, alignment, 
calibration, etc.) for the full experiment life cycle (detector concept development, 
detector optimization, construction, operation). 
It offers a consistent description through a single source of detector information for 
simulation, reconstruction, analysis, etc. It distributed under the LGPLv3 

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
%setup -q -n DD4hep-01-30
%patch -P 0 -p1

%build
#TBB should be fixed
%if  %{?rhel}%{!?rhel:0} == 8
%cmake  -DBUILD_TESTING:BOOL=OFF -DDD4HEP_RELAX_PYVER:BOOL=ON  -H. -B. -DDD4HEP_USE_HEPMC3=ON -DDD4HEP_USE_LCIO=ON -DLCIO_DIR=%{_libdir}/cmake -DDD4HEP_USE_XERCESC=ON -DDD4HEP_USE_TBB=OFF -DTBB_DIR=%{_libdir}/cmake/tbb/ -DDD4HEP_USE_GEANT4=ON -DCLHEP_DIR=%{_libdir}/$(clhep-config --version| tr ' ' '-')
make -j 2
#{?_smp_mflags}
%endif
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} > 8
%cmake  -DBUILD_TESTING:BOOL=OFF -DDD4HEP_RELAX_PYVER:BOOL=ON -DCMAKE_SKIP_RPATH=ON -S. -B. -DDD4HEP_USE_HEPMC3=ON -DDD4HEP_USE_LCIO=ON -DLCIO_DIR=%{_libdir}/cmake -DDD4HEP_USE_XERCESC=ON -DDD4HEP_USE_TBB=ON  -DTBB_DIR=%{_libdir}/cmake/tbb/ -DDD4HEP_USE_GEANT4=ON -DCLHEP_DIR=%{_libdir}/$(clhep-config --version| tr ' ' '-')
make -j 2
#{?_smp_mflags}
%endif 
%if 0%{?suse_version}
%cmake  -DBUILD_TESTING:BOOL=OFF  -DDD4HEP_RELAX_PYVER:BOOL=ON        -DDD4HEP_USE_HEPMC3=ON -DDD4HEP_USE_LCIO=ON -DLCIO_DIR=%{_libdir}/cmake -DDD4HEP_USE_XERCESC=ON -DDD4HEP_USE_TBB=ON  -DTBB_DIR=%{_libdir}/cmake/tbb/ -DDD4HEP_USE_GEANT4=ON -DCLHEP_DIR=%{_libdir}/$(clhep-config --version| tr ' ' '-')
%cmake_build
%endif


%install
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
%make_install
%endif
%if 0%{?suse_version}
%cmake_install
%endif

#mkdir -p $RPM_BUILD_ROOT/usr/share/cmake/
#rm -rf $RPM_BUILD_ROOT/usr/bin/*.sh
#mv $RPM_BUILD_ROOT/usr/*cmake $RPM_BUILD_ROOT/usr/share/cmake/
#mkdir -p $RPM_BUILD_ROOT/usr/%_lib
#mv $RPM_BUILD_ROOT/usr/lib/lib* $RPM_BUILD_ROOT/usr/_lib
#mv $RPM_BUILD_ROOT/usr/lib/G__* $RPM_BUILD_ROOT/usr/%_lib
#mv $RPM_BUILD_ROOT/usr/lib/python* $RPM_BUILD_ROOT/usr/%_lib
#mkdir -p $RPM_BUILD_ROOT/usr/share/DD4hep
#mv $RPM_BUILD_ROOT/usr/DDDetectors $RPM_BUILD_ROOT/usr/share/DD4hep
#mv $RPM_BUILD_ROOT/usr/examples $RPM_BUILD_ROOT/usr/share/DD4hep

%if 0%{?suse_version}
sed -Ei "1{s|/usr/bin/env python|/usr/bin/python3|}" $RPM_BUILD_ROOT/usr/bin/*
%endif
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
%py3_shebang_fix $RPM_BUILD_ROOT/usr/bin/*
%endif

%post -p /sbin/ldconfig

%postun -p /sbin/ldconfig

%files
%{_bindir}/*
%{_libdir}/*
%{_prefix}/share/DD4hep/
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
* Mon Nov 15 2021 Andrii Verbytskyi 1.18
- Bump to 1.18
* Wed Jul 28 2021 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> 
- Version bump
* Mon May 03 2021 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> 
- Try to have a first version for RHEL8
