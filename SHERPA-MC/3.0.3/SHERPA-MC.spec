%define major       0
%define libname     SHERPA-MC
%define sharename   SHERPA-MC-common
%define openmpiname SHERPA-MC-openmpi
%define libnamedev  SHERPA-MC-devel
%define develnamestatic  SHERPA-MC-static-devel
%define _unpackaged_files_terminate_build 0
%global _missing_build_ids_terminate_build 0



Name:           SHERPA-MC
Version:        3.0.3
Release:        2%{?dist}
License:        GPLv2
Url:              https://sherpa.hepforge.org
Source0:          https://gitlab.com/sherpa-team/sherpa/-/archive/v%{version}/sherpa-v%{version}.tar.gz
Summary:          Multipurpose Monte Carlo Event Generator for High Energy physics
Patch0:           patch-SHERPA-MC-0.txt

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} 
BuildRequires:    gcc-gfortran gcc-c++ root pythia8-devel pythia8   Rivet Rivet-devel cmake
BuildRequires:    root-core root HepMC3-rootIO-devel HepMC3-devel HepMC3-search HepMC3-search-devel  HepMC3-rootIO HepMC3  
BuildRequires:    autoconf automake libtool sqlite-devel sqlite subversion root-physics libzip-devel openmpi-devel
BuildRequires:    openmpi environment-modules lhapdf-devel fastjet fastjet-devel  YODA-devel Rivet-devel zlib zlib-devel openloops root-tree-ntuple-utils
Requires:         lhapdf  HepMC3-rootIO HepMC3  sqlite root root-core openloops root Rivet YODA pythia8 openmpi fastjet  zlib root-tree-ntuple-utils
Requires:         hztool libgfortran  recola qd openssl  HepMC3-search
BuildRequires:    swig  recola qd qd-devel openssl-devel openssl
Requires:         blackhat blackhat-data MG5_aMC
BuildRequires:    blackhat-devel blackhat  MG5_aMC
BuildRequires:    texinfo git
%endif
%if 0%{?suse_version}
BuildRequires:    gcc-fortran gcc-c++ pythia-devel libpythia8   Rivet Rivet-devel cmake
BuildRequires:    root6-libs root6-devel root6-config root6 HepMC3-devel  libHepMC4  
BuildRequires:    autoconf automake libtool sqlite-devel sqlite subversion  libzip-devel openmpi4-devel
BuildRequires:    openmpi environment-modules LHAPDF-devel fastjet fastjet-devel  YODA-devel Rivet-devel zlib zlib-devel  openloops
Requires:         libHepMC4 libLHAPDF libHepMC4  sqlite root6 root6-libs openloops Rivet YODA libpythia8 openmpi4 fastjet  zlib
Requires:         hztool gcc-fortran  recola libqd0 openssl
BuildRequires:    swig  recola libqd0 qd-devel openssl-devel openssl
Requires:         blackhat blackhat-data libgfortran5
BuildRequires:    blackhat-devel blackhat  MG5_aMC
BuildRequires:    texinfo git  MG5_aMC
%endif


%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
BuildRequires:    python3 python3-devel
Requires:         python3
Requires:  MCFM
BuildRequires: MCFM
%endif

%if 0%{?suse_version}
BuildRequires:    python3 python3-devel
Requires:         python3
Requires:  MCFM
BuildRequires: MCFM
%endif


Prefix: %{_prefix}

%description
Sherpa is a Monte Carlo event generator for the Simulation of High-Energy 
Reactions of PArticles in lepton-lepton, lepton-photon, photon-photon, 
lepton-hadron and hadron-hadron collisions. Simulation programs - also 
dubbed event generators - like Sherpa are indispensable work horses for 
current particle physics phenomenology and are (at) the interface between 
theory and experiment.  


%package  devel
Summary:        Libraries and headers for %{name}
Provides:       %{name}-devel = %{version}-%{release}

%description devel
%{libnamedev} contains the libraries and header files needed to
develop programs which make use of %{name}.
The library documentation is available on header files.

%package  common
Summary:        Libraries and headers for %{name}

Provides:       %{name}-common = %{version}-%{release}

%description common
Contains the common files and header files needed to
develop programs which make use of %{name}.
The library documentation is available on header files.





%package  openmpi
Summary:        Libraries and headers for %{name}

Provides:       %{name}-openmpi = %{version}-%{release}

%description openmpi
Contains the common files and header files needed to
develop programs which make use of %{name}.
The library documentation is available on header files.

%package  openmpi-devel
Summary:        Libraries and headers for %{name}

Provides:       %{name}-openmpi-devel = %{version}-%{release}

%description openmpi-devel
%{libnamedev} contains the libraries and header files needed to
develop programs which make use of %{name}.
The library documentation is available on header files.

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0}
%package -n python3-%{name}
Summary:   %{name}  Python 3 bindings
%{?python_provide:%python_provide python3-%{name}}
%description -n python3-%{name}
This package provides the Python 3 bindings for %{name}


%package -n python3-%{name}-openmpi
Summary:   %{name}  Python 3 bindings
%description -n python3-%{name}-openmpi
This package provides the Python 2 bindings for %{name}-openmpi
%endif


%{!?_openmpi_load: %define _openmpi_load() \
 . /etc/profile.d/modules.sh; \
 module load mpi/openmpi-%{_arch}; \
 export CFLAGS="$CFLAGS %{optflags}"; \
} 
%{!?_openmpi_unload: %define _openmpi_unload() \
 . /etc/profile.d/modules.sh; \
 module unload mpi/openmpi-%{_arch}; \
}



%prep
%setup -q -n sherpa-v%{version}
%patch -P 0 -p1

%build
# Build serial version, dummy arguments

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
%cmake -DSHERPA_ENABLE_ANALYSIS:BOOL=ON -DSHERPA_ENABLE_BINRELOC:BOOL=ON -DSHERPA_ENABLE_BLACKHAT:BOOL=ON \
-DSHERPA_ENABLE_DIHIGGS:BOOL=OFF -DSHERPA_ENABLE_EXAMPLES:BOOL=ON  \
-DSHERPA_ENABLE_GZIP:BOOL=ON -DSHERPA_ENABLE_HEPMC3:BOOL=ON -DSHERPA_ENABLE_HEPMC3_ROOT:BOOL=ON  \
-DSHERPA_ENABLE_INTERNAL_PDFS:BOOL=ON -DSHERPA_ENABLE_INSTALL_LIBZIP:BOOL=OFF -DSHERPA_ENABLE_INSTALL_LHAPDF:BOOL=OFF \
 -DSHERPA_ENABLE_LHOLE:BOOL=ON -DSHERPA_ENABLE_MADLOOP:BOOL=ON -DSHERPA_ENABLE_MCFM:BOOL=ON \
  -DSHERPA_ENABLE_MPI:BOOL=OFF -DSHERPA_ENABLE_OPENLOOPS:BOOL=ON -DSHERPA_ENABLE_PYTHIA8:BOOL=ON -DSHERPA_ENABLE_PYTHON:BOOL=ON \
   -DSHERPA_ENABLE_RECOLA:BOOL=ON -DSHERPA_ENABLE_RIVET:BOOL=ON -DSHERPA_ENABLE_ROOT:BOOL=ON -DSHERPA_ENABLE_THREADING:BOOL=ON \
  -DSHERPA_ENABLE_UFO:BOOL=ON -DSHERPA_ENABLE_EWSUD:BOOL=ON \
  -DSHERPA_ENABLE_GOSAM:BOOL=OFF -DSHERPA_ENABLE_MANUAL:BOOL=OFF -S .


%{_openmpi_load}
mkdir $MPI_COMPILER; 
cd $MPI_COMPILER
%cmake -DSHERPA_ENABLE_ANALYSIS:BOOL=ON -DSHERPA_ENABLE_BINRELOC:BOOL=ON -DSHERPA_ENABLE_BLACKHAT:BOOL=ON \
-DSHERPA_ENABLE_DIHIGGS:BOOL=OFF -DSHERPA_ENABLE_EXAMPLES:BOOL=ON  \
-DSHERPA_ENABLE_GZIP:BOOL=ON -DSHERPA_ENABLE_HEPMC3:BOOL=ON -DSHERPA_ENABLE_HEPMC3_ROOT:BOOL=ON  \
-DSHERPA_ENABLE_INTERNAL_PDFS:BOOL=ON -DSHERPA_ENABLE_INSTALL_LIBZIP:BOOL=OFF -DSHERPA_ENABLE_INSTALL_LHAPDF:BOOL=OFF \
 -DSHERPA_ENABLE_LHOLE:BOOL=ON -DSHERPA_ENABLE_MADLOOP:BOOL=ON -DSHERPA_ENABLE_MCFM:BOOL=ON \
  -DSHERPA_ENABLE_MPI:BOOL=OFF -DSHERPA_ENABLE_OPENLOOPS:BOOL=ON -DSHERPA_ENABLE_PYTHIA8:BOOL=ON -DSHERPA_ENABLE_PYTHON:BOOL=ON \
   -DSHERPA_ENABLE_RECOLA:BOOL=ON -DSHERPA_ENABLE_RIVET:BOOL=ON -DSHERPA_ENABLE_ROOT:BOOL=ON -DSHERPA_ENABLE_THREADING:BOOL=ON \
  -DSHERPA_ENABLE_UFO:BOOL=ON -DSHERPA_ENABLE_EWSUD:BOOL=ON \
  -DSHERPA_ENABLE_GOSAM:BOOL=OFF -DSHERPA_ENABLE_MANUAL:BOOL=OFF -S ../  \
  -DCMAKE_INSTALL_PREFIX=$MPI_HOME
cd ..
%{_openmpi_unload}

%cmake_build

%{_openmpi_load}
cd $MPI_COMPILER
%cmake_build
cd ..
%{_openmpi_unload}


%else
LDFLAGS="$(echo $LDFLAGS | sed 's/-Wl,--no-undefined//')"
export LDFLAGS
TOP=$(pwd)
%cmake -DSHERPA_ENABLE_ANALYSIS:BOOL=ON -DSHERPA_ENABLE_BINRELOC:BOOL=ON -DSHERPA_ENABLE_BLACKHAT:BOOL=ON \
-DSHERPA_ENABLE_DIHIGGS:BOOL=OFF -DSHERPA_ENABLE_EXAMPLES:BOOL=ON  \
-DSHERPA_ENABLE_GZIP:BOOL=ON -DSHERPA_ENABLE_HEPMC3:BOOL=ON -DSHERPA_ENABLE_HEPMC3_ROOT:BOOL=OFF  \
-DSHERPA_ENABLE_INTERNAL_PDFS:BOOL=ON -DSHERPA_ENABLE_INSTALL_LIBZIP:BOOL=OFF -DSHERPA_ENABLE_INSTALL_LHAPDF:BOOL=OFF \
 -DSHERPA_ENABLE_LHOLE:BOOL=ON -DSHERPA_ENABLE_MADLOOP:BOOL=ON -DSHERPA_ENABLE_MCFM:BOOL=ON \
  -DSHERPA_ENABLE_MPI:BOOL=OFF -DSHERPA_ENABLE_OPENLOOPS:BOOL=ON -DSHERPA_ENABLE_PYTHIA8:BOOL=ON -DSHERPA_ENABLE_PYTHON:BOOL=ON \
   -DSHERPA_ENABLE_RECOLA:BOOL=ON -DSHERPA_ENABLE_RIVET:BOOL=ON -DSHERPA_ENABLE_ROOT:BOOL=ON -DSHERPA_ENABLE_THREADING:BOOL=ON \
  -DSHERPA_ENABLE_UFO:BOOL=ON -DSHERPA_ENABLE_EWSUD:BOOL=ON \
  -DSHERPA_ENABLE_GOSAM:BOOL=OFF -DSHERPA_ENABLE_MANUAL:BOOL=OFF \
  '-DCMAKE_EXE_LINKER_FLAGS=-Wl,--as-needed -Wl,-z,now' '-DCMAKE_MODULE_LINKER_FLAGS= -Wl,--as-needed' '-DCMAKE_SHARED_LINKER_FLAGS= -Wl,--as-needed -Wl,-z,now'  \
  -S $TOP 

%cmake_build


#{_openmpi_load}
%setup_openmpi
mkdir $TOP/$MPI_COMPILER; 
cd $TOP/$MPI_COMPILER
%cmake -DSHERPA_ENABLE_ANALYSIS:BOOL=ON -DSHERPA_ENABLE_BINRELOC:BOOL=ON -DSHERPA_ENABLE_BLACKHAT:BOOL=ON \
-DSHERPA_ENABLE_DIHIGGS:BOOL=OFF -DSHERPA_ENABLE_EXAMPLES:BOOL=ON  \
-DSHERPA_ENABLE_GZIP:BOOL=ON -DSHERPA_ENABLE_HEPMC3:BOOL=ON -DSHERPA_ENABLE_HEPMC3_ROOT:BOOL=OFF  \
-DSHERPA_ENABLE_INTERNAL_PDFS:BOOL=ON -DSHERPA_ENABLE_INSTALL_LIBZIP:BOOL=OFF -DSHERPA_ENABLE_INSTALL_LHAPDF:BOOL=OFF \
 -DSHERPA_ENABLE_LHOLE:BOOL=ON -DSHERPA_ENABLE_MADLOOP:BOOL=ON -DSHERPA_ENABLE_MCFM:BOOL=ON \
  -DSHERPA_ENABLE_MPI:BOOL=OFF -DSHERPA_ENABLE_OPENLOOPS:BOOL=ON -DSHERPA_ENABLE_PYTHIA8:BOOL=ON -DSHERPA_ENABLE_PYTHON:BOOL=ON \
   -DSHERPA_ENABLE_RECOLA:BOOL=ON -DSHERPA_ENABLE_RIVET:BOOL=ON -DSHERPA_ENABLE_ROOT:BOOL=ON -DSHERPA_ENABLE_THREADING:BOOL=ON \
  -DSHERPA_ENABLE_UFO:BOOL=ON -DSHERPA_ENABLE_EWSUD:BOOL=ON \
  -DSHERPA_ENABLE_GOSAM:BOOL=OFF -DSHERPA_ENABLE_MANUAL:BOOL=OFF -S $TOP  \
  -DCMAKE_INSTALL_PREFIX=$MPI_HOME 

%cmake_build
cd $TOP
#{_openmpi_unload}


%endif



%install
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
%cmake_install
mkdir -p %{buildroot}%{_sysconfdir}/ld.so.conf.d
echo %{_libdir}/%{name} >   %{buildroot}%{_sysconfdir}/ld.so.conf.d/%{name}-%{_arch}.conf
%py3_shebang_fix %{buildroot}//usr/share/SHERPA-MC/plot_graphs


%{_openmpi_load}
cd $MPI_COMPILER
%cmake_install
%py3_shebang_fix %{buildroot}/$MPI_HOME/share/SHERPA-MC/plot_graphs
rm -rf %{buildroot}/$MPI_HOME/share
cd ..
%{_openmpi_unload}


rm -rf $RPM_BUILD_ROOT/usr/share/info/dir
export QA_RPATHS=3
%else
TOP=$(pwd)
%cmake_install
mkdir -p %{buildroot}%{_sysconfdir}/ld.so.conf.d
echo %{_libdir}/%{name} >   %{buildroot}%{_sysconfdir}/ld.so.conf.d/%{name}-%{_arch}.conf
%py3_shebang_fix %{buildroot}//usr/share/SHERPA-MC/plot_graphs

%setup_openmpi
#{_openmpi_load}
cd $TOP/$MPI_COMPILER
%cmake_install
%py3_shebang_fix %{buildroot}/$MPI_HOME/share/SHERPA-MC/plot_graphs
rm -rf %{buildroot}/$MPI_HOME/share
cd ..
#{_openmpi_unload}


rm -rf $RPM_BUILD_ROOT/usr/share/info/dir
export QA_RPATHS=3

%endif


%post -p /sbin/ldconfig    
%postun -p /sbin/ldconfig

%files -n %{libname}
%{_libdir}/SHERPA-MC/*.so*
#{_libdir}/SHERPA-MC/*a
%{_bindir}/*
%{_sysconfdir}/ld.so.conf.d/%{name}-%{_arch}.conf


%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0}
%files  -n python3-%{name}
#{python3_sitearch}/*Sherpa*
/usr/lib/python%{python3_version}/site-packages/*


%files  -n python3-%{name}-openmpi
/usr/%_lib/openmpi/lib/python%{python3_version}/site-packages/*
%endif

%files -n %{sharename}
/usr/share/SHERPA-MC/*

%files -n %{libnamedev}
%{_includedir}/SHERPA-MC/*


%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0}
%files  -n %{openmpiname} 
/usr/%_lib/openmpi/%_lib/SHERPA-MC/*
/usr/%_lib/openmpi/bin/*

%files -n %{openmpiname}-devel
/usr/%_lib/openmpi/include/SHERPA-MC/*
%endif


%changelog
* Mon Nov 29 2021 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
- Bump to 3.0.0alpha1
* Sun Aug 01 2021 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
  - RPATH
* Wed May 26 2021 Andrii Verbytskyi 2.2.11
- Added ldconfig scripts
* Thu Nov 23 2017 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
+ Recola
* Thu May 26 2016 Andrii Verbytskyi 2.2.0
+ Initial spec file

