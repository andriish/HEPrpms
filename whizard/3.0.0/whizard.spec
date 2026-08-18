%undefine _debugsource_packages
Summary:  Multipurpose Monte Carlo Event Generator for High Energy Physics
Name: whizard
Version: 3.0.0
Release: 1%{?dist}
License: GPLv2
Group: System Environment/Libraries
Source: https://www.hepforge.org/archive/whizard/whizard-%{version}.tar.gz
URL:    https://whizard.hepforge.org/
Patch0:         patch-whizard-0.txt
Patch1:         patch-whizard-1.txt


%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0}
Requires: fastjet hoppet HepMC3 HepMC3-rootIO gosam openloops ocaml recola2 pythia8 LCIO TAUOLA hdf5
BuildRequires: fastjet hoppet HepMC3 HepMC3-search HepMC3-rootIO openloops ocaml  lhapdf
BuildRequires: fastjet-devel HepMC3-devel HepMC3-rootIO-devel HepMC3-search-devel gosam recola2 lhapdf-devel  autoconf automake libtool
BuildRequires: pythia8 pythia8-devel  libtirpc  libtirpc-devel lhapdf-sets-whizard
BuildRequires: root  python3 python3-devel  openmpi openmpi-devel noweb LCIO LCIO-devel TAUOLA TAUOLA-devel hdf5 hdf5-devel

#Fails for RH
%if %{?fedora}%{!?fedora:0}
BuildRequires:  hevea 
%endif

Requires: LoopTools
BuildRequires:  LoopTools

BuildRequires: python3-lhapdf gcc-gfortran gcc-c++ 


%if %{?rhel}%{!?rhel:0} || %{?fedora}%{!?fedora:0} <= 31
%global _use_internal_dependency_generator 0
%global __find_provides /usr/lib/rpm/ocaml-find-provides.sh
%global __find_requires /usr/lib/rpm/ocaml-find-requires.sh
%else
%global _use_internal_dependency_generator 1
%endif

%endif


%if 0%{?suse_version}
Requires: fastjet hoppet libHepMC3-1  gosam openloops ocaml  gosam recola libpythia8 LCIO  libLHAPDF TAUOLA hdf5
BuildRequires: fastjet hoppet libHepMC3-1 gosam openloops ocaml  libLHAPDF LHAPDF-devel
BuildRequires: fastjet-devel HepMC3-devel recola LHAPDF-devel  autoconf automake libtool
BuildRequires: libpythia8 pythia-devel   lhapdf-sets-whizard
BuildRequires: root6 root6-libs root6-devel   python3-devel  openmpi openmpi-devel noweb LCIO LCIO-devel TAUOLA TAUOLA-devel hdf5 hdf5-devel
Requires: LoopTools 
BuildRequires:  LoopTools  hevea
BuildRequires: python3-LHAPDF gcc-fortran gcc-c++  libtirpc-devel libboost_iostreams-devel
#libtirpc  
%endif


%description
 WHIZARD is a program system designed for the efficient calculation of multi-particle 
 scattering cross sections and simulated event samples.
Tree-level matrix elements are generated automatically for arbitrary partonic 
processes by using the Optimized Matrix Element Generator O'Mega. Matrix elements 
obtained by alternative methods (e.g., including loop corrections) may be interfaced
 as well. The program is able to calculate numerically stable signal and background
  cross sections and generate unweighted event samples with reasonable efficiency 
  for processes with up to eight final-state particles; more particles are possible.
   For more particles, there is the option to generate processes as decay cascades 
   including complete spin correlations. Different options for QCD parton showers are available. 
%prep 
%setup -q
%patch0 -p1
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0}
%patch1 -p1
%endif

%build 
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
autoreconf --force --install --verbose .
export CXXFLAGS="%{optflags} -Wno-error -std=c++1y -Wno-error=format-security "
export CFLAGS="%{optflags} -Wno-error -Wno-error=format-security"
export FFLAGS="%{optflags} -Wno-error "
export FCLAGS="%{optflags} -Wno-error "

%configure --disable-dependency-tracking  --enable-fc-openmp  --enable-fc-quadruple     \
    --enable-recola     --with-recola=/usr/%_lib \
    --enable-gosam      --with-gosam=/usr  --with-golem=/usr  --with-ninja=/usr --with-samurai=/usr \
    --enable-fastjet    --with-fastjet=/usr \
    --enable-hoppet     --with-hoppet=/usr  \
    --enable-lhapdf     --with-lhapdf=/usr \
    --enable-hepmc      --with-hepmc=/usr \
    --enable-hoppet     --with-hoppet=/usr     \
    --enable-lcio       --with-lcio=/usr \
    --enable-pythia8    --with-pythia8=/usr \
    --enable-openloops  --with-openloops=/usr/%_lib/openloops  \
    --enable-looptools  --with-looptools=/usr  --with-mpi-lib=openmpi
    
#cat   src/system/system_dependencies.f90
#sed -i 's|       \&$||' src/system/system_dependencies.f90

#sed -i 's|\&\&|\&|g' src/system/system_dependencies.f90
#cat   src/system/system_dependencies.f90


## cteq6l1, CT10
%make_build
%endif



%if 0%{?suse_version}
autoreconf --force --install --verbose .
export CXXFLAGS="%{optflags} -Wno-error -std=c++1y -Wno-error=format-security "
export CFLAGS="%{optflags} -Wno-error -Wno-error=format-security"
export FFLAGS="%{optflags} -Wno-error "
export FCLAGS="%{optflags} -Wno-error "

%configure --disable-dependency-tracking  --enable-fc-openmp  --enable-fc-quadruple     \
    --enable-recola     --with-recola=/usr/%_lib \
    --enable-gosam      --with-gosam=/usr  --with-golem=/usr  --with-ninja=/usr --with-samurai=/usr \
    --enable-fastjet    --with-fastjet=/usr \
    --enable-hoppet     --with-hoppet=/usr  \
    --enable-lhapdf     --with-lhapdf=/usr \
    --enable-hepmc      --with-hepmc=/usr \
    --enable-hoppet=yes     --with-hoppet=/usr     \
    --enable-lcio       --with-lcio=/usr \
    --enable-pythia8    --with-pythia8=/usr \
    --enable-openloops  --with-openloops=/usr/%_lib/openloops  \
    --enable-looptools  --with-looptools=/usr  --with-mpi-lib=openmpi
    
#cat   src/system/system_dependencies.f90
#sed -i 's|       \&$||' src/system/system_dependencies.f90

#sed -i 's|\&\&|\&|g' src/system/system_dependencies.f90
#cat   src/system/system_dependencies.f90


## cteq6l1, CT10
%make_build
%endif

%install 
%make_install
%files
%defattr(-,root,root)
/usr/bin/*
/usr/%_lib/*
/usr/share/*
/usr/lib/mod/*
/usr/include/*

%clean
rm -rf %{buildroot}

%post 
ldconfig 

%changelog
* Wed May 26 2021 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - Version 3.0.0
* Thu Nov 23 2017 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - Initial
