%undefine _debugsource_packages
Summary:  Multipurpose Monte Carlo Event Generator for High Energy Physics
Name: whizard
Version: 3.0.3
Release: 2%{?dist}
License: GPLv2
Group: System Environment/Libraries
Source: https://www.hepforge.org/archive/whizard/whizard-%{version}.tar.gz
URL:    https://whizard.hepforge.org/
Patch0:         patch-whizard-0.txt

Requires: recola
BuildRequires: recola

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0}
Requires: fastjet hoppet HepMC3 HepMC3-rootIO gosam openloops ocaml  pythia8 LCIO TAUOLA hdf5
BuildRequires: fastjet hoppet HepMC3 HepMC3-search HepMC3-rootIO openloops ocaml  lhapdf
BuildRequires: fastjet-devel HepMC3-devel HepMC3-rootIO-devel HepMC3-search-devel gosam  lhapdf-devel  autoconf automake libtool
BuildRequires: pythia8 pythia8-devel  libtirpc  libtirpc-devel lhapdf-sets-whizard
BuildRequires: root  python3 python3-devel  openmpi openmpi-devel noweb LCIO LCIO-devel TAUOLA TAUOLA-devel hdf5 hdf5-devel

#Fails for RH
%if %{?fedora}%{!?fedora:0}
BuildRequires:  hevea 
%endif

Requires: LoopTools
BuildRequires:  LoopTools
BuildRequires: python3-lhapdf gcc-gfortran gcc-c++ 

%if %{?fedora}%{!?fedora:0} >=34
BuildRequires: chrpath
%endif

%if %{?rhel}%{!?rhel:0} || %{?fedora}%{!?fedora:0} <= 31
%global _use_internal_dependency_generator 0
%global __find_provides /usr/lib/rpm/ocaml-find-provides.sh
%global __find_requires /usr/lib/rpm/ocaml-find-requires.sh
%else
%global _use_internal_dependency_generator 1
%endif
%endif


%if 0%{?suse_version}
Requires: fastjet hoppet libHepMC3-3  gosam openloops ocaml  gosam  libpythia8 LCIO  libLHAPDF TAUOLA hdf5
BuildRequires: fastjet hoppet libHepMC3-3 gosam openloops ocaml  libLHAPDF LHAPDF-devel
BuildRequires: fastjet-devel HepMC3-devel  LHAPDF-devel  autoconf automake libtool
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

%build 
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8

FC_OPTFLAGS=`echo "%optflags" | sed -e 's/-mtune=[^ ]\+//'  -e 's@-specs=/usr/lib/rpm/redhat/redhat-annobin-cc1@@g' -e 's@-specs=/usr/lib/rpm/redhat/redhat-hardened-cc1@@g'  -e 's@-Werror=format-security@@g' `

%if %{?fedora}%{!?fedora:0} >=34 || %{?rhel}%{!?rhel:0} > 8
export CXXFLAGS="$FC_OPTFLAGS -Wno-error -std=c++1z -Wno-error=format-security "
export FFLAGS="$FC_OPTFLAGS -Wno-error -fallow-argument-mismatch "
export FCFLAGS="$FC_OPTFLAGS -Wno-error -fallow-argument-mismatch "
export LDFLAGS=" "
%else
export CXXFLAGS="$FC_OPTFLAGS -Wno-error -std=c++1y -Wno-error=format-security "
export FFLAGS=$FC_OPTFLAGS 
export FCFLAGS="$FC_OPTFLAGS   -Wno-error"
export LDFLAGS=" "
%endif

echo $FCFLAGS
echo $FFFLAGS
echo $FC_OPTFLAGS

export CFLAGS="$FC_OPTFLAGS -Wno-error -Wno-error=format-security "

autoreconf --force --install --verbose .
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

## cteq6l1, CT10
%make_build
%endif



%if 0%{?suse_version}
autoreconf --force --install --verbose .
export CXXFLAGS="%{optflags} -Wno-error -std=c++1y -Wno-error=format-security "
export CFLAGS="%{optflags} -Wno-error -Wno-error=format-security "
export FFLAGS="%{optflags} -Wno-error -fallow-argument-mismatch"
export FCFLAGS="%{optflags} -Wno-error -fallow-argument-mismatch"

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

## cteq6l1, CT10
%make_build
%endif

%install 
%make_install
%if %{?fedora}%{!?fedora:0} >=34
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/whizard
chrpath --delete $RPM_BUILD_ROOT%{_libdir}/libwhizard.so*
%endif

%files
%defattr(-,root,root)
%{_bindir}/*
/usr/%_lib/*
/usr/share/*
/usr/lib/mod/*
/usr/include/*

%clean
rm -rf %{buildroot}

%post 
ldconfig 

%changelog
* Wed Jan 19 2022 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - Version 3.0.2 
* Wed Jul 28 2021 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - Version 3.0.1 and using chrpath
* Wed May 26 2021 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - Version 3.0.0
* Thu Nov 23 2017 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - Initial
