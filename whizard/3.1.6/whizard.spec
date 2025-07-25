%global __brp_check_rpaths %{nil}
%if 0%{?suse_version}
%{!?python3_pkgversion:%global python3_pkgversion 3}
%endif
%undefine _debugsource_packages
Summary:  Multipurpose Monte Carlo Event Generator for High Energy Physics
Name: whizard
Version: 3.1.6
Release: 2%{?dist}
License: GPLv2
Group: System Environment/Libraries
Source: https://www.hepforge.org/archive/whizard/whizard-%{version}.tar.gz
URL:    https://whizard.hepforge.org/
Patch0:         patch-whizard-0.txt

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
BuildRequires:  python3-devel
%endif
%if %{?rhel}%{!?rhel:0} == 8
BuildRequires:  python36-rpm-macros
%endif
%if 0%{?suse_version}
BuildRequires:  python3-devel python-rpm-macros
%endif
######
%if %{?rhel}%{!?rhel:0} >= 8
BuildRequires: python3 python3-devel  platform-python-devel python3-Cython
%endif
%if %{?fedora}%{!?fedora:0}
%if %{?fedora}%{!?fedora:0} >= 35
BuildRequires: python-setuptools
%endif
BuildRequires:   Cython python3 python3-devel python3-libs 
%endif
%if 0%{?suse_version}
BuildRequires: python3-Cython python3-devel  python3-setuptools
%endif
####
Requires: recola
BuildRequires: recola

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0}
Requires: fastjet hoppet HepMC3 HepMC3-rootIO gosam openloops ocaml  pythia8 LCIO TAUOLA hdf5
BuildRequires: fastjet hoppet HepMC3 HepMC3-search HepMC3-rootIO openloops ocaml  lhapdf
BuildRequires: fastjet-devel HepMC3-devel HepMC3-rootIO-devel HepMC3-search-devel gosam  lhapdf-devel  autoconf automake libtool
BuildRequires: pythia8 pythia8-devel  libtirpc  libtirpc-devel lhapdf-sets-whizard
BuildRequires: root  python3 python3-devel  openmpi openmpi-devel noweb LCIO LCIO-devel TAUOLA TAUOLA-devel hdf5 hdf5-devel
BuildRequires: hdf5-devel
#Fails for RH
%if %{?fedora}%{!?fedora:0}|| %{?rhel}%{!?rhel:0}
BuildRequires:  hevea
BuildRequires:  tex(latex) 
%endif

Requires: LoopTools
BuildRequires:  LoopTools
BuildRequires: python3-lhapdf gcc-gfortran gcc-c++ 

%if %{?fedora}%{!?fedora:0} >=34
BuildRequires: chrpath
%endif

%if %{?rhel}%{!?rhel:0} == 8 
BuildRequires:   autoconf binutils automake libtool
%global _use_internal_dependency_generator 0
%global __find_provides /usr/lib/rpm/ocaml-find-provides.sh
%global __find_requires /usr/lib/rpm/ocaml-find-requires.sh
%else
%global _use_internal_dependency_generator 1
%endif
%endif


%if 0%{?suse_version}
Requires: fastjet hoppet libHepMC3-4  gosam openloops ocaml  gosam  libpythia8 LCIO  libLHAPDF TAUOLA hdf5
BuildRequires: fastjet hoppet libHepMC3-4 gosam openloops ocaml  libLHAPDF LHAPDF-devel
BuildRequires: fastjet-devel HepMC3-devel  LHAPDF-devel  autoconf automake libtool
BuildRequires: libpythia8 pythia-devel   lhapdf-sets-whizard
BuildRequires: root6 root6-libs root6-devel   python3-devel  openmpi openmpi-devel noweb LCIO LCIO-devel TAUOLA TAUOLA-devel hdf5 hdf5-devel
Requires: LoopTools 
BuildRequires:  LoopTools  hevea
BuildRequires: python3-LHAPDF gcc-fortran gcc-c++  libtirpc-devel libboost_iostreams-devel
#libtirpc  
BuildRequires:  tex(latex)
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


%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
%package  -n python%{python3_pkgversion}-%{name}
Summary:        python bindings for %{name}
Provides:       python%{python3_pkgversion}-%{name} = %{version}-%{release}

%description -n python%{python3_pkgversion}-%{name}
python%{python3_pkgversion}-%{name} contains python bindings for %{name}.
%endif


#if 0{?suse_version}
#package  -n python3-#{name}
#Summary:        python bindings for #{name}
#Provides:       python3-#{name} = #{version}-#{release}

#description -n python3-#{name}
#python-#{name} contains python bindings for #{name}.
#endif




%prep 
%setup -q
%patch -P 0 -p1

%build 

###
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
export PYTHON=%{_bindir}/python3
%endif
#if 0#{?suse_version}
#export PYTHON_VERSION=#{py3_ver}
#endif
###
autoreconf --force --install --verbose .

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
%global optflags %{optflags} -Wno-incompatible-pointer-types -Wno-int-conversion
FC_OPTFLAGS=`echo "%optflags" | sed -e 's/-mtune=[^ ]\+//'  -e 's@-flto=auto@@g' -e 's@-ffat-lto-objects@@g'  -e 's@-specs=/usr/lib/rpm/redhat/redhat-annobin-cc1@@g' -e 's@-specs=/usr/lib/rpm/redhat/redhat-hardened-cc1@@g'  -e 's@-Werror=format-security@@g' `
#-flto=auto -ffat-lto-objects 
%if %{?fedora}%{!?fedora:0} >=34 || %{?rhel}%{!?rhel:0} >= 8
export CXXFLAGS="$FC_OPTFLAGS -Wno-error -std=c++1z -Wno-error=format-security -Wno-incompatible-pointer-types"
export FFLAGS="$FC_OPTFLAGS -Wno-error -fallow-argument-mismatch "
export FCFLAGS="$FC_OPTFLAGS -Wno-error -fallow-argument-mismatch "
%if %{?rhel}%{!?rhel:0} == 8
export FFLAGS="$FC_OPTFLAGS -Wno-error "
export FCFLAGS="$FC_OPTFLAGS -Wno-error "
%endif
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


%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 9
%if %{?fedora}%{!?fedora:0} > 42 || %{?rhel}%{!?rhel:0} > 9
export LDFLAGS="$LDFLAGS -Wl,-z,noexecstack"
%endif
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
#    --enable-python             

%else
%configure --disable-dependency-tracking  --enable-fc-openmp  --enable-fc-quadruple     \
    --disable-ocaml  \
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
#    --enable-python     
%endif

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

#    --enable-python     
## cteq6l1, CT10
%make_build
%endif

%install 
%make_install
rm -f $RPM_BUILD_ROOT%{_libdir}/whizard/install_files.txt
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

#if #{?fedora}#{!?fedora:0}  || #{?rhel}#{!?rhel:0} >= 8
#files -n python#{python3_pkgversion}-#{name}
#{python3_sitearch}/*
#endif


#if 0#{?suse_version}
#files -n python3-#{name}
#/usr/#_lib/python*/site-packages/*
#endif


%clean
rm -rf %{buildroot}

%post 
ldconfig 

%changelog
* Mon Dec 30 2024 Andrii Verbytskyi 3.1.5
- Update to 3.1.5
* Fri Nov 10 2023 Andrii Verbytskyi 3.1.3
- Update to 3.1.3
* Fri Mar 10 2023 Andrii Verbytskyi 3.1.0
- Update to 3.1.0
* Wed Jan 19 2022 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - Version 3.0.2 
* Wed Jul 28 2021 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - Version 3.0.1 and using chrpath
* Wed May 26 2021 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - Version 3.0.0
* Thu Nov 23 2017 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - Initial
