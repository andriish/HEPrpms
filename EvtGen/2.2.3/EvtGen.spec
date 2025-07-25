%define major       0
%define libname     EvtGen
%define libnamedev  EvtGen-devel
%define libnamedoc  EvtGen-doc
%define develnamestatic  EvtGen-static-devel

Name:           EvtGen
Version:        2.2.3
Release:        5%{?dist}
License:        GPLv3
Url:            http://evtgen.warwick.ac.uk
Source0:        https://evtgen.hepforge.org/downloads/EvtGen-02.02.03.tar.gz
Patch0:         patch-EvtGen-0.txt

Summary:        EvtGen is a Monte Carlo event generator
BuildRequires:  PHOTOS >= 3.62 
BuildRequires:  PHOTOS-devel >= 3.62 
BuildRequires:  TAUOLA >= 1.1.7
BuildRequires:  TAUOLA-devel >= 1.1.7
Requires:       PHOTOS >= 3.62 
Requires:       TAUOLA >= 1.1.7
BuildRequires:  tex(latex) tex(epsf.sty) ghostscript
BuildRequires:  doxygen
BuildRequires:  gcc-c++



%if 0%{?rhel} || 0%{?fedora}
BuildRequires:  pythia8-devel HepMC HepMC-devel HepMC3 HepMC3-devel HepMC3-search HepMC3-search-devel
Requires:       pythia8 libgfortran HepMC HepMC3 HepMC3-search
BuildRequires:  root-core
BuildRequires:  root-hist
BuildRequires:  root-io
BuildRequires:  root-tree
%endif

%if 0%{?suse_version}
BuildRequires:  pythia-devel HepMC2-devel HepMC3-devel pythia-doc
Requires:       libpythia8  libHepMC4 libHepMC3-4  root6-libs  root6-utils pythia-doc
BuildRequires:  root6 root6-libs root6-devel root6-config  root6-utils
%endif


%if %{?rhel}%{!?rhel:0} >= 8 || %{?fedora}%{!?fedora:0} >= 31
BuildRequires: ghostscript-tools-dvipdf
%endif

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
BuildRequires: gcc-c++ gcc gcc-gfortran
%endif

BuildRequires: cmake >= 3.0.0

Prefix: %{_prefix}

%description
EvtGen is a Monte Carlo event generator that simulates the decays of 
heavy flavour particles, primarily B and D mesons. It contains a range
of decay models for intermediate and final states containing scalar, 
vector and tensor mesons or resonances, as well as leptons, photons 
and baryons. Decay amplitudes are used to generate each branch of a 
given full decay tree, taking into account angular and time-dependent 
correlations which allows for the simulation of CP-violating
processes. Originally written by Anders Ryd and David Lange, this 
package is used by many particle physics experiments worldwide,
including BaBar, Belle(-II), CLEO(-c), CDF, D0, LHCb, ATLAS and CMS.
The maintenance and development of the package is now performed by 
the particle physics group at the University of Warwick (in 
particular by John Back, Thomas Latham and Michal Kreps). 

%package  devel
Summary:        Libraries and headers for %{name}
Provides:       %{name}-devel = %{version}-%{release}

%description devel
%{libnamedev} contains the libraries and header files needed to
develop programs which make use of %{name}.
The library documentation is available on header files.


%package  doc
Summary:        Documentation for %{name}
Provides:       %{name}-doc = %{version}-%{release}
%description doc
%{libnamedoc} contains the documentation for %{name}.


%prep
%setup -q -n EvtGen/R02-02-03
%patch -P 0 -p1

%build
mkdir -p build

%if %{?rhel}%{!?rhel:0} >= 8 || %{?fedora}%{!?fedora:0}
export CXXFLAGS="-O2 -g -pipe -Wall -Werror=format-security  -m64"
export CFLAGS="-O2 -g -pipe -Wall -Werror=format-security  -m64"
export LDFLAGS=" "
%if %{?fedora}%{!?fedora:0} >= 34 || %{?rhel}%{!?rhel:0} >= 8
export CMAKE_CXX_STANDARD=17
%endif

%cmake -H. -Bbuild -DCMAKE_SKIP_RPATH=ON \
 -DEVTGEN_PYTHIA:BOOL=ON \
 -DEVTGEN_PHOTOS:BOOL=ON \
 -DEVTGEN_TAUOLA:BOOL=ON \
 -DEVTGEN_HEPMC3:BOOL=ON  \
 -DEVTGEN_BUILD_DOC:BOOL=ON  \
 -DEVTGEN_BUILD_VALIDATIONS:BOOL=ON
make -C build %{?_smp_mflags} 
%endif

%if 0%{?suse_version}
export CXXFLAGS="-O2 -g -pipe -Wall -Werror=format-security  -m64"
export CFLAGS="-O2 -g -pipe -Wall -Werror=format-security  -m64"
export LDFLAGS=" "
export CMAKE_CXX_STANDARD=17
%cmake  -DCMAKE_SKIP_RPATH=ON \
 -DEVTGEN_PYTHIA:BOOL=ON \
 -DEVTGEN_PHOTOS:BOOL=ON \
 -DEVTGEN_TAUOLA:BOOL=ON \
 -DEVTGEN_HEPMC3:BOOL=ON  \
 -DEVTGEN_BUILD_DOC:BOOL=ON  \
 -DEVTGEN_BUILD_VALIDATIONS:BOOL=ON  -DCMAKE_SHARED_LINKER_FLAGS=" "
make %{?_smp_mflags} SHELL='sh -x'
%endif

%install
%if %{?rhel}%{!?rhel:0}  || %{?fedora}%{!?fedora:0}
%make_install -C build
%endif
%if 0%{?suse_version}
%make_install -C build
%endif


mv %{buildroot}%{_datadir}/EvtGen/*.*  %{buildroot}%{_datadir}/

%files -n %{libname}
/usr/%_lib/*

%files -n %{libnamedev}
%{_includedir}/*
%{_datadir}/*.*
%{_datadir}/%{name}/cmake/*

%files -n  %{libnamedoc}
%{_datadir}/%{name}/AUTHORS
%{_datadir}/%{name}/COPYING
%{_datadir}/%{name}/validation/*
%if %{?rhel}%{!?rhel:0}  || %{?fedora}%{!?fedora:0}
%{_prefix}/share/doc/EvtGen/guide.ps
%{_prefix}/share/doc/EvtGen/*md
%endif
%if 0%{?suse_version}
%{_prefix}/share/doc/packages/EvtGen/guide.ps
%{_prefix}/share/doc/packages/EvtGen/*md
%endif

%changelog
* Tue Jul 12 2022 Andrii Verbytskyi 2.2.0
+ 2.2.0
* Thu Oct 28 2021 Andrii Verbytskyi 2.1.1
+ 2.1.1
* Sun Mar 7 2021 Andrii Verbytskyi 2.0.0
+ Suse compatibility
* Mon Feb 3 2014 Andrii Verbytskyi 1.0.4
+ Initial spec file

