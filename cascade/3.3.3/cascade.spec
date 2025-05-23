Name:       cascade
Version:    3.3.3
Release:    4%{?dist}
Summary:    Multipurpose Monte Carlo Event Generator for High Energy physics

License:    GPLv2
URL:        https://cascade.hepforge.org/
Source0:    https://cascade.hepforge.org/downloads/cascade-%{version}.tar.gz
Patch0:     patch-cascade-0.txt

BuildRequires: cmake
%if 0%{?fedora}
BuildRequires: chrpath
%endif
%if 0%{?rhel} || 0%{?fedora}
BuildRequires:  autoconf automake libtool gcc-gfortran HepMC HepMC-devel lhapdf lhapdf-devel 
BuildRequires:  HepMC3  HepMC3-search HepMC3-devel HepMC3-search-devel Rivet-devel Rivet YODA YODA-devel
BuildRequires:  tex(latex) ghostscript tex(sectsty.sty)
Requires:       libgfortran HepMC lhapdf  pythia6 HepMC3 Rivet YODA HepMC3  HepMC3-search HepMC3-devel HepMC3-search-devel
BuildRequires:  gsl gsl-devel zlib zlib-devel 
Requires:       gsl zlib
BuildRequires:  fastjet-devel fastjet gcc-c++ gcc-gfortran tmdlib tmdlib-devel pythia8 pythia8-devel
Requires:       fastjet tmdlib  pythia8
%endif
%if 0%{?suse_version}
BuildRequires:  autoconf automake libtool gcc-fortran libHepMC4 HepMC2-devel libLHAPDF LHAPDF-devel 
BuildRequires:  libHepMC3-4  HepMC3-devel  Rivet-devel Rivet YODA YODA-devel 
BuildRequires:  texlive-palatino texlive-helvetic texlive-courier tex(latex) ghostscript tex(sectsty.sty) tex(cite.sty) tex(lineno.sty) tex(sectsty.sty)
Requires:       libHepMC4 libLHAPDF libHepMC3-4 Rivet YODA libHepMC4 
BuildRequires:  gsl gsl-devel zlib zlib-devel unzip
Requires:       gsl zlib
BuildRequires:  fastjet-devel fastjet gcc-c++ gcc-fortran tmdlib tmdlib-devel libpythia8 pythia-devel
Requires:       fastjet tmdlib libpythia8
BuildRequires:  root6-config root6-libs root6-devel root6 nlohmann_json-devel
%endif

Prefix: %{_prefix}
%if %{?rhel}%{!?rhel:0} == 8
BuildRequires: ghostscript-tools-dvipdf
%endif
%description
The Monte Carlo program CASCADE generates a full hadron event record 
according to the HEP common standards. CASCADE was originally intended 
for small x processes and used only gluon chains in the initial state cascade.
With the development of the Parton Branching TMDs, which are valid over 
a large range in x and Q2, a new development has started (CASCADE3, 
CASCADE-lhe): CASCADE-lhe makes use of LHE files (either from collinear 
NLO calcualtions like POWHEG or MC@NLO or off-shell calculations from KaTie) 
and has a full flavor initial state parton shower, which follows directly 
the TMD from the Parton Brachning method. Different sets of TMDs are 
now accessed via TMDlib. 

%prep
%setup -q -n cascade-%{version}
%patch -P 0 -p1

%build
%if %{?fedora}%{!?fedora:0} >= 37
%cmake -DCASCADE_BUILD_DOCS:BOOL=OFF -DCMAKE_SKIP_RPATH:BOOL=YES -DCMAKE_CXX_STANDARD=17
%else
%if 0%{?suse_version}
#-Wl,--as-needed -Wl,--no-undefined -Wl,-z,now
#export LDFLAGS="-Wl,--allow-shlib-undefined -Wl,--no-as-needed "
%cmake -DCMAKE_EXE_LINKER_FLAGS=" " -DCMAKE_MODULE_LINKER_FLAGS=" " -DCMAKE_SHARED_LINKER_FLAGS="-Wl,--allow-shlib-undefined -Wl,--no-as-needed " -DCMAKE_CXX_STANDARD=17
%else
%cmake -DCMAKE_SKIP_RPATH:BOOL=YES -DCMAKE_CXX_STANDARD=17
%endif

%endif

%cmake_build

%install
%cmake_install

%post -p /sbin/ldconfig

%postun -p /sbin/ldconfig


%files
%{_bindir}/*
%{_libdir}/*.so*
%{_includedir}/cascade/*
%{_datadir}/*

%changelog
* Thu Dec 21 2023 Andrii Verbytskyi <andrii.verbtskyi@mpp.mpg.de>
+ 3.3.3
* Fri Mar 10 2023 Andrii Verbytskyi 3.3.1
- Update to 3.3.1
* Mon Oct 17 2022 Andrii verbytskyi <andrii.verbytskyi@mpp.mpg.de> - 3.3.0
- Bump to 3.3.0
* Thu Oct 6 2022 Andrii verbytskyi <andrii.verbytskyi@mpp.mpg.de> - 3.2.4
- Bump to 3.2.4
* Tue Jan 25 2022 Andrii verbytskyi <andrii.verbytskyi@mpp.mpg.de> - 3.2.1
- Bump to 3.2.1
* Mon May 31 2021 Andrii Verbytskyi 3.1.01
+ Cleanup of dependencies
* Fri Nov 29 2019 Andrii Verbytskyi 3.0.01
+ Cleanup
* Mon Oct 15 2018 Andrii verbytskyi <andrii.verbytskyi@mpp.mpg.de> - 3.0.01
- init
