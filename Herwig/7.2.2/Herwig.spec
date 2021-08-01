%define major            0
%define libname          Herwig
%define libnamedev       Herwig-devel
%define develnamestatic  Herwig-static-devel
%undefine __brp_mangle_shebangs
%define _configure_libtool_hardening_hack 0


Name:           Herwig
Version:        7.2.2
Release:        3%{?dist}

Summary:        Herwig is a multi-purpose particle physics event generator.
License:        GPLv3
Source0:        http://www.hepforge.org/archive/herwig/%{name}-%{version}.tar.bz2
Patch0:         patch-Herwig-0.txt
Url:            https://herwig.hepforge.org/
BuildRequires:  MG5_aMC lhapdf-sets-Herwig zlib zlib-devel fastjet-devel 
BuildRequires:  openloops  njet gsl-devel gosam autoconf automake libtool   gengetopt
Requires:       MG5_aMC zlib fastjet  gosam   gsl  njet openloops  
Requires:       ThePEG  == 2.2.2
Requires:       ThePEG-devel  == 2.2.2
BuildRequires:  ThePEG  == 2.2.2 
BuildRequires:  ThePEG-devel  == 2.2.2 

%if 0%{?rhel} || 0%{?fedora}
BuildRequires:  lhapdf-devel   pythia8-devel  boost boost-devel
Requires:       boost lhapdf pythia8
%endif
%if 0%{?suse_version}
BuildRequires:  LHAPDF-devel   pythia-devel pythia-doc libLHAPDF libpythia8
Requires:        libLHAPDF libpythia8  pythia-doc
BuildRequires:   libboost_filesystem-devel
BuildRequires:   libboost_headers-devel
BuildRequires:   libboost_system-devel
BuildRequires:   libboost_test-devel PHOTOS PHOTOS-devel TAUOLA TAUOLA-devel
%endif

Requires: EvtGen >= 2.0.0
Requires: EvtGen-devel >= 2.0.0
BuildRequires: EvtGen >= 2.0.0
BuildRequires: EvtGen-devel >= 2.0.0

Requires:       VBFNLO 
BuildRequires:  VBFNLO 
%if  %{?rhel}%{!?rhel:0} >= 8
BuildRequires: python2 platform-python-devel
%endif

%if 0%{?suse_version}
BuildRequires: python3
%endif

%if %{?fedora}%{!?fedora:0} 
BuildRequires: python2  python3-devel
%endif

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
BuildRequires: gcc-c++ gcc-gfortran
%endif


%if 0%{?suse_version}
BuildRequires: gcc-c++ gcc-fortran
%endif



Prefix: %{_prefix}

%description
It is built based on the experience gained with both the HERWIG 6 and 
Herwig++ 2 event generators. Continuing the Herwig++ 2 development, 
Herwig 7.0 (Herwig++ 3.0) replaces any prior HERWIG or Herwig++ versions.
Herwig provides significantly improved and extended physics capabilities 
when compared to both its predecessors, HERWIG 6 and Herwig++ 2, while 
keeping the key model motivations such as coherent parton showers 
(including angular-ordered and dipole-based evolution), the cluster 
hadronization model, an eikonal multiple-interaction model, highly 
flexible BSM capabilities and improved perturbative input using 
next-to-leading order QCD.


%package  devel
Summary:        Libraries and headers for %{name}
Requires:       %{libname} = %{version}
Provides:       %{name}-devel = %{version}-%{release}
Prefix: %{_prefix}
%description devel
%{libnamedev} contains the libraries and header files needed to
develop programs which make use of %{name}.
The library documentation is available on header files.

#Yes, suse is stupid
%if 0%{?suse_version}

%debug_package

%endif

%prep
%setup -q 
%patch0 -p 1

%build

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
pathfix.py -i /usr/bin/python2  -p -n  ./
autoreconf --force --install --verbose .
automake -a --force
export LDFLAGS='-Wl,-z,lazy'
%if %{?fedora}%{!?fedora:0}
FCFLAGS="-fallow-argument-mismatch"
FFLAGS="-fallow-argument-mismatch"
%endif
%configure  --disable-rpath      --infodir=/usr/share/info  --with-evtgen=%_prefix --with-madgraph=%_prefix  --with-openloops=%_prefix/%_lib/openloops --with-pythia=%_prefix --with-evtgen=%_prefix --with-thepeg=%_prefix --with-gosam=%_prefix --with-gosam-contrib=%_libdir  --with-njet=%_prefix  --libdir=/usr/%_lib        --with-vbfnlo=/usr
make %{?_smp_mflags}
%endif



%if 0%{?suse_version}
#pathfix.py -i /usr/bin/python2  -p -n  ./
autoreconf --force --install --verbose .
automake -a --force
export LDFLAGS='-Wl,-z,lazy'
FCFLAGS="-fallow-argument-mismatch"
FFLAGS="-fallow-argument-mismatch"
%configure  --disable-rpath      --infodir=/usr/share/info  --with-evtgen=%_prefix --with-madgraph=%_prefix  --with-openloops=%_prefix/%_lib/openloops --with-pythia=%_prefix --with-evtgen=%_prefix --with-thepeg=%_prefix --with-gosam=%_prefix --with-gosam-contrib=%_libdir  --with-njet=%_prefix  --libdir=/usr/%_lib        --with-vbfnlo=/usr
make %{?_smp_mflags}
%endif

%install

%make_install 
sed -i "s|${RPM_BUILD_ROOT}||g" $RPM_BUILD_ROOT/%{_prefix}/share/Herwig/defaults/PDF.in
# contains Rivet /usr/share/Herwig/snippets/DipoleMerging.in
%files -n %{libname}
%_bindir/*
%_datadir/Herwig
%_libdir/Herwig



%files -n %{libnamedev}
%_bindir/herwig-config
%_includedir/Herwig
%_libdir/Herwig/*.so
%_libdir/Herwig/*.la

%changelog
* Sun Aug 01 2021 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
  - RPATH
* Thu Feb 18 2021 Andrii Verbytskyi 7.2.2
+ Spec for 7.2.2
* Sat Jan 09 2021 Andrii Verbytskyi 7.2.1
+ Spec for 7.2.1 on EL8
* Thu Jan 17 2019 Andrii Verbytskyi 7.1.4
+ Spec for 7.1.4
* Fri Dec 22 2017 Andrii Verbytskyi 7.1.2
+ Spec for 7.1.2
* Thu May 26 2016 Andrii Verbytskyi 7.0.2
+ Initial spec file
