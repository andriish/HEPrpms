%undefine _debugsource_packages
%define major       0
%define libname     EvtGen
%define libnamedev  EvtGen-devel
%define develnamestatic  EvtGen-static-devel

Name:           EvtGen
Version:        1.7.0
Release:        4%{?dist}
License:        Unknown
Url:            http://evtgen.warwick.ac.uk
Source0:        https://evtgen.hepforge.org/downloads/EvtGen-01.07.00.tar.gz
Patch0:         patch-EvtGen-0.txt
Summary:        Robust Independent Validation of Experiment and Theory
BuildRequires:  gcc-gfortran libgfortran gcc-c++ PHOTOS-devel TAUOLA-devel pythia8-devel HepMC HepMC-devel 
Requires:    PHOTOS TAUOLA pythia8 libgfortran
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


%prep
%setup -q -n EvtGen/R01-07-00
%patch0 -p0

%build
mkdir -p $(pwd)/tmp/usr
mkdir -p $(pwd)/tmp/%_lib
./configure  --prefix=/usr --libdir=/usr/%_lib --hepmcdir=/usr --pythiadir=/usr --photosdir=/usr --tauoladir=/usr
make 
make   lib_shared
make  libext_shared

%install
make install DESTDIR=$(pwd)/tmp  LIBDIR=/usr/%_lib
mkdir -p $RPM_BUILD_ROOT/usr
cp -r $(pwd)/tmp/usr    $RPM_BUILD_ROOT


%files -n %{libname}
/usr/%_lib/*

%files -n %{libnamedev}
/usr/share/*
%{_includedir}/*

%changelog
* Mon Feb 3 2014 Andrii Verbytskyi 1.0.4
+ Initial spec file

