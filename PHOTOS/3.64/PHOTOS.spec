%define major       0
%define libname     PHOTOS
%define libnamedev  PHOTOS-devel
%define develnamestatic  PHOTOS-static-devel


Name:           PHOTOS
Version:        3.64
Release:        7%{?dist}
License:        MIT
Url:            http://photospp.web.cern.ch/photospp
Source0:        https://photospp.web.cern.ch/photospp/resources/%{name}.%{version}/%{name}.%{version}.tar.gz
Patch0:         patch-PHOTOS-0.txt
Summary:        Monte Carlo program for bremsstrahlung in the decay of particles and resonances
BuildRequires:  gcc-c++ autoconf automake libtool  tex(latex) tex(fmtcount.sty) ghostscript doxygen
%if 0%{?rhel} || 0%{?fedora}
BuildRequires:  gcc-gfortran  pythia8 pythia8-devel HepMC-devel HepMC3-devel HepMC HepMC3 tex(eurosym.sty)
Requires:       HepMC HepMC3 libgfortran pythia8
%endif
%if 0%{?suse_version}
BuildRequires:  gcc-fortran   libpythia8 pythia-devel HepMC2-devel HepMC3-devel libHepMC4 libHepMC3-4 tex(eurosym.sty) tex(booktabs.sty)
Requires:       libHepMC4 libHepMC3-4 gcc-fortran  libpythia8
%endif
BuildRequires:  TAUOLA >= 1.1.7
BuildRequires:  TAUOLA-devel >= 1.1.7
Requires:       TAUOLA >= 1.1.7
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
BuildRequires:  ghostscript-tools-dvipdf
%endif
Prefix: %{_prefix}
%description
Monte Carlo for bremsstrahlung in the decay of particles and
resonances


%package  devel
Summary:        Libraries and headers for %{name}
Requires:       %{libname} = %{version}
Provides:       %{name}-devel = %{version}-%{release}

%description devel
%{libnamedev} contains the libraries and header files needed to
develop programs which make use of %{name}.
The library documentation is available on header files.


%package  doc
Summary:        Documentation for %{name}
Requires:       %{libname} = %{version}
BuildArch:      noarch
Provides:       %{name}-doc = %{version}-%{release}

%description doc
 %{name}-doc contains the documentation for libraries and header files needed to
develop programs which make use of %{name}.






%prep
%setup -qn %{name}
%patch -P 0 -p1

%build
autoreconf -fisv
%configure --with-hepmc=%prefix --with-hepmc3=%prefix --with-pythia8=%prefix  --with-tauola=%prefix
make 
make -C documentation/latex_documentation
make -C documentation/doxygen
chmod +x lib/*

%install
%make_install
chmod +x %{buildroot}/%prefix/%_lib/*
mkdir -p %{buildroot}/%{_docdir}/%{name}/html
install -m 644 documentation/latex_documentation/*pdf   %{buildroot}/%{_docdir}/%{name}
install -m 644 documentation/doxygen/html/*   %{buildroot}/%{_docdir}/%{name}/html

%files -n %{libname}
%{_libdir}/*

%files -n %{libnamedev}
%{_includedir}/Photos/*

%files -n %{name}-doc
%{_docdir}/%{name}/*



%changelog
* Wed Feb 19 2020 Andrii Verbytskyi 3.64
+ New version Prerelease.
*Mon Jan 27 2020 Andrii Verbytskyi 3.62
+ HepMC3
* Mon Feb 3 2014 Andrii Verbytskyi 3.54
+ Initial spec file

