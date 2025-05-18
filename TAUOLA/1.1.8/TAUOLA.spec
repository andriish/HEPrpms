%define major            0
%define libname          TAUOLA
%define libnamedev       TAUOLA-devel
%define develnamestatic  TAUOLA-static-devel


Name:           TAUOLA
Version:        1.1.8
Release:        8%{?dist}
License:        Unknown
Url:            http://tauolapp.web.cern.ch/tauolapp
Source0:        https://tauolapp.web.cern.ch/tauolapp/resources/%{name}.%{version}/%{name}.%{version}.tar.gz
Patch0:         patch-TAUOLA-0.txt
Summary:        Tau lepton decay Monte Carlo
BuildRequires:  gcc-c++ autoconf automake libtool  
BuildRequires:  tex(latex) tex(fmtcount.sty) ghostscript doxygen tex(eurosym.sty)
%if 0%{?rhel} || 0%{?fedora}
Requires:       HepMC HepMC3 libgfortran pythia8
BuildRequires:  gcc-gfortran pythia8 pythia8-devel  HepMC-devel HepMC3-devel HepMC HepMC3 
BuildRequires:  lhapdf lhapdf-devel 
%endif
%if 0%{?suse_version}
BuildRequires:  tex(eurosym.sty)
BuildRequires:  gcc-fortran libpythia8 pythia-devel  HepMC3-devel HepMC2-devel libHepMC4 libHepMC3-4
BuildRequires:  libLHAPDF LHAPDF-devel 
Requires:       libHepMC4 libHepMC3-4 gcc-fortran libpythia8
%endif

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
BuildRequires: ghostscript-tools-dvipdf
%endif
Prefix: %{_prefix}
%description
Because of their narrow width, tau decays can be well separated from 
their production process. Only spin degrees of freedom connect these 
two parts of the physics process of interest for high energy collision 
experiments. In the following, we present a Monte Carlo algorithm which 
is based on that property. The interface supplements events generated 
by other programs, with tau decays




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
%configure  --with-hepmc=%prefix --with-hepmc3=%prefix --with-pythia8=%prefix  --with-lhapdf=%prefix
make 
make -C documentation/latex_documentation
make -C documentation/doxy_documentation
chmod 755 lib/*


%install
%make_install
chmod 755 %{buildroot}/%prefix/%_lib/*
mkdir -p %{buildroot}/%{_docdir}/%{name}/html
install -m 644 documentation/latex_documentation/*pdf   %{buildroot}/%{_docdir}/%{name}
install -m 644 documentation/doxy_documentation/html/*   %{buildroot}/%{_docdir}/%{name}/html


%files -n %{libname}
%{_libdir}/*a
%{_libdir}/libTauolaHepMC3.so.1.1.8
%{_libdir}/libTauolaCxxInterface.so.1.1.8
%{_libdir}/libTauolaHEPEVT.so.1.1.8
%{_libdir}/libTauolaHepMC.so.1.1.8
%{_libdir}/libTauolaFortran.so.1.1.8
%{_libdir}/libTauolaHepMC3.so
%{_libdir}/libTauolaCxxInterface.so
%{_libdir}/libTauolaHEPEVT.so
%{_libdir}/libTauolaHepMC.so
%{_libdir}/libTauolaFortran.so


%files -n %{libnamedev}
%{_includedir}/Tauola/*

%files -n %{name}-doc
%{_docdir}/%{name}/*



%changelog
* Sun Jan 30 2022 Andrii Verbytskyi 1.1.8
+ Fix HepMC3 interfaces.
* Wed Feb 19 2020 Andrii Verbytskyi 1.1.8
+ New version Prerelease.
* Thu Jan 23 2020 Andrii Verbytskyi 1.1.7
+ New version Prerelease.
* Fri Dec 22 2017 Andrii Verbytskyi 1.1.6c
+ New version
* Thu May 26 2016 Andrii Verbytskyi 1.1.5
+ Initial spec file

