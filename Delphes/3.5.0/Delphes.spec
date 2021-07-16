%{!?_pkgdocdir: %global _pkgdocdir %{_docdir}/%{name}-%{version}}
Name:       Delphes
Version:    3.5.0
Release:    1%{?dist}
Summary:    Delphes is a C++ framework, performing a fast multipurpose detector response simulation. 

License:    GPLv3
URL:        https://cp3.irmp.ucl.ac.be/projects/delphes
Source0:    http://github.com/delphes/delphes/archive/%{version}.tar.gz
#Patch0:         patch-Delphes-0.txt

#The ROOT cmake file used by this project requires cmake 3.4.3
BuildRequires:    cmake >= 3.4.3
BuildRequires:    gcc-c++
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
%ifnarch s390x
BuildRequires:    root-core
BuildRequires:    root-hist
BuildRequires:    root-io
BuildRequires:    root-tree
BuildRequires:    root-montecarlo-eg
BuildRequires:    root-graf3d-eve
BuildRequires:    root-gui-html
BuildRequires:    root-gui
BuildRequires:    root-genvector
BuildRequires:    root-physics
BuildRequires:    root-matrix
BuildRequires:    root-mathcore
%endif
BuildRequires:    zlib-devel fastjet fastjet-devel
BuildRequires:    pythia8 pythia8-devel
BuildRequires:    HepMC HepMC-devel
%endif
%if 0%{?suse_version}
BuildRequires:    root6-libs
BuildRequires:    root6
BuildRequires:    root6-devel
BuildRequires:    zlib-devel fastjet fastjet-devel
BuildRequires:    libpythia8 pythia-devel
BuildRequires:    libHepMC4 HepMC2-devel
%endif


%description
Delphes is a C++ framework, performing a fast multipurpose detector response simulation. 
The simulation includes a tracking system, embedded into a magnetic field, calorimeters 
and a muon system. The framework is interfaced to standard file formats (e.g. Les 
Houches Event File or HepMC) and outputs observables such as isolated leptons, missing 
transverse energy and collection of jets which can be used for dedicated analyses. 
The simulation of the detector response takes into account the effect of magnetic 
field, the granularity of the calorimeters and sub-detector resolutions. Visualisation 
of the final state particles is also built-in using the corresponding ROOT library. 

%package doc
Summary:    C++ Event Record for Monte Carlo Generators - documentation
BuildArch:    noarch

%description doc
This package provides HepMC manuals and examples.




%prep
%setup -q -n delphes-%{version}
#%patch0 -p1

%build
%cmake 
%cmake_build

%install
%cmake_install


mkdir -p %{buildroot}/%{_pkgdocdir}
mkdir -p %{buildroot}/%{_includedir}/%{name}/

mv %{buildroot}/%{_prefix}/cards      %{buildroot}/%{_pkgdocdir}/cards
mv %{buildroot}/%{_prefix}/examples   %{buildroot}/%{_pkgdocdir}/examples
mv %{buildroot}/%{_prefix}/lib        %{buildroot}/%{_libdir}
mv %{buildroot}/%{_prefix}/include/ExRootAnalysis  %{buildroot}/%{_includedir}/%{name}/
mv %{buildroot}/%{_prefix}/include/modules  %{buildroot}/%{_includedir}/%{name}/
mv %{buildroot}/%{_prefix}/include/display  %{buildroot}/%{_includedir}/%{name}/
mv %{buildroot}/%{_prefix}/include/classes  %{buildroot}/%{_includedir}/%{name}/


%files
%{_bindir}/*
%{_libdir}/*
%{_includedir}/%{name}/*



%files doc
%{_pkgdocdir}/cards
%{_pkgdocdir}/examples
%license COPYING


%changelog
* Thu Nov 28 2019 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> - 3.4.2-0
- Initial
