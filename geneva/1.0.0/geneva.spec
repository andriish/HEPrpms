Name:           geneva
Version:        1.0.0
Release:        1%{?dist}
License:        GPLv3
Url:            https://gitlab.desy.de/geneva
Source0:        https://gitlab.desy.de/geneva/geneva-public/-/archive/1.0-RC3/geneva-public-1.0-RC3.tar.gz
Patch0:         patch-geneva-0.txt

Summary:        geneva is a Monte Carlo event generator
BuildRequires:  openloops-devel 
BuildRequires:  fastjet-devel
BuildRequires:  pythia8-devel
BuildRequires:  HepMC-devel
BuildRequires:  lhapdf-devel
BuildRequires:  python3-devel
BuildRequires:  gcc-c++



%if 0%{?rhel} || 0%{?fedora}
BuildRequires:  pythia8-devel HepMC HepMC-devel 
Requires:       pythia8 libgfortran HepMC HepMC3 HepMC3-search
BuildRequires:  root-core
BuildRequires:  root-hist
BuildRequires:  root-io
BuildRequires:  root-tree
%endif

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
BuildRequires: gcc-c++ gcc gcc-gfortran
%endif

BuildRequires: cmake >= 3.0.0

Prefix: %{_prefix}

%description
geneva is a Monte Carlo event generator 

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
%setup -q -n geneva-public-1.0-RC3
#patch0 -p1

%build
rm -rf external/fastjet/ external/hepmc/ external/lhapdf/ external/openloops/ external/pythia8/ external/rivet/


%cmake -S . -B BUILD -DDgeneva_enable_python=ON \
      -Dgeneva_enable_hepmc=OFF \
      -Dgeneva_enable_lhapdf=ON \
      -Dgeneva_enable_openloops=ON \
      -Dgeneva_enable_pythia8=OFF \
      -Dopenloops_ROOT=/usr   \
      -Dlhapdf_ROOT=/usr


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
%{_prefix}/share/doc/geneva/guide.ps
%{_prefix}/share/doc/geneva/*md


%changelog
* Tue Jul 12 2022 Andrii Verbytskyi 1.0.0
+ 1.0.0
