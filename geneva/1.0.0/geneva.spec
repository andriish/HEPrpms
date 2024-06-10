%define libname     geneva
%define libnamedev  geneva-devel
%define libnamedoc  geneva-doc
%undefine _py3_shebang_s
%undefine _py3_shebang_P

Name:           geneva
Version:        1.0.0
Release:        6%{?dist}
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

%if %{?fedora}%{!?fedora:0} >= 39
BuildRequires: python3-rpm-macros
%endif

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


%package -n python3-%{name}
Summary:   %{name}  Python 3 bindings
%{?python_provide:%python_provide python3-%{name}}
%description -n python3-%{name}
This package provides the Python 3 bindings for %{name}


%prep
%setup -q -n geneva-public-1.0-RC3
%patch0 -p1
sed -i 's@DESTINATION\ \"lib@DESTINATION\ \"lib64@g' python/CMakeLists.txt 
sed -i 's@DESTINATION\ lib@DESTINATION\ lib64@g' src/CMakeLists.txt packages/*/CMakeLists.txt


%build
rm -rf external/fastjet/ external/hepmc/ external/lhapdf/ external/openloops/ external/pythia8/ external/rivet/

%if %{?fedora}%{!?fedora:0} >= 39
%py3_shebang_fix  ./python
%py3_shebang_fix  ./python/bin/geneva*
%else
pathfix.py -pn -i %{__python3}  ./python
pathfix.py -pn -i %{__python3}  ./python/bin/geneva*
%endif

%cmake  -DDgeneva_enable_python=ON \
      -Dgeneva_enable_hepmc=OFF \
      -Dgeneva_enable_lhapdf=ON \
      -Dgeneva_enable_openloops=ON \
      -Dgeneva_enable_pythia8=OFF \
      -Dopenloops_ROOT=/usr/lib64/openloops/   \
      -Dlhapdf_ROOT=/usr

%cmake_build


%install
%cmake_install
%cmake_build --target beamfunc-install-data
mv share/Geneva/beamfunc %{buildroot}/%{_datadir}/Geneva

%if %{?fedora}%{!?fedora:0} >= 39
%py3_shebang_fix  %{buildroot}/%_bindir/geneva*
%else
pathfix.py -pn -i %{__python3} %{buildroot}/%_bindir/geneva*
%endif

%files -n %{libname}
%{_bindir}/*
%_libdir/libGene*
%_libdir/python*/site-packages/*
%{_datadir}/Geneva

%files -n %{libnamedev}
%{_includedir}/Geneva/*

%files  -n python3-%{name}
%{python3_sitearch}/*


%changelog
* Tue Jul 12 2022 Andrii Verbytskyi 1.0.0
+ 1.0.0
