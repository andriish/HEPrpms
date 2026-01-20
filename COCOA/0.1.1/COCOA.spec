%undefine __cmake_in_source_build
%undefine __cmake3_in_source_build
%global cocoablob 25788cd9b2d860a655e72186bcdbfe040b1d0f87

Name:		COCOA
Version:	0.1.1
Release:	3%{?dist}
Summary:	A nearly-hermetic calorimeter simulated with Geant4 and interfaced to the Pythia8 event generator

License:	GPL
URL:		https://github.com/cocoa-hep/cocoa-hep
Source0:	https://github.com/andriish/cocoa-hep/archive/%{cocoablob}.zip

BuildRequires:	cmake >= 3.15
BuildRequires:	gcc-c++
BuildRequires:	clhep-devel
BuildRequires:	zlib-devel
BuildRequires:	PTL-devel 
%if 0%{?rhel} || 0%{?fedora}
BuildRequires:	 expat expat-devel  xerces-c-devel xerces-c tbb-devel 
BuildRequires:	 HepMC HepMC-devel root root-hist root-net root-tree root-genvector pythia8-devel fastjet-devel jsoncpp-devel pythia8 liburing-devel fastjet
BuildRequires:	 geant4 geant4-devel geant4-data
%endif
%if 0%{?suse_version}
BuildRequires:	 libexpat-devel libxerces-c-devel xerces-c 
BuildRequires:	 unzip
BuildRequires:	 HepMC2-devel root6-config root6-libs root6-devel root6  root6-utils fastjet fastjet-devel libpythia8 pythia-devel jsoncpp-devel
BuildRequires:	 geant4 geant4-devel geant4-data
%endif

%description
A nearly-hermetic calorimeter simulated with Geant4 and interfaced to the Pythia8 event generator

%prep
%setup -q -n cocoa-hep-%{cocoablob}

%build
cd COCOA
sed -i 's/TMath::Abs/std::abs/g' src/Tracking_func.cc
%cmake 
%cmake_build

%install
cd COCOA
%cmake_install

%files
%{_bindir}/COCOA
%{_libdir}/lib*.so*
%{_libdir}/*.pcm
%{_libdir}/*.rootmap
%{_datadir}/COCOA

%changelog
* Fri Nov 17 2023 Andrii Verbytskyi 0.1.1
- Initial

