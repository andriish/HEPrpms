Name:          HEPrpms
Version:       2023.12.30
Release:       1%{?dist}
Summary:       Collection of software
Group:         Development/Libraries
License:       GPL+
URL:           https://github.com/andriish/HEPrpms/
Source0:       README.txt
Requires: COCOA == 0.1.1
Requires: Delphes == 3.5.1pre10
Requires: Delphes-doc == 3.5.1pre10
Requires: EvtGen == 2.2.1
Requires: EvtGen-doc == 2.2.1
Requires: FeynHiggs == 2.19.0
Requires: FeynHiggs-devel == 2.19.0
Requires: HJets == 1.3
Requires: HepPDT == 3.04.01
Requires: HepPDT-devel == 3.04.01
Requires: HepPDT-doc == 3.04.01
Requires: Herwig == 7.3.0
Requires: Herwig-devel == 7.3.0
Requires: JetVHeto == 3.0.0
Requires: LCIO == 2.20.02
Requires: LCIO-devel == 2.20.02
Requires: LoopTools == 2.16
Requires: MC-TESTER == 1.25.1
Requires: MC-TESTER-devel == 1.25.1
Requires: MG5_aMC == 3.5.3
Requires: PHOTOS-doc == 3.64
Requires: Professor == 2.4.2
Requires: SHERPA-MC == 2.2.15
Requires: SHERPA-MC-common == 2.2.15
Requires: SHERPA-MC-devel == 2.2.15
Requires: SHERPA-MC-openmpi == 2.2.15
Requires: SHERPA-MC-openmpi-devel == 2.2.15
Requires: TAUOLA-doc == 1.1.8
Requires: TheP8I == 2.0.3
Requires: VBFNLO == 3.0.0beta5
Requires: applgrid == 1.6.35
Requires: ariadne == 4.12
Requires: cascade == 3.3.3
Requires: cernlib == 2023.10.31.0
Requires: cernlib-devel == 2023.10.31.0
Requires: cernlib-packlib-gfortran == 2023.10.31.0
Requires: cernlib-static == 2023.10.31.0
Requires: cernlib-utils == 2023.10.31.0
Requires: chaplin == 1.2
Requires: cuba == 4.2.2
Requires: cuba-devel == 4.2.2
Requires: f90cache == 0.99c
Requires: fastnlo == 2.5.0.2826
Requires: fastnlo-devel == 2.5.0.2826
Requires: form-doc == 4.3.1
Requires: geant321-gfortran == 2023.10.31.0
Requires: golem95 == 1.3.3
Requires: golem95-devel == 1.3.3
Requires: gosam == 2.1.1
Requires: iminuit == 2.21.1
Requires: kuipc-gfortran == 2023.10.31.0
Requires: nlojet++ == 4.1.3
#Requires: noweb == 2.13
Requires: patchy-gfortran == 2023.10.31.0
Requires: paw-gfortran == 2023.10.31.0
Requires: pythia6 == 6.4.28
Requires: python3-LCIO == 2.20.02
Requires: python3-SHERPA-MC == 2.2.15
Requires: python3-SHERPA-MC-openmpi == 2.2.15
Requires: python3-fastjet == 3.4.2
##### Requires: python3-whizard == 3.1.4
Requires: qcdloop == 2.0.9
Requires: qcdloop-devel == 2.0.9
Requires: qcdnum == 18.00.0
Requires: qcdnum-devel == 18.00.0
Requires: qgraf == 3.6.7
Requires: rapgap == 3.4.0
##### Requires: recola2 == 2.2.4
##### Requires: recola2-SM == 2.2.3
Requires: topdrawer == 1.4e
Requires: ugs == 2.10e
##### Requires: whizard == 3.1.4

%description
All packages of HEPrpms.


%prep
mkdir -p %{name}-%{version}
cd %{name}-%{version}
cp %{SOURCE0} ./

%build

%install
cd %{name}-%{version}
mkdir -p %{buildroot}/%_docdir/%{name}
install README.txt  %{buildroot}/%_docdir/%{name}/

%clean
rm -rf %{buildroot}

%post -p /sbin/ldconfig

%postun -p /sbin/ldconfig

%files
%_docdir/%{name}/README.txt 

%changelog
* Sat Dec 30 2023 Andrii Verbytskyi 2023.12.30
- This is just a technical package
