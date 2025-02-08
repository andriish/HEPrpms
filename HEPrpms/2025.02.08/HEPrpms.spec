Name:          HEPrpms
Version:       2025.02.08
Release:       1%{?dist}
Summary:       Collection of software
Group:         Development/Libraries
License:       GPL+
URL:           https://github.com/andriish/HEPrpms/
Source0:       README.txt

Requires: apfel-devel == 3.1.1
Requires: apfel == 3.1.1
Requires: applgrid == 1.6.36
Requires: ariadne == 4.12
Requires: binder == 1.4.2
Requires: blackhat-data == 0.9.9
Requires: blackhat-devel == 0.9.9
Requires: blackhat == 0.9.9
Requires: cascade == 3.3.3
Requires: cernlib-devel == 2024.09.16.0
Requires: cernlib-packlib-gfortran == 2024.09.16.0
Requires: cernlib-static == 2024.09.16.0
Requires: cernlib-utils == 2024.09.16.0
Requires: cernlib == 2024.09.16.0
Requires: chaplin == 1.2
Requires: clhep-devel == 2.4.7.1
Requires: clhep == 2.4.7.1
Requires: collier == 1.2.8
Requires: cuba-devel == 4.2.2
Requires: cuba == 4.2.2
Requires: Delphes-doc == 3.5.1pre11
Requires: Delphes == 3.5.1pre11
Requires: EvtGen-devel == 2.2.3
Requires: EvtGen-doc == 2.2.3
Requires: EvtGen == 2.2.3
Requires: f90cache == 0.99c
Requires: fastjet-devel == 3.4.3
Requires: fastjet == 3.4.3
Requires: fastnlo-devel == 2.5.0.2826
Requires: fastnlo == 2.5.0.2826

Requires: fjcontrib-devel == 1.100
Requires: fjcontrib == 1.100
Requires: form-doc == 4.3.1
Requires: form == 4.3.1
Requires: geant321-gfortran == 2024.09.16.0
Requires: geant4-data == 11.3.0
Requires: geant4-devel == 11.3.0
Requires: geant4-doc == 11.3.0
Requires: geant4 == 11.3.0

Requires: golem95-devel == 1.3.3
Requires: golem95 == 1.3.3
Requires: gosam-contrib == 2.0.20200904
Requires: gosam == 2.1.2
Requires: HepPDT-devel == 3.04.01
Requires: HepPDT-doc == 3.04.01
Requires: HepPDT == 3.04.01
Requires: Herwig-devel == 7.3.0
Requires: Herwig == 7.3.0
Requires: HJets == 1.3
Requires: hoppet == 1.2.0
Requires: hztool == 4.3.2
Requires: iminuit == 2.21.1
Requires: JetVHeto == 3.0.0
Requires: kuipc-gfortran == 2024.09.16.0
Requires: lhapdf-sets-Herwig == 7.1.6
Requires: lhapdf-sets-whizard == 2.8.3
Requires: LoopTools == 2.16
Requires: MCFM == 10.3
Requires: MC-TESTER-devel == 1.25.1
Requires: MC-TESTER == 1.25.1
Requires: MG5_aMC == 3.5.7
Requires: njet == 2.1.1
Requires: nlojet++ == 4.1.3
Requires: noweb == 2.13
Requires: openloops == 2.1.3
Requires: patchy-gfortran == 2024.09.16.0
Requires: paw-gfortran == 2024.09.16.0
Requires: PHOTOS-devel == 3.64
Requires: PHOTOS-doc == 3.64
Requires: PHOTOS == 3.64
Requires: Professor == 2.5.0
Requires: PTL-devel == 2.3.3
Requires: PTL == 2.3.3
Requires: pythia6 == 6.4.28
Requires: python3-apfel == 3.1.1
Requires: python3-fastjet == 3.4.3
Requires: python3-LCIO == 2.22.02
Requires: python3-YODA == 2.0.2
Requires: qcdloop-devel == 2.1.0
Requires: qcdloop == 2.1.0
Requires: qcdnum-devel == 18.00.0
Requires: qcdnum == 18.00.0
Requires: qgraf == 3.6.7
Requires: rapgap == 3.4.0

Requires: recola == 1.4.4
Requires: Rivet-devel == 4.0.2
Requires: Rivet == 4.0.2

Requires: TAUOLA-devel == 1.1.8
Requires: TAUOLA-doc == 1.1.8
Requires: TAUOLA == 1.1.8
Requires: TheP8I == 2.0.3
Requires: ThePEG-devel == 2.3.0
Requires: ThePEG == 2.3.0
Requires: tmdlib-devel == 2.2.11
Requires: tmdlib == 2.2.11
Requires: topdrawer == 1.4e
Requires: ugs == 2.10e
Requires: VBFNLO == 3.0.0beta5

Requires: YODA-devel == 2.0.2
Requires: YODA == 2.0.2
Requires: python3-uproot4 == 4.3.3

%if 0%{?rhel} || 0%{?fedora}
Requires: python3-whizard == 3.1.5       										
Requires: whizard == 3.1.5

##Fails
#Requires: ggvvamp-devel == 1.0
#Requires: ggvvamp == 1.0
#Requires: nlox == 1.2.1
#Requires: SHERPA-MC-common == 3.0.1
#Requires: SHERPA-MC-devel == 3.0.1
#Requires: SHERPA-MC-openmpi-devel == 3.0.1
#Requires: SHERPA-MC-openmpi == 3.0.1
#Requires: SHERPA-MC == 3.0.1
#Requires: python3-SHERPA-MC-openmpi == 3.0.1
#Requires: python3-SHERPA-MC == 3.0.1
#Requires: FeynHiggs-devel == 2.19.0
#Requires: FeynHiggs == 2.19.0

##Conflicts
#Requires: recola2-SM == 2.2.3
#Requires: recola2 == 2.2.4


%endif
%if 0%{?suse_version}
Requires: SHERPA-MC-common == 2.2.16
Requires: SHERPA-MC-devel == 2.2.16
Requires: SHERPA-MC-openmpi-devel == 2.2.16
Requires: SHERPA-MC-openmpi == 2.2.16
Requires: SHERPA-MC == 2.2.16
Requires: python3-SHERPA-MC-openmpi == 2.2.16
Requires: python3-SHERPA-MC == 2.2.16

##Fails
#Requires: ggvvamp-devel == 1.0
#Requires: ggvvamp == 1.0
#Requires: nlox == 1.2.1

##Conflicts
#Requires: recola2-SM == 2.2.3
#Requires: recola2 == 2.2.4
%endif



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
