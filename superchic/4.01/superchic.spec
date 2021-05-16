
Name:           superchic
Version:        4.01
Release:        1%{?dist}
License:        GPL
Url:            https://superchic.hepforge.org/
Source0:        https://superchic.hepforge.org/%{name}%{version}.tar.gz
Patch0:         patch-superchic-0.txt
Summary:        A Fortran based Monte Carlo event generator for exclusive and photon-initiated production in proton and heavy ion collisions
BuildRequires:  gcc-gfortran gcc-c++ 
BuildRequires:  HepMC HepMC-devel lhapdf lhapdf-devel apfel apfel-devel
Requires:  HepMC lhapdf apfel

Prefix: %{_prefix}

%description 
SuperChic is a Fortran based Monte Carlo event generator for exclusive and photon-initiated production in proton and 
heavy ion collisions. A range of Standard Model final states are implemented, in most cases with spin correlations where
relevant, and a fully differential treatment of the soft survival factor is given. Arbitrary user-defined histograms and 
cuts may be made, as well as unweighted events in the HEPEVT, HEPMC and LHE formats. For further information see the user manual. 
 
%prep
%setup  -q -n %{name}%{version}
%autopatch -p1

%build

%make_build FC=gfortran prefix=%{prefix} bindir=%{_bindir} libdir=%{_libdir} docdir=%{_docdir}

%install
%make_install FC=gfortran  prefix=%{prefix} bindir=%{_bindir} libdir=%{_libdir} docdir=%{_docdir}

%files 
%{_bindir}/superchic
%{_bindir}/init_superchic
%{_libdir}/libsuperchic.so
%{_docdir}/superchic4.pdf

%changelog
* Sun Feb 21 2021 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
+ Initial spec file
