%define major            0
%define libname          MG5_aMC
%undefine __brp_mangle_shebangs 
%undefine _debugsource_packages

Name:          MG5_aMC
Version:       3.5.4
Release:       1%{?dist}

Summary:       MG5_aMC is a multi-purpose particle physics event generator.
License:       http://www.opensource.org/licenses/UoI-NCSA.php
Source0:       https://launchpad.net/mg5amcnlo/3.0/3.5.x/+download/%{name}_v%{version}.tar.gz
Url:           http://amcatnlo.web.cern.ch/amcatnlo/list_refs.htm
%if 0%{?rhel} || 0%{?fedora}
BuildRequires:  gcc-c++ 
BuildRequires:  gcc-gfortran
%endif
%if 0%{?suse_version}
BuildRequires:  gcc-c++
BuildRequires:  gcc-fortran
BuildRequires:  python-rpm-macros
%endif

Prefix: %{_prefix}

%if  %{?rhel}%{!?rhel:0} >= 8
BuildRequires: platform-python-devel
%endif
%if %{?fedora}%{!?fedora:0} >=31
BuildRequires: python3-devel
%endif
%if %{?fedora}%{!?fedora:0} >= 39
BuildRequires: python3-rpm-macros
%endif

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
Requires: python3 csh
BuildRequires: python3 python3-devel 
%endif
%if 0%{?suse_version}
Requires: python 
BuildRequires: python python-devel python3-devel 
%endif


%description
MadGraph5_aMC@NLO is a framework that aims at providing all the elements 
necessary for SM and BSM phenomenology, such as the computations of 
cross sections, the generation of hard events and their matching with 
event generators, and the use of a variety of tools relevant to event 
manipulation and analysis. Processes can be simulated to LO accuracy 
for any user-defined Lagrangian, and the NLO accuracy in the case of 
QCD corrections to SM processes. Matrix elements at the tree- and 
one-loop-level can also be obtained.
MadGraph5_aMC@NLO is the new version of both MadGraph5 and aMC@NLO that 
unifies the LO and NLO lines of development of automated tools within 
the MadGraph family. It therefore supersedes all the MadGraph5 1.5.x 
versions and all the beta versions of aMC@NLO.
The standard reference for the use of the code is: J. Alwall et al, 
"The automated computation of tree-level and next-to-leading order 
differential cross sections, and their matching to parton shower 
simulations", arXiv:1405.0301 [hep-ph]. A more complete list of 
references can be found here: 
http://amcatnlo.web.cern.ch/amcatnlo/list_refs.htm

%prep
%setup -q  -n MG5_aMC_v3_5_4
%build

%install

mkdir -p $RPM_BUILD_ROOT/%_bindir
mkdir -p $RPM_BUILD_ROOT/%_datadir/MG5_aMC/
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
mkdir -p $RPM_BUILD_ROOT/%{python3_sitearch}
%if %{?fedora}%{!?fedora:0} >= 39
%py3_shebang_fix    ./bin/m*
%py3_shebang_fix    ./
%else
pathfix.py -pn -i %{__python3}  ./bin/m*
pathfix.py -pn -i %{__python3}  ./
%endif
cp bin/* $RPM_BUILD_ROOT/%_bindir
cp -r aloha vendor madgraph input Template MadSpin  mg5decay  models $RPM_BUILD_ROOT/%{python3_sitearch}
cp VERSION $RPM_BUILD_ROOT/%_datadir/MG5_aMC/
rm -rf $RPM_BUILD_ROOT/%{python3_sitearch}/madgraph/VERSION
cp VERSION $RPM_BUILD_ROOT/%{python3_sitearch}/madgraph
%endif
%if 0%{?suse_version}
mkdir -p $RPM_BUILD_ROOT/%{python_sitearch}
cp bin/* $RPM_BUILD_ROOT/%_bindir
cp -r aloha vendor madgraph input Template MadSpin  mg5decay  models $RPM_BUILD_ROOT/%{python_sitearch}
rm -rf $RPM_BUILD_ROOT/%{python_sitearch}/madgraph/VERSION
cp VERSION $RPM_BUILD_ROOT/%{python_sitearch}/madgraph
%endif 

%files -n %{libname}
%_bindir/mg*
/usr/share/*
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
%{python3_sitearch}/*
%endif
%if 0%{?suse_version}
%{python_sitearch}/*
%endif

%changelog
* Thu Aug 17 2023 Andrii Verbytskyi 3.5.1
- Bump to 3.5.1
* Fri Sep 30 2022 Andrii Verbytskyi 3.4.2
- Bump to 3.4.2
* Fri Aug 12 2022 Andrii Verbytskyi 3.4.0
- Update to 3.4.0
* Sat Mar 13 2021 Andrii Verbytskyi 2.9.2
- Update to 2.9.2 
* Wed Dec 11 2019 Andrii Verbytskyi 2.6.7
+ Simplified

