Name:           HJets
Version:        1.3
Release:        2%{?dist}
License:        GPL
Url:            http://HJets.hepforge.org/
Source0:        https://hjets.hepforge.org/downloads/HJets-%{version}.tar.gz
Summary:        HJets is a tool for calculating electroweak cross sections for Higgs plus up to three jets at NLO in QCD. 
BuildRequires:   gcc-c++ Herwig Herwig-devel ThePEG ThePEG-devel gsl gsl-devel  boost-devel zlib zlib-devel
%if 0%{?rhel} || 0%{?fedora}
BuildRequires:  gcc-gfortran  boost
%endif
%if 0%{?suse_version}
BuildRequires:  gcc-fortran 
%endif


Prefix: %{_prefix}

%description 
HJets is a tool for calculating electroweak cross sections for Higgs 
plus up to three jets at NLO in QCD. It is the code behind 
Phys.Rev.Lett. 111 (2013) 211802.
 
%prep
%setup -q

%build
%configure
make %{?_smp_mflags}

%install
%make_install

%files
%{_libdir}/*
%{_datadir}/HJets/*
%{_includedir}/HJets/*

%changelog
* Mon Feb 3 2014 Andrii Verbytskyi 1.0.4
+ Initial spec file
