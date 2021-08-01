Name:           TheP8I
Version:        2.0.1
Release:        3%{?dist}
License:        GPL
Url:            https://gitlab.cern.ch/TheP8I/TheP8I
Source0:        https://gitlab.cern.ch/TheP8I/TheP8I/-/archive/2.0.1/%{name}-2.0.1.tar.gz
Prefix:         %{_prefix}
Summary:        Lund hadronisation for Herwig
%if 0%{?rhel} || 0%{?fedora}
BuildRequires:  pythia8-devel pythia8  gsl   lhapdf lhapdf-devel
Requires:       pythia8 gsl ThePEG
%endif
%if 0%{?suse_version}
BuildRequires:  pythia-devel libpythia8  gsl  libLHAPDF LHAPDF-devel
Requires:       libpythia8 gsl ThePEG
%endif
BuildRequires: gcc-c++ gcc ThePEG-devel ThePEG autoconf automake libtool gsl-devel




%description
Lund hadronisation for Herwig. Part of earlier ThePEG codes.

%prep
%setup -qn TheP8I-2.0.1


%build
%configure --disable-rpath 
make %{?_smp_mflags}

%install
%make_install
export QA_RPATHS=3


%files
%{_libdir}/ThePEG/*
%_datadir/*

%changelog
* Sun Aug 01 2021 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
  - RPATH
* Thu Mar 12 2020 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> 2.0.0
+ Update to 2.0.0

* Thu Jan 17 2019 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> 1.0.0
+ test

