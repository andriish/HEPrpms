Name:           TheP8I
Version:        2.0.2
Release:        1%{?dist}
License:        GPL
Url:            https://gitlab.cern.ch/TheP8I/TheP8I
Source0:        https://gitlab.cern.ch/TheP8I/TheP8I/-/archive/%{version}/%{name}-%{version}.tar.gz
Patch0:         patch-TheP8I-0.txt
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
BuildRequires: gcc-c++ gcc ThePEG-devel ThePEG gsl-devel 
BuildRequires: cmake >= 3.4.3




%description
Lund hadronisation for Herwig. Part of earlier ThePEG codes.

%prep
%setup -qn TheP8I-%{version}
%patch0 -p1

%build
export LDFLAGS=" "
%if 0%{?suse_version}
%cmake -DCMAKE_INSTALL_LIBDIR=%{_libdir}/ThePEG -DTHEP8I_ENABLE_TEST=OFF -DTHEP8I_CXX_STANDARD=17 -DCMAKE_MODULE_LINKER_FLAGS=" "  -DCMAKE_SHARED_LINKER_FLAGS=" "
%else
%cmake -DCMAKE_INSTALL_LIBDIR=%{_libdir}/ThePEG -DTHEP8I_ENABLE_TEST=OFF
%endif
%cmake_build

%install
%cmake_install


%files
%{_libdir}/ThePEG/libTheP8I*
%_datadir/TheP8I/*in

%changelog
* Tue Jul 12 2022 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
  - Deal with comp. failures
* Sun Aug 01 2021 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
  - RPATH
* Thu Mar 12 2020 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> 2.0.0
+ Update to 2.0.0

* Thu Jan 17 2019 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> 1.0.0
+ test

