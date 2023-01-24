Name:		HEJ
Version:	2.1.3
Release:	1%{?dist}
Summary:	HEJ

License:	GPL
URL:		https://hej.hepforge.org
Source0:	https://hej.hepforge.org/downloads/HEJ_2.1.3.tar.gz


BuildRequires:	cmake >= 3.9
BuildRequires:	gcc-c++
BuildRequires:	high-five-devel
BuildRequires:	clhep-devel
BuildRequires:	lhapdf-devel
BuildRequires:	qcdloop
BuildRequires:	HepMC3-devel
BuildRequires:	yaml-cpp-devel
BuildRequires:	fastjet-devel
BuildRequires:	root
BuildRequires:	git
BuildRequires:	boost-devel




%description
HEJ




%prep
%setup -n HEJ-2.1.3 -c


%build

%cmake -DEXCLUDE_ROOT=OFF -DEXCLUDE_HepMC=TRUE
%cmake_build


%install
%cmake_install



%files
%{_bindir}/*
%{_includedir}/*
%{_prefix}/lib/lib*.so*
%{_prefix}/lib/cmake/*


%changelog
* Tue Jan 24 2023  Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de>
- v0.0.0
