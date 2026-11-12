%define debug_package %{nil}
Name:       f90cache
Version:    0.99h
Release:    2%{?dist}
Summary:    Cacheing
License:    GPL
URL:        https://perso.univ-rennes1.fr/edouard.canot/f90cache/
# Official site temporarily unavailable:
#Source0:    https://perso.univ-rennes1.fr/edouard.canot/f90cache/f90cache-%{version}.tar.gz
Source0:    https://github.com/andriish/HEPsources/raw/master/f90cache-%{version}.tar.gz

BuildRequires:    gcc-c++
BuildRequires:    libtool

%description
Fortran ccache

%prep
%autosetup -p1


%build
%configure


%make_build

%install
%make_install


%files
%{_bindir}/f90cache
%{_mandir}/man1/*

%changelog
* Thu Nov 12 2026 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> - 0.99h-2
- Use the source mirror in andriish/HEPsources (official site unavailable)
* Thu Jan 30 2020 Fedora Release Engineering <releng@fedoraproject.org> - 2.3.22-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_32_Mass_Rebuild
