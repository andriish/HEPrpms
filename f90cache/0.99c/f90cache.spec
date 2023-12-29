%define debug_package %{nil}
Name:       f90cache
Version:    0.99c
Release:    2%{?dist}
Summary:    Cacheing
License:    GPL
URL:        https://perso.univ-rennes1.fr/edouard.canot/f90cache/
Source0:    https://perso.univ-rennes1.fr/edouard.canot/f90cache/f90cache-%{version}.tar.gz
Patch0:     patch-f90cache-0.txt

BuildRequires:    gcc-c++
BuildRequires:    libtool

%description
Fortran ccache

%prep
%autosetup -p1


%build
sed -i 's/9<GNU_MAJOR_VERSION_NUM/20<GNU_MAJOR_VERSION_NUM/g' f90cache.c
cat f90cache.c | grep NU_MAJOR_VERSIO
%configure 


%make_build

%install
%make_install


%files 
%{_bindir}/f90cache
%{_mandir}/man1/*

%changelog
* Thu Jan 30 2020 Fedora Release Engineering <releng@fedoraproject.org> - 2.3.22-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_32_Mass_Rebuild
