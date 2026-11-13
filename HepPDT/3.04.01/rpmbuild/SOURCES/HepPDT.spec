%if 0%{?suse_version}
%define _pkgdocdir %{buildroot}%{_docdir}/%{name}
%endif
Name:		HepPDT
Version:	3.04.01
Release:	3%{?dist}
Summary:	Helpers for HEP Monte Carlo Generators

License:	GPLv2+
URL:		http://hepmc.web.cern.ch/hepmc/
Source0:	https://hepmc.web.cern.ch/hepmc/releases/%{name}-%{version}.tar.gz

BuildRequires:	gcc-c++

%description
The HepPDT package 

%package devel
Summary:	Helpers for HEP Monte Carlo Generators
Requires:	%{name}%{?_isa} = %{version}-%{release}

%description devel
This package provides development files of HepPDT.

%package doc
Summary:	Helpers for HEP Monte Carlo Generators - documentation
BuildArch:	noarch

%description doc
This package provides HepPDT manuals and examples.

%prep
%setup -q

%build
%configure  --disable-static
make %{?_smp_mflags}

%install
%make_install


mkdir -p %{buildroot}%{_pkgdocdir}
mv %{buildroot}/usr/examples  %{buildroot}%{_pkgdocdir}
mv %{buildroot}/usr/doc %{buildroot}%{_pkgdocdir}
mkdir -p %{buildroot}%{_datadir}/%{name}/
mv %{buildroot}/usr/data %{buildroot}%{_datadir}/%{name}/

%check
#make check

%ldconfig_scriptlets

%files
%{_libdir}/lib*.so*


%files devel
%{_libdir}/lib*.so
%{_includedir}/%{name}
%{_includedir}/HepPID

%files doc
%dir %{_pkgdocdir}
%dir  %{_datadir}/%{name}/
%{_datadir}/%{name}/*
%doc %{_pkgdocdir}/


%changelog
* Tue Jan 28 2020 AV <andrii.verbytskyi@mpp.mpg.de> - 3.04.01-1
- Initial
