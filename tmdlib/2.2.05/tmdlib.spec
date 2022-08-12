Name:       tmdlib
Version:    2.2.05
Release:    2%{?dist}
Summary:    Library and plotting tools for Transverse Momentum Dependent parton distributions 
Prefix:     %{_prefix}
License:    GPLv3
URL:        https://tmdlib.hepforge.org/
Source0:    http://www.hepforge.org/archive/tmdlib/tmdlib-%{version}.tar.gz
Patch0:     patch-tmdlib-0.txt


%if 0%{?rhel} || 0%{?fedora}
BuildRequires: tex(latex) tex(sectsty.sty) gcc-c++  gcc-gfortran gsl-devel autoconf automake libtool lhapdf-devel root root-core boost boost-devel
Requires:       lhapdf libgfortran root root-core gsl
%endif
%if 0%{?suse_version}
BuildRequires: texlive-palatino texlive-helvetic texlive-courier tex(latex) tex(sectsty.sty) tex(listings.sty) tex(cite.sty) gcc-c++  gcc-fortran gsl gsl-devel autoconf automake libtool libLHAPDF LHAPDF-devel root6 root6-libs root6-devel boost-devel
BuildRequires: tex(lineno.sty)
Requires:       libLHAPDF libgfortran5 root6 root6-libs gsl
%endif

%description
TMDlib and TMDplotter: library and plotting tools for Transverse 
Momentum Dependent parton distributions  


%package  devel
Summary:        Libraries and headers for %{name}
Provides:       %{name}-devel = %{version}-%{release}

%description  devel
Contains the libraries and header files needed to
develop programs which make use of %{name}.
The library documentation is available on header files.


%prep
%setup -q
%patch0 -p1

%build
sed -i 's/2\.71/2\.65/g' configure.ac
autoreconf -fisv
%configure --with-lhapdf=/usr  --with-gsl=/usr

%install
make %{?_smp_mflags} 
%make_install 
rm -f $RPM_BUILD_ROOT//usr/%_lib/*.la

%post -p /sbin/ldconfig

%postun -p /sbin/ldconfig


%files
/usr/bin/*
/usr/%_lib/*
/usr/share/man/TMDlib*.pdf
/usr/share/tmdlib/*



%files  devel
/usr/include/*

%changelog
* Mon Jan 17 2022 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - 2.2.05
* Sun Feb 21 2021 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - Cleanup 
* Mon Oct 15 2018 Andrii verbytskyi <andrii.verbytskyi@mpp.mpg.de> - init
- init

