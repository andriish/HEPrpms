Name:           fjcontrib
Version:        1.049
Release:        1%{?dist}
License:        GPLv2+
Url:            http://www.fjcontrib.fr
Source0:        https://fastjet.hepforge.org/contrib/downloads/%{name}-%{version}.tar.gz
Patch0:         patch-fjcontrib-0.txt
Prefix:         %{_prefix}
Summary:        The fastjet-contrib space is intended to provide a common location for access to 3rd party extensions of FastJet. 
BuildRequires:  gcc-c++ fastjet-devel fastjet
Requires:       fastjet
%if 0%{?rhel} || 0%{?fedora}
BuildRequires:  gcc-gfortran
%endif
%if 0%{?suse_version}
BuildRequires:  gcc-fortran
%endif



%description
The fastjet-contrib space is intended to provide a common 
location for access to 3rd party extensions of FastJet.  


%package  devel
Summary:        Libraries and headers for %{name}
Provides:       %{name}-devel = %{version}-%{release}

%description  devel
Contains the libraries and header files needed to
develop programs which make use of %{name}.
The library documentation is available on header files.


%prep
%setup -q
%patch0 -p2


%build

%configure  CXXFLAGS="%{optflags} -fPIC"
make  
make fragile-shared  
%install
make install  DESTDIR=%{buildroot}
make  fragile-shared-install  DESTDIR=%{buildroot}

%post 
ldconfig 

%files 
%_libdir/*

%files devel
%_includedir/fastjet/contrib/*


%changelog
* Tue Jul 12 2022 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - Version bump
* Sun Feb 21 2021 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - Cleanup              
* Thu Nov 23 2017 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - Initial

