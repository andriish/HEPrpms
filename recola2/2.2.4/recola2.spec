%undefine _debugsource_packages
Summary:  A Fortran95 computer program for the automated generation and numerical computation of EW and QCD amplitudes.
Name: recola2
Version: 2.2.4
Release: 1%{?dist}
License: GPLv3
Prefix: %{_prefix}
Source0: https://www.hepforge.org/archive/recola/recola2-%{version}.tar.gz
Source1: https://www.hepforge.org/archive/recola/SM_2.2.3.tar.gz
URL:   https://recola.gitlab.io/recola2/
BuildRequires:      collier gcc-c++ 
Requires:      collier
%if 0%{?rhel} || 0%{?fedora}
BuildRequires:      gcc-gfortran
BuildRequires:      python3-devel
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
BuildRequires:      cmake >= 3.4.3
%else
BuildRequires:      cmake3 >= 3.4.3
%endif
%endif
%if 0%{?suse_version}
BuildRequires:      gcc-fortran
BuildRequires:      cmake >= 3.4.3
BuildRequires: python3-devel
%endif


%description
Recola is a Fortran95 computer program for the automated generation and 
numerical computation of EW and QCD amplitudes in the Standard Model at 
next-to-leading order. 
%prep 
%setup -q 
%setup -q -T -D -a 1
%build
cd SM_2.2.3

%if 0%{?rhel} || 0%{?fedora}
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
%cmake \
%else
%cmake3 \
%endif
 .   -DCOLLIER_LIB_PATH=/usr/share/cmake -DSYSCONFIG_INSTALL_DIR=%{_prefix}/share/cmake/  
%make_build 
%endif
%if 0%{?suse_version}
%cmake .   -DCOLLIER_LIB_PATH=/usr/share/cmake -DSYSCONFIG_INSTALL_DIR=%{_prefix}/share/cmake/  
%make_build -C build
%endif

cd ..


%if 0%{?rhel} || 0%{?fedora}
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
%cmake \
%else
%cmake3 \
%endif
 .   -DCOLLIER_LIB_PATH=/usr/share/cmake -DSYSCONFIG_INSTALL_DIR=%{_prefix}/share/cmake/  -Dcollier_DIR=/usr/share/cmake/  -Dmodelfile_path=$(pwd)/SM_2.2.3 -Dwith_python3=On
%make_build 
%endif
%if 0%{?suse_version}
%cmake .   -DCOLLIER_LIB_PATH=/usr/share/cmake -DSYSCONFIG_INSTALL_DIR=%{_prefix}/share/cmake/  -Dcollier_DIR=/usr/share/cmake/ -Dmodelfile_path=$(pwd)/SM_2.2.3 -Dwith_python3=On
%make_build -C build
%endif

%install
cd SM_2.2.3

%if 0%{?rhel} || 0%{?fedora}
%if %{?fedora}%{!?fedora:0}
%cmake_install
%else
%make_install
%endif
%endif
%if 0%{?suse_version}
%make_install -C build
%endif

cd ..


%if 0%{?rhel} || 0%{?fedora}
%if %{?fedora}%{!?fedora:0}
%cmake_install
%else
%make_install
%endif
%endif
%if 0%{?suse_version}
%make_install -C build
%endif

%files
%defattr(-,root,root)
%{_includedir}/*
%{_libdir}/*
%{_prefix}/share/cmake/*
%{python3_sitelib}/pyrecola*

%clean
rm -rf %{buildroot}

%post 
ldconfig 

%changelog
* Tue Jul 12 2022 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
  - 2.2.4
* Thu Nov 23 2017 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - Initial