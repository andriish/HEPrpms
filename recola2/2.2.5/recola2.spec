%undefine _debugsource_packages
Summary:  A Fortran95 computer program for the automated generation and numerical computation of EW and QCD amplitudes.
Name: recola2
Version: 2.2.5
Release: 3%{?dist}
License: GPLv3
Prefix: %{_prefix}
Source0: https://gitlab.com/recola/recola2/-/archive/%{version}/recola2-%{version}.tar.gz
Patch0:         patch-recola2-0.txt
Patch1:         mr-26.patch
URL:   https://recola.gitlab.io/recola2/
BuildRequires:      collier gcc-c++ recola2-SM 
Requires:      collier recola2-SM
%if 0%{?fedora} || %{?rhel}%{!?rhel:0} >= 10
BuildRequires: chrpath
%endif
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
%patch -P 0 -p1
%patch -P 1 -p1

%build


%if 0%{?rhel} || 0%{?fedora}
%cmake   -DCOLLIER_LIB_PATH=/usr/share/cmake -DSYSCONFIG_INSTALL_DIR=%{_prefix}/share/cmake/  -Dcollier_DIR=/usr/share/cmake/  -Dmodelfile_path=/usr/share/cmake  -Dwith_python3=On
%cmake_build 
%endif
%if 0%{?suse_version}
%cmake  -DCOLLIER_LIB_PATH=/usr/share/cmake -DSYSCONFIG_INSTALL_DIR=%{_prefix}/share/cmake/  -Dcollier_DIR=/usr/share/cmake/ -Dmodelfile_path=/usr/share/cmake   -Dwith_python3=On
%make_build
%endif

%install




%if 0%{?rhel} || 0%{?fedora}
%cmake_install
%if 0%{?fedora} || %{?rhel}%{!?rhel:0} >= 10
chrpath --delete $RPM_BUILD_ROOT%{_libdir}/librecola.so
chrpath --delete $RPM_BUILD_ROOT/usr/lib/python3.*/site-packages/pyrecola.so
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
* Wed Oct 08 2025 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
  - 2.2.5
* Tue Jul 12 2022 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
  - 2.2.4
* Thu Nov 23 2017 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - Initial
