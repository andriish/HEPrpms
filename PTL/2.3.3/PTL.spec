%undefine _debugsource_packages
Summary: Lightweight C++11 multithreading tasking system featuring thread-pool, task-groups, and lock-free task queue
Name: PTL
Version: 2.3.3
Release: 1003%{?dist}
License: MIT
Prefix: %{_prefix}
Source: https://github.com/jrmadsen/PTL/archive/v%{version}.tar.gz
Patch0:     patch-PTL-0.txt
URL: https://github.com/jrmadsen/PTL
Requires:       tbb-devel
BuildRequires:  gcc-c++ tbb-devel
BuildRequires:  cmake >= 3.4.3

%description
Lightweight C++11 multithreading tasking system featuring thread-pool, task-groups, and lock-free task queue.

%package devel
Summary: Lightweight C++11 multithreading tasking system featuring thread-pool, task-groups, and lock-free task queue
Requires: %{name}%{?_isa} = %{version}-%{release}

%description devel
PTL Lightweight C++11 multithreading tasking system featuring thread-pool, task-groups, and lock-free task queue


%prep 
%setup -q -n PTL-%{version}
%patch -P 0 -p1

%build
%cmake     -DPTL_INSTALL_INCLUDEDIR=%{_includedir} -DPTL_INSTALL_LIBDIR=%{_libdir}
%cmake_build 

%install
%cmake_install

%files  
%defattr(-,root,root)
%{_libdir}/lib*



%files  devel
%defattr(-,root,root)
%{_includedir}/PTL
%{_libdir}/cmake/*
%{_libdir}/pkgconfig/*


%clean
rm -rf %{buildroot}

%post 
ldconfig 

%changelog
* Fri Mar 10 2023 Andrii Verbytskyi 2.3.3
- Update to 2.3.3
* Wed Aug 11 2021 Andrii Verbytskyi 2.0.0
- Update to 2.0.0     
* Sat Mar 13 2021 Andrii Verbytskyi 1.0.2
- Update to 1.0.2     
* Sun Jan 10 2021 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - Initial
