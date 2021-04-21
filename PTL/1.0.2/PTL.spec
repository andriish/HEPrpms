%undefine _debugsource_packages
Summary:  PTL Lightweight C++11 multithreading tasking system featuring thread-pool, task-groups, and lock-free task queue
Name: PTL
Version: 1.0.2
Release: 2%{?dist}
License: MIT
Prefix: %{_prefix}
Source: https://github.com/jrmadsen/PTL/archive/v%{version}.tar.gz
Requires:       tbb-devel
BuildRequires:  gcc-c++ tbb-devel
BuildRequires:  cmake >= 3.4.3

%description
PTL Lightweight C++11 multithreading tasking system featuring thread-pool, task-groups, and lock-free task queue

%package devel
Summary: Lightweight C++11 multithreading tasking system featuring thread-pool, task-groups, and lock-free task queue
Requires: %{name}%{?_isa} = %{version}-%{release}

%description devel
PTL Lightweight C++11 multithreading tasking system featuring thread-pool, task-groups, and lock-free task queue


%prep 
%setup -q -n PTL-%{version}


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
%{_libdir}/PTL


%clean
rm -rf %{buildroot}

%post 
ldconfig 

%changelog
* Sat Mar 13 2021 Andrii Verbytskyi 1.0.2
- Update to 1.0.2     
* Sun Jan 10 2021 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - Initial
