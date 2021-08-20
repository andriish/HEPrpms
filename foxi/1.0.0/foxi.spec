%undefine _debugsource_packages
%define git_version c278588e34e535f0bb8f00df3880d26928038cad
Name:           foxi
Version:        1.0.0
Release:        1%{?dist}
Summary:        Library for C++

License:        BSD
URL:            https://github.com/houseroad/foxi
Source0:        https://github.com/houseroad/foxi/archive/%{git_version}.tar.gz


BuildRequires:  gcc
BuildRequires:  gcc-c++
BuildRequires:  cmake

%description
Some lib

%package        devel
Summary:        Development files for %{name}
Requires:       %{name}%{?_isa} = %{version}-%{release}


%description    devel
This package contains the header file for using %{name}.


%prep
%setup -q -n %{name}-%{git_version}
sed -i 's@foxi_dummy SHARED foxi/onnxifi_dummy.c@foxi_dummy SHARED EXCLUDE_FROM_ALL foxi/onnxifi_dummy.c @g'  CMakeLists.txt
sed -i 's@foxi_wrapper MODULE foxi/onnxifi_wrapper.c@foxi_wrapper MODULE EXCLUDE_FROM_ALL foxi/onnxifi_wrapper.c @g'  CMakeLists.txt
sed -i 's@DESTINATION lib@DESTINATION lib64@g'  CMakeLists.txt

    
    
%build
%cmake   
%cmake_build
touch libfoxi_dummy.so
touch libfoxi.so
touch x86_64-redhat-linux-gnu/libfoxi.so
touch x86_64-redhat-linux-gnu/libfoxi_dummy.so

%install
%cmake_install
rm -f %{buildroot}/%{_libdir}/libfoxi.so
rm -f %{buildroot}/%{_libdir}/libfoxi_dummy.so

%files 
%{_libdir}/*

%files devel
%{_includedir}/*

%changelog
* Fri Jan 26 2018 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> 1.7.0
- start
