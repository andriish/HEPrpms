%undefine _debugsource_packages
%if 0%{?suse_version}
%define python3_pkgversion 38
%endif

Summary:  recola - Recola is a Fortran95 computer program for the automated generation and numerical computation of EW and QCD amplitudes in the Standard Model at next-to-leading order. 
Name: recola2
Version: 2.2.2
Release: 3%{?dist}
License: GPLv3
Prefix: %{_prefix}
Source0: https://www.hepforge.org/archive/recola/recola2-%{version}.tar.gz
Source1: https://www.hepforge.org/archive/recola/SM_2.2.3.tar.gz
#Source2: https://www.hepforge.org/archive/recola/SM_BFM_2.2.3.tar.gz
#Source3: https://www.hepforge.org/archive/recola/SM_FERM_2.2.3.tar.gz
#Source4: https://www.hepforge.org/archive/recola/SM_FERM_YUK_2.2.3.tar.gz
#Source5: https://www.hepforge.org/archive/recola/SM_NF5_2.2.3.tar.gz
#Source6: https://www.hepforge.org/archive/recola/SM_NF4_2.2.3.tar.gz


Requires: collier
%if 0%{?rhel} || 0%{?fedora}
%if  %{?fedora}%{!?fedora:0} >= 35
BuildRequires: chrpath
%endif
BuildRequires: collier gcc-gfortran gcc-c++ 
BuildRequires:	python%{python3_pkgversion}-devel
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
BuildRequires:	cmake >= 3.4.3
%else
BuildRequires:	cmake3 >= 3.4.3
%endif
%endif
%if 0%{?suse_version}
BuildRequires: collier  gcc-c++ 
BuildRequires:	python%{python3_pkgversion}-devel
BuildRequires: gcc-fortran python-rpm-macros
BuildRequires:	cmake >= 3.4.3
%endif


%description
Recola is a Fortran95 computer program for the automated generation and 
numerical computation of EW and QCD amplitudes in the Standard Model at 
next-to-leading order. 

%package -n python%{python3_pkgversion}-%{name}
Summary:	HepMC3 Python 3 bindings
%{?python_provide:%python_provide python%{python3_pkgversion}-%{name}}
Requires:	%{name}%{?_isa} = %{version}-%{release}

%description -n python%{python3_pkgversion}-%{name}
This package provides the Python 3 bindings for %{name}.

%prep 
%setup -q 
%setup -q -T -D -a 1
sed -i 's|3.4 3.5 3.6 3.8| 3.10 3.9 3.8 3.6 3.5 3.4 |g' src/CMakeLists.txt
sed -i 's|lib/python|lib64/python|g' src/CMakeLists.txt



%build
cd SM_2.2.3


%if  %{?fedora}%{!?fedora:0}||%{?rhel}%{!?rhel:0} >= 8
%cmake -DCMAKE_SKIP_RPATH=ON  -DCOLLIER_LIB_PATH=/usr/share/cmake -DSYSCONFIG_INSTALL_DIR=%{_prefix}/share/cmake/  -Dcollier_DIR=/usr/share/cmake/
%cmake_build 
%endif


%if 0%{?suse_version}
%cmake  -DCMAKE_SKIP_RPATH=ON  -DCOLLIER_LIB_PATH=/usr/share/cmake -DSYSCONFIG_INSTALL_DIR=%{_prefix}/share/cmake/  -Dcollier_DIR=/usr/share/cmake/
%cmake_build
cd ..
%endif

cd ..
pwd

%if  %{?fedora}%{!?fedora:0}||%{?rhel}%{!?rhel:0} >= 8
find $(pwd)/SM_2.2.3 
%cmake \
     -DCOLLIER_LIB_PATH=/usr/share/cmake -DSYSCONFIG_INSTALL_DIR=%{_prefix}/share/cmake/  \
     -Dcollier_DIR=/usr/share/cmake/  -Dwith_python3=ON \
     -Dmodelfile_path=$(pwd)/SM_2.2.3 
%cmake_build
%endif


%if 0%{?suse_version}
pwd
#find $(pwd)
ls $(pwd)/SM_2.2.3
%cmake   -DCOLLIER_LIB_PATH=/usr/share/cmake -DSYSCONFIG_INSTALL_DIR=%{_prefix}/share/cmake/  \
         -Dcollier_DIR=/usr/share/cmake/  -Dwith_python3=ON \
         -DMDL_SEARCH_PATH=$(pwd)/../SM_2.2.3 -Dmodelfile_DIR=$(pwd)/../SM_2.2.3
%cmake_build
%endif



%install

%if  %{?fedora}%{!?fedora:0}||%{?rhel}%{!?rhel:0} >= 8
cd SM_2.2.3
%cmake_install
cd ..
%cmake_install

%if  %{?fedora}%{!?fedora:0} >= 35
chrpath --delete $RPM_BUILD_ROOT%{_prefix}/%_lib/librecola.so
chrpath --delete $RPM_BUILD_ROOT%{_prefix}/lib*/python*/site-packages/pyrecola.so
%endif
%endif

%if 0%{?suse_version}
cd SM_2.2.3
%cmake_install
pwd
cd ..
%cmake_install
%endif



%files
%defattr(-,root,root)
/%{_prefix}/include/*
%{_prefix}/%_lib/*
%{_prefix}/share/cmake/*

%files -n python%{python3_pkgversion}-%{name}
%{python3_sitearch}/pyrecola.so

%post 
ldconfig 

%changelog             
* Thu Nov 23 2017 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - Initial
