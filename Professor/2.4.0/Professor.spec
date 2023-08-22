%define major       0
%define libname     Professor
%define libnamedev  Professor-devel

Name:           Professor
Version:        2.4.0
Release:        1%{?dist}
License:        Unknown
Url:            http://professor.hepforge.org/
Source0:        https://www.hepforge.org/archive/professor/%{name}-%{version}.tar.gz
Patch0:         patch-Professor-0.txt
Prefix:         %{_prefix}
Summary:        A tuning tool for Monte Carlo event generators.
Requires:       YODA 
BuildRequires:  YODA-devel eigen3-devel gcc-c++ 


%if %{?rhel}%{!?rhel:0} >= 8
Requires: python3 python3-numpy root-core
BuildRequires: python3-numpy python3-devel platform-python-devel python3-Cython root-core python3-pip python3-setuotools
%endif
%if %{?fedora}%{!?fedora:0} 
Requires: root-core
BuildRequires: python3-devel  Cython root-core  python3-pip python3-setuotools
%endif
%if 0%{?suse_version}
Requires: root6-libs root6
BuildRequires: python3-devel  python3-Cython  python3-tools root6-libs root6
%endif
%if %{?fedora}%{!?fedora:0} >= 39
BuildRequires: python3-rpm-macros
%endif

%description
 Professor is a tuning tool for Monte Carlo event generators, based on 
 the ideas described in "Tuning and Test of Fragmentation Models Based 
 on Identified Particles and Precision Event Shape Data" (Z. Phys., C73 (1996) 11-60).

%prep
%setup -q
%patch0 -p1


%build

%if  %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
export PYTHON=/usr/bin/python3
export CXXFLAGS='%{optflags} -O0 ' 
export CPPFLAGS=-I/usr/include/eigen3
sed -i 's@python@python3@1'   Makefile
%if %{?fedora}%{!?fedora:0} >= 39
%py3_shebang_fix   ./
%py3_shebang_fix   ./bin/prof*
%py3_shebang_fix  ./contrib/prof*
%else
pathfix.py -pn -i %{__python3}  ./
pathfix.py -pn -i %{__python3}  ./bin/prof*
pathfix.py -pn -i %{__python3}  ./contrib/prof*
%endif
%make_build %{?_smp_mflags}
%endif

%if 0%{?suse_version}
export PYTHON=/usr/bin/python3
export CXXFLAGS='%{optflags} -O0 ' 
export CPPFLAGS=-I/usr/include/eigen3
%make_build
%endif


%install
%make_install PREFIX=%{_prefix} LIBDIR=%{_prefix}/%{_lib} PYTHONPATH=$PYTHONPATH:$RPM_BUILD_ROOT/%{python3_sitearch}
rm -rf $RPM_BUILD_ROOT/%{_prefix}/jupyter/*
rm -rf $RPM_BUILD_ROOT/%{python3_sitearch}/*pth


%files -n %{libname}
%{_bindir}/*
%{_libdir}/libProfessor2.so
%{python3_sitearch}/professor2*
%{_prefix}/contrib/*
%{_prefix}/include/Professor/*




%changelog
* Fri Jan 20 2012 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> 1.4.0
+ imported package Professor

