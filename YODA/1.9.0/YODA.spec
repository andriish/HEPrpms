%define major       0
%define libname     YODA
%define libnamedev  YODA-devel

Name:           YODA
Version:        1.9.0
Release:        1002%{?dist}
License:        GPLv3
Url:            http://yoda.hepforge.org/
Source0:        http://www.hepforge.org/archive/yoda/%{name}-%{version}.tar.gz
Patch0:         patch-YODA-0.txt
Prefix:         %{_prefix}
Summary:        Plotting and histogramming tool
BuildRequires:   autoconf binutils automake libtool  zlib-devel
%if %{?rhel}%{!?rhel:0} >= 8
BuildRequires: gcc-c++  python3-Cython   zlib
%endif
%if %{?fedora}%{!?fedora:0}
BuildRequires: gcc-c++  Cython   zlib
%endif
%if 0%{?suse_version}
BuildRequires:  gcc-c++ python3-devel python3-libs pkgconfig(zlib)
%endif


%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
BuildRequires:   python3  python3-devel python3-libs
%endif
%if %{?rhel}%{!?rhel:0} >= 8
BuildRequires: platform-python-devel
%endif


%description
Needed for Rivet.

%package  devel
Summary:        Libraries and headers for %{name}
Provides:       %{name}-devel = %{version}-%{release}

%description devel
%{libnamedev} contains the libraries and header files needed to
develop programs which make use of %{name}.




%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
%package  -n python%{python3_pkgversion}-%{name}
Summary:        python bindings for %{name}
Provides:       python%{python3_pkgversion}-%{name} = %{version}-%{release}

%description -n python%{python3_pkgversion}-%{name}
python%{python3_pkgversion}-%{name} contains python bindings for %{name}.
%endif


%if 0%{?suse_version}
%package  -n python3-%{name}
Summary:        python bindings for %{name}
Provides:       python3-%{name} = %{version}-%{release}

%description -n python3-%{name}
python-%{name} contains python bindings for %{name}.
%endif

#Yes, suse is stupid
%if 0%{?suse_version}

%debug_package

%endif

%prep
%setup -q
%patch0 -p1
%if 0%{?suse_version}
#FROM SUSE

# USE PYTHON3 FOR HASHBANGS
sed -Ei "1{s|/usr/bin/python|/usr/bin/python3|}" bin/*
sed -Ei "1{s|/usr/bin/env python|/usr/bin/python3|}" bin/*
sed -Ei "1{s|/usr/bin/env python|/usr/bin/python3|}" tests/pytest-*

# FIX env BASED HASHBANGS
sed -E -i "s|^#! /usr/bin/env bash|#! /bin/bash|" bin/yoda-config*

# REMOVE AN UNNECESSARY ONE
sed -E -i "1{s|^#! /usr/bin/env python||}" pyext/yoda/search.py
%endif


%build


%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
export PYTHON=%{_bindir}/python3
export CXXFLAGS="%{optflags} -Wformat -Wno-error"
pathfix.py -pn -i %{__python3}  ./
pathfix.py -pn -i %{__python3}  bin/flat2yoda
pathfix.py -pn -i %{__python3}  bin/yoda*
pathfix.py -pn -i %{__python3}  bin/aida*
autoreconf -fi
%configure  --disable-root
%make_build %{?_smp_mflags}
%endif

%if 0%{?suse_version}
export PYTHON_VERSION=%{py3_ver}
export CXXFLAGS="%{optflags} -Wformat -Wno-error -g"
autoreconf -fi
%configure  --disable-root
%make_build %{?_smp_mflags}
%endif

%install
%make_install
find %{buildroot}/%{_libdir}/ -name ".la" -delete

%files -n %{libname}
/etc/bash_completion.d/yoda-completion
%doc AUTHORS README COPYING
/usr/%_lib/lib*
/usr/%_lib/pkgconfig/*.pc
%_bindir/*



%files -n %{libnamedev}
%_includedir/YODA/*.h
%_includedir/YODA/*.icc
%_includedir/YODA/Utils/*.h
%_includedir/YODA/Config/*.h




%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
%files -n python%{python3_pkgversion}-%{name}
%{python3_sitearch}/*
%endif


%if 0%{?suse_version}
%files -n python3-%{name}
/usr/%_lib/python*/site-packages/*
%endif


%changelog
* Sat Jul 03 2021 Andrii Verbytskyi 1.9.0
- Added support for newest python
* Thu Apr 01 2021 Andrii Verbytskyi 1.9.0
- version bump
* Sat Mar 13 2021 Andrii Verbytskyi 1.8.5
- Added root and python3
* Fri Jan 26 2018 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> 1.7.0
+ imported package YODA

