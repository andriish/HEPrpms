%global so_ver 11

Name:             ginac
Version:          1.8.6
Release:          2
Summary:          C++ library for symbolic calculations
License:          GPLv2+
URL:              https://www.ginac.de/
Source0:          https://www.ginac.de/%{name}-%{version}.tar.bz2

BuildRequires:    gcc-c++
BuildRequires:    bison
BuildRequires:    cln-devel
BuildRequires:    cmake
BuildRequires:    flex
BuildRequires:    doxygen
BuildRequires:    python3-devel
BuildRequires:    readline-devel

%if 0%{?rhel} || 0%{?fedora}
BuildRequires:    tex(dvips)
BuildRequires:    tex(latex)
BuildRequires:    tex(latex-base)
BuildRequires:    texinfo
BuildRequires:    texinfo-tex
%endif
%if 0%{?suse_version}
BuildRequires:    tex(latex) texlive-filesystem
%endif

BuildRequires:    transfig
Obsoletes:        GiNaC < 1.3.2-999
Provides:         GiNaC = %{version}-%{release}
Provides:         GiNaC%{?_isa} = %{version}-%{release}

%description
GiNaC (which stands for "GiNaC is Not a CAS (Computer Algebra System)") is an
open framework for symbolic computation within the C++ programming language.

%package          devel
Summary:          Development files for %{name}
Requires:         %{name}%{?_isa} = %{version}-%{release}
Requires:         cln-devel%{?_isa}
Obsoletes:        GiNaC-devel < 1.3.2-999
Provides:         GiNaC-devel = %{version}-%{release}
Provides:         GiNaC-devel%{?_isa} = %{version}-%{release}

%description      devel
This package contains libraries and header files for
developing applications that use %{name}.

%package          utils
Summary:          GiNaC-related utilities
Requires:         %{name}%{?_isa} = %{version}-%{release}
Obsoletes:        GiNaC-utils < 1.3.2
Provides:         GiNaC-utils = %{version}-%{release}

%description      utils
This package includes ginsh ("GiNaC interactive shell") which provides a
simple and easy-to-use CAS-like interface to GiNaC for non-programmers, and
the tool "viewgar" which displays the contents of GiNaC archives.

%prep
%autosetup -p1
# Destroy the RPATH.
sed -i 's| @GINACLIB_RPATH@||' ginac.pc.{in,cmake}

%build
%cmake -DCMAKE_INSTALL_RPATH="" -DLIBEXECDIR=%{_libexecdir}
%cmake_build

%if %{?fedora}%{!?fedora:0}
%cmake_build --target ginac_html
%endif

%install
%cmake_install
rm -frv %{buildroot}%{_infodir}/dir
find %{buildroot} -name '*.la' -delete -print

for f in $(find %{buildroot} -name "*.py") ; do
  sed -i.orig "s:^#\!/usr/bin/env\s\+python:#!%{__python3}:" $f
  touch -r $f.orig $f
  rm $f.orig
done

%check
%if %{?fedora}%{!?fedora:0}
export CTEST_OUTPUT_ON_FAILURE=1
%cmake_build --target check
%endif

%files
%license COPYING
%{_libdir}/*.so.%{so_ver}
%{_libdir}/*.so.%{so_ver}.*
%{_libexecdir}/ginac-excompiler

%files devel
%doc AUTHORS NEWS README
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} > 8 
%doc %{_vpath_builddir}/doc/tutorial/ginac.html
%{_infodir}/*.info*
%endif
%{_includedir}/ginac/
%{_libdir}/*.so
%{_libdir}/pkgconfig/ginac.pc
%dir %{_libdir}/cmake
%{_libdir}/cmake/ginac/

%files utils
%{_bindir}/*

%changelog
* Fri Mar 10 2023 Andrii Verbytskyi 1.8.6
- Update to 1.8.6
