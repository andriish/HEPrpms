%undefine __cmake_in_source_build

%bcond_with doc
Name:           fmt
Version:        7.0.3
Release:        2%{?dist}
Summary:        Small, safe and fast formatting library for C++

License:        BSD
URL:            https://github.com/fmtlib/%{name}
Source0:        %{url}/archive/%{version}.tar.gz
# See https://github.com/fmtlib/fmt/issues/443 and https://lists.fedoraproject.org/archives/list/devel@lists.fedoraproject.org/message/LVKYLDLJVWAVJE4MQVKDO6PYZRD5MCP6/
Patch1:         doc-build-removed-all-pip-internet-stuff.patch
Patch3:         doc-build-do-not-create-virtual-environment.patch
Patch4:         doc-_templates-layout-stripped-Google-Analytics.patch
Patch5:         doc-_templates-layout-stripped-download-links.patch
Patch6:         doc-index-removed-GitHub-iframe.patch
Patch7:         doc-build-use-sphinx-build-3.patch
Patch8:         doc-build-use-python3.patch

BuildRequires:  gcc
BuildRequires:  gcc-c++
BuildRequires:  ninja-build
%if 0%{?rhel} && 0%{?rhel} <= 7
BuildRequires:  cmake3
%else
BuildRequires:  cmake
%endif
%if %{with doc}
BuildRequires:  doxygen
BuildRequires:  nodejs-less
%if 0%{?rhel} && 0%{?rhel} <= 7
BuildRequires:  python%{python3_version_nodots}-sphinx
BuildRequires:  python%{python3_version_nodots}-breathe
%else
BuildRequires:  python3-sphinx
BuildRequires:  python3-breathe
%endif
%else
Provides:       %{name}-doc = %{?epoch:%{epoch}:}%{version}-%{release}
Obsoletes:      %{name}-doc < %{?epoch:%{epoch}:}%{version}-%{release}
%endif

# This package replaces the old name of cppformat
Provides:       cppformat = %{?epoch:%{epoch}:}%{version}-%{release}
Obsoletes:      cppformat < %{?epoch:%{epoch}:}%{version}-%{release}

%description
C++ Format is an open-source formatting library for C++. It can be used as a
safe alternative to printf or as a fast alternative to IOStreams.

%package        devel
Summary:        Development files for %{name}
Requires:       %{name}%{?_isa} = %{?epoch:%{epoch}:}%{version}-%{release}

# This package replaces the old name of cppformat
Provides:       cppformat-devel = %{?epoch:%{epoch}:}%{version}-%{release}
Obsoletes:      cppformat-devel < %{?epoch:%{epoch}:}%{version}-%{release}

%description    devel
This package contains the header file for using %{name}.

%if %{with doc}
%package        doc
Summary:        Documentation files for %{name}
License:        Python
BuildArch:      noarch

# This package replaces the old name of cppformat
Provides:       cppformat-doc = %{?epoch:%{epoch}:}%{version}-%{release}
Obsoletes:      cppformat-doc < %{?epoch:%{epoch}:}%{version}-%{release}

%description    doc
This package contains documentation for developer documentation for %{name}.
%endif

%prep
%autosetup -p1

%if %{with doc}
# Remove --clean-css since that plugin isn't available
sed -i "s/'--clean-css',//" doc/build.py
%endif

%build
%if 0%{?rhel} && 0%{?rhel} <= 7
%cmake3                                       \
%else
%cmake                                        \
%endif
    -G Ninja                                  \
    -DCMAKE_BUILD_TYPE=RelWithDebInfo         \
    -DCMAKE_POSITION_INDEPENDENT_CODE=ON      \
    -DFMT_CMAKE_DIR=%{_libdir}/cmake/%{name}  \
    -DFMT_LIB_DIR=%{_libdir}

%cmake_build \
%if %{with doc}
    --target doc \
%endif
    --target all

%if %{with doc}
# Remove temporary build products
rm -rf %{_vpath_builddir}/doc/html/{.buildinfo,.doctrees,objects.inv}
%endif

%install
%cmake_install

%check
%ctest

%files
%license LICENSE.rst
%doc ChangeLog.rst README.rst
%{_libdir}/lib%{name}.so.7*

%files devel
%{_includedir}/%{name}
%{_libdir}/lib%{name}.so
%{_libdir}/cmake/%{name}
%{_libdir}/pkgconfig/%{name}.pc

%if %{with doc}
%files doc
%doc %{_datadir}/doc/%{name}
%license doc/python-license.txt
%endif

%changelog
* Mon May 03 2021 Vitaly Zaitsev <vitaly@easycoding.org> - 7.0.3-2
- Fixed RHBZ#1956521.

* Sat Aug 08 2020 Vitaly Zaitsev <vitaly@easycoding.org> - 7.0.3-1
- Updated to version 7.0.3.

* Wed Jul 29 2020 Vitaly Zaitsev <vitaly@easycoding.org> - 7.0.2-1
- Updated to version 7.0.2.

* Mon Jul 27 2020 Fedora Release Engineering <releng@fedoraproject.org> - 7.0.1-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_33_Mass_Rebuild

* Wed Jul 08 2020 Vitaly Zaitsev <vitaly@easycoding.org> - 7.0.1-1
- Updated to version 7.0.1.

* Sat May 09 2020 Vitaly Zaitsev <vitaly@easycoding.org> - 6.2.1-1
- Updated to version 6.2.1.

* Thu Apr 30 2020 Kefu Chai <tchaikov@gmail.com> - 6.2.0-2
- Incorporate patch from upstream to address https://github.com/fmtlib/fmt/issues/1631

* Mon Apr 06 2020 Vitaly Zaitsev <vitaly@easycoding.org> - 6.2.0-1
- Updated to version 6.2.0.

* Tue Jan 28 2020 Fedora Release Engineering <releng@fedoraproject.org> - 6.1.2-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_32_Mass_Rebuild

* Wed Dec 18 2019 Vitaly Zaitsev <vitaly@easycoding.org> - 6.1.2-1
- Updated to version 6.1.2.
- Recreated all documentation patches.
- SPEC file cleanup.

* Thu Jul 25 2019 Fedora Release Engineering <releng@fedoraproject.org> - 5.3.0-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_31_Mass_Rebuild

* Mon Jun 10 2019 Jan Staněk <jstanek@redhat.com> - 5.3.0-1
- Update to 5.3.0
- Recreate documentation build patches
- Package new pkg-config files

* Thu Jan 31 2019 Fedora Release Engineering <releng@fedoraproject.org> - 5.2.1-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_30_Mass_Rebuild

* Thu Oct 11 2018 Kefu Chai <tchaikov@gmail.com> - 5.2.1-1
- Update to 5.2.1
- Build using python3 packages on fedora
- Remove links in document accessing network
- Package ChangeLog.rst and README.rst
- Drop fmt-static package

* Fri Aug 31 2018 Leigh Scott <leigh123linux@googlemail.com> - 3.0.2-7
- Fix python2 issue for doc

* Fri Jul 13 2018 Fedora Release Engineering <releng@fedoraproject.org> - 3.0.2-6
- Rebuilt for https://fedoraproject.org/wiki/Fedora_29_Mass_Rebuild

* Wed Feb 07 2018 Fedora Release Engineering <releng@fedoraproject.org> - 3.0.2-5
- Rebuilt for https://fedoraproject.org/wiki/Fedora_28_Mass_Rebuild

* Wed Aug 09 2017 Dave Johansen <davejohansen@gmail.com> - 3.0.2-4
- Patch for Test 8 segfault

* Wed Aug 02 2017 Fedora Release Engineering <releng@fedoraproject.org> - 3.0.2-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_27_Binutils_Mass_Rebuild

* Wed Jul 26 2017 Fedora Release Engineering <releng@fedoraproject.org> - 3.0.2-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_27_Mass_Rebuild

* Sat Jun 24 2017 Dave Johansen <davejohansen@gmail.com> - 3.0.2-1
- Upstream release

* Mon May 15 2017 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 3.0.1-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_26_27_Mass_Rebuild

* Tue Dec 27 2016 Dave Johansen <davejohansen@gmail.com> - 3.0.1-2
- Build documentation

* Fri Nov 25 2016 Dave Johansen <davejohansen@gmail.com> - 3.0.1-1
- Upstream release

* Tue Nov 15 2016 Dave Johansen <davejohansen@gmail.com> - 3.0.0-2
- Fix expected unqualified-id before numeric constant error

* Wed Aug 24 2016 Dave Johansen <davejohansen@gmail.com> - 3.0.0-1
- Initial RPM release
