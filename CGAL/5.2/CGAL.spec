# CGAL-5.0 is now a header-only library, with dependencies. It no
# longer has any binary to build, but cannot be noarch because of
# arch-specific dependencies
%global debug_package %{nil}

# Min dependencies
%global boost_version 1.66
%global qt_version 5.9
%global cmake_version 3.11

%global fullversion %{version}
#global fullversion 5.2-beta1


Name:           CGAL
Version:        5.2
Release:        1%{?dist}
Summary:        Computational Geometry Algorithms Library

License:        LGPLv3+ and GPLv3+ and Boost
URL:            http://www.cgal.org/
Source0:        https://github.com/CGAL/cgal/releases/download/v%{fullversion}/%{name}-%{fullversion}.tar.xz


# Required devel packages.
BuildRequires: cmake >= %{cmake_version}
BuildRequires: gcc-c++
BuildRequires: gmp-devel
BuildRequires: boost-devel >= %{boost_version}
BuildRequires: mpfr-devel
BuildRequires: make
%if 0%{?rhel} || 0%{?fedora}
BuildRequires: qt5-qtbase-devel >= %{qt_version}
BuildRequires: qt5-qtsvg-devel >= %{qt_version}
BuildRequires: qt5-qtscript-devel >= %{qt_version}
BuildRequires: qt5-qttools-devel >= %{qt_version}
%endif
%if 0%{?suse_version}
BuildRequires:  cmake(Qt5) >= 5.3
BuildRequires:  cmake(Qt5OpenGL) >= 5.3
BuildRequires:  cmake(Qt5Svg) >= 5.3
BuildRequires:  cmake(Qt5Xml) >= 5.3
%endif





%description
Libraries for CGAL applications.
CGAL is a collaborative effort of several sites in Europe and
Israel. The goal is to make the most important of the solutions and
methods developed in computational geometry available to users in
industry and academia in a C++ library. The goal is to provide easy
access to useful, reliable geometric algorithms.


%package devel
Summary:        Development files and tools for CGAL applications
Provides:       CGAL-static = %{version}-%{release}
Requires:       cmake
%if 0%{?rhel} || 0%{?fedora}
Requires:       boost-devel%{?_isa} >= %{boost_version}
%endif
%if 0%{?suse_version}
Requires:       boost-devel >= %{boost_version}
%endif
Requires:       gmp-devel%{?_isa}
Requires:       mpfr-devel%{?_isa}
Requires:     zlib-devel%{?_isa}
Requires:     eigen3-devel
%description devel
Libraries for CGAL applications.
CGAL is a collaborative effort of several sites in Europe and
Israel. The goal is to make the most important of the solutions and
methods developed in computational geometry available to users in
industry and academia in a C++ library. The goal is to provide easy
access to useful, reliable geometric algorithms.
The %{name}-devel package provides the headers files and tools you may need to
develop applications using CGAL.



%package qt5-devel
Summary:        Development files and tools for CGAL applications using CGAL_Qt5



Requires:       %{name}-devel = %{version}-%{release}
%if 0%{?rhel} || 0%{?fedora}
Requires:       qt5-qtbase-devel%{?_isa} >= %{qt_version}
Requires:       qt5-qtsvg-devel%{?_isa} >= %{qt_version}
Requires:       qt5-qtscript-devel%{?_isa} >= %{qt_version}
Requires:       qt5-qttools-devel%{?_isa} >= %{qt_version}
%endif

%if 0%{?suse_version}
Requires:  cmake(Qt5) >= 5.3
Requires:  cmake(Qt5OpenGL) >= 5.3
Requires:  cmake(Qt5Svg) >= 5.3
Requires:  cmake(Qt5Xml) >= 5.3
%endif


%description qt5-devel
The %{name}-qt5-devel package provides the headers files and tools you
may need to develop applications using the CGAL_Qt5 component of CGAL.


%package demos-source
Summary:        Examples and demos of CGAL algorithms
Requires:       %{name}-devel = %{version}-%{release}
%description demos-source
The %{name}-demos-source package provides the sources of examples and demos of
CGAL algorithms.


%prep
%setup -q -n %{name}-%{fullversion}

# Fix some file permissions
#chmod a-x include/CGAL/export/ImageIO.h

# Install README.Fedora here, to include it in %%doc
cat << 'EOF' > ./README.Fedora
Header-only
-----------
CGAL is a header-only library since version 5.0.

Packages
--------
In Fedora, the CGAL tarball is separated in several packages:
  - CGAL is empty since CGAL-5.0
  - CGAL-devel contains header files, and several files and tools needed to
  develop CGAL applications,
  - CGAL-demos-source contains the source of examples and demos of CGAL.


Documentation
-------------
Note that the CGAL documentation cannot be packaged for Fedora due to unclear
license conditions. The complete documentation in PDF and HTML is
available at http://www.cgal.org/Manual/index.html
EOF

%build



%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
mkdir build
pushd build
%cmake -DCGAL_DO_NOT_WARN_ABOUT_CMAKE_BUILD_TYPE=ON -DCGAL_INSTALL_LIB_DIR=%{_datadir} -DCGAL_INSTALL_DOC_DIR= ..
%cmake_build
popd
%endif

%if 0%{?suse_version}
%cmake -DCGAL_DO_NOT_WARN_ABOUT_CMAKE_BUILD_TYPE=ON -DCGAL_INSTALL_LIB_DIR=%{_datadir} -DCGAL_INSTALL_DOC_DIR= ..
%cmake_build
%endif


%install


%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
pushd build
%cmake_install
popd
%endif

%if 0%{?suse_version}
%cmake_install
%endif


# Install demos and examples
mkdir -p %{buildroot}%{_datadir}/CGAL
touch -r demo %{buildroot}%{_datadir}/CGAL/
cp -a demo %{buildroot}%{_datadir}/CGAL/demo
cp -a examples %{buildroot}%{_datadir}/CGAL/examples

%check
rm -rf include/
mkdir build-example
cd build-example
cmake -L "-DCMAKE_PREFIX_PATH=%{buildroot}/usr" %{buildroot}%{_datadir}/CGAL/examples/Triangulation_2
make constrained_plus
ldd ./constrained_plus
./constrained_plus


%files devel
%license AUTHORS LICENSE LICENSE.BSL LICENSE.RFL LICENSE.LGPL LICENSE.GPL
%doc CHANGES.md README.Fedora
%{_includedir}/CGAL
%exclude %{_includedir}/CGAL/Qt
%dir %{_datadir}/CGAL
%{_datadir}/cmake/CGAL
%exclude %{_datadir}/cmake/CGAL/demo
%{_bindir}/*
%exclude %{_bindir}/cgal_make_macosx_app
%{_mandir}/man1/cgal_create_cmake_script.1.gz

%files qt5-devel
%{_includedir}/CGAL/Qt
%{_datadir}/cmake/CGAL/demo

%files demos-source
%{_datadir}/CGAL/demo
%{_datadir}/CGAL/examples
%exclude %{_datadir}/CGAL/*/*/skip_vcproj_auto_generation

%changelog
* Tue Dec 22 2020 Laurent Rineau <laurent.rineau@cgal.org> - 5.2-1
- New upstream release

* Wed Nov 18 2020 Laurent Rineau <laurent.rineau@cgal.org> - 5.2-0.1.beta1
- New upstream release

* Mon Nov 16 2020 Laurent Rineau <laurent.rineau@cgal.org> - 5.1.1-1
- New upstream release

* Tue Sep  8 2020 Laurent Rineau <laurent.rineau@cgal.org> - 5.1-1
- New upstream release

* Tue Jul 28 2020 Laurent Rineau <laurent.rineau@cgal.org> - 5.1-0.2.beta2
- Install CMake files in `/usr/share/cmake/CGAL/`.
- Add a `%%check` section.

* Tue Jul 28 2020 Laurent Rineau <laurent.rineau@cgal.org> - 5.1-0.1-beta2
- New upstream release 5.1-beta2

* Mon Jul 27 2020 Laurent Rineau <laurent.rineau@cgal.org> - 5.0.2-4
- Fix for Fedora 33

* Mon Jul 27 2020 Fedora Release Engineering <releng@fedoraproject.org> - 5.0.2-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_33_Mass_Rebuild

* Mon Mar  9 2020 Laurent Rineau <laurent.rineau@cgal.org> - 5.0.2-2
- Fix Bug 1811647:
      %%{?_isa} qualifier unnecessary / broken for BuildRequires
  https://bugzilla.redhat.com/show_bug.cgi?id=1811647

* Tue Feb 25 2020 Laurent Rineau <laurent.rineau@cgal.org> - 5.0.2-1
- New upstream release
- Remove the Source10 (replaced by a heredoc)

* Tue Jan 28 2020 Fedora Release Engineering <releng@fedoraproject.org> - 5.0.1-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_32_Mass_Rebuild

* Mon Jan 27 2020 Laurent Rineau <laurent.rineau@cgal.org> - 5.0.1-1
- New upstream release

* Fri Nov  8 2019 Laurent Rineau <laurent.rineau@cgal.org> - 5.0-1
- New upstream release

* Thu Oct 31 2019 Laurent Rineau <laurent.rineau@cgal.org> - 5.0-0.4.beta2
- New upstream beta release, 5.0-beta2
- Re-add the dependency to zlib with `Recommends:`
- Add a sub-package CGAL-qt5-devel, that requires Qt5 devel packages

* Tue Oct  1 2019 Laurent Rineau <laurent.rineau@cgal.org> - 5.0-0.3.beta1
- CGAL-devel is now noarch

* Tue Oct  1 2019 Laurent Rineau <laurent.rineau@cgal.org> - 5.0-0.2.beta1
- Remove the CGAL main package
- Add Provides: CGAL-static

* Tue Oct  1 2019 Laurent Rineau <laurent.rineau@cgal.org> - 5.0-0.1.beta1
- New upstream beta release, header-only
- Remove the dependency on Qt5 and Zlib

* Tue Oct  1 2019 Laurent Rineau <laurent.rineau@cgal.org> - 4.14.1-1
- New upstream release

* Wed Jul 24 2019 Fedora Release Engineering <releng@fedoraproject.org> - 4.14-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_31_Mass_Rebuild

* Mon Apr  1 2019 Laurent Rineau <laurent.rineau@cgal.org> - 4.14-1
- New upstream release

* Wed Mar 27 2019 Laurent Rineau <laurent.rineau@cgal.org> - 4.14-0.3beta3
- New upstream release

* Mon Mar 25 2019 Laurent Rineau <laurent.rineau@cgal.org> - 4.14-0.1beta2
- New upstream release

* Thu Jan 31 2019 Fedora Release Engineering <releng@fedoraproject.org> - 4.13-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_30_Mass_Rebuild

* Thu Jan 24 2019 Jonathan Wakely <jwakely@redhat.com> - 4.13-2
- Rebuilt for Boost 1.69

* Tue Oct  9 2018 Laurent Rineau <laurent.rineau@cgal.org> - 4.13-1
- New upstream version

- Add `CGAL_DO_NOT_WARN_ABOUT_CMAKE_BUILD_TYPE` in the CMake
  configuration, to suppress a warning.

* Wed Aug 22 2018 Laurent Rineau <laurent.rineau@cgal.org> - 4.13-0.2.beta1
- add weak dependency to eigen3-devel

* Wed Aug 22 2018 Laurent Rineau <laurent.rineau@cgal.org> - 4.13-0.1.beta1
- New upstream release

* Thu Jul 12 2018 Fedora Release Engineering <releng@fedoraproject.org> - 4.12-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_29_Mass_Rebuild

* Fri Apr 27 2018 Laurent Rineau <laurent.rineau@cgal.org> - 4.12-1
- New upstream version

* Tue Feb 27 2018 Laurent Rineau <laurent.rineau@cgal.org> - 4.12-0.2beta2
- New upstream release

* Tue Feb 27 2018 Laurent Rineau <laurent.rineau@cgal.org> - 4.11.1-2
- Restore the SPEC file changelog

* Tue Feb 27 2018 Laurent Rineau <laurent.rineau@cgal.org> - 4.11.1-1
- New upstream release

* Wed Feb 07 2018 Fedora Release Engineering <releng@fedoraproject.org> - 4.11-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_28_Mass_Rebuild

* Mon Jan 22 2018 Jonathan Wakely <jwakely@redhat.com> - 4.11-2
- Rebuilt for Boost 1.66
