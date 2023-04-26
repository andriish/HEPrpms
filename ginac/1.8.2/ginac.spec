%global so_ver 11

Name:             ginac
Version:          1.8.2
Release:          1%{?dist}
Summary:          C++ library for symbolic calculations.
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
%if %{?fedora}%{!?fedora:0}
%doc %{_vpath_builddir}/doc/tutorial/ginac.html
%endif
%{_includedir}/ginac/
%if  0%{?rhel} || 0%{?fedora}
%{_infodir}/*.info*
%endif
%{_libdir}/*.so
%{_libdir}/cmake
%{_libdir}/pkgconfig/ginac.pc

%files utils
%{_bindir}/*

%changelog
* Sun Oct 25 2020 Till Hofmann <thofmann@fedoraproject.org> - 1.7.9-5
- Adapt to cmake out-of-source build

* Wed Sep 23 2020 Jeff Law <law@redhat.com> - 1.7.9-4
- Use cmake_in_source_build to fix FTBFS due to recent cmake macro changes

* Sat Aug 01 2020 Fedora Release Engineering <releng@fedoraproject.org> - 1.7.9-3
- Second attempt - Rebuilt for
  https://fedoraproject.org/wiki/Fedora_33_Mass_Rebuild

* Mon Jul 27 2020 Fedora Release Engineering <releng@fedoraproject.org> - 1.7.9-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_33_Mass_Rebuild

* Sun Apr 12 2020 Till Hofmann <thofmann@fedoraproject.org> - 1.7.9-1
- Update to 1.7.9

* Tue Jan 28 2020 Fedora Release Engineering <releng@fedoraproject.org> - 1.7.8-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_32_Mass_Rebuild

* Wed Oct 09 2019 Till Hofmann <thofmann@fedoraproject.org> - 1.7.8-1
- Update to 1.7.8
- Build with python3

* Sun Oct 06 2019 Till Hofmann <thofmann@fedoraproject.org> - 1.7.7-1
- Update to 1.7.7

* Thu Jul 25 2019 Fedora Release Engineering <releng@fedoraproject.org> - 1.7.6-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_31_Mass_Rebuild

* Tue Apr 23 2019 Till Hofmann <thofmann@fedoraproject.org> - 1.7.6-1
- Update to 1.7.6

* Thu Mar  7 2019 Tim Landscheidt <tim@tim-landscheidt.de> - 1.7.5-3
- Remove obsolete requirements for %%post/%%preun scriptlets

* Mon Feb 18 2019 Igor Gnatenko <ignatenkobrain@fedoraproject.org> - 1.7.5-2
- Rebuild for readline 8.0

* Sun Feb 17 2019 Till Hofmann <thofmann@fedoraproject.org> - 1.7.5-1
- Update to 1.7.5

* Sun Feb 17 2019 Igor Gnatenko <ignatenkobrain@fedoraproject.org> - 1.7.4-7
- Rebuild for readline 8.0

* Thu Jan 31 2019 Fedora Release Engineering <releng@fedoraproject.org> - 1.7.4-6
- Rebuilt for https://fedoraproject.org/wiki/Fedora_30_Mass_Rebuild

* Wed Jan 23 2019 Björn Esser <besser82@fedoraproject.org> - 1.7.4-5
- Append curdir to CMake invokation. (#1668512)

* Sat Jul 14 2018 Till Hofmann <thofmann@fedoraproject.org> - 1.7.4-4
- Add patch to avoid using unversioned python in cmake
- Replace unversioned python shebangs in all installed python files

* Fri Jul 13 2018 Fedora Release Engineering <releng@fedoraproject.org> - 1.7.4-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_29_Mass_Rebuild

* Wed Jun 06 2018 Till Hofmann <thofmann@fedoraproject.org> - 1.7.4-2
- Replace tetex BRs by virtual packages (e.g., tex(latex))

* Wed Feb 21 2018 Till Hofmann <thofmann@fedoraproject.org> - 1.7.4-1
- Update to 1.7.4

* Wed Feb 07 2018 Fedora Release Engineering <releng@fedoraproject.org> - 1.7.2-7
- Rebuilt for https://fedoraproject.org/wiki/Fedora_28_Mass_Rebuild

* Wed Aug 02 2017 Fedora Release Engineering <releng@fedoraproject.org> - 1.7.2-6
- Rebuilt for https://fedoraproject.org/wiki/Fedora_27_Binutils_Mass_Rebuild

* Wed Jul 26 2017 Fedora Release Engineering <releng@fedoraproject.org> - 1.7.2-5
- Rebuilt for https://fedoraproject.org/wiki/Fedora_27_Mass_Rebuild

* Mon May 15 2017 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.7.2-4
- Rebuilt for https://fedoraproject.org/wiki/Fedora_26_27_Mass_Rebuild

* Fri Feb 10 2017 Fedora Release Engineering <releng@fedoraproject.org> - 1.7.2-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_26_Mass_Rebuild

* Thu Jan 12 2017 Igor Gnatenko <ignatenko@redhat.com> - 1.7.2-2
- Rebuild for readline 7.x

* Wed Jan 11 2017 Zbigniew Jędrzejewski-Szmek <zbyszek@in.waw.pl> - 1.7.2-1
- Update to latest version (#1411745)

* Sun Oct 09 2016 Till Hofmann <till.hofmann@posteo.de> - 1.7.1-2
- Add patch to replace mktemp with mkstemp

* Mon Oct 03 2016 Till Hofmann <till.hofmann@posteo.de> - 1.7.1-1
- Update to 1.7.1

* Wed May 11 2016 Zbigniew Jędrzejewski-Szmek <zbyszek@bupkis> - 1.7.0-1
- Update to latest version

* Wed May 11 2016 Zbigniew Jędrzejewski-Szmek <zbyszek@in.waw.pl> - 1.6.7-1
- Update to 1.6.7

* Wed Feb 03 2016 Fedora Release Engineering <releng@fedoraproject.org> - 1.6.5-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_24_Mass_Rebuild

* Sun Aug 09 2015 Christopher Meng <rpm@cicku.me> - 1.6.5-1
- Update to 1.6.5

* Wed Jun 17 2015 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.6.3-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_23_Mass_Rebuild

* Sat May 02 2015 Kalev Lember <kalevlember@gmail.com> - 1.6.3-2
- Rebuilt for GCC 5 C++11 ABI change

* Mon Jan 26 2015 Christopher Meng <rpm@cicku.me> - 1.6.3-1
- Update to 1.6.3

* Sat Aug 16 2014 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.6.2-10.20140630gitedfa67d
- Rebuilt for https://fedoraproject.org/wiki/Fedora_21_22_Mass_Rebuild

* Wed Jul 02 2014 Christopher Meng <rpm@cicku.me> - 1.6.2-9.20140630gitedfa67d
- Update to latest git snapshot.

* Sat Jun 07 2014 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.6.2-8.20131231git9843321
- Rebuilt for https://fedoraproject.org/wiki/Fedora_21_Mass_Rebuild

* Fri Jan 10 2014 Christopher Meng <rpm@cicku.me> - 1.6.2-7.20131231git9843321
- info and man pages are no longer available.

* Tue Dec 31 2013 Christopher Meng <rpm@cicku.me> - 1.6.2-6.20131231git9843321
- Update to latest git snapshot.
- SPEC cleanup, drop patch merged upstream.

* Sat Aug 03 2013 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.6.2-5
- Rebuilt for https://fedoraproject.org/wiki/Fedora_20_Mass_Rebuild

* Wed Feb 13 2013 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.6.2-4
- Rebuilt for https://fedoraproject.org/wiki/Fedora_19_Mass_Rebuild

* Thu Jul 19 2012 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.6.2-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_18_Mass_Rebuild

* Tue Feb 28 2012 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.6.2-2
- Rebuilt for c++ ABI breakage

* Tue Jan 24 2012 Orion Poplawski <orion@cora.nwra.com> - 1.6.2-1
- Update to 1.6.2 (resolves bug #781720)

* Fri Jan 13 2012 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.5.8-4
- Rebuilt for https://fedoraproject.org/wiki/Fedora_17_Mass_Rebuild

* Sat Mar 05 2011 Christoph Wickert <cwickert@fedoraproject.org> - 1.5.8-3
- Fix several spec file problems (#560197)

* Tue Feb 08 2011 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.5.8-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_15_Mass_Rebuild

* Tue Sep 07 2010 Rakesh Pandit <rakesh@fedoraproject.org> - 1.5.6-1
- Updated to 1.5.8

* Sat Jan 30 2010 Rakesh Pandit <rakesh@fedoraproject.org> - 1.5.6-1
- Updated to 1.5.6

* Fri Dec 04 2009 Rakesh Pandit <rakesh@fedoraproject.org> - 1.5.5-1
- Updated to 1.5.5

* Fri Jul 24 2009 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.5.1-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_12_Mass_Rebuild

* Thu Jul  9 2009 Alex Lancaster <alexlan[AT]fedoraproject org> - 1.5.1-2
- Rebuild to fix broken deps

* Tue Mar 17 2009 Rakesh Pandit <rakesh@fedoraproject.org> - 1.5.1-1
- Patched up lexer.cpp for missing header
- Removed rpaths in pkgconfig file #487612
- Updated to 1.5.1:
-   Added polynomial factorization.
-   New, faster (recursive descent) expression parser.
-   Faster GCD computation.
-   Replaced custom RTTI by standard C++ RTTI.
-   Fixed recursion in polynomial divide that caused a significant slowdown in sqrfree().
-   Improved lsolve() of systems containing non-numeric coefficients.
-   Improved configuration and compatibility.

* Tue Feb 24 2009 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.4.4-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_11_Mass_Rebuild

* Fri Jan 16 2009 Rakesh Pandit <rakesh@fedoraproject.org> 1.4.4-1
- Updated to 1.4.4

* Tue Apr 29 2008 Quentin Spencer <qspencer@users.sf.net> 1.4.3-1
- Update to 1.4.3. Remove old patch.

* Sun Mar  2 2008 Alex Lancaster <alexlan[AT]fedoraproject org> - 1.4.1-4
- Patch for building with GCC 4.3 (this has been applied upstream and so
  can be dropped in the next release of ginac).

* Wed Feb 27 2008 Quentin Spencer <qspencer@users.sf.net> 1.4.1-3
- Rebuild for new release of cln.

* Tue Feb 19 2008 Fedora Release Engineering <rel-eng@fedoraproject.org> - 1.4.1-2
- Autorebuild for GCC 4.3

* Thu Jan  3 2008 Quentin Spencer <qspencer@users.sf.net> 1.4.1-1
- Update to 1.4.1.

* Thu Sep 13 2007 Quentin Spencer <qspencer@users.sf.net> 1.4.0-2
- Add pkgconfig as a dependency of -devel.

* Wed Sep 12 2007 Quentin Spencer <qspencer@users.sf.net> 1.4.0-1
- New release. Changes file lists to reflect the removal of some files
  previously in the devel package.

* Tue Aug 21 2007 Quentin Spencer <qspencer@users.sf.net> 1.3.7-1
- New release.

* Wed Jan 10 2007 Quentin Spencer <qspencer@users.sf.net> 1.3.6-1
- New release.

* Mon Aug 28 2006 Quentin Spencer <qspencer@users.sf.net> 1.3.5-1
- New release.

* Fri Apr 14 2006 Quentin Spencer <qspencer@users.sf.net> 1.3.4-1
- New release. Old patch removed.

* Mon Feb 13 2006 Quentin Spencer <qspencer@users.sf.net> 1.3.3-4
- Rebuild for Fedora Extras 5.

* Thu Feb  2 2006 Quentin Spencer <qspencer@users.sf.net> 1.3.3-3
- Patch so it builds on gcc 4.1.
- Disable static libs from build and enable parallel build.

* Wed Feb  1 2006 Quentin Spencer <qspencer@users.sf.net> 1.3.3-2
- Exclude /usr/share/info/dir from package.
- New URL.
- Exclude static libs.

* Mon Oct 31 2005 Quentin Spencer <qspencer@users.sf.net> 1.3.3-1
- New upstream release.

* Tue Aug  2 2005 Quentin Spencer <qspencer@users.sf.net> 1.3.2-1
- New upstream release. Changed package name to lowercase letters to
  mirror upstream sources.  Added Provides and Obsoletes for upgrade.

* Sat Jun 11 2005 Quentin Spencer <qspencer@users.sf.net> 1.3.1-5
- Added cln-devel as dependency of GiNaC-devel

* Fri May 27 2005 Quentin Spencer <qspencer@users.sf.net> 1.3.1-5
- Removed gmp-devel--it should be in cln-devel instead

* Fri May 27 2005 Quentin Spencer <qspencer@users.sf.net> 1.3.1-4
- Added gmp-devel to BuildRequires

* Thu May 26 2005 Quentin Spencer <qspencer@users.sf.net> 1.3.1-3
- Added transfig to BuildRequires

* Thu May 26 2005 Quentin Spencer <qspencer@users.sf.net> 1.3.1-2
- Added dist tag

* Wed May 18 2005 Quentin Spencer <qspencer@users.sf.net> 1.3.1-1
- New upstream release.
- Added missing BuildRequires (readline-devel, tetex-*, doxygen).

* Wed May 11 2005 Quentin Spencer <qspencer@users.sf.net> 1.3.0-2
- Exclude .la lib.
- Remove processing of info files (this is supposed to be automatic).

* Fri Apr 22 2005 Quentin Spencer <qspencer@users.sf.net> 1.3.0-2
- Added release to Requires for devel and utils

* Thu Apr 21 2005 Quentin Spencer <qspencer@users.sf.net> 1.3.0-1
- Adapted spec file for Fedora Extras
- Fixed missing BuildRequires
- Fixed broken install-info command

* Thu Nov 20 2003 Christian Bauer <Christian.Bauer@uni-mainz.de>
- added pkg-config metadata file to devel package

* Thu Nov  1 2001 Christian Bauer <Christian.Bauer@uni-mainz.de>
- moved ginsh and viewgar to "utils" package

* Thu Oct  5 2000 Christian Bauer <Christian.Bauer@uni-mainz.de>
- cleaned up a bit

* Wed Jan 26 2000 Christian Bauer <Christian.Bauer@uni-mainz.de>
- split into user and devel packages

* Wed Dec  1 1999 Christian Bauer <Christian.Bauer@uni-mainz.de>
- aclocal macros get installed
