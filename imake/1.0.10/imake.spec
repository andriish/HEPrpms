Summary: imake source code configuration and build system
Name: imake
Version: 1.0.10
Release: 4%{?dist}
License: MIT-open-group AND HPND
URL: http://www.x.org

Source0: https://www.x.org/pub/individual/util/imake-%{version}.tar.xz
Source1: https://www.x.org/pub/individual/util/makedepend-1.0.8.tar.xz
Source2: https://www.x.org/pub/individual/util/gccmakedep-1.0.3.tar.bz2
Source3: https://www.x.org/pub/individual/util/xorg-cf-files-1.0.8.tar.xz
Source4: https://www.x.org/pub/individual/util/lndir-1.0.4.tar.xz
Patch11: imake-1.0.2-abort.patch
Patch12: xorg-cf-files-1.0.8-DEFAULT_SOURCE.patch

BuildRequires: make
BuildRequires: pkgconfig
BuildRequires: xorg-x11-util-macros
BuildRequires: xorg-x11-proto-devel
BuildRequires: gcc
BuildRequires: gcc-c++
# imake is not functional without cc
Requires:      gcc

Provides: ccmakedep cleanlinks gccmakedep lndir makedepend makeg
Provides: mergelib mkdirhier mkhtmlindex revpath xmkmf

%description
Imake is a deprecated source code configuration and build system which
has traditionally been supplied by and used to build the X Window System
in X11R6 and previous releases.  As of the X Window System X11R7 release,
the X Window system has switched to using GNU autotools as the primary
build system, and the Imake system is now deprecated, and should not be
used by new software projects.  Software developers are encouraged to
migrate software to the GNU autotools system.

%prep
%setup -q -c %{name}-%{version} -a1 -a2 -a3 -a4

# imake patches
pushd %{name}-%{version}
%patch -P 11 -p1 -b .abort
popd
pushd xorg-cf-files-1.0.8
%patch -P 12 -p1 -b .defaultsource
popd

%build
# Build everything
{
   for pkg in imake makedepend gccmakedep lndir xorg-cf-files ; do
      pushd $pkg-*
      case $pkg in
         imake|xorg-cf-files)
            %configure --with-config-dir=%{_datadir}/X11/config
            ;;
         *)
            %configure
            ;;
      esac
      make
      popd
   done
}

%install
# Install everything
{
   for pkg in imake makedepend gccmakedep lndir xorg-cf-files ; do
      pushd $pkg-*
      make install DESTDIR=$RPM_BUILD_ROOT
      popd
   done
}

%files
%{_bindir}/ccmakedep
%{_bindir}/cleanlinks
%{_bindir}/gccmakedep
%{_bindir}/imake
%{_bindir}/lndir
%{_bindir}/makedepend
%{_bindir}/makeg
%{_bindir}/mergelib
%{_bindir}/mkdirhier
%{_bindir}/mkhtmlindex
%{_bindir}/revpath
%{_bindir}/xmkmf
%dir %{_datadir}/X11/config
%{_datadir}/X11/config/*.cf
%{_datadir}/X11/config/*.def
%{_datadir}/X11/config/*.rules
%{_datadir}/X11/config/*.tmpl
#%%dir %%{_mandir}/man1x
%{_mandir}/man1/ccmakedep.1*
%{_mandir}/man1/cleanlinks.1*
%{_mandir}/man1/gccmakedep.1*
%{_mandir}/man1/imake.1*
%{_mandir}/man1/lndir.1*
%{_mandir}/man1/makedepend.1*
%{_mandir}/man1/makeg.1*
%{_mandir}/man1/mergelib.1*
%{_mandir}/man1/mkdirhier.1*
%{_mandir}/man1/mkhtmlindex.1*
%{_mandir}/man1/revpath.1*
%{_mandir}/man1/xmkmf.1*

%changelog
* Fri Jan 17 2025 Fedora Release Engineering <releng@fedoraproject.org> - 1.0.10-4
- Rebuilt for https://fedoraproject.org/wiki/Fedora_42_Mass_Rebuild

* Thu Jul 18 2024 Fedora Release Engineering <releng@fedoraproject.org> - 1.0.10-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_41_Mass_Rebuild

* Sun Apr 21 2024 Charles R. Anderson <cra@alum.wpi.edu> - 1.0.10-2
- _BSD_SOURCE and _SVID_SOURCE are deprecated, use _DEFAULT_SOURCE
- Convert License tag to SPDX format and add missing license

* Tue Jan 30 2024 Orion Poplawski <orion@nwra.com> - 1.0.10-1
- Update to 1.0.10

* Wed Jan 24 2024 Fedora Release Engineering <releng@fedoraproject.org> - 1.0.9-6
- Rebuilt for https://fedoraproject.org/wiki/Fedora_40_Mass_Rebuild

* Sat Jan 20 2024 Fedora Release Engineering <releng@fedoraproject.org> - 1.0.9-5
- Rebuilt for https://fedoraproject.org/wiki/Fedora_40_Mass_Rebuild

* Thu Jul 20 2023 Fedora Release Engineering <releng@fedoraproject.org> - 1.0.9-4
- Rebuilt for https://fedoraproject.org/wiki/Fedora_39_Mass_Rebuild

* Thu Jan 19 2023 Fedora Release Engineering <releng@fedoraproject.org> - 1.0.9-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_38_Mass_Rebuild

* Thu Oct 20 2022 Orion Poplawski <orion@nwra.com> - 1.0.9-2
- Rebuild for koji issue

* Thu Oct 20 2022 Orion Poplawski <orion@nwra.com> - 1.0.9-1
- Update to 1.0.9

* Thu Jul 21 2022 Fedora Release Engineering <releng@fedoraproject.org> - 1.0.8-9
- Rebuilt for https://fedoraproject.org/wiki/Fedora_37_Mass_Rebuild

* Thu Jan 20 2022 Fedora Release Engineering <releng@fedoraproject.org> - 1.0.8-8
- Rebuilt for https://fedoraproject.org/wiki/Fedora_36_Mass_Rebuild

* Thu Jul 22 2021 Fedora Release Engineering <releng@fedoraproject.org> - 1.0.8-7
- Rebuilt for https://fedoraproject.org/wiki/Fedora_35_Mass_Rebuild

* Mon Mar 29 2021 Petr Pisar <ppisar@redhat.com> - 1.0.8-6
- Adapt ar invocation to binutils 2.36 (bug #1943274)

* Tue Jan 26 2021 Fedora Release Engineering <releng@fedoraproject.org> - 1.0.8-5
- Rebuilt for https://fedoraproject.org/wiki/Fedora_34_Mass_Rebuild

* Tue Jul 28 2020 Fedora Release Engineering <releng@fedoraproject.org> - 1.0.8-4
- Rebuilt for https://fedoraproject.org/wiki/Fedora_33_Mass_Rebuild

* Wed Jan 29 2020 Fedora Release Engineering <releng@fedoraproject.org> - 1.0.8-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_32_Mass_Rebuild

* Thu Jul 25 2019 Fedora Release Engineering <releng@fedoraproject.org> - 1.0.8-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_31_Mass_Rebuild

* Fri Feb 01 2019 Fedora Release Engineering <releng@fedoraproject.org> - 1.0.7-16
- Rebuilt for https://fedoraproject.org/wiki/Fedora_30_Mass_Rebuild

* Tue Jul 31 2018 Florian Weimer <fweimer@redhat.com> - 1.0.7-15
- Rebuild with fixed binutils

* Sat Jul 28 2018 Igor Gnatenko <ignatenkobrain@fedoraproject.org> - 1.0.7-14
- Requires: gcc

* Tue Jul 24 2018 Tom Callaway <spot@fedoraproject.org> - 1.0.7-13
- add BuildRequires: gcc and gcc-c++ to ensure proper build

* Fri Jul 13 2018 Fedora Release Engineering <releng@fedoraproject.org> - 1.0.7-12
- Rebuilt for https://fedoraproject.org/wiki/Fedora_29_Mass_Rebuild

* Thu Jul 05 2018 Adam Jackson <ajax@redhat.com> - 1.0.7-11
- xorg-cf-files 1.0.6
- Drop pointless %%defattr
- HTTPS URLs

* Wed Feb 07 2018 Fedora Release Engineering <releng@fedoraproject.org> - 1.0.7-10
- Rebuilt for https://fedoraproject.org/wiki/Fedora_28_Mass_Rebuild

* Wed Aug 02 2017 Fedora Release Engineering <releng@fedoraproject.org> - 1.0.7-9
- Rebuilt for https://fedoraproject.org/wiki/Fedora_27_Binutils_Mass_Rebuild

* Wed Jul 26 2017 Fedora Release Engineering <releng@fedoraproject.org> - 1.0.7-8
- Rebuilt for https://fedoraproject.org/wiki/Fedora_27_Mass_Rebuild

* Fri Feb 10 2017 Fedora Release Engineering <releng@fedoraproject.org> - 1.0.7-7
- Rebuilt for https://fedoraproject.org/wiki/Fedora_26_Mass_Rebuild

* Thu Feb 04 2016 Fedora Release Engineering <releng@fedoraproject.org> - 1.0.7-6
- Rebuilt for https://fedoraproject.org/wiki/Fedora_24_Mass_Rebuild

* Wed Jun 17 2015 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.0.7-5
- Rebuilt for https://fedoraproject.org/wiki/Fedora_23_Mass_Rebuild

* Wed Oct 01 2014 Adam Jackson <ajax@redhat.com> 1.0.7-4
- imake 1.0.7
- gccmakedep 1.0.3

* Sat Aug 16 2014 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.0.6-4
- Rebuilt for https://fedoraproject.org/wiki/Fedora_21_22_Mass_Rebuild

* Sat Jun 07 2014 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.0.6-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_21_Mass_Rebuild

* Mon Mar 17 2014 Marcin Juszkiewicz <mjuszkiewicz@redhat.com> - 1.0.6-2
- Backport AArch64 support

* Mon Jan 20 2014 Adam Jackson <ajax@redhat.com> 1.0.6-1
- imake 1.0.6

* Mon Dec 09 2013 Adam Jackson <ajax@redhat.com> 1.0.5-8
- Fix imake build with -Werror=format-security (#1037129)

* Sat Aug 03 2013 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.0.5-8
- Rebuilt for https://fedoraproject.org/wiki/Fedora_20_Mass_Rebuild

* Thu Feb 07 2013 Jon Ciesla <limburgher@gmail.com> 1.0.5-7
- Merge review fixes, BZ 225898.

* Thu Jan 03 2013 Adam Jackson <ajax@redhat.com> 1.0.5-6
- Drop unused patches

* Thu Jul 19 2012 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.0.5-5
- Rebuilt for https://fedoraproject.org/wiki/Fedora_18_Mass_Rebuild

* Thu Mar 08 2012 Adam Jackson <ajax@redhat.com> 1.0.5-4
- imake 1.0.5
- lndir 1.0.3
- makedepend 1.0.4
