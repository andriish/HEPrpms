%global sover 0.23

Name:           f2c
Summary:        A Fortran 77 to C/C++ conversion program
Version:        20210928
Release:        10%{?dist}
License:        MIT

URL:            http://www.netlib.org/f2c/
Source0:        http://www.netlib.org/f2c/src.tgz
Source1:        http://www.netlib.org/f2c/libf2c.zip
Source2:        http://www.netlib.org/f2c/f2c.pdf
Source3:        http://www.netlib.org/f2c/f2c.ps
Source4:        http://www.netlib.org/f2c/fc

# Patch makefile to build a shared library
Patch0:         f2c-20110801.patch
Patch1:         libf2c-20110801-format-security.patch

BuildRequires:  gcc
BuildRequires:  make
BuildRequires:  unzip

Requires:       %{name}-libs%{?_isa} = %{version}-%{release}
Provides:       %{name}-devel = %{version}-%{release}


%description
F2c converts Fortran 77 source code to C or C++ source files. If no
Fortran files are named on the command line, f2c can read Fortran from
standard input and write C to standard output.


%package libs
Summary:        Dynamic libraries from %{name}

%description libs
Dynamic libraries from %{name}.


%prep
%autosetup -N -c %{name}-%{version}
mkdir libf2c
unzip -qq %{SOURCE1} -d libf2c
%autopatch -p1

# Set library soversion
sed -i "s/@SOVER@/%{sover}/" libf2c/makefile.u

# Copy in other source files.
cp %{SOURCE2} %{SOURCE3} %{SOURCE4} .


%build
make -C src -f makefile.u %{?_smp_mflags} CFLAGS="%{optflags}" f2c
make -C libf2c -f makefile.u %{?_smp_mflags} CFLAGS="%{optflags} -fPIC"


%install
install -D -p -m 644 src/f2c.h  %{buildroot}%{_includedir}/f2c.h
install -D -p -m 755 src/f2c    %{buildroot}%{_bindir}/f2c
install -D -p -m 644 src/f2c.1t %{buildroot}%{_mandir}/man1/f2c.1
install -D -p -m 755 libf2c/libf2c.so.%{sover} %{buildroot}%{_libdir}/libf2c.so.%{sover}
ln -sr %{buildroot}%{_libdir}/libf2c.so.%{sover} %{buildroot}%{_libdir}/libf2c.so.0
ln -sr %{buildroot}%{_libdir}/libf2c.so.%{sover} %{buildroot}%{_libdir}/libf2c.so

# Setup f77 script
sed -i "s/@lib@/%{_lib}/" fc
install -Dpm 0755 fc %{buildroot}%{_bindir}/f77

%ldconfig_scriptlets 


%files
%doc f2c.ps f2c.pdf src/changes src/README
%license src/Notice
%{_bindir}/f2c
%{_bindir}/f77
%{_mandir}/man1/f2c.1*
%{_includedir}/f2c.h
%{_libdir}/libf2c.so

%files libs
%doc libf2c/README
%license libf2c/Notice
%{_libdir}/libf2c.so.*


%changelog
* Thu Jan 16 2025 Fedora Release Engineering <releng@fedoraproject.org> - 20210928-10
- Rebuilt for https://fedoraproject.org/wiki/Fedora_42_Mass_Rebuild

* Wed Jul 17 2024 Fedora Release Engineering <releng@fedoraproject.org> - 20210928-9
- Rebuilt for https://fedoraproject.org/wiki/Fedora_41_Mass_Rebuild

* Wed Jan 24 2024 Fedora Release Engineering <releng@fedoraproject.org> - 20210928-8
- Rebuilt for https://fedoraproject.org/wiki/Fedora_40_Mass_Rebuild

* Fri Jan 19 2024 Fedora Release Engineering <releng@fedoraproject.org> - 20210928-7
- Rebuilt for https://fedoraproject.org/wiki/Fedora_40_Mass_Rebuild

* Wed Jul 19 2023 Fedora Release Engineering <releng@fedoraproject.org> - 20210928-6
- Rebuilt for https://fedoraproject.org/wiki/Fedora_39_Mass_Rebuild

* Fri May 19 2023 Richard Shaw <hobbes1069@gmail.com> - 20210928-5
- Current license is already compliant for SPDX.
- Modernize %%prep using %%autosetup / %%autopatch.

* Thu Jan 19 2023 Fedora Release Engineering <releng@fedoraproject.org> - 20210928-4
- Rebuilt for https://fedoraproject.org/wiki/Fedora_38_Mass_Rebuild

* Thu Jul 21 2022 Fedora Release Engineering <releng@fedoraproject.org> - 20210928-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_37_Mass_Rebuild

* Thu Jan 20 2022 Fedora Release Engineering <releng@fedoraproject.org> - 20210928-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_36_Mass_Rebuild

* Sat Jan 01 2022 Richard Shaw <hobbes1069@gmail.com> - 20210928-1
- Update to 20210928.
- Add fc script as /usr/bin/f77, fixes RHBZ#1980463.

* Wed Jul 21 2021 Fedora Release Engineering <releng@fedoraproject.org> - 20190311-6
- Rebuilt for https://fedoraproject.org/wiki/Fedora_35_Mass_Rebuild

* Tue Jan 26 2021 Fedora Release Engineering <releng@fedoraproject.org> - 20190311-5
- Rebuilt for https://fedoraproject.org/wiki/Fedora_34_Mass_Rebuild

* Mon Jul 27 2020 Fedora Release Engineering <releng@fedoraproject.org> - 20190311-4
- Rebuilt for https://fedoraproject.org/wiki/Fedora_33_Mass_Rebuild

* Tue Jan 28 2020 Fedora Release Engineering <releng@fedoraproject.org> - 20190311-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_32_Mass_Rebuild

* Thu Jul 25 2019 Fedora Release Engineering <releng@fedoraproject.org> - 20190311-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_31_Mass_Rebuild

* Sun Mar 17 2019 Richard Shaw <hobbes1069@gmail.com> - 20190311-1
- Update to 20190311.

* Thu Jan 31 2019 Fedora Release Engineering <releng@fedoraproject.org> - 20160102-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_30_Mass_Rebuild

* Fri Jul 13 2018 Fedora Release Engineering <releng@fedoraproject.org> - 20160102-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_29_Mass_Rebuild

* Mon Feb 19 2018 Richard Shaw <hobbes1069@gmail.com> - 20160102
- Update to 20160102.
- Major overhaul on spec file.

* Wed Feb 07 2018 Fedora Release Engineering <releng@fedoraproject.org> - 20110801-13
- Rebuilt for https://fedoraproject.org/wiki/Fedora_28_Mass_Rebuild

* Wed Aug 02 2017 Fedora Release Engineering <releng@fedoraproject.org> - 20110801-12
- Rebuilt for https://fedoraproject.org/wiki/Fedora_27_Binutils_Mass_Rebuild

* Wed Jul 26 2017 Fedora Release Engineering <releng@fedoraproject.org> - 20110801-11
- Rebuilt for https://fedoraproject.org/wiki/Fedora_27_Mass_Rebuild

* Fri Feb 10 2017 Fedora Release Engineering <releng@fedoraproject.org> - 20110801-10
- Rebuilt for https://fedoraproject.org/wiki/Fedora_26_Mass_Rebuild

* Wed Feb 03 2016 Fedora Release Engineering <releng@fedoraproject.org> - 20110801-9
- Rebuilt for https://fedoraproject.org/wiki/Fedora_24_Mass_Rebuild

* Wed Jun 17 2015 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 20110801-8
- Rebuilt for https://fedoraproject.org/wiki/Fedora_23_Mass_Rebuild

* Sat Aug 16 2014 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 20110801-7
- Rebuilt for https://fedoraproject.org/wiki/Fedora_21_22_Mass_Rebuild

* Fri Jun 13 2014 Yaakov Selkowitz <yselkowi@redhat.com> - 20110801-6
- Fix FTBFS with -Werror=format-security (#1037057, #1106245)

* Sat Jun 07 2014 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 20110801-5
- Rebuilt for https://fedoraproject.org/wiki/Fedora_21_Mass_Rebuild

* Sat Aug 03 2013 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 20110801-4
- Rebuilt for https://fedoraproject.org/wiki/Fedora_20_Mass_Rebuild

* Wed Feb 13 2013 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 20110801-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_19_Mass_Rebuild

* Thu Jul 19 2012 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 20110801-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_18_Mass_Rebuild

* Sun Apr 08 2012 Carl Byington <carl@five-ten-sg.com> 20110801-1
- update to newer upstream version
- patch from Jaroslav Škarvada for 4 byte ints on x86_64

* Fri Jan 13 2012 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 20090411-8
- Rebuilt for https://fedoraproject.org/wiki/Fedora_17_Mass_Rebuild

* Tue Feb 08 2011 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 20090411-7
- Rebuilt for https://fedoraproject.org/wiki/Fedora_15_Mass_Rebuild

* Wed Jul 07 2010 Carl Byington <carl@five-ten-sg.com> 20090411-6
- Subpackage Licensing, move Notice to -libs.

* Sun Dec 05 2009 Carl Byington <carl@five-ten-sg.com> 20090411-5
- fully versioned provides

* Sat Dec 05 2009 Carl Byington <carl@five-ten-sg.com> 20090411-4
- remove -devel subpackage, merge it into the main package which
  provides -devel and requires -libs.

* Sat Dec 05 2009 Carl Byington <carl@five-ten-sg.com> 20090411-3
- remove patch backups
- add comment for patch purpose

* Thu Dec 03 2009 Carl Byington <carl@five-ten-sg.com> 20090411-2
- add symlink to fix rpmlint error
- remove unnecessary parts of the patch, which enables building a
  shared library.
- main package now requires -devel since that is needed to be useful.
- summary changed to specify this only works on F77 code.
- %%files use explicit libf2c rather than * wildcard

* Wed Dec 02 2009 Carl Byington <carl@five-ten-sg.com> 20090411-1
- update to newer upstream version
- add .pdf documentation also
- trim changelog
- move all the license related files into -libs, and both the
  main package and -devel require -libs, to avoid either duplicating
  files or installing any package without the license files.

* Sun Nov 25 2009 Carl Byington <carl@five-ten-sg.com> 20031026-3.0.3
- don't install the static library.
- preserve the alpha architecture patch and ifdef in the spec file
  even if it is not used by fedora.
- split off -libs and -devel packages.
- full version/release in requires

* Wed Nov 25 2009 Carl Byington <carl@five-ten-sg.com> 20031026-3.0.2
- convert to fedora compatible spec file.

* Sat Jun 14 2008 Axel Thimm <Axel.Thimm@ATrpms.net> - 20031026-3.0.1
- Fix not utf-8 specfile entries.

