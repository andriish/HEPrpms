%bcond_with static_libs # don't build static libraries

Summary:        High quality system independent, open source libm
Name:           openlibm
Version:        0.5.3
Release:        12%{?dist}
License:        BSD and MIT and ISC and Public Domain
Source0:        https://github.com/JuliaLang/openlibm/archive/v%{version}.tar.gz
URL:            https://github.com/JuliaLang/openlibm/
ExclusiveArch:  %{arm} %{ix86} x86_64 aarch64 %{power64}

BuildRequires: make
BuildRequires:  gcc
%description
OpenLIBM is an effort to have a high quality standalone LIBM library.
It is meant to be used standalone in applications and programming language
implementations. The OpenLIBM code derives from the FreeBSD msun implementation,
which in turn derives from FDLIBM 5.3. As a result, it has a number of fixes
and updates that have accumulated over the years in msun, and also optimized
assembly versions of many functions.

%package devel
Summary:    High quality system independent, open source libm
Requires:   %{name}%{?_isa} = %{version}-%{release}

%description devel
Contains header files for developing applications that use the %{name}
library.

%package static
Summary:    High quality system independent, open source libm
Requires:   %{name}-devel%{?_isa} = %{version}-%{release}

%description static
Static %{name} library.

%prep
%setup -q

# File under the Apple Public Source License Version 1.1
rm -f test/ieeetestnew.c
# File under the Apple Public Source License Version 2.0
rm -f i387/osx_asm.h

%build
make %{?_smp_mflags} \
      FFLAGS="%{optflags}" \
      CFLAGS="%{optflags}"

%check
# Tests still fail on ARM because of floating-point exceptions
# which are not set correctly (but other tests are fine)
%ifnarch %{arm}
make test
%endif

%install
make install prefix=%{_prefix} \
             libdir=%{_libdir} \
             includedir=%{_includedir} \
             DESTDIR=%{buildroot}

%if ! %{with static_libs}
rm %{buildroot}/%{_libdir}/libopenlibm.a
%endif

%ldconfig_scriptlets

%files
%doc LICENSE.md README.md
%{_libdir}/libopenlibm.so.2*

%files devel
%{_libdir}/libopenlibm.so
%{_libdir}/pkgconfig/openlibm.pc
%{_includedir}/openlibm/

%if %{with static_libs}
%files static
%{_libdir}/libopenlibm.a
%endif

%changelog
* Tue Jan 26 2021 Fedora Release Engineering <releng@fedoraproject.org> - 0.5.3-12
- Rebuilt for https://fedoraproject.org/wiki/Fedora_34_Mass_Rebuild

* Tue Jul 28 2020 Fedora Release Engineering <releng@fedoraproject.org> - 0.5.3-11
- Rebuilt for https://fedoraproject.org/wiki/Fedora_33_Mass_Rebuild

* Wed Jan 29 2020 Fedora Release Engineering <releng@fedoraproject.org> - 0.5.3-10
- Rebuilt for https://fedoraproject.org/wiki/Fedora_32_Mass_Rebuild

* Thu Jul 25 2019 Fedora Release Engineering <releng@fedoraproject.org> - 0.5.3-9
- Rebuilt for https://fedoraproject.org/wiki/Fedora_31_Mass_Rebuild

* Fri Feb 01 2019 Fedora Release Engineering <releng@fedoraproject.org> - 0.5.3-8
- Rebuilt for https://fedoraproject.org/wiki/Fedora_30_Mass_Rebuild

* Fri Jul 13 2018 Fedora Release Engineering <releng@fedoraproject.org> - 0.5.3-7
- Rebuilt for https://fedoraproject.org/wiki/Fedora_29_Mass_Rebuild

* Thu Feb 08 2018 Fedora Release Engineering <releng@fedoraproject.org> - 0.5.3-6
- Rebuilt for https://fedoraproject.org/wiki/Fedora_28_Mass_Rebuild

* Thu Aug 03 2017 Fedora Release Engineering <releng@fedoraproject.org> - 0.5.3-5
- Rebuilt for https://fedoraproject.org/wiki/Fedora_27_Binutils_Mass_Rebuild

* Thu Jul 27 2017 Fedora Release Engineering <releng@fedoraproject.org> - 0.5.3-4
- Rebuilt for https://fedoraproject.org/wiki/Fedora_27_Mass_Rebuild

* Sat Feb 11 2017 Fedora Release Engineering <releng@fedoraproject.org> - 0.5.3-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_26_Mass_Rebuild

* Tue Aug 23 2016 Dan Horák <dan[at]danny.cz> 0.5.3-2
- set ExclusiveArch to arches where openlibm has been ported

* Sat Aug 13 2016 Peter Robinson <pbrobinson@fedoraproject.org> 0.5.3-1
- Update to 0.5.3 (aarch64 support)

* Tue Mar 8 2016 Milan Bouchet-Valat <nalimilan@club.fr> - 0.5.0-1
- New upstream release (bumping SONAME).
- Remove ExclusiveArch since PPC is now supported (and passes the tests).
- Enable tests on i686.
- Remove custom code for computing ARCH, which is now correctly detected.

* Thu Feb 04 2016 Fedora Release Engineering <releng@fedoraproject.org> - 0.4.1-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_24_Mass_Rebuild

* Wed Jun 17 2015 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 0.4.1-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_23_Mass_Rebuild

* Sun Feb 1 2015 Milan Bouchet-Valat <nalimilan@club.fr> - 0.4.1-1
- New upstream release.
- Run tests on architectures where they are supposed to work thanks to new fixes.

* Sat Oct 25 2014 Peter Robinson <pbrobinson@fedoraproject.org> 0.4-3
- Fix build on armv7hl

* Sun Aug 17 2014 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 0.4-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_21_22_Mass_Rebuild

* Sun Jul 27 2014 Milan Bouchet-Valat <nalimilan@club.fr> - 0.4-1
- New upstream release.
- Fix path of two files to remove.

* Sat Jun 07 2014 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 0.3-7
- Rebuilt for https://fedoraproject.org/wiki/Fedora_21_Mass_Rebuild

* Sun May 4 2014 Milan Bouchet-Valat <nalimilan@club.fr> - 0.3-6
- Disable tests for now.

* Thu May 1 2014 Milan Bouchet-Valat <nalimilan@club.fr> - 0.3-5
- Add tests.

* Tue Apr 29 2014 Milan Bouchet-Valat <nalimilan@club.fr> - 0.3-4
- Add ISC to list of licenses.
- Add bug reference about the failing tests.

* Mon Apr 21 2014 Milan Bouchet-Valat <nalimilan@club.fr> - 0.3-3
- Use Group System Environment/Libraries for base package.

* Sun Apr 20 2014 Milan Bouchet-Valat <nalimilan@club.fr> - 0.3-2
- Put libopenlibm.so.0 in base package instead of in -devel.

* Sat Apr 19 2014 Milan Bouchet-Valat <nalimilan@club.fr> - 0.3-1
- Initial version.
