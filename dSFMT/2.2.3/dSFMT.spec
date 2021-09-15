Name:           dSFMT
Version:        2.2.3
Release:        16%{?dist}
Summary:        Double precision SIMD-oriented Fast Mersenne Twister

License:        BSD
URL:            http://www.math.sci.hiroshima-u.ac.jp/m-mat/MT/SFMT/index.html
Source0:        http://www.math.sci.hiroshima-u.ac.jp/m-mat/MT/SFMT/%{name}-src-%{version}.tar.gz

Patch0:         %{name}-%{version}_sharedlib.patch
Patch1:         %{name}-%{version}_pkgconfig.patch
Patch2:         %{name}-%{version}_exportfuns.patch

BuildRequires:  gcc
BuildRequires: make
%description
The purpose of dSFMT is to speed up the generation by avoiding
the expensive conversion of integer to double (floating point).
dSFMT directly generates double precision floating point
pseudo-random numbers which have the IEEE Standard for Binary
Floating-Point Arithmetic (ANSI/IEEE Std 754-1985) format.

dSFMT is only available on the CPUs which use IEEE 754 format
double precision floating point numbers.

dSFMT doesn't support integer outputs.
dSFMT supports the output of double precision floating point
pseudo-random numbers which distribute in the range of
[1, 2), [0, 1), (0, 1] and (0, 1).
And it also supports the various periods form 2607-1 to 2132049-1.

%package        devel
Summary:        Development files for %{name}
Requires:       %{name}%{?_isa} = %{version}-%{release}

%description    devel
The %{name}-devel package contains libraries and header files for
developing applications that use %{name}.

%package        devel-doc
Summary:        Development documentation files for %{name}
BuildArch:      noarch

%description    devel-doc
The %{name}-devel-doc package contains API documentation for
developing applications that use %{name}.

%prep
%setup -q -n %{name}-src-%{version}
%patch0 -p 1
%patch1 -p 0
%patch2 -p 1

%build
make %{?_smp_mflags} \
     sharedlib \
     libdir=%{_libdir} CCFLAGS="-fPIC %{optflags} -fno-strict-aliasing"

%install
%make_install \
    DESTDIR=$RPM_BUILD_ROOT libdir=%{_libdir}

install -pm644 %{name}.pc $RPM_BUILD_ROOT%{_libdir}/pkgconfig/

%check
make std-check

%ldconfig_scriptlets

%files
%doc ./CHANGE-LOG.txt ./LICENSE.txt
%doc README.txt README.jp.txt
%{_libdir}/*.so.*

%files devel
%{_libdir}/*.so
%{_libdir}/pkgconfig/%{name}.pc
%{_includedir}/*.h

%files devel-doc
%doc html/

%changelog
* Tue Jan 26 2021 Fedora Release Engineering <releng@fedoraproject.org> - 2.2.3-16
- Rebuilt for https://fedoraproject.org/wiki/Fedora_34_Mass_Rebuild

* Mon Jul 27 2020 Fedora Release Engineering <releng@fedoraproject.org> - 2.2.3-15
- Rebuilt for https://fedoraproject.org/wiki/Fedora_33_Mass_Rebuild

* Tue Jan 28 2020 Fedora Release Engineering <releng@fedoraproject.org> - 2.2.3-14
- Rebuilt for https://fedoraproject.org/wiki/Fedora_32_Mass_Rebuild

* Wed Jul 24 2019 Fedora Release Engineering <releng@fedoraproject.org> - 2.2.3-13
- Rebuilt for https://fedoraproject.org/wiki/Fedora_31_Mass_Rebuild

* Thu Jan 31 2019 Fedora Release Engineering <releng@fedoraproject.org> - 2.2.3-12
- Rebuilt for https://fedoraproject.org/wiki/Fedora_30_Mass_Rebuild

* Wed Jan 23 2019 Milan Bouchet-Valat <nalimilan@club.fr> - 2.2.3-12
- Add -fno-strict-aliasing to CCFLAGS to build with GCC 9.

* Thu Jul 12 2018 Fedora Release Engineering <releng@fedoraproject.org> - 2.2.3-11
- Rebuilt for https://fedoraproject.org/wiki/Fedora_29_Mass_Rebuild

* Wed Feb 07 2018 Fedora Release Engineering <releng@fedoraproject.org> - 2.2.3-10
- Rebuilt for https://fedoraproject.org/wiki/Fedora_28_Mass_Rebuild

* Wed Aug 02 2017 Fedora Release Engineering <releng@fedoraproject.org> - 2.2.3-9
- Rebuilt for https://fedoraproject.org/wiki/Fedora_27_Binutils_Mass_Rebuild

* Wed Jul 26 2017 Fedora Release Engineering <releng@fedoraproject.org> - 2.2.3-8
- Rebuilt for https://fedoraproject.org/wiki/Fedora_27_Mass_Rebuild

* Fri Feb 10 2017 Fedora Release Engineering <releng@fedoraproject.org> - 2.2.3-7
- Rebuilt for https://fedoraproject.org/wiki/Fedora_26_Mass_Rebuild

* Wed Feb 03 2016 Fedora Release Engineering <releng@fedoraproject.org> - 2.2.3-6
- Rebuilt for https://fedoraproject.org/wiki/Fedora_24_Mass_Rebuild

* Wed Jun 17 2015 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2.2.3-5
- Rebuilt for https://fedoraproject.org/wiki/Fedora_23_Mass_Rebuild

* Wed Sep 3 2014 Milan Bouchet-Valat <nalimilan@club.fr> - 2.2.3-4
- Move documentation to -devel-doc package.
- Run tests (modify and simplify Makefile patch to do this).

* Sat Jun 28 2014 Milan Bouchet-Valat <nalimilan@club.fr> - 2.2.3-3
- Add patch to export functions required to use the library.

* Tue Jun 17 2014 Xavier Lamien <laxathom@fedoraproject.org> - 2.2.3-2
- Update license.
- lower variable into Makefile.

* Thu Jun  5 2014 Xavier Lamien <laxathom@fedoraproject.org> - 2.2.3-1
- Initial RPM release.
