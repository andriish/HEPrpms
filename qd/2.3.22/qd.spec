# Fortran module directory
%{!?_fmoddir: %global _fmoddir %{_libdir}/gfortran/modules}

Name:		qd
Version:	2.3.22
Release:	2%{?dist}
Summary:	Double-Double and Quad-Double Arithmetic
License:	LBNL BSD
URL:		http://crd.lbl.gov/~dhbailey/mpdist/
Source0:	https://www.davidhbailey.com/dhbsoftware/%{name}-%{version}.tar.gz
# Fix underlinked libraries
Patch0:		%{name}-lib-deps.patch

BuildRequires:	gcc-c++
BuildRequires:	libtool
BuildRequires:	tex(latex)
%if 0%{?rhel} || 0%{?fedora}
BuildRequires:	gcc-gfortran
BuildRequires:	ghostscript-tools-dvipdf
%endif
%if 0%{?suse_version}
BuildRequires:	gcc-fortran
BuildRequires:	ghostscript
%endif

%description
This package provides numeric types of twice the precision of IEEE
double (106 mantissa bits, or approximately 32 decimal digits) and
four times the precision of IEEE double (212 mantissa bits, or
approximately 64 decimal digits).  Due to features such as operator
and function overloading, these facilities can be utilized
with only minor modifications to conventional C++ and Fortran-90
programs.

In addition to the basic arithmetic operations (add, subtract,
multiply, divide, square root), common transcendental functions such
as the exponential, logarithm, trigonometric and hyperbolic functions
are also included. 

%package devel
Summary:	Double-Double and Quad-Double Arithmetic
Requires:	%{name} = %{version}-%{release}

%description devel
This package provides numeric types of twice the precision of IEEE
double (106 mantissa bits, or approximately 32 decimal digits) and
four times the precision of IEEE double (212 mantissa bits, or
approximately 64 decimal digits).  Due to features such as operator
and function overloading, these facilities can be utilized
with only minor modifications to conventional C++ and Fortran-90
programs.

In addition to the basic arithmetic operations (add, subtract,
multiply, divide, square root), common transcendental functions such
as the exponential, logarithm, trigonometric and hyperbolic functions
are also included.

%prep
%autosetup -p0

# Force documentation rebuild
rm -f docs/qd.pdf
sed -i 's/docsqd\.pdf/docs qd.pdf/' Makefile.am

# Regenerate the makefiles due to the changes above and timestamp issues
autoreconf -fi .

%build
%ifarch s390 s390x aarch64 ppc64le
%global optflags %{optflags} -ffp-contract=off
%endif
# FIXME: This should not be necessary!
%ifarch aarch64
%global optflags %{optflags} -mabi=lp64
%endif
export CC=gcc
export CXX=g++	
export FC=gfortran
export FCFLAGS="%{optflags}"

%configure --enable-shared --disable-static

# Get rid of undesirable hardcoded rpaths; workaround libtool reordering
# -Wl,--as-needed after all the libraries.
sed -e 's|^hardcode_libdir_flag_spec=.*|hardcode_libdir_flag_spec=""|g' \
    -e 's|^runpath_var=LD_RUN_PATH|runpath_var=DIE_RPATH_DIE|g' \
    -e 's|CC="gfortran"|CC="gfortran -Wl,--as-needed"|' \
    -e 's|CC=.g[c+][c+]|& -Wl,--as-needed|' \
    -i libtool

# Supply missing fortran tags
sed -i '/F77/s/\$(AM_V_lt)/& --tag=FC/' fortran/Makefile

%make_build

%install
%make_install

# Fix location of documentation
%if  %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0}
mv %{buildroot}%{_docdir}/qd/* .
rm -rf %{buildroot}%{_datadir}
%endif

# Move Fortran modules to {_fmoddir}
mkdir -p %{buildroot}%{_fmoddir}/%{name}
mv %{buildroot}%{_includedir}/qd/*.mod %{buildroot}%{_fmoddir}/%{name}

# Remove la file
rm %{buildroot}%{_libdir}/*.la

# Fix pkgconfig file on 64-bit systems
if [ "%{_lib}" = "lib64" ]; then
  sed -i 's/^libdir=.*/&64/' %{buildroot}%{_libdir}/pkgconfig/qd.pc
fi

%check
LD_LIBRARY_PATH=$PWD/src/.libs:$PWD/fortran/.libs make check

%files
%doc AUTHORS NEWS README TODO
%license BSD-LBNL-License.doc COPYING
%{_libdir}/libqd*.so.0*

%files devel
%if  %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0}
%doc qd.pdf
%endif
%if 0%{?suse_version}
/usr/share/doc/qd/
%endif
%{_bindir}/qd-config
%{_fmoddir}/qd/
%{_includedir}/qd/
%{_libdir}/libqd*.so
%{_libdir}/pkgconfig/qd.pc

%changelog
* Thu Jan 30 2020 Fedora Release Engineering <releng@fedoraproject.org> - 2.3.22-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_32_Mass_Rebuild

* Wed Aug 28 2019 Jerry James <loganjerry@gmail.com> - 2.3.22-1
- Update to 2.3.22
- Change License to LBNL BSD
- Add -lib-deps patch to fix underlinked libraries
- Drop ancient Obsoletes
- Rebuild the documentation
- Add ppc64le to the list of arches that need -ffp-contract=off
- Remove rpaths and fix overlinked libraries

* Fri Jul 26 2019 Fedora Release Engineering <releng@fedoraproject.org> - 2.3.15-12
- Rebuilt for https://fedoraproject.org/wiki/Fedora_31_Mass_Rebuild

* Sat Feb 02 2019 Fedora Release Engineering <releng@fedoraproject.org> - 2.3.15-11
- Rebuilt for https://fedoraproject.org/wiki/Fedora_30_Mass_Rebuild

* Sat Jul 14 2018 Fedora Release Engineering <releng@fedoraproject.org> - 2.3.15-10
- Rebuilt for https://fedoraproject.org/wiki/Fedora_29_Mass_Rebuild

* Fri Feb 09 2018 Fedora Release Engineering <releng@fedoraproject.org> - 2.3.15-9
- Rebuilt for https://fedoraproject.org/wiki/Fedora_28_Mass_Rebuild

* Thu Aug 03 2017 Fedora Release Engineering <releng@fedoraproject.org> - 2.3.15-8
- Rebuilt for https://fedoraproject.org/wiki/Fedora_27_Binutils_Mass_Rebuild

* Thu Jul 27 2017 Fedora Release Engineering <releng@fedoraproject.org> - 2.3.15-7
- Rebuilt for https://fedoraproject.org/wiki/Fedora_27_Mass_Rebuild

* Sat Feb 11 2017 Fedora Release Engineering <releng@fedoraproject.org> - 2.3.15-6
- Rebuilt for https://fedoraproject.org/wiki/Fedora_26_Mass_Rebuild

* Wed Aug 24 2016 Dan Horák <dan[at]danny.cz> - 2.3.15-5
- fix precision issue on s390(x) and aarch64 (#1076070)

* Thu Feb 04 2016 Fedora Release Engineering <releng@fedoraproject.org> - 2.3.15-4
- Rebuilt for https://fedoraproject.org/wiki/Fedora_24_Mass_Rebuild

* Thu Jun 18 2015 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2.3.15-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_23_Mass_Rebuild

* Sat May 02 2015 Kalev Lember <kalevlember@gmail.com> - 2.3.15-2
- Rebuilt for GCC 5 C++11 ABI change

* Sat Oct 25 2014 Peter Robinson <pbrobinson@fedoraproject.org> 2.3.15-1
- Update to 2.3.15.
- Don't fail build on tests for s390/aarch64

* Sun Aug 17 2014 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2.3.14-4
- Rebuilt for https://fedoraproject.org/wiki/Fedora_21_22_Mass_Rebuild

* Sun Jun 08 2014 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2.3.14-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_21_Mass_Rebuild

* Sun Aug 04 2013 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2.3.14-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_20_Mass_Rebuild

* Wed Jun 05 2013 Susi Lehtola <jussilehtola@fedoraproject.org> - 2.3.14-1
- Update to 2.3.14.

* Thu Feb 14 2013 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2.3.13-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_19_Mass_Rebuild

* Sun Jul 22 2012 Jussi Lehtola <jussilehtola@fedoraproject.org> - 2.3.13-1
- Update to 2.3.13.
- Drop shared library patch, which isn't necessary anymore.

* Sat Jul 21 2012 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2.3.11-6
- Rebuilt for https://fedoraproject.org/wiki/Fedora_18_Mass_Rebuild

* Tue Feb 28 2012 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2.3.11-5
- Rebuilt for c++ ABI breakage

* Sat Jan 14 2012 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2.3.11-4
- Rebuilt for https://fedoraproject.org/wiki/Fedora_17_Mass_Rebuild

* Tue Feb 08 2011 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2.3.11-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_15_Mass_Rebuild

* Wed Nov 03 2010 Jussi Lehtola <jussilehtola@fedoraproject.org> - 2.3.11-2
- Build shared library using Nils Petersen's patch (BZ #648964).

* Tue Nov 02 2010 Jussi Lehtola <jussilehtola@fedoraproject.org> - 2.3.11-1
- Update to 2.3.11.

* Wed Sep 16 2009 Jussi Lehtola <jussilehtola@fedoraproject.org> - 2.3.7-7
- Fix location of Fortran modules.

* Sun Jul 26 2009 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2.3.7-6
- Rebuilt for https://fedoraproject.org/wiki/Fedora_12_Mass_Rebuild

* Sun Mar 08 2009 Jussi Lehtola <jussilehtola@fedoraproject.org> - 2.3.7-5
- Fix license.

* Thu Dec 18 2008 Jussi Lehtola <jussilehtola@fedoraproject.org> - 2.3.7-4
- Change base name back to qd.

* Sat Dec 13 2008 Jussi Lehtola <jussilehtola@fedoraproject.org> - 2.3.7-3
- Removed debug package.

* Sat Dec 13 2008 Jussi Lehtola <jussilehtola@fedoraproject.org> - 2.3.7-2
- Review fixes.
- Rename package to qd-devel.

* Thu Nov 06 2008 Jussi Lehtola <jussilehtola@fedoraproject.org> - 2.3.7-1
- First release.
