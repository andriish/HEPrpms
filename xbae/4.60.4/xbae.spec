Name:           xbae
Version:        4.60.4
Release:        29%{?dist}
Summary:        Motif matrix, caption and text input widgets
# all the files are covered by the MIT license, except DebugUtil.c LGPLv2+
License:        LGPLv2+
URL:            http://xbae.sourceforge.net/
Source0:        https://downloads.sourceforge.net/xbae/xbae-%{version}.tar.gz
# this fixes the link of the example using Wcl, it shouldn't be of use
# now that Wcl isn't buildrequired, but it is still better.
Patch0:         xbae-link_Mri_with_lXmp.diff
Patch1:         xbae-4.60.4-multilib.patch

BuildRequires:  gcc
# libXp-devel and libXext-devel are required by openmotif-devel or
# lesstif-devel
BuildRequires:  libXpm-devel 
# needed for examples
BuildRequires:  libXmu-devel

%if 0%{?rhel}
BuildRequires:  openmotif-devel
%else
BuildRequires:  motif-devel
# to be sure that we link against lesstif even if openmotif provides the same
# soname
#Requires:       lesstif
%endif
# Wcl-devel should only needed by an example, which adds the Xbae widgets 
# to Wcl, so there is no real need for it.
#BuildRequires:  Wcl-devel

# name with capitalized X was used for the xbae package shipped up to FC-1
Provides:       Xbae = %{version}-%{release}
Obsoletes:      Xbae < %{version}-%{release}


%description
XbaeMatrix is a free Motif(R) table widget (also compatible with the free 
LessTif) which presents an editable array of string data to the user in a 
scrollable table similar to a spreadsheet. The rows and columns of the Matrix 
may optionally be labelled. A number of "fixed" and "trailing fixed" rows 
or columns may be specified.

The XbaeCaption widget is a simple Motif manager widget that associates 
a label with a child.

In addition the XbaeInput widget is being distributed, a text input field 
that provides generic customised data entry and formatting for strings.


%package        devel
Summary:        Development files for %{name}
Requires:       %{name} = %{version}-%{release}
%if 0%{?rhel}
Requires:       openmotif-devel 
%else
Requires:       motif-devel
%endif
Requires:       libXpm-devel
# for the aclocal directory
Requires:       automake
Provides:       Xbae-devel = %{version}-%{release}
Obsoletes:      Xbae-devel < %{version}-%{release}

%description    devel
The %{name}-devel package contains libraries and header files for
developing applications that use %{name}.


%prep
%setup -q
%patch 0 -p1
%patch 1 -p1 -b .multilib

for file in COPYING ChangeLog NEWS; do
 iconv -f latin1 -t utf8 < $file > $file.utf8
 touch -c -r $file $file.utf8
 mv $file.utf8 $file
done


%build
%configure --disable-static --disable-dependency-tracking
make %{?_smp_mflags}


%install
rm -rf $RPM_BUILD_ROOT
make install DESTDIR=$RPM_BUILD_ROOT mandir=%{_mandir} INSTALL='install -p'
# fix timestamps for configure generated man pages
pushd src
for file in XbaeCaption.3 XbaeInput.3 XbaeMatrix.3; do
  touch -r $file.in $RPM_BUILD_ROOT%{_mandir}/man3/$file
done
popd
# and include file
touch -r NEWS $RPM_BUILD_ROOT%{_includedir}/Xbae/patchlevel.h

# the configure test doesn't find the aclocal dir, so we install
# the .m4 file by hand
install -d -m755 $RPM_BUILD_ROOT%{_datadir}/aclocal
install -p -m644 ac_find_xbae.m4 $RPM_BUILD_ROOT%{_datadir}/aclocal

rm $RPM_BUILD_ROOT%{_libdir}/libXbae.la

# prepare the main docs
# Use systematically __dist_* to avoid directory name clash.
rm -rf __dist_Xbae-docs
mv $RPM_BUILD_ROOT%{_datadir}/Xbae/ __dist_Xbae-docs
# remove duplicate files already in %%doc
rm __dist_Xbae-docs/README
rm __dist_Xbae-docs/NEWS
# examples for builderXcessory are arch specific, this avoids clash of
# doc files between arches
mv __dist_Xbae-docs/examples/builderXcessory __dist_Xbae-docs/examples/builderXcessory-%{_arch}

# first clean examples
rm -rf __dist_examples
cp -pr examples __dist_examples
make -C __dist_examples clean
find __dist_examples -name '*akefile*' -exec rm {} \;
rm __dist_examples/extest
rm __dist_examples/testall

for file in __dist_examples/*/*.c __dist_examples/README; do
 iconv -f latin1 -t utf8 < $file > $file.utf8
 touch -c -r $file $file.utf8
 mv $file.utf8 $file
done

# the builderXcessory directory is duplicated in the main doc and in the
# example code. The master dir is considered to be in the main doc.
# the README is better with the main examples doc, not in code examples
mv __dist_examples/builderXcessory/README __dist_Xbae-docs/examples/builderXcessory-%{_arch}/
# remove the duplicate dir and replace it with a link
rm -rf __dist_examples/builderXcessory/
ln -s ../examples/builderXcessory-%{_arch}/ __dist_examples/builderXcessory-%{_arch}

# then put the examples in a code_examples directory
rm -rf __dist_code_examples
mkdir __dist_code_examples
mv __dist_examples __dist_code_examples/code_examples



%ldconfig_scriptlets


%files
%doc AUTHORS ChangeLog NEWS COPYING README
%{_libdir}/libXbae.so.*

%files devel
%doc __dist_Xbae-docs/* __dist_code_examples/code_examples
%{_includedir}/Xbae/
%{_libdir}/libXbae.so
%{_mandir}/man*/Xbae*
%{_datadir}/aclocal/*


%changelog
* Sun Feb 03 2019 Fedora Release Engineering <releng@fedoraproject.org> - 4.60.4-28
- Rebuilt for https://fedoraproject.org/wiki/Fedora_30_Mass_Rebuild

* Sat Jul 14 2018 Fedora Release Engineering <releng@fedoraproject.org> - 4.60.4-27
- Rebuilt for https://fedoraproject.org/wiki/Fedora_29_Mass_Rebuild

* Fri Feb 09 2018 Fedora Release Engineering <releng@fedoraproject.org> - 4.60.4-26
- Rebuilt for https://fedoraproject.org/wiki/Fedora_28_Mass_Rebuild

* Thu Aug 03 2017 Fedora Release Engineering <releng@fedoraproject.org> - 4.60.4-25
- Rebuilt for https://fedoraproject.org/wiki/Fedora_27_Binutils_Mass_Rebuild

* Thu Jul 27 2017 Fedora Release Engineering <releng@fedoraproject.org> - 4.60.4-24
- Rebuilt for https://fedoraproject.org/wiki/Fedora_27_Mass_Rebuild

* Sat Feb 11 2017 Fedora Release Engineering <releng@fedoraproject.org> - 4.60.4-23
- Rebuilt for https://fedoraproject.org/wiki/Fedora_26_Mass_Rebuild

* Fri Feb 05 2016 Fedora Release Engineering <releng@fedoraproject.org> - 4.60.4-22
- Rebuilt for https://fedoraproject.org/wiki/Fedora_24_Mass_Rebuild

* Thu Oct 01 2015 Jon Ciesla <limburgher@gmail.com> - 4.60.4-21
- Move from lesstif to motif, BZ 1267852.

* Fri Jun 19 2015 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 4.60.4-20
- Rebuilt for https://fedoraproject.org/wiki/Fedora_23_Mass_Rebuild

* Mon Aug 18 2014 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 4.60.4-19
- Rebuilt for https://fedoraproject.org/wiki/Fedora_21_22_Mass_Rebuild

* Sun Jun 08 2014 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 4.60.4-18
- Rebuilt for https://fedoraproject.org/wiki/Fedora_21_Mass_Rebuild

* Sun Aug 04 2013 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 4.60.4-17
- Rebuilt for https://fedoraproject.org/wiki/Fedora_20_Mass_Rebuild

* Fri Feb 15 2013 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 4.60.4-16
- Rebuilt for https://fedoraproject.org/wiki/Fedora_19_Mass_Rebuild

* Sun Jul 22 2012 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 4.60.4-15
- Rebuilt for https://fedoraproject.org/wiki/Fedora_18_Mass_Rebuild

* Sat Jan 14 2012 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 4.60.4-14
- Rebuilt for https://fedoraproject.org/wiki/Fedora_17_Mass_Rebuild

* Mon Feb 07 2011 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 4.60.4-13
- Rebuilt for https://fedoraproject.org/wiki/Fedora_15_Mass_Rebuild

* Mon Jul 27 2009 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 4.60.4-12
- Rebuilt for https://fedoraproject.org/wiki/Fedora_12_Mass_Rebuild

* Thu Feb 26 2009 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 4.60.4-11
- Rebuilt for https://fedoraproject.org/wiki/Fedora_11_Mass_Rebuild

* Sat Sep 20 2008 Patrice Dumas <pertusus@free.fr> 4.60.4-10
- use %%patch0 instead of %%patch (#463002)

* Tue Feb 19 2008 Fedora Release Engineering <rel-eng@fedoraproject.org> - 4.60.4-9
- Autorebuild for GCC 4.3

* Wed Aug 22 2007 Patrice Dumas <pertusus@free.fr> 4.60.4-8
- fix license and source url

* Wed Jul 25 2007 Patrice Dumas <pertusus@free.fr> 4.60.4-7
- add rhel conditionals

* Sat May 12 2007 Patrice Dumas <pertusus@free.fr> 4.60.4-6
- keep timestamps
- examples/builderXcessory is arch specific

* Sun Dec 10 2006 Patrice Dumas <pertusus@free.fr> 4.60.4-5
- Requires automake is for -devel (#219047)

* Sat Sep  9 2006 Patrice Dumas <pertusus@free.fr> 4.60.4-4
- add BuildRequires libXmu-devel for examples

* Thu Aug 31 2006 Patrice Dumas <pertusus@free.fr> 4.60.4-3
- rebuild against lesstif
- add Obsolete/Provides for Xbae

* Fri Aug 25 2006 Patrice Dumas <pertusus@free.fr> 4.60.4-2
- remove dependency on Wcl-devel (was only of use for an example)
- clean docs

* Thu May 18 2006 Patrice Dumas <pertusus@free.fr> 4.60.4-1
- Packaged for fedora extras
