%global _texmf /usr/share/texlive/texmf-local

%ifnarch %{ocaml_native_compiler}
%global debug_package %{nil}
%endif

Name:		hevea
Version:	2.32
Release:	2%{?dist}
Summary:	LaTeX to HTML translator
License:	QPL
URL:		http://hevea.inria.fr/
Source0:	https://github.com/maranget/hevea/archive/refs/tags/v2.32.tar.gz
BuildRequires:	ocaml, tex(latex)
BuildRequires:  ocaml-ocamlbuild
Requires:	tex(latex) netpbm-progs ghostscript tex(dvips)


%description
HEVEA is a quite complete and fast LATEX to HTML translator.
HEVEA renders symbols by using the so-called HTML "entities", which
modern browsers display correctly most of the time.


%prep
%setup -q

# Fix encoding
iconv -f iso-8859-1 -t utf-8 CHANGES > CHANGES.utf8
touch -r CHANGES CHANGES.utf8
mv -f CHANGES.utf8 CHANGES


%build
ulimit -s unlimited
make %{?_smp_mflags} \
%ifnarch %{ocaml_native_compiler}
	TARGET=byte \
%endif
	PREFIX=%{_prefix} \
	LIBDIR=%{_datadir}/%{name} \
	LATEXLIBDIR=%{_texmf}/tex/latex/hevea \
	OCAMLFLAGS="-g -w +a-4-9"


%install
make install \
%ifnarch %{ocaml_native_compiler}
	TARGET=byte \
%endif
	DESTDIR=%{buildroot}
	PREFIX=%{_prefix} \
	LIBDIR=%{_datadir}/hevea \
	LATEXLIBDIR=%{_texmf}/tex/latex/hevea

	
%files
%doc README CHANGES examples 
%license LICENSE
%{_bindir}/*
%{_datadir}/hevea
%{_texmf}/tex/latex/hevea/


%changelog
* Wed Sep 12 2018 Jerry James <loganjerry@gmail.com> - 2.32-1
- New upstream release

* Fri Jul 13 2018 Fedora Release Engineering <releng@fedoraproject.org> - 2.31-6
- Rebuilt for https://fedoraproject.org/wiki/Fedora_29_Mass_Rebuild

* Wed Jul 11 2018 Richard W.M. Jones <rjones@redhat.com> - 2.31-5
- OCaml 4.07.0 (final) rebuild.

* Tue Jun 19 2018 Richard W.M. Jones <rjones@redhat.com> - 2.31-4
- OCaml 4.07.0-rc1 rebuild.

* Thu Apr 26 2018 Richard W.M. Jones <rjones@redhat.com> - 2.31-3
- OCaml 4.07.0-beta2 rebuild.

* Wed Feb 07 2018 Fedora Release Engineering <releng@fedoraproject.org> - 2.31-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_28_Mass_Rebuild

* Mon Jan 15 2018 Jerry James <loganjerry@gmail.com> - 2.31-1
- New upstream release

* Tue Nov 07 2017 Richard W.M. Jones <rjones@redhat.com> - 2.30-4
- OCaml 4.06.0 rebuild.

* Mon Aug 07 2017 Richard W.M. Jones <rjones@redhat.com> - 2.30-3
- OCaml 4.05.0 rebuild.

* Wed Aug 02 2017 Fedora Release Engineering <releng@fedoraproject.org> - 2.30-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_27_Binutils_Mass_Rebuild

* Wed Jul 26 2017 Richard W.M. Jones <rjones@redhat.com> - 2.30-1
- New upstream version 2.30.

* Wed Jul 26 2017 Fedora Release Engineering <releng@fedoraproject.org> - 2.29-7
- Rebuilt for https://fedoraproject.org/wiki/Fedora_27_Mass_Rebuild

* Mon Jun 26 2017 Richard W.M. Jones <rjones@redhat.com> - 2.29-6
- OCaml 4.04.2 rebuild.

* Thu May 11 2017 Richard W.M. Jones <rjones@redhat.com> - 2.29-5
- Bump release and rebuild.

* Thu May 11 2017 Richard W.M. Jones <rjones@redhat.com> - 2.29-4
- OCaml 4.04.1 rebuild.

* Fri Feb 10 2017 Fedora Release Engineering <releng@fedoraproject.org> - 2.29-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_26_Mass_Rebuild

* Sat Nov 05 2016 Richard W.M. Jones <rjones@redhat.com> - 2.29-2
- Rebuild for OCaml 4.04.0.
- Add explicit dependency on ocamlbuild.

* Fri Sep 16 2016 Jerry James <loganjerry@gmail.com> - 2.29-1
- New upstream release

* Wed Feb 03 2016 Fedora Release Engineering <releng@fedoraproject.org> - 2.28-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_24_Mass_Rebuild

* Thu Jan 28 2016 Jerry James <loganjerry@gmail.com> - 2.28-1
- New upstream release

* Sat Oct  3 2015 Jerry James <loganjerry@gmail.com> - 2.25-1
- New upstream release

* Tue Jul 28 2015 Richard W.M. Jones <rjones@redhat.com> - 2.23-5
- OCaml 4.02.3 rebuild.

* Wed Jun 24 2015 Richard W.M. Jones <rjones@redhat.com> - 2.23-4
- ocaml-4.02.2 final rebuild.

* Wed Jun 17 2015 Richard W.M. Jones <rjones@redhat.com> - 2.23-3
- ocaml-4.02.2 rebuild.

* Wed Jun 17 2015 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2.23-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_23_Mass_Rebuild

* Wed Mar 18 2015 Jerry James <loganjerry@gmail.com> - 2.23-1
- New upstream release
- Use the license macro
- Include examples in doc

* Mon Feb 16 2015 Richard W.M. Jones <rjones@redhat.com> - 2.18-2
- ocaml-4.02.1 rebuild.

* Thu Oct 30 2014 Jerry James <loganjerry@gmail.com> - 2.18-1
- New upstream release

* Sat Aug 30 2014 Richard W.M. Jones <rjones@redhat.com> - 2.16-5
- ocaml-4.02.0 final rebuild.

* Sat Aug 23 2014 Richard W.M. Jones <rjones@redhat.com> - 2.16-4
- ocaml-4.02.0+rc1 rebuild.

* Sat Aug 16 2014 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2.16-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_21_22_Mass_Rebuild

* Sat Aug 02 2014 Richard W.M. Jones <rjones@redhat.com> - 2.16-2
- ocaml-4.02.0-0.8.git10e45753.fc22 rebuild.

* Thu Jun 12 2014 Jerry James <loganjerry@gmail.com> - 2.16-1
- New upstream release

* Sat Jun 07 2014 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2.14-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_21_Mass_Rebuild

* Tue May 13 2014 Jerry James <loganjerry@gmail.com> - 2.14-1
- New upstream release

* Wed Apr 23 2014 Richard W.M. Jones <rjones@redhat.com> - 2.13-2
- Remove ocaml_arches (see rhbz#1087794).

* Mon Mar 31 2014 Jerry James <loganjerry@gmail.com> - 2.13-1
- New upstream release
- Unbreak bytecode build

* Thu Jan 16 2014 Jerry James <loganjerry@gmail.com> - 2.12-2
- Unlimit the stack to fix the ppc64 build (really fixes bz 879050)

* Thu Jan 16 2014 Jerry James <loganjerry@gmail.com> - 2.12-1
- New upstream release
- Work around broken texlive _texmf_main macro (bz 1054317)

* Mon Sep 16 2013 Jerry James <loganjerry@gmail.com> - 2.09-2
- Fix typo in Requires(postun)

* Mon Sep 16 2013 Jerry James <loganjerry@gmail.com> - 2.09-1
- New upstream release (hopefully fixes bz 879050)
- Build for OCaml 4.01.0
- Enable debuginfo
- Use _texmf_main macro (bz 989703)
- Modernize the spec file

* Sat Aug 03 2013 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.10-11
- Rebuilt for https://fedoraproject.org/wiki/Fedora_20_Mass_Rebuild

* Sun Jul 21 2013 Peter Lemenkov <lemenkov@gmail.com> - 1.10-10
- Exclude ppc64 arches - it's not possible to build hevea on these ones

* Thu Feb 14 2013 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.10-9
- Rebuilt for https://fedoraproject.org/wiki/Fedora_19_Mass_Rebuild

* Thu Jul 19 2012 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.10-8
- Rebuilt for https://fedoraproject.org/wiki/Fedora_18_Mass_Rebuild

* Fri Jan 13 2012 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.10-7
- Rebuilt for https://fedoraproject.org/wiki/Fedora_17_Mass_Rebuild

* Wed Feb 09 2011 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.10-6
- Rebuilt for https://fedoraproject.org/wiki/Fedora_15_Mass_Rebuild

* Tue Sep 22 2009 Dennis Gilmore <dennis@ausil.us> - 1.10-5
- switch ExclusiveArch to ExcludeArch on arches withot ocaml

* Fri Jul 24 2009 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.10-4
- Rebuilt for https://fedoraproject.org/wiki/Fedora_12_Mass_Rebuild

* Tue Feb 24 2009 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.10-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_11_Mass_Rebuild

* Wed Nov 26 2008 Richard W.M. Jones <rjones@redhat.com> - 1.10-2
- Rebuild for OCaml 3.11.0+rc1.

* Mon Feb 11 2008 Andreas Thienemann <andreas@bawue.net> - 1.10-1
- Updated to 1.10

* Wed Aug 22 2007 Andreas Thienemann <andreas@bawue.net> - 1.09-3
- Added EA x86_64 as it was forgotten in -2

* Tue Aug 14 2007 Andreas Thienemann <andreas@bawue.net> - 1.09-2
- Added EA to prevent building on ppc64 until ocaml is available

* Tue Aug 14 2007 Andreas Thienemann <andreas@bawue.net> - 1.09-1
- Updated to 1.09

* Fri Sep 08 2006 Andreas Thienemann <andreas@bawue.net> - 1.08-6
- FE6 Rebuild

* Mon May 01 2006 Andreas Thienemann <andreas@bawue.net> - 1.08-5
- Typofix in %%post

* Sun Apr 30 2006 Andreas Thienemann <andreas@bawue.net> - 1.08-4
- Included Requirements for imagen

* Fri Apr 28 2006 Andreas Thienemann <andreas@bawue.net> - 1.08-3
- Better comformity to FHS

* Fri Apr 28 2006 Andreas Thienemann <andreas@bawue.net> - 1.08-2
- Cleaned up and adapted for FE

* Sat Jul  2 2005 Gerard Milmeister <gemi@bluewin.ch> - 1.08-1
- New Version 1.08

* Fri Mar 19 2004 Gerard Milmeister <gemi@bluewin.ch> - 0:1.07-0.fdr.1
- First Fedora release
