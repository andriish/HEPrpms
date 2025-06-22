# OCaml packages not built on i686 since OCaml 5 / Fedora 39.
ExcludeArch: %{ix86}

%ifnarch %{ocaml_native_compiler}
%global debug_package %{nil}
%endif

Name:		hevea
Version:	2.36
Release:	10%{?dist}
Summary:	LaTeX to HTML translator

# QPL-1.0-INRIA-2004 WITH QPL-1.0-INRIA-2004-exception: the project as a whole
# LPPL-1.3a: hrlang.hva, lstlang*.sty, examples/natbib.sty
# GPL-2.0-or-later: html/mathpartir.hva, examples/mathpartir-test.tex
License:	QPL-1.0-INRIA-2004 WITH QPL-1.0-INRIA-2004-exception AND LPPL-1.3a AND GPL-2.0-or-later
URL:		http://hevea.inria.fr/
Source0:	https://github.com/maranget/hevea/archive/refs/tags/v%{version}.tar.gz

BuildRequires:	make
BuildRequires:	ocaml
BuildRequires:	ocaml-ocamlbuild
BuildRequires:	tex(latex)
BuildRequires:	tex(amsfonts.sty)
#BuildRequires:	tex(comment.sty)
BuildRequires:	tex(keyval.sty)
BuildRequires:	tex(url.sty)

Requires:	ghostscript
Requires:	netpbm-progs
Requires:	tex(dvips)
Requires:	tex(latex)
Requires:	tex(amsfonts.sty)
#Requires:	tex(comment.sty)
Requires:	tex(keyval.sty)
Requires:	tex(url.sty)


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
# The next line causes ocamlbuild to pass -g everywhere:
echo true: debug >> _tags
ulimit -s unlimited
%make_build \
%ifnarch %{ocaml_native_compiler}
	TARGET=byte \
%endif
	PREFIX=%{_prefix} \
	LIBDIR=%{_datadir}/%{name} \
	LATEXLIBDIR=%{_texmf}/tex/latex/hevea


%install
%make_install \
%ifnarch %{ocaml_native_compiler}
	TARGET=byte \
%endif
	PREFIX=%{_prefix} \
	LIBDIR=%{_datadir}/hevea \
	LATEXLIBDIR=%{_texmf}/tex/latex/hevea

# Link, rather than copy, identical files
rm %{buildroot}%{_datadir}/hevea/{info,text}/report.hva
ln %{buildroot}%{_datadir}/hevea/html/report.hva \
   %{buildroot}%{_datadir}/hevea/info/report.hva
ln %{buildroot}%{_datadir}/hevea/html/report.hva \
   %{buildroot}%{_datadir}/hevea/text/report.hva

# Fix up the examples for installation
rm examples/.gitignore
rm examples/hevea.sty
ln -s %{_texmf}/tex/latex/hevea/hevea.sty examples


%files
%doc README CHANGES examples
%license LICENSE
%{_bindir}/*
%{_datadir}/hevea
%{_texmf}/tex/latex/hevea/


%changelog
* Mon Dec 18 2023 Richard W.M. Jones <rjones@redhat.com> - 2.36-10
- OCaml 5.1.1 + s390x code gen fix for Fedora 40

* Tue Dec 12 2023 Richard W.M. Jones <rjones@redhat.com> - 2.36-9
- OCaml 5.1.1 rebuild for Fedora 40

* Thu Oct 05 2023 Richard W.M. Jones <rjones@redhat.com> - 2.36-8
- OCaml 5.1 rebuild for Fedora 40

* Wed Oct  4 2023 Jerry James <loganjerry@gmail.com> - 2.36-7
- Link, rather than copy, identical files
- Fix up the examples for installation

* Thu Jul 20 2023 Fedora Release Engineering <releng@fedoraproject.org> - 2.36-7
- Rebuilt for https://fedoraproject.org/wiki/Fedora_39_Mass_Rebuild

* Tue Jul 11 2023 Richard W.M. Jones <rjones@redhat.com> - 2.36-6
- OCaml 5.0 rebuild for Fedora 39

* Mon Jul 10 2023 Jerry James <loganjerry@gmail.com> - 2.36-5
- OCaml 5.0.0 rebuild
- Update QPL portion of the license tag

* Tue Jan 24 2023 Richard W.M. Jones <rjones@redhat.com> - 2.36-4
- Rebuild OCaml packages for F38

* Thu Jan 19 2023 Fedora Release Engineering <releng@fedoraproject.org> - 2.36-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_38_Mass_Rebuild

* Tue Dec 20 2022 Jerry James <loganjerry@gmail.com> - 2.36-2
- Convert License tag to SPDX

* Thu Jul 21 2022 Fedora Release Engineering <releng@fedoraproject.org> - 2.36-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_37_Mass_Rebuild

* Mon Jul 18 2022 Jerry James <loganjerry@gmail.com> - 2.36-1
- Version 2.36
- Add dependencies on required LaTeX packages

* Sat Jun 18 2022 Richard W.M. Jones <rjones@redhat.com> - 2.35-8
- OCaml 4.14.0 rebuild

* Fri Feb 04 2022 Richard W.M. Jones <rjones@redhat.com> - 2.35-7
- OCaml 4.13.1 rebuild to remove package notes

* Thu Jan 20 2022 Fedora Release Engineering <releng@fedoraproject.org> - 2.35-6
- Rebuilt for https://fedoraproject.org/wiki/Fedora_36_Mass_Rebuild

* Mon Oct 04 2021 Richard W.M. Jones <rjones@redhat.com> - 2.35-5
- OCaml 4.13.1 build

* Thu Jul 22 2021 Fedora Release Engineering <releng@fedoraproject.org> - 2.35-4
- Rebuilt for https://fedoraproject.org/wiki/Fedora_35_Mass_Rebuild

* Fri May 14 2021 Jerry James <loganjerry@gmail.com> - 2.35-3
- Depend on ghostscript, not ghostscript-core

* Mon Mar  1 13:12:05 GMT 2021 Richard W.M. Jones <rjones@redhat.com> - 2.35-2
- OCaml 4.12.0 build

* Thu Feb  4 2021 Jerry James <loganjerry@gmail.com> - 2.35-1
- Version 2.35

* Wed Jan 27 2021 Richard W.M. Jones <rjones@redhat.com> - 2.34-11
- Bump and rebuild for s390.

* Tue Jan 26 2021 Fedora Release Engineering <releng@fedoraproject.org> - 2.34-10
- Rebuilt for https://fedoraproject.org/wiki/Fedora_34_Mass_Rebuild

* Tue Sep 01 2020 Richard W.M. Jones <rjones@redhat.com> - 2.34-9
- OCaml 4.11.1 rebuild

* Fri Aug 21 2020 Richard W.M. Jones <rjones@redhat.com> - 2.34-8
- OCaml 4.11.0 rebuild

* Wed Jul 29 2020 Richard W.M. Jones <rjones@redhat.com> - 2.34-7
- Force -g flag everywhere.

* Tue Jul 28 2020 Fedora Release Engineering <releng@fedoraproject.org> - 2.34-6
- Rebuilt for https://fedoraproject.org/wiki/Fedora_33_Mass_Rebuild

* Mon May 04 2020 Richard W.M. Jones <rjones@redhat.com> - 2.34-5
- OCaml 4.11.0+dev2-2020-04-22 rebuild

* Tue Apr 21 2020 Richard W.M. Jones <rjones@redhat.com> - 2.34-4
- OCaml 4.11.0 pre-release attempt 2

* Fri Apr 17 2020 Richard W.M. Jones <rjones@redhat.com> - 2.34-3
- OCaml 4.11.0 pre-release

* Thu Apr 02 2020 Richard W.M. Jones <rjones@redhat.com> - 2.34-2
- Update all OCaml dependencies for RPM 4.16.

* Fri Mar 27 2020 Jerry James <loganjerry@gmail.com> - 2.34-1
- Version 2.34

* Wed Feb 26 2020 Richard W.M. Jones <rjones@redhat.com> - 2.33-2
- OCaml 4.10.0 final.

* Wed Feb 19 2020 Jerry James <loganjerry@gmail.com> - 2.33-1
- Version 2.33

* Wed Jan 29 2020 Fedora Release Engineering <releng@fedoraproject.org> - 2.32-11
- Rebuilt for https://fedoraproject.org/wiki/Fedora_32_Mass_Rebuild

* Sun Jan 19 2020 Richard W.M. Jones <rjones@redhat.com> - 2.32-10
- OCaml 4.10.0+beta1 rebuild.

* Thu Jan 09 2020 Richard W.M. Jones <rjones@redhat.com> - 2.32-9
- OCaml 4.09.0 for riscv64

* Thu Dec 05 2019 Richard W.M. Jones <rjones@redhat.com> - 2.32-8
- OCaml 4.09.0 (final) rebuild.

* Fri Aug 16 2019 Richard W.M. Jones <rjones@redhat.com> - 2.32-7
- OCaml 4.08.1 (final) rebuild.

* Wed Jul 31 2019 Richard W.M. Jones <rjones@redhat.com> - 2.32-6
- OCaml 4.08.1 (rc2) rebuild.

* Thu Jul 25 2019 Fedora Release Engineering <releng@fedoraproject.org> - 2.32-5
- Rebuilt for https://fedoraproject.org/wiki/Fedora_31_Mass_Rebuild

* Thu Jun 27 2019 Richard W.M. Jones <rjones@redhat.com> - 2.32-4
- OCaml 4.08.0 (final) rebuild.

* Mon Apr 29 2019 Richard W.M. Jones <rjones@redhat.com> - 2.32-3
- OCaml 4.08.0 (beta 3) rebuild.

* Fri Feb 01 2019 Fedora Release Engineering <releng@fedoraproject.org> - 2.32-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_30_Mass_Rebuild

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
