%global debug_package %{nil}
Name:           form
Version:        4.3.0
Release:        1%{?dist}
License:        GPLv3
Url:            https://github.com/vermaseren/form
Source0:        https://github.com/vermaseren/form/releases/download/v%{version}/form-%{version}.tar.gz
Summary:        The FORM project for symbolic manipulation of very big expressions
BuildRequires:  autoconf automake libtool  gcc-c++ doxygen 
%if 0%{?rhel} || 0%{?fedora}
BuildRequires:  gcc-gfortran
%endif
%if 0%{?suse_version}
BuildRequires:  gcc-fortran texlive-tex4ht texlive-helvetic texlive-wasy texlive-courier texlive-filesystem
%endif
BuildRequires:  tex(latex) ghostscript 
BuildRequires:  tex(tabu.sty) tex(multirow.sty) tex(multicol.sty) tex(adjustbox.sty) tex(sectsty.sty) tex(tocloft.sty) 
BuildRequires:   tex(ulem.sty)  tex(wasysym.sty) tex(float.sty) 
%if 0%{?suse_version}
BuildRequires: tex(hanging.sty) tex(stackengine.sty)  tex(newunicodechar.sty) tex(etoc.sty) tex(fancyvrb.sty)  tex(enumitem.sty) tex(alphalph.sty)
%endif
%if  %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} > 8 
BuildRequires: tex(hanging.sty) tex(stackengine.sty)  tex(newunicodechar.sty) tex(etoc.sty) tex(ulem.sty)
%endif
%if %{?rhel}%{!?rhel:0} >= 8 || %{?fedora}%{!?fedora:0} >= 31
BuildRequires: ghostscript-tools-dvipdf
%endif
Prefix: %{_prefix}

%description
FORM is a Symbolic Manipulation System. It reads symbolic expressions from files and executes 
symbolic/algebraic transformations upon them. The answers are returned in a textual 
mathematical representation. As its landmark feature, the size of the considered 
expressions in FORM is only limited by the available disk space and not by 
the available RAM.
FORM's original author is Jos Vermaseren of NIKHEF, the Dutch institute for 
subatomic physics. Other people that have made contributions can be found in 
the file "AUTHORS".

%package doc
Summary:       Documentation for %{name} 

%description doc
Documentation for %{name}.

%prep 
%setup -q -n form-%{version}

%build
autoreconf -fisv
%configure  
make %{?_smp_mflags}
make %{?_smp_mflags} -C doc pdf
make %{?_smp_mflags} -C doc html

%install
%make_install 

%files 
%_bindir/*

%files  doc
%{_mandir}/man1/form.1.gz
%doc doc/manual/manual.pdf
%doc doc/devref/devref.pdf


%changelog
* Fri Mar 10 2023 Andrii Verbytskyi 4.3.0
- Update to 4.3.0
* Fri Feb 19 2021 Andrii Verbytskyi 4.2.1
+ Initial spec file fro 4.2.1
* Sat Sep 23 2017 Andrii Verbytskyi 4.2
+ Initial spec file
* Thu May 26 2016 Andrii Verbytskyi 4.1
+ Initial spec file

