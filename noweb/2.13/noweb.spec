%undefine _debugsource_packages
##
##  noweb.spec -- OpenPKG RPM Package Specification
##  Copyright (c) 2000-2005 OpenPKG Foundation e.V. <http://openpkg.net/>
##  Copyright (c) 2000-2005 Ralf S. Engelschall <http://engelschall.com/>
##  Copyright (c) 2016-     Dilawar Singh <dilawars@ncbs.res.in>
##
##  Permission to use, copy, modify, and distribute this software for
##  any purpose with or without fee is hereby granted, provided that
##  the above copyright notice and this permission notice appear in all
##  copies.
##
##  THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
##  WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
##  MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
##  IN NO EVENT SHALL THE AUTHORS AND COPYRIGHT HOLDERS AND THEIR
##  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
##  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
##  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
##  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
##  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
##  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
##  OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
##  SUCH DAMAGE.
##

#   package information

Name:         noweb
Summary:      Literate Programming Tool
URL:          http://www.eecs.harvard.edu/~nr/noweb/
Group:        Development/Tools/Other
License:      GPL-2.0+
Version:      2.13
Release:      1
Source0:      https://github.com/nrnrnr/noweb/archive/refs/tags/v2_13.tar.gz
AutoReqProv:  no
BuildRequires:  sed gawk gcc
Requires:  gawk

%description
    Literate programming is the art of preparing programs for human
    readers. Noweb is designed to meet the needs of literate programmers
    while remaining as simple as possible. Its primary advantages are
    simplicity, extensibility, and language-independence. noweb uses 5
    control sequences to WEB's 27. The noweb manual is only 3 pages; an
    additional page explains how to customize its LaTeX output. Noweb
    works ``out of the box'' with any programming language, and supports
    TeX, LaTeX, HTML, and troff back ends.

Author:       Norman Ramsey

%prep
%setup  -q -n noweb-2_13 -c

%build
cd noweb-2_13/src
./awkname gawk
make %{?_smp_mflags} \
    CFLAGS="${CFLAGS} ${RPM_OPT_FLAGS}" \
    LIBSRC=awk \
    BIN=%{_bindir} \
    MAN=%{_datadir}/man \
    LIB=%{_libdir}/noweb \
    TEXINPUTS=%{_datadir}/noweb/tex \
    ELISP=%{_datadir}/noweb/elisp

%install
mkdir -p -m 755 \
    $RPM_BUILD_ROOT%{_bindir} \
    $RPM_BUILD_ROOT%{_datadir}/man \
    $RPM_BUILD_ROOT%{_libdir}/noweb \
    $RPM_BUILD_ROOT%{_datadir}/noweb/tex \
    $RPM_BUILD_ROOT%{_datadir}/noweb/elisp
cd noweb-2_13/src
make %{?_smp_mflags} \
    LIBSRC=awk \
    BIN=$RPM_BUILD_ROOT%{_bindir} \
    MAN=$RPM_BUILD_ROOT%{_datadir}/man \
    LIB=$RPM_BUILD_ROOT%{_libdir}/noweb \
    TEXINPUTS=$RPM_BUILD_ROOT%{_datadir}/noweb/tex \
    ELISP=$RPM_BUILD_ROOT%{_datadir}/noweb/elisp install
cd ..
rm -rf $RPM_BUILD_ROOT%{_datadir}/noweb/elisp
strip $RPM_BUILD_ROOT%{_bindir}/* >/dev/null 2>&1 || true
( cd $RPM_BUILD_ROOT%{_prefix}
  sed -i \
      -e "s;$RPM_BUILD_ROOT/;/;g" \
      $(find . -type f)
) || exit $?

%files
%defattr(-,root,root)
%{_bindir}/noweb
%{_bindir}/notangle
%{_bindir}/noweave
%{_bindir}/nountangle
%{_bindir}/nodefs
%{_bindir}/noroots
%{_bindir}/nuweb2noweb
%{_bindir}/cpif
%{_bindir}/htmltoc
%{_bindir}/noroff
%{_bindir}/noindex

%dir %{_libdir}/noweb
%dir %{_datadir}/noweb
%dir %{_datadir}/noweb/tex
%{_libdir}/noweb/*
%{_datadir}/noweb/tex/*

%doc %{_mandir}/man1/*
%doc %{_mandir}/man7/*

%clean
    rm -rf $RPM_BUILD_ROOT

%changelog
Mon 6 Nov 2023 Andrii Verbytskyi
 * 2.13