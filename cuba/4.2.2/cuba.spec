%define         libname         %{name}
%define         develname       %{name}-devel
%define         srcname         Cuba

Name:           cuba
Version:        4.2.2
Release:        4%{?dist}
Summary:        A library for multidimensional numerical integration.
License:        LGPLv3
Prefix:         %{_prefix}
URL:            http://www.feynarts.de/cuba
Source0:        https://www.feynarts.de/cuba/%{srcname}-%{version}.tar.gz
#build shared lib
Patch0:         cuba-4.2.1-shlib.patch
#remove all deps to Wolfwram Mathematica
Patch1:         cuba-4.2-nukewolf.patch
#sanity settings:
#https://github.com/MoMEMta/MoMEMta/blob/master/external/cuba/
#set IPC pages to be removed after process detachment
#increase definition of zero from 1pow(104) -> pow(250)
Patch2:         cuba-4.2-stddecl-safe.patch
Patch3:         cuba-4.2-Qt5.patch


BuildRequires:  gcc-c++ autoconf automake libtool
%if 0%{?rhel} || 0%{?fedora}
BuildRequires:  gcc-gfortran
BuildRequires:  libquadmath-devel
BuildRequires:  libgomp
%endif
%if 0%{?suse_version}
BuildRequires:  gcc-fortran
BuildRequires:  libquadmath0
BuildRequires:  libgomp1
%endif

%description 
The Cuba library offers a choice of four independent routines for
multidimensional numerical integration: Vegas, Suave, Divonne, and
Cuhre. All four have a C/C++ and Fortran interface and can integrate
vector integrands. Their invocation is very similar, so it is easy to
substitute one method by another for cross-checking. For further
safeguarding, the output is supplemented by a chi-square probability
which quantifies the reliability of the error estimate. C functions
can be called from Fortran directly, so there is no need for adapter
code. Similarly, linking Fortran code with the library is
straightforward and requires no extra tools. In Fortran and C/C++ the
Cuba library can (and usually does) automatically parallelize the
sampling of the integrand.


%package devel
License:        LGPLv3
Summary:        Headers and modules for the Cuba library
Provides:       %{name}-devel = %{version}-%{release}
Requires:       %{libname} = %{version}-%{release}

%description devel
%{summary}.


%prep

%setup -q -n %{srcname}-%{version}
%autopatch -p1


%build
autoreconf -fi
%if %{?fedora}%{!?fedora:0} >= 31
LDFLAGS=' '
CXXFLAGS='-fcommon -g'
CFLAGS='-fcommon -g'
%endif
%if %{?fedora}%{!?fedora:0} >= 41
CFLAGS='-fcommon -g -std=gnu17' 
%endif
%configure
#parallel build broken
make


%install
%make_install
rm -f %{buildroot}%{_datadir}/cuba.pdf

#nuke la files from libtools
find %{buildroot} -name '*.*a' -delete


%files 
%{_libdir}/libcuba.so*
%doc COPYING cuba.pdf


%files -n %{develname}
%doc ChangeLog
%{_includedir}/*.h


%changelog
* Tue Jul 12 2022 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de 4.2.1-1
 - Version bump
* Sun Feb 21 2021 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de 4.2.1-1
 - Cleanup, copied patches from Mageia 
 
* Sat Mar 30 2019 umeabot <umeabot> 4.2-6.mga7
+ Revision: 1381487
- Qt5 Rebuild

* Sun Sep 23 2018 umeabot <umeabot> 4.2-5.mga7
+ Revision: 1297345
- Mageia 7 Mass Rebuild

* Thu May 17 2018 daviddavid <daviddavid> 4.2-4.mga7
+ Revision: 1230021
- rebuild with fixed release tag

* Thu May 17 2018 daviddavid <daviddavid> 4.2-3.mga7
+ Revision: 1230020
- port to Qt5
- remove unneeded static libraries

* Mon May 08 2017 eatdirt <eatdirt> 4.2-3.mga6
+ Revision: 1099687
- Add patch to sanitize ipc memory and zero definition

* Wed May 03 2017 eatdirt <eatdirt> 4.2-2.mga6
+ Revision: 1098849
- Fix missing symbols in the shared library (divonne_)

* Sat Apr 22 2017 eatdirt <eatdirt> 4.2-1.mga6
+ Revision: 1097070
- imported package cuba


* Sat Apr 22 2017 Chris Ringeval <eatdirt@mageia.org> 4.2-1.mga6
- Importing the cuba library 4.2
