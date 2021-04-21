%undefine _debugsource_packages
Summary: Unified Grahics System
Name: ugs
Version: 2.10e
Release: 4%{?dist}
#Copyright: Free <= obsolete keyword for rpm 4.x
License: Free
Source: http://ftp.riken.jp/iris/ugs/ugs.tar.gz
#Patch: ugs-24bit-color.patch
Patch0:  ugs_OSX_fix.patch
Patch1:  patch-ugs-1.txt
Prefix: %{_prefix}
BuildRequires: imake  libX11-devel libXt-devel  libSM-devel libICE-devel libXext-devel  


%if 0%{?rhel} || 0%{?fedora}
Requires: libX11 libXt  libSM libICE libXext
BuildRequires: libX11 libXt  libSM libICE libXext
BuildRequires:  gcc-gfortran
%endif
%if 0%{?suse_version}
Requires: libX11-6 libXt6  libSM6 libICE6 libXext6
BuildRequires: libX11-6 libXt6  libSM6 libICE6 libXext6
BuildRequires:  gcc-fortran
%endif


%description
Unified Graphics System developed at SLAC.  Currently only 
PostScript, X Window System, and Tektronics 4010 are supported.  

%prep
%setup -n ugs -q
%patch0 -p1
%patch1 -p1

%build
xmkmf -a
sed -i 's@fort77@gfortran\ \-std=legacy@' Makefile
sed -i 's@fort77@gfortran\ \-std=legacy@' */Makefile
sed -i 's@fort77@gfortran\ \-std=legacy@' src.2.10e/*/*akefile
sed -i 's@f77@gfortran\ \-std=legacy@' Makefile
sed -i 's@f77@gfortran\ \-std=legacy@' */Makefile
sed -i 's@f77@gfortran\ \-std=legacy@' src.2.10e/*/Makefile
sed -i 's@\-Nq300@@g' src.2.10e/*/Makefile
sed -i 's@\-Nq300@@g' */Makefile
sed -i 's@\-Nq300@@g' Makefile
sed -i 's@\-\\!bs@@g'  src.2.10e/*/Makefile
sed -i 's@\-\\!bs@@g'  */Makefile
sed -i 's@\-\\!bs@@g'  Makefile
sed -i 's@\%LOC@LOC@g' src.2.10e/*.f
sed -i 's@\%LOC@LOC@g' src.2.10e/*.F

make clean
make

%install
mkdir -p $RPM_BUILD_ROOT/usr/%_lib
cp -a ugs.a $RPM_BUILD_ROOT/usr/%_lib/libugs.a

%clean

%files
%defattr(-,root,root)
/usr/%_lib/*
%doc README doc test

%changelog
* Thu Nov 23 2017 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - 2.10e for RedHat7
* Tue Oct 16 2007 Hiroyuki Okamura <okamura@rcnp.osaka-u.ac.jp>
- updated for Vine-4.1 (Copyright => License)

* Sun Apr 17 2005 Hiroyuki Okamura <okamura@cyric.tohoku.ac.jp>
- updated for Vine-3.1 (optimize level down to -O1 for gcc-3.x)

* Wed Mar 30 2005 Hiroyuki Okamura <okamura@cyric.tohoku.ac.jp>
- bug fixed for 24-bit color

* Thu Jan 30 2003 Hiroyuki Okamura <okamura@phy.saitama-u.ac.jp>
- updated for Vine-2.6 (with misc. modification to common inconsistency)

