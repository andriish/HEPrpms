Summary: A  keyword-driven  interface  designed  to  produce physics  graphs  with  minimal specifications
Name: topdrawer
Version: 1.4e
Release: 5%{?dist}
License: Free
Source: https://ftp.riken.jp/iris/topdrawer/topdrawer.tar.gz
URL:    https://ribf.riken.jp/comp/doc/topdrawer/
#Patch: ugs-24bit-color.patch
Patch0:  topdrawer_OSX_fix.patch
Patch1:  patch-topdrawer-0.txt
Prefix: %{_prefix}
BuildRequires: f2c ugs imake  
BuildRequires: libX11-devel libXt-devel  libSM-devel libICE-devel libXext-devel
Requires:  ugs

%if 0%{?rhel} || 0%{?fedora}
Requires: libX11 libXt  libSM libICE libXext
BuildRequires: libX11 libXt  libSM libICE libXext
BuildRequires:  gcc-gfortran
%endif
%if 0%{?suse_version}
Requires: libX11-6 libXt6  libSM6 libICE6 libXext6
BuildRequires: libX11-6 libXt6  libSM6 libICE6 libXext6
BuildRequires:  gcc-fortran
BuildRequires:  f2c-devel libf2c0
%endif

%description
Topdrawer is  a  keyword-driven  interface  designed  to  produce
physics  graphs  with  minimal specifications.  It was originally
developed by R.B. Chaffee at SLAC.  

%prep
%setup -n topdrawer -q
%patch -P 0 -p1
%patch -P 1 -p1

%build
xmkmf -a


make clean
%if 0%{?rhel} || 0%{?fedora}
%if %{?fedora}%{!?fedora:0}  >= 31 || %{?rhel}%{!?rhel:0} > 8
%if %{?fedora}%{!?fedora:0}  >= 42 
make FC='gfortran -std=legacy  -fallow-argument-mismatch -fallow-invalid-boz -g'  CC='gcc -std=gnu17' UGS=/usr/%_lib/libugs.a
%else
make FC='gfortran -std=legacy  -fallow-argument-mismatch -fallow-invalid-boz -g' UGS=/usr/%_lib/libugs.a
%endif
%else
make FC='gfortran -std=legacy  -g' UGS=/usr/%_lib/libugs.a
%endif
%endif
%if 0%{?suse_version}
make FC='gfortran -std=legacy  -fallow-argument-mismatch -fallow-invalid-boz' UGS=/usr/%_lib/libugs.a
%endif

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT/usr/bin
cp -a td $RPM_BUILD_ROOT/usr/bin


%files
%defattr(-,root,root)
/usr/bin/td
%doc README doc 

%changelog
* Thu Nov 23 2017 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - 1.4e for RedHat7
