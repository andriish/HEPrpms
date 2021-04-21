Summary: Unified Grahics System
Name: topdrawer
Version: 1.4e
Release: 1%{?dist}
License: Free
Source: ftp://ftp.riken.jp/iris/topdrawer/topdrawer.tar.gz
#Patch: ugs-24bit-color.patch
Patch:  topdrawer_OSX_fix.patch
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
Unified Graphics System developed at SLAC.  Currently only 
PostScript, X Window System, and Tektronics 4010 are supported.  

%prep
%setup -n topdrawer -q
%patch -p1

%build
xmkmf -a


make clean
%if 0%{?rhel} || 0%{?fedora}
%if %{?fedora}%{!?fedora:0}  >= 31
make FC='gfortran -std=legacy  -fallow-argument-mismatch -fallow-invalid-boz -g' UGS=/usr/%_lib/libugs.a
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
