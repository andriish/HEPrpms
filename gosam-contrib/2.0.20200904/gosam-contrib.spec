
Name:           gosam-contrib
Version:        2.0.20200904
Release:        2%{?dist}
License:        Custom
Url:            https://github.com/gudrunhe/gosam-contrib
Source0:        https://github.com/gudrunhe/gosam-contrib/archive/gosam-contrib-2.0-20200904.tar.gz
Summary:        A collection of tools used by the GoSam package
BuildRequires:  autoconf automake libtool 
BuildRequires:  gcc-c++ 
Requires: python3 

%if 0%{?rhel} || 0%{?fedora}
BuildRequires:  gcc-gfortran
%endif
%if 0%{?suse_version}
BuildRequires:  gcc-fortran
%endif

Prefix: %{_prefix}
%description
The gosam-contrib project 


%prep 
%setup -q -n gosam-contrib-gosam-contrib-2.0-20200904

%build
sh autogen.sh
%configure  --libdir=%_libdir/gosam-contrib/
make 


%install
%make_install 
rm -f %{buildroot}/%_libdir/gosam-contrib/*.la

%files 
%doc AUTHORS README COPYING
%_bindir/*
%{_datadir}/%{name}/
%_includedir/%{name}/
%_includedir/ninja/
%_libdir/gosam-contrib/pkgconfig/samurai.pc
%_libdir/gosam-contrib/libavh_olo.a
%_libdir/gosam-contrib/libavh_olo.so
%_libdir/gosam-contrib/libavh_olo.so.0
%_libdir/gosam-contrib/libavh_olo.so.0.0.0
%_libdir/gosam-contrib/libff.a
%_libdir/gosam-contrib/libff.so
%_libdir/gosam-contrib/libff.so.0
%_libdir/gosam-contrib/libff.so.0.0.0
%_libdir/gosam-contrib/libgolem.a
%_libdir/gosam-contrib/libgolem.so
%_libdir/gosam-contrib/libgolem.so.0
%_libdir/gosam-contrib/libgolem.so.0.0.0
%_libdir/gosam-contrib/libninja.a
%_libdir/gosam-contrib/libninja.so
%_libdir/gosam-contrib/libninja.so.0
%_libdir/gosam-contrib/libninja.so.0.0.0
%_libdir/gosam-contrib/libqcdloop.a
%_libdir/gosam-contrib/libqcdloop.so
%_libdir/gosam-contrib/libqcdloop.so.0
%_libdir/gosam-contrib/libqcdloop.so.0.0.0
%_libdir/gosam-contrib/libsamurai.a
%_libdir/gosam-contrib/libsamurai.so
%_libdir/gosam-contrib/libsamurai.so.0
%_libdir/gosam-contrib/libsamurai.so.0.0.0


%changelog
* Fri Feb 19 2021 Andrii Verbytskyi 2.0-20200904
+ New version
* Thu May 26 2016 Andrii Verbytskyi 2.0.3
+ Initial spec file

