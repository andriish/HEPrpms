Name:           qgraf
Version:        3.4.2
Release:        2%{?dist}
License:        Unknown
Url:            http://cfif.ist.utl.pt/~paulo/qgraf.html
Source0:        https://github.com/andriish/HEPsources/raw/master/%{name}-%{version}.tgz
Source1:        qgraf-patch.tar
Summary:        A piece of software that can generate Feynman diagrams
%if 0%{?rhel} || 0%{?fedora}
BuildRequires:  gcc-gfortran gcc gcc-c++
%endif
%if 0%{?suse_version}
BuildRequires:  gcc-fortran gcc gcc-c++
%endif
BuildRequires:  autoconf automake libtool 
Prefix: %{_prefix}
%description
 QGRAF is a piece of software that can generate Feynman diagrams and 
represent them by symbolic expressions; no graphical output is 
generated, though. It was written to assist in large perturbative 
calculations, in the context of Quantum Field Theory. 


%define __arch_install_post %(/bin/true)

%prep 
%setup -q -D -n . 
tar -xf %SOURCE1
touch ./NEWS
touch ./README
touch ./AUTHORS
touch ./ChangeLog


%build
autoreconf -fisv
%configure  
make %{?_smp_mflags}

%install
%make_install 


%files
%_bindir/*
%doc AUTHORS README COPYING

%changelog
* Sun Feb 21 2021 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - Cleanup 
* Thu May 26 2016 Andrii Verbytskyi 3.1.4
+ Initial spec file

