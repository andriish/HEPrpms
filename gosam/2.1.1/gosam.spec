%define major            0
%define libname          gosam
%define libnamedev       gosam-devel
%define develnamestatic  gosam-static-devel
%define __os_install_post %{nil}
%undefine _debugsource_packages

Name:           gosam
Version:        2.1.1
Release:        5%{?dist}
License:        GPLv3
Url:            https://github.com/gudrunhe/gosam
Source0:        https://github.com/gudrunhe/gosam/releases/download/2.1.1/gosam-2.1.1-4b98559.tar.gz
Patch0:         patch-gosam-0.txt
Summary:        Automated calculation of one-loop amplitudes 
Requires:       gosam-contrib form
BuildRequires:  autoconf automake libtool 
Prefix: %{_prefix}
Requires: python3
BuildRequires: python3  python3-devel



%if 0%{?rhel} || 0%{?fedora}
BuildRequires: gcc-gfortran python3-setuptools
%endif
%if 0%{?suse_version}
BuildRequires: gcc-fortran python-rpm-macros
%endif

%description
The package GoSam allows for the automated calculation of one-loop 
amplitudes for multi-particle processes in renormalizable quantum 
field theories. The amplitudes, which are generated in terms of Feynman 
diagrams, can be reduced using either D-dimensionalintegrand-level
 decomposition or by tensor reduction. GoSam can be used to calculate 
 one-loop QCD and/or electroweak corrections to Standard Model processes
 and offers the flexibility to link model files for theories Beyond the 
 Standard Model. A standard interface (BLHA1, BLHA2) to programs 
 calculating real radiation is also implemented. 



%prep
%setup -q -n gosam-2.1.1-4b98559
%patch0 -p1

%build

%install
python3 ./setup.py install --no-compile --prefix=%{buildroot}/%_prefix

sed -i "s|${RPM_BUILD_ROOT}||g" $RPM_BUILD_ROOT/%_bindir/gosam.py
sed -i "s|${RPM_BUILD_ROOT}||g" $RPM_BUILD_ROOT/%_bindir/gosam-config.py

sed -i "s|${RPM_BUILD_ROOT}||g" $RPM_BUILD_ROOT/%{python3_sitelib}/golem/*.py*
sed -i "s|${RPM_BUILD_ROOT}||g" $RPM_BUILD_ROOT/%{python3_sitelib}/golem/util/*.py*

%files -n %{libname}
%_bindir/gosam.py
%_bindir/gosam-config.py
%{python3_sitelib}/golem/
%{python3_sitelib}/gosam*
%{_datadir}/golem/

%changelog
* Mon Nov 15 2021 Andrii Verbytskyi 2.1.1
- Bump to 2.1.1
* Fri Feb 19 2021 Andrii Verbytskyi 2.1.0
+ New version
* Thu May 26 2016 Andrii Verbytskyi 2.0.3
+ Initial spec file

