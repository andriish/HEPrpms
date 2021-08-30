Name:       BAT
Version:    1.0.0
Release:    4%{?dist}
Summary:    BAT -- Bayesian Analysis Toolkit

License:    LGPL v3
URL:        https://bat.mpp.mpg.de/
Source:     http://github.com/bat/bat/releases/download/v1.0.0/BAT-%{version}.tar.gz
Prefix: %{_prefix}

BuildRequires:    cuba-devel
Requires:         cuba
BuildRequires:    gcc-c++
%if 0%{?rhel} || 0%{?fedora}
BuildRequires:    root-core root-hist root-io root-tree
Requires:         root-core root-hist root-io root-tree
%endif
%if 0%{?suse_version}
BuildRequires:    root6-config root6-devel root6-libs root6 root6-utils
Requires:         root6-config root6-devel root6-libs root6 root6-utils
%endif


%description
The Bayesian Analysis Toolkit, BAT, is a software package which addresses the 
points above. It is designed to help solve statistical problems encountered 
in Bayesian inference. BAT is based on Bayes' Theorem and is realized with 
the use of Markov Chain Monte Carlo. This gives access to the full posterior 
probability distribution and enables straightforward parameter estimation, 
limit setting and uncertainty propagation. 


%prep
%setup -q

%build 
%configure --with-cuba=/usr/include  --with-rootsys=/usr

make %{?_smp_mflags} 

%install

make %{?_smp_mflags} install DESTDIR=%{?buildroot}

%clean
rm -rf %{buildroot}

%post -p /sbin/ldconfig
%postun -p /sbin/ldconfig

%files
%{_bindir}/*
%{_libdir}/*
%{_includedir}/*
/usr/share/BAT/*

%changelog
* Tue Apr 20 2021 Andrii Verbytskyi <andrii.verbtskyi@mpp.mpg.de>
+ Preparation for release
* Fri Nov 29 2019 Andrii Verbytskyi 1.0.0
+ 1.0.0 Initial BAT spec
