Name:       pythia6
Version:    6.4.28
Release:    8%{?dist}
Summary:    Multipurpose Monte Carlo Event Generator for High Energy Physics.

License:    Unknown
URL:        http://home.thep.lu.se/~torbjorn/Pythia.html
Source0:    https://pythia.org/download/pythia6/pythia6428-split.tgz
Patch0:     patch-pythia6-0.txt
%if 0%{?rhel} || 0%{?fedora}
BuildRequires:  gcc-gfortran
%endif
%if 0%{?suse_version}
BuildRequires:  gcc-fortran
%endif
Prefix: %{_prefix}

%description
PYTHIA is a program for the generation of high-energy physics events, i.e.
for the description of collisions at high energies between elementary
particles such as e+, e-, p and anti-p in various combinations. It contains
theory and models for a number of physics aspects, including hard and soft
interactions, parton distributions, initial and final-state parton showers,
multiple interactions, fragmentation and decay.


%prep
%setup -q -c   pythia-6.4.28
%patch0 -p1

%build
export FFLAGS="%{optflags} -fPIC"
export LDFLAGS="--build-id"
make %{?_smp_mflags}  lib

%install
mkdir -p %{buildroot}%{_libdir}
install -m 755 libpythia6.so %{buildroot}%{_libdir}
install -m 755 libpythia6.a %{buildroot}%{_libdir}
install -m 755 libpythia6_dummy.so %{buildroot}%{_libdir}
install -m 755 libpythia6_dummy.a %{buildroot}%{_libdir}



%post -p /sbin/ldconfig
%postun -p /sbin/ldconfig

%files
%{_libdir}/*

%changelog
* Tue Jul 15 2021 Andrii Verbytskyi
- So far use github for source.

