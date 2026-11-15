Name:       lhapdf-sets-Herwig
Version:    7.1.6
Release:    2%{?dist}
Summary:    Set of PDFs needed to build Herwig
License:    GPLv2+
URL:        http://lhapdf.hepforge.org/
Source0:    README.txt
%if 0%{?suse_version}
# OBS builds have no network: the PDF sets are sources, not downloaded by lhapdf install
Source1:    http://lhapdfsets.web.cern.ch/lhapdfsets/current/MMHT2014lo68cl.tar.gz
Source2:    http://lhapdfsets.web.cern.ch/lhapdfsets/current/MMHT2014nlo68cl.tar.gz
Source3:    http://lhapdfsets.web.cern.ch/lhapdfsets/current/CT14lo.tar.gz
Source4:    http://lhapdfsets.web.cern.ch/lhapdfsets/current/CT14nlo.tar.gz
%endif
Prefix:    %{_prefix}

%if 0%{?rhel} || 0%{?fedora}
BuildRequires:    lhapdf >= 6.3.0 
BuildRequires: python3-lhapdf
%endif
%if 0%{?suse_version}
BuildRequires:    libLHAPDF >= 6.3.0  LHAPDF-devel
BuildRequires: python3-LHAPDF
%endif

%description
Set of PDFs needed to build Herwig.


%prep


%build

%install
%if 0%{?suse_version}
mkdir -p %{buildroot}/usr/share/LHAPDF
tar -xzf %{SOURCE1} -C %{buildroot}/usr/share/LHAPDF
tar -xzf %{SOURCE2} -C %{buildroot}/usr/share/LHAPDF
tar -xzf %{SOURCE3} -C %{buildroot}/usr/share/LHAPDF
tar -xzf %{SOURCE4} -C %{buildroot}/usr/share/LHAPDF
%else
lhapdf  --pdfdir=%{buildroot}/usr/share/LHAPDF/ install  --upgrade MMHT2014lo68cl MMHT2014nlo68cl CT14lo CT14nlo  
%endif


%post -p /sbin/ldconfig

%postun -p /sbin/ldconfig


%files
/usr/share/LHAPDF/*

%changelog
* Sun Nov 15 2026 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de>
- openSUSE: provide the PDF sets as sources (no network in OBS builds)

* Thu May 26 2016 Andrii Verbytskyi 7.1.5
- This is just a technical package
