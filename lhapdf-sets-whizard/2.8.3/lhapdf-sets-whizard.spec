Name:       lhapdf-sets-whizard
Version:    2.8.3
Release:    2%{?dist}
Summary:    Set of PDFs needed to build whizard.
License:    GPLv2+
URL:        http://lhapdf.hepforge.org/
Source0:    README.txt
Prefix:     %{_prefix}

%if 0%{?rhel} || 0%{?fedora}
BuildRequires:    lhapdf >= 6.3.0 
BuildRequires: python3-lhapdf
%endif
%if 0%{?suse_version}
BuildRequires:    libLHAPDF >= 6.3.0  LHAPDF-devel
BuildRequires: python3-LHAPDF
%endif

%description
Set of PDFs needed to build whizard.


%prep


%build

%install
lhapdf  --pdfdir=%{buildroot}/usr/share/LHAPDF/ install --upgrade  cteq6l1 CT10


%clean
rm -rf %{buildroot}

%post -p /sbin/ldconfig

%postun -p /sbin/ldconfig


%files
/usr/share/LHAPDF/*

%changelog
* Thu May 26 2016 Andrii Verbytskyi 7.1.5
- This is just a technical package
