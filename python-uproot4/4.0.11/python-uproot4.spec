%global srcname uproot4

Name:           python-%{srcname}
Version:        4.0.11
Release:        1%{?dist}
Summary:        A reader and a writer of the ROOT file format using only Python and Numpy

License:        MIT
URL:            https://pypi.python.org/pypi/example
Source0:        https://github.com/scikit-hep/uproot4/archive/%{version}.tar.gz

BuildArch:      noarch

%global _description %{expand:
A reader and a writer of the ROOT file format using only Python and Numpy.}

%description %_description

%package -n python3-%{srcname}
Summary:        %{summary}
%if 0%{?rhel} || 0%{?fedora}
BuildRequires:  python3-devel
BuildRequires:  python3-setuptools
Requires:       python3-numpy
%endif
%if 0%{?suse_version}
BuildRequires:  fdupes
BuildRequires:  python-rpm-macros
BuildRequires:  python3
BuildRequires:  python3-devel
BuildRequires:  python3-setuptools
Requires:       python3-numpy
%endif

%description -n python3-%{srcname} %_description

%prep
%autosetup -n %{srcname}-%{version}

%build
%if 0%{?rhel} || 0%{?fedora}
%py3_build
%endif
%if 0%{?suse_version}
%py3_build
%endif

%install
%if 0%{?rhel} || 0%{?fedora}
%py3_install
%endif
%if 0%{?suse_version}
%py3_install
%endif



# Note that there is no %%files section for the unversioned python module
%files -n python3-%{srcname}

%{python3_sitelib}/uproot-*.egg-info/
%{python3_sitelib}/uproot/


%changelog
* Wed May 26 2021 Andrii Verbytskyi 4.0.8
- Update to 4.0.8
* Sat Mar 13 2021 Andrii Verbytskyi 4.0.6
- Update to 4.0.6 
