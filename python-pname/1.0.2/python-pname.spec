%global srcname pname

Name:           python-%{srcname}
Version:        1.0.2
Release:        1%{?dist}
Summary:        Check whether a package name is available on PyPI
License:        MIT
URL:            https://pypi.python.org/pypi/example
Source0:        https://files.pythonhosted.org/packages/source/p/pname/pname-%{version}.tar.gz

BuildArch:      noarch

%global _description %{expand:
 Check whether a package name is available on PyPI.}

%description %_description

%package -n python3-%{srcname}
Summary:        %{summary}
%if 0%{?rhel} || 0%{?fedora}
BuildRequires:  python3-setuptools
%endif
%if 0%{?suse_version}
BuildRequires:  fdupes
BuildRequires:  python-rpm-macros
BuildRequires:  python3
BuildRequires:  python3-setuptools
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

%{python3_sitelib}/*



%changelog
* Wed May 26 2021 Andrii Verbytskyi 4.0.8
- Update to 1.0.2 
