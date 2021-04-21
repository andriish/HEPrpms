%global srcname iminuit
%global sum An example python module

Name:           %{srcname}
Version:        1.3.6
Release:        1%{?dist}
Summary:        %{sum}

License:        MIT, LGPL
URL:            https://pypi.python.org/pypi/%{srcname}
Source0:        https://files.pythonhosted.org/packages/01/82/cea3e9db377ba7db6454c1de93c83a39f83e66157dc0d9738c54aaba3d1f/iminuit-1.3.6.tar.gz
#Source0:        https://pypi.python.org/packages/c9/fd/87931ec898e16557b8f6ae6e576743d00ca085c1cb4eb4673d8f4b02ac20/%{srcname}-%{version}.tar.gz

BuildRequires: gcc-c++
 
%if %{?rhel}%{!?rhel:0} == 8 || %{?fedora}%{!?fedora:0} >= 31
BuildRequires: python3  python3-devel python3-numpy
%endif

%if 0%{?suse_version}
BuildRequires: python3  python3-devel python3-numpy python3-setuptools python3-numpy-devel
%endif



%if %{?rhel}%{!?rhel:0} == 8
Requires: python2 python2-numpy 
BuildRequires: python2-Cython
%endif
%if %{?fedora}%{!?fedora:0} > 0
%if %{?fedora}%{!?fedora:0} < 31
BuildRequires:  python2-Cython python3-Cython
%endif
%endif
%if %{?fedora}%{!?fedora:0} >= 31 
BuildRequires: python3-devel  Cython
%endif
%if 0%{?suse_version} 
BuildRequires: python3-devel  python3-Cython
%endif


Prefix: %{_prefix}
%description
An python module which provides a convenient example.



%if %{?rhel}%{!?rhel:0} == 8 || %{?fedora}%{!?fedora:0} >= 31
%package -n python3-%{srcname}
Summary:        %{sum}
%{?python_provide:%python_provide python3-%{srcname}}
%description -n python3-%{srcname}
An python module which provides a convenient example.
%endif

%if 0%{?suse_version} 
%package -n python3-%{srcname}
Summary:        %{sum}
%{?python_provide:%python_provide python3-%{srcname}}
%description -n python3-%{srcname}
An python module which provides a convenient example.
%endif

%prep
%autosetup -n %{srcname}-%{version}

%build

python3 setup.py  build

%install
python3 setup.py  install -O1 --skip-build --root %{buildroot} --prefix=/usr

# Note that there is no %%files section for the unversioned python module if we are building for several python runtimes
%files -n %{srcname}
%{python3_sitearch}/iminuit*

%changelog
