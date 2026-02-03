Name:           python-mpmath
Version:        1.2.1
Release:        1
Summary:        A pure Python library for multiprecision floating-point arithmetic
License:        BSD
URL:            https://mpmath.org
# Source code
Source0:        https://github.com/fredrik-johansson/mpmath/archive/%{version}/%{name}-%{version}.tar.gz

# Switch to 'traditional' theme in RHEL since 'classic' isn't available
Patch0:         python-mpmath-1.0.0-sphinx.patch

# #1974835 - CVE-2021-29063 python-mpmath: Regular expression denial of service in the mpmathify function
Patch1:         https://github.com/fredrik-johansson/mpmath/pull/570.patch

BuildRequires:  python3-devel
BuildRequires:  python3-pip
BuildRequires:  python3-setuptools
BuildRequires:  python3-setuptools_scm
BuildRequires:  python3-pytest
BuildRequires:  python3-sphinx
BuildRequires:  xorg-x11-server-Xvfb

# For building documentation
BuildRequires:  dvipng
BuildRequires:  tex(latex)

BuildArch:      noarch

%global _description %{expand:
Mpmath is a pure-Python library for multiprecision floating-point
arithmetic. It provides an extensive set of transcendental functions,
unlimited exponent sizes, complex numbers, interval arithmetic,
numerical integration and differentiation, root-finding, linear
algebra, and much more. Almost any calculation can be performed just
as well at 10-digit or 1000-digit precision, and in many cases mpmath
implements asymptotically fast algorithms that scale well for
extremely high precision work. If available, mpmath will (optionally)
use gmpy to speed up high precision operations.}

%description %_description

%package -n python3-mpmath
Summary:        A pure Python library for multiprecision floating-point arithmetic
%if 0%{?fedora} || 0%{?rhel} > 7
Recommends: python3-matplotlib
%endif
%{?python_provide:%python_provide python3-mpmath}

%description -n python3-mpmath %_description

If you require plotting capabilities in mpmath, install python3-matplotlib.


%package doc
Summary:        HTML documentation for %{name}
Requires:       python3-mpmath = %{version}-%{release}

%description doc
This package contains the HTML documentation for %{name}.


%prep
%setup -q -n mpmath-%{version}
%if 0%{?rhel} == 6 || 0%{?rhel} == 7
%patch0 -p1 -b .sphinx
%endif
%patch1 -p1

# Convert line encodings
for doc in CHANGES LICENSE README.rst TODO mpmath/tests/runtests.py; do
 sed "s|\r||g" $doc > $doc.new && \
 touch -r $doc $doc.new && \
 mv $doc.new $doc
done
find doc -name *.txt -exec sed -i "s|\r||g" {} \;

shebangs="mpmath/matrices/eigen.py mpmath/matrices/eigen_symmetric.py mpmath/tests/runtests.py mpmath/tests/test_eigen.py mpmath/tests/test_eigen_symmetric.py mpmath/tests/test_levin.py"
# Get rid of unnecessary shebangs
for lib in $shebangs; do
 sed '/^#!.*/d; 1q' $lib > $lib.new && \
 touch -r $lib $lib.new && \
 mv $lib.new $lib
done

sed -i -r 's/use_scm_version=True/version="%{version}"/' setup.py

%build
%py3_build

# Build documentation
cd doc
%{__python3} build.py

%install
%py3_install

%check
cd build/lib/mpmath/tests/
#xvfb-run -a pytest-3 -v

%files -n python3-mpmath
%license LICENSE
%doc CHANGES README.rst TODO
%{python3_sitelib}/mpmath/
%{python3_sitelib}/mpmath-%{version}-*.egg-info

%files doc
%doc doc/build/*

%changelog
* Sun Jan 10 2021 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - Initial
