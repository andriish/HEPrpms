# We used to build with Theano support.  However, we no longer have a compatible
# version of Theano in Fedora.  If aesara is ever packaged for Fedora, we can
# use it instead.

# We are archful (see below), but there are no ELF objects in the binary RPM.
%global debug_package %{nil}

%global giturl  https://github.com/sympy/sympy
%global commit  fe935ceb303891d1f8bea4c03b19fd9ec9464b02

Name:           sympy
Version:        1.14.0
Release:        1
Summary:        A Python library for symbolic mathematics

# The project as a whole is BSD-3-Clause.
# The files in sympy/parsing/latex are MIT.
License:        BSD-3-Clause AND MIT
URL:            https://sympy.org/
VCS:            git:%{giturl}.git
Source0:        %{giturl}/archive/%{name}-%{version}.tar.gz
# For intersphinx
Source1:       objects-matplotlib.inv
Source2:        objects-scipy.inv
Source3:        objects-numpy.inv
# Do not depend on intersphinx_registry, which is not available in Fedora
Patch:          %{name}-intersphinx.patch
Patch:          0001-pyproject.toml.patch

# This package used to be noarch, and should still be noarch.  However, because
# there is no JDK available on i686 anymore, the antlr4 package is also not
# available on i686.  When we can stop building on i686 altogether, we can bring
# this back.  In the meantime, we cannot claim to be noarch, because the i686
# build is different from the other arches in lacking BuildRequires: antlr4.
# BuildArch:      noarch

BuildRequires:  fdupes
BuildRequires:  gcc
BuildRequires:  gcc-c++
BuildRequires:  gcc-gfortran
BuildRequires:  python3-devel
BuildRequires:  python3-clang
BuildRequires:  %{py3_dist cython}
BuildRequires:  %{py3_dist gmpy2}
BuildRequires:  %{py3_dist matplotlib}
BuildRequires:  %{py3_dist numexpr}
# BuildRequires:  %{py3_dist pycosat}
BuildRequires:  python3-numpy-f2py
BuildRequires:  %{py3_dist scipy}

%global _description\
SymPy aims to become a full-featured computer algebra system (CAS)\
while keeping the code as simple as possible in order to be\
comprehensible and easily extensible. SymPy is written entirely in\
Python and does not require any external libraries.

%description %_description

%package -n python3-%{name}
Summary:        A Python3 library for symbolic mathematics
Recommends:     tex(latex)
Recommends:     tex(amsfonts.sty)
Recommends:     tex(amsmath.sty)
Recommends:     tex(euler.sty)
Recommends:     tex(eulervm.sty)
Recommends:     tex(standalone.cls)
Recommends:     %{py3_dist cython}
Recommends:     %{py3_dist gmpy2}
Recommends:     %{py3_dist matplotlib}
Recommends:     %{py3_dist numexpr}
Recommends:     %{py3_dist pycosat}
Recommends:     %{py3_dist pyglet}
Recommends:     %{py3_dist scipy}

%description -n python3-%{name}
SymPy aims to become a full-featured computer algebra system (CAS)
while keeping the code as simple as possible in order to be
comprehensible and easily extensible. SymPy is written entirely in
Python and does not require any external libraries.

%prep
%autosetup -p1 -n %{name}-%{name}-%{version}

#conf
#fixtimestamp() {
#  touch -r $1.orig $1
#  rm -f $1.orig
#}

# Remove bogus shebangs
for fil in sympy/physics/mechanics/models.py \
           sympy/physics/optics/polarization.py; do
  sed -i.orig '/env python/d' $fil
  #fixtimestamp $fil
done

# Do not depend on env
for fil in $(grep -rl "^#\![[:blank:]]*%{_bindir}/env" .); do
  sed -i.orig 's,^\(#\![[:blank:]]*%{_bindir}/\)env python,\1python3,' $fil
  #fixtimestamp $fil
done

# Use local objects.inv for intersphinx
sed -e "s|\('https://matplotlib\.org/stable/', \)None|\1'%{SOURCE1}'|" \
    -e "s|\(.https://docs\.scipy\.org/doc/scipy/., \)None|\1'%{SOURCE2}'|" \
    -e "s|\(.https://numpy\.org/doc/stable/., \)None|\1'%{SOURCE3}'|" \
    -i doc/src/conf.py

# Help sphinx find the git commit
echo -n '%{commit}' > doc/commit_hash.txt

# Permit use of antlr4 4.13
sed -i s'/4\.11/4.13/g' sympy/parsing/autolev/_parse_autolev_antlr.py \
    sympy/parsing/latex/_parse_latex_antlr.py

%generate_buildrequires
%pyproject_buildrequires -x dev

%build
# Build
%pyproject_wheel

%install
%pyproject_install
%pyproject_save_files -l isympy sympy

## Remove extra files
rm -f %{buildroot}%{_bindir}/{,doc}test

# Fix permissions
chmod 0755 %{buildroot}%{python3_sitelib}/sympy/benchmarks/bench_symbench.py \
      %{buildroot}%{python3_sitelib}/sympy/testing/tests/diagnose_imports.py

%files -n python3-%{name} -f %{pyproject_files}
%doc AUTHORS README.md
%{_bindir}/isympy
%{_mandir}/man1/isympy.1*

%changelog
* Tue Jul 12 2022 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
  - Deal with comp. failures
