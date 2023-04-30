%global debug_package %{nil}
# header only lib

Name:           safeint
Version:        3.0.27
Release:       1
Summary:        Class library for C++ that manages integer overflows

License:        MIT
URL:            https://github.com/dcleblanc/SafeInt
Source0:        https://github.com/dcleblanc/SafeInt/archive/%{version}/SafeInt-%{version}.tar.gz

# https://github.com/dcleblanc/SafeInt/pull/48
Patch0:         safeint-3.0.27-gcc13_fix.patch

BuildRequires:  gcc-c++
BuildRequires:  cmake

# Test fail on i686 arch
# https://bugzilla.redhat.com/show_bug.cgi?id=2144864#c5
ExcludeArch: %{ix86}

%global _description %{expand:
An integer overflow library that was originally created in Microsoft
Office in 2003, and later was made open source on CodePlex using the MS-PL
license. After CodePlex was deprecated, the project was moved to github
and the license was changed to the MIT license.}


%description
%{_description}


%package devel
Summary: %{summary}
Provides: %{name}-static = %{version}-%{release}


%description devel
%{_description}


%prep
%autosetup -p1 -n SafeInt-%{version}

%check
%cmake
%cmake_build
%ctest


%build


%install
install -d %{buildroot}%{_includedir}/SafeInt
install -D -p SafeInt.hpp -t %{buildroot}%{_includedir}/SafeInt/
install -D -p safe_math.h -t %{buildroot}%{_includedir}/SafeInt/
install -D -p safe_math_impl.h -t %{buildroot}%{_includedir}/SafeInt/


%files devel
%license LICENSE
%doc README.md
%dir %{_includedir}/SafeInt
%{_includedir}/SafeInt/SafeInt.hpp
%{_includedir}/SafeInt/safe_math.h
%{_includedir}/SafeInt/safe_math_impl.h


%changelog
* Tue Jul 12 2022 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - bump