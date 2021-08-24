Name:           onnxoptimizer
Version:        0.2.6
Release:        4%{?dist}
Summary:        Library for C++

License:        MIT
URL:           https://github.com/onnx/optimizer
Source0:        https://github.com/onnx/optimizer/archive/refs/tags/v0.2.6.tar.gz
Patch0:         patch-onnxoptimizer-0.txt

BuildRequires:  gcc onnx-devel onnx pybind11-devel onnx_proto protobuf protobuf-devel
BuildRequires:  gcc-c++
BuildRequires:  cmake
BuildRequires:  python3-devel
Requires: onnx

%description
Some lib

%package        devel
Summary:        Development files for %{name}
Requires:       %{name}%{?_isa} = %{version}-%{release}


%description    devel
This package contains the header file for using %{name}.


%prep
%setup -q -n optimizer-%{version}
%autopatch -p1
sed -i 's@set(ONNX_ROOT \${PROJECT_SOURCE_DIR}/third_party/onnx)@@g' CMakeLists.txt
#sed -i 's@add_subdirectory(\${ONNX_ROOT})@find_package\(onnx\)@g' CMakeLists.txt
sed -i 's@add_subdirectory(\${ONNX_ROOT})@find_package\(ONNX\)@g' CMakeLists.txt
sed -i 's@target_link_libraries(onnx_optimizer PUBLIC onnx)@target_link_libraries(onnx_optimizer PUBLIC onnx_proto onnx  protobuf)@g' CMakeLists.txt
sed -i '/add_library(onnx_optimizer \${onnx_opt_srcs})/a target_compile_definitions(onnx_optimizer PUBLIC ONNX_ML=1 ONNX_NAMESPACE=onnx)' CMakeLists.txt
#sed -i '/add_library(onnx_optimizer \${onnx_opt_srcs})/a target_compile_definitions(onnx_optimizer PUBLIC  ONNX_NAMESPACE=onnx)' CMakeLists.txt


%build
#if %{?rhel}%{!?rhel:0} == 8
#export CXXFLAGS=" -O2 -g -std=c++11"
#export LDFLAGS=" "
#endif

%cmake  -DBUILD_ONNX_PYTHON:BOOL=ON
%cmake_build

%install
%cmake_install
mkdir -p %{buildroot}/%{_includedir}/onnx/optimizer
cp -r %{buildroot}/%{_includedir}/onnxoptimizer/*  %{buildroot}/%{_includedir}/onnx/optimizer

%files 
%{_libdir}/*

%files devel
%{_includedir}/onnx/optimizer
%{_includedir}/onnxoptimizer


%changelog
* Fri Jan 26 2018 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> 1.7.0
- start
