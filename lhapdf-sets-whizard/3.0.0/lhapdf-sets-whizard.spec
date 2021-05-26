Name:       lhapdf-sets-whizard
Version:    3.0.0
Release:    1%{?dist}
Summary:    Set of PDFs needed to build whizard.
License:    GPLv2+
URL:        http://lhapdf.hepforge.org/
Source0:    README.txt
Source1:    https://lhapdf.hepforge.org/downloads/pdfsets/5.9.1/GSG961.LHgrid
Source2:    https://lhapdf.hepforge.org/downloads/pdfsets/5.9.1/cteq5l.LHgrid
Source3:    lhapdf5to6
Source4:    https://lhapdf.hepforge.org/downloads/old/lhapdf-5.9.1.tar.gz
Prefix:     %{_prefix}

%if 0%{?rhel} || 0%{?fedora}
BuildRequires:    lhapdf >= 6.3.0 
BuildRequires: python3-lhapdf python2 python2-devel swig python2-numpy
%endif
%if 0%{?suse_version}
BuildRequires:    libLHAPDF >= 6.3.0  LHAPDF-devel
BuildRequires: python3-LHAPDF python2 python2-devel swig python2-numpy
%endif

%description
Set of PDFs needed to build whizard.


%prep
cp %{_sourcedir}/*.LHgrid ./
cp %{_sourcedir}/lhapdf5to6 ./
chmod +x ./lhapdf5to6
cp %{_sourcedir}/lhapdf-5.9.1.tar.gz ./
tar zxvf lhapdf-5.9.1.tar.gz


%build
cd lhapdf-5.9.1
export PYTHON=/usr/bin/python2
export FCFLAGS='-std=legacy  -Wno-errors'
export FFLAGS='-std=legacy  -Wno-error'
export CXXFLAGS=' -O0 -Wno-error'
export CFLAGS=' -O0 -Wno-error'
export LDFLAGS=' '
./configure --disable-octave --disable-doxygen --prefix=$(pwd)/../temp
rm -rf pyext/lhapdf_wrap.cc
make
make install
cd ..
mkdir -p temp/share/lhapdf/PDFsets/
mv *.LHgrid temp/share/lhapdf/PDFsets/

%install
lhapdf  --pdfdir=%{buildroot}/usr/share/LHAPDF/ install --upgrade  cteq6l1 cteq6ll CT10
export PYTHONPATH=temp/%_lib/python2.7/site-packages:temp/%_lib/python2.7
export LD_LIBRARY_PATH=temp/lib/:$LD_LIBRARY_PATH

./lhapdf5to6 GSG961.LHgrid --resample --desc='GS-G-96  photon PDFs' --id=350
mv GSG961 %{buildroot}/usr/share/LHAPDF/
./lhapdf5to6 cteq5l.LHgrid --resample --desc='CTEQ5L' --id=9005
mv cteq5l %{buildroot}/usr/share/LHAPDF/


%clean
rm -rf %{buildroot}

%post -p /sbin/ldconfig

%postun -p /sbin/ldconfig


%files
/usr/share/LHAPDF/*

%changelog
* Wed May 26 2021 Andrii Verbytskyi 3.0.0
- Update to 3.0.0
* Thu May 26 2016 Andrii Verbytskyi 2.8.3
- This is just a technical package
