%define major       0
%define libname     openloops
%define libnamedev  openloops-devel
%define libnamedata  openloops-data
#undefine _debugsource_packages

Name:           openloops
Version:        2.1.4
Release:        3%{?dist}
License:        GPL
Url:            http://www.openloops.hepforge.org
Source0:        https://gitlab.com/openloops/OpenLoops/-/archive/OpenLoops-%{version}/OpenLoops-OpenLoops-%{version}.tar.gz
Patch0:         patch-openloops-0.txt
Prefix:         %{_prefix}
Summary:        Automated calculation of one-loop amplitudes 
BuildRequires:  autoconf
BuildRequires:  automake
BuildRequires:  libtool
%if 0%{?rhel} || 0%{?fedora}
BuildRequires:  gcc-gfortran
%endif
%if %{?fedora}%{!?fedora:0} >= 39 || %{?rhel}%{!?rhel:0} >= 10
BuildRequires: python3-rpm-macros
%endif
%if 0%{?suse_version}
BuildRequires:  gcc-fortran
Requires: python3
BuildRequires: python3 python3-devel scons
%endif
%if %{?rhel}%{!?rhel:0} >= 8
Requires: python3
BuildRequires: python3 python3-devel   platform-python-devel python3-scons
%endif
%if %{?fedora}%{!?fedora:0} >= 31
Requires: python3
BuildRequires: python3-devel python3-scons
%endif



%description
 The OpenLoops program is a fully automated implementation of the 
 Open Loops algorithm for the fast numerical evaluation of tree and 
 one-loop matrix elements for any Standard Model process. 

%prep
%setup -qn OpenLoops-OpenLoops-%{version}
%patch -P 0 -p1

%build
sed -i  's@.*process_lib_dir.*@process_lib_dir = /usr/'%_lib'/openloops/proclib@g'  pyol/config/default.cfg

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
%if %{?fedora}%{!?fedora:0} >= 39 || %{?rhel}%{!?rhel:0} >= 10
%py3_shebang_fix ./
%else
pathfix.py -pn -i %{__python3}  ./
%endif
%endif


%if %{?rhel}%{!?rhel:0} == 8
scons-3  debug=1
%else
scons debug=1
%endif 

%install
mkdir -p $RPM_BUILD_ROOT/usr/bin
mkdir -p $RPM_BUILD_ROOT/usr/%_lib/openloops
sed -i  's@.*process_lib_dir.*@process_lib_dir = '$RPM_BUILD_ROOT'/usr/'%_lib'/openloops/proclib@g'  pyol/config/default.cfg
sed -i  's@.*process_repositories.*@process_repositories = matrix,public,powheg,ATLAS@g'  pyol/config/default.cfg
./openloops libinstall  eett eettj heftpphj pph2 pphj2 pphjj2 pphll2 pphllj_ew ppjj ppjjj ppll pplla pplla_ew ppllaj ppllaj_ew ppllj pplljj pplljjj ppllll ppllll2 ppllll2_interf ppllll2_interf_notb ppllll2_nf4 ppllll2_nf5_massless ppllll2_nf5_notri ppllll2_noh ppllll2_onlyh ppllllbb ppllllbbbb_fac ppllllbb_split ppllllj ppllllj2 ppllllj2_interf ppllllj2_interf_notb ppllllj2_nf4 ppllllj2_nf5_notri ppllllj2_noh ppllllj2_onlyh ppllllj_ew ppln pplnj pptln pptt pptt2 ppttbb pptt_im ppttj ppttj_dec pptttt pptw pptwj ppwwbb_semileptonic tbw   compile_extra=1 debug=0;

#tbln
#tbln_ew
#tbqq
#tbw
#pptttt
#pptttt_ew
#pptt
#pptt_ew
#ppttbb
#ppttj
#ppttj_ew
#ppttjj
#pptaj
#pptajj
#pptllj
#pptlljj
#pptln
#pptw
#pptwj
#pptzj
#pptzjj
#ppthj
#ppthjj
#pptj
#pptjj
#ppjj
#ppjj_ew
#ppjjj
#ppjjj_ew
#ppjjj_nf5
#ppjjjj
#pplllvvv_ew
#ppatt
#ppatt_ew
#ppattj
#pplltt
#pplltt_ew
#ppllttj
#ppllttj_ew
#pplntt
#pplnttj
#ppwtt
#ppwtt_ew
#ppwttj
#ppwttj_ew
#ppztt
#ppztt_ew
#ppzttj
#ppaatt
#ppwwtt
#ppzatt
#ppzztt
#ppvvvv
#ppaaaj2
#ppllaa
#ppllaaj
#pplllla
#ppvvv
#ppvvv2
#ppvvv_ew
#ppvvvj
#ppaajj
#ppaajj2
#ppaajjj
#pplla
#pplla2
#pplla_ew
#ppllaj
#ppllaj2
#ppllaj_ew
#ppllaj_nf5
#ppllajj
#ppllll
#ppllll2
#ppllll2_nf5
#ppllll2_onlyh
#ppllll_ew
#ppllllbb
#ppllllj
#ppllllj2
#ppllllj2_nf5
#ppllllj2_nf5_notridr
#ppllllj2_nf5_sr
#ppllllj2_onlyh
#ppllnnjj_ew
#ppllnnjj_vbs
#pplnaj_ckm
#pplnajj
#pplnajj_ckm
#ppvv
#ppvv2
#ppvv_ew
#ppvvj
#ppvvj2
#ppvvj_ew
#ppwajj
#ppwwjj
#ppzajj
#ppzwj_ew
#ppzwjj
#ppzzjj
#ppajj
#ppajj2
#ppajj_ew
#ppajjj
#ppllj
#ppllj2
#ppllj_ew
#ppllj_nf5
#pplljj
#pplljj_ew
#pplljjj
#pplnj_ckm
#pplnjj
#pplnjj_ckm
#pplnjj_ew
#pplnjjj
#ppnnjj_ew
#ppnnjjj
#ppvj
#ppvj2
#ppvj_ew
#ppwj_ckm
#ppwjj
#ppwjj_ckm
#ppwjj_ew
#ppwjjj
#ppzjj
#ppzjj_ew
#ppzjjj
#pphtt
#pphtt_ew
#pphttj
#pphlltt
#pphll
#pphll2
#pphll_ew
#pphllj
#pphllj2
#pphllj_ew
#pphlljj
#pphlljj_top
#pphlnj_ckm
#pphlnjj
#pphv
#pphv_ew
#pphwjj
#pphz2
#pphzj2
#pphzjj
#pphhtt
#pphhv
#pphhh2
#heftpphh
#heftpphhj
#heftpphhjj
#pphh2
#pphhj2
#pphhjj2
#pphhjj_vbf
#bbhj
#heftpphj
#heftpphjj
#heftpphjjj
#pphbb
#pphbbj
#pphj2
#pphjj2
#pphjj_vbf
#pphjj_vbf_ew
#pphjjj2
#eetttt
#eettttj
#eellllbb
#eett
#eett_ew
#eettj
#eettjj
#eevtt
#eevttj
#eevttjj
#eevvtt
#eevvttj
#eellll_ew
#eevv_ew
#eevvjj
#eell_ew
#eevjj
#eehtt
#eehttj
#eehll_ew
#eehvtt
#eehhtt
#heftppllj
#heftpplljj
#heftpplljjj


sed -i  's@.*process_lib_dir.*@process_lib_dir = /usr/'%_lib'/openloops/proclib@g'  pyol/config/default.cfg

mkdir -p $RPM_BUILD_ROOT/%{_bindir}
mkdir -p $RPM_BUILD_ROOT/%{_libdir}/openloops
sed -i  's@.*process_lib_dir.*@process_lib_dir = '$RPM_BUILD_ROOT%{_libdir}'/openloops/proclib@g'  pyol/config/default.cfg
./openloops libinstall pptt  compile_extra=1  debug=0;
sed -i  's@.*process_lib_dir.*@process_lib_dir = '%{_libdir}'/openloops/proclib@g'  pyol/config/default.cfg
sed -i  's@^LIBDIR=.*@LIBDIR=\"../lib64/openloops/lib\"@' openloops
sed -i  's@^PROCLIBDIR=.*@PROCLIBDIR=\"../'%_lib'/openloops/proclib\"@' openloops
sed -i  's@^PYOLBINDIR=.*@PYOLBINDIR=\"../'%_lib'/openloops/pyol/bin\"@' openloops
sed -i  's@^PYOLMODDIR=.*@PYOLMODDIR=\"../'%_lib'/openloops/pyol/tools\"@' openloops
sed -i  's@^INCDIR=.*@INCDIR=\"../include\"@' openloops
chmod +x ./openloops
cp -r ./openloops  $RPM_BUILD_ROOT/%{_bindir}/openloops
cp    ./SConstruct $RPM_BUILD_ROOT/%{_libdir}/openloops
ln -s  %{_bindir}/scons $RPM_BUILD_ROOT/%{_libdir}/openloops/scons
cp -r ./lib  $RPM_BUILD_ROOT/%{_libdir}/openloops
cp -r ./pyol  $RPM_BUILD_ROOT/%{_libdir}/openloops
mkdir -p $RPM_BUILD_ROOT/%_includedir/openloops/lib_src
cp  ./include/*.h $RPM_BUILD_ROOT/usr/include
mkdir -p $RPM_BUILD_ROOT/%_includedir/openloops/lib_src/collier
cp -r  lib_src/collier/mod  $RPM_BUILD_ROOT/%_includedir/openloops/lib_src/collier
mkdir -p $RPM_BUILD_ROOT/%_includedir/openloops/lib_src/cuttools
cp -r  lib_src/cuttools/mod  $RPM_BUILD_ROOT/%_includedir/openloops/lib_src/cuttools
mkdir -p $RPM_BUILD_ROOT/%_includedir/openloops/lib_src/olcommon
cp -r  lib_src/olcommon/mod  $RPM_BUILD_ROOT/%_includedir/openloops/lib_src/olcommon
mkdir -p $RPM_BUILD_ROOT/%_includedir/openloops/lib_src/oneloop
cp -r  lib_src/oneloop/mod  $RPM_BUILD_ROOT/%_includedir/openloops/lib_src/oneloop
mkdir -p $RPM_BUILD_ROOT//%_includedir/openloops/lib_src/openloops
cp -r  lib_src/openloops/mod  $RPM_BUILD_ROOT/%_includedir/openloops/lib_src/openloops
mkdir -p $RPM_BUILD_ROOT/%_includedir/openloops/lib_src/rambo
cp -r  lib_src/rambo/mod  $RPM_BUILD_ROOT/%_includedir/openloops/lib_src/rambo
mkdir -p $RPM_BUILD_ROOT/%_includedir/openloops/lib_src/trred
cp -r  lib_src/trred/mod  $RPM_BUILD_ROOT/%_includedir/openloops/lib_src/trred


%files -n %{libname}
%doc README COPYING
%{_bindir}/%{name}
%{_libdir}/*
%_includedir/openloops/*
%_includedir/openloops.h

%changelog
* Fri May 10 2024 Andrii Verbytskyi 2.1.3
- add modules
* Fri Jan 19 2024 Andrii Verbytskyi 2.1.3
- Update to 2.1.3
* Sat Mar 13 2021 Andrii Verbytskyi 2.1.2
- Update to 2.1.2 and added python3 to Fedora
* Thu May 26 2016 Andrii Verbytskyi 1.3.1
+ Initial spec file
