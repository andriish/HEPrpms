Name:           qgraf
Version:        3.6.7
Release:        1%{?dist}
License:        Unknown
Url:            http://cfif.ist.utl.pt/~paulo/qgraf.html
Source0:        %{name}-%{version}.tgz
Summary:        Software that can generate Feynman diagrams
%if 0%{?rhel} || 0%{?fedora}
BuildRequires:  gcc-gfortran
%endif
%if 0%{?suse_version}
BuildRequires:  gcc-fortran
%endif
Prefix: %{_prefix}
%description
 QGRAF is a piece of software that can generate Feynman diagrams and 
represent them by symbolic expressions; no graphical output is 
generated, though. It was written to assist in large perturbative 
calculations, in the context of Quantum Field Theory. 


%define __arch_install_post %(/bin/true)

%prep
mkdir -p qgraf-%{version}
cd qgraf-%{version}
tar zxvf %{SOURCE0}

%build
cd qgraf-%{version}
mkdir -p fmodules
gfortran ${FFLAGS} ${LDFLAGS} -Jfmodules qgraf-%{version}.f08 -o qgraf
chmod +x qgraf

%install
cd qgraf-%{version}
mkdir -p %{buildroot}/%_bindir
mkdir -p %{buildroot}/%_docdir/%{name}
mkdir -p %{buildroot}/%_includedir/%{name}
install qgraf %{buildroot}/%_bindir
#install fmodules/* #{buildroot}/#_includedir/#{name}
#install qgraf-#{version}.pdf #{buildroot}/#_docdir/#{name}
#install qgraf-3.0.pdf #{buildroot}/#_docdir/#{name}


%files
%_bindir/%{name}
#_includedir/#{name}
#_docdir/#{name}/qgraf-#{version}.pdf
#_docdir/#{name}/qgraf-3.0.pdf


%changelog
* Sun Feb 21 2021 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - Cleanup 
* Thu May 26 2016 Andrii Verbytskyi 3.1.4
+ Initial spec file

