%define git_version d246a06562593670f62951fed0541e2a894f9788
Name:		arcanist
Version:	1.0.0
Release:	1%{?dist}
Summary:	Command-line interface for Phabricator

Group:		Application/System
License:	Apache 2.0
URL:		https://github.com/phacility/arcanist
Source0:	https://github.com/phacility/arcanist/archive/%{git_version}.tar.gz
BuildArch:	noarch

Requires:	libphutil


%description
This package is a versioned "snapshot" of arcanist
%prep
%setup -q -n %{name}-%{git_version}

%install
cd ../
mkdir -p $RPM_BUILD_ROOT/opt/
mkdir -p $RPM_BUILD_ROOT/etc/
umask 022
cp -r arcanist-%{git_version} $RPM_BUILD_ROOT/opt/arcanist
rm -rf $RPM_BUILD_ROOT/opt/arcanist/.arc*
rm -rf $RPM_BUILD_ROOT/opt/arcanist/.edit*
rm -rf $RPM_BUILD_ROOT/opt/arcanist/.gitignore* 
umask 133



%files
%defattr(-,root,root,-)
%dir /opt/arcanist
/opt/arcanist/*


%post
ln -s /opt/arcanist/bin/arc /usr/bin/arc
cat << EOF
Add the following to /etc/arcconfig:
{
	"phabricator.uri" : PHABRICATOR_HOSTNAME
}
EOF


%postun
rm -f /usr/bin/arc


%changelog
* Mon Sep 06 2021 Andrii Verbytskyi
- Bump
* Wed May 27 2015 Kaitlin Poskaitis <katiepru@nbcs.rutgers.edu> 15.5.27-1.ru6
- Version bump

* Wed Jan 21 2015 Jungsoo Park <jp1326@nbcs.rutgers.edu> 15.1.15-2.ru6
- Removed config options from install.

* Thu Jan 15 2015 Jungsoo Park <jp1326@nbcs.rutgers.edu> 15.1.15-1.ru6
- Initial build.
