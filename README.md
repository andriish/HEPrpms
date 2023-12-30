# HEPrpms

HEPrpms is a collection of .spec files that enable creation of RPM packages for some widely used the HEP software.
The binary packages build from HEPrpms spec files are available at

HEPrpms https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/

and 

HEPrpmsSUSE https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpmsSUSE/

repositories. Most of the packages in these repositories are related to the High Energy Physics 
experiment/theory/phenomenology.

As of December 2023, the packages are built for Fedora 38, Fedora 39, Fedora rawhide, CentOS8, CentOS9 and OpenSUSE Tumbleweed.
The packages are built on COPR https://copr.fedorainfracloud.org/ service, 
however, other build platforms can be used as well.


|       |HEPrpms    |HEPrpmsSUSE|
|-------|-----------|-----------|
|binder |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/binder/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/binder/)  |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpmsSUSE/package/binder/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpmsSUSE/package/binder/)   |

## Using the software repositories
To enable and use the software repositories on 
### CentOS8/9 and other RH-compatible distros:

 - Install EPEL (as root)
   ```
   yum install epel-release
   ```
 - Install `copr` plugin for `yum` and enable the HEPrpms  (as root)
   ```
   yum install yum-plugin-copr
   yum copr enable averbyts/HEPrpms
   ```
 - Run yum to install the needed software, e.g. (as root)
   ``` 
   yum install Herwig 
   ```
There are no known conflicts between the EPEL/CentOS8 and HEPrpms repositories.
However, if needed, the software can be black/whitelisted in /etc/yum.repos.d/*.repo files using the
`exclude=` or `installonlypkgs=` directives. 
See https://access.redhat.com/documentation/en-us/red_hat_enterprise_linux/7/html/system_administrators_guide/ch-yum 
for details.

### Fedora:

 - Enable HEPrpms with `dnf`   (as root)
   ```
   dnf copr enable enable averbyts/HEPrpms 
   ```
 - Run yum to install the needed software, e.g. (as root)
   ``` 
   yum install Herwig 
   ```
There are no known conflicts between the Fedora and HEPrpms repositories. However `dnf` supports the same functionality as `yum` 
and black/whitelisting is possible, see https://docs.fedoraproject.org/en-US/quick-docs/dnf/ for details.

### SUSE Tumbleweed:
 - Add `science` repository (as root)
   ```
   zypper addrepo https://download.opensuse.org/repositories/science/openSUSE_Tumbleweed/science.repo
   ```
 - Download the repository file of `HEPrpmsSUSE`
   ```
   wget https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpmsSUSE/repo/opensuse-tumbleweed/averbyts-HEPrpmsSUSE-opensuse-tumbleweed.repo
   
   ```
 - Replace the `$basearch` in the repository with `x86_64`
 - Install the repository file (as root)
   ```
   zypper addrepo averbyts-HEPrpmsSUSE-opensuse-tumbleweed.repo 
   ```
 - Install the needed software with (as root)
   ```
   zypper install Herwig
   ```
`zypper` uses another mechanism than `yum` or `dnf` to blacklist/whitelist packages. To exclude some conflicting packages one has to use zypper locks.
Because the `science` repository has some packages with the same names as in `HEPrpmsSUSE`, 
it is recommended to add the following locks when using the  `HEPrpmsSUSE` repository:
```
zypper al -r science  *YODA*
zypper al -r science  *Rivet*
zypper al -r science  *Herwig*
zypper al -r science  *ThePEG*
zypper al -r science  *fastjet*
zypper al -r science  *fjcontrib*
zypper al -r science  *SHERPA*
zypper al -r science  *geant4*
zypper al -r science  PTL*
zypper al -r science  form*
zypper al -r oss  *ginac*
```

## Using the software repositories w/o root privileges

Installation of some packages in user mode is also possible, however, it is a bit more complicated and is, basically, requires copying and unpacking the 
RPM packages into a specific directory.

For RHEL8/9-compatible systems, the process can be simplified a bit using the `yumdownloader` script.

 - Copy the system `yum.conf`  into some user directory.   
   ```
   cp /etc/yum.conf ./myyum.conf
   ```
 - Get the .repo file for HEPrpms
   ```
    wget https://copr.fedorainfracloud.org/coprs/averbyts/fastjet/repo/epel-8/averbyts-HEPrpms-epel-8.repo
   ```
 -  Append averbyts-HEPrpms-epel-8.repo to the to the end of the myyum.conf:
   ```
   cat averbyts-HEPrpms-epel-8.repo >> myyum.conf
   ```
 - Download the needed packages with yumdownloader command. Note: the dependencies will be downloaded as well.
   ```
   yumdownloader  --resolve  --downloadonly --config=./myyum.conf  --downloaddir=/my/download/directory    SHERPA-MC
   ```
 -  Extract the content of downloaded RPMs, e.g.
   ```
    rpm2cpio /my/download/directory/SHERPA-MC-2.2.8-14.el8.x86_64.rpm  | cpio -idmv   /place/where/you/want/to/install/it
   ```
The usage of software installed in this way will require adjustments of $PATH, $LD_LIBRARY_PATH, and potentially other environment variables.




## Building the packages locally

It is possible to build packages locally if the following conditions are satisfied: 
 - all the build-time requirements for the specific package are installed
 - the system has installed `rpmdevtools` and  `wget` 
 - internet connection  is available
 
To  build a package named `mypackagename`, run from the top directory of the source code
```
sh srpmsbuild.sh mypackagename mypackageversion --build
```
The .spec file located in `mypackagename/mypackageversion`  will be parsed, the sources will be downloaded,
 source and binary packages will be built. If only SRPMS files are needed, the `--build` should be omitted.
The whole build process will happen in `mypackagename/mypackageversion/rpmbuild` directory 
so multiple parallel builds of different packages/versions are possible.
To build multiple packages in parallel, a wrapper script `alllocal.sh` can be used.

When invoked, `srpmsbuild.sh` script checks for the presence of file `do.sh` in the `mypackagename/mypackageversion`
 directory. If the file is present, it will be executed. This is a simple and reliable way to produce some small 
 patches or manipulate the sources.  


## Contributing to HEPrpms

If you have suggestions on how to improve the repositories please don't hesitate to create an issue or a merge request.
A lot of useful documentation on the RPM packaging can be found
https://docs.fedoraproject.org/en-US/packaging-guidelines/ 

## Dependencies and features

The packages are compiled with the maximal set of features. Some exceptions are listed at 
https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/

## Patches for the packages

For most packages the sources are taken from their upstream repositories and only some patches that allow for the builds are applied.
However some packages contain an extended functionality of a larger amount of added codes, or even created from scratch. 
See details at https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/.
All the patches are available in the repository.

## Creating own spin-off repositories in COPR

It is quite easy to create own repository in COPR using just the web GUI and build the packages using "custom method".
However, if there is a need, one can easily automate the process using the copr command-line client.
The script below demonstrates a way to add multiple packages to `myREPOSITORY` for `opensuse-tumbleweed-x86_64` using the command-line interface.
Please note that copr command-line utilities and copr access token are needed.
```
#!/bin/bash
declare -a BUILDLIST=( applgrid:1.5.46 YODA:1.8.5 )
mkdir -p log
for a in "${BUILDLIST[@]}" 
do
export name=$(echo $a | cut -f1 -d: )
export version=$(echo $a | cut -f2 -d: )
envsubst <<EOF > temp.sh
#!/bin/bash
git clone --depth 1 https://github.com/andriish/HEPspecs.git
cd HEPspecs
sh srpmsbuild.sh  $name $version
EOF
copr add-package-custom myREPOSITORY \
        --name $name \
        --script temp.sh \
        --script-resultdir HEPspecs/$name/$version/rpmbuild/SOURCES/ \
        --script-builddeps 'git rpmdevtools wget' \
        --script-chroot opensuse-tumbleweed-x86_64
mv temp.sh log/$name$version
done
```
