# HEPrpms

HEPrpms is a collection of .spec files that enable creation of RPM packages for some widely used the HEP software.
The binary packages build from HEPrpms spec files are available at

HEPrpms https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/

and 

HEPrpmsSUSE https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpmsSUSE/

repositories. The `HEPrpmsSUSE` packages are also built in the openSUSE Build Service project
https://build.opensuse.org/project/show/home:averbyts:HEPrpmsSUSE (see below). Most of the packages in these repositories are related to the High Energy Physics 
experiment/theory/phenomenology.

As of November 2026, the packages are built for Fedora 43, Fedora 44, Fedora 45, Fedora rawhide, CentOS/RHEL-compatible (EPEL) 9 and 10,
and openSUSE Tumbleweed (x86_64). A few packages (`OpenPHIGS`, `cernlib`) are also built for Fedora i686 (`i386` chroots), and `cernlib` also for Fedora aarch64.
EPEL 8 is no longer built. The packages are built on the COPR https://copr.fedorainfracloud.org/ service
(and, for openSUSE, in OBS), however, other build platforms can be used as well.

## Citing

The approach, the motivation and the tests of the packages on openSUSE, RHEL clones and Fedora are described in

T. Hahn, A. Verbytskyi, *Deployment of High Energy Physics software with a standard method*,
J. Phys. Conf. Ser. 2438 (2023) 012024, [arXiv:2210.17261](https://arxiv.org/abs/2210.17261),
[DOI:10.1088/1742-6596/2438/1/012024](https://doi.org/10.1088/1742-6596/2438/1/012024),
[INSPIRE](https://inspirehep.net/literature/2173441).

The idea is to base the deployment on the RPM package manager, so that precompiled HEP software can be installed without detailed
Linux system knowledge and is updated together with the system by the usual update process. Please cite this paper if you use HEPrpms.

|       |HEPrpms    |HEPrpmsSUSE (OBS)|
|-------|-----------|-----------|
|apfel |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/apfel/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/apfel/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/apfel/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/apfel) |
|applgrid |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/applgrid/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/applgrid/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/applgrid/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/applgrid) |
|ariadne |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/ariadne/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/ariadne/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/ariadne/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/ariadne) |
|binder |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/binder/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/binder/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/binder/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/binder) |
|blackhat |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/blackhat/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/blackhat/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/blackhat/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/blackhat) |
|cascade |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/cascade/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/cascade/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/cascade/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/cascade) |
|cernlib |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/cernlib/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/cernlib/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/cernlib/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/cernlib) |
|chaplin |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/chaplin/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/chaplin/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/chaplin/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/chaplin) |
|clhep |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/clhep/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/clhep/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/clhep/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/clhep) |
|COCOA |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/COCOA/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/COCOA/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/COCOA/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/COCOA) |
|collier |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/collier/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/collier/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/collier/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/collier) |
|cuba |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/cuba/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/cuba/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/cuba/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/cuba) |
|Delphes |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/Delphes/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/Delphes/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/Delphes/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/Delphes) |
|EvtGen |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/EvtGen/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/EvtGen/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/EvtGen/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/EvtGen) |
|f2c |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/f2c/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/f2c/) | |
|f90cache |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/f90cache/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/f90cache/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/f90cache/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/f90cache) |
|fastjet |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/fastjet/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/fastjet/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/fastjet/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/fastjet) |
|fastnlo |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/fastnlo/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/fastnlo/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/fastnlo/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/fastnlo) |
|FeynHiggs |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/FeynHiggs/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/FeynHiggs/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/FeynHiggs/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/FeynHiggs) |
|fjcontrib |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/fjcontrib/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/fjcontrib/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/fjcontrib/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/fjcontrib) |
|form |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/form/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/form/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/form/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/form) |
|geant4 |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/geant4/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/geant4/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/geant4/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/geant4) |
|ggvvamp |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/ggvvamp/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/ggvvamp/) | |
|ginac |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/ginac/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/ginac/) | |
|golem95 |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/golem95/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/golem95/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/golem95/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/golem95) |
|gosam |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/gosam/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/gosam/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/gosam/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/gosam) |
|gosam-contrib |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/gosam-contrib/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/gosam-contrib/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/gosam-contrib/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/gosam-contrib) |
|HepPDT |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/HepPDT/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/HepPDT/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/HepPDT/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/HepPDT) |
|Herwig |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/Herwig/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/Herwig/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/Herwig/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/Herwig) |
|hevea |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/hevea/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/hevea/) | |
|highfive |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/highfive/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/highfive/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/highfive/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/highfive) |
|HJets |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/HJets/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/HJets/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/HJets/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/HJets) |
|hoppet |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/hoppet/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/hoppet/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/hoppet/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/hoppet) |
|hztool |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/hztool/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/hztool/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/hztool/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/hztool) |
|iminuit |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/iminuit/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/iminuit/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/iminuit/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/iminuit) |
|JADE |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/JADE/package/JADE/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/JADE/package/JADE/) | |
|JetVHeto |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/JetVHeto/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/JetVHeto/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/JetVHeto/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/JetVHeto) |
|LCIO |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/LCIO/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/LCIO/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/LCIO/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/LCIO) |
|lhapdf-sets-Herwig |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/lhapdf-sets-Herwig/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/lhapdf-sets-Herwig/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/lhapdf-sets-Herwig/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/lhapdf-sets-Herwig) |
|lhapdf-sets-whizard |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/lhapdf-sets-whizard/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/lhapdf-sets-whizard/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/lhapdf-sets-whizard/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/lhapdf-sets-whizard) |
|LoopTools |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/LoopTools/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/LoopTools/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/LoopTools/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/LoopTools) |
|MC-TESTER |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/MC-TESTER/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/MC-TESTER/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/MC-TESTER/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/MC-TESTER) |
|MCFM |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/MCFM/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/MCFM/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/MCFM/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/MCFM) |
|MG5_aMC |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/MG5_aMC/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/MG5_aMC/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/MG5_aMC/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/MG5_aMC) |
|njet |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/njet/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/njet/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/njet/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/njet) |
|nlojet++ |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/nlojet++/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/nlojet++/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/nlojet++/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/nlojet++) |
|nlox |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/nlox/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/nlox/) | |
|noweb |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/noweb/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/noweb/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/noweb/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/noweb) |
|openloops |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/openloops/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/openloops/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/openloops/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/openloops) |
|OpenPHIGS |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/OpenPHIGS/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/OpenPHIGS/) | |
|PHOTOS |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/PHOTOS/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/PHOTOS/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/PHOTOS/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/PHOTOS) |
|Professor |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/Professor/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/Professor/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/Professor/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/Professor) |
|PTL |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/PTL/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/PTL/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/PTL/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/PTL) |
|pythia6 |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/pythia6/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/pythia6/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/pythia6/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/pythia6) |
|python-mpmath |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/python-mpmath/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/python-mpmath/) | |
|python-uproot4 |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/python-uproot4/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/python-uproot4/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/python-uproot4/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/python-uproot4) |
|qcdloop |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/qcdloop/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/qcdloop/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/qcdloop/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/qcdloop) |
|qcdnum |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/qcdnum/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/qcdnum/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/qcdnum/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/qcdnum) |
|qd |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/qd/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/qd/) | |
|qgraf |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/qgraf/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/qgraf/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/qgraf/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/qgraf) |
|qqvvamp |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/qqvvamp/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/qqvvamp/) | |
|rapgap |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/rapgap/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/rapgap/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/rapgap/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/rapgap) |
|recola |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/recola/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/recola/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/recola/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/recola) |
|recola2 |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/recola2/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/recola2/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/recola2/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/recola2) |
|recola2-SM |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/recola2-SM/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/recola2-SM/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/recola2-SM/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/recola2-SM) |
|Rivet |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/Rivet/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/Rivet/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/Rivet/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/Rivet) |
|SHERPA-MC |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/SHERPA-MC/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/SHERPA-MC/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/SHERPA-MC/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/SHERPA-MC) |
|sympy |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/sympy/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/sympy/) | |
|TAUOLA |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/TAUOLA/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/TAUOLA/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/TAUOLA/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/TAUOLA) |
|TheP8I |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/TheP8I/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/TheP8I/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/TheP8I/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/TheP8I) |
|ThePEG |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/ThePEG/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/ThePEG/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/ThePEG/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/ThePEG) |
|tmdlib |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/tmdlib/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/tmdlib/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/tmdlib/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/tmdlib) |
|topdrawer |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/topdrawer/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/topdrawer/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/topdrawer/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/topdrawer) |
|ugs |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/ugs/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/ugs/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/ugs/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/ugs) |
|VBFNLO |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/VBFNLO/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/VBFNLO/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/VBFNLO/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/VBFNLO) |
|whizard |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/whizard/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/whizard/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/whizard/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/whizard) |
|xbae | |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/xbae/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/xbae) |
|YODA |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/YODA/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/YODA/) |[![OBS build status](https://build.opensuse.org/projects/home:averbyts:HEPrpmsSUSE/packages/YODA/badge.svg?repository=openSUSE_Tumbleweed&architecture=x86_64&type=default)](https://build.opensuse.org/package/show/home:averbyts:HEPrpmsSUSE/YODA) |

## Using the software repositories
To enable and use the software repositories on 
### CentOS/RHEL 9/10 (Alma, Rocky, ...) and other RH-compatible distros:

 - Install EPEL (as root)
   ```
   dnf install epel-release
   ```
   (on RHEL itself, and on CRB-dependent builds, also enable CodeReady Builder: `crb enable`)
 - Install the `copr` plugin for `dnf` and enable the HEPrpms (as root)
   ```
   dnf install 'dnf-command(copr)'
   dnf copr enable averbyts/HEPrpms
   ```
 - Run dnf to install the needed software, e.g. (as root)
   ``` 
   dnf install Herwig 
   ```
There are no known conflicts between the EPEL/CentOS and HEPrpms repositories.
However, if needed, the software can be black/whitelisted in /etc/yum.repos.d/*.repo files using the
`exclude=` or `installonlypkgs=` directives. 
See https://access.redhat.com/documentation/en-us/red_hat_enterprise_linux/7/html/system_administrators_guide/ch-yum 
for details.

### Fedora:

 - Enable HEPrpms with `dnf`   (as root)
   ```
   dnf copr enable averbyts/HEPrpms 
   ```
 - Run dnf to install the needed software, e.g. (as root)
   ``` 
   dnf install Herwig 
   ```
There are no known conflicts between the Fedora and HEPrpms repositories. However `dnf` supports the same functionality as `yum` 
and black/whitelisting is possible, see https://docs.fedoraproject.org/en-US/quick-docs/dnf/ for details.

### SUSE Tumbleweed, from the openSUSE Build Service (OBS):
The packages of `HEPrpmsSUSE` are also built in the OBS project `home:averbyts:HEPrpmsSUSE`
(https://build.opensuse.org/project/show/home:averbyts:HEPrpmsSUSE) for openSUSE Tumbleweed (x86_64).
The OBS packages are built from the same spec files in this repository, in the versions of the `HEPrpms` COPR project.
Use either the COPR repository below or the OBS repository, not both.
 - Add the `science` repository (as root), the OBS project builds against it and needs some of its packages (e.g. `root6`)
   ```
   zypper addrepo --refresh https://download.opensuse.org/repositories/science/openSUSE_Tumbleweed/science.repo
   ```
 - Add the OBS repository and import its signing key (as root)
   ```
   zypper addrepo --refresh https://download.opensuse.org/repositories/home:/averbyts:/HEPrpmsSUSE/openSUSE_Tumbleweed/home:averbyts:HEPrpmsSUSE.repo
   zypper --gpg-auto-import-keys refresh
   ```
 - Install the needed software with (as root)
   ```
   zypper install Herwig
   ```
If the `HEPrpmsSUSE` COPR repository was added before, disable or remove it (as root) so that the packages come only from OBS.
The repository alias is the name in the first line of the repository file, e.g. `copr:copr.fedorainfracloud.org:averbyts:HEPrpmsSUSE`:
```
zypper lr -u | grep -i HEPrpmsSUSE
zypper modifyrepo --disable <alias>     # keep the repository but do not use it
zypper removerepo <alias>               # or remove it completely
```
Installed packages stay as they are; to switch them to the OBS builds run (after disabling the COPR repository)
```
zypper lr                                              # find the alias or number of the OBS repository, e.g. HEPrpmsSUSE
zypper dup --from <alias-or-number> --allow-vendor-change
```
`--from` takes the alias, name or number shown by `zypper lr`, not the OBS project name. `--allow-vendor-change` is needed because
the packages change the vendor from `Fedora Copr - user averbyts` to `obs://build.opensuse.org/home:averbyts`. To make zypper
accept this change without the option, create `/etc/zypp/vendors.d/heprpms.conf` (as root) with
```
[main]
vendors = obs://build.opensuse.org/home:averbyts,Fedora Copr - user averbyts
```
The package locks added for the COPR repository can stay.
To enable the COPR repository again use `zypper modifyrepo --enable <alias>`.

The package locks described for the `HEPrpmsSUSE` COPR repository below apply to the OBS repository as well.
Not every package builds in OBS yet; see the build state at the project page above or with
`osc results home:averbyts:HEPrpmsSUSE`.
Builds in OBS have no network access, therefore some data that Fedora/EPEL builds download at build time is provided differently:
the OpenLoops process libraries are not downloaded (only the `eett` process library is installed, so anything that needs other
OpenLoops processes, e.g. Sherpa or Herwig with OpenLoops, does not work), while the Geant4 datasets and the LHAPDF sets of
`lhapdf-sets-Herwig` and `lhapdf-sets-whizard` are included in the packages.

### SUSE Tumbleweed, from COPR (duplicate of the OBS packages above, alternative):
The `HEPrpmsSUSE` COPR repository contains the same packages as the OBS repository described above. Use either one, not both; the OBS repository is the preferred one.

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

## Using HEPrpms in container images

The repositories can be used in container images (Docker, Podman, Apptainer/Singularity, CI runners, ...), which is convenient for
reproducible analysis environments, CI and for HPC sites where root is not available at run time. There are several options.
The commands below use `podman`; `docker` works the same way.

### Option 1: build an image from a Containerfile/Dockerfile

Fedora:
```
FROM registry.fedoraproject.org/fedora:44
RUN dnf -y install 'dnf-command(copr)' && dnf -y copr enable averbyts/HEPrpms \
 && dnf -y install Herwig Rivet fastjet-devel && dnf clean all
```
AlmaLinux/Rocky 9 or 10 (EPEL 9/10 chroots):
```
FROM quay.io/almalinuxorg/almalinux:9
RUN dnf -y install epel-release && crb enable && dnf -y install 'dnf-command(copr)' \
 && dnf -y copr enable averbyts/HEPrpms && dnf -y install SHERPA-MC && dnf clean all
```
openSUSE Tumbleweed (COPR `HEPrpmsSUSE`, see the locks above; the `$basearch` has to be replaced):
```
FROM registry.opensuse.org/opensuse/tumbleweed
RUN zypper -n ar https://download.opensuse.org/repositories/science/openSUSE_Tumbleweed/science.repo \
 && curl -o /etc/zypp/repos.d/HEPrpmsSUSE.repo https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpmsSUSE/repo/opensuse-tumbleweed/averbyts-HEPrpmsSUSE-opensuse-tumbleweed.repo \
 && sed -i 's/$basearch/x86_64/g' /etc/zypp/repos.d/HEPrpmsSUSE.repo \
 && zypper -n --gpg-auto-import-keys install Herwig && zypper clean -a
```
The OBS repository (see above) can be used instead of the COPR one on openSUSE.
Build with `podman build -t myhep .`. Put all `dnf`/`zypper` calls into one `RUN` and clean the caches to keep the image small.
Pin the base image tag, and note that COPR keeps only the latest builds, so for strict reproducibility build the image once and store it in a registry.

### Option 2: use a pre-built distribution image interactively

```
podman run -it --rm registry.fedoraproject.org/fedora:44 bash -c \
  "dnf -y install 'dnf-command(copr)' && dnf -y copr enable averbyts/HEPrpms && dnf -y install Rivet && exec bash"
```
The installation is lost when the container exits; mount a volume (`-v $PWD:/work`) for the data.

### Option 3: Apptainer/Singularity (HPC, no root)

Convert the image above or build directly from a definition file:
```
Bootstrap: docker
From: quay.io/almalinuxorg/almalinux:9
%post
    dnf -y install epel-release && crb enable
    dnf -y install 'dnf-command(copr)' && dnf -y copr enable averbyts/HEPrpms
    dnf -y install Herwig Rivet && dnf clean all
```
`apptainer build hep.sif hep.def` (use `--fakeroot` without root); then `apptainer exec hep.sif rivet --version`.
Alternatively, `apptainer build hep.sif docker-daemon://myhep:latest` converts a local Docker image.

### Option 4: mock chroots and rootless unpacking

For building or testing against the packages without a full container, `mock` can use the COPR repository, e.g.
`mock -r fedora-rawhide-x86_64 --enable-network --addrepo https://download.copr.fedorainfracloud.org/results/averbyts/HEPrpms/fedora-rawhide-x86_64/ --install Herwig`.
Without any privileges, the RPMs can be unpacked into a directory as described in the section above.

### Notes

 - The packages are built for x86_64 (only `OpenPHIGS` and `cernlib` also for i686, and `cernlib` also for aarch64); other architectures are not available.
 - On openSUSE the OpenLoops process libraries are not included (see above).
 - The `HEPrpms` meta package installs the full collection; it is large, so install only what is needed in images.
 - Packages install into the standard system prefix, so normally no `PATH`/`LD_LIBRARY_PATH` setup is needed.

## Using the software repositories w/o root privileges

Installation of some packages in user mode is also possible, however, it is a bit more complicated and is, basically, requires copying and unpacking the 
RPM packages into a specific directory.

For RHEL 9/10-compatible systems, the process can be simplified a bit using the `yumdownloader` script (`dnf download --resolve` works as well).

 - Copy the system `yum.conf`  into some user directory.   
   ```
   cp /etc/yum.conf ./myyum.conf
   ```
 - Get the .repo file for HEPrpms
   ```
    wget https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/repo/epel-9/averbyts-HEPrpms-epel-9.repo
   ```
 -  Append averbyts-HEPrpms-epel-9.repo to the to the end of the myyum.conf:
   ```
   cat averbyts-HEPrpms-epel-9.repo >> myyum.conf
   ```
 - Download the needed packages with yumdownloader command. Note: the dependencies will be downloaded as well.
   ```
   yumdownloader  --resolve  --downloadonly --config=./myyum.conf  --downloaddir=/my/download/directory    SHERPA-MC
   ```
 -  Extract the content of downloaded RPMs, e.g.
   ```
    rpm2cpio /my/download/directory/SHERPA-MC-<version>.el9.x86_64.rpm | (cd /place/where/you/want/to/install/it && cpio -idmv)
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
