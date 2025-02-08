#!/bin/bash
yum --disablerepo="*" --enablerepo=copr:copr.fedorainfracloud.org:averbyts:HEPrpmsSUSE list available | grep copr: | tr -s ' ' | tr ' ' ':' > 1.txt
yum list installed | grep copr:copr.fedorainfracloud.org:averbyts:HEPrpmsSUSE | tr -s ' ' | tr ' ' ':' >> 1.txt

(for a in $(cat 1.txt | grep -v \.src | sort ); do 
 v=$(echo $a| cut -f 2 -d: | cut -f 1 -d\-); 
 p=$(echo $a| cut -f 1 -d: | cut -f 1 -d.); 
 echo $p':'$v; 
 done) > 2.txt
 
 (for a in $(cat 2.txt | grep -v 'debug'); do 
 v=$(echo $a| cut -f 2 -d: | cut -f 1 -d\-); 
 p=$(echo $a| cut -f 1 -d: | cut -f 1 -d.); 
 echo 'Requires:' $p' == '$v; 
 done)
 
exit
L="|XXX |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/XXX/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpms/package/XXX/)  |[![Copr build status](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpmsSUSE/package/XXX/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/averbyts/HEPrpmsSUSE/package/XXX/)   |"

for a in $(copr list-package-names averbyts/HEPrpmsSUSE | sort); do echo $(echo $L | sed 's/XXX/'$a'/g') ; done 
