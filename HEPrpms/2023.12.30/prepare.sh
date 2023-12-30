#!/bin/bash
yum --disablerepo="*" --enablerepo=copr:copr.fedorainfracloud.org:averbyts:HEPrpms list available | grep copr: | tr -s ' ' | tr ' ' ':' > 1.txt

(for a in $(cat 1.txt | grep -v '.src'); do 
 v=$(echo $a| cut -f 2 -d: | cut -f 1 -d\-); 
 p=$(echo $a| cut -f 1 -d: | cut -f 1 -d.); 
 echo $p':'$v; 
 done) > 2.txt
 
 (for a in $(cat 2.txt | grep -v 'debug'); do 
 v=$(echo $a| cut -f 2 -d: | cut -f 1 -d\-); 
 p=$(echo $a| cut -f 1 -d: | cut -f 1 -d.); 
 echo 'Requires:' $p' == '$v; 
 done)