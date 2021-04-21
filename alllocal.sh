#!bin/bash

export PATH=$PATH:$(pwd)

set -x 
declare -a BUILDLIST=(
#Rivet:3.1.4
MC-TESTER:1.26.0
#rapgap:3.303
#cascade:3.1.01
#VBFNLO:3.0.0beta5
#iminuit:2.4.0
#MCFM:10.0
#ugs:2.10e
#Professor:2.3.3
#njet:2.1.1
#MCFM:10.0
#binder:1.1.0
#topdrawer:1.4e
#rapgap:3.303
#recola:1.4.1
#LoopTools:2.15
#hztool:4.3.2
#hoppet:1.2.0
#ariadne:4.12
#njet:2.0.0
#Rivet:3.1.3
#PHOTOS:3.64
#TAUOLA:1.1.8
#tmdlib:2.2.0
#vincia:2.3.02
#pythia6:6.4.28
#LoopTools:2.15
#JetVHeto:3.0.0
#hztool:4.3.2#
#hoppet:1.2.0
#fjcontrib:1.045
#gosam:2.1.0
#cascade:3.0.01-beta01
#clhep:2.4.4.1
#binder:1.1.0
#applgrid:1.5.46
#ariadne:4.12
#EvtGen:2.0.0
#DIRE:2.004
#gosam-contrib:2.0.20200904
#lfortran:0.10.0
#Herwig:7.2.2
#gosam:2.1.0
#YODA:1.9.0
#cascade:3.0.01-beta01
#rapgap:3.303
#hevea:2.32
#fastnlo:2.3.1.2585
#iminuit:1.3.6
#Professor:2.3.2
#recola2:2.2.2
#SHERPA-MC:2.2.11
#hztool:4.3.2
#blackhat:0.9.9
#Herwig:7.2.2
#whizard:2.8.5
#clhep:2.4.4.1
#rapgap:3.303
#lhapdf-sets-whizard:2.8.3
#noweb:2.11
#whizard:2.8.5
#geant4:10.07.p01
#FeynHiggs:2.18.0
#apfel:3.0.4
#DD4hep:01.15
#TheP8I:2.0.1
#qcdnum:17.01.16
#njet:2.0.0
#cuba:4.2.1
#ariadne:4.12
#hoppet:1.2.0
#form:4.2.1
)

for a in "${BUILDLIST[@]}" 
do
p=$(echo $a | cut -f1 -d: )
v=$(echo $a | cut -f2 -d: )
(sh srpmsbuild.sh $p $v --build &)
done
wait
exit
