#!bin/bash

export PATH=$PATH:$(pwd)

set -x 
declare -a BUILDLIST=(
#apfel:3.0.4
#applgrid:1.5.46
#geant4:10.07.p01
MC-TESTER:1.25.1
#LCIO:02.16.01
#DD4hep:01.16.01
#ariadne:4.12
#BAT:1.0.0
#binder:1.1.0
#blackhat:0.9.9
#cascade:3.0.01-beta01  
#cascade:3.1.01
#cernlib:2006
#CGAL:5.2
#chaplin:1.2
#clhep:2.4.4.1
#collier:1.2.5
#cuba:4.2.1
#Delphes:3.4.3pre10
#EvtGen:2.0.0
#fastjet:3.3.4  
#cascade:3.4.0
#fastnlo:2.3.1.2585
#FeynHiggs:2.18.0
#fjcontrib:1.045
#form:4.2.1
#geant4:10.07.p01
#ginac:1.8.0
#golem95:1.3.3
#gosam:2.1.0
#gosam-contrib:2.0.20200904
#HepPDT:3.04.01
#Herwig:7.2.2
#hevea:2.32  
#cascade:2.35
#HJets:1.3
#hoppet:1.2.0
#hztool:4.3.2
#iminuit:1.3.6  
#cascade:2.4.0
#JetVHeto:3.0.0
#LCIO:02.16.01
#lhapdf-sets-Herwig:7.1.6
#lhapdf-sets-whizard:2.8.3
#LoopTools:2.15
#MCFM:10.0  
#cascade:9.1
#MC-TESTER:1.25.0
#MG5_aMC:2.9.2
#njet:2.1.1
#nlojet++:4.1.3
#noweb:2.11
#openloops:2.1.2
#PHOTOS:3.64
#Professor:2.3.3
#PTL:1.0.2
#pythia6:6.4.28
#python-uproot4:4.0.6
#qcdloop:2.0.6
#qcdnum:17.01.16
#qd:2.3.22
#qgraf:3.4.2
#rapgap:3.303
#recola:1.4.1
#recola2:2.2.2  
#cascade:2.2.3
#Rivet:3.1.4
#SHERPA-MC:2.2.11
#superchic:4.01
#TAUOLA:1.1.8
#TheP8I:2.0.1
#ThePEG:2.2.2
#tmdlib:2.2.0  
#cascade:2.2.01
#topdrawer:1.4e
#ugs:2.10e
#VBFNLO:3.0.0beta5
#whizard:2.8.5
#xbae:4.60.4
#YODA:1.9.0
)

for a in "${BUILDLIST[@]}" 
do
p=$(echo $a | cut -f1 -d: )
v=$(echo $a | cut -f2 -d: )
(sh srpmsbuild.sh $p $v --build &)
done
wait
exit
