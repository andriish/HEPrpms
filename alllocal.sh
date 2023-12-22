#!bin/bash

export PATH=$PATH:$(pwd)

#set -x 
declare -a BUILDLIST=(
#OK apfel:3.1.1
#OK cascade:3.3.3
#OK binder:1.4.0
#OK cernlib:2023.10.31.0
#OK geant4:11.2.0
iminuit:2.23.0
#OK LCIO:02.20.02
pybind11:2.11.1
#qd ?
#uproot?
#hevea?
#CGAL?
#hoppet!
#OK YODA:1.9.9
Rivet:3.1.9
#apfel:3.0.7
#apfel:3.1.0
#binder:1.3.0
#ThePEG:2.3.0
#Herwig:7.3.0
#geant4:11.1.3
#COCOA:0.1.1
#whizard:3.1.4

#noweb:2.13
#SHERPA-MC:2.2.15

#Delphes:3.5.1pre10

#cernlib:2023.08.14.0
#applgrid:1.6.35
#collier:1.2.8
#Professor:2.4.1
#recola:1.4.4
#EvtGen:2.2.1
#whizard:3.1.3

#form:4.3.1
#geant4:11.1.2
#tmdlib:2.2.10

#fastjet:3.4.2
#clhep:2.4.7.1
#fjcontrib:1.053
#LCIO:02.20.00
#LoopTools:2.16
#cernlib:2023.08.14.0
#ginac:1.8.7
#TheP8I:2.0.3
#openloops:2.1.2
#whizard:3.1.2
#Rivet:3.1.8
#YODA:1.9.8
#applgrid:1.6.32
#MG5_aMC:3.4.2
#gosam:2.1.1
#qd:2.3.23
#recola2:2.2.4
#geant4:11.1.1
#HEJ:2.1.3
#njet:3.1.1
#collier:1.2.7
#cascade:3.3.1
#MCFM:10.3
#clhep:2.4.6.4
#blackhat:0.9.9
#binder:1.3.0
#SHERPA-MC:2.2.15
#FeynHiggs:2.19.0
#f90cache:0.99c
#tmdlib:2.2.08
#whizard:3.1.0
#geant4:11.1.1
#fjcontrib:1.051
#form:4.3.0
#ginac:1.8.6
#LCIO:02.19.01
#PTL:2.3.3
#qcdnum:18.00.00
#lhapdf:6.5.3
#rapgap:3.4.0
#cascade:3.3.0
#Rivet:3.1.7
#Herwig:7.2.3
#iminuit:2.21.3
#cascade:3.2.1
#qgraf:3.4.2
#YODA:1.9.7
#whizard:3.0.3
#MG5_aMC:3.5.1
#MCFM:10.2.1
#f2c:20210928
#DD4hep:01.20.00
#MG5_aMC:2.9.11
#qcdloop:2.0.9
#TheP8I:2.0.2
#applgrid:1.6.27
#python-uproot4:4.3.3
#FeynHiggs:2.18.1
#tmdlib:2.2.06
#fjcontrib:1.049
#EvtGen:2.2.0
#cuba:4.2.2
#clhep:2.4.5.3
#recola2-SM:2.2.3
#recola2:2.2.4
#Herwig:7.2.3
#LCIO:02.17.01
#ThePEG:2.2.3
#Rivet:3.1.6
#YODA:1.9.5
#whizard:3.0.3
#ginac:1.8.3
#TheP8I:2.0.2
#geant4:11.0.0
#binder:1.2.0
#SHERPA-MC:2.2.12
#SHERPA-MC:3.0.0alpha1
#qcdnum:17.01.83
#MCFM:10.0.1
#iminuit:2.8.4
#DD4hep:01.18.00
#applgrid:1.6.22
#python-uproot4:4.1.8
#fastnlo:2.5.0.2826
#LCIO:02.17.00
#gosam:2.1.1
#clhep:2.4.5.1
#Rivet:3.1.5
#YODA:1.9.2
#LoopTools:2.16
#Herwig:7.2.2
#BAT:1.0.0
#qcdnum:17.01.82
#qcdloop:2.0.8
#ginac:1.8.1
#pythia6:6.4.28
#PTL:2.0.0
#DD4hep:01.17.00
#Rivet:3.1.4
#EvtGen:2.1.1
#geant4:10.07.p02
#qcdloop:2.0.8
#fastjet:3.4.0
#python-uproot4:4.0.11
#Delphes:3.5.0
#YODA:1.9.0
#apfel:3.0.4
#applgrid:1.5.46
#ariadne:4.12
#BAT:1.0.0
#binder:1.1.0
#blackhat:0.9.9
#cascade:3.0.01-beta01  
#cascade:3.1.01
#cernlib:2006
#CGAL:5.2
#chaplin:1.2
#clhep:2.4.4.2
#collier:1.2.5
#cuba:4.2.1
#Delphes:3.4.3pre10
#Delphes:3.4.3pre12
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
#iminuit:2.6.1  
#cascade:2.4.0
#JetVHeto:3.0.0
#LCIO:02.16.01
#lhapdf-sets-Herwig:7.1.6
#lhapdf-sets-whizard:2.8.3
#lhapdf-sets-whizard:3.0.0
#LoopTools:2.16
#MCFM:10.0  
#cascade:9.1
#MC-TESTER:1.25.0
#MG5_aMC:2.9.2
#MG5_aMC:2.9.4
#njet:2.1.1
#nlojet++:4.1.3
#noweb:2.11
#openloops:2.1.2
#PHOTOS:3.64
#Professor:2.4.0
#PTL:1.0.2
#pythia6:6.4.28
#python-uproot4:4.0.6
#python-uproot4:4.0.8
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
#whizard:3.0.0
#xbae:4.60.4
#YODA:1.9.0
)

for a in "${BUILDLIST[@]}" 
do
p=$(echo $a | cut -f1 -d: )
v=$(echo $a | cut -f2 -d: )
mkdir -p  logs
(sh srpmsbuild.sh $p $v --build  &> logs/$p$v".log" || echo "$p $v build failed" &)
done
wait
exit
