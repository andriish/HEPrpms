#!/bin/bash
declare -a BUILDLIST=(
apfel:3.1.1
applgrid:1.6.36
ariadne:4.12
binder:1.4.2
blackhat:0.9.9
cascade:3.3.3
cernlib:2025.04.04.0
chaplin:1.2
clhep:2.4.7.1
COCOA:0.1.1
collier:1.2.8
cuba:4.2.2
Delphes:3.5.1pre12
EvtGen:2.2.3
f90cache:0.99c
fastjet:3.4.3
fastnlo:2.5.0.2826
FeynHiggs:2.19.0
fjcontrib:1.100
form:4.3.1
geant4:11.3.0
ggvvamp:1.0
golem95:1.3.4
gosam-contrib:2.0.20200904
gosam:2.1.2
HepPDT:3.04.01
Herwig:7.3.0
HJets:1.3
hoppet:1.2.0
hztool:4.3.2
iminuit:2.21.3
JetVHeto:3.0.0
LCIO:02.22.02
lhapdf-sets-Herwig:7.1.6
lhapdf-sets-whizard:2.8.3
LoopTools:2.16
MCFM:10.3
MC-TESTER:1.25.1
MG5_aMC:3.6.3
njet:2.1.1
nlojet++:4.1.3
nlox:1.2.1
noweb:2.13
openloops:2.1.4
PHOTOS:3.64
Professor:2.5.0
PTL:2.3.3
pythia6:6.4.28
python-uproot4:4.3.3
qcdloop:2.1.0
qcdnum:18.00.00
qgraf:3.6.7
qqvvamp:1.1
rapgap:3.4.0
recola2-SM:2.2.3
recola2:2.2.5
recola:1.4.4
Rivet:4.0.2
SHERPA-MC:3.0.1
TAUOLA:1.1.8
TheP8I:2.0.3
ThePEG:2.3.0
tmdlib:2.2.11
topdrawer:1.4e
ugs:2.10e
VBFNLO:3.0.0beta5
whizard:3.1.6
YODA:2.0.3
qd:2.3.24
ginac:1.8.9
imake:1.0.10
hevea:2.32
highfive:2.3.1
pythia8:8.3.12
)
mkdir -p log
for a in "${BUILDLIST[@]}" 
do
export name=$(echo $a | cut -f1 -d: )
export version=$(echo $a | cut -f2 -d: )
envsubst <<EOF > temp.sh
#!/bin/bash
git clone --depth 1 https://github.com/andriish/HEPrpms.git -b epel8
cd HEPrpms
sh srpmsbuild.sh  $name $version
EOF
copr add-package-custom HEPrpmsepel8 \
        --name $name \
        --script temp.sh \
        --script-resultdir HEPrpms/$name/$version/rpmbuild/SOURCES/ \
        --script-builddeps 'git rpmdevtools wget' \
        --script-chroot epel-8-x86_64
mv temp.sh log/$name$version
done
