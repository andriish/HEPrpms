#!/bin/bash
declare -a BUILDLIST=(
apfel:3.1.1
applgrid:1.6.36
ariadne:4.12
cascade:3.3.3
COCOA:0.1.1
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
golem95:1.3.4
HepPDT:3.04.01
Herwig:7.3.0
HJets:1.3
iminuit:2.21.1
JetVHeto:3.0.0
nlojet++:4.1.3
noweb:2.13
openloops:2.1.4
Professor:2.5.0
python3-uproot4:4.3.3
qcdloop:2.1.0
qcdnum:18.00.0
rapgap:3.4.0
recola2-SM:2.2.3
recola2:2.2.5
recola:1.4.4
Rivet:4.0.2
SHERPA-MC:3.0.1
TheP8I:2.0.3
ThePEG:2.3.0
tmdlib:2.2.11
topdrawer:1.4e
ugs:2.10e
whizard:3.1.6
YODA:2.0.3
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
        --script-chroot epel8
mv temp.sh log/$name$version
done
