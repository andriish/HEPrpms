
declare -a BATCH1=(
FeynHiggs
HepPDT
JetVHeto
LCIO
LoopTools
MC-TESTER
MCFM
MG5_aMC
PHOTOS
PTL
cernlib
chaplin
clhep
topdrawer
ugs
lhapdf-sets-Herwig
lhapdf-sets-whizard
gosam-contrib
YODA
applgrid
ariadne
binder
blackhat
cuba
f90cache
fastjet
nlojet++
noweb
openloops
pythia6
python-uproot4
qcdloop
qcdnum
qgraf
njet
tmdlib
form
collier
hoppet
golem95
VBFNLO
iminuit
)

declare -a BATCH2=(
geant4
TAUOLA
fjcontrib
gosam
Professor
fastnlo
hztool
BAT
)


declare -a BATCH3=(
EvtGen
Rivet
Delphes
recola2
recola2-SM
recola
)


declare -a BATCH4=(
rapgap
cascade
ThePEG
SHERPA-MC
whizard
)


declare -a BATCH5=(
TheP8I
Herwig
)

declare -a BATCH6=(
HJets
)

for a in "${BATCH1[@]}" 
do
(copr-cli build-package --name  $p averbyts/HEPrpms &)
done
wait $(jobs -p)

for a in "${BATCH2[@]}" 
do
(copr-cli build-package --name  $p averbyts/HEPrpms &)
done
wait $(jobs -p)


for a in "${BATCH3[@]}" 
do
(copr-cli build-package --name  $p averbyts/HEPrpms &)
done
wait $(jobs -p)

for a in "${BATCH4[@]}" 
do
(copr-cli build-package --name  $p averbyts/HEPrpms &)
done
wait $(jobs -p)


for a in "${BATCH5[@]}" 
do
(copr-cli build-package --name  $p averbyts/HEPrpms &)
done
wait $(jobs -p)

for a in "${BATCH6[@]}" 
do
(copr-cli build-package --name  $p averbyts/HEPrpms &)
done
wait $(jobs -p)




