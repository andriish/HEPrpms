#! /bin/sh

# This file can be modified by user to provide local path or afs path to
# different platofrm or version of HepMC, Pythia8 and ROOT.

# Safeguard test if running with /afs paths.
if test ! -d /afs/cern.ch/sw/lcg; then
	echo ""
	echo "This script is for use on CERN server only."
	echo ""
	exit 0
fi

# Prevents user to modify these paths by using e.g.
# ./configure --with-HepMC=<path>
export AFS_PATHS=yes

export HEPMCLOCATION=/afs/cern.ch/sw/lcg/external/HepMC/2.03.09/slc4_amd64_gcc34
export PYTHIALOCATION=/afs/cern.ch/sw/lcg/external/MCGenerators/pythia8/135/slc4_amd64_gcc34

# Path to MC-Tester in afs compatible with above HepMC and Pythia8
# export MCTESTERLOCATION=/afs/cern.ch/sw/lcg/external/MCGenerators/mctester/1.23.1/slc4_amd64_gcc34
export PYTHIA8DATA=$PYTHIALOCATION/xmldoc

export PATH=/afs/cern.ch/sw/lcg/app/releases/ROOT/5.22.00/slc4_amd64_gcc34/root/bin:$PATH

ROOTLIBPATH=`root-config --libdir`
HERE=`(cd .. && pwd)`

export LD_LIBRARY_PATH=$HERE/lib:$ROOTLIBPATH:$HEPMCLOCATION/lib:$PYTHIALOCATION/lib:$LD_LIBRARY_PATH

echo ""
echo "Paths for HepMC, Pythia8 and ROOT set to AFS software"
echo ""
