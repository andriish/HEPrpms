pkginclude_HEADERS = caseffic.inc  casluco.inc    caslujets.inc  caspara5.inc  caspart.inc casshwr.inc \
casgki.inc    casludat1.inc  casluparm.inc  caspara.inc   casprkt.inc casvalues.inc\
casglu.inc    casludat2.inc  casmult.inc    casparam.inc  dateqq.inc \
casherup.inc  casludat3.inc  caspara1.inc   casparas.inc  casscal.inc timeqq.inc casvers.inc \
caspsipol.inc caspar.inc     caslhe.inc     qcdnum.inc    PythiaHelper.h  HepMCSteering.inc Pythia6ToHepMC3.inc Pythia6ToHepMC2.inc RivetInterface.inc


all-local: test

test:
	if [ -f casvers.inc ]; then rm casvers.inc ; fi
	echo > casvers.inc "        VERSQQ = '$(VERSION)'"
	if [ -f casvers-tex.inc ]; then rm casvers-tex.inc ; fi
	echo > casvers-tex.inc "$(VERSION)"

DISTCLEANFILES = casvers.inc casvers-tex.inc
