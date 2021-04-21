LIBFLAGlha = -lLHAPDF
LIBFLAGapfel = -lAPFEL -lAPFELevol
LHAPDFLIB = /usr/local/Cellar/lhapdf/6.2.1/lib
APFELLIB = /Users/luch/Desktop/superchics/apfel_install/lib
FC = gfortran

#####################

HOME = $(PWD)
SOURCEDIR = $(PWD)/src
INCPATH = $(SOURCEDIR)/inc

OBJ_PATH = $(PWD)/obj/

$(shell mkdir -p lib/)

# FFLAGS 	= -fno-automatic -fno-f2c -O2 -g  -I$(INCPATH) 
FFLAGS 	= -fno-automatic -fno-f2c -fPIC -O2 -g  -I$(INCPATH)

DIRS	 =	$(SOURCEDIR)/int:\
		$(SOURCEDIR)/main:\
		$(SOURCEDIR)/mes:\
		$(SOURCEDIR)/PDFs:\
		$(SOURCEDIR)/phase:\
		$(SOURCEDIR)/sPDFs:\
		$(SOURCEDIR)/subamps:\
		$(SOURCEDIR)/surv:\
		$(SOURCEDIR)/EW:\
		$(SOURCEDIR)/unw:\
		$(SOURCEDIR)/user:\
		$(SOURCEDIR)/int:\
		$(SOURCEDIR)/var:\
		$(SOURCEDIR)/init:\
		$(SOURCEDIR)/LbyL:\
		$(SOURCEDIR)/inition:\
		$(SOURCEDIR)/diss:\

VPATH = $(DIRS)

#############

Dissf = \
CLAS.o \
gd-fit-11.o \
CB.o \
Elastic.o \
F1F2.o \
F2ev.o \
rfit.o \
alphaEM.o \
qinit.o \
apfelinit.o \

LbyLf = \
B0F2M.o \
D0404M.o \
xspenz.o \
eett_aux.o \
C01_gen.o \

Mesf = \
calcmes.o \
mesint.o \
wfinit.o \
wfoctet.o \
wfsinglet.o \

PDFsfLHA = \
alphas.o \
inpdf.o \

PDFsfUSER = \
alphasuser.o \
inpdfuser.o \

Intf = \
vegas.o \
rann.o \

Phasef = \
2body.o \
2bodyw.o \
2jetps.o \
2jetpsm.o \
3jetps.o \
boost.o \
chic0decay3.o \
chic1decay3.o \
chic1decay2s.o \
chic1decay2f.o \
chic2decay3.o \
chic2decay2s.o \
chic2decay2f.o \
jpsidecayphot.o \
genpol1.o \
genpol1rf.o \
genpol2.o \
rambo.o \
6body.o \
6bodyinit.o \
4body.o \
4bodyinit.o \
3body.o \
3bodyinit.o \
2bodyinit.o \
wwcorr.o \
jpsidecay.o \
rhodecay.o \
chidecay.o \
monow.o \
alpdecay.o \
pAboost.o \
pAinit.o \
AAinit.o \

Subampsf = \
chi0.o \
chi1.o \
chi2.o \
etaq.o \
higgs.o \
higgsinit.o \
pipi.o \
qqjets.o \
diphoton.o \
etaeta.o \
etapetap.o \
etaetap.o \
eta.o \
gggjets.o \
pipixy.o \
rhorho.o \
djpsi.o \
djpsip.o \
djpsipp.o \
ggjets.o \
qqgjets.o \
rhorhoxy.o \
wwpol.o \
llpol.o \
mhv.o \
lightlightpol.o \
higgsgam.o \
higgsgaminit.o \
alp.o \
mmpol.o \
monop.o \
lloff.o \
spinors.o \

Survf = \
initparsr.o \
formfac.o \
formfacphot.o \
formfacgam.o \
seik.o \
seikphot.o\
seikgam.o \
screeningint.o \
readscreen.o \
formfacgamel.o \
formfacgamion.o \
formfacgamionp.o \
tpint.o \
seikgamion.o \
screeningionint.o \
seikion.o \
betaionex.o \
betaion.o \
seikphotionp.o \
formfacphotionp.o \
formfacgamoff.o \
formfacgamoff_surv.o \
s2_diss_int.o \
s2_diss_init.o \

Userf = \
cuts.o \
histo.o \

Mainf = \
bare.o \
header.o \
main.o \
process.o \
wtgen.o \
wtgengam.o \

sPDFsf = \
calchg.o \
hpdfint.o \
calcsud.o \
sudint.o \
sPDF.o \

Unwf = \
unweight.o \
headerlhe.o \
hepmcfunc.o \
unweight_q.o \
unwprint_q.o \
unwprint.o \
unweight.o \

Varf = \
mu.o \
nf.o \
string.o \
varfuncs.o \
gaminit.o \
gauss.o \

InitfLHA = \
alphas.o \
initsud.o \
nf.o \
string.o \
hg.o \
inithg.o \
initpars.o \
calcop.o \
calcscreen.o \
opacityint.o \
screeningint.o \
screening.o \
opacity.o \
PDF.o \
PDFlha.o \
Sudakov.o \
inpdf.o \
dd.o \
Elastic.o \
sdcoh.o \
rann.o \
readscreen.o \
sdincoh.o \
F1F2.o \
F2ev.o \
rfit.o \
gd-fit-11.o \
CLAS.o \
apfelinit.o \
dd_test.o \

InitfION = \
rho.o \
ionpars.o \
rhonorm.o \
rhoxy.o \
rhoxycalc.o \
tpcalc.o \
tp.o \
rhoxyint.o \
betaion.o \
opacpcalc.o \
opacp.o \
opacpint.o \
opacpb.o \
opacpbcalc.o \
opacpbp.o \
opacpbpcalc.o \
opacpbint.o \
opacpbpint.o \
screencalc.o \
screen.o \
string.o \
s2qcdion.o \
s2qcdionp.o \
ioninit.o \
tpqcd.o \
tpqcdcalc.o \
tpqcdint.o \

InitfUSER = \
alphasuser.o \
init.o \
initsud.o \
nf.o \
string.o \
hg.o \
inithg.o \
initpars.o \
calcop.o \
calcscreen.o \
opacityint.o \
screeningint.o \
screening.o \
opacity.o \
PDF.o \
PDFuser.o \
Sudakov.o \
inpdfuser.o \
dd.o \
Elastic.o \
sdcoh.o \
rann.o \
readscreen.o \
sdincoh.o \
F1F2.o \
F2ev.o \
rfit.o \
gd-fit-11.o \
CLAS.o \
apfelinit.o \

#

sCODELHAi = $(Mainf) $(Mesf) $(EW) $(PDFsfLHA) $(Intf) $(Phasef) $(Subampsf) $(Survf) $(Userf) $(sPDFsf) $(Unwf) $(Varf) $(LbyLf) $(InitfION) $(Dissf)

sCODELHA = $(patsubst %,$(OBJ_PATH)%,$(sCODELHAi))

iCODELHAi = $(InitfLHA) 
iCODELHA = $(patsubst %,$(OBJ_PATH)%,$(iCODELHAi))

iCODEIONi = $(InitfION) 
iCODEION = $(patsubst %,$(OBJ_PATH)%,$(iCODEIONi))

###########

all : init superchic superchicLib

superchicLib: $(sCODELHA) 
	$(FC) -L$(LHAPDFLIB) -L$(APFELLIB) $(LIBFLAGlha) $(LIBFLAGapfel) -mcmodel=large -shared -fPIC -o lib/libsuperchic.so $^
	ar rc lib/libsuperchic.a obj/*.o

superchic.o:	superchic.f
	$(FC) $(FFLAGS) -I$(INCPATH) -c  $< -o $@

init.o:	init.f
	$(FC) $(FFLAGS) -I$(INCPATH) -c  $< -o $@

$(OBJ_PATH)%.o: %.f
	$(FC) $(FFLAGS) -I$(INCPATH) -c  $< -o $@

superchic : $(OBJ_PATH)superchic.o $(sCODELHA) 
	$(FC) $^ -L$(LHAPDFLIB) -L$(APFELLIB) $(LIBFLAGlha) $(LIBFLAGapfel) -o bin/$@

init : $(OBJ_PATH)init.o $(iCODELHA) 
	$(FC) $^ -L$(LHAPDFLIB) -L$(APFELLIB) $(LIBFLAGlha) $(LIBFLAGapfel) -o bin/$@

clean:
	rm -f bin/init bin/superchic lib/lib* *.o $(OBJ_PATH)*.o
