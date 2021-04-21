#
# recompilation of sources only - no linking....
#

######## Platform dependent compiler settings : ######################
include ../make.inc
######################################################################


######## Targets #####################################################
LIB_OBJ = HepMC3Event.o  HepMC3Particle.o \

LIB_HDR = HepMC3Event.h  HepMC3Particle.h \
          LinkDef.h

LIB     = ../lib/libHepMC3Event.${LIB_EXT}
LIB_A   = ../lib/libHepMC3Event.a
LIB_DIR = ../lib

#if ROOT is used we'll need dictionaries also...
DICT3_SRC = HepMC3EventDict.cxx
DICT3_HDR = HepMC3EventDict.h
DICT3_OBJ = HepMC3EventDict.o

ifeq ($(USE_ROOT),yes)
override LIB_OBJ += $(DICT3_OBJ)
endif
# override CXXFLAGS += -U_USE_ROOT_

HEPEVENT_INC=-I../include/
HEPMC3_INC=-I$(HEPMC3LOCATION)/include

ROOT_CINT=$(ROOTBIN)/rootcint

##############   What has to be built ###############################


all:  HEPMC3_HEADERS $(LIB) $(LIB_A)
#	ln -s *.H ../../include

#%.o: %.cxx %.H 
#	$(CXX) $(CXXFLAGS) $(HEPMC3_INC) $(CLHEP_INC) $(HEPEVENT_INC) -fexceptions -c $<
%.o: %.cxx %.h
	$(CXX) $(CXXFLAGS) $(HEPMC3_INC) $(CLHEP_INC) $(HEPEVENT_INC) -c -fexceptions  $<

$(DICT3_SRC) $(DICT3_HDR): $(LIB_HDR) 
	$(ROOT_CINT) -f $(DICT3_SRC) -c -p -D_USE_ROOT_ ${INC} \
	$(HEPMC3_INC) $(CLHEP_INC) $(HEPEVENT_INC) $(LIB_HDR)

$(LIB): $(LIB_OBJ)
	$(LD)  $(SOFLAGS) -o $(LIB) $(LIB_OBJ) -L$(ROOTLIBPATH) -lCore  -L$(CURDIR)/../lib -lHEPEvent

$(LIB_A): $(LIB_OBJ)
	ar cr $(LIB_A) $(LIB_OBJ)

HEPMC3_HEADERS: #HepMC3 settings
ifeq ($(HEPMC3LOCATION), )
	@echo "HEPMC3LOCATION must be set"
	@false
endif		


clean:
	rm -f  *~ core $(LIB_OBJ) 
	rm -f $(DICT3_SRC) $(DICT3_OBJ) $(DICT3_HDR)

Clean: clean	
	rm -f  $(LIB) $(LIB_A)
