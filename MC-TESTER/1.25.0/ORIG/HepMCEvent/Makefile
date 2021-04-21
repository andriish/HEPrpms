#
# recompilation of sources only - no linking....
#

######## Platform dependent compiler settings : ######################
include ../make.inc
######################################################################


######## Targets #####################################################
LIB_OBJ = HepMCEvent.o  HepMCParticle.o \

LIB_HDR = HepMCEvent.H  HepMCParticle.H \
          LinkDef.h

LIB     = ../lib/libHepMCEvent.${LIB_EXT}
LIB_A   = ../lib/libHepMCEvent.a
LIB_DIR = ../lib

#if ROOT is used we'll need dictionaries also...
DICT_SRC = HepMCEventDict.cxx
DICT_HDR = HepMCEventDict.h
DICT_OBJ = HepMCEventDict.o

ifeq ($(USE_ROOT),yes)
override LIB_OBJ += $(DICT_OBJ)
endif
# override CXXFLAGS += -U_USE_ROOT_

HEPEVENT_INC=-I../include/
HEPMC_INC=-I$(HEPMCLOCATION)/include

ROOT_CINT=$(ROOTBIN)/rootcint

##############   What has to be built ###############################


all:  HEPMC_HEADERS $(LIB) $(LIB_A)
#	ln -s *.H ../../include

%.o: %.cxx %.H 
	$(CXX) $(CXXFLAGS) $(HEPMC_INC) $(CLHEP_INC) $(HEPEVENT_INC) -c $<
%.o: %.cxx %.h
	$(CXX) $(CXXFLAGS) $(HEPMC_INC) $(CLHEP_INC) $(HEPEVENT_INC) -c $<

$(DICT_SRC) $(DICT_HDR): $(LIB_HDR) 
	$(ROOT_CINT) -f $(DICT_SRC) -c -p -D_USE_ROOT_ ${INC} \
	$(HEPMC_INC) $(CLHEP_INC) $(HEPEVENT_INC) $(LIB_HDR)

$(LIB): $(LIB_OBJ)
	$(LD) $(LDFLAGS) $(SOFLAGS) -o $(LIB) $(LIB_OBJ) -L$(ROOTLIBPATH) -lCore -lCint -L$(CURDIR)/../lib -lHEPEvent

$(LIB_A): $(LIB_OBJ)
	ar cr $(LIB_A) $(LIB_OBJ)

HEPMC_HEADERS: #HepMC settings
ifeq ($(HEPMCLOCATION), )
	@echo "HEPMCLOCATION must be set"
	@false
endif		


clean:
	rm -f  *~ core $(LIB_OBJ) 
	rm -f $(DICT_SRC) $(DICT_OBJ) $(DICT_HDR)

Clean: clean	
	rm -f  $(LIB) $(LIB_A)
