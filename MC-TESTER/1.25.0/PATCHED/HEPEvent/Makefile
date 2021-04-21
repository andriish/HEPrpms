#
# recompilation of sources only - no linking....
#

######## Platform dependent compiler settings : ######################
include ../make.inc
######################################################################

UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
  LD_LIBRARY_PATH := $(shell $(ROOTBIN)/root-config --libdir):$(LD_LIBRARY_PATH)
  export LD_LIBRARY_PATH
endif

ifeq ($(UNAME), Darwin)
  DYLD_LIBRARY_PATH := $(shell $(ROOTBIN)/root-config --libdir):$(DYLD_LIBRARY_PATH)
  export DYLD_LIBRARY_PATH
endif

######## Targets #####################################################
LIB_OBJ = HEPParticle.o HEPEvent.o \
          LUJETSEvent.o LUJETSParticle.o \
          PYJETSEvent.o PYJETSParticle.o \
          HerwigParticle.o HerwigEvent.o \
          HEPEVTEvent.o HEPEVTParticle.o \
          THEPEvent.o   THEPParticle.o  \
          MC3Vector.o MC4Vector.o \

LIB_HDR = HEPParticle.H HEPEvent.H \
          LUJETSEvent.H LUJETSParticle.H \
          PYJETSEvent.H PYJETSParticle.H \
          HerwigParticle.H HerwigEvent.H \
          HEPEVTEvent.H HEPEVTParticle.H \
          THEPEvent.H   THEPParticle.H   \
          MC3Vector.H MC4Vector.H \
          LinkDef.h

LIB     = ../lib/libHEPEvent.$(LIB_EXT)
LIB_A   = ../lib/libHEPEvent.a
LIB_DIR = ../lib

PRG1_OBJ = t1.o
PRG1_EXE = t1

#if ROOT is used we'll need dictionaries also...
DICT_SRC = HEPEventDict.cxx
DICT_HDR = HEPEventDict.h
DICT_OBJ = HEPEventDict.o

ifeq ($(USE_ROOT),yes)
override LIB_OBJ += $(DICT_OBJ)
endif

##############   What has to be built ###############################


#all:     $(LIB) $(PRG1_EXE)
all:     $(LIB) $(LIB_A)

%.o: %.f
	$(F77) $(F77FLAGS) -c $<
%.o: %.cxx %.H 
	$(CXX) $(CXXFLAGS) -c $<
%.o: %.cxx %.h
	$(CXX) $(CXXFLAGS) -c $<

#$(DICT_SRC) $(DICT_HDR): $(LIB_HDR) 
#	$(ROOTSYS)/bin/rootcint -f $(DICT_SRC) -c -p -DDebug ${INC} $(LIB_HDR)

$(DICT_SRC) $(DICT_HDR): $(LIB_HDR)
	$(ROOTBIN)/rootcint -f $(DICT_SRC) -c -p -D_USE_ROOT_ $(LIB_HDR)

$(LIB): $(LIB_OBJ)
	$(LD) $(LDFLAGS) $(SOFLAGS) -o $(LIB) $(LIB_OBJ) -L$(ROOTLIBPATH) -lCore 

$(LIB_A): $(LIB_OBJ)
	ar cr $(LIB_A) $(LIB_OBJ)

#$(PRG1_EXE): $(PRG1_OBJ) $(LIB_OBJ)
#	@echo LIB_DIR is $(LIB_DIR)
#	$(LD) -o $(PRG1_EXE) $(PRG1_OBJ) $(LIB_DIR)/$(LIB) $(EXT_LIBS) \
#	$(LIB_DIR)/libPythia.so -lg2c

clean:
	rm -rf  *~ core $(LIB) $(LIB_A) $(LIB_OBJ) 
	rm -rf $(PRG1_EXE) $(PRG1_OBJ)
	rm -rf $(DICT_OBJ)
	rm -rf $(DICT_SRC) $(DICT_HDR)

Clean: clean

hepmcevent:
