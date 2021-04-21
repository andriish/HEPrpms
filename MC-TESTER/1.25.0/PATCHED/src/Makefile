EXT_LIBS=-lg2c 
include ../make.inc

UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
  LD_LIBRARY_PATH := $(shell $(ROOTBIN)/root-config --libdir):$(LD_LIBRARY_PATH)
  export LD_LIBRARY_PATH
endif

ifeq ($(UNAME), Darwin)
  DYLD_LIBRARY_PATH := $(shell $(ROOTBIN)/root-config --libdir):$(DYLD_LIBRARY_PATH)
  export DYLD_LIBRARY_PATH
endif

LIB_DIR = ../lib/
LIB1	 = libTGen.so
LIB2     = libHEPEvent.so 


LIB       = ../lib/libMCTester.$(LIB_EXT)
LIB_A     = ../lib/libMCTester.a

LIB_HDR = TDecayMode.H TDecayResult.H Setup.H TUserAnalysis.H Generate.h MCTester-F77.h GenerationDescription.H MCTest01.H MCTest02.H MCTest03.H UserEventAnalysis.H MCTesterEvent.H UserTreeAnalysis.H LinkDef.h
LIB_OBJ = TDecayMode.o TDecayResult.o Setup.o TUserAnalysis.o Generate.o MCTester-F77.o GenerationDescription.o MCTest01.o MCTest02.o MCTest03.o UserEventAnalysis.o MCTesterEvent.o UserTreeAnalysis.o

#if ROOT is used we'll need dictionaries also...
DICT_SRC = MCTesterDict.cxx
DICT_HDR = MCTesterDict.h
DICT_OBJ = MCTesterDict.o


override LIB_OBJ += $(DICT_OBJ)


override CXXFLAGS += -I../HEPEvent

######## Targets #####################################################
all: $(LIB) $(LIB_A)

%.o: %.f
	$(F77) $(F77FLAGS) -c $<
%.o : %.cxx %.H 
	$(CXX)  $(CXXFLAGS) -c $<
%.o : %.cxx %.h
	$(CXX) $(CXXFLAGS) -c $<
%.o : %.C %.H
	$(CXX) $(CXXFLAGS) -c $<


$(DICT_SRC) $(DICT_HDR): $(LIB_HDR) 
	$(ROOTBIN)/rootcint  -f $(DICT_SRC) -c -I../HEPEvent $(LIB_HDR) 


$(LIB): $(LIB_OBJ)
	$(LD) $(LDFLAGS) $(SOFLAGS) -o $(LIB) $(LIB_OBJ) -L$(ROOTLIBPATH) -lCore  -lHist -lGpad -lGraf -lRIO -L$(CURDIR)/../lib -lHEPEvent

$(LIB_A): $(LIB_OBJ)
	ar cr $(LIB_A) $(LIB_OBJ) 

Clean: clean

clean:
	rm -rf  *~ core xxx $(LIB) $(LIB_A) $(LIB_OBJ) mc-results.*
	rm -rf  $(DICT_OBJ)
	rm -rf $(DICT_HDR) $(DICT_SRC) 


