
if USE_READLINE
AM_CPPFLAGS += -DUSE_READLINE -DUSE_HISTORY
endif			 		


BH_PATH_OBJ = $(abs_top_builddir)/src/

if PUBLIC
bin_PROGRAMS= LH_reader
else
bin_PROGRAMS= calculator LH_reader
calculator_SOURCES = calculator.cpp
endif

LH_reader_SOURCES = LH_reader.cpp
LH_reader_CXXFLAGS = -fPIC

BH_LIBS = 

if ! PUBLIC
BH_LIBS += $(BH_PATH_OBJ)/.libs/libtools.la 
endif

BH_LIBS += \
	$(BH_PATH_OBJ)/Interface/.libs/libInterface.la \
	$(BH_PATH_OBJ)/.libs/libassembly.la \
	$(BH_PATH_OBJ)/.libs/libOLA.la \
	$(BH_PATH_OBJ)/.libs/libRatPart.la \
	$(BH_PATH_OBJ)/.libs/libCutPart.la \
	$(BH_PATH_OBJ)/ratext/.libs/libratext.la \
	$(BH_PATH_OBJ)/.libs/libBH.la \
	$(BH_PATH_OBJ)/trees_eval/.libs/libAmpl_eval.la \
	$(BH_PATH_OBJ)/rational_eval/.libs/libRateval.la \
	$(BH_PATH_OBJ)/cut/.libs/libCut_wCI.la \
	$(BH_PATH_OBJ)/cut_eval/.libs/libCuteval.la \
	$(BH_PATH_OBJ)/.libs/libBG.la       \
	$(BH_PATH_OBJ)/.libs/libBHcore.la    \
	$(BH_PATH_OBJ)/.libs/libIntegrals.la    \
	$(BH_PATH_OBJ)/.libs/libSpinors.la           

if ! PUBLIC
BH_LIBS += \
	$(BH_PATH_OBJ)/../FeynDiagram/.libs/libfd.la      
endif

if BH_USE_GMP
BH_LIBS += $(BH_PATH_OBJ)/.libs/libmpreal.la
endif

LINKER_OPTS = \
	-Wl,-rpath -Wl,$(BH_PATH_OBJ)/Interface/.libs \
	-Wl,-rpath -Wl,$(BH_PATH_OBJ)/cut/.libs \
	-Wl,-rpath -Wl,$(BH_PATH_OBJ)/cut_eval/.libs \
	-Wl,-rpath -Wl,$(BH_PATH_OBJ)/.libs \
	-Wl,-rpath -Wl,$(BH_PATH_OBJ)/ratext/.libs \
	-Wl,-rpath -Wl,$(BH_PATH_OBJ)/rational_eval/.libs \
	-Wl,-rpath -Wl,$(BH_PATH_OBJ)/../FeynDiagram/.libs \
	-Wl,-rpath -Wl,$(BH_PATH_OBJ)/trees/.libs \
	-Wl,-rpath -Wl,$(BH_PATH_OBJ)/trees_eval/.libs

LDFLAGS += $(LINKER_OPTS)

if ! PUBLIC
calculator_LDADD = $(BH_LIBS) -ldl
if USE_READLINE
calculator_LDADD += -lreadline 
endif

if BH_USE_GMP
calculator_LDADD += -lmpfr -lgmpxx -lgmp
endif
endif

LH_reader_LDADD = $(BH_LIBS) -ldl -lcrypto


if BH_USE_GMP
LH_reader_LDADD += -lmpfr -lgmpxx -lgmp 
endif

