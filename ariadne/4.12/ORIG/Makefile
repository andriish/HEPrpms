# This is the Makefile for the Ariadne program. You should edit it to 
# suite the way your system is set up. The macros you should customize
# are: FC FFLAGS PYTHIA LEPTO and INSTALLDIR. A few examples are
# given for each macro.

# set FC to the FORTRAN 77 compiler you want to use.
FC = f77

# set FFLAGS to the compiler options you want to use:
FFLAGS = -O

#set CPP to the C preprocessor you want to use
CPP = gcc -E -P

# set ARFLAGS to the 'ar' options you want
ARFLAGS = r

# PYTHIA sould be set to the name of the object/library file for PYTHIA v.5 +
# JETSET v.7
PYTHIA = -L/cern/pro/lib -ljetset74 -lpacklib -lkernlib -lmathlib

# PYTHIA6 sould be set to the name of the object/library file for PYTHIA v.56
PYTHIA6 = -L/cern/pro/lib -lpythia6136 -lpacklib -lkernlib -lmathlib

# LEPTO sould be set to the name of the object/library file for LEPTO v.6
LEPTO = -L/cern/pro/lib -llepto -lkernlib 

# INSTALLDIR should be set to the name of the directory where the compiled 
# Ariadne library should be placed.
INSTALLDIR = /usr/local/lib

#
# YOU SHOULD NOT CHANGE ANYTHING BELOW THIS LINE
#

include Makefile.common

P6:
	mkdir P6
	cd P6; ln -s ../Makefile.P6 Makefile
	cd P6; for file in $(SOURCES) $(MAINFILES) $(INCLUDEFILES) $(FPPFILES); do ln -s ../$$file . ; done

all: lib

atest: libartest.a atest.f
	@cd P6; $(MAKE) FC=$(FC) CPP="$(CPP)" FFLAGS="$(FFLAGS)" AR=$(AR) ARFLAGS="$(ARFLAGS)" PYTHIA6="$(PYTHIA6)" atest
	@ln -s -f P6/atest .

a5test: libartestp5.a atest.o
	$(FC) $(FFLAGS) -o a5test atest.o -L. -lartestp5 $(PYTHIA)

jtest: libartest.a jtest.f
	@cd P6; $(MAKE) FC=$(FC) CPP="$(CPP)" FFLAGS="$(FFLAGS)" AR=$(AR) ARFLAGS="$(ARFLAGS)" PYTHIA6="$(PYTHIA6)" jtest
	@ln -f -s P6/jtest .

j5test: libartestp5.a jtest.o
	$(FC) $(FFLAGS) -o j5test jtest.o -L. -lartestp5 $(PYTHIA)

ltest: libartestp5.a ltest.f
	$(FC) $(FFLAGS) -o ltest ltest.f $(LEPTO) $(PYTHIA) -L. -lartestp5 $(PYTHIA)

ptest: libartest.a ptest.f
	@cd P6; $(MAKE) FC=$(FC) CPP="$(CPP)" FFLAGS="$(FFLAGS)" AR=$(AR) ARFLAGS="$(ARFLAGS)" PYTHIA6="$(PYTHIA6)" ptest
	@ln -s -f P6/ptest .

p5test: libartestp5.a ptest.o
	$(FC) $(FFLAGS) -o p5test ptest.o $(PYTHIA) -L. -lartestp5 $(PYTHIA)

lib: libartest.a

libp5: libartestp5.a

lib++:
	@cd LDC; $(MAKE) FC=$(FC) CXX=$(CXX) FFLAGS=$(FFLAGS) CXXFLAGS=$(CXXFLAGS) ARFLAGS=$(ARFLAGS) LIBCXX=$(LIBCXX) PYTHIA=$(PYTHIA) LEPTO=$(LEPTO) lib++

libartest.a: P6 $(SOURCES) $(FPPFILES)
	cd P6; $(MAKE) CPP="$(CPP)" FC=$(FC) FFLAGS="$(FFLAGS)" AR=$(AR) ARFLAGS="$(ARFLAGS)" libartest.a
	@ln -s -f P6/libartest.a .

libartestp5.a: $(OBJECTS)
	$(AR) $(ARFLAGS) $@ $?
	ranlib $@

libar4.a: libartest.a
	cp libartest.a libar4.a
	ranlib libar4.a

libar4p5.a: libartestp5.a
	cp libartest.a libar4.a
	ranlib libar4.a

install: libar4.a libar4p5.a
	cp libar4.a libar4p5.a $(INSTALLDIR)

clean:
	@cd P6; make clean
	rm -f *.c *.o *~ core *.bak $(PPFILES)

realclean: clean
	@cd P6; make realclean
	rm -f atest jtest ltest ptest libartest.a libartestp5.a libar4.a libar4p5.a

.F.f:
	$(CPP) $*.F > $*.f
