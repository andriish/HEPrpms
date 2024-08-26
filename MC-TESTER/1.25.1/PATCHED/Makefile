include make.inc

all: lib
	make -C HEPEvent
	make -C src
	@make libHepMCEvent
	@make libHepMC3Event
	@echo
	@echo "###############################################################"
	@echo "  MC-TESTER libraries compiled."
ifeq ($(HEPMCLOCATION), )
	@echo "  (Compiled without HepMC interface library: libHepMCEvent)"
endif
ifeq ($(HEPMC3LOCATION), )
	@echo "  (Compiled without HepMC3 interface library: libHepMC3Event)"
endif
	@echo ""
	@echo "  To build and run demonstation programs go to the directories: "
	@echo "   examples-F77 or examples-C++"
	@echo "###############################################################"

libHepMCEvent:
ifneq ($(HEPMCLOCATION), )
	make -C HepMCEvent
	@echo "HepMC interface library libHepMCEvent created."
endif

libHepMC3Event:
ifneq ($(HEPMC3LOCATION), )
	make -C HepMC3Event
	@echo "HepMC3 interface library libHepMC3Event created."
endif
install:
	mkdir -p  $(DESTDIR)/$(PREFIX)/include/MC-TESTER/
	cp include/*  $(DESTDIR)/$(PREFIX)/include/MC-TESTER/
	mkdir -p  $(DESTDIR)$(LIBDIR)/
	cp lib/*  $(DESTDIR)$(LIBDIR)/
	mkdir -p  $(DESTDIR)$(PREFIX)/share/doc/MC-TESTER/
	cp -r analyze $(DESTDIR)$(PREFIX)/share/doc/MC-TESTER/
	cp src/TDecayMode.H $(DESTDIR)/$(PREFIX)/include/MC-TESTER/
	cp src/TDecayResult.H $(DESTDIR)/$(PREFIX)/include/MC-TESTER/
	cp src/TUserAnalysis.H $(DESTDIR)/$(PREFIX)/include/MC-TESTER/
	cp src/UserEventAnalysis.H $(DESTDIR)/$(PREFIX)/include/MC-TESTER/
	cp src/UserTreeAnalysis.H $(DESTDIR)/$(PREFIX)/include/MC-TESTER/
	cp src/Generate.h $(DESTDIR)/$(PREFIX)/include/MC-TESTER/
	cp src/MCTester-F77.h $(DESTDIR)/$(PREFIX)/include/MC-TESTER/


make.inc:
	@echo ""
	@echo "Please execute ./configure first!"
	@echo "(consider using 'source platform/afs.paths.sh' [or .csh] first)"
	@echo ""
	@false

dictionary:
	make -C HEPEvent dictionary
	make -C src dictionary

clean:
	make -C src clean
	make -C HEPEvent clean
ifneq ($(HEPMCLOCATION), )
	make -C HepMCEvent clean
endif
ifneq ($(HEPMC3LOCATION), )
	make -C HepMC3Event clean
endif
	make -C analyze clean
	rm -rf lib/*

lib:
	mkdir lib

Clean: clean
	rm -rf config.log config.status autom4te.cache
	rm -f platform/make.inc platform/configure.paths.sh make.inc
