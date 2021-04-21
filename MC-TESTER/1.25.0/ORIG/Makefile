include make.inc

all:
	make -C HEPEvent
	make -C src
	@make libHepMCEvent
	@echo
	@echo "###############################################################"
	@echo "  MC-TESTER libraries compiled."
ifeq ($(HEPMCLOCATION), )
	@echo "  (Compiled without HepMC interface library: libHepMCEvent)"
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

install:
	mkdir -p $(PREFIX)/include
	cp include/* $(PREFIX)/include/.
	mkdir -p $(PREFIX)/lib
	cp lib/* $(PREFIX)/lib/.

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
	make -C HepMCEvent clean
	make -C analyze clean
	rm -rf lib/*

Clean: clean
	rm -rf config.log config.status autom4te.cache
	rm -f platform/make.inc platform/configure.paths.sh make.inc
