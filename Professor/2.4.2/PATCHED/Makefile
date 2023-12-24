## Makefile for Professor 2.x

VERSION := 2.4.2
DISTNAME := Professor-$(VERSION)
$(info Building Professor $(VERSION))


## Default values for user-specifiable build variables

SHELL := /bin/bash

ifndef PREFIX
  PREFIX := /usr/local
endif

CXXSTD := c++11

ifndef CXX
  CXX := g++
endif

ifndef CPPFLAGS
  CPPFLAGS := -I/usr/include/eigen3
endif

ifndef CXXFLAGS
  CXXFLAGS := -O3 -march=native
  ifdef DEBUG
	ifneq ($(DEBUG),0)
	  CXXFLAGS += -g
	endif
  endif
endif

ifndef PYTHON
  PYTHON := python3
endif

ifndef CYTHON
  CYTHON := cython
endif

ifndef ROOTCONFIG
  ROOTCONFIG := root-config
endif


## Check for compatible dependencies

ROOT_VERSION := $(shell $(ROOTCONFIG) --version 2> /dev/null)
ifeq "$(ROOT_VERSION)" ""
  ROOT_VERSION := 0
else
  HAVE_ROOT := $(ROOT_VERSION)
endif
ifdef HAVE_ROOT
  $(info ROOT $(ROOT_VERSION) is available... providing 'root' make target)
else
  $(info ROOT is not available... not making any ROOT integration)
endif


#CYTHON_VERSION := $(shell $(CYTHON) --version 1> /dev/null)
CYTHON_VERSION := $(shell $(CYTHON) --version < /dev/null 2>&1 | grep -i 'Cython version' | cut -d\  -f3)
ifeq "$(CYTHON_VERSION)" ""
  CYTHON_VERSION := NONE
  CYTHON_VERSION1 := 0
  CYTHON_VERSION2 := 0
else
  CYTHON_VERSION := $(shell echo $(CYTHON_VERSION) | sed -e 's/\([0-9\.]\+\)[ab].*/\1/')
  CYTHON_VERSION1 := $(shell echo $(CYTHON_VERSION) | cut -d. -f1)
  CYTHON_VERSION2 := $(shell echo $(CYTHON_VERSION) | cut -d. -f2)
  HAVE_GOOD_CYTHON := $(shell test "$(CYTHON_VERSION1)" -eq 0 -a "$(CYTHON_VERSION2)" -ge 20 && echo 1 || echo 0)
endif
ifdef HAVE_GOOD_CYTHON
  $(info Cython $(CYTHON_VERSION) found... will rebuild C++/Python interface)
else
  $(info Incompatible Cython $(CYTHON_VERSION) found... will build C++/Python interface from bundled C file)
endif


## Build-file variables

LIBHEADERS := $(wildcard include/Professor/*.h)
LIBSOURCES := $(wildcard src/*.cc)
LIBOBJECTS := $(patsubst %,obj/%.o, ParamPoints Ipol Version)
TESTSOURCES := $(wildcard test/*.cc test/testPython*)
TESTPROGS  := test/testParamPoints test/testIpol
BINPROGS := $(wildcard bin/*)
CONTRIBPROGS := $(wildcard contrib/*)
JUPYTERNBS := $(wildcard jupyter/*)
PYTHONSOURCES := $(wildcard pyext/professor2/*.py)
CYTHONSOURCES := $(wildcard pyext/professor2/*.pxd) $(wildcard pyext/professor2/*.pyx)


## Make targets

.PHONY: all lib pyext tests cxxtests pytests pydeps check icheck clean root dist

all: lib pyext tests
	@true

lib: lib/libProfessor2.so
	@true

lib/libProfessor2.so: $(LIBOBJECTS)
	mkdir -p lib
	$(CXX) -shared -o $@ $(LIBOBJECTS)

obj/%.o: src/%.cc $(LIBHEADERS)
	mkdir -p obj
	$(CXX) -std=$(CXXSTD) -DPROF_VERSION="$(VERSION)" -I./ -I./include $(CPPFLAGS) $(CXXFLAGS) -c -fPIC $< -o $@
obj/Version.o: src/Version.cc $(LIBHEADERS) Makefile
	mkdir -p obj
	$(CXX) -std=$(CXXSTD) -DPROF_VERSION="$(VERSION)" -Iinclude $(CPPFLAGS) $(CXXFLAGS) -c -fPIC $< -o $@

pydeps:
#	@$(PYTHON) -m pip &> /dev/null || { echo "Python doesn't have pip support"; exit 1; }
#	@$(PYTHON) -c "import setuptools" &> /dev/null || { echo "Python setuptools needs to be installed: pip install setuptools"; exit 1; }
#	@$(PYTHON) -c "import cython" &> /dev/null || { echo "Cython needs to be installed: pip install Cython"; exit 1; }

pyext: pydeps pyext/professor2/core.cpp $(wildcard pyext/professor2/*.py)
#	PROF_VERSION=$(VERSION) $(PYTHON) -m pip install -vv ./pyext/

ifdef HAVE_GOOD_CYTHON
pyext/professor2/core.cpp: $(LIBHEADERS) $(CYTHONSOURCES) lib
	$(CYTHON) pyext/professor2/core.pyx --cplus -Ipyext/professor2
else
pyext/professor2/core.cpp: $(LIBHEADERS) $(CYTHONSOURCES) lib
	$(info "Cython >= 0.20 not available; can't build $@")
endif

# pyext/professor2/core.so: pyext/professor2/core.cpp
# 	PROF_VERSION=$(VERSION) $(PYTHON) pyext/setup.py build_ext -i --force

tests: cxxtests pytests
	@true

cxxtests: $(TESTPROGS)
	@true

test/%: test/%.cc $(LIBHEADERS) lib
	$(CXX) -std=$(CXXSTD) -Iinclude $(CPPFLAGS) $(CXXFLAGS) $< -Llib -lProfessor2 -o $@

ifdef HAVE_ROOT
root: src/testRoot.cc $(LIBHEADERS) lib
	$(CXX) -std=$(CXXSTD) $(CPPFLAGS) $(CXXFLAGS) $< -Iinclude `root-config --cflags --libs` -Llib -lProfessor2 -o test/test$@
endif

pytests: pyext
	@true

check: tests
	@echo
	@echo "testParamPoints" && test/testParamPoints && echo "\n\n"
	@echo "testIpol" && test/testIpol && echo "\n\n"

icheck: tests
	test/testPython
	test/testPython1D
	test/testPython2D

install: all
	mkdir -p $(DESTDIR)$(PREFIX)/bin && cp bin/* $(DESTDIR)$(PREFIX)/bin/
	mkdir -p $(DESTDIR)$(PREFIX)/contrib && cp contrib/* $(DESTDIR)$(PREFIX)/contrib/
	mkdir -p $(DESTDIR)$(PREFIX)/jupyter && cp jupyter/* $(DESTDIR)$(PREFIX)/jupyter/
	mkdir -p $(DESTDIR)$(PREFIX)/include && cp -r include/Professor $(DESTDIR)$(PREFIX)/include/
	test -d lib   && mkdir -p $(DESTDIR)$(LIBDIR)   && cp -r lib/libProfessor2.so $(DESTDIR)$(LIBDIR)/ || true
	cd pyext && $(PYTHON) setup.py install --prefix=$(DESTDIR)$(PREFIX)
#	cp setup.sh $(PREFIX)

dist: all
	rm -rf $(DISTNAME)
	mkdir -p $(DISTNAME)
	cp --parents \
       README Makefile Makefile.mac \
       $(LIBHEADERS) \
       $(LIBSOURCES) \
       $(BINPROGS) \
       $(CONTRIBPROGS) \
       $(JUPYTERNBS) \
       $(TESTSOURCES) \
       $(PYTHONSOURCES) pyext/setup.py pyext/professor2/misc/*py pyext/professor2/ml/*py\
       $(CYTHONSOURCES) $(wildcard pyext/professor2/*.cpp) \
       $(DISTNAME)/
	tar czf $(DISTNAME).tar.gz $(DISTNAME)

clean:
	rm -rf obj/*.o lib/*
	rm -f pyext/professor2/core.cpp pyext/professor2/core.so
	rm -f $(TESTPROGS)
	rm -rf $(DISTNAME) $(DISTNAME).tar.gz
