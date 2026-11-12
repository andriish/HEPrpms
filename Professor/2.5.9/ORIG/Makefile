## Makefile for Professor 2.x

## Include the config
include Makefile.inc

DESTDIR ?=
PIP_PREFIX := $(DESTDIR)$(PREFIX)

DISTNAME := Professor-$(VERSION)
$(info Building Professor $(VERSION))

## Default values for user-specifiable build variables
# TODO: migrate to configure flags?

# --- defaults ---
DEFAULT_OPTFLAGS  ?= -O3
DEFAULT_ARCHFLAGS ?=
DEFAULT_CXXFLAGS  ?= $(DEFAULT_OPTFLAGS) $(DEFAULT_ARCHFLAGS)

# --- capture whatever the user provided ---
USER_CXXFLAGS := $(CXXFLAGS)

# --- compose: defaults first, user last ---
CXXFLAGS := $(DEFAULT_CXXFLAGS) $(USER_CXXFLAGS)

# Debug
ifeq ($(DEBUG),1)
  CXXFLAGS += -g
endif

CPPFLAGS ?=
CPPFLAGS += $(EIGEN_CPPFLAGS)

## Check for compatible dependencies
ifneq "$(ROOTCONFIG)" ""
  ROOT_VERSION := $(shell $(ROOTCONFIG) --version 2> /dev/null)
  ifeq "$(ROOT_VERSION)" ""
    ROOT_VERSION := 0
  else
    HAVE_ROOT := $(ROOT_VERSION)
  endif
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
ifndef HAVE_GOOD_CYTHON
  $(error Incompatible Cython $(CYTHON_VERSION) found... update using ${PYTHON} -m pip install -U Cython)
endif


## Build-file variables

LIBHEADERS := $(wildcard include/Professor/*.h)
LIBSOURCES := $(wildcard src/*.cc)
LIBOBJECTS := $(patsubst %,obj/%.o, ParamPoints Ipol Version)
TESTSOURCES := $(wildcard test/*.cc test/testPython*)
TESTPROGS  := test/testParamPoints test/testIpol
BINPROGS := $(wildcard bin/prof2-*)
CONTRIBPROGS := $(wildcard contrib/*)
JUPYTERNBS := $(wildcard jupyter/*)
PYTHONSOURCES := $(wildcard pyext/professor2/*.py)
CYTHONSOURCES := $(wildcard pyext/professor2/*.pxd) $(wildcard pyext/professor2/*.pyx)


## Make targets

.PHONY: all lib pyext pyext-user tests cxxtests pytests pydeps check icheck clean root dist

all: lib pyext tests Makefile.inc
	@true

lib: lib/libProfessor2.so Makefile.inc
	@true

Makefile.inc: Makefile configure
	if [[ -e Makefile.inc ]]; then cat Makefile.inc | sed -n 's:^# \(\./configure.*\):\1:p'; else echo "You need to run 'configure' first"; fi

lib/libProfessor2.so: $(LIBOBJECTS)
	mkdir -p lib
	$(CXX) -shared -o $@ $(LIBOBJECTS)

obj/%.o: src/%.cc $(LIBHEADERS)
	mkdir -p obj
	$(CXX) -std=$(CXXSTD) -DPROF_VERSION="$(VERSION)" -I include $(CPPFLAGS) $(CXXFLAGS) -c -fPIC $< -o $@

obj/Version.o: src/Version.cc $(LIBHEADERS) Makefile
	mkdir -p obj
	$(CXX) -std=$(CXXSTD) -DPROF_VERSION="$(VERSION)" -Iinclude $(CPPFLAGS) $(CXXFLAGS) -c -fPIC $< -o $@

pydeps: Makefile.inc
	@$(PYTHON) -m pip &> /dev/null || { echo "Python doesn't have pip support, run ${PYTHON} -m ensurepip"; exit 1; }
	@$(PYTHON) -c "import setuptools" &> /dev/null || { echo "Python setuptools needs to be installed: ${PYTHON} -m pip install setuptools"; exit 1; }
	@$(PYTHON) -c "import cython" &> /dev/null || { echo "Cython needs to be installed: ${PYTHON} -m pip install Cython"; exit 1; }
	@$(PYTHON) -c "import numpy" &> /dev/null || { echo "numpy needs to be installed: ${PYTHON} -m pip install numpy"; exit 1; }

pyext: pydeps pyext/professor2/core.cpp $(wildcard pyext/professor2/*.py) Makefile.inc
	cd pyext/ && PROF_VERSION=$(VERSION) PROF_ROOT=$(PWD) \
	  $(PYTHON) -m pip wheel -v --no-deps -w dist ./

pyext-user: pydeps pyext/professor2/core.cpp $(wildcard pyext/professor2/*.py)
	@echo "Installing professor2 Python bindings into the current Python environment (dev convenience)"
	cd pyext/ && PROF_VERSION=$(VERSION) PROF_ROOT=$(PWD) \
	  $(PYTHON) -m pip install -vv ./

pyext/professor2/core.cpp: $(LIBHEADERS) $(CYTHONSOURCES) lib
	$(CYTHON) pyext/professor2/core.pyx --cplus

# pyext/professor2/core.so: pyext/professor2/core.cpp
# 	PROF_VERSION=$(VERSION) $(PYTHON) pyext/setup.py build_ext -i --force

tests: cxxtests pytests
	@true

cxxtests: $(TESTPROGS) Makefile.inc
	@true

test/%: test/%.cc $(LIBHEADERS) lib
	$(CXX) -std=$(CXXSTD) -Iinclude $(CPPFLAGS) $(CXXFLAGS) $< -Llib -lProfessor2 -o $@

ifdef HAVE_ROOT
root: src/testRoot.cc $(LIBHEADERS) lib Makefile.inc
	$(CXX) -std=$(CXXSTD) $(CPPFLAGS) $(CXXFLAGS) $< -Iinclude `root-config --cflags --libs` -Llib -lProfessor2 -o test/test$@
endif

pytests: pyext
	@true

check: tests Makefile.inc
	@echo
	@echo "testParamPoints" && test/testParamPoints && echo "\n\n"
	@echo "testIpol" && test/testIpol && echo "\n\n"

icheck: tests
	test/testPython
	test/testPython1D
	test/testPython2D

install: all
	mkdir -p $(PREFIX)/bin && cp bin/prof2-* $(PREFIX)/bin/
	#mkdir -p $(PREFIX)/contrib && cp contrib/* $(PREFIX)/contrib/
	#mkdir -p $(PREFIX)/jupyter && cp jupyter/* $(PREFIX)/jupyter/
	mkdir -p $(PREFIX)/include && cp -r include/Professor $(PREFIX)/include/
	test -d lib   && mkdir -p $(PREFIX)/lib   && cp -r lib/* $(PREFIX)/lib/ || true
	test -d lib64 && mkdir -p $(PREFIX)/lib64 && cp -r lib64/* $(PREFIX)/lib64/ || true
	# TODO: use proper install mechanism, bundle via PyPI
	cd pyext/ && PROF_VERSION=$(VERSION) PROF_ROOT=$(PWD) \
	  $(PYTHON) -m pip install -v --no-deps --no-user --prefix "$(PIP_PREFIX)" --force-reinstall dist/*$(VERSION)*.whl
#	cp setup.sh $(PREFIX)

dist: all
	rm -rf $(DISTNAME)
	mkdir -p $(DISTNAME)
	cp --parents \
       README.md configure Makefile \
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
