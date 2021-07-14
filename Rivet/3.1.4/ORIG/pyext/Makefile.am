SUBDIRS = rivet

if ENABLE_PYEXT

PYEXT_ENV = CC="$(CXX)" CXX="$(CXX)" CXXFLAGS="$(PYEXT_CXXFLAGS)" ARCHFLAGS=""

## Always force setup.py, it's not good at guessing what needs to rebuild
all-local:
	cd $(srcdir); \
	$(PYEXT_ENV) RIVET_LOCAL=1 $(PYTHON) \
	  $(abs_builddir)/setup.py build \
	  --build-base $(abs_builddir)/build \
	  --verbose \
	  --force

install-exec-local:
	$(PYEXT_ENV) $(PYTHON) \
	  $(abs_builddir)/setup.py install \
	  --prefix $(DESTDIR)$(prefix) \
	  --verbose \
	  --skip-build \
	  --force

uninstall-local:
	dir1=$(DESTDIR)$(pythondir); \
	dir2=$${dir1/lib/lib64}; \
	for i in $$dir1 $$dir2; do \
	 rm -rf $$i/rivet; \
	 rm -rf $$i/rivet*.egg-info; \
	done

clean-local:
	rm -rf $(builddir)/build

endif
