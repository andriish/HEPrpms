EXTRA_DIST = analists/mk-analysis-html analists/mk-analysis-json analists/mk-analysis-txt
DOCS =
DOCDIRS =

if ENABLE_PYEXT

## Analysis-list HTML pages
DOCS += analists/analyses.html
DOCDIRS += analists/analyses
analists/analyses.html: $(top_srcdir)/analyses $(srcdir)/analists/mk-analysis-html
	$(mkdir_p) analists && cd analists && \
    LD_LIBRARY_PATH=$(abs_top_builddir)/src/.libs:$(FASTJETLIBPATH):$(FJCONTRIBLIBPATH):$(HEPMCLIBPATH):$(HEPMC3LIBPATH):$(YODALIBPATH):$(LD_LIBRARY_PATH) \
    DYLD_LIBRARY_PATH=$(abs_top_builddir)/src/.libs:$(FASTJETLIBPATH):$(FJCONTRIBLIBPATH):$(HEPMCLIBPATH):$(HEPMC3LIBPATH):$(YODALIBPATH):$(DYLD_LIBRARY_PATH) \
    PYTHONPATH=$(shell realpath $(abs_top_builddir)/pyext/build/lib*):$(YODA_PYTHONPATH):$(PYTHONPATH) \
    RIVET_ANALYSIS_PATH=$(abs_top_builddir)/analyses:: \
    $(PYTHON) $(abs_srcdir)/analists/mk-analysis-html || true

## Analysis-list JSON file
DOCS += analists/analyses.json
analists/analyses.json: $(top_srcdir)/analyses $(srcdir)/analists/mk-analysis-json
	$(mkdir_p) analists && cd analists && \
    LD_LIBRARY_PATH=$(abs_top_builddir)/src/.libs:$(FASTJETLIBPATH):$(FJCONTRIBLIBPATH):$(HEPMCLIBPATH):$(HEPMC3LIBPATH):$(YODALIBPATH):$(LD_LIBRARY_PATH) \
    DYLD_LIBRARY_PATH=$(abs_top_builddir)/src/.libs:$(FASTJETLIBPATH):$(FJCONTRIBLIBPATH):$(HEPMCLIBPATH):$(HEPMC3LIBPATH):$(YODALIBPATH):$(DYLD_LIBRARY_PATH) \
    PYTHONPATH=$(shell realpath $(abs_top_builddir)/pyext/build/lib*):$(YODA_PYTHONPATH):$(PYTHONPATH) \
    RIVET_ANALYSIS_PATH=$(abs_top_builddir)/analyses:: \
    $(PYTHON) $(abs_srcdir)/analists/mk-analysis-json || true

## Analysis-list text file
DOCS += analists/analyses.dat
analists/analyses.dat: $(top_srcdir)/analyses $(srcdir)/analists/mk-analysis-txt
	$(mkdir_p) analists && cd analists && \
    LD_LIBRARY_PATH=$(abs_top_builddir)/src/.libs:$(FASTJETLIBPATH):$(FJCONTRIBLIBPATH):$(HEPMCLIBPATH):$(HEPMC3LIBPATH):$(YODALIBPATH):$(LD_LIBRARY_PATH) \
    DYLD_LIBRARY_PATH=$(abs_top_builddir)/src/.libs:$(FASTJETLIBPATH):$(FJCONTRIBLIBPATH):$(HEPMCLIBPATH):$(HEPMC3LIBPATH):$(YODALIBPATH):$(DYLD_LIBRARY_PATH) \
    PYTHONPATH=$(shell realpath $(abs_top_builddir)/pyext/build/lib*):$(YODA_PYTHONPATH):$(PYTHONPATH) \
    RIVET_ANALYSIS_PATH=$(abs_top_builddir)/analyses:: \
    $(PYTHON) $(abs_srcdir)/analists/mk-analysis-txt || true

## Analysis coverage
DOCS += coverage/rivet-coverage.html \
  coverage/rivet-coverage-heavyiononly.html \
  coverage/rivet-coverage-noheavyion.html \
  coverage/rivet-coverage-searchesonly.html \
  coverage/rivet-coverage-nosearches.html \
  coverage/rivet-coverage-nosearches-noheavyion.html
coverage/rivet-coverage.html: analists $(wildcard $(srcdir)/coverage/inspire*.json) $(wildcard $(srcdir)/coverage/*.rank) $(top_srcdir)/analyses $(srcdir)/coverage/mk-coverage-html
	$(mkdir_p) coverage && cd coverage && \
    LD_LIBRARY_PATH=$(abs_top_builddir)/src/.libs:$(FASTJETLIBPATH):$(FJCONTRIBLIBPATH):$(LD_LIBRARY_PATH):$(LD_LIBRARY_PATH) \
    DYLD_LIBRARY_PATH=$(abs_top_builddir)/src/.libs:$(FASTJETLIBPATH):$(FJCONTRIBLIBPATH):$(DYLD_LIBRARY_PATH):$(DYLD_LIBRARY_PATH) \
    PYTHONPATH=$(shell realpath $(abs_top_builddir)/pyext/build/lib*):$(YODA_PYTHONPATH):$(PYTHONPATH) \
    RIVET_ANALYSIS_PATH=$(abs_top_builddir)/analyses:: \
    $(abs_srcdir)/coverage/mk-coverage-htmls || true

endif


## Doxygen
if WITH_DOXYGEN
DOCS += doxygen/html
EXTRA_DIST += doxygen
dox: doxygen/html
	@true
doxygen/html: doxygen/Doxyfile doxygen/index.dox $(top_srcdir)/analyses $(top_srcdir)/include/Rivet $(top_srcdir)/src
	@cd  $(builddir)/doxygen/ && $(DOXYGEN) Doxyfile || true
	@cp $(abs_srcdir)/doxygen/dynsections.js doxygen/html/
install-dox:
	$(install_sh_DATA) doxygen/html/* $(DESTDIR)$(pkgdatadir)/doc/doxygen/
endif


## Analyses diffs between releases
DOCS += anadiff/anadiffs.txt
anadiff/anadiffs.txt: $(top_srcdir)/analyses $(abs_srcdir)/anadiff/diffanas
	cd anadiff && \
    LD_LIBRARY_PATH=$(abs_top_builddir)/src/.libs:$(FASTJETLIBPATH):$(FJCONTRIBLIBPATH):$(HEPMCLIBPATH):$(HEPMC3LIBPATH):$(YODALIBPATH) \
    DYLD_LIBRARY_PATH=$(abs_top_builddir)/src/.libs:$(FASTJETLIBPATH):$(FJCONTRIBLIBPATH):$(HEPMCLIBPATH):$(HEPMC3LIBPATH):$(YODALIBPATH) \
    PYTHONPATH=$(shell realpath $(abs_top_builddir)/pyext/build/lib*):$(YODA_PYTHONPATH):$(PYTHONPATH) \
    RIVET_ANALYSIS_PATH=$(abs_top_builddir)/analyses:: \
    PATH=$(abs_top_srcdir)/bin:$(PATH) \
    $(abs_srcdir)/anadiff/diffanas > anadiffs.txt


## TODO: Add a Markdown compiler and README.md + tutorials/: (py)pandoc?
EXTRA_DIST += tutorials
#if WITH_PANDOC
#for i in *.md; do j=${i/.md/.html}; echo "Making tutorial $j"; pandoc -f markdown -t html $i > $j; done
#endif

################


RSH = rsync
DEST = login.hepforge.org:rivet/public_html/


.PHONY = doc dox json anadiff coverage upload

doc: $(DOCS)
	@true

html-local: analists/analyses.html
	@true

json: analists/analyses.json
	@true

dat: analists/analyses.dat
	@true

anadiff: anadiff/anadiffs.txt
	@true

coverage: coverage/rivet-coverage.html
	@true

## TODO: upload *and* make version-numbered copies on the server
upload: $(DOCS) $(DOCDIRS)
	$(RSH) -r $? $(DEST)


all-local:
	@echo "No default target in doc: run 'make doc' to build documentation"
	@true

install-data-local:
	@echo "No local docs installation: copy the documentation to wherever you require"
	@true

clean-local:
	@rm -rf coverage/rivet-coverage-*.html $(DOCS) $(DOCDIRS)
	@rm -rf $(builddir)/doxy

uninstall-local:
	@rm -rf $(DESTDIR)$(pkgdatadir)/doc
	@rm -rf $(DESTDIR)$(pkgdatadir)/analyses
	@rm -f $(DESTDIR)$(pkgdatadir)/analyses.*
	@rm -rf $(DESTDIR)$(pkgdatadir)/refyodas
	@test ! -d $(DESTDIR)$(pkgdatadir) || rmdir $(DESTDIR)$(pkgdatadir) || true
