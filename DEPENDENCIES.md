# HEPrpms package dependencies

This file records the dependencies **between packages that are packaged in this repository**.
It was derived from the `BuildRequires:` / `Requires:` lines of the newest spec file of every package
(newest version directory, as of November 2026). Dependencies on packages that come from the distribution
(compilers, python, cmake, TeX, `root`, `lhapdf`, `HepMC`, `HepMC3`, `pythia8`, `hdf5`, `boost`, `gsl`, ...) are
listed separately at the end and are not part of the graph.

The repository only keeps package versions that are built in the HEPrpms or HEPrpmsSUSE COPR projects, versions
newer than those (pending updates), and the `HEPrpms` meta package (see `scripts/git-rm-unused.sh`).
`CGAL`, `DD4hep`, `imake`, `motif`, `pythia8` and `transfig` were removed from the repository because COPR does not
build them; their dependents use the distribution packages of the same name.

When a package is bumped, re-check its `BuildRequires:`/`Requires:` and update this file if the
repository-internal dependencies changed.

## Conventions

- **B** = needed at build time (`BuildRequires`), **R** = needed at run time (`Requires`).
  An entry without a marker is both.
- A dependency on `X` includes its `-devel`, `-data`, `python3-X` etc. subpackages where relevant.
- Spec files contain Fedora/EPEL and openSUSE branches. The names differ between them, but they refer to the
  same repository package or distribution package:

  | openSUSE name        | Fedora/EPEL name |
  |----------------------|------------------|
  | `libpythia8`, `pythia-devel`, `pythia-doc` | `pythia8`, `pythia8-devel` |
  | `libHepMC4`, `HepMC2-devel`   | `HepMC`, `HepMC-devel` |
  | `libHepMC3-4`        | `HepMC3` |
  | `libLHAPDF`, `LHAPDF-devel`   | `lhapdf`, `lhapdf-devel` |
  | `libqd0`             | `qd` |
  | `root6`, `root6-libs`, `root6-devel` | `root`, `root-*` |
  | `cgal-devel`         | `CGAL-devel` (distribution package) |

- `HEPrpms` is a meta package that `Requires:` pinned versions of nearly every other package. It is not part of the
  graph below (see [The HEPrpms meta package](#the-heprpms-meta-package)).
- There are no dependency cycles.

## Direct dependencies per package

Only repository-internal dependencies are listed. Packages not listed here have no dependency on other
packages of this repository (see [Leaf packages](#leaf-packages-no-repository-dependencies)).

| Package | Depends on |
|---------|------------|
| applgrid | hoppet |
| blackhat | qd |
| cascade | fastjet, fjcontrib (B), pythia6 (R), Rivet, tmdlib, YODA |
| cernlib | xbae |
| chaplin | hoppet (B) |
| COCOA | clhep, fastjet, geant4, PTL (all B) |
| collier | fastjet, hoppet |
| Delphes | fastjet (B) |
| EvtGen | PHOTOS, TAUOLA |
| fastnlo | fastjet, qcdnum, hoppet (B) |
| fjcontrib | fastjet |
| geant4 | clhep (B), PTL (B) |
| ggvvamp | ginac |
| gosam | form (R), qgraf (R) |
| Herwig | ThePEG, MG5_aMC, EvtGen, fastjet, gosam, njet, openloops, VBFNLO, lhapdf-sets-Herwig; PHOTOS (B), TAUOLA (B) |
| HJets | Herwig (B), ThePEG (B) |
| JetVHeto | chaplin, hoppet |
| LCIO | clhep (B) |
| njet | qd (B) |
| nlox | qcdloop (B) |
| PHOTOS | TAUOLA |
| Professor | YODA |
| qqvvamp | ginac |
| rapgap | ariadne (R), fastjet, Rivet, YODA |
| recola | collier |
| recola2 | collier, recola2-SM |
| recola2-SM | collier |
| Rivet | fastjet, fjcontrib, highfive, YODA |
| SHERPA-MC | blackhat, fastjet, MCFM, MG5_aMC, openloops, qd, recola, Rivet, YODA; hztool (R) |
| TheP8I | ThePEG |
| ThePEG | fastjet, fjcontrib, MG5_aMC, Rivet, YODA |
| topdrawer | ugs, f2c (B) |
| whizard | fastjet, gosam, hoppet, LCIO, LoopTools, openloops, recola, TAUOLA; hevea (B), noweb (B), lhapdf-sets-whizard (B) |
| YODA | highfive (B) |

Notes:

- `ginac` is also provided by some distributions; `ggvvamp`/`qqvvamp` use it from whichever source is available.
- `collier` lists `fastjet` and `hoppet` in the spec even though they are not obviously needed by the library itself.
- `lhapdf-sets-Herwig` and `lhapdf-sets-whizard` need only the distribution `lhapdf` (to download the PDF sets).
- `OpenPHIGS`, like `cernlib` and `xbae`, requires Motif from the distribution (`motif-devel`, or `openmotif-devel` on EPEL 8/9).
- `iminuit`, `python-uproot4`, `python-mpmath` and `sympy` are Python packages; they have no repository dependencies.

## Leaf packages (no repository dependencies)

apfel, ariadne, binder, clhep, cuba, f2c, f90cache, fastjet, FeynHiggs, form, ginac, golem95, gosam-contrib, HepPDT,
hevea, highfive, hoppet, hztool, iminuit, lhapdf-sets-Herwig\*, lhapdf-sets-whizard\*, LoopTools, MC-TESTER, MCFM,
MG5_aMC, nlojet++, noweb, openloops, OpenPHIGS, PTL, pythia6, python-mpmath, python-uproot4, qcdloop, qcdnum, qd,
qgraf, sympy, TAUOLA, tmdlib, ugs, VBFNLO, xbae

\* these need only the distribution `lhapdf`.

## Build order

Packages in a layer depend only on packages in lower layers, so a layer can be built (e.g. on COPR) once all
previous layers are built.

| Layer | Packages |
|-------|----------|
| 0 | apfel, ariadne, binder, clhep, cuba, f2c, f90cache, fastjet, FeynHiggs, form, ginac, golem95, gosam-contrib, HepPDT, hevea, highfive, hoppet, hztool, iminuit, lhapdf-sets-Herwig, lhapdf-sets-whizard, LoopTools, MC-TESTER, MCFM, MG5_aMC, nlojet++, noweb, openloops, OpenPHIGS, PTL, pythia6, python-mpmath, python-uproot4, qcdloop, qcdnum, qd, qgraf, sympy, TAUOLA, tmdlib, ugs, VBFNLO, xbae |
| 1 | applgrid, blackhat, cernlib, chaplin, collier, Delphes, fastnlo, fjcontrib, geant4, ggvvamp, gosam, LCIO, njet, nlox, PHOTOS, qqvvamp, topdrawer, YODA |
| 2 | COCOA, EvtGen, JetVHeto, Professor, recola, recola2-SM, Rivet |
| 3 | cascade, rapgap, recola2, SHERPA-MC, ThePEG, whizard |
| 4 | Herwig, TheP8I |
| 5 | HJets |

## Reverse dependencies (what must be rebuilt when a package changes)

Direct dependents only; follow the chain for transitive ones.

| Package | Used by |
|---------|---------|
| ariadne | rapgap |
| blackhat | SHERPA-MC |
| chaplin | JetVHeto |
| clhep | COCOA, geant4, LCIO |
| collier | recola, recola2, recola2-SM |
| EvtGen | Herwig |
| f2c | topdrawer |
| fastjet | cascade, COCOA, collier, Delphes, fastnlo, fjcontrib, Herwig, rapgap, Rivet, SHERPA-MC, ThePEG, whizard |
| fjcontrib | cascade, Rivet, ThePEG |
| form | gosam |
| geant4 | COCOA |
| ginac | ggvvamp, qqvvamp |
| gosam | Herwig, whizard |
| Herwig | HJets |
| hevea | whizard |
| highfive | Rivet, YODA |
| hoppet | applgrid, chaplin, collier, fastnlo, JetVHeto, whizard |
| hztool | SHERPA-MC |
| LCIO | whizard |
| lhapdf-sets-Herwig | Herwig |
| lhapdf-sets-whizard | whizard |
| LoopTools | whizard |
| MCFM | SHERPA-MC |
| MG5_aMC | Herwig, SHERPA-MC, ThePEG |
| njet | Herwig |
| noweb | whizard |
| openloops | Herwig, SHERPA-MC, whizard |
| PHOTOS | EvtGen, Herwig |
| PTL | COCOA, geant4 |
| pythia6 | cascade |
| qcdloop | nlox |
| qcdnum | fastnlo |
| qd | blackhat, njet, SHERPA-MC |
| qgraf | gosam |
| recola | SHERPA-MC, whizard |
| recola2-SM | recola2 |
| Rivet | cascade, rapgap, SHERPA-MC, ThePEG |
| TAUOLA | EvtGen, Herwig, PHOTOS, whizard |
| ThePEG | Herwig, HJets, TheP8I |
| tmdlib | cascade |
| ugs | topdrawer |
| VBFNLO | Herwig |
| xbae | cernlib |
| YODA | cascade, Professor, rapgap, Rivet, SHERPA-MC, ThePEG |

## Dependency graph

Arrows point from a package to the package it depends on. Only packages with at least one repository dependency
are shown.

```mermaid
graph LR
  Herwig --> ThePEG & MG5_aMC & EvtGen & gosam & njet & openloops & VBFNLO & fastjet & PHOTOS & TAUOLA & lhapdf-sets-Herwig
  HJets --> Herwig & ThePEG
  TheP8I --> ThePEG
  ThePEG --> MG5_aMC & Rivet & YODA & fjcontrib & fastjet
  Rivet --> YODA & fjcontrib & fastjet & highfive
  YODA --> highfive
  fjcontrib --> fastjet
  EvtGen --> PHOTOS & TAUOLA
  PHOTOS --> TAUOLA
  gosam --> form & qgraf
  njet --> qd
  SHERPA-MC --> blackhat & MCFM & MG5_aMC & openloops & recola & qd & hztool & fastjet & Rivet & YODA
  blackhat --> qd
  whizard --> recola & gosam & openloops & LCIO & TAUOLA & LoopTools & hoppet & fastjet & hevea & noweb & lhapdf-sets-whizard
  LCIO --> clhep
  recola --> collier
  recola2 --> collier & recola2-SM
  recola2-SM --> collier
  collier --> fastjet & hoppet
  cascade --> Rivet & YODA & tmdlib & fastjet & fjcontrib & pythia6
  rapgap --> ariadne & Rivet & YODA & fastjet
  Professor --> YODA
  COCOA --> geant4 & clhep & PTL & fastjet
  geant4 --> clhep & PTL
  Delphes --> fastjet
  fastnlo --> qcdnum & hoppet & fastjet
  applgrid --> hoppet
  JetVHeto --> chaplin & hoppet
  chaplin --> hoppet
  nlox --> qcdloop
  ggvvamp --> ginac
  qqvvamp --> ginac
  cernlib --> xbae
  topdrawer --> ugs & f2c
```

## The HEPrpms meta package

`HEPrpms/<version>/HEPrpms.spec` has no sources; it `Requires:` an exact version (`== x.y.z`) of nearly every
package and subpackage in this repository, so that installing `HEPrpms` pulls in the whole collection. The pinned
versions are updated by hand and may lag behind the newest version directories; some of the pinned versions are no
longer present in the repository. Any new package that should be part of the collection must be added there as well.
The meta package is kept in the repository even though COPR does not build it.

## Dependencies on distribution packages

Not packaged here; they come from Fedora/EPEL/openSUSE.

| Distribution package | Used by |
|----------------------|---------|
| `root` (`root-core`, `root-tree-ntuple-utils`, ...) | applgrid, cascade, COCOA, Delphes, EvtGen, fastnlo, LCIO, MC-TESTER, Professor, SHERPA-MC, tmdlib, VBFNLO, whizard |
| `lhapdf` | apfel, cascade, fastnlo, Herwig, JetVHeto, lhapdf-sets-\*, MCFM, TAUOLA, TheP8I, ThePEG, tmdlib, VBFNLO, whizard |
| `HepMC` (HepMC2) | cascade, COCOA, Delphes, EvtGen, MC-TESTER, PHOTOS, rapgap, TAUOLA, VBFNLO |
| `HepMC3` (`-search`, `-rootIO`) | cascade, EvtGen, MC-TESTER, PHOTOS, rapgap, Rivet, SHERPA-MC, TAUOLA, ThePEG, whizard |
| `hdf5` | highfive, Rivet, whizard |
| `boost` | Herwig, HJets, ThePEG, tmdlib, whizard |
| `gsl` | cascade, HJets, Herwig, Rivet, TheP8I, ThePEG, tmdlib, VBFNLO |
| `tbb` | PTL |
| `xerces-c`, `expat` | COCOA, geant4 |
| `cln` | ginac, ggvvamp, qqvvamp |
| `yaml-cpp` | fastnlo, JetVHeto, YODA |
| `openmpi` | SHERPA-MC, whizard |
| `ocaml` | hevea, whizard |
| `clang`, `llvm`, `pybind11` | binder, iminuit |
| `eigen3` | Professor |
| TeX Live / `ghostscript` / `doxygen` | documentation builds of most packages |
| Python (`python3-devel`, `Cython`, `numpy`, `setuptools`, `matplotlib`) | Python bindings of apfel, fastjet, fastnlo, LCIO, Professor, Rivet, whizard, YODA, MG5_aMC, openloops, njet |
| X11 / `libX*` | cernlib, OpenPHIGS, topdrawer, ugs, xbae |
| `pythia8` (`pythia8-devel`; `libpythia8`, `pythia-devel` on openSUSE) | cascade, COCOA, Delphes, EvtGen, Herwig, PHOTOS, rapgap, SHERPA-MC, TAUOLA, TheP8I, whizard |
| `CGAL` (`CGAL-devel`) | fastjet |
| `motif` (`motif-devel`; `openmotif-devel` on EPEL 8/9) | cernlib, OpenPHIGS, xbae |
| `imake` | topdrawer, ugs |
| `transfig` | ginac |
| `glew`, `gl2ps`, `mesa-libGLU` (`Mesa-libGLU` on openSUSE), `libpng` | OpenPHIGS |
