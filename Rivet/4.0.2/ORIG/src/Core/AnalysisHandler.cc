// -*- C++ -*-
#include "Rivet/Config/RivetCommon.hh"
#include "Rivet/AnalysisHandler.hh"
#include "Rivet/Analysis.hh"
#include "Rivet/Tools/ParticleName.hh"
#include "Rivet/Tools/BeamConstraint.hh"
#include "Rivet/Tools/RivetPaths.hh"
#include "Rivet/Tools/RivetYODA.hh"
#include "Rivet/Tools/Logging.hh"
#include "Rivet/Projections/Beam.hh"

#include "YODA/Counter.h"
#include "YODA/Histo.h"
#include "YODA/Profile.h"
#include "YODA/Scatter.h"
#include "YODA/IO.h"
#include "YODA/WriterYODA.h"

#include <iostream>
#include <regex>

using namespace std;

namespace {

  /// Fold expression to call lambda @a F with
  /// each argument in the variadic set
  template<typename... Args, typename F>
  constexpr void for_each_arg(F&& f) {
    (( f(Args{}) ), ...);
  }

}

namespace Rivet {


  AnalysisHandler::AnalysisHandler(const string& runname)
    : _runname(runname),
      _ntrials(0.0),
      _isEndOfFile(false),
      _userxs{NAN, NAN},
      _initialised(false),
      _checkBeams(true),
      _skipMultiWeights(false),
      _matchWeightNames(""),
      _unmatchWeightNames(""),
      _nominalWeightName(""),
      _weightCap(0.),
      _NLOSmearing(0.),
      _defaultWeightIdx(0),
      _rivetDefaultWeightIdx(0),
      _customDefaultWeightIdx(-1),
      _dumpPeriod(0), _dumping(false) {
    registerDefaultTypes<double,int,string>();
  }


  AnalysisHandler::~AnalysisHandler() {
    static bool printed = false;
    // Print out MCnet boilerplate
    if (!printed && getLog().getLevel() <= 20) {
      cout << endl
           << "The MCnet usage guidelines apply to Rivet: see http://www.montecarlonet.org/GUIDELINES" << endl
           << "Please acknowledge Rivet in results made using it, and cite https://arxiv.org/abs/1912.05451" << endl;
      // << "https://arxiv.org/abs/1003.0694" << endl;
      printed = true;
    }
  }


  /// @todo Can we inline this?
  Log& AnalysisHandler::getLog() const {
    return Log::getLog("Rivet.AnalysisHandler");
  }


  /// http://stackoverflow.com/questions/4654636/how-to-determine-if-a-string-is-a-number-with-c
  namespace {
    bool is_number(const std::string& s) {
      std::string::const_iterator it = s.begin();
      while (it != s.end() && std::isdigit(*it)) ++it;
      return !s.empty() && it == s.end();
    }
  }

  /// Check if any of the weight names is not a number
  bool AnalysisHandler::haveNamedWeights() const {
    bool dec = false;
    for (size_t i = 0; i <_weightNames.size(); ++i) {
      const string& s = _weightNames[i];
      if (!is_number(s)) {
        dec = true;
        break;
      }
    }
    return dec;
  }


  void AnalysisHandler::init(const GenEvent& ge) {
    if (_initialised)
      throw UserError("AnalysisHandler::init has already been called: cannot re-initialize!");

    /// @todo Should the Rivet analysis objects know about weight names?

    /// @todo Get the HepMC3::GenRunInfo object from the first event and store/wrap it?

    // Assemble the weight streams to be used
    setWeightNames(ge);
    if (_skipMultiWeights) {
      MSG_INFO("Only using nominal weight: variation weights will be ignored");
    } else if (haveNamedWeights()) {
      MSG_DEBUG("Using named weights");
    } else {
      MSG_WARNING("NOT using named weights: assuming first weight is nominal");
    }

    // Set the Run's beams based on this first event
    /// @todo Improve this const ugliness
    const Event evt(const_cast<GenEvent&>(ge), _weightIndices);
    setRunBeams(Rivet::beams(evt));

    MSG_DEBUG("Initialising the analysis handler");
    _eventNumber = ge.event_number();

    // Set bootstrap file if a name has been set
    if (!_bootstrapfilename.empty()) _fbootstrap = ofstream(_bootstrapfilename);

    // Create the multi-weighted event counter
    _eventCounter = CounterPtr(weightNames(), Counter("_EVTCOUNT"));

    // Create a per-file event counter
    _fileCounter = CounterPtr(weightNames(), Counter("_FILECOUNT"));

    // Create a running counter for the cross-section variance
    _xserr = CounterPtr(weightNames(), Counter("XSECERR"));

    // Set the cross section based on what is reported by the init-event, else zero
    if (ge.cross_section())  setCrossSection(evt.crossSections());
    else {
      MSG_DEBUG("No cross-section detected in first event: setting default to 0 pb");
      setCrossSection({0.0, 0.0});
    }

    // Check that analyses are beam-compatible, and note those that aren't
    const size_t num_anas_requested = analysisNames().size();
    vector<string> anamestodelete;
    for (const AnaHandle& a : analyses()) {
      if (_checkBeams && !a->compatibleWithRun()) anamestodelete += a->name();
    }
    // Remove incompatible analyses from the run
    for (const string& aname : anamestodelete) {
      MSG_WARNING("Analysis '" << aname << "' is incompatible with the provided beams: removing");
      removeAnalysis(aname);
    }
    if (num_anas_requested > 0 && analysisNames().empty()) {
      throw Error("All analyses were incompatible with the first event's beams");
    }

    // Warn if any analysis' status is not unblemished
    for (const AnaHandle& a : analyses()) {
      if ( a->info().preliminary() ) {
        MSG_WARNING("Analysis '" << a->name() << "' is preliminary: be careful, it may change and/or be renamed!");
      } else if ( a->info().obsolete() ) {
        MSG_WARNING("Analysis '" << a->name() << "' is obsolete: please update!");
      } else if (( a->info().unvalidated() ) ) {
        MSG_WARNING("Analysis '" << a->name() << "' is unvalidated: be careful, it may be broken!");
      }
    }

    // Initialize the remaining analyses
    _stage = Stage::INIT;
    for (const AnaHandle& a : analyses()) {
      MSG_DEBUG("Initialising analysis: " << a->name());
      try {
        // Allow projection registration in the init phase onwards
        a->_allowProjReg = true;
        a->init();
        a->setProjectionHandler(_projHandler);
        a->syncDeclQueue();
        //MSG_DEBUG("Checking consistency of analysis: " << a->name());
        //a->checkConsistency();
      } catch (const Error& err) {
        throw Error(a->name() + "::init method error: " + err.what());
      }
      MSG_DEBUG("Done initialising analysis: " << a->name());
    }
    _stage = Stage::OTHER;
    _initialised = true;
    MSG_DEBUG("Analysis handler initialised");

    // Write out list of analysis objects to bootstrap file, if active
    if (_fbootstrap.is_open()) {
      _fbootstrap << "#";
      for (const AnaHandle& a : analyses()) {
        for (const auto& ao : a->analysisObjects()) {
          ao.get()->initBootstrap();
          _fbootstrap << " " << ao.get()->basePath();
          _fbootstrap << "," << ao.get()->fillOutcomes().size();
        }
      }
      _fbootstrap << endl;
    }

  }


  // void AnalysisHandler::init(GenEvent* ge) {
  //   if (ge == nullptr) {
  //     MSG_ERROR("AnalysisHandler received null pointer to GenEvent");
  //     //throw Error("AnalysisHandler received null pointer to GenEvent");
  //   }
  //   init(*ge);
  // }


  void AnalysisHandler::setWeightNames(const GenEvent& ge) {
    setWeightNames( HepMCUtils::weightNames(ge) );
  }

  void AnalysisHandler::setWeightNames(const vector<string>& weightNames) {
    _weightNames = weightNames;

    // If there are no weights, add a nominal one
    if (_weightNames.empty()) {
      _weightNames.push_back("");
      _customDefaultWeightIdx =_rivetDefaultWeightIdx = _defaultWeightIdx = 0;
      _weightIndices = { 0 };
      return;
    }

    // Find default weights, starting with the chosen or preferred name (default = "")
    size_t nDefaults = 0;
    int customNomIdx = -1;
    string nom_winner = "";
    vector<string> nom_shortlist;
    _weightIndices.clear();
    for (size_t i = 0, N = _weightNames.size(); i < N; ++i) {
      _weightIndices.push_back(i);
      if (_weightNames[i] == "") {
        nom_shortlist.push_back("'"+ _weightNames[i] +"'");
        if (nDefaults == 0) {
          nom_winner = _weightNames[i];
          _defaultWeightIdx = i;
        }
        ++nDefaults;
      }
      else if (customNomIdx < 0 && _weightNames[i] == _nominalWeightName) {
        customNomIdx = i;
      }
    }

    // If there are no weights with the preferred name, look for acceptable alternatives
    if (nDefaults == 0) {
      for (size_t i = 0, N = _weightNames.size(); i < N; ++i) {
        const string W = toUpper(_weightNames[i]);
        if (W == "WEIGHT" || W == "0" || W == "DEFAULT" || W == "NOMINAL") {
          nom_shortlist.push_back("'"+ _weightNames[i] +"'");
          if (nDefaults == 0 || (int)i == customNomIdx) {
            nom_winner = _weightNames[i];
            _defaultWeightIdx = i;
          }
          ++nDefaults;
        }
      }
    }

    // If no nominal weight could be identified,
    // check whether the user has specified a nominal
    if (nDefaults == 0 && customNomIdx >=0) {
      ++nDefaults;
      nom_winner = _weightNames[customNomIdx];
      _defaultWeightIdx = customNomIdx;
      customNomIdx = -1;
    }

    // Warn user if still no nominal weight could be identified
    if (nDefaults == 0) {
      MSG_WARNING("Could not identify nominal weight. Will continue assuming variations-only run.");
      // Put candidates in quotes in case weight name contains whitespace
      MSG_WARNING("Candidate weight names:\n    '" << join(_weightNames, "'\n    '") << "'");
    }

    // Warn if multiple weight names were acceptable alternatives
    if (nDefaults > 1) {
      MSG_WARNING("Found " << nDefaults << " default weight candidates: "
                  << join(nom_shortlist, ", ") << ". Will use: '"
                  <<  nom_winner << "'");
    }

    if (_skipMultiWeights) {

      // If running in single-weight mode, remove all bar the nominal weight
      _weightIndices.clear();
      _weightNames.clear();
      if (customNomIdx >= 0 && customNomIdx != (int)_defaultWeightIdx) {
        _weightIndices.push_back(customNomIdx);
        _weightNames.push_back(_weightNames[customNomIdx]);
        _customDefaultWeightIdx = 0;
        MSG_WARNING("Specified nominal weight different from auto-detected nominal weight. Will retain both.");
      }
      _weightIndices.push_back(_defaultWeightIdx);
      _weightNames.push_back(_weightNames[_defaultWeightIdx]);
      _rivetDefaultWeightIdx = _weightIndices.size() - 1;

    }
    else {

      // Check if weight name matches a supplied string/regex and filter to select those only
      if (_matchWeightNames != "") {
        MSG_DEBUG("Select weight names that match pattern \"" << _matchWeightNames << "\"");
        // Compile regex from each string in the comma-separated list
        vector<std::regex> patterns;
        for (const string& pattern : split(_matchWeightNames, ",")) {
          patterns.push_back( std::regex(pattern) );
        }
        // Check which weights match supplied weight-name pattern
        vector<string> selected_subset; vector<size_t> selected_indices;
        for (size_t i = 0, N = _weightNames.size(); i < N; ++i) {
          // The default weight cannot be "unselected"
          if (_weightIndices[i] == _defaultWeightIdx) {
            _rivetDefaultWeightIdx = selected_indices.size();
            selected_indices.push_back(_weightIndices[i]);
            selected_subset.push_back(_weightNames[i]);
            MSG_DEBUG("Selected nominal weight: \"" << _weightNames[i] << "\"");
            continue;
          }
          else if ((int)_weightIndices[i] == customNomIdx) {
            _customDefaultWeightIdx = selected_indices.size();
            selected_indices.push_back(_weightIndices[i]);
            selected_subset.push_back(_weightNames[i]);
            MSG_DEBUG("Selected custom nominal weight: " << _weightNames[i]);
            continue;
          }
          for (const std::regex& re : patterns) {
            if ( std::regex_match(_weightNames[i], re) ) {
              selected_indices.push_back(_weightIndices[i]);
              selected_subset.push_back(_weightNames[i]);
              MSG_DEBUG("Selected variation weight: \"" << _weightNames[i] << "\"");
              break;
            }
          }
        }
        _weightNames = selected_subset;
        _weightIndices = selected_indices;
      }

      // Check if the remaining weight names match supplied string/regexes and *de*select accordingly
      vector<std::regex> patterns = { std::regex("^IRREG.*", std::regex_constants::icase) };
      if (_unmatchWeightNames != "") {
        MSG_DEBUG("Deselect weight names that match pattern \"" << _unmatchWeightNames << "\"");
        // Compile regex from each string in the comma-separated list
        for (const string& pattern : split(_unmatchWeightNames, ",")) {
          patterns.push_back( std::regex(pattern) );
        }
      }
      // Check which weights match supplied weight-name pattern
      vector<string> selected_subset; vector<size_t> selected_indices;
      for (size_t i = 0, N = _weightNames.size(); i < N; ++i) {
        // The default weight cannot be vetoed
        if (_weightIndices[i] == _defaultWeightIdx) {
          _rivetDefaultWeightIdx = selected_indices.size();
          selected_indices.push_back(_weightIndices[i]);
          selected_subset.push_back(_weightNames[i]);
          MSG_DEBUG("Selected nominal weight: " << _weightNames[i]);
          continue;
        }
        else if ((int)_weightIndices[i] == customNomIdx) {
          _customDefaultWeightIdx = selected_indices.size();
          selected_indices.push_back(_weightIndices[i]);
          selected_subset.push_back(_weightNames[i]);
          MSG_DEBUG("Selected custom nominal weight: " << _weightNames[i]);
          continue;
        }
        bool skip = false;
        for (const std::regex& re : patterns) {
          if ( std::regex_match(_weightNames[i], re) ) { skip = true; break; }
        }
        if (skip) continue;
        selected_indices.push_back(_weightIndices[i]);
        selected_subset.push_back(_weightNames[i]);
        MSG_DEBUG("Selected variation weight: " << _weightNames[i]);
      }
      _weightNames = selected_subset;
      _weightIndices = selected_indices;

    }

    // Done (de-)selecting weights: show useful debug messages
    MSG_DEBUG("Default weight name: \"" <<  _weightNames[_rivetDefaultWeightIdx] << "\"");
    MSG_DEBUG("Default weight position (in Rivet): " << _rivetDefaultWeightIdx);
    MSG_DEBUG("Default weight index (in original weight vector): " << _defaultWeightIdx);

    // Set Rivet's preferred weight name for the intended nominal
    if (_customDefaultWeightIdx < 0)  _customDefaultWeightIdx = _rivetDefaultWeightIdx;
    _weightNames[_customDefaultWeightIdx] = "";

    // Write weight names into the bootstrap file, if active
    if (_fbootstrap.is_open()) {
      _fbootstrap << "#";
      for (const string& wn : _weightNames) {
        if (wn.empty()) {
          _fbootstrap << " " << nom_winner.substr(1, nom_winner.length()-2);
          continue;
        }
        _fbootstrap << " " << wn;
      }
      _fbootstrap << endl;
    }

  }


  // bool AnalysisHandler::consistentWithRun(Event& event) {
  //   const PdgIdPair beamids = beamIDs(event);
  //   const double sqrts = sqrtS(event);
  //   return compatibleBeams(beamids, runBeamIDs()) && compatibleBeamEnergy(sqrts, runSqrtS());
  // }


  void AnalysisHandler::analyze(GenEvent& ge) {
    // Call init with event as template if not already initialised
    if (!_initialised) init(ge);
    assert(_initialised);

    // Create the Rivet event wrapper
    //bool strip = ( getEnvParam("RIVET_STRIP_HEPMC", string("NOOOO") ) != "NOOOO" );
    const Event event(ge, _weightIndices);

    // Ensure that beam details match those from the first event (if we're checking beams)
    if (_checkBeams) {
      const ParticlePair evtbeams = beams(event);
      const double sqrts = Rivet::sqrtS(event);
      MSG_DEBUG("Event beams = " << evtbeams << " at sqrt(s) = " << sqrts/GeV << " GeV");
      if (evtbeams.first.pid() == PID::ANY && evtbeams.second.pid() == PID::ANY) {
        throw Error("No event beams found: please fix the events, or run with beam-checking disabled");
      }
      if (!compatibleBeams(evtbeams, _beams) || !fuzzyEquals(sqrts, this->runSqrtS())) {
        ostringstream errmsg;
        errmsg << "Event beams mismatch with run: "
                  << PID::toBeamsString(beamIDs(event)) << " @ " << sqrts/GeV << " GeV" << " vs. expected "
                  << this->runBeams() << " @ " << this->runSqrtS()/GeV << " GeV";
        throw Error(errmsg.str());
      }
    }

    // If the HepMC file has changed, keep track of the best cross-section estimate from the
    // previous file before updating the cross-section using the information from the new file
    if (_isEndOfFile)  updateCrossSection();

    // Set the cross section based on what is reported by this event
    if (ge.cross_section())  setCrossSection(event.crossSections());

    // If the event number has changed, sync the sub-event analysis objects to persistent
    // NB. Won't happen for first event because _eventNumber is set in init()
    /// @todo Need to be able to turn this off, in the case of slightly malformed events without event numbers
    if (_eventNumber != ge.event_number()) {
      collapseEventGroup();
      _eventNumber = ge.event_number();

      // Dump current final histograms
      if ( _dumpPeriod > 0 && numEvents() > 0 && numEvents() % _dumpPeriod == 0 ) {
        MSG_DEBUG("Dumping intermediate results to " << _dumpFile << ".");
        _dumping = numEvents()/_dumpPeriod;
        finalize();
        writeData(_dumpFile);
        _dumping = 0;
      }

    }

    // Make a new sub-event: affects every analysis object
    MSG_DEBUG("Starting new sub-event");
    _eventCounter.get()->newSubEvent();
    _fileCounter.get()->newSubEvent();
    for (const AnaHandle& a : analyses()) {
      for (const auto& ao : a->analysisObjects()) {
        ao.get()->newSubEvent();
      }
    }

    // Append to sub-event weights list, modulo weight-capping to avoid spikes
    _subEventWeights.push_back(event.weights());
    if (_weightCap != 0.) {
      MSG_DEBUG("Implementing weight cap using a maximum |weight| = " << _weightCap << " for latest subevent");
      size_t lastSub = _subEventWeights.size() - 1;
      for (size_t i = 0; i < _subEventWeights[lastSub].size(); ++i) {
        if (abs(_subEventWeights[lastSub][i]) > _weightCap) {
          _subEventWeights[lastSub][i] = sign(_subEventWeights[lastSub][i]) * _weightCap;
        }
      }
    }
    MSG_DEBUG("Analyzing subevent #" << _subEventWeights.size() - 1 << ".");

    // Warn if the subevent list is getting very long without flushing
    if (_subEventWeights.size() % 1000 == 0) {
      MSG_WARNING("Sub-event weight list has " << _subEventWeights.size()
                  << " elements: are the weight numbers correctly set in the input events?");
    }

    // Update the event counter
    // NB. updated on sub-events, but synced cf. histos on full-event boundaries
    _eventCounter->fill();
    _fileCounter->fill();

    // Run the analyses
    for (const AnaHandle& a : analyses()) {
      MSG_TRACE("About to run analysis " << a->name());
      try {
        a->analyze(event);
      } catch (const Error& err) {
        throw Error(a->name() + "::analyze method error: " + err.what());
      } catch (const std::bad_cast &err) {
        std::string message = a->name() + "::analyze method error: " + err.what();
        message += ". A type mismatch occurred, possibly due to attempting to retrieve a different type in ::analyze than was specified in ::init.";
        throw Error(message);
      }
      MSG_TRACE("Finished running analysis " << a->name());
    }

  }


  void AnalysisHandler::analyze(GenEvent* ge) {
    if (ge == nullptr) {
      MSG_ERROR("AnalysisHandler received null pointer to GenEvent");
      //throw Error("AnalysisHandler received null pointer to GenEvent");
    }
    analyze(*ge);
  }


  void AnalysisHandler::collapseEventGroup() {
    if ( _subEventWeights.empty() ) return;
    MSG_TRACE("AnalysisHandler::analyze(): Pushing _eventCounter to persistent.");
    _eventCounter.get()->collapseEventGroup(_subEventWeights);
    _fileCounter.get()->collapseEventGroup(_subEventWeights);
    for (const AnaHandle& a : analyses()) {
      for (const auto& ao : a->analysisObjects()) {
        MSG_TRACE("AnalysisHandler::analyze(): Pushing " << a->name()
                  << "'s " << ao->name() << " to persistent.");
        ao.get()->collapseEventGroup(_subEventWeights, _NLOSmearing);
      }
      MSG_TRACE("AnalysisHandler::analyze(): finished pushing "
                << a->name() << "'s objects to persistent.");
    }

    // Write out bootstrap acceptances if possible
    /// @todo Only a placeholder... complete by writing out actual bin-weights from instrumented ao wrappers
    if (_fbootstrap.is_open()) {
      if (_subEventWeights.size() == 1) { // correlated subevents currently not supported
        // write out weights
        _fbootstrap << "W";
        for (const double sew : _subEventWeights[0]) {
          _fbootstrap << " " << sew;
        }
        _fbootstrap << endl;
        // write out fill outcomes
        _fbootstrap << "O";
        for (const bool out : fillOutcomes()) {
          _fbootstrap << " " << out;
        }
        _fbootstrap << endl;
        // write out fill fractions
        _fbootstrap << "F";
        for (const double frac : fillFractions()) {
          _fbootstrap << " " << frac;
        }
        _fbootstrap << endl;
      }
    }

    // Clean up
    _subEventWeights.clear();
  }


  vector<pair<string,size_t>> AnalysisHandler::fillLayout() const {
    if (!_initialised)
      throw UserError("AnalysisHandler::init has not been called yet!");
    vector<pair<string,size_t>> rtn;
    for (const AnaHandle& a : analyses()) {
      for (const auto& ao : a->analysisObjects()) {
        rtn.push_back({ao.get()->basePath(), ao.get()->fillOutcomes().size() });
      }
    }
    return rtn;
  }


  vector<bool> AnalysisHandler::fillOutcomes() const {
    vector<bool> rtn;
    for (const AnaHandle& a : analyses()) {
      for (const auto& ao : a->analysisObjects()) {
        for (bool res : ao.get()->fillOutcomes()) {
          rtn.push_back(res);
        }
      }
    }
    return rtn;
  }


  vector<double> AnalysisHandler::fillFractions() const {
    vector<double> rtn;
    for (const AnaHandle& a : analyses()) {
      for (const auto& ao : a->analysisObjects()) {
        for (double res : ao.get()->fillFractions()) {
          rtn.push_back(res);
        }
      }
    }
    return rtn;
  }


  void AnalysisHandler::finalize() {
    if (!_initialised) return;
    MSG_DEBUG("Finalising analyses");

    _stage = Stage::FINALIZE;

    // First push all analyses' objects to persistent and final
    MSG_TRACE("AnalysisHandler::finalize(): Pushing analysis objects to persistent.");
    collapseEventGroup();
    // Warn if no cross-section was set
    if (!nominalCrossSection()) {
      MSG_WARNING("Null nominal cross-section: setting to 10^-10 pb to allow rescaling");
      setCrossSection(1.0e-10, 0.0);
    }
    // update Ntrials heuristic calculation
    const double ntrials = _ntrials + safediv(_fileCounter.get()->persistent(defaultWeightIndex())->sumW(),
                                              _xs.get()->persistent(defaultWeightIndex())->val());
    if (ntrials != 0.) {
      const double nFiles = _xserr.get()->persistent(defaultWeightIndex())->numEntries() + 1.0;
      for (size_t iW = 0; iW < numWeights(); ++iW) {
        const double sumw  = _eventCounter.get()->persistent(iW)->sumW();
        //const double sumw2 = _eventCounter.get()->persistent(iW)->sumW2();
        const double lastXSE = _xs.get()->persistent(iW)->totalErrAvg();
        const double xse2 = _xserr.get()->persistent(iW)->sumW2() + sqr(lastXSE);
        const double xse = _xserr.get()->persistent(iW)->sumW() + lastXSE;
        const double xs = sumw / ntrials;
        // This would be exact, but we don't have the actual number of trials
        //xserr = (sumw2/ntrials) -  sqr(sumw/ntrials);
        //xserr /= ntrials - 1;
        // Work out variance of cross-section uncertainties instead,
        // unless there is only one file to begin with
        double xserr = xse/nFiles;
        if (nFiles > 1.0)  xserr += sqrt(xse2/nFiles - sqr(xse/nFiles));
        _xs.get()->persistent(iW)->reset();
        _xs.get()->persistent(iW)->set(xs, xserr);
      }
    }

    // Copy all histos to finalize versions.
    _eventCounter.get()->pushToFinal();
    _xs.get()->pushToFinal();
    for (const AnaHandle& a : analyses()) {
      for (const auto& ao : a->analysisObjects()) {
        ao.get()->pushToFinal();
      }
    }

    // Run finalize for each supporting analysis
    for (const AnaHandle& a : analyses()) {
      if ( _dumping && !a->info().reentrant() )  {
        if ( _dumping == 1 ) { //< print on first attempt to dump
          MSG_DEBUG("Skipping finalize in periodic dump of " << a->name() << " as it is not declared re-entrant.");
        }
        continue;
      }
      for (size_t iW = 0; iW < numWeights(); ++iW) {
        _xs.get()->setActiveFinalWeightIdx(iW);
        _eventCounter.get()->setActiveFinalWeightIdx(iW);

        for (const auto& ao : a->analysisObjects()) {
          ao.get()->setActiveFinalWeightIdx(iW);
        }
        try {
          MSG_TRACE("running " << a->name() << "::finalize() for weight " << iW << ".");
          a->finalize();
        } catch (const Error& err) {
          throw Error(a->name() + "::finalize method error: " + err.what());
        }
      }
    }

    // Print out number of events processed
    _eventCounter.get()->setActiveFinalWeightIdx(defaultWeightIndex());
    _xs.get()->setActiveFinalWeightIdx(defaultWeightIndex());
    if (!_dumping) {
      const int nevts = numEvents();
      MSG_DEBUG("Processed " << nevts << " event" << (nevts != 1 ? "s" : ""));
    }

    _stage = Stage::OTHER;

  }


  AnalysisHandler& AnalysisHandler::addAnalysis(const string& analysisname, std::map<string, string> pars) {
     // Make an option handle.
    std::string parHandle = "";
    for (map<string, string>::iterator par = pars.begin(); par != pars.end(); ++par) {
      parHandle +=":";
      parHandle += par->first + "=" + par->second;
    }
    return addAnalysis(analysisname + parHandle);
  }


  AnalysisHandler& AnalysisHandler::addAnalysis(const string& analysisname) {
    // Check for a duplicate analysis
    /// @todo Might we want to be able to run an analysis twice, with different params?
    ///       Requires avoiding histo tree clashes, i.e. storing the histos on the analysis objects.
    string ananame = analysisname;
    vector<string> anaopt = split(analysisname, ":");
    if ( anaopt.size() > 1 ) ananame = anaopt[0];
    AnaHandle analysis( AnalysisLoader::getAnalysis(ananame) );
    if (analysis.get() != 0) { // < Check for null analysis.
      MSG_DEBUG("Adding analysis '" << analysisname << "'");
      map<string,string> opts;
      for ( int i = 1, N = anaopt.size(); i < N; ++i ) {
        vector<string> opt = split(anaopt[i], "=");
        if ( opt.size() != 2 ) {
          MSG_WARNING("Error in option specification. Skipping analysis " << analysisname);
          return *this;
        }
        if ( !analysis->info().validOption(opt[0], opt[1]) )
          MSG_WARNING("Setting the option '" << opt[0] << "' to '"
                      << opt[1] << "' for " << analysisname
                      << " has not been declared in the info file"
                      << " and may be ignored in the analysis.");
        opts[opt[0]] = opt[1];
      }
      for ( auto opt: opts) {
        analysis->_options[opt.first] = opt.second;
        analysis->_optstring += ":" + opt.first + "=" + opt.second;
      }
      for (const AnaHandle& a : analyses()) {
        if (a->name() == analysis->name() ) {
          MSG_WARNING("Analysis '" << analysisname << "' already registered: skipping duplicate");
          return *this;
        }
      }
      analysis->_analysishandler = this;
      _analyses[analysisname] = analysis;
    } else {
      MSG_WARNING("Analysis '" << analysisname << "' not found.");
    }
    // MSG_WARNING(_analyses.size());
    // for (const AnaHandle& a : _analyses) MSG_WARNING(a->name());
    return *this;
  }


  AnalysisHandler& AnalysisHandler::removeAnalysis(const string& analysisname) {
    MSG_DEBUG("Removing analysis '" << analysisname << "'");
    if (_analyses.find(analysisname) != _analyses.end()) _analyses.erase(analysisname);
    // }
    return *this;
  }


  void AnalysisHandler::stripOptions(YODA::AnalysisObjectPtr ao,
                                     const vector<string> & delopts) const {
    string path = ao->path();
    string ananame = split(path, "/")[0];
    vector<string> anaopts = split(ananame, ":");
    for ( int i = 1, N = anaopts.size(); i < N; ++i )
      for ( auto opt : delopts )
        if ( opt == "*" || anaopts[i].find(opt + "=") == 0 )
          path.replace(path.find(":" + anaopts[i]), (":" + anaopts[i]).length(), "");
    ao->setPath(path);
  }


  /// @todo Should really be mergeYODAs()...
  void AnalysisHandler::mergeYODAs(const vector<string> &aofiles,
                                   const vector<string> &delopts,
                                   const vector<string> &addopts,
                                   const vector<string> &matches,
                                   const vector<string> &unmatches,
                                   const bool equiv, const bool reentrantOnly) {

    // Parse option adding.
    vector<string> optAnas;
    vector<string> optKeys;
    vector<string> optVals;
    for (string addopt : addopts) {
      size_t pos1 = addopt.find(":");
      size_t pos2 = addopt.find("=");
      if (pos1 == string::npos || pos2 == string::npos || pos2 < pos1) {
        MSG_WARNING("Malformed analysis option: "+addopt+". Format as :OPT=VAL or ANA:OPT=VAL");
        continue;
      }
      optAnas.push_back(addopt.substr(0, pos1));
      optKeys.push_back(addopt.substr(pos1 +1, pos2 - pos1 - 1));
      optVals.push_back(addopt.substr(pos2 +1 , addopt.size() - pos2 - 1));
    }

    // Go through all files and collect information
    /// @todo Move this to the script interface, with the API working in terms
    ///   of <real_filename,weight> pairs rather than decoding a CLI convention in C++
    bool overwrite_xsec = false;
    size_t nfiles = 0, nfilestot = aofiles.size();
    map<string, YODA::AnalysisObjectPtr> allaos;
    map<string, std::array<double,4>> allxsecs;
    for (string file : aofiles) {
      ++nfiles;
      std::cout << "[" << nfiles << "/" << nfilestot << "] Merging data file " << file << "\r";
      std::cout.flush();
      MSG_DEBUG("Reading in data from " << file);

      // Check for user-supplied scaling, assign 1 otherwise
      /// @todo
      size_t colonpos = file.rfind(":");
      double fileweight = 1.0;
      if (colonpos != string::npos) {
        string suffix = file.substr(colonpos+1);
        try {
          if (suffix.at(0) == '=') {
            // case I: file.yoda:=1.23
            //-> set cross-section to 1.23
            overwrite_xsec = true;
            suffix = suffix.substr(1);
          }
          else if (suffix.at(0) == 'x') {
            // case II: file.yoda:x1.23
            // (same as file.yoda:1.23)
            //-> multiply cross-section with 1.23
            suffix = suffix.substr(1);
          }
          fileweight = std::stod(suffix);
          file = file.substr(0, colonpos);
        } catch (...) {
          throw UserError("Unexpected error in processing argument " + file + " with file:scale format");
        }
      }

      // Try to read the file and build path-AO map
      /// @todo move this map construction into YODA?
      vector<YODA::AnalysisObject*> aos_raw;
      map<string,YODA::AnalysisObjectPtr> raw_map;
      size_t rawcount = 0, tmpcount = 0;
      try {
        YODA::read(file, aos_raw);
        for (YODA::AnalysisObject* aor : aos_raw) {
          const string& aopath = aor->path();
          if (aopath == "/TMP/_BEAMPZ") {
            raw_map[aopath].reset(aor);
            ++rawcount; ++tmpcount;
            continue;
          }
          // skip everything that isn't pre-finalize
          const AOPath aop_obj(aopath);
          if (!aop_obj.isRaw()) {
            delete aor;
            continue;
          }
          if (isTmpPath(aopath, true))  ++tmpcount;
          ++rawcount;
          bool skip = false;
          if (aopath != "" && aopath != "/RAW/_XSEC" && aopath != "/RAW/_EVTCOUNT") {
            if (matches.size()) {
              skip = !std::any_of(matches.begin(), matches.end(), [&](const string &exp){
                                  return std::regex_search(aopath, std::regex(exp));} );
            }
            if (unmatches.size()) {
              skip |= std::any_of(unmatches.begin(), unmatches.end(), [&](const string &exp){
                                 return std::regex_search(aopath, std::regex(exp));} );
            }
          }
          if (skip) {
            delete aor;
            continue;
          }
          raw_map[aopath].reset(aor);
        }
      }
      catch (...) { //< YODA::ReadError&
        throw UserError("Unexpected error in reading file: " + file);
      }
      if (raw_map.empty()) {
        MSG_WARNING("No AOs selected from file: " << file);
        continue;
      }
      if (equiv && (2*rawcount - tmpcount) != aos_raw.size()) {
        MSG_DEBUG("Number of pre- and post-finalize AOs do not match for file: " << file);
        //continue; <| Can't guarantee this is consistent in older output files! :(
      }

      // merge AOs from current file into "allaos"
      mergeAOS(allaos, raw_map, allxsecs, delopts, optAnas, optKeys, optVals,
                                equiv, overwrite_xsec, fileweight);

    } // loop over all input files ends
    std::cout << std::endl;

    if (allaos.empty()) {
      cerr << "Insufficient pre-finalize AOs to do a reentrant run!" << endl;
      exit(1);
    }

    MSG_DEBUG("Finalize cross-section scaling ...");
    for (const auto& item : allxsecs) {
      const string& wname = item.first;
      double xs = item.second[0];
      double xserr = item.second[1];
      const double counts = item.second[3];
      auto xs_it = allaos.find("/RAW/_XSEC"s + wname);
      assert( xs_it != allaos.end() );
      if (equiv) {
        MSG_DEBUG("Equivalent mode: work out combined cross-section");
        const double ntrials = item.second[2];
        if (ntrials && counts) {
          const double xse = xs, xse2 = xserr; // re-purposed for propagating
          auto ec_it = allaos.find("/RAW/_EVTCOUNT" + wname);
          assert( ec_it != allaos.end() );
          const double sumw = std::static_pointer_cast<YODA::Counter>(ec_it->second)->sumW();
          //const double sumw2 = std::static_pointer_cast<YODA::Counter>(ec_it->second)->sumW2();
          xs = sumw/ntrials;
          // This would be exact, but we don't have the actual number of trials
          //item.second.second = ((sumw2/ntrials) - sqr(sumw/ntrials))/(ntrials-1);
          xserr = xse/counts; // mean of all uncertainties in the files
          if (counts > 1.0)  xserr += sqrt(xse2/counts - sqr(xse/counts)); // variance of the uncertainty values
        }
        else {
          xs = xserr = 0.;
        }
      }
      YODA::Estimate0DPtr xsec = std::dynamic_pointer_cast<YODA::Estimate0D>(xs_it->second);
      if (xsec) {
        xsec->reset();
        xsec->set(xs, xserr);
      }
      else { // old-style cross-section
        YODA::Scatter1DPtr xsec = std::static_pointer_cast<YODA::Scatter1D>(xs_it->second);
        xsec->reset();
        xsec->addPoint(xs, xserr);
      }
    }

    // initialise analyses and load merged AOs back into memory
    // set unscale to true if equiv is false
    loadAOs(allaos, !equiv, reentrantOnly);

  }


  void AnalysisHandler::mergeAOS(map<string, YODA::AnalysisObjectPtr> &allaos,
                                 const map<string, YODA::AnalysisObjectPtr> &newaos,
                                 map<string, std::array<double,4>> &allxsecs,
                                 const vector<string> &delopts,
                                 const vector<string> &optAnas,
                                 const vector<string> &optKeys,
                                 const vector<string> &optVals,
                                 const bool equiv,
                                 const bool overwrite_xsec,
                                 const double user_xsec) {


    map<string, double> scales;
    const string beaminfokey("/TMP/_BEAMPZ");
    for (const auto& item : newaos) {
      const string& aopath = item.first;
      YODA::AnalysisObjectPtr ao = item.second;
      //AOPath path(ao->path());
      AOPath path(aopath);
      if ( !path ) {
        throw UserError("Invalid path name in new AO set!");
      }
      if (aopath == beaminfokey) {
        if (!equiv)  continue;
        auto beam_it = allaos.find(beaminfokey);
        if ( beam_it == allaos.end() ) { // assign first occurrence
          addAO(ao, allaos[beaminfokey], 1.0);
        }
        else {
          if (!beamInfoCompatible(ao, beam_it->second)) {
            throw UserError("Equivalent merging requires matching beams across input files!");
          }
        }
        continue;
      }
      // skip everything that isn't pre-finalize
      if ( !path.isRaw() ) continue;


      MSG_DEBUG(" " << ao->path());

      // Get the nominal sumW
      double nomSumW = 1.0;
      auto ec_it = newaos.find("/RAW/_EVTCOUNT"s);
      if ( ec_it != newaos.end() ) {
        YODA::CounterPtr cPtr = std::static_pointer_cast<YODA::Counter>(ec_it->second);
        if (cPtr->sumW())  nomSumW = cPtr->sumW();
      }

      const string& wname = path.weightComponent();
      if ( scales.find(wname) == scales.end() ) {
        scales[wname] = 1.0;
        // get the sum of weights and number of entries for the current weight
        double sumw = 1.0;
        auto ec_it = newaos.find("/RAW/_EVTCOUNT"s + wname);
        if ( ec_it != newaos.end() ) {
          YODA::CounterPtr cPtr = std::static_pointer_cast<YODA::Counter>(ec_it->second);
          sumw = cPtr->sumW()? cPtr->sumW() : 1;
        }
        else if (!equiv) {
          throw UserError("Missing event counter, needed for non-equivalent merging!");
        }
        // in stacking mode: add up all the cross sections
        // in equivalent mode: weight the cross-sections
        // estimates by the corresponding number of entries
        const string xspath = "/RAW/_XSEC"s + wname;
        auto xs_it = newaos.find(xspath);
        if ( xs_it != newaos.end() ) {
          // get iterator to the existing (or newly created) key-value pair
          auto xit = allxsecs.insert( make_pair(wname, std::array<double,4>{0.,0.,0.,0.}) ).first;
          // Get cross-section AO, which was a S1D in V2 and then became a E0D from V3
          YODA::Estimate0DPtr xsec = std::dynamic_pointer_cast<YODA::Estimate0D>(xs_it->second);
          if (xsec) { // for >= V3 ASCII
            if (overwrite_xsec) {
              MSG_DEBUG("Set user-supplied weight: " << user_xsec);
              xsec->setVal(user_xsec*sumw/nomSumW);
              xsec->setErr(0.0);
            }
            else {
              MSG_DEBUG("Multiply user-supplied weight: " << user_xsec);
              xsec->scale(user_xsec);
            }
            if (equiv) {
              // keep track of (squared) cross-section error sums
              xit->second[0] += xsec->totalErrAvg();
              xit->second[1] += sqr(xsec->totalErrAvg());
              xit->second[2] += sumw / xsec->val();
              xit->second[3] += 1.0;
            }
            else {
              // only in stacking mode: multiply each AO by cross-section / sumW
              xit->second[0] += xsec->val();
              xit->second[1] += xsec->totalErrAvg();
              scales[wname] = xsec->val() / sumw;
            }
          }
          else { // for <= V2 ASCII
            // old-style cross-section
            YODA::Scatter1DPtr xsec = std::static_pointer_cast<YODA::Scatter1D>(xs_it->second);
            if (overwrite_xsec) {
              MSG_DEBUG("Set user-supplied weight: " << user_xsec);
              xsec->point(0).setX(user_xsec*sumw/nomSumW);
              xsec->point(0).setXErrs(0.0, 0.0);
            }
            else {
              MSG_DEBUG("Multiply user-supplied weight: " << user_xsec);
              xsec->scale(0, user_xsec);
            }
            // weighted average of cross-sections
            if (equiv) {
              // keep track of (squared) cross-section error sums
              xit->second[0] += xsec->point(0).xErrAvg(); // cross-section error
              xit->second[1] += sqr(xsec->point(0).xErrAvg()); // squared cross-section error
              xit->second[2] += sumw / xsec->point(0).x();
              xit->second[3] += 1.0;
            }
            else {
              // only in stacking mode: multiply each AO by cross-section / sumW
              xit->second[0] += xsec->point(0).x();
              xit->second[1] += xsec->point(0).xErrAvg();
              scales[wname] = xsec->point(0).x() / sumw;
            }
          }
        }
        else if (!equiv) {
          throw UserError("Missing cross-section, needed for non-equivalent merging!");
        }
      }

      // Now check if any options should be removed
      for ( const string& delopt : delopts ) {
        if ( path.hasOption(delopt) )  path.removeOption(delopt);
      }
      // ...or added
      if (path.analysis() != "") {
        // adding analysis options only makes sense for analysis routines
        for (size_t i = 0; i < optAnas.size(); ++i) {
          if (optAnas[i] == "" || path.path().find(optAnas[i]) != string::npos) {
            path.setOption(optKeys[i], optVals[i]);
            path.fixOptionString();
          }
        }
      }
      path.setPath();

      // merge AOs
      const string& key = path.path();
      const double sf = key.find("_EVTCOUNT") != string::npos? 1 : scales[wname];
      if ( !addAO(ao, allaos[key], sf) ) { // assigns first occurrence, stacks subsequent ones
        MSG_DEBUG("Cannot merge objects with path " << key << " of type " << ao->type() << " using scale " << sf);
      }
    } // loop over all new AOs ends
  }


  void AnalysisHandler::loadAOs(const map<string, YODA::AnalysisObjectPtr>& allAOs,
                                const bool unscale, const bool reentrantOnly) {

    // Check that AH hasn't already been initialised
    if (_initialised) {
      throw UserError("AnalysisHandler::init has already been called: cannot re-initialize!");
    }

    const string beaminfokey("/TMP/_BEAMPZ");
    if (allAOs.find(beaminfokey) == allAOs.end()) {
      // beam info invalid for non-equivalent merging
      MSG_DEBUG("No beaminfo provided (probably in heterogeneous merging mode): setting empty beam info.");
      _beaminfo = make_shared<YODA::BinnedEstimate<string>>(beaminfokey);
    }

    // get list of analyses & multi-weights to be initialised
    set<string> foundAnalyses;
    set<string> foundWeightNames;
    for (const auto& pair : allAOs) {
      if (pair.first == beaminfokey) {
        _setRunBeamInfo(pair.second);
        continue;
      }
      AOPath path(pair.first);
      if ( path.analysisWithOptions() != "" ) {
        foundAnalyses.insert(path.analysisWithOptions());
      }
      foundWeightNames.insert(path.weight());
    }

    // Make analysis handler aware of the weight names present
    setWeightNames( vector<string>(foundWeightNames.begin(), foundWeightNames.end()) );

    // Then we create and initialize all analyses
    for (const string& ananame : foundAnalyses) { addAnalysis(ananame); }
    _stage = Stage::INIT;
    vector<string> anamestodelete;
    for (const AnaHandle& a : analyses() ) {
      MSG_TRACE("Initialising analysis: " << a->name());
      if ( !a->info().reentrant() ) {
        if (reentrantOnly) {
          MSG_DEBUG("Analysis " << a->name() << " has not been validated to have "
                      << "a reentrant finalize method and will be removed.");
          anamestodelete.push_back(a->name());
          continue;
        }
        else {
          MSG_WARNING("Analysis " << a->name() << " has not been validated to have "
                      << "a reentrant finalize method. The merged result is unpredictable.");
        }
      }
      try {
        // Allow projection registration in the init phase onwards
        a->_allowProjReg = true;
        a->init();
        a->setProjectionHandler(_projHandler);
        a->syncDeclQueue();
      } catch (const Error& err) {
        cerr << "Error in " << a->name() << "::init method: " << err.what() << endl;
        exit(1);
      }
      MSG_TRACE("Done initialising analysis: " << a->name());
    } // analyses
    if (anamestodelete.size()) removeAnalyses(anamestodelete);
    _stage = Stage::OTHER;
    _initialised = true;
    _isEndOfFile = true; // in case this is a re-entrant run

    // Collect global weights and cross sections and fix scaling for all files
    MSG_DEBUG("Getting event counter and cross-section from "
              << weightNames().size() << " " << numWeights());
    _eventCounter = CounterPtr(weightNames(), Counter("_EVTCOUNT"));
    _fileCounter = CounterPtr(weightNames(), Counter("_FILECOUNT"));
    _xserr = CounterPtr(weightNames(), Counter("XSECERR"));
    _xs = Estimate0DPtr(weightNames(), Estimate0D("_XSEC"));
    vector<double> scales(numWeights(), 1.0);
    for (size_t iW = 0; iW < numWeights(); ++iW) {
      MSG_DEBUG("Weight # " << iW << " of " << numWeights());
      _eventCounter.get()->setActiveWeightIdx(iW);
      _fileCounter.get()->setActiveWeightIdx(iW);
      _xs.get()->setActiveWeightIdx(iW);
      // set the sum of weights
      auto aoit = allAOs.find(_eventCounter->path());
      if (aoit != allAOs.end()) {
        *_fileCounter = *_eventCounter = *std::static_pointer_cast<YODA::Counter>(aoit->second);
      }

      // set the cross-section
      const auto xit = allAOs.find(_xs->path());
      if ( xit != allAOs.end() ) {
        YODA::Estimate0DPtr xstmp = std::dynamic_pointer_cast<YODA::Estimate0D>(xit->second);
        if (xstmp) { // for >= V3 ASCII
          *_xs = *xstmp;
        }
        else { // for <= V2 ASCII
          // old-style cross-section
          YODA::Scatter1DPtr xstmp = std::static_pointer_cast<YODA::Scatter1D>(xit->second);
          _xs->setVal(xstmp->point(0).x());
          _xs->setErr(xstmp->point(0).xErrs());
        }
        if (unscale && _xs->val()) {
          // in stacking mode: need to unscale prior to finalize
          scales[iW] = _eventCounter->sumW()/_xs->val();
        }
      }
      else {
        throw UserError("Missing cross-section for " + _xs->path());
      }

      // Go through all analyses and add stuff to their analysis objects;
      MSG_DEBUG("Load objects into analyses");
      for (const AnaHandle& a : analyses()) {
        for (const auto& ao : a->analysisObjects()) {
          ao.get()->setActiveWeightIdx(iW);
          YODA::AnalysisObjectPtr yao = ao.get()->activeAO();
          auto aoit = allAOs.find(yao->path());
          if (aoit != allAOs.end()) {
            if ( !addAO(aoit->second, yao, scales[iW]) ) {
              MSG_DEBUG("Overwriting incompatible starting version of " << yao->path()
                        << " using scale " << scales[iW]);
              copyAO(aoit->second, yao, 1.0); // input already scaled by addAO
            }
          }
          else {
            MSG_DEBUG("Cannot merge objects with path " << yao->path()
                      << " of type " << yao->annotation("Type"));
          }
          a->rawHookIn(yao);
          ao.get()->unsetActiveWeight();
        }
      }
      _eventCounter.get()->unsetActiveWeight();
      _xs.get()->unsetActiveWeight();
    }
  }


  void AnalysisHandler::loadAOs(const vector<string>& aoPaths,
                                const vector<double>& aoData) {

    // Check that AH hasn't already been initialised
    if (_initialised)
      throw UserError("AnalysisHandler::init has already been called: cannot re-initialize!");


    // get list of analyses & multi-weights to be initialised
    set<string> foundAnalyses;
    set<string> foundWeightNames;
    for (const string& aopath : aoPaths) {
      if (aopath == "/TMP/_BEAMPZ")  continue;
      AOPath path(aopath);
      if ( path.analysisWithOptions() != "" ) {
        foundAnalyses.insert(path.analysisWithOptions());
      }
      foundWeightNames.insert(path.weight());
    }

    // Make analysis handler aware of the weight names present
    _weightNames.clear();
    _rivetDefaultWeightIdx = _defaultWeightIdx = 0;
    _weightNames = vector<string>(foundWeightNames.begin(), foundWeightNames.end());

    // Then we create and initialize all analyses
    for (const string& ananame : foundAnalyses) { addAnalysis(ananame); }
    _stage = Stage::INIT;
    for (const AnaHandle& a : analyses() ) {
      MSG_TRACE("Initialising analysis: " << a->name());
      if ( !a->info().reentrant() )
        MSG_WARNING("Analysis " << a->name() << " has not been validated to have "
                    << "a reentrant finalize method. The merged result is unpredictable.");
      try {
        // Allow projection registration in the init phase onwards
        a->_allowProjReg = true;
        a->init();
        a->setProjectionHandler(_projHandler);
        a->syncDeclQueue();
      } catch (const Error& err) {
        cerr << "Error in " << a->name() << "::init method: " << err.what() << endl;
        exit(1);
      }
      MSG_TRACE("Done initialising analysis: " << a->name());
    } // analyses
    _stage = Stage::OTHER;
    _initialised = true;
    _isEndOfFile = true; // in case this is a re-entrant run

    // Collect global weights and cross sections and fix scaling for all files
    MSG_DEBUG("Getting event counter and cross-section from "
              << weightNames().size() << " " << numWeights());
    _eventCounter = CounterPtr(weightNames(), Counter("_EVTCOUNT"));
    _fileCounter = CounterPtr(weightNames(), Counter("_FILECOUNT"));
    _xserr = CounterPtr(weightNames(), Counter("XSECERR"));
    _xs = Estimate0DPtr(weightNames(), Estimate0D("_XSEC"));

    // load AOs into memory
    MSG_TRACE("Attempt to deserialize AO data.");
    try {
      deserializeContent(aoData);
    } catch (const Error& err) {
      cerr << "Error when trying to deserialize AO data: " << err.what() << endl;
      exit(1);
    }
    *_fileCounter = *_eventCounter;
    MSG_TRACE("Successfully deserialized AO data.");

    for (size_t iW = 0; iW < numWeights(); ++iW) {
      MSG_DEBUG("Weight # " << iW << " of " << numWeights());

      // Call rawHookIn for Correlators
      for (const AnaHandle& a : analyses()) {
        for (const auto& ao : a->analysisObjects()) {
          ao.get()->setActiveWeightIdx(iW);
          YODA::AnalysisObjectPtr yao = ao.get()->activeAO();
          a->rawHookIn(yao);
          ao.get()->unsetActiveWeight();
        }
      }
    }
  }


  void AnalysisHandler::merge(AnalysisHandler& other) {

    // Check if both AHs have been initialised
    if (!_initialised || !other._initialised)
      throw UserError("AnalysisHandler::init has not been called: cannot merge!");

    // Check if both AHs contain the same registered analyses
    bool is_equal = beamInfoCompatible(_beaminfo, other._beaminfo);
    if (is_equal) {
      const std::vector<std::string> &this_anaNames = analysisNames();
      const std::vector<std::string> &that_anaNames = other.analysisNames();
      is_equal &= this_anaNames.size() == that_anaNames.size();
      if (is_equal)  is_equal = std::equal(this_anaNames.begin(),
                                           this_anaNames.end(),
                                           that_anaNames.begin());
    }
    if (!is_equal)  throw UserError("The AnalysisHandlers are not equivalent!");

    /// @todo Do we need to check that the sequence of weight indices is the same?

    // Check if the registered analyses are reentrant safe
    for (const AnaHandle& a : analyses() ) {
      MSG_TRACE("Initialising analysis: " << a->name());
      if ( !a->info().reentrant() )
        MSG_WARNING("Analysis " << a->name() << " has not been validated to have "
                    << "a reentrant finalize method. The merged result is unpredictable.");
    }
    _stage = Stage::OTHER;

    // First push all analyses' objects to persistent and final
    MSG_TRACE("AnalysisHandler::merge(): Pushing analysis objects to persistent.");
    collapseEventGroup();
    other.collapseEventGroup();

    // Collect global weights and cross sections and fix scaling for all AHs
    MSG_DEBUG("Getting event counter and cross-section from "
              << weightNames().size() << " " << numWeights());

    // Update ntrials counter
    _ntrials += other._ntrials + safediv(other._fileCounter.get()->persistent(defaultWeightIndex())->sumW(),
                                         other._xs.get()->persistent(defaultWeightIndex())->val());
    for (size_t iW = 0; iW < numWeights(); ++iW) {
      MSG_DEBUG("Weight # " << iW << " of " << numWeights());
      // combine sumW counter
      _eventCounter.get()->setActiveWeightIdx(iW);
      other._eventCounter.get()->setActiveWeightIdx(iW);
      *_eventCounter += *other._eventCounter;
      // combine cross-section error
      _xserr.get()->setActiveWeightIdx(iW);
      other._xserr.get()->setActiveWeightIdx(iW);
      *_xserr += *other._xserr;

      // Go through all analyses and merge other's AOs with current AOs
      for (const auto& apair : other.analysesMap()) {
        for (const auto& other_ao : apair.second->analysisObjects()) {
          other_ao.get()->setActiveWeightIdx(iW);
          YODA::AnalysisObjectPtr other_yao = other_ao.get()->activeAO();
          // Find corresponding YODA::AO in current AH
          for (const MultiplexAOPtr& this_ao : analysis(apair.first)->analysisObjects()) {
            this_ao.get()->setActiveWeightIdx(iW);
            if (this_ao->path() != other_ao->path())  continue;
            YODA::AnalysisObjectPtr this_yao = this_ao.get()->activeAO(); // found it!
            // attempt merge
            if ( !addAO(other_yao, this_yao, 1.0) ) {
              MSG_DEBUG("Overwriting incompatible starting version of " << this_yao->path());
              copyAO(other_yao, this_yao, 1.0); // input already scaled by addAO
            }
            analysis(apair.first)->rawHookIn(this_yao);
            this_ao.get()->unsetActiveWeight();
          }
          other_ao.get()->unsetActiveWeight();
          /// @todo warn if AO could not be found? Throw an error even?
          ///   e.g. throw LookupError("Data object " + other_ao->path() + " not found");
        }
      }
      other._eventCounter.get()->unsetActiveWeight();
      _eventCounter.get()->unsetActiveWeight();
      other._xserr.get()->unsetActiveWeight();
      _xserr.get()->unsetActiveWeight();
    } // end of loop over weights
    // leave it to user to call finalize()
  }


  void AnalysisHandler::readData(std::istream& istr, const string& fmt, bool preload) {

    vector<YODA::AnalysisObject*> aos_raw;
    map<string,YODA::AnalysisObjectPtr> aomap;
    try {
      YODA::read(istr, aos_raw, fmt);
      for (YODA::AnalysisObject* aor : aos_raw)
        aomap[aor->path()] = YODA::AnalysisObjectPtr(aor);
    } catch (...) { //< YODA::WriteError&
      throw UserError("Unexpected error in reading input");
    }
    if (preload)  _preloads = std::move(aomap);
    else          loadAOs(aomap);

  }

  void AnalysisHandler::readData(const string& filename, bool preload) {
    map<string,YODA::AnalysisObjectPtr> aomap;
    try {
      /// @todo Use new YODA SFINAE to fill the smart ptr vector directly
      vector<YODA::AnalysisObject*> aos_raw;
      YODA::read(filename, aos_raw);
      for (YODA::AnalysisObject* aor : aos_raw)
        aomap[aor->path()] = YODA::AnalysisObjectPtr(aor);
    } catch (...) { //< YODA::ReadError&
      throw UserError("Unexpected error in reading file: " + filename);
    }
    if (preload)  _preloads = std::move(aomap);
    else          loadAOs(aomap);
  }


  vector<MultiplexAOPtr> AnalysisHandler::getRivetAOs() const {
    vector<MultiplexAOPtr> rtn;

    for (const AnaHandle& a : analyses()) {
      for (const auto& ao : a->analysisObjects()) {
        rtn.push_back(ao);
      }
    }
    rtn.push_back(_eventCounter);
    rtn.push_back(_xs);
    return rtn;
  }


  vector<YODA::AnalysisObjectPtr> AnalysisHandler::getYodaAOs(const bool includeraw,
                                                              const bool mkinert) const {

    // First get all multiweight AOs
    vector<MultiplexAOPtr> raos = getRivetAOs();
    vector<YODA::AnalysisObjectPtr> output;
    output.reserve(raos.size() * numWeights() * (includeraw ? 2 : 1) + 1); // plus one for beaminfo

    // Identify an index ordering so that default weight is written out first
    vector<size_t> order{ (size_t)_customDefaultWeightIdx };
    for (int i = 0, N = (int)numWeights(); i < N; ++i) {
      if (i != _customDefaultWeightIdx) order.push_back(i);
    }

    // add beam info object
    output.push_back( _beaminfo );

    // Then we go through all finalized, non-TMP AOs one weight at a time
    size_t nantrigger = 0;
    for (size_t iW : order) {
      for (auto rao : raos) {
        rao.get()->setActiveFinalWeightIdx(iW);
        if ( isTmpPath(rao->path(), true) )  continue;
        // skip leading-underscored analysis-level histos
        YODA::AnalysisObjectPtr aop = rao.get()->activeAO();
        // Convert to an inert type (e.g. estimate)
        const std::string aopath = aop->path();
        if (mkinert && aopath.find("Scatter") == std::string::npos) {
          aop.reset(aop->mkInert(aopath, "stats"));
          if (!iW && aop->hasAnnotation("NanFraction")) {
            const double nanc = aop->annotation<double>("NanFraction", 0.);
            const double nanw = aop->annotation<double>("WeightedNanFraction", 0.);
            if (nanc > 0.1 || nanw > 0.1) {
              MSG_DEBUG("Analysis with path " << aopath << " has (weighted) NaN fraction " << nanc << " (" << nanw << ")");
              ++nantrigger;
            }
          }
        }
        if (nantrigger) {
          MSG_WARNING("Found " << nantrigger << " analyses with unusually large NaN fraction (> 10%)! Run DEBUG mode for more info.");
        }
        // Push to output
        output.push_back(aop);
      }

      if (includeraw) {
        // Analyses can make changes necessary for merging to RAW objects before writing
        for (const auto& a : analyses()) {
          a->rawHookOut(raos, iW);
        }

        // Finally write the RAW objects
        for (auto rao : raos) {
          rao.get()->setActiveWeightIdx(iW);
          output.push_back(rao.get()->activeAO());
        }
      }
    }

    return output;
  }


  vector<YODA::AnalysisObjectPtr> AnalysisHandler::getRawAOs() const {

    // Prepare output vector
    vector<YODA::AnalysisObjectPtr> output;
    vector<MultiplexAOPtr> raos = getRivetAOs();
    output.reserve(raos.size() * numWeights() + 1);
    output.push_back(_beaminfo);

    // Get all multiweight AOs
    for (auto rao : raos) {
      for (size_t iW = 0; iW < numWeights(); ++iW) {
        rao.get()->setActiveWeightIdx(iW);
        output.push_back(rao.get()->activeAO());
      }
      rao.get()->unsetActiveWeight();
    }

    return output;
  }


  vector<std::string> AnalysisHandler::getRawAOPaths() const {

    // Prepare output vector
    vector<std::string> output;
    vector<MultiplexAOPtr> raos = getRivetAOs();
    output.reserve(raos.size() * numWeights() + 1);
    output.push_back(_beaminfo->path());

    // Get all multiweight AOs
    for (auto rao : raos) {
      for (size_t iW = 0; iW < numWeights(); ++iW) {
        rao.get()->setActiveWeightIdx(iW);
        output.push_back(rao.get()->activeAO()->path());
      }
    }

    return output;
  }


  void AnalysisHandler::writeData(std::ostream& ostr, const string& fmt) const {
    const vector<YODA::AnalysisObjectPtr> output = getYodaAOs(true);
    try {
      YODA::write(ostr, begin(output), end(output), fmt);
    } catch (...) { //< YODA::WriteError&
      throw UserError("Unexpected error in writing output");
    }
  }


  void AnalysisHandler::writeData(const string& filename) const {
    const vector<YODA::AnalysisObjectPtr> output = getYodaAOs(true);
    try {
      YODA::write(filename, begin(output), end(output));
    } catch (...) { //< YODA::WriteError&
      throw UserError("Unexpected error in writing file: " + filename);
    }
  }


  string AnalysisHandler::runName() const {
    return _runname;
  }


  std::vector<std::string> AnalysisHandler::analysisNames() const {
    std::vector<std::string> rtn;
    for (const AnaHandle& a : analyses()) {
      rtn.push_back(a->name());
    }
    return rtn;
  }


  std::vector<std::string> AnalysisHandler::stdAnalysisNames() const {
    // std::vector<std::string> rtn;
    // const string anadatpath = findAnalysisDataFile("analyses.dat");
    // if (fileexists(anadatpath)) {
    //   std::ifstream anadat(anadatpath);
    //   string ananame;
    //   while (anadat >> ananame) rtn += ananame;
    // }
    // return rtn;
    return AnalysisLoader::stdAnalysisNames();
  }


  AnalysisHandler& AnalysisHandler::addAnalyses(const std::vector<std::string>& analysisnames) {
    for (const string& aname : analysisnames) {
      //MSG_DEBUG("Adding analysis '" << aname << "'");
      addAnalysis(aname);
    }
    return *this;
  }


  AnalysisHandler& AnalysisHandler::removeAnalyses(const std::vector<std::string>& analysisnames) {
    for (const string& aname : analysisnames) removeAnalysis(aname);
    return *this;
  }


  void AnalysisHandler::setCrossSection(const vector<pair<double,double>>& xsecs, bool isUserSupplied) {

    if (xsecs.empty()) {
      throw UserError("No cross-section supplied!");
    }

    if (xsecs.size() == 1)  setCrossSection(xsecs[0], isUserSupplied);
    else {
      // Update the user xsec
      if (isUserSupplied) _userxs = xsecs[0];

      // If not setting the user xsec, and a user xsec is already set, exit early
      if (!isUserSupplied && notNaN(_userxs.first)) return;

      // Otherwise, update the xs estimate
      _xs = Estimate0DPtr(weightNames(), Estimate0D("_XSEC"));
      for (size_t iW = 0; iW < numWeights(); ++iW) {
        _xs.get()->setActiveWeightIdx(iW);
        _xs->set(xsecs[iW].first, xsecs[iW].second);
      }
      _xs.get()->unsetActiveWeight();
    }
  }


  void AnalysisHandler::setCrossSection(const pair<double,double>& xsec, bool isUserSupplied) {
    // Update the user xsec
    if (isUserSupplied) {
      MSG_DEBUG("Setting user cross-section = " << xsec.first << " +- " << xsec.second << " pb");
      _userxs = xsec;
    }

    // If not setting the user xsec, and a user xsec is already set, do nothing and exit early
    if (!isUserSupplied && notNaN(_userxs.first)) return;

    // Otherwise, update the xs scatter: xs_var = xs_nom * (sumW_var/sumW_nom)
    /// @todo Performance optimization? Overwriting the whole scatter wrapper on every event seems inefficient...
    MSG_TRACE("Setting nominal cross-section = " << xsec.first << " +- " << xsec.second << " pb");
    _xs = Estimate0DPtr(weightNames(), Estimate0D("_XSEC"));
    _eventCounter.get()->setActiveWeightIdx(defaultWeightIndex());
    const double nomwgt = sumW();
    const double nomwt2 = sumW2();
    for (size_t iW = 0; iW < numWeights(); ++iW) {
      _eventCounter.get()->setActiveWeightIdx(iW);
      const double s  = nomwgt? (sumW() / nomwgt) : 1.0;
      const double s2 = nomwt2? sqrt(sumW2() / nomwt2) : 1.0;
      _xs.get()->setActiveWeightIdx(iW);
      _xs->set(xsec.first*s, xsec.second*s2);
    }
    _eventCounter.get()->unsetActiveWeight();
    _xs.get()->unsetActiveWeight();
  }


  double AnalysisHandler::nominalCrossSection() const {
    _xs.get()->setActiveWeightIdx(defaultWeightIndex());
    double xs = _xs->val();
    if (isnan(xs)) {
      string errMsg = "Value missing when requesting nominal cross-section";
      throw Error(errMsg);
    }
    _xs.get()->unsetActiveWeight();
    return xs;
  }


  double AnalysisHandler::nominalCrossSectionError() const {
    _xs.get()->setActiveWeightIdx(defaultWeightIndex());
    double xserr = _xs->errAvg();
    if (isnan(xserr)) {
      string errMsg = "Value missing when requesting nominal cross-section error";
      throw Error(errMsg);
    }
    _xs.get()->unsetActiveWeight();
    return xserr;
  }


  // update the weighted cross-section estimate with the cross-section
  // information from the previous HepMC file (this method gets called
  // before the first event from the new file gets processed).
  void AnalysisHandler::updateCrossSection() {

    collapseEventGroup();

    // update Ntrials calculation
    _xs.get()->setActiveWeightIdx(defaultWeightIndex());
    _fileCounter.get()->setActiveWeightIdx(defaultWeightIndex());
    _ntrials += _fileCounter->sumW() / _xs->val();

    // reset file-based counter
    for (size_t iW = 0; iW < numWeights(); ++iW) {
      _fileCounter.get()->setActiveWeightIdx(iW);
      _fileCounter->reset();
      _fileCounter.get()->unsetActiveWeight();
      _xs.get()->setActiveWeightIdx(iW);
      _xserr.get()->persistent(iW)->fill(_xs->totalErrAvg());
    }
    // Information processed - we're good to continue
    _xs.get()->unsetActiveWeight();
    // processing the current event file
    _isEndOfFile = false;
  }

  AnalysisHandler& AnalysisHandler::addAnalysis(Analysis* analysis) {
    analysis->_analysishandler = this;
    // _analyses.insert(AnaHandle(analysis));
    _analyses[analysis->name()] = AnaHandle(analysis);
    return *this;
  }


  vector<double> AnalysisHandler::weightSumWs() const {
    vector<double> rtn; rtn.reserve(numWeights());
    for (size_t iW = 0; iW < numWeights(); ++iW) {
      _eventCounter.get()->setActiveWeightIdx(iW);
      rtn.push_back(_eventCounter->sumW());
    }
    _eventCounter.get()->unsetActiveWeight();
    return rtn;
  }


  AnalysisHandler& AnalysisHandler::setRunBeams(const ParticlePair& beams) {
    _beams = beams;
    _setRunBeamInfo(beams);
    MSG_DEBUG("Setting run beams = " << beams << " @ " << sqrtS(beams)/GeV << " GeV");
    return *this;
  }

  void AnalysisHandler::_setRunBeamInfo(const ParticlePair& beams) {
    PdgIdPair beamids = pids(beams);
    pair<FourMomentum,FourMomentum> beammoms = moms(beams);
    const bool first_pos = beammoms.first.pz() > 0.;
    vector<string> labels{_mkBeamInfoLabel(1, first_pos? beamids.second : beamids.first),
                          _mkBeamInfoLabel(2, first_pos? beamids.first : beamids.second)};
    _beaminfo = make_shared<YODA::BinnedEstimate<string>>(labels, "/TMP/_BEAMPZ");
    _beaminfo->bin(1+first_pos).setVal(beammoms.first.pz()/GeV);
    _beaminfo->bin(2-first_pos).setVal(beammoms.second.pz()/GeV);
  }

  void AnalysisHandler::_setRunBeamInfo(YODA::AnalysisObjectPtr ao) {
    if (!_beaminfo) {
      YODA::BinnedEstimatePtr<string> beaminfo = std::dynamic_pointer_cast<YODA::BinnedEstimate<string>>(ao);
      _beaminfo = make_shared<YODA::BinnedEstimate<string>>(*beaminfo);
    }
  }

  PdgIdPair AnalysisHandler::runBeamIDs() const {
    return pids(runBeams());
  }

  pair<double,double> AnalysisHandler::runBeamEnergies() const {
    return energies(runBeams());
  }

  double AnalysisHandler::runSqrtS() const {
    double rtn = sqrtS(runBeams());
    if (rtn <= 0. && _beaminfo && _beaminfo->numBins()==2) { // try falling back to _beaminfo
      rtn = sqrtS(fabs(_beaminfo->bin(1).val()), fabs(_beaminfo->bin(2).val()));
    }
    return rtn;
  }

  bool AnalysisHandler::copyAO(YODA::AnalysisObjectPtr src, YODA::AnalysisObjectPtr dst, const double scale) {
    const TypeRegisterItr& typeHandle = _register.find(src->type());
    if (typeHandle == _register.end())  return false;
    return typeHandle->second->copyAO(src, dst, scale); // uses type-specific implementation
  }

  bool AnalysisHandler::addAO(YODA::AnalysisObjectPtr src, YODA::AnalysisObjectPtr& dst, const double scale) {
    const TypeRegisterItr& typeHandle = _register.find(src->type());
    if (typeHandle == _register.end())  return false;
    return typeHandle->second->addAO(src, dst, scale); // uses type-specific implementation
  }

  /// Register a default set of types in the calling singleton
  template<typename... Args>
  void AnalysisHandler::registerDefaultTypes() {

    // load 0D types
    registerType<YODA::Counter>();
    registerType<YODA::Estimate0D>();

    // load scatters and short-hand types
    auto addShorthands = [&](auto I) {
      registerType<YODA::HistoND<I+1>>();
      registerType<YODA::ProfileND<I+1>>();
      registerType<YODA::ScatterND<I+1>>();
    };
    MetaUtils::staticFor<3>(addShorthands);

    // load BinnedHisto/BinnedProfile in 1D
    for_each_arg<Args...>([&](auto&& arg) {
      using A1 = decay_t<decltype(arg)>;
      using BH = YODA::BinnedHisto<A1>;
      registerType<BH>();
      using BP = YODA::BinnedProfile<A1>;
      registerType<BP>();
      using BE = YODA::BinnedEstimate<A1>;
      registerType<BE>();

      // load BinnedHisto/BinnedProfile in 2D
      for_each_arg<Args...>([&](auto&& arg) {
        using A2 = decay_t<decltype(arg)>;
        using BH = YODA::BinnedHisto<A1,A2>;
        registerType<BH>();
        using BP = YODA::BinnedProfile<A1,A2>;
        registerType<BP>();
        using BE = YODA::BinnedEstimate<A1,A2>;
        registerType<BE>();

        // load BinnedHisto/BinnedProfile in 3D
        /*for_each_arg<Args...>([&](auto&& arg) {
          using A3 = decay_t<decltype(arg)>;
          using BH = YODA::BinnedHisto<A1,A2,A3>;
          registerType<BH>();
          //using BP = YODA::BinnedProfile<A1,A2,A3>;
          //registerType<BP>(BP().type());
          using BE = YODA::BinnedEstimate<A1,A2,A3>;
          registerType<BE>();
        });*/
      });
    });
    // for now, only load HistoND<d,d,d>
    /// @todo Do we need more 3D types?
    registerType<YODA::HistoND<3>>();
  }

}
