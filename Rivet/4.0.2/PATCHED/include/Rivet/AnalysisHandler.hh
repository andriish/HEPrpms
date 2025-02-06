// -*- C++ -*-
#ifndef RIVET_RivetHandler_HH
#define RIVET_RivetHandler_HH

#include "Rivet/Config/RivetCommon.hh"
#include "Rivet/Particle.hh"
#include "Rivet/AnalysisLoader.hh"
#include "Rivet/Tools/RivetYODA.hh"
#include "Rivet/Tools/Utils.hh"
#include "Rivet/ProjectionHandler.hh"
#include "YODA/ReaderYODA.h"

#include <fstream>
#include <unordered_map>

namespace Rivet {


  // Forward declaration and smart pointer for Analysis
  class Analysis;
  using AnaHandle = std::shared_ptr<Analysis>;


  /// @brief The key class for coordination of Analysis objects and the event loop
  ///
  /// A class which handles a number of analysis objects to be applied to
  /// generated events. An {@link Analysis}' AnalysisHandler is also responsible
  /// for handling the final writing-out of histograms.
  class AnalysisHandler {

    using TypeHandlePtr = std::shared_ptr<TypeBaseHandle>;
    using TypeRegister = std::unordered_map<string, TypeHandlePtr>;
    using TypeRegisterItr = typename TypeRegister::const_iterator;

  public:

    using Annotations = std::map<std::string, std::string>;

    /// Preferred constructor, with optional run name.
    AnalysisHandler(const string& runname="");

    /// The copy constructor is deleted, so it can never be called.
    AnalysisHandler(const AnalysisHandler&) = delete;

    /// The assignment operator is deleted, so it can never be called.
    AnalysisHandler& operator=(const AnalysisHandler&) = delete;

    /// The destructor is not virtual, as this class should not be inherited from.
    ~AnalysisHandler();


    /// @name Run properties
    /// @{

    /// Get the name of this run.
    string runName() const;

    /// Get the number of events seen. Should only really be used by external
    /// steering code or analyses in the finalize phase.
    ///
    /// N.B. This only reports the count for the last collapsed event group
    /// and hence ignores any additional sub-events seen so far.
    size_t numEvents() const {
      const double N = _eventCounter.get()->persistent(defaultWeightIndex())->numEntries();
      return  size_t(N + 0.5 - (N<0)); // round to nearest integer
    }

    /// Get the effective number of events seen. Should only really be used by external
    /// steering code or analyses in the finalize phase.
    ///
    /// N.B. This only reports the count for the last collapsed event group
    /// and hence ignores any additional sub-events seen so far.
    double effNumEvents() const {
      if ((bool)_eventCounter) { return _eventCounter->effNumEntries(); }
      return _eventCounter.get()->persistent(defaultWeightIndex())->effNumEntries();
    }

    /// @brief Access the sum of the event weights seen
    ///
    /// This is the weighted equivalent of the number of events. It should only
    /// be used by external steering code or analyses in the finalize phase.
    double sumW() const {
      if ((bool)_eventCounter) { return _eventCounter->sumW(); }
      return _eventCounter.get()->persistent(defaultWeightIndex())->sumW();
    }
    /// Access to the sum of squared-weights
    double sumW2() const {
      if ((bool)_eventCounter) { return _eventCounter->sumW2(); }
      return _eventCounter.get()->persistent(defaultWeightIndex())->sumW2();
    }

    /// @}


    /// @name Event weights
    /// @{

    /// Names of event weight categories
    const vector<string>& weightNames() const { return _weightNames; }

    /// Are any of the weights non-numeric?
    size_t numWeights() const { return _weightNames.size(); }

    /// Are any of the weights non-numeric?
    bool haveNamedWeights() const;

    /// Set the weight names from a GenEvent
    void setWeightNames(const GenEvent& ge);

    /// Set the weight names from a vector<string>
    void setWeightNames(const vector<string>& weightNames);

    /// Get the index of the nominal weight-stream
    size_t defaultWeightIndex() const { return _rivetDefaultWeightIdx; }

    /// @brief Access the array of sum of the event weights seen
    vector<double> weightSumWs() const;

    /// Set the weight cap
    void setWeightCap(const double maxWeight) { _weightCap = maxWeight; }

    /// Set the name of the nominal weight stream
    void setNominalWeightName(const std::string& name) { _nominalWeightName = name; }

    /// Ignore all weight streams other than the nominal
    void skipMultiWeights(bool skip=false) { _skipMultiWeights = skip; }

    /// Specify weight-name patterns to accept
    void matchWeightNames(const std::string& patterns) { _matchWeightNames = patterns; }

    /// Specify weight-name patterns to reject
    void unmatchWeightNames(const std::string& patterns) { _unmatchWeightNames = patterns; }

    /// Set the relative width of the NLO smearing window.
    void setNLOSmearing(double frac) { _NLOSmearing = frac; }

    /// @}


    /// @name Cross-sections
    /// @{

    /// Get the cross-section known to the handler
    Estimate0DPtr crossSection() const { return _xs; }

    /// Set all cross-sections for the process being generated specifically (preferred)
    void setCrossSection(const vector<pair<double,double>>& xsecs, bool isUserSupplied = false);

    /// Set all cross-sections for the process being generated, based on nominal weight
    void setCrossSection(const pair<double, double>& xsec, bool isUserSupplied=false);

    /// Set the cross-section for the process being generated (alternative signature)
    void setCrossSection(double xsec, double xsecerr, bool isUserSupplied=false) {
      setCrossSection({xsec, xsecerr}, isUserSupplied);
    }

    /// Update the internal cross-section average when running over multiple files
    ///
    /// @note This method should only be called when switching HepMC file mid-run.
    void updateCrossSection();

    /// Toggle to signal a change in HepMC input file
    void notifyEndOfFile() { _isEndOfFile = true; }

    /// Get the nominal cross-section
    double nominalCrossSection() const;

    /// Get the nominal cross-section
    double nominalCrossSectionError() const;

    /// @}


    /// @name Beams
    /// @{

    /// Set the beam particles for this run
    AnalysisHandler& setRunBeams(const ParticlePair& beams);

    /// Get the beam particles for this run, usually determined from the first event
    const ParticlePair& runBeams() const { return _beams; }

    /// Get beam IDs for this run, usually determined from the first event
    PdgIdPair runBeamIDs() const;

    /// Get beam IDs for this run, usually determined from the first event
    pair<double,double> runBeamEnergies() const;

    /// Get energy for this run, usually determined from the first event
    double runSqrtS() const;

    /// Option to disable analysis-compatibility checks
    void setCheckBeams(bool check=true) { _checkBeams = check; }

    /// Option to disable run-consistency checks
    // void setCheckConsistency(bool check=true) { _checkConsistency = check; }
    // Check event consistency with the run, usually determined from the first event
    // bool consistentWithRun(Event& event) {

    /// @}


    /// @name Run-based annotations
    /// @{

    // Get all the annotation names
    std::vector<std::string> annotations() const {
      return _beaminfo->annotations();
    }

    /// Check if an annotation is defined
    bool hasAnnotation(const std::string& name) const {
      return _beaminfo->hasAnnotation(name);
    }

    /// Get an annotation by name (as a string)
    const std::string& annotation(const std::string& name) const {
      return _beaminfo->annotation(name);
    }

    /// Get an annotation by name (as a string) with a default in case the annotation is not found
    const std::string& annotation(const std::string& name, const std::string& defaultreturn) const {
      return _beaminfo->annotation(name, defaultreturn);
    }

    /// @brief Get an annotation by name (copied to another type)
    ///
    /// @note Templated on return type
    template <typename T>
    const T annotation(const std::string& name) const {
      return _beaminfo->annotation<T>(name);
    }

    /// @brief Get an annotation by name (copied to another type) with a default in case the annotation is not found
    ///
    /// @note Templated on return type
    template <typename T>
    const T annotation(const std::string& name, T&& defaultreturn) const {
      return _beaminfo->annotation<T>(name, std::forward<T>(defaultreturn));
    }

    /// @brief Add or set an annotation by name (templated for remaining types)
    ///
    /// @note Templated on arg type, but stored as a string.
    template <typename T>
    void setAnnotation(const std::string& name, T&& value) {
      _beaminfo->annotation<T>(name, std::forward<T>(value));
    }


    /// Set all annotations at once
    void setAnnotations(const Annotations& anns) {
      _beaminfo->setAnnotations(anns);
    }

    /// Delete an annotation by name
    void rmAnnotation(const std::string& name) {
      _beaminfo->rmAnnotation(name);
    }


    /// Delete an annotation by name
    void clearAnnotations() {
      _beaminfo->clearAnnotations();
    }

    /// @}


    /// @name AO type handling
    /// @{

    /// Register an AO type handle into type map and YODA reader
    template<typename T>
    void registerType() {
      const std::string name = T().type();
      const TypeRegisterItr& res = _register.find(name);
      if (res == _register.end()) {
        _register[name] = make_shared<TypeHandle<T>>();
      }
      _reader.registerType<T>(); // also let YODA know
    }

    /// If @a dst is the same subclass as @a src, copy the contents of
    /// @a src into @a dst and return true. Otherwise return false.
    bool copyAO(YODA::AnalysisObjectPtr src, YODA::AnalysisObjectPtr dst, const double scale=1.0);

    /// If @a dst is the same subclass as @a src, scale the contents of @a src with
    /// @a scale and add it to @a dst and return true. Otherwise return false.
    bool addAO(YODA::AnalysisObjectPtr src, YODA::AnalysisObjectPtr& dst, const double scale);

    /// @}


    /// @name Analysis handling
    /// @{

    /// Get a list of the currently registered analyses' names.
    std::vector<std::string> analysisNames() const;

    /// Get a list of the official analysis names for this release.
    std::vector<std::string> stdAnalysisNames() const;

    /// Get the collection of currently registered analyses.
    const std::map<std::string, AnaHandle>& analysesMap() const {
      return _analyses;
    }

    /// Get the collection of currently registered analyses.
    std::vector<AnaHandle> analyses() const {
      std::vector<AnaHandle> rtn;
      rtn.reserve(_analyses.size());
      for (const auto& apair : _analyses) rtn.push_back(apair.second);
      return rtn;
    }

    /// Get a registered analysis by name.
    AnaHandle analysis(const std::string& analysisname) {
      if ( _analyses.find(analysisname) == _analyses.end() )
        throw LookupError("No analysis named '" + analysisname + "' registered in AnalysisHandler");
      try {
        return _analyses[analysisname];
      } catch (...) {
        throw LookupError("No analysis named '" + analysisname + "' registered in AnalysisHandler");
      }
    }

    /// Add an analysis to the run list by object
    AnalysisHandler& addAnalysis(Analysis* analysis);

    /// @brief Add an analysis to the run list using its name.
    ///
    /// The actual Analysis to be used will be obtained via
    /// AnalysisLoader::getAnalysis(string).  If no matching analysis is found,
    /// no analysis is added (i.e. the null pointer is checked and discarded.
    AnalysisHandler& addAnalysis(const std::string& analysisname);

    /// @brief Add an analysis with a map of analysis options.
    AnalysisHandler& addAnalysis(const std::string& analysisname, std::map<string, string> pars);

    /// @brief Add analyses to the run list using their names.
    ///
    /// The actual {@link Analysis}' to be used will be obtained via
    /// AnalysisHandler::addAnalysis(string), which in turn uses
    /// AnalysisLoader::getAnalysis(string). If no matching analysis is found
    /// for a given name, no analysis is added, but also no error is thrown.
    AnalysisHandler& addAnalyses(const std::vector<std::string>& analysisnames);


    /// Remove an analysis from the run list using its name.
    AnalysisHandler& removeAnalysis(const std::string& analysisname);

    /// Remove analyses from the run list using their names.
    AnalysisHandler& removeAnalyses(const std::vector<std::string>& analysisnames);

    /// @}


    /// @name Main init/execute/finalise
    /// @{

    /// Initialize a run, with the run beams taken from the example event
    void init(const GenEvent& event);

    /// @brief Analyze the given @a event by reference
    ///
    /// This function will call the AnalysisBase::analyze() function of all
    /// included analysis objects.
    ///
    /// @note Despite the event being passed as const, its units etc. may be changed, hence non-const.
    void analyze(GenEvent& event);

    /// @brief Analyze the given @a event by pointer
    ///
    /// This function will call the AnalysisBase::analyze() function of all
    /// included analysis objects, after checking the event pointer validity.
    void analyze(GenEvent* event);

    /// Finalize a run. This function calls the AnalysisBase::finalize()
    /// functions of all included analysis objects.
    void finalize();

    /// @}


    /// @name Histogram / data object access
    /// @{

    /// After all subevents in an event group have been processed, push
    /// all histo fills to the relevant histograms.
    void collapseEventGroup();

    /// @brief Read analysis plots into the histo collection from the given stream
    ///
    /// Use the @a fmt flag to specify the YODA output format (yoda, yoda.gz, yoda.h5, ...)
    void readData(std::istream& istr, const string& fmt, bool preload = true);

    /// Read analysis plots into the histo collection (via addData) from the named file.
    void readData(const std::string& filename, bool preload = true);

    /// Get all YODA analysis objects (across all weights, optionally including RAW)
    ///
    /// @note We'll live with the mixed-case "Yoda" here, since the consistent all-caps would be worse!
    vector<YODA::AnalysisObjectPtr> getYodaAOs(const bool includeraw=false, const bool mkinert=true) const;

    /// Get all raw YODA analysis objects (across all weights)
    vector<YODA::AnalysisObjectPtr> getRawAOs() const;

    /// Get all raw YODA analysis object paths (across all weights)
    vector<std::string> getRawAOPaths() const;

    /// Get a pointer to a preloaded yoda object with the given path,
    /// or null if path is not found.
    const YODA::AnalysisObjectPtr getPreload(const string& path) const {
      auto it = _preloads.find(path);
      if ( it == _preloads.end() ) return nullptr;
      return it->second;
    }

    /// @brief Write all analyses' plots (via getData) to the given stream.
    ///
    /// Use the @a fmt flag to specify the YODA output format (yoda, yoda.gz, yoda.h5, ...)
    void writeData(std::ostream& ostr, const string& fmt) const;

    /// Write all analyses' plots (via getData) to the named file.
    void writeData(const string& filename) const;

    /// @brief Configure the AnalysisObject dump rate and destination.
    ///
    /// Tell Rivet to dump intermediate result to a file named @a
    /// dumpfile every @a period'th event. If @a period is not positive,
    /// no periodic finalization will be done.
    void setFinalizePeriod(const string& dumpfile, int period) {
      _dumpPeriod = period;
      _dumpFile = dumpfile;
    }
    /// @brief Configure the AnalysisObject dump rate and destination.
    void setNoFinalizePeriod() {
      setFinalizePeriod("DUMMY", -1);
    }

    /// @brief Set filename of the bootstrap file
    void setBootstrapFilename(const string& filename) {
      _bootstrapfilename = filename;
    }

    /// Return a vector of (AO path, AO numBins) pairs to decode the fills layout
    vector<pair<string,size_t>> fillLayout() const;

    /// Return a vector of the binary fill outcome (was/wasn't filled) at each fill position
    vector<bool> fillOutcomes() const;

    /// Return a vector of the fill fraction at each fill position
    vector<double> fillFractions() const;

    /// @brief Merge the vector of YODA files, using the cross-section and weight information provided in each.
    ///
    /// Each file in @a aofiles is assumed to have been produced by Rivet. By
    /// default the files are assumed to contain different processes (or the
    /// same processs but mutually exclusive cuts), but if @a equiv if true, the
    /// files are assumed to contain output of completely equivalent (but
    /// statistically independent) Rivet runs. The corresponding analyses will
    /// be loaded and their analysis objects will be filled with the merged
    /// result. finalize() will be run on each relevant analysis. The resulting
    /// YODA file can then be written out by writeData().
    ///
    /// If @a delopts is non-empty, it is assumed to contain names of different
    /// options to be merged into the same analysis objects.
    ///

    void mergeYODAs(const vector<string>& aofiles,
                    const vector<string>& delopts=vector<string>(),
                    const vector<string>& addopts=vector<string>(),
                    const vector<string>& matches=vector<string>(),
                    const vector<string>& unmatches=vector<string>(),
                    const bool equiv=false, const bool reentrantOnly = true);

    /// A method to merge another AnalysisHandler into the current one
    void merge(AnalysisHandler &other);

    /// @brief A method to prepare a re-entrant run for a given set
    /// of AO paths and serialized AO data
    ///
    /// The @a unscale parameter multiplies fillable objects with sumW/xsec to counteract
    /// the cross-section scaling in finalize() when merging different processes (non-equiv)
    void loadAOs(const vector<string>& aoPaths, const vector<double>& aoData);

    /// @}

    /// @name MPI (de-)serialisation
    ///@{

    vector<double> serializeContent(bool fixed_length = false) {
      if (!_initialised)
        throw Error("AnalysisHandler has not been initialised!");

      collapseEventGroup();

      // Loop over raw AOs and work out the size of the content data
      const vector<YODA::AnalysisObjectPtr> raos = getRawAOs();
      size_t total = 0;
      for (size_t i = 0; i < raos.size(); ++i) {
        total += raos[i]->lengthContent(fixed_length)+1;
      }
      total += _beaminfo->numBins()+1;

      // Loop over raw AOs and retrieve the content data
      std::vector<double> data; // serialized data vector
      data.reserve(total); // pre-allocate enough memory
      // Add beam IDs
      data.push_back(_beaminfo->numBins());
      for (const string& beamID : _beaminfo->xEdges()) {
        data.push_back(_beamInfoLabelToID(beamID));
      }
      // Add raw YODA AO content
      for (size_t i = 0; i < raos.size(); ++i) {
        vector<double> tmp = raos[i]->serializeContent(fixed_length);
        data.push_back(tmp.size()); // length of the AO
        data.insert(std::end(data),
                    std::make_move_iterator(std::begin(tmp)),
                    std::make_move_iterator(std::end(tmp)));
      }
      return data;
    }

    void deserializeContent(const vector<double>& data, size_t nprocs = 0) {
      if (!_initialised)
        throw Error("AnalysisHandler has not been initialised!");

      collapseEventGroup();

      // get Rivet AOs for access to raw AO pointers
      vector<MultiplexAOPtr> raos = getRivetAOs();


      // beam info first
      size_t iAO = 0, iW = 0, nBeams = data[0], offset = 1;
      if (nprocs)  nBeams /= nprocs;
      const auto itr = data.cbegin();
      // set beam IDs
      vector<int> edges{itr+offset, itr+offset+nBeams};
      vector<string> labels; labels.reserve(edges.size());
      size_t id = 0;
      for (int edge : edges) {
        if (nprocs >= 2)  edge /= nprocs;
        labels.push_back(_mkBeamInfoLabel(++id, edge));
      }

      _beaminfo = make_shared<YODA::BinnedEstimate<string>>(labels, "/TMP/_BEAMPZ");
      offset += nBeams;
      // set beam momenta
      size_t beamLen = *(itr + offset); ++offset;
      if (nprocs)  beamLen /= nprocs;
      std::vector<double> energies{itr+offset, itr+offset+beamLen};
      if (nprocs >= 2) {
        for (double& e : energies) { e /= nprocs; }
      }
      _beaminfo->deserializeContent(energies);
      for (auto& b : _beaminfo->bins(true))  b.rmErrs();
      offset += beamLen;

      // then the multiweighted AOs
      while (offset < data.size()) {
        if (iW < numWeights())  raos[iAO].get()->setActiveWeightIdx(iW);
        else {
          raos[iAO].get()->unsetActiveWeight();
          iW = 0; ++iAO; // move on to next AO
          raos[iAO].get()->setActiveWeightIdx(iW);
        }

        // obtain content length and set content iterators
        size_t aoLen = *(itr + offset); ++offset;
        if (nprocs)  aoLen /= nprocs;
        auto first = itr + offset;
        auto last = first + aoLen;
        // load data into AO
        raos[iAO].get()->activeAO()->deserializeContent(std::vector<double>{first, last});

        ++iW; offset += aoLen; // increment offset
      }
      raos[iAO].get()->unsetActiveWeight();
      // Reset cross-section bookkeeping
      _ntrials = 0.0;
      _fileCounter = CounterPtr(weightNames(), Counter("_FILECOUNT"));
      _xserr = CounterPtr(weightNames(), Counter("XSECERR"));
      if (nprocs >= 2) {
        for (size_t iW = 0; iW < numWeights(); ++iW) {
          *_fileCounter.get()->persistent(iW) = *_eventCounter.get()->persistent(iW);
          _xs.get()->persistent(iW)->scale(1.0/nprocs);
        }
      }
    }

    /// @}


    /// @name Processing stage
    /// @{

    /// Indicate which Rivet stage we're in.
    /// At the moment, only INIT is used to enable booking.
    enum class Stage { OTHER, INIT, FINALIZE };

    /// Return the current processing stage.
    Stage stage() const { return _stage; }

    /// @}

  private:

    /// @name Internal helper functions
    /// @{

    /// Get a logger object.
    Log& getLog() const;

    /// Get all multi-weight Rivet analysis object wrappers.
    vector<MultiplexAOPtr> getRivetAOs() const;

    /// Helper function to strip specific options from data object paths.
    void stripOptions(YODA::AnalysisObjectPtr ao, const vector<string>& delopts) const;

    /// @brief Merge the AO map @a newaos into @a allaos
    void mergeAOS(map<string, YODA::AnalysisObjectPtr> &allaos,
                  const map<string, YODA::AnalysisObjectPtr> &newaos,
                  map<string, std::array<double,4>> &allxsecs,
                  const vector<string>& delopts=vector<string>(),
                  const vector<string>& optAnas=vector<string>(),
                  const vector<string>& optKeys=vector<string>(),
                  const vector<string>& optVals=vector<string>(),
                  const bool equiv=false,
                  const bool overwrite_xsec = false,
                  const double user_xsec = 1.0);


    /// @brief A method to prepare a re-entrant run for a given set of analysis objects
    ///
    /// The @a unscale parameter multiplies fillable objects with sumW/xsec to counteract
    /// the cross-section scaling in finalize() when merging different processes (non-equiv)
    void loadAOs(const map<string, YODA::AnalysisObjectPtr>& allAOs,
                 const bool unscale = false, const bool reentrantOnly = true);

    /// @brief A method to set the internal _beaminfo object from a pair of beams
    void _setRunBeamInfo(const ParticlePair& beams);

    /// @brief A method to set the internal _beaminfo object from an existing YODA AO
    void _setRunBeamInfo(YODA::AnalysisObjectPtr ao);

    /// Construct beaminfo label from ID
    string _mkBeamInfoLabel(size_t n, PdgId id) {
      return "BEAM"+std::to_string(n)+"("+std::to_string(id)+")";
    }

    /// Retrieve ID from beaminfo label
    PdgId _beamInfoLabelToID(const string& label) {
      size_t pos = label.find("(");
      string beamID = label.substr(pos+1, label.size()-pos-2);
      return std::stoi(beamID);
    }


    /// @}


  private:

    /// Current handler processing stage.
    Stage _stage = Stage::OTHER;

    /// The collection of Analysis objects to be used.
    std::map<std::string, AnaHandle> _analyses;

    /// A vector of pre-loaded object which do not have a valid Analysis plugged in.
    ///
    /// @todo Rename to _preloadedAOs for consistency
    map<string,YODA::AnalysisObjectPtr> _preloads;

    /// A vector containing copies of analysis objects after finalize() has been run.
    vector<YODA::AnalysisObjectPtr> _finalizedAOs;

    /// Loads a default set of YODA AO types into the TypeHandleMap
    template<typename... Args>
    void registerDefaultTypes();

    /// The TypeRegister of all registered types
    TypeRegister _register;

    /// A reference to the ReaderYODA singleton
    YODA::Reader& _reader = YODA::ReaderYODA::create();


    /// @name Run properties
    /// @{

    /// Weight names
    std::vector<std::string> _weightNames;
    std::vector<std::valarray<double> > _subEventWeights;
    //size_t _numWeightTypes; // always == WeightVector.size()

    /// Weight indices
    std::vector<size_t> _weightIndices;

    /// Run name
    std::string _runname;

    /// Event counter
    CounterPtr _eventCounter;

    /// Cross-section known to AH
    Estimate0DPtr _xs;

    /// Running average of the cross-section uncertainties
    CounterPtr _xserr;

    /// Beam info known to AH
    YODA::BinnedEstimatePtr<string> _beaminfo;

    /// Total number of trials
    double _ntrials;

    /// Total number of entries of the current HepMC file
    CounterPtr _fileCounter;

    /// Toggle for multi-file runs
    bool _isEndOfFile;

    /// Nominal cross-section
    std::pair<double,double> _userxs;

    /// Beams used by this run.
    ParticlePair _beams;

    /// Flag to check if init has been called
    bool _initialised;

    /// Flag whether input event beams should be ignored in compatibility check
    bool _checkBeams;

    /// Flag to check if multiweights should be ignored
    bool _skipMultiWeights;

    /// String of weight names (or regex) to select multiweights
    std::string _matchWeightNames;

    /// String of weight names (or regex) to veto multiweights
    std::string _unmatchWeightNames;

    /// String giving the nominal weight name
    std::string _nominalWeightName;

    /// weight cap value
    double _weightCap;

    /// The relative width of the NLO smearing window.
    ///
    /// @todo Improve & standardise name
    double _NLOSmearing;

    /// Current event number
    int _eventNumber;

    /// The index in the (original) weight vector for the nominal weight stream
    size_t _defaultWeightIdx;

    /// The index in the (possibly pruned) weight vector for the nominal weight stream
    size_t _rivetDefaultWeightIdx;

    /// The index of the (possibly user-specified) intended default stream
    int _customDefaultWeightIdx;

    /// How often Rivet runs finalize() and writes the result to a YODA file.
    int _dumpPeriod;

    /// The name of a YODA file to which Rivet periodically dumps results.
    string _dumpFile;

    /// Flag to indicate periodic AO dumping is in progress
    bool _dumping;

    /// Flag to indicate bootstrap dumping is in progress
    ofstream _fbootstrap;

    /// Name of the file that the bootstrap dumping should be written to
    std::string _bootstrapfilename;

    /// Projection Handler
    ProjectionHandler _projHandler;

    /// @}

  };


}

#endif
