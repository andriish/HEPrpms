// -*- C++ -*-
//
// This is the implementation of the non-inlined, non-templated member
// functions of the RivetAnalysis class.
//
#include <config.h>
#include "ThePEG/Interface/Interfaced.h"
#include "ThePEG/Interface/ClassDocumentation.h"
#include "ThePEG/Interface/ParVector.h"
#include "ThePEG/Interface/Parameter.h"
#include "ThePEG/Interface/Switch.h"
#include "ThePEG/Persistency/PersistentOStream.h"
#include "ThePEG/Persistency/PersistentIStream.h"
#include "ThePEG/Vectors/HepMCConverter.h"
#include "ThePEG/Config/HepMCHelper.h"
#include "ThePEG/Repository/EventGenerator.h"
#include "ThePEG/Repository/CurrentGenerator.h"
#include "RivetAnalysis.h"
#include "Rivet/AnalysisHandler.hh"
#include "Rivet/Tools/RivetPaths.hh"
#include "Rivet/Tools/Logging.hh"
#include "ThePEG/Utilities/DescribeClass.h"

using namespace ThePEG;

RivetAnalysis::RivetAnalysis() :  _debug(false), _rivet(), _nevent(0), _checkBeams(true)
{}

void RivetAnalysis::analyze(ThePEG::tEventPtr event, long ieve, int loop, int state) {
  ++_nevent;
  AnalysisHandler::analyze(event, ieve, loop, state);
  // Rotate to CMS, extract final state particles and call analyze(particles).
  // convert to hepmc
  HepMC::GenEvent * hepmc = ThePEG::HepMCConverter<HepMC::GenEvent>::convert(*event);
  // analyse the event
  if(_nevent>1) CurrentGenerator::Redirect stdout(cout);
  if ( _rivet ){
#if ThePEG_RIVET_VERSION > 1
    try {
      _rivet->analyze(*hepmc);
    } catch (const YODA::Exception & e) {
      Throw<Exception>() << "Warning: Rivet/Yoda got the exception: "<< e.what()<<"\n"
                         << Exception::warning;
    }
#else
#error "Unknown ThePEG_RIVET_VERSION"
#endif
  }
  if(_nevent<=1) {
    // check that analysis list is still available
    if ( _rivet->analysisNames().size() != _analyses.size() ) {
      throw ThePEG::Exception() 
	<< "Rivet could not find all requested analyses.\n"
	<< "Use 'rivet --list-analyses' to check availability.\n"
	<< ThePEG::Exception::runerror;
    }
  }
  // delete hepmc event
  delete hepmc;
}

ThePEG::IBPtr RivetAnalysis::clone() const {
  return new_ptr(*this);
}

ThePEG::IBPtr RivetAnalysis::fullclone() const {
  return new_ptr(*this);
}

void RivetAnalysis::persistentOutput(ThePEG::PersistentOStream & os) const {
  os << _analyses << _preload << _paths << _filename << _debug << _checkBeams;
}

void RivetAnalysis::persistentInput(ThePEG::PersistentIStream & is, int) {
  is >> _analyses >> _preload >> _paths >> _filename >> _debug >> _checkBeams;
}

// The following static variable is needed for the type
// description system in ThePEG.
DescribeClass<RivetAnalysis,AnalysisHandler>
describeRivetAnalysis("ThePEG::RivetAnalysis", "RivetAnalysis.so");

void RivetAnalysis::Init() {

  static ThePEG::ClassDocumentation<RivetAnalysis> documentation
    ("The RivetAnalysis class is a simple class to allow analyses"
     " from the Rivet library to be called from ThePEG");

  static ThePEG::ParVector<RivetAnalysis,string> interfaceAnalyses
    ("Analyses",
     "The names of the Rivet analyses to use",
     &RivetAnalysis::_analyses, -1, "", "","" "",
     false, false, ThePEG::Interface::nolimits);

  static ParVector<RivetAnalysis,string> interfacePreLoad
    ("PreLoad",
     "The yoda files to be preloaded",
     &RivetAnalysis::_preload, -1, "", "", "",
     false, false, Interface::nolimits);
  
  static ThePEG::ParVector<RivetAnalysis,string> interfacePaths
    ("Paths",
     "The directory paths where Rivet should look for analyses.",
     &RivetAnalysis::_paths, -1, "", "","" "",
     false, false, ThePEG::Interface::nolimits);

  static Parameter<RivetAnalysis,string> interfaceFilename
    ("Filename",
#if ThePEG_RIVET_VERSION > 1
     "The name of the file where the YODA histograms are put. If empty, "
     "the run name will be used instead. '.yoda' will in any case be "
     "appended to the file name.",
#else
#error "Unknown ThePEG_RIVET_VERSION"
#endif
     &RivetAnalysis::_filename, "", true, false);


  static Switch<RivetAnalysis,bool> interfaceDebug
    ("Debug",
     "Enable debug information from Rivet",
     &RivetAnalysis::_debug, false, true, false);
  static SwitchOption interfaceDebugNo
    (interfaceDebug,
     "No",
     "Disable debug information.",
     false);
  static SwitchOption interfaceDebugYes
    (interfaceDebug,
     "Yes",
     "Enable debug information from Rivet.",
     true);

  static Switch<RivetAnalysis,bool> interfaceCheckBeams
    ("CheckBeams",
     "Whether or not to get rivet to check the beams for the analyses",
     &RivetAnalysis::_checkBeams, true, false, false);
  static SwitchOption interfaceCheckBeamsYes
    (interfaceCheckBeams,
     "Yes",
     "Check the beams(default)",
     true);
  static SwitchOption interfaceCheckBeamsNo
    (interfaceCheckBeams,
     "No",
     "Don't check the beams",
     false);

  interfaceAnalyses.rank(10);

}

void RivetAnalysis::dofinish() {
  AnalysisHandler::dofinish();
  if( _nevent > 0 && _rivet ) {
    CurrentGenerator::Redirect stdout(cout);
#if ThePEG_RIVET_VERSION > 2
    _rivet->setCrossSection(make_pair(generator()->integratedXSec()/picobarn,
                                      generator()->integratedXSecErr()/picobarn));
#else
    _rivet->setCrossSection(generator()->integratedXSec()/picobarn);
#endif
    _rivet->finalize();

    string fname = _filename;
#if ThePEG_RIVET_VERSION > 1
    if ( fname.empty() )
      fname = generator()->path() + "/" + generator()->runName() + ".yoda";
#else
#error "Unknown ThePEG_RIVET_VERSION"
#endif
    _rivet->writeData(fname);
  }
  delete _rivet;
  _rivet = nullptr;
}

void RivetAnalysis::doinit() {
  AnalysisHandler::doinit();
  if(_analyses.empty()) 
    throw ThePEG::Exception() << "Must have at least one analysis loaded in "
			      << "RivetAnalysis::doinitrun()"
			      << ThePEG::Exception::runerror;

  // check that analysis list is available
  _rivet = new Rivet::AnalysisHandler; //(fname);
  _rivet->checkBeams(_checkBeams);
  for ( int i = 0, N = _paths.size(); i < N; ++i ) Rivet::addAnalysisLibPath(_paths[i]);
  _rivet->addAnalyses(_analyses);
  if ( _rivet->analysisNames().size() != _analyses.size() ) {
    throw ThePEG::Exception() 
      << "Rivet could not find all requested analyses.\n"
      << "Use 'rivet --list-analyses' to check availability.\n"
      << ThePEG::Exception::runerror;
  }
  delete _rivet;
  _rivet = 0;
}

void RivetAnalysis::doinitrun() {
  AnalysisHandler::doinitrun();
  // create Rivet analysis handler
  CurrentGenerator::Redirect stdout(cout);
  _rivet = new Rivet::AnalysisHandler;
  _rivet->checkBeams(_checkBeams);
  for ( int i = 0, N = _paths.size(); i < N; ++i ) Rivet::addAnalysisLibPath(_paths[i]);
  _rivet->addAnalyses(_analyses);
  // check that analysis list is still available
  if ( _rivet->analysisNames().size() != _analyses.size() ) {
    throw ThePEG::Exception() 
      << "Rivet could not find all requested analyses.\n"
      << "Use 'rivet --list-analyses' to check availability.\n"
      << ThePEG::Exception::runerror;
  }
  // preload files
#if ThePEG_RIVET_VERSION > 2
  for(string fname : _preload) {
    _rivet->readData(fname);
  }
#else
  if(!_preload.empty())
    throw Exception() << "You have requested yoda files are preloaded by Rivet but this only supported for Rivet 3 and above"; 
#endif
  // debugging
  if ( _debug )
    Rivet::Log::setLevel("Rivet",Rivet::Log::DEBUG);
}
