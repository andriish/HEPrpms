// -*- C++ -*-
//
// GoSamAmplitude.cc is a part of Herwig - A multi-purpose Monte Carlo event generator
// Copyright (C) 2002-2019 The Herwig Collaboration
//
// Herwig is licenced under version 3 of the GPL, see COPYING for details.
// Please respect the MCnet academic guidelines, see GUIDELINES for details.
//
//
// This is the implementation of the non-inlined, non-templated member
// functions of the GoSamAmplitude class.
//

#include "GoSamAmplitude.h"

#include "ThePEG/Interface/ClassDocumentation.h"
#include "ThePEG/EventRecord/Particle.h"
#include "ThePEG/Repository/UseRandom.h"
#include "ThePEG/Repository/EventGenerator.h"
#include "ThePEG/Utilities/DescribeClass.h"
#include "ThePEG/Interface/Reference.h"
#include "ThePEG/Interface/RefVector.h"
#include "ThePEG/Interface/Parameter.h"
#include "ThePEG/Interface/Switch.h"
#include "ThePEG/Interface/Command.h"

#include "ThePEG/Persistency/PersistentOStream.h"
#include "ThePEG/Persistency/PersistentIStream.h"
#include "ThePEG/Utilities/StringUtils.h"

#include "Herwig/MatrixElement/Matchbox/MatchboxFactory.h"
#include "Herwig/API/Filesystem.h"

#include <fstream>
#include <sstream>
#include <string>
#include <cstdlib>
#include <exception>

namespace bfs = Herwig::filesystem;

using namespace Herwig;

#ifndef HERWIG_BINDIR
#error Makefile.am needs to define HERWIG_BINDIR
#endif
#ifndef HERWIG_PKGDATADIR
#error Makefile.am needs to define HERWIG_PKGDATADIR
#endif
#ifndef GOSAM_PREFIX
#error Makefile.am needs to define GOSAM_PREFIX
#endif

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


GoSamAmplitude::GoSamAmplitude() : 
  theAccuracyTarget(6),theCodeExists(false),theFormOpt(true),theNinja(true),
  theHiggsEff(false),theMassiveLeptons(false),theLoopInducedOption(0),
  isitDR(false),doneGoSamInit(false),doneGoSamInitRun(false),
  bindir_(HERWIG_BINDIR), pkgdatadir_(HERWIG_PKGDATADIR), GoSamPrefix_(GOSAM_PREFIX)
{}

GoSamAmplitude::~GoSamAmplitude() {}

IBPtr GoSamAmplitude::clone() const {
  return new_ptr(*this);
}

IBPtr GoSamAmplitude::fullclone() const {
  return new_ptr(*this);
}


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


void GoSamAmplitude::doinit() {
  optionalContractFile() = name() + ".OLPContract.lh";
  MatchboxOLPME::doinit();
  doneGoSamInit = true;
}

void GoSamAmplitude::doinitrun() {
  optionalContractFile() = name() + ".OLPContract.lh";
  MatchboxOLPME::doinitrun();
  doneGoSamInitRun = true;
}


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


extern "C" void OLP_Start(const char*, int* i);
extern "C" void OLP_Polvec(double*, double*, double*);
extern "C" void OLP_SetParameter(char*, double*, double*, int*);
extern "C" void OLP_PrintParameter(char*);
extern "C" void OLP_EvalSubProcess2(int*, double*, double*, double*, double*);


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


bool GoSamAmplitude::startOLP(const map<pair<Process, int>, int>& procs) {

  char char_cwd[256];
  getcwd(char_cwd, sizeof(char_cwd));
  string cwd = string(char_cwd);

  string folderMatchboxBuild = factory()->buildStorage();
  folderMatchboxBuild.erase(folderMatchboxBuild.begin());

  // set all necessary path and file names
  gosamPath = gosamPathInterface == "" ? cwd + folderMatchboxBuild + "GoSam" : gosamPathInterface;
  // When transitioning to C++ 11 this length()-1 workaround can be replaced by string.back()
  if (gosamPath.at(gosamPath.length()-1) != '/') gosamPath.append("/");
  gosamSourcePath = gosamPath + "source/";
  gosamInstallPath = gosamPath + "build/";

  // create all the directories
  if (!bfs::is_directory(gosamPath)){
    try {
      bfs::create_directory(gosamPath);
    } catch (exception& e) {
      throw Exception()
	<< "--------------------------------------------------------------------------------\n"
	<< "The following exception occured:\n\n"
	<< " " << e.what() << "\n\n"
	<< " -> Please create the parent directory of\n"
	<< "      " << gosamPath << "\n"
	<< "    manually!\n"
	<< "--------------------------------------------------------------------------------\n"
	<< Exception::runerror;
    }
  }
  if (!bfs::is_directory(gosamSourcePath)) bfs::create_directory(gosamSourcePath);
  if (!bfs::is_directory(gosamInstallPath)) bfs::create_directory(gosamInstallPath);

  contractFileTitle = name() + ".OLPContract.lh";
  contractFileName = gosamPath + "/" + contractFileTitle;

  string orderFileName = gosamPath + "/" + name() + ".OLPOrder.lh";

  // Set the path variable (plus file name) where to find the GoSam specific input file
  gosamSetupInFileName = gosamSetupInFileNameInterface == "" ? gosamPath + "/setup.gosam.in" : gosamSetupInFileNameInterface;

  // Use the python script gosam2herwig to make replacements in the GoSam 
  // specific input file at gosamSetupInFileName. If the GoSam input file
  // does not exist yet at gosamSetupInFileName the python script will get 
  // it from src/defaults/ before making the replacements.
  string cmd = "python "+bindir_+"/gosam2herwig ";
  cmd+=" --usrinfile="+gosamSetupInFileNameInterface;
  cmd+=" --infile="+gosamSetupInFileName+".tbu";
  cmd+=" --definfile="+pkgdatadir_+"/defaults/setup.gosam.in";
  cmd+=" --formtempdir="+StringUtils::replace(gosamSourcePath, string("/"), string("\\/"));    //@FORMTEMPDIR@
  cmd+=" --reduction="+(theNinja ? string("ninja,golem95") : string("samurai,golem95"));       //@REDUCTIONPROGRAMS@
  cmd+=" --formopt="+(theFormOpt ? string("") : string(", noformopt"));   //@FORMOPT@
  cmd+=" --higgseff="+(theHiggsEff ? string("smehc") : string("smdiag"));   //@MODEL@
  std::system(cmd.c_str());

  if ( factory()->initVerbose() ) {

    generator()->log() << "\n\n>>> NOTE: According to the repository settings for the GoSam interface:\n" << flush;

    if (theHiggsEff) generator()->log() << "\n    -- GoSam will use a model with an effective ggH coupling (model=smehc).\n" << flush;
    else if (!theHiggsEff) generator()->log() << "\n    -- GoSam will use its default model (model=smdiag).\n" << flush;

    if (theNinja) generator()->log() << "    -- GoSam will use Ninja as reduction program (reduction_programs=ninja,golem95).\n" << flush;
    else if (!theNinja) generator()->log() << "    -- GoSam will use Samurai as reduction program (reduction_programs=samurai,golem95).\n" << flush;

    if (theFormOpt) generator()->log() << "    -- Form optimization switched on (extensions=autotools).\n" << flush;
    else if (!theFormOpt) generator()->log() << "    -- Form optimization switched off  (extensions=autotools, noformopt).\n" << flush;

    if (theNinja && !theFormOpt) throw Exception() << "GoSamAmplitude: Ninja reduction needs form optimization!\n" << Exception::runerror;

    if (gosamSetupInFileNameInterface == "") {
      generator()->log() << "\n    Please be aware that you are using a copy of the default GoSam input file!\n" 
			 << "    Please note that if you need special options to be considered for the specific\n"
			 << "    process you are looking at (diagram filtering, etc.) these are not automatically\n"
			 << "    set for you. In that case please consider to specify your own GoSam input file\n"
			 << "    via 'set " << name() << ":SetupInFilename' in the input file.\n\n" << flush; 
    }

    // If one uses a custom GoSam input file at gosamSetupInFileName = gosamSetupInFileNameInterface
    // then please note that not all options in there might match the corresponding Herwig repository
    // options
    if (gosamSetupInFileNameInterface != "") {
      generator()->log() << "\n    Please be aware that you are using a custom GoSam input file!\n" 
			 << "    Please note that if you have set the options for model, reduction_programs,\n" 
			 << "    extensions and/or form.tempdir manually these will of course not be replaced\n" 
			 << "    by the corresponding repository settings mentioned above.\n\n" << flush;
    }

    generator()->log() << "\n>>> NOTE: GoSam may return the set of used parameters for this process via the OLP_PrintParameter() function:\n\n"
		       << "    -- If Debug::level > 1, the OLP parameters are being written to file: at " << factory()->runStorage() + name() + ".OLPParameters.lh.\n\n" << flush;

  }

  double accuracyTarget = 1.0/pow(10.0,accuracyTargetNegExp());
  time_t rawtime;
  time (&rawtime);
  accuracyFileTitle = name() + ".OLPAccuracy.lh";
  accuracyFile = factory()->buildStorage() + accuracyFileTitle;
  ofstream accuracyFileStream;

  if ( Debug::level > 1 ) {
    accuracyFileStream.open(accuracyFile.c_str()); // Opening accuracyFile once here removes all previous content before the read step
    accuracyFileStream << "\nFile to contain those PSPs for which GoSam evaluated one-loop interference terms or loop induced ME2s\n"
                       << "with acc > target accuracy = " << accuracyTarget << ". Date/Time: " << ctime(&rawtime) << endl;
  }

  if ( factory()->initVerbose() ) {

    generator()->log() << "\n>>> NOTE: GoSam will return the accuracy of one-loop interference terms or loop induced ME2s\n"
		       << "    at every PSP via the BLHA2 acc parameter:\n\n"
		       << "    -- In cases where acc > 10^-AccuracyTarget = " << accuracyTarget << " the corresponding PSPs are being dis-\n"
		       << "       carded.\n"
		       << "    -- The default value for AccuracyTarget is 6, but you may consider setting it otherwise\n"
		       << "       via 'set " << name() << ":AccuracyTarget' in the input file.\n"
		       << "    -- Currently the value for AccuracyTarget is set to " << accuracyTargetNegExp() << ".\n"
		       << "    -- If Debug::level > 1, the discarded PSPs are being written to file: at " + accuracyFile << ".\n"
		       << "    -- If the amount of PSPs with acc > " << accuracyTarget << " is significant, please consider to re-evaluate\n"
		       << "       your process setup (accuracy target, masses, cuts, etc.)!\n\n\n" << flush;

  }

  // check for old order file and create it if it doesn't already exist
  fillOrderFile(procs, orderFileName);

  ifstream ifile(contractFileName.c_str());
  if(!ifile){
    signOLP(orderFileName, contractFileName);
  }

  if ( !checkOLPContract(contractFileName) ) {
    throw Exception() << "GoSamAmplitude: failed to start GoSam" << Exception::runerror;
  }

    if (!( DynamicLoader::load(gosamInstallPath+"/lib/libgolem_olp.so")
          || DynamicLoader::load(gosamInstallPath+"/lib64/libgolem_olp.so")
          || DynamicLoader::load(gosamInstallPath+"/lib/libgolem_olp.dylib")
          || DynamicLoader::load(gosamInstallPath+"/lib64/libgolem_olp.dylib"))) buildGoSam();

  int status = -1;
  startOLP(contractFileTitle, status);

  if ( status != 1 ) return false;

  return true;

}

void GoSamAmplitude::startOLP(const string& contract, int& status) {

  string tempcontract = contract;

  char char_cwd[256];
  getcwd(char_cwd, sizeof(char_cwd));
  string cwd = string(char_cwd);

  string folderMatchboxBuild = factory()->buildStorage();
  folderMatchboxBuild.erase(folderMatchboxBuild.begin());

  gosamPath = gosamPathInterface == "" ? cwd + folderMatchboxBuild + "GoSam" : gosamPathInterface;
  // When transitioning to C++ 11 this length()-1 workaround can be replaced by string.back()
  if (gosamPath.at(gosamPath.length()-1) != '/') gosamPath.append("/");

    if (!( DynamicLoader::load(gosamPath+"build/lib/libgolem_olp.so")
          || DynamicLoader::load(gosamPath+"build/lib64/libgolem_olp.so")
          || DynamicLoader::load(gosamPath+"build/lib/libgolem_olp.dylib")
          || DynamicLoader::load(gosamPath+"build/lib64/libgolem_olp.dylib")))
    throw Exception() << "GoSamAmplitude: Failed to load GoSam. Please check the log file.\n"
		      << Exception::runerror;
  tempcontract = gosamPath + tempcontract;

  OLP_Start(tempcontract.c_str(), &status);

  // hand over input parameters for EW scheme considered
  int pStatus = 0;
  double zero = 0.0;
  if ( SM().ewScheme() == 0 || SM().ewScheme() == 6 ) { // EW/Scheme Default and EW/Scheme Independent
    throw Exception() << "GoSamAmplitude: `Best value' schemes are not supported by GoSam"
                      << Exception::runerror;
  } else if ( SM().ewScheme() == 4 ) { // EW/Scheme mW (uses mW,GF,sin2thetaW) seems not to be supported by GoSam
    throw Exception() << "GoSamAmplitude: `mW' scheme is not supported by GoSam"
                      << Exception::runerror;
  } else if ( SM().ewScheme() == 1 ) { // EW/Scheme GMuScheme (uses mW,mZ,GF)
    double in1=getParticleData(ParticleID::Z0)->hardProcessMass()/GeV;
    double in2=getParticleData(ParticleID::Wplus)->hardProcessMass()/GeV;
    double in3=SM().fermiConstant()*GeV2;
    OLP_SetParameter((char *)"mass(23)",&in1,&zero,&pStatus);
    OLP_SetParameter((char *)"mass(24)",&in2,&zero,&pStatus);
    OLP_SetParameter((char *)"Gf",&in3,&zero,&pStatus);
  } else if ( SM().ewScheme() == 2 ) { // EW/Scheme alphaMZScheme (uses mW,mZ,alpha(mZ))
    double in1=getParticleData(ParticleID::Z0)->hardProcessMass()/GeV;
    double in2=getParticleData(ParticleID::Wplus)->hardProcessMass()/GeV;
    double in3=SM().alphaEMMZ();
    OLP_SetParameter((char *)"mass(23)",&in1,&zero,&pStatus);
    OLP_SetParameter((char *)"mass(24)",&in2,&zero,&pStatus);
    OLP_SetParameter((char *)"alpha",&in3,&zero,&pStatus);
  } else if ( SM().ewScheme() == 3 ) { // EW/Scheme NoMass (uses alpha(mZ),GF,sin2thetaW)
    double in1=SM().fermiConstant()*GeV2;
    double in2=SM().alphaEMMZ();
    double in3=SM().sin2ThetaW();
    OLP_SetParameter((char *)"Gf",&in1,&zero,&pStatus);
    OLP_SetParameter((char *)"alpha",&in2,&zero,&pStatus);
    OLP_SetParameter((char *)"sw2",&in3,&zero,&pStatus);
  } else if ( SM().ewScheme() == 5 ) { // EW/Scheme mZ (uses mZ,alphaEM,sin2thetaW)
    double in1=getParticleData(ParticleID::Z0)->hardProcessMass()/GeV;
    double in2=SM().alphaEMMZ();
    double in3=SM().sin2ThetaW();
    OLP_SetParameter((char *)"mass(23)",&in1,&zero,&pStatus);
    OLP_SetParameter((char *)"alpha",&in2,&zero,&pStatus);
    OLP_SetParameter((char *)"sw2",&in3,&zero,&pStatus);
  } else if ( SM().ewScheme() == 7 ) { // EW/Scheme FeynRulesUFO (uses mZ,GF,alpha(mZ))
    double in1=getParticleData(ParticleID::Z0)->hardProcessMass()/GeV;
    double in2=SM().alphaEMMZ();
    double in3=SM().fermiConstant()*GeV2;
    OLP_SetParameter((char *)"mass(23)",&in1,&zero,&pStatus);
    OLP_SetParameter((char *)"alpha",&in2,&zero,&pStatus);
    OLP_SetParameter((char *)"Gf",&in3,&zero,&pStatus);
  }
	
  // hand over widths of Z and W
  double wZ = getParticleData(23)->hardProcessWidth()/GeV;
  double wW = getParticleData(24)->hardProcessWidth()/GeV;
  OLP_SetParameter((char*)"width(23)",&wZ,&zero,&pStatus);
  OLP_SetParameter((char*)"width(24)",&wW,&zero,&pStatus);

  // hand over mass and width of the Higgs
  double wH = getParticleData(25)->hardProcessWidth()/GeV;
  double mH = getParticleData(25)->hardProcessMass()/GeV;
  OLP_SetParameter((char*)"width(25)",&wH,&zero,&pStatus);
  OLP_SetParameter((char*)"mass(25)",&mH,&zero,&pStatus);

  // hand over initial input parameter for alphaS
  double as = SM().alphaS();
  OLP_SetParameter((char *)"alphaS", &as, &zero, &pStatus);

  // fill massive Particle vector
  if (massiveParticles.empty()) {
    // with quark masses
    for (int i=1; i<=6; ++i) 
      if (getParticleData(i)->hardProcessMass()/GeV > 0.0) massiveParticles.push_back(i);
    // with lepton masses
      if (theMassiveLeptons && getParticleData(11)->hardProcessMass()/GeV > 0.0) massiveParticles.push_back(11);
      if (theMassiveLeptons && getParticleData(13)->hardProcessMass()/GeV > 0.0) massiveParticles.push_back(13);
      if (theMassiveLeptons && getParticleData(15)->hardProcessMass()/GeV > 0.0) massiveParticles.push_back(15);
  }

  // hand over quark (and possibly lepton) masses and widths (iff massive)
  if ( massiveParticles.size() != 0 ) {
    for ( vector<int>::const_iterator mID = massiveParticles.begin(); mID != massiveParticles.end(); ++mID ) {
        string mstr;
        string wstr;
        int mInt = *mID;
        double mass=getParticleData(mInt)->hardProcessMass()/GeV;
        double width=getParticleData(mInt)->hardProcessWidth()/GeV;
        std::stringstream ss;
        ss << mInt;
        string str = ss.str();
        mstr="mass("+str+")";
        wstr="width("+str+")";
        char * mchar = new char[mstr.size()+1];
        char * wchar = new char[wstr.size()+1];
        std::copy(mstr.begin(),mstr.end(),mchar);
        std::copy(wstr.begin(),wstr.end(),wchar);
        mchar[mstr.size()] = '\0';
        wchar[wstr.size()] = '\0';
        OLP_SetParameter( mchar, &mass, &zero, &pStatus );
        OLP_SetParameter( wchar, &width, &zero, &pStatus );
        delete[] mchar;
        delete[] wchar;
        
//      Nicer but not working properly:
        
//      double mass=getParticleData(*mID)->hardProcessMass()/GeV;
//      double width=getParticleData(*mID)->hardProcessWidth()/GeV;
//      string mstr="mass("+static_cast<ostringstream*>(&(ostringstream()<<(*mID)))->str()+")";
//      string wstr="width("+static_cast<ostringstream*>(&(ostringstream()<<(*mID)))->str()+")";
//        cout<<"\n massiv "<<mstr;
//        
//      OLP_SetParameter((char *)&mstr,&mass, &zero, &pStatus );
//      OLP_SetParameter((char *)&wstr,&width, &zero, &pStatus );
    }
  }

  // Note: In the GoSam input file, the standard is to set the parameter 
  // 'symmetries' for quark families and lepton generations, which allow
  // for flavour changing only between families/generations. If this pa-
  // rameter is set, GoSam won't allow to set electron and muon mass and
  // width via the interface. Also setting mass and width for the tau is 
  // not yet considered.

  // print OLP parameters
  if ( Debug::level > 1 ) {
    string ppstr = factory()->runStorage() + name() + ".OLPParameters.lh";
    OLP_PrintParameter(const_cast<char*>(ppstr.c_str()));
  }

  didStartOLP() = true;

}


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


void GoSamAmplitude::fillOrderFile(const map<pair<Process, int>, int>& procs, string orderFileName) {

  for ( map<pair<Process, int>, int>::const_iterator p = procs.begin() ; p != procs.end() ; ++p ) {
    std::stringstream Processstr;
    std::stringstream Typestr;
    Processstr << (*p).first.first.legs[0]->id() << " " << (*p).first.first.legs[1]->id() << " -> ";
    for ( PDVector::const_iterator o = (*p).first.first.legs.begin() + 2 ; o != (*p).first.first.legs.end() ; ++o )
      Processstr << (**o).id() << " ";
    if ( (*p).first.second == ProcessType::treeME2 ) {
      Typestr << "Tree";
    } else if ( (*p).first.second == ProcessType::loopInducedME2 ) {
      Typestr << "LoopInduced";
    } else if ( (*p).first.second == ProcessType::colourCorrelatedME2 ) {
      Typestr << "ccTree";
    } else if ( (*p).first.second == ProcessType::spinColourCorrelatedME2 ) {
      Typestr << "scTree";
    } else if ( (*p).first.second == ProcessType::oneLoopInterference ) {
      Typestr << "Loop";
    }
    gosamprocinfo pro = gosamprocinfo((*p).second, -1, Processstr.str(), Typestr.str());
    pro.setOAs(p->first.first.orderInAlphaS);
    pro.setOAew(p->first.first.orderInAlphaEW);
    processmap[(*p).second] = pro;
  }

  ifstream oldOrderFileStream(orderFileName.c_str());
  if (oldOrderFileStream){
    oldOrderFileStream.close();
    return;
  }

  ofstream orderFile(orderFileName.c_str());
  int asPower = 100;
  int minlegs = 100;
  int maxlegs = -1;
  int maxasPower = -1;
  int aewPower = 100;
  int maxaewPower = -1;

  for ( map<pair<Process, int>, int>::const_iterator t = procs.begin() ; t != procs.end() ; ++t ) {
    asPower = min(asPower, static_cast<int>(t->first.first.orderInAlphaS));
    minlegs = min(minlegs, static_cast<int>(t->first.first.legs.size()));
    maxlegs = max(maxlegs, static_cast<int>(t->first.first.legs.size()));
    maxasPower = max(maxasPower, static_cast<int>(t->first.first.orderInAlphaS));
    aewPower = min(aewPower, static_cast<int>(t->first.first.orderInAlphaEW));
    maxaewPower = max(maxaewPower, static_cast<int>(t->first.first.orderInAlphaEW));
  }

  orderFile << "# OLP order file created by Herwig/Matchbox for GoSam\n\n";
  orderFile << "InterfaceVersion         BLHA2\n";
  orderFile << "MatrixElementSquareType  CHsummed\n";
  orderFile << "CorrectionType           QCD\n";
  orderFile << "IRregularisation         " << (isDR() ? "DRED" : "CDR") << "\n";

  // loop over quarks to check if they have non-zero masses
  for (int i=1; i<=6; ++i) if (getParticleData(i)->hardProcessMass()/GeV > 0.0) massiveParticles.push_back(i);

  // check if leptons have non-zero masses (iff theMassiveLeptons==true)
  if (theMassiveLeptons && getParticleData(11)->hardProcessMass()/GeV > 0.0) massiveParticles.push_back(11);
  if (theMassiveLeptons && getParticleData(13)->hardProcessMass()/GeV > 0.0) massiveParticles.push_back(13);
  if (theMassiveLeptons && getParticleData(15)->hardProcessMass()/GeV > 0.0) massiveParticles.push_back(15);

  if ( massiveParticles.size() != 0 ) {
    orderFile << "MassiveParticles         ";
    for ( vector<int>::const_iterator mID = massiveParticles.begin(); mID != massiveParticles.end(); ++mID ) {
      int mInt = *mID;
      orderFile << mInt << " ";
    }
    orderFile << "\n";
  }
  orderFile << "\n";

  vector < string > types;
  types.push_back("Tree");
  types.push_back("LoopInduced");
  types.push_back("ccTree");
  types.push_back("scTree");
  types.push_back("Loop");

  for ( int i = asPower ; i != maxasPower + 1 ; i++ ) {
    for ( int j = aewPower ; j != maxaewPower + 1 ; j++ ) {
      orderFile << "\nAlphasPower              " << i << "\n";
      orderFile << "AlphaPower              " << j << "\n";
      for ( vector<string>::iterator it = types.begin() ; it != types.end() ; it++ ) {
        if ( *it == "LoopInduced" ) continue;
        for ( map<int, gosamprocinfo>::iterator p = processmap.begin() ; p != processmap.end() ; ++p )
          if ( (*p).second.Tstr() == *it && i == (*p).second.orderAs() && j == (*p).second.orderAew() ) {
            orderFile << "\nAmplitudeType " << *it << "\n";
                  break;
          }
        for ( map<int, gosamprocinfo>::iterator p = processmap.begin() ; p != processmap.end() ; ++p )
          if ( (*p).second.Tstr() == *it && i == (*p).second.orderAs() && j == (*p).second.orderAew() ) {
            orderFile << (*p).second.Pstr() << "\n";
          }
      }
    }
  }

  // Write out the loop induced processes separately
  int asPowerLI = 100;
  int aewPowerLI = 100;
  for ( map<int, gosamprocinfo>::iterator p = processmap.begin() ; p != processmap.end() ; ++p ) {
    if ( (*p).second.Tstr() != "LoopInduced" ) continue;
    if ( (*p).second.orderAs() != asPowerLI || (*p).second.orderAew() != aewPowerLI ) {
      asPowerLI = (*p).second.orderAs();
      aewPowerLI = (*p).second.orderAew();
      // At the moment GoSam requires for qcd loop induced processes the as coupling power 
      // which would correspond to an associated  fictitious Born process
      orderFile << "\nAlphasPower              " << (asPowerLI-2) << "\n";
      orderFile << "AlphaPower              " << aewPowerLI << "\n";
      orderFile << "\nAmplitudeType " << "LoopInduced" << "\n";
    }
    orderFile << (*p).second.Pstr() << "\n";
  }
	orderFile << flush;

}

void GoSamAmplitude::signOLP(const string& order, const string& contract) {
  if(!theCodeExists){
    char char_cwd[256];
    getcwd(char_cwd, sizeof(char_cwd));
    string cwd = string(char_cwd);

    string folderMatchboxBuild = factory()->buildStorage();
    folderMatchboxBuild.erase(folderMatchboxBuild.begin());

    generator()->log() << "\n>>> generating GoSam amplitudes. This may take some time, please be patient.\n"
                       << ">>> see " + cwd + folderMatchboxBuild + "gosam-amplitudes.log for details.\n" << flush;
    string cmd = GoSamPrefix_+"/bin/gosam.py --olp --output-file=" + contract + " --config=" + 
      gosamSetupInFileName+".tbu" + " --destination=" + gosamSourcePath + " " + order + " > " + cwd + folderMatchboxBuild + "gosam-amplitudes.log 2>&1";
    std::system(cmd.c_str());
    cmd = "python "+bindir_+"/gosam2herwig ";
    cmd += " --makelink ";
    // cmd += " --makelinkfrom=contract ";
    cmd += " --makelinkfrom="+gosamPath+"/"+name()+".OLPContract.lh";
    cmd += " --makelinkto="+factory()->buildStorage() + name() + ".OLPContract.lh";
    std::system(cmd.c_str());
  }
}

bool GoSamAmplitude::checkOLPContract(string contractFileName) {

  ifstream infile(contractFileName.c_str());
  string line;
  vector < string > contractfile;

  while (std::getline(infile, line)) contractfile.push_back(line);

  for ( map<int, gosamprocinfo>::iterator p = processmap.begin() ; p != processmap.end() ; p++ ) {
    bool righttype = false;
    for ( vector<string>::iterator linex = contractfile.begin() ; linex != contractfile.end() ; ++linex ) {
      if ( (*linex).find("AmplitudeType ")!= std::string::npos ) {
        if ( (*linex).find(" " + (*p).second.Tstr() + " ")!= std::string::npos ) {
          righttype = true;
        } else {
          righttype = false;
        }
      }
      if ( righttype ) {
        if ( (*linex).find((*p).second.Pstr()) != std::string::npos && (*p).second.Pstr().length() == (*linex).find("|") ) {
          string sub = (*linex).substr((*linex).find("|") + 1, (*linex).find("#") - (*linex).find("|") - 1); // | 1 23 # buggy??
          if ( sub.find(" 1 ") != 0 ) 
	    throw Exception() << "GoSamAmplitude: Failed to check contractfile. Please check the logfile.\n"
			      << Exception::runerror;
          string subx = sub.substr(3);
          int subint;
          istringstream(subx) >> subint;
          (*p).second.setGID(subint);
        }
      }
    }
  }

  string ids = factory()->buildStorage() + "GoSam.ids.dat";
  ofstream IDS(ids.c_str());
  
  idpair.clear();
  for ( map<int, gosamprocinfo>::iterator p = processmap.begin() ; p != processmap.end() ; p++ )
    idpair.push_back(-1);
  idpair.push_back(-1);

  for ( map<int, gosamprocinfo>::iterator p = processmap.begin() ; p != processmap.end() ; p++ ) {
    
    idpair[(*p).second.HID()]=(*p).second.GID();
    IDS << (*p).second.HID() << " " << (*p).second.GID() << " " << (*p).second.Tstr() << "\n";
    if ( (*p).second.GID() == -1 ) return 0;
  }
  IDS << flush;

  return 1;

}

bool GoSamAmplitude::buildGoSam() {
  if(!theCodeExists){
    generator()->log() << "\n>>> compiling GoSam amplitudes. This may take some time, please be patient.\n"
                       << ">>> see " + gosamSourcePath + "gosam-build.log for details.\n\n" << flush;
    string cmd = "cd " + gosamSourcePath + " && sh autogen.sh FCFLAGS=-g --prefix=" + 
      gosamInstallPath + " --disable-static > gosam-build.log 2>&1";
    std::system(cmd.c_str());
    if (!gosamBuildScript.empty()) {
      cmd = "cd " + gosamSourcePath + " && " + gosamBuildScript + " >> gosam-build.log 2>&1";
      std::system(cmd.c_str());
    }
    std::system(cmd.c_str());
    cmd = "cd " + gosamSourcePath + " && make install >> gosam-build.log 2>&1";
    std::system(cmd.c_str());
  }
  theCodeExists=true;
  return 1;
}

void GoSamAmplitude::getids() const {
  string line = factory()->buildStorage() + "GoSam.ids.dat";
  ifstream infile(line.c_str());
  int hid;
  int gid;
  string type;
  while (std::getline(infile, line)) {
    idpair.push_back(-1);
    idtypepair.push_back(" ");
  }
  infile.close();
  string line2 = factory()->buildStorage() + "GoSam.ids.dat";
  ifstream infile2(line2.c_str());
  idpair.push_back(-1);
  idtypepair.push_back(" ");
  while (std::getline(infile2, line2)) {
    istringstream(line2) >> hid >> gid >> type;
    idpair[hid]=gid;
    
    idtypepair[hid]=type;
  }
  infile.close();
}


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

void GoSamAmplitude::evalSubProcess() const {

  useMe();

  double units = pow(lastSHat() / GeV2, int(mePartonData().size()) - 4.);
  fillOLPMomenta(lastXComb().meMomenta(),mePartonData(),reshuffleMasses());

  double scale = sqrt(mu2() / GeV2);

  if (hasRunningAlphaS()) {
    int pStatus = 0;
    double zero = 0.0;
    double as;
    as = lastAlphaS();
    OLP_SetParameter((char *)"alphaS", &as, &zero, &pStatus);
  }

  double out[7] = { };
  double acc;

  if ( idpair.size() == 0 ){ getids(); }
  int id = -99;
  if ( olpId()[ProcessType::loopInducedME2] ) id = olpId()[ProcessType::loopInducedME2];
  else if ( olpId()[ProcessType::oneLoopInterference] ) id = olpId()[ProcessType::oneLoopInterference];
  else id = olpId()[ProcessType::treeME2];

  int callid(idpair[id]); // If id denotes the Herwig ID, this returns the GoSam ID
  string calltype(idtypepair[id]); // If id denotes the Herwig ID, this returns the amplitude type

  OLP_EvalSubProcess2(&(callid), olpMomenta(), &scale, out, &acc);

  double accuracyTarget = 1.0/pow(10.0,accuracyTargetNegExp());
  accuracyFileTitle = name() + ".OLPAccuracy.lh";
  accuracyFile = factory()->buildStorage() + accuracyFileTitle;
  ofstream accuracyFileStream;

  if ( (olpId()[ProcessType::oneLoopInterference]||olpId()[ProcessType::loopInducedME2]) &&  acc > accuracyTarget ) {
    if ( Debug::level > 1 ) {
      accuracyFileStream.open(accuracyFile.c_str(),ios::app);
      vector<Lorentz5Momentum> currentpsp = lastXComb().meMomenta();
      time_t rawtime;
      time (&rawtime);
      if (doneGoSamInit) accuracyFileStream << "READ phase: ";
      else if (doneGoSamInitRun) accuracyFileStream << "RUN phase: ";
      accuracyFileStream << "Sub-process with Herwig ID = " << id << " and GoSam ID = " << callid << ", " << ctime(&rawtime);
      accuracyFileStream << "GoSam evaluated one-loop interference or loop induced ME2 with acc = " << acc 
                         << " > target accuracy = " << accuracyTarget << ", at PSP [in units of GeV]:" << endl;
      for (size_t i=0; i!=currentpsp.size(); ++i) {
        accuracyFileStream << "(t,x,y,z,mass;m)[" << i << "]=("
                           << currentpsp[i].t()/GeV << ","
                           << currentpsp[i].x()/GeV << ","
                           << currentpsp[i].y()/GeV << ","
                           << currentpsp[i].z()/GeV << ","
                           << currentpsp[i].mass()/GeV << ";"
                           << currentpsp[i].m()/GeV << ")"
                           << endl;
      }
      accuracyFileStream << endl;
    }
    throw Veto(); // Dispose of PSP
  }

  if ( olpId()[ProcessType::oneLoopInterference] ) {
    if (calculateTreeME2()) lastTreeME2(out[3] * units);
    lastOneLoopInterference((out[2])* units);
    lastOneLoopPoles(pair<double, double>(out[0] * units, out[1] * units));
  } else if ( olpId()[ProcessType::treeME2] ) {
    lastTreeME2(out[3] * units);
  } else if ( olpId()[ProcessType::loopInducedME2] ) {
    lastTreeME2(out[2] * units);
  }

}

void GoSamAmplitude::evalColourCorrelator(pair<int, int> ) const {

  double units = pow(lastSHat() / GeV2, int(mePartonData().size()) - 4.);
  fillOLPMomenta(lastXComb().meMomenta(),mePartonData(),reshuffleMasses());

  double scale = sqrt(mu2() / GeV2);

  if (hasRunningAlphaS()) {
    int pStatus = 0;
    double zero = 0.0;
    double as;
    as = lastAlphaS();
    OLP_SetParameter((char *)"alphaS", &as, &zero, &pStatus);
  }

  int n = lastXComb().meMomenta().size();
  colourCorrelatorResults.resize(n * (n - 1) / 2);

  if ( idpair.size() == 0 ) getids();

  int callid(idpair[olpId()[ProcessType::colourCorrelatedME2]]);
  double acc;
  OLP_EvalSubProcess2(&(callid), olpMomenta(), &scale, &colourCorrelatorResults[0], &acc);

  cPDVector particles = lastXComb().matrixElement()->mePartonData();

  for ( int i = 0 ; i < n ; ++i ) {
    for ( int j = i + 1 ; j < n ; ++j ) {
      lastColourCorrelator(make_pair(i, j), colourCorrelatorResults[i+j*(j-1)/2] * units);
    }
  }

}

void GoSamAmplitude::evalSpinColourCorrelator(pair<int , int > ) const {

  double units = pow(lastSHat() / GeV2, int(mePartonData().size()) - 4.);
  fillOLPMomenta(lastXComb().meMomenta(),mePartonData(),reshuffleMasses());

  double scale = sqrt(mu2() / GeV2);

  if (hasRunningAlphaS()) {
    int pStatus = 0;
    double zero = 0.0;
    double as;
    as = lastAlphaS();
    OLP_SetParameter((char *)"alphaS", &as, &zero, &pStatus);
  }

  int n = lastXComb().meMomenta().size();
  spinColourCorrelatorResults.resize(2*n*n);

  if ( idpair.size() == 0 ) getids();

  double acc;
  int callid(idpair[olpId()[ProcessType::spinColourCorrelatedME2]]);
  OLP_EvalSubProcess2(&(callid), olpMomenta(), &scale, &spinColourCorrelatorResults[0], &acc);

  for ( int i = 0; i < n; ++i ) {
    for ( int j = 0; j < n; ++j ) {
      Complex scc(spinColourCorrelatorResults[2*i+2*n*j]*units, spinColourCorrelatorResults[2*i+2*n*j+1]*units);
      lastColourSpinCorrelator(make_pair(i,j),scc);
    }
  }

}

LorentzVector<Complex> GoSamAmplitude::plusPolarization(const Lorentz5Momentum& p, const Lorentz5Momentum& n, int inc) const {

  double pvec[4] = {p.t()/GeV,p.x()/GeV,p.y()/GeV,p.z()/GeV};
  double nvec[4] = {n.t()/GeV,n.x()/GeV,n.y()/GeV,n.z()/GeV};
  double out[8] ={ };
  OLP_Polvec(pvec,nvec,out);

  LorentzVector<Complex> res;
  Complex a(out[0],out[1]);
  res.setT(a);
  Complex b(out[2],out[3]);
  res.setX(b);
  Complex c(out[4],out[5]);
  res.setY(c);
  Complex d(out[6],out[7]);
  res.setZ(d);

  if (inc<2)
    return res.conjugate();
  else 
    return res;

}


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


// If needed, insert default implementations of virtual function defined
// in the InterfacedBase class here (using ThePEG-interfaced-impl in Emacs).


void GoSamAmplitude::persistentOutput(PersistentOStream & os) const {
  os << idpair << idtypepair << processmap << gosamPathInterface
     << gosamSetupInFileNameInterface << gosamBuildScript << gosamPath
     << gosamSourcePath << gosamInstallPath << gosamSetupInFileName
     << orderFileTitle << contractFileTitle
     << contractFileName << orderFileName
     << theCodeExists << theFormOpt << theNinja << isitDR << massiveParticles << theHiggsEff
     << theAccuracyTarget << theMassiveLeptons << theLoopInducedOption
     << doneGoSamInit << doneGoSamInitRun
     << bindir_ << pkgdatadir_ << GoSamPrefix_;
}

void GoSamAmplitude::persistentInput(PersistentIStream & is, int) {
  is >> idpair >> idtypepair >> processmap >> gosamPathInterface
     >> gosamSetupInFileNameInterface >> gosamBuildScript >> gosamPath
     >> gosamSourcePath >> gosamInstallPath >> gosamSetupInFileName
     >> orderFileTitle >> contractFileTitle
     >> contractFileName >> orderFileName
     >> theCodeExists >> theFormOpt >> theNinja >> isitDR >> massiveParticles >> theHiggsEff
     >> theAccuracyTarget >> theMassiveLeptons >> theLoopInducedOption
     >> doneGoSamInit >> doneGoSamInitRun
     >> bindir_ >> pkgdatadir_ >> GoSamPrefix_;
}


// *** Attention *** The following static variable is needed for the type
// description system in ThePEG. Please check that the template arguments
// are correct (the class and its base class), and that the constructor
// arguments are correct (the class name and the name of the dynamically
// loadable library where the class implementation can be found).
DescribeClass<GoSamAmplitude, MatchboxOLPME> describeHerwigGoSamAmplitude("Herwig::GoSamAmplitude", "HwMatchboxGoSam.so");

void GoSamAmplitude::Init() {

  static ClassDocumentation<GoSamAmplitude> 
    documentation("GoSamAmplitude implements an interface to GoSam.",
		  "Matrix elements have been calculated using GoSam \\cite{Cullen:2011xs}, \\cite{Cullen:2014yla}",
		  "%\\cite{Cullen:2011xs}\n"
		  "\\bibitem{Cullen:2011xs}\n"
		  "G.~Cullen et al.,\n"
		  "``GoSam: A Program for Automated One-Loop Calculations,''\n"
		  "arXiv:1111.6534 [hep-ph].\n"
		  "%%CITATION = ARXIV:1111.6534;%%\n"
		  "%\\cite{Cullen:2014yla}\n"
		  "\\bibitem{Cullen:2014yla}\n"
		  "G.~Cullen et al.,\n"
		  "``GoSaam-2.0: a tool for automated one-loop calculations within the Standard Model and beyond,''\n"
		  "arXiv:1404.7096 [hep-ph].\n"
		  "%%CITATION = ARXIV:1404.7096;%%");

  static Parameter<GoSamAmplitude,string> interfaceProcessPath
       ("ProcessPath",
        "Prefix for the process source code, include files and library produced by GoSam.",
        &GoSamAmplitude::gosamPathInterface, "",
        false, false);

  static Parameter<GoSamAmplitude,string> interfaceSetupInFilename
       ("SetupInFilename",
        "File name of the GoSam infile (typically setup.gosam.in) to be used. If left empty a new setup.gosam.in is created in the location specified in Path",
        &GoSamAmplitude::gosamSetupInFileNameInterface, "",
        false, false);

  static Switch<GoSamAmplitude,bool> interfaceCodeExists
         ("CodeExists",
          "Switch on or off if Code already exists/not exists.",
          &GoSamAmplitude::theCodeExists, true, false, false);
  static SwitchOption interfaceCodeExistsYes
         (interfaceCodeExists,
          "Yes",
          "Switch True if Code already exists.",
          true);
  static SwitchOption interfaceCodeExistsNo
         (interfaceCodeExists,
          "No",
          "Switch False if Code has to be build.",
          false);

  static Switch<GoSamAmplitude,bool> interfaceisitDR
         ("isDR",
          "Switch on or off DR.",
          &GoSamAmplitude::isitDR, false, false, false);
  static SwitchOption interfaceisitDRYes
         (interfaceisitDR,
          "Yes",
          "Switch True.",
          true);
  static SwitchOption interfaceisitDRNo
         (interfaceisitDR,
          "No",
          "Switch False.",
          false);
  
  static Switch<GoSamAmplitude,bool> interfaceFormOpt
         ("FormOpt",
          "Switch On/Off formopt",
          &GoSamAmplitude::theFormOpt, true, false, false);
  static SwitchOption interfaceFormOptYes
         (interfaceFormOpt,
          "Yes",
          "Yes",
          true);
  static SwitchOption interfaceFormOptNo
         (interfaceFormOpt,
          "No",
          "No",
          false);

  static Switch<GoSamAmplitude,bool> interfaceNinja
         ("Ninja",
          "Switch On/Off for reduction with Ninja. If Off then Samurai is used.",
          &GoSamAmplitude::theNinja, true, false, false);
  static SwitchOption interfaceNinjaYes
         (interfaceNinja,
          "Yes",
          "Yes",
          true);
  static SwitchOption interfaceNinjaNo
         (interfaceNinja,
          "No",
          "No",
          false);
 
  static Switch<GoSamAmplitude,bool> interfaceHiggsEff
         ("HiggsEff",
          "Switch On/Off for effective higgs model.",
          &GoSamAmplitude::theHiggsEff, false, false, false);
  static SwitchOption interfaceHiggsEffYes
         (interfaceHiggsEff,
          "Yes",
          "Yes",
          true);
  static SwitchOption interfaceHiggsEffNo
         (interfaceHiggsEff,
          "No",
          "No",
          false);

  static Parameter<GoSamAmplitude,string> interfaceBuildScript
       ("BuildScript",
        "File name of a custom build script, which is called between 'autogen.sh'"
        "and 'make install'. It can be used for parallelization.",
        &GoSamAmplitude::gosamBuildScript, "",
        false, false);

  static Parameter<GoSamAmplitude,int> interfaceAccuracyTarget
       ("AccuracyTarget",
        "Integer to parametrize the threshold value for the BLHA2 acc parameter, returned by GoSam in the case of "
        "sub-processes with one-loop intereference terms or loop induced sub-processes."
        "If acc > 10^-AccuracyTarget the corresponding PSP is being discarded. Discarded PSPs are written to file "
        "if Debug::level > 1.",
        &GoSamAmplitude::theAccuracyTarget, 6, 0, 0,
        false, false, Interface::lowerlim);

  static Switch<GoSamAmplitude,bool> interfaceMassiveLeptons
         ("MassiveLeptons",
          "If set to Yes, then pass on the light lepton masses - as well as the tau mass - to GoSam."
          "Otherwise GoSam will use light leptons of zero mass as default, as well as its own default tau mass.",
          &GoSamAmplitude::theMassiveLeptons, false, false, false);
  static SwitchOption interfaceMassiveLeptonsNo
         (interfaceMassiveLeptons,
          "No",
          "No",
          false);
  static SwitchOption interfaceMassiveLeptonsYes
         (interfaceMassiveLeptons,
          "Yes",
          "Yes",
          true);

  static Switch<GoSamAmplitude,int> interfaceLoopInducedOption
         ("LoopInducedOption",
          "Options for the GoSam interface, in the case that a loop induced process is being considered. The default "
          "option is 0, for which only the squared one-loop amplitude in the Standard Model is being considered. All "
          "other options consider additional contributions from a model with an effective interaction, which lead to "
          "the same final state,  such as the squared effective amplitude, or the interference term between the one- "
          "loop amplitude in the Standard Model and the effective amplitude, or any additive combinations therefrom. "
          "In order to use those options an appropriate model has to be used.",
          &GoSamAmplitude::theLoopInducedOption, 0, false, false);
  static SwitchOption interfaceLoopInducedOptionLI2
         (interfaceLoopInducedOption,
          "LI2",
          "Only consider the squared one-loop amplitude in the Standard Model.",
          0);
  static SwitchOption interfaceLoopInducedOptionEff2
         (interfaceLoopInducedOption,
          "Eff2",
          "Only consider the squared effective amplitude.",
          1);
  static SwitchOption interfaceLoopInducedOptionLIEffInterference
         (interfaceLoopInducedOption,
          "LIEffInterference",
          "Only consider the interference term between the one-loop amplitude "
          "in the Standard Model and the effective amplitude.",
          2);
  static SwitchOption interfaceLoopInducedOptionLI2plusEff2
         (interfaceLoopInducedOption,
          "LI2plusEff2",
          "Consider the sum of the squared one-loop amplitude in the Standard "
          "Model plus the squared effective amplitude.",
          3);
  static SwitchOption interfaceLoopInducedOptionLI2plusLIEffInterference
         (interfaceLoopInducedOption,
          "LI2plusEffInterference",
          "Consider the sum of the squared one-loop amplitude in the Standard "
          "Model plus the interference term between the one-loop amplitude in "
          "the Standard Model and the effective amplitude.",
          4);
  static SwitchOption interfaceLoopInducedOptionEff2plusLIEffInterference
         (interfaceLoopInducedOption,
          "Eff2plusEffInterference",
          "Consider the sum of the squared effective amplitude plus the inter- "
          "ference term between the one-loop amplitude in the Standard Model "
          "and the effective amplitude.",
          5);
  static SwitchOption interfaceLoopInducedOptionAllAdditions
         (interfaceLoopInducedOption,
          "AllAdditions",
          "Consider the sum of the squared one-loop amplitude in the Standard "
          "Model plus all other contributions,  which come with the effective "
          "Model.",
          6);
    
  static Parameter<GoSamAmplitude,string> interfaceBinDir
    ("BinDir",
     "The location for the installed executable",
     &GoSamAmplitude::bindir_, string(HERWIG_BINDIR),
     false, false);

  static Parameter<GoSamAmplitude,string> interfacePKGDATADIR
    ("DataDir",
     "The location for the installed Herwig data files",
     &GoSamAmplitude::pkgdatadir_, string(HERWIG_PKGDATADIR),
     false, false);
    
  static Parameter<GoSamAmplitude,string> interfaceGoSamPrefix
    ("GoSamPrefix",
     "The prefix for the location of GoSam",
     &GoSamAmplitude::GoSamPrefix_, string(GOSAM_PREFIX),
     false, false);
}

