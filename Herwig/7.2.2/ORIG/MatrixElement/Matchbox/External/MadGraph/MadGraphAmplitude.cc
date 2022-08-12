// -*- C++ -*-
//
// MadGraphAmplitude.cc is a part of Herwig - A multi-purpose Monte Carlo event generator
// Copyright (C) 2002-2019 The Herwig Collaboration
//
// Herwig is licenced under version 3 of the GPL, see COPYING for details.
// Please respect the MCnet academic guidelines, see GUIDELINES for details.
//
//
// This is the implementation of the non-inlined, non-templated member
// functions of the MadGraphAmplitude class.
//

#include "MadGraphAmplitude.h"
#include "ThePEG/Interface/ClassDocumentation.h"
#include "ThePEG/EventRecord/Particle.h"
#include "ThePEG/Repository/UseRandom.h"
#include "ThePEG/Repository/EventGenerator.h"
#include "ThePEG/Utilities/DescribeClass.h"
#include "Herwig/MatrixElement/Matchbox/MatchboxFactory.h"
#include "ThePEG/Interface/Parameter.h"
#include "ThePEG/Interface/Switch.h"
#include "ThePEG/Persistency/PersistentOStream.h"
#include "ThePEG/Persistency/PersistentIStream.h"
#include "ThePEG/PDT/EnumParticles.h"
#include "Herwig/API/Filesystem.h"
#include "Herwig/API/RunDirectories.h"
#include <cstdlib>
#include <dlfcn.h>
#include <errno.h>
#include <sstream>

using namespace Herwig;
#ifndef HERWIG_BINDIR
#error Makefile.am needs to define HERWIG_BINDIR
#endif
#ifndef HERWIG_PKGDATADIR
#error Makefile.am needs to define HERWIG_PKGDATADIR
#endif
#ifndef MADGRAPH_PREFIX
#error Makefile.am needs to define MADGRAPH_PREFIX
#endif

extern "C" void mginitproc_(char *i,int);
extern "C" void MG_Calculate_wavefunctions_virt(int* proc,double*,double*);
extern "C" void MG_Calculate_wavefunctions_born(int* proc,double*, int*);
extern "C" void MG_Jamp                        (int* proc,int*, double*);
extern "C" void MG_LNJamp                        (int* proc,int*, double*);
extern "C" void MG_Virt                        (int* proc,double*);
extern "C" void MG_NCol                        (int* proc,int*);
extern "C" void MG_vxxxxx                      (double* p,double* n,int* inc,double*  );
extern "C" void MG_Colour                      (int* proc,int* i,int* j ,int* color);

MadGraphAmplitude::MadGraphAmplitude()
  : keepinputtopmass(false){ }

MadGraphAmplitude::~MadGraphAmplitude() {

}

string MadGraphAmplitude::theProcessPath="";
string MadGraphAmplitude::madgraphPrefix_=MADGRAPH_PREFIX;
string MadGraphAmplitude::theMGmodel="loop_sm";
string MadGraphAmplitude::bindir_=HERWIG_BINDIR;
string MadGraphAmplitude::includedir_=HERWIG_INCLUDEDIR;
string MadGraphAmplitude::pkgdatadir_=HERWIG_PKGDATADIR;

IBPtr MadGraphAmplitude::clone() const {
  return new_ptr(*this);
}

IBPtr MadGraphAmplitude::fullclone() const {
  return new_ptr(*this);
}


bool MadGraphAmplitude::initializedMad=false;
vector<string> MadGraphAmplitude::BornAmplitudes=vector<string>();
vector<string> MadGraphAmplitude::VirtAmplitudes=vector<string>();

void MadGraphAmplitude::initProcess(const cPDVector& ) {
    
  if ( lastMatchboxXComb()->initialized() )
    return;


  if ( !DynamicLoader::load(mgProcLibPath()+"InterfaceMadGraph.so") )
    throw Exception() << "MadGraphAmplitude: Failed to load MadGraph amplitudes\n"
                      << DynamicLoader::lastErrorMessage
                      << Exception::runerror;

  if (!initializedMad){
    string mstr=(factory()->runStorage()+"MadGraphAmplitudes"+"/param_card"+((theMGmodel=="loop_sm")?"":("_"+theMGmodel))+".dat");
    if( theMGmodel[0]=='/')mstr="param_card.dat";
    size_t len = mstr.size();
    mginitproc_(const_cast<char*>(mstr.c_str()),len);
    initializedMad=true;
  }
  lastMatchboxXComb()->isInitialized();
}



bool MadGraphAmplitude::writeAmplitudesDat(){
  bool res=false;

  string born= mgProcLibPath()+"BornAmplitudes.dat";
  if ( !filesystem::exists(born) ) {
    ofstream borns(born.c_str());
    for (vector<string>::iterator amps=BornAmplitudes.begin();amps!=BornAmplitudes.end();amps++)
      borns<<*amps<<endl;
    borns.close();
    res=true;
  }
  string virt= mgProcLibPath()+"VirtAmplitudes.dat";
  if ( !filesystem::exists(virt) ) {
    ofstream virts(virt.c_str());
    for (vector<string>::iterator amps=VirtAmplitudes.begin();amps!=VirtAmplitudes.end();amps++)
      virts<<*amps<<endl;
    virts.flush();
    virts.close();
    res=true;
  }
  return res;
}



bool MadGraphAmplitude::checkAmplitudes(){
  
  string born= mgProcLibPath()+"BornAmplitudes.dat";
  string virt= mgProcLibPath()+"VirtAmplitudes.dat";
  assert ( filesystem::exists(born)|| filesystem::exists(virt));
  
  bool foundallborns=true;
  for (vector<string>::iterator amps=BornAmplitudes.begin();amps!=BornAmplitudes.end();amps++){
    ifstream borns(born.c_str());
    string line;
    bool foundthisborn=false;
    while (std::getline(borns, line)) {
      if(line==*amps)foundthisborn=true;
    }
    foundallborns&=foundthisborn;
  }
  
  bool foundallvirts=true;
  for (vector<string>::iterator amps=VirtAmplitudes.begin();amps!=VirtAmplitudes.end();amps++){
    ifstream virts(virt.c_str());
    string line;
    bool foundthisvirt=false;
    while (std::getline(virts, line)) {
      if(line==*amps)foundthisvirt=true;
    }
    foundallvirts&=foundthisvirt;
  }
  
  if (!foundallborns||!foundallvirts)

  throw Exception() << "MadGraphAmplitude: The MadGraph amplitudes did not match the process.\n" 
                    << "                   Please remove:"<<mgProcLibPath()<< "\n" 
                    << "                   or set a process path via the interface:\n"
                    << "                   set /Herwig/MatrixElements/Matchbox/Amplitudes/MadGraph:ProcessPath ..."
    << Exception::runerror;
  
  return foundallborns && foundallvirts;
  
}



string MadGraphAmplitude::mgProcLibPath(){
  string res=theProcessPath == "" ? factory()->buildStorage()+"MadGraphAmplitudes" : theProcessPath;
  if (res.at(res.length()-1) != '/') res.append("/");
  return res;
}




bool MadGraphAmplitude::initializeExternal() {


  
  if ( filesystem::exists(mgProcLibPath()) ) {
    if ( !filesystem::is_directory(mgProcLibPath()) )
      throw Exception() << "MadGraphAmplitude: MadGraph amplitude storage '"
			<< mgProcLibPath() << "' existing but not a directory."
			<< Exception::runerror;
  } else {
    filesystem::create_directory(mgProcLibPath());
  }



  string runAmplitudes = factory()->runStorage() + "/MadGraphAmplitudes";
  if ( filesystem::exists(runAmplitudes) ) {
    if ( !filesystem::is_directory(runAmplitudes) )
      throw Exception() << "MadGraphAmplitude: MadGraph amplitude storage '"
			<< runAmplitudes << "' existing but not a directory."
			<< Exception::runerror;
  } else {
    filesystem::create_directory(runAmplitudes);
  }
 
    
  //EW-consistency check:
  Energy MW=getParticleData(ParticleID::Wplus)->hardProcessMass();
  Energy MZ=getParticleData(ParticleID::Z0)->hardProcessMass();
  if( MW!= sqrt(MZ*MZ/2.0+sqrt(MZ*MZ*MZ*MZ/4.0-Constants::pi*SM().alphaEMMZ()*MZ*MZ/ sqrt(2.0)/SM().fermiConstant()))){  
    generator()->log()<<"\n\n-----!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!-----";
    generator()->log() << "\nYou are using a EW scheme which is inconsistent with the MadGraph parametisation:\n\n"     
                      <<MW/GeV<< " GeV==MW!= sqrt(MZ^2/2+sqrt(MZ^4/4.0-pi*alphaEMMZ*MZ^2/ sqrt(2)/G_f))=="<<
                      sqrt(MZ*MZ/2.0+sqrt(MZ*MZ*MZ*MZ/4.0-Constants::pi*SM().alphaEMMZ()*MZ*MZ/ sqrt(2.0)/SM().fermiConstant()))/GeV
                      <<" GeV\n\n-----!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!-----\n";
  }
  
  
  
      
  string para= factory()->runStorage()+"/MadGraphAmplitudes"+"/MG-Parameter.dat";
    
  
  ofstream params(para.c_str());
  params<<"$WZ$ "    <<std::setiosflags(ios::scientific)  <<getParticleData(ParticleID::Z0)->hardProcessWidth()     /GeV;
  params<<"\n$WW$ "    <<std::setiosflags(ios::scientific)   <<getParticleData(ParticleID::Wplus)->hardProcessWidth()/GeV;
  params<<"\n$alphas$ " <<std::setiosflags(ios::scientific)  <<SM().alphaS();
  params<<"\n$GF$ "     <<std::setiosflags(ios::scientific)  <<SM().fermiConstant()*GeV2   ;
  params<<"\n$alphaMZ$ " <<std::setiosflags(ios::scientific) <<1/SM().alphaEMMZ();
  params<<"\n$MZ$ "     <<std::setiosflags(ios::scientific)  <<getParticleData(ParticleID::Z0)->hardProcessMass() /GeV<<flush;
  params<<"\n$MW$ "    <<std::setiosflags(ios::scientific)   <<getParticleData(ParticleID::Wplus)->hardProcessMass() /GeV<<flush;
  params<<"\n$sw2$ "    <<std::setiosflags(ios::scientific)   << SM().sin2ThetaW() <<flush;
  if(theMGmodel=="heft"&&!keepinputtopmass){
    if ( factory()->initVerbose() ) {
      generator()->log()<<"\n---------------------------------------------------------------";
      generator()->log()<<"\n---------------------------------------------------------------";
      generator()->log()<<"\nNote:    You are using the Higgs Effective model (heft) in     ";
      generator()->log()<<"\n         Madgraph. We assume you try to calculate NLO with ";
      generator()->log()<<"\n         the GoSam virtual amplitudes. To match the models we ";
      generator()->log()<<"\n         therefore set the topmass to 10000000 GeV.";
      generator()->log()<<"\n\n         For more information see the \\tau parameter in:";     
      generator()->log()<<"\n         https://cp3.irmp.ucl.ac.be/projects/madgraph/wiki/Models/HiggsEffective";
      generator()->log()<<"\n\n         The Effective Higgs model in Gosam is using mT=infinity";
      generator()->log()<<"\n\n\n         If you want to use the LO matrixelements of MadGraph with finite' topmass you need to add:  ";
      generator()->log()<<"\n\n             set Madgraph:KeepInputTopMass Yes";
      generator()->log()<<"\n\n         to your input file.";
      generator()->log()<<"\n---------------------------------------------------------------";
      generator()->log()<<"\n---------------------------------------------------------------\n";
    }
    params<<"\n$MT$ 10000000." <<flush;
  }else{
      params<<"\n$MT$ "    <<std::setiosflags(ios::scientific)   << getParticleData(ParticleID::t)->hardProcessMass() /GeV <<flush;
  }
  params<<"\n$WT$ "    <<std::setiosflags(ios::scientific)   << getParticleData(ParticleID::t)->hardProcessWidth() /GeV <<flush;
  params<<"\n$MB$ "    <<std::setiosflags(ios::scientific)   << getParticleData(ParticleID::b)->hardProcessMass() /GeV <<flush;
  params<<"\n$MH$ "    <<std::setiosflags(ios::scientific)   << getParticleData(ParticleID::h0)->hardProcessMass() /GeV <<flush;
  params<<"\n$WH$ "    <<std::setiosflags(ios::scientific)   << getParticleData(ParticleID::h0)->hardProcessWidth() /GeV <<flush;
  params<<"\n$MTA$ "    <<std::setiosflags(ios::scientific)   << getParticleData(ParticleID::tauplus)->hardProcessMass() /GeV <<flush;

  
  string cmd = "python " + bindir_ + "/mg2herwig ";
  cmd +=" --buildpath "+mgProcLibPath();
  cmd += !theProcessPath.empty() ? " --absolute-links" : "";
  cmd +=" --model "+theMGmodel;
  cmd +=" --runpath "+factory()->runStorage()+"/MadGraphAmplitudes ";
  cmd +=" --datadir "+pkgdatadir_;
  cmd +=" --includedir "+includedir_;
  cmd +=" --cacheprefix "+RunDirectories::prefix();
  std::stringstream as,aem;
  as << factory()->orderInAlphaS();
  cmd +=" --orderas "+as.str() ;
  aem <<factory()->orderInAlphaEW();
  cmd +=" --orderew "+aem.str();
  
  // TODO move to boost::system
  writeAmplitudesDat();

  if (filesystem::exists(mgProcLibPath()+"InterfaceMadGraph.so") ){
    //set the parameters
    
    checkAmplitudes();
    
    std::system(cmd.c_str());
    ranMadGraphInitializeExternal = true;
    return true;
  }
  char cwd[1024];
  if ( !getcwd(cwd,sizeof(cwd)) )
    throw Exception() << "MadGraphAmplitude: failed to determine current working directory\n"
                      << Exception::runerror;
     
  cmd +=" --madgraph " + madgraphPrefix_ + "/bin " ;
  cmd +="--build > ";
  cmd +=  mgProcLibPath()+"MG.log 2>&1";
  
  
  generator()->log() << "\n>>> Compiling MadGraph amplitudes. This may take some time -- please be patient.\n"
                     << ">>> In case of problems see " << mgProcLibPath() << "MG.log for details.\n\n"
                     << flush;
  std::system(cmd.c_str());
  
  
  cmd = "python " + bindir_ + "/mg2herwig ";
  cmd +=" --buildpath "+mgProcLibPath();
  cmd +=" --model "+theMGmodel;
  cmd +=" --runpath "+factory()->runStorage()+"/MadGraphAmplitudes ";
  cmd +=" --datadir "+pkgdatadir_;  
  cmd +=" --cacheprefix "+RunDirectories::prefix();
  as.clear();
  aem.clear();
  as << factory()->orderInAlphaS();
  cmd +=" --orderas "+as.str() ;
  aem <<factory()->orderInAlphaEW();
  cmd +=" --orderew "+aem.str();
  
  std::system(cmd.c_str());

  ranMadGraphInitializeExternal = true;

  return filesystem::exists(mgProcLibPath()+"InterfaceMadGraph.so");

}

int MadGraphAmplitude::externalId(const cPDVector& proc) {
    for (int i=0;i<100;i++){
    colourindex.push_back(-2);
  }
  assert(!BornAmplitudes.empty()||!VirtAmplitudes.empty());
  
  writeAmplitudesDat();
  
  int res=0;
  string amp="";
  int k=0;
  for (cPDVector::const_iterator it=proc.begin();it!=proc.end();it++,k++){
    amp+=std::to_string( (*it)->id())+" ";if (k==1)amp+=" > ";
  }
  
  
  string born= mgProcLibPath()+"BornAmplitudes.dat";
  string virt= mgProcLibPath()+"VirtAmplitudes.dat";
  assert ( filesystem::exists(born)|| filesystem::exists(virt));
  
  
  ifstream borns(born.c_str());
  string line;
  while (std::getline(borns, line)) {
    res+=1;
    if(line==amp)return res;
  }
  ifstream virts(virt.c_str());
  while (std::getline(virts, line)) {
    res+=1;
    if(line==amp)return res;
  }
  
  throw Exception() << "MadGraphAmplitude: One amplitude has no externalId. Please remove the MadGraphAmplitude-folder and rebuild.\n"     << Exception::runerror;
  return res;
}

bool MadGraphAmplitude::ranMadGraphInitializeExternal = false;

void MadGraphAmplitude::doinit() {
  
  if ( !ranMadGraphInitializeExternal ) {
    initializeExternal();
  }
  MatchboxAmplitude::doinit();
}

void MadGraphAmplitude::doinitrun() {
  if ( !ranMadGraphInitializeExternal ) {
    initializeExternal();
  }
  MatchboxAmplitude::doinitrun();
}

bool MadGraphAmplitude::canHandle(const PDVector& p,
                                  Ptr<MatchboxFactory>::tptr factory,
                                  bool virt) const {
  if ( factory->processData()->diagramMap().find(p) !=
       factory->processData()->diagramMap().end() )
    return true;
  vector<Ptr<Tree2toNDiagram>::ptr> diags =
    factory->diagramGenerator()->generate(p,orderInGs(),orderInGem());
  if ( diags.empty() )
    return false;
  factory->processData()->diagramMap()[p] = diags;
  string amp="";
  int k=0;
  for (PDVector::const_iterator it=p.begin();it!=p.end();it++,k++){
    amp+=std::to_string( (*it)->id())+" ";if (k==1)amp+=" > ";
  }
   if (virt && factory->highestVirt()>=p.size()){
    VirtAmplitudes.push_back(amp);
  }else{
    BornAmplitudes.push_back(amp);
  }
  
  return true;
}

void MadGraphAmplitude::prepareAmplitudes(Ptr<MatchboxMEBase>::tcptr me) {

  useMe();

  if ( !calculateTreeAmplitudes() ) {
    MatchboxAmplitude::prepareAmplitudes(me);
    return;
  }
  
  if (colourindex.empty()) {
    for (int i=0;i<100;i++){
      colourindex.push_back(-2);
    }
  }
  
  lastMatchboxXComb()->clearheljamp();
  lastMatchboxXComb()->clearhelLNjamp();
  initProcess(mePartonData());
  MatchboxAmplitude::prepareAmplitudes(me);

}

Complex MadGraphAmplitude::evaluate(size_t i, const vector<int>& hel, Complex& largeN) {
  
  
  //find the colourline:
  int ii = -1;
  int xx=lastMatchboxXComb()->externalId();
  
  if (colourindex.size()<=i) {
    colourindex.clear();
    for (size_t l=0;l<=i+10;l++){
      colourindex.push_back(-2);
    }
  }
  
  if(colourindex[i]!=-2){

    ii = colourindex[i];
    if (ii==-1) {
      largeN = Complex(0.0);
      return Complex(0.0);
    }
  } else {
    set<vector<size_t> > a ;
    if(lastMatchboxXComb()->colourBasisDim()>0)a=colourOrdering(i);
    int ncol=-1;
    MG_NCol(&xx,&ncol);
    assert(ncol!=-1);
    for( int it = 0; it < ncol; it++ ){
      int n = 0;
      for ( cPDVector::const_iterator nx = mePartonData().begin();
            nx != mePartonData().end(); nx++ )
        if ( (*nx)->coloured() ) n++;
      set<vector<size_t> > tmpset;
      vector<size_t> tmpvek;
      for ( int it2 = 0; it2 < n; it2++ ) {
        int ret=-2;
        MG_Colour(&xx,&it,&it2,&ret);
        assert(ret !=-2);
        if (ret== -1)
          break;
        if ( ret == 0 ) {
          n++;
          tmpset.insert(tmpvek);
          tmpvek.clear();
        } else {
          tmpvek.push_back(ret-1);
        }
        if( it2 == n-1 ) tmpset.insert(tmpvek);
      }
      bool found_all = true;
      for ( set<vector<size_t> >::iterator it3 = a.begin(); it3 != a.end(); it3++ ) {
        bool found_it3=false;
        for ( set<vector<size_t> >::iterator it4 = tmpset.begin(); it4 != tmpset.end(); it4++ ) {
          vector<size_t> it3tmp = gluonsFirst(*it3);
          vector<size_t> it4tmp = (*it4);
          if ( it3tmp.size() != it4tmp.size() ) continue;
          if ( it3tmp == it4tmp ) found_it3 = true;
        }
        found_all = found_all && found_it3;
      }

      if ( found_all ) {
        colourindex[i]=it;
      

        ii=it;
      }
    }
  }
  if ( ii == -1 ){

    colourindex[i]=ii;
    largeN = Complex(0.0);
    return Complex(0.0);
  }
  
  const map<vector<int>,vector < complex<double> > >& tmp = lastMatchboxXComb()->heljamp();
    const map<vector<int>,vector < complex<double> > >& tmpLN = lastMatchboxXComb()->helLNjamp();

  if( tmp.find(hel) != tmp.end()) {
    largeN = tmpLN.find(hel)->second[ii];
    return tmp.find(hel)->second[ii];;
  }

  double units = pow(sqrt(lastSHat())/GeV,int(hel.size())-4);

  int heltmp[10];
  
  for(size_t j=0;j<hel.size();j++){
    int cross=crossingMap()[j];
    if( (cross>1&&j<=1)||(cross<=1&&j>1)){
    heltmp[cross]=-1*hel[j];}
    else{heltmp[cross]=hel[j];}
  }

  vector<Lorentz5Momentum> reshuffled = meMomenta();
  if ( !reshuffleMasses().empty() && reshuffled.size() > 3 ) {
    const cPDVector& pdata = mePartonData();
    const map<long,Energy>& masses = reshuffleMasses();
    lastMatchboxXComb()->reshuffle(reshuffled,pdata,masses);
  }

  double momenta[50];
  
  size_t j=0;
  for (size_t i=0;i<mePartonData().size();i++){
    momenta[j]=abs(reshuffled[i].e()/GeV)<1.e-13?0.:double(reshuffled[i].e()/GeV);
    momenta[j+1]=abs(reshuffled[i].x()/GeV)<1.e-13?0.:double(reshuffled[i].x()/GeV);
    momenta[j+2]=abs(reshuffled[i].y()/GeV)<1.e-13?0.:double(reshuffled[i].y()/GeV);
    momenta[j+3]=abs(reshuffled[i].z()/GeV)<1.e-13?0.:double(reshuffled[i].z()/GeV);
    if(momenta[j  ] == 0. && momenta[j+1] == 0. &&
       momenta[j+2] == 0. && momenta[j+3] == 0. )
      return 0.;
    j+=4;
  }
  
  MG_Calculate_wavefunctions_born(&xx, &momenta[0],  &heltmp[0]);
  
  
  int ncol=-1;
  MG_NCol(&xx,&ncol);

  Complex res;
  Complex resLN;
  for( int it = 0; it < ncol; it++ ){
    double dd[2];
    MG_Jamp(&xx,&it,&dd[0]);
    Complex d(dd[0],dd[1]);
    if(it==ii)res=d*units;
    lastMatchboxXComb()->pushheljamp(hel,d*units);
    double ddLN[2];
    MG_LNJamp(&xx,&it,&ddLN[0]);
    Complex dLN(ddLN[0],ddLN[1]);
    if(it==ii)resLN=dLN*units;
    lastMatchboxXComb()->pushhelLNjamp(hel,dLN*units);
  }

  
  largeN = resLN;
  return res;

}

vector<unsigned int> MadGraphAmplitude::physicalHelicities(const vector<int>& hel) const {
  vector<unsigned int> res(hel.size(),0);
  for ( size_t j = 0; j < hel.size(); ++j ) { 
    int cross = crossingMap()[j];
    int xhel = 0;
    if ( (cross > 1 && j <= 1) || (cross <= 1 && j > 1) )
      xhel = -1*hel[j];
    else
      xhel = hel[j];
    if ( mePartonData()[cross]->iSpin() == PDT::Spin1Half )
      res[cross] = (xhel == -1 ? 0 : 1);
    else if ( mePartonData()[cross]->iSpin() == PDT::Spin1 )
      res[cross] = (unsigned int)(xhel + 1);
    else if ( mePartonData()[cross]->iSpin() == PDT::Spin0 )
      res[cross] = 0;
    else assert(false);
  }
  return res;
}

LorentzVector<Complex> MadGraphAmplitude::plusPolarization(const Lorentz5Momentum& p,
                                                           const Lorentz5Momentum& n,
                                                           int i) const {
                                                             
                                                             
                                                     

  int tmp=i;
  double pg[4],ng[4],poltmp[8];
  

  pg[0]=p.e()/GeV;pg[1]=p.x()/GeV;pg[2]=p.y()/GeV;pg[3]=p.z()/GeV;
  ng[0]=n.e()/GeV;ng[1]=n.x()/GeV;ng[2]=n.y()/GeV;ng[3]=n.z()/GeV;

  MG_vxxxxx(&pg[0],&ng[0],&tmp,&poltmp[0]);

  complex<double> pol[6];
  pol[0]=Complex(poltmp[0],poltmp[1]);
  pol[1]=Complex(poltmp[2],poltmp[3]);
  pol[2]=Complex(poltmp[4],poltmp[5]);
  pol[3]=Complex(poltmp[6],poltmp[7]);

  LorentzVector<Complex> polarization(pol[1],pol[2],pol[3],pol[0]);

  return polarization;
}
 
bool equalsModulo(unsigned int i, const vector<int>& a, const vector<int>& b) {
  assert(a.size()==b.size());
  if ( a[i] == b[i] )
    return false;
  for ( unsigned int k = 0; k < a.size(); ++k ) {
    if ( k == i )
      continue;
    if ( a[k] != b[k] )
      return false;
  }
  return true;
}

vector<size_t> MadGraphAmplitude::gluonsFirst(vector<size_t> vec) {
  vector<size_t> vecout;
  for(vector<size_t>::iterator it= vec.begin();it!= vec.end();++it)
    if ( mePartonData()[crossingMap()[*it]]->id()==21)
      vecout.push_back(crossingMap()[*it]);

  for(vector<size_t>::iterator it= vec.begin();it!= vec.end();++it)
    if ( mePartonData()[crossingMap()[*it]]->id()!=21)
      vecout.push_back(crossingMap()[*it]);

  return vecout;
  
}

double MadGraphAmplitude::spinColourCorrelatedME2(pair<int,int> ij,
                                                  const SpinCorrelationTensor& c) const {

  
  vector<Lorentz5Momentum> reshuffled = meMomenta();
  if ( !reshuffleMasses().empty() && reshuffled.size() > 3 ) {
    const cPDVector& pdata = mePartonData();
    const map<long,Energy>& masses = reshuffleMasses();
    lastMatchboxXComb()->reshuffle(reshuffled,pdata,masses);
  }
  
  Lorentz5Momentum p = reshuffled[ij.first];
  Lorentz5Momentum n = reshuffled[ij.second];

  LorentzVector<Complex> polarization = plusPolarization(p,n,ij.first<2?-1:1);
  
  
  int iCrossed = -1;
  for ( unsigned int k = 0; k < crossingMap().size(); ++k )
    if ( crossingMap()[k] == ij.first ) {
      iCrossed = k;
      break;
    }
  assert(iCrossed!=-1);
  
  if(ij.first>1) polarization =polarization.conjugate();
  if(iCrossed<2) polarization =polarization.conjugate();
  Complex pFactor = (polarization*c.momentum())/sqrt(abs(c.scale()));


  double avg =
    colourCorrelatedME2(ij)*(-c.diagonal()+ (c.scale() > ZERO ? 1. : -1.)*norm(pFactor));
   
  
  Complex csCorr = 0.0;

  if ( calculateColourSpinCorrelator(ij) ) {
    set<const CVector*> done;
    for ( AmplitudeConstIterator a = lastAmplitudes().begin();
          a != lastAmplitudes().end(); ++a ) {
      if ( done.find(&(a->second)) != done.end() )
        continue;
      AmplitudeConstIterator b = lastAmplitudes().begin();
      while ( !equalsModulo(iCrossed,a->first,b->first) )
        if ( ++b == lastAmplitudes().end() )
          break;
      if ( b == lastAmplitudes().end() || done.find(&(b->second)) != done.end() )
        continue;
      done.insert(&(a->second)); done.insert(&(b->second));
      if ( a->first[iCrossed] == 1 )
        swap(a,b);
      csCorr -= colourBasis()->colourCorrelatedInterference(ij,mePartonData(),a->second,b->second);
    }
    lastColourSpinCorrelator(ij,csCorr);
  } else {
    csCorr = lastColourSpinCorrelator(ij);
  }
  double corr =
    2.*real(csCorr*sqr(pFactor));

  double Nc = generator()->standardModel()->Nc();
  double cfac = 1.;
  if ( mePartonData()[ij.first]->iColour() == PDT::Colour8 ) {
    cfac = Nc;
  } else if ( mePartonData()[ij.first]->iColour() == PDT::Colour3 ||
              mePartonData()[ij.first]->iColour() == PDT::Colour3bar ) {
    cfac = (sqr(Nc)-1.)/(2.*Nc);
  } else assert(false);

  return
    ( avg +(c.scale() > ZERO ? 1. : -1.)*corr/cfac);

}




void MadGraphAmplitude::prepareOneLoopAmplitudes(Ptr<MatchboxMEBase>::tcptr ){
  assert(false);
}


double MadGraphAmplitude::oneLoopInterference() const {
  if ( !calculateOneLoopInterference() )
    return lastOneLoopInterference();
  evaloneLoopInterference();
  return lastOneLoopInterference();
}


void MadGraphAmplitude::evaloneLoopInterference() const  {
  
  
  double units = pow(lastSHat()/GeV2,int(mePartonData().size())-4);
  
  vector<Lorentz5Momentum> reshuffled = meMomenta();
  if ( !reshuffleMasses().empty() && reshuffled.size() > 3 ) {
    const cPDVector& pdata = mePartonData();
    const map<long,Energy>& masses = reshuffleMasses();
    lastMatchboxXComb()->reshuffle(reshuffled,pdata,masses);
  }
  
  double virt[20];
  double momenta[50];
  
  size_t j=0;
  for (size_t i=0;i<mePartonData().size();i++){
    momenta[j]=abs(reshuffled[i].e()/GeV)<1.e-13?0.:double(reshuffled[i].e()/GeV);
    momenta[j+1]=abs(reshuffled[i].x()/GeV)<1.e-13?0.:double(reshuffled[i].x()/GeV);
    momenta[j+2]=abs(reshuffled[i].y()/GeV)<1.e-13?0.:double(reshuffled[i].y()/GeV);
    momenta[j+3]=abs(reshuffled[i].z()/GeV)<1.e-13?0.:double(reshuffled[i].z()/GeV);
    j+=4;
  }

  int xx=lastMatchboxXComb()->externalId();
  
  MG_Calculate_wavefunctions_virt(&xx,&momenta[0],&virt[0]);

    double ifact = 1.;

    ifact = 1./4.;
    if (lastMatchboxXComb()->matchboxME()->mePartonData()[0]->iColour() == PDT::Colour3 ||
        lastMatchboxXComb()->matchboxME()->mePartonData()[0]->iColour() == PDT::Colour3bar )
      ifact /= SM().Nc();
    else if ( lastMatchboxXComb()->matchboxME()->mePartonData()[0]->iColour() == PDT::Colour8 )
      ifact /= (SM().Nc()*SM().Nc()-1.);

    if ( lastMatchboxXComb()->matchboxME()->mePartonData()[1]->iColour() == PDT::Colour3 ||
         lastMatchboxXComb()->matchboxME()->mePartonData()[1]->iColour() == PDT::Colour3bar )
      ifact /= SM().Nc();
    else if ( mePartonData()[1]->iColour() == PDT::Colour8 )
      ifact /= (SM().Nc()*SM().Nc()-1.);
  

    ifact *= lastMatchboxXComb()->matchboxME()->finalStateSymmetry();
 

  lastOneLoopInterference(virt[1]/ifact*units);
  lastOneLoopPoles(pair<double, double>(virt[2]/ifact*units,virt[3]/ifact*units));
}
 
void MadGraphAmplitude::persistentOutput(PersistentOStream & os) const {
  os << theOrderInGs << theOrderInGem
     << colourindex<<crossing;
  
    //Static variables need to be written only once.
  os << theProcessPath << theMGmodel << bindir_
     << pkgdatadir_ << madgraphPrefix_;
  theProcessPath.clear();
  theMGmodel.clear();
  bindir_.clear();
  pkgdatadir_.clear();
  madgraphPrefix_.clear();
  
  os << BornAmplitudes.size();
  os << VirtAmplitudes.size();
  for (auto amp : BornAmplitudes) os<<amp;
  for (auto amp : VirtAmplitudes) os<<amp;
  
  BornAmplitudes.clear();
  VirtAmplitudes.clear();
}
 
void MadGraphAmplitude::persistentInput(PersistentIStream & is, int) {
  is >> theOrderInGs >> theOrderInGem
  >> colourindex>>crossing;
  
  string input=""; is>>input; if (!input.empty())theProcessPath=input;
  input=""; is>>input; if (!input.empty())theMGmodel=input;
  input=""; is>>input; if (!input.empty())bindir_=input;
  input=""; is>>input; if (!input.empty())pkgdatadir_=input;
  input=""; is>>input; if (!input.empty())madgraphPrefix_=input;
  
  int bs,vs;
  is >> bs;
  is >> vs;
  string amp="";
  for (int i=0;i<bs;i++){
    is>>amp;
    BornAmplitudes.push_back(amp);
    amp="";
  }
  for (int i=0;i<vs;i++){
    is>>amp;
    VirtAmplitudes.push_back(amp);
    amp="";
  }
  
}






void MadGraphAmplitude::setProcessPath(string p){
  theProcessPath=p;
}
string MadGraphAmplitude::getProcessPath() const{
  return theProcessPath;
}

void MadGraphAmplitude::setBinDir(string p){
  bindir_=p;
}
string MadGraphAmplitude::getBinDir() const {
  return bindir_;
}

void MadGraphAmplitude::setDataDir(string p){
  pkgdatadir_=p;
}
string MadGraphAmplitude::getDataDir() const {
  return pkgdatadir_;
}

void MadGraphAmplitude::setModel(string p) {
 theMGmodel=p;
}
string MadGraphAmplitude::getModel() const {
  return theMGmodel;
}

void MadGraphAmplitude::setMadgraphPrefix(string p) {
  madgraphPrefix_=p;
}
string MadGraphAmplitude::getMadgraphPrefix() const {
  return madgraphPrefix_;
}


// *** Attention *** The following static variable is needed for the type
// description system in ThePEG. Please check that the template arguments
// are correct (the class and its base class), and that the constructor
// arguments are correct (the class name and the name of the dynamically
// loadable library where the class implementation can be found).
DescribeClass<MadGraphAmplitude,MatchboxAmplitude> 
describeHerwigMadGraphAmplitude("Herwig::MadGraphAmplitude", "HwMatchboxMadGraph.so");
 
void MadGraphAmplitude::Init() {
   
  static ClassDocumentation<MadGraphAmplitude> 
    documentation("MadGraphAmplitude",
		  "Matrix elements have been calculated using MadGraph5 \\cite{Alwall:2011uj}",
		  "%\\cite{Alwall:2011uj}\n"
		  "\\bibitem{Alwall:2011uj}\n"
		  "J. Alwall et al.,\n"
		  "``MadGraph 5 : Going Beyond,''\n"
		  "arXiv:1106.0522 [hep-ph].\n"
		  "%%CITATION = ARXIV:1106.0522;%%");
   
  static Parameter<MadGraphAmplitude,string> interfaceProcessPath
    ("ProcessPath",
     "The Process Path.",
     0 , "",false, false,
     &MadGraphAmplitude::setProcessPath,
     &MadGraphAmplitude::getProcessPath);
  
  static Parameter<MadGraphAmplitude,string> interfaceModel
    ("Model",
     "The MadGraph-Model.",
     0, "loop_sm",false, false,
     &MadGraphAmplitude::setModel,
     &MadGraphAmplitude::getModel);
  
  static Switch<MadGraphAmplitude,bool> interfacekeepinputtopmass
         ("KeepInputTopMass",
          "Switch On/Off formopt",
          &MadGraphAmplitude::keepinputtopmass, false, false, false);
  static SwitchOption interfacekeepinputtopmassYes
         (interfacekeepinputtopmass,
          "Yes",
          "Yes",
          true);
  static SwitchOption interfacekeepinputtopmassNo
         (interfacekeepinputtopmass,
          "No",
          "No",
          false);  
  
    static Parameter<MadGraphAmplitude,string> interfaceMadgraphPrefix
    ("MadgraphPrefix",
     "The prefix for the location of MadGraph",
     0, string(MADGRAPH_PREFIX),
     false, false,
     &MadGraphAmplitude::setMadgraphPrefix,
     &MadGraphAmplitude::getMadgraphPrefix);
  
    /// JB: I don't know why these should be needed:
  static Parameter<MadGraphAmplitude,string> interfaceBinDir
    ("BinDir",
     "The location for the installed executable",
     0, string(HERWIG_BINDIR),
     false, false,
     &MadGraphAmplitude::setBinDir,
     &MadGraphAmplitude::getBinDir);

  static Parameter<MadGraphAmplitude,string> interfacePKGDATADIR
    ("DataDir",
     "The location for the installed Herwig data files",
     0, string(HERWIG_PKGDATADIR),
     false, false,
     &MadGraphAmplitude::setDataDir,
     &MadGraphAmplitude::getDataDir);
    


}
