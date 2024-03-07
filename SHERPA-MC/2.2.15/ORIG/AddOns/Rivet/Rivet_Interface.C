#include "SHERPA/Tools/Analysis_Interface.H"

#include "ATOOLS/Math/MathTools.H"
#include "ATOOLS/Org/CXXFLAGS.H"
#include "ATOOLS/Org/CXXFLAGS_PACKAGES.H"
#include "ATOOLS/Org/Data_Reader.H"
#include "ATOOLS/Org/Message.H"
#include "ATOOLS/Org/Exception.H"
#include "ATOOLS/Org/Run_Parameter.H"
#include "ATOOLS/Org/Library_Loader.H"
#include "ATOOLS/Org/Shell_Tools.H"
#include "ATOOLS/Org/MyStrStream.H"
#include "ATOOLS/Org/My_MPI.H"
#include "SHERPA/Single_Events/Event_Handler.H"
#ifdef USING__RIVET3
#include "Rivet/Config/RivetConfig.hh"
#include "Rivet/AnalysisHandler.hh"
#include "Rivet/Tools/Logging.hh"
#include "YODA/Config/BuildConfig.h"
#ifdef RIVET_ENABLE_HEPMC_3
#include "SHERPA/Tools/HepMC3_Interface.H"
#include "HepMC3/GenEvent.h"
#include "HepMC3/GenCrossSection.h"
#define SHERPA__HepMC_Interface SHERPA::HepMC3_Interface
#define HEPMCNS HepMC3
#define HEPMC_HAS_CROSS_SECTION
#else
#include "SHERPA/Tools/HepMC2_Interface.H"
#include "HepMC/GenEvent.h"
#include "HepMC/GenCrossSection.h"
#include "HepMC/WeightContainer.h"
#ifdef USING__HEPMC2__DEFS
#include "HepMC/HepMCDefs.h"
#endif
#define SHERPA__HepMC_Interface SHERPA::HepMC2_Interface
#define HEPMCNS HepMC
#endif


namespace SHERPARIVET {
  typedef std::pair<std::string, int> RivetMapKey;
  typedef std::map<RivetMapKey, Rivet::AnalysisHandler*> Rivet_Map;

  class Rivet_Interface: public SHERPA::Analysis_Interface {
  private:

    std::string m_inpath, m_infile, m_outpath, m_tag;
    std::vector<std::string> m_analyses;

    size_t m_nevt;
    bool   m_finished;
    bool   m_splitjetconts, m_splitSH, m_splitpm, m_splitcoreprocs,
      m_ignorebeams, m_usehepmcshort, m_skipweights;
    size_t m_hepmcoutputprecision, m_xsoutputprecision;

    Rivet_Map         m_rivet;
    SHERPA__HepMC_Interface       m_hepmc2;
    std::vector<ATOOLS::btp::code> m_ignoreblobs;
    std::map<std::string,size_t>   m_weightidxmap;

    Rivet::AnalysisHandler* GetRivet(std::string proc,
                                     int jetcont);
    std::string             GetCoreProc(const std::string& proc);

  public:
    Rivet_Interface(const std::string &inpath,
                    const std::string &infile,
                    const std::string &outpath,
                    const std::vector<ATOOLS::btp::code> &ignoreblobs,
                    const std::string &tag);
    ~Rivet_Interface();

    bool Init();
    bool Run(ATOOLS::Blob_List *const bl);
    bool Finish();

    void ShowSyntax(const int i);
  };

  class RivetShower_Interface: public Rivet_Interface {};
  class RivetME_Interface: public Rivet_Interface {};
}


using namespace SHERPARIVET;
using namespace SHERPA;
using namespace ATOOLS;
using namespace Rivet;


Rivet_Interface::Rivet_Interface(const std::string &inpath,
                                 const std::string &infile,
                                 const std::string &outpath,
                                 const std::vector<btp::code> &ignoreblobs,
                                 const std::string& tag) :
  Analysis_Interface("Rivet"),
  m_inpath(inpath), m_infile(infile), m_outpath(outpath), m_tag(tag),
  m_nevt(0), m_finished(false),
  m_splitjetconts(false), m_splitSH(false), m_splitpm(false),
  m_splitcoreprocs(false),
  m_ignoreblobs(ignoreblobs),
  m_hepmcoutputprecision(15), m_xsoutputprecision(6)
{
  if (m_outpath[m_outpath.size()-1]=='/')
    m_outpath=m_outpath.substr(0,m_outpath.size()-1);
#ifdef USING__MPI
  if (mpi->Rank()==0) {
#endif
    if (m_outpath.rfind('/')!=std::string::npos)
      MakeDir(m_outpath.substr(0,m_outpath.rfind('/')));
#ifdef USING__MPI
  }
  if (mpi->Size()>1) {
    m_outpath.insert(m_outpath.length(),"_"+rpa->gen.Variable("RNG_SEED"));
  }
#endif
}

Rivet_Interface::~Rivet_Interface()
{
  if (!m_finished) Finish();
  for (Rivet_Map::iterator it(m_rivet.begin());
       it!=m_rivet.end();++it) {
    delete it->second;
  }
  m_rivet.clear();
}

AnalysisHandler* Rivet_Interface::GetRivet(std::string proc,
                                           int jetcont)
{
  DEBUG_FUNC(proc<<" "<<jetcont);
  RivetMapKey key = std::make_pair(proc, jetcont);
  Rivet_Map::iterator it=m_rivet.find(key);
  if (it!=m_rivet.end()) {
    msg_Debugging()<<"found "<<key.first<<" "<<key.second<<std::endl;
    return it->second;
  }
  else {
    msg_Debugging()<<"create new "<<key.first<<" "<<key.second<<std::endl;
    AnalysisHandler* rivet(new AnalysisHandler());
    rivet->setIgnoreBeams(m_ignorebeams);
    rivet->skipMultiWeights(m_skipweights);
    rivet->addAnalyses(m_analyses);
    m_rivet.insert(std::make_pair(key, rivet));
    return rivet;
  }
}

std::string Rivet_Interface::GetCoreProc(const std::string& proc)
{
  DEBUG_FUNC(proc);
  size_t idx=5;
  std::vector<ATOOLS::Flavour> flavs;
  while (idx<proc.size()) {
    std::string fl(1, proc[idx]);
    if (fl=="_") {
      ++idx;
      continue;
    }
    for (++idx; idx<proc.size(); ++idx) {
      if (proc[idx]=='_') break;
      fl+=proc[idx];
    }
    bool bar(false);
    if (fl.length()>1) {
      if (fl[fl.length()-1]=='b') {
        fl.erase(fl.length()-1,1);
        bar=true;
      }
      else if ((fl[0]=='W' || fl[0]=='H')) {
        if (fl[fl.length()-1]=='-') {
          fl[fl.length()-1]='+';
          bar=true;
        }
      }
      else if (fl[fl.length()-1]=='+') {
        fl[fl.length()-1]='-';
        bar=true;
      }
    }
    Flavour flav(s_kftable.KFFromIDName(fl));
    if (bar) flav=flav.Bar();
    flavs.push_back(flav);
  }

  std::vector<Flavour> nojetflavs;
  for (size_t i=2; i<flavs.size(); ++i) {
    if (!Flavour(kf_jet).Includes(flavs[i])) nojetflavs.push_back(flavs[i]);
  }

  std::vector<Flavour> noresflavs;
  for (size_t i=0; i<nojetflavs.size(); ++i) {
    if (!Flavour(kf_resummed).Includes(nojetflavs[i])) noresflavs.push_back(nojetflavs[i]);
  }

  std::vector<Flavour> finalflavs;
  // start with initial state
  for (size_t i=0; i<2; ++i) {
    if (Flavour(kf_jet).Includes(flavs[i]))
      finalflavs.push_back(Flavour(kf_jet));
    else if (Flavour(kf_resummed).Includes(flavs[i]))
      finalflavs.push_back(Flavour(kf_resummed));
    else
      finalflavs.push_back(flavs[i]);
  }
  // add all non-jet and non-resummed particles
  for (size_t i=0; i<noresflavs.size(); ++i) {
    finalflavs.push_back(noresflavs[i]);
  }
  // add all resummed particles
  for (size_t i=0; i<nojetflavs.size()-noresflavs.size(); ++i) {
    if (finalflavs.size()>3) break;
    finalflavs.push_back(Flavour(kf_resummed));
  }
  // add all jet particles
  for (size_t i=0; i<flavs.size()-2-nojetflavs.size(); ++i) {
    if (finalflavs.size()>3) break;
    finalflavs.push_back(Flavour(kf_jet));
  }

  std::string ret;
  for (size_t i=0; i<finalflavs.size(); ++i) {
    ret+=finalflavs[i].IDName();
    ret+="__";
  }
  while (ret[ret.length()-1]=='_') {
    ret.erase(ret.length()-1, 1);
  }

  DEBUG_VAR(ret);
  return ret;
}

bool Rivet_Interface::Init()
{
  if (m_nevt==0) {
    Data_Reader reader(" ",";","//");
    reader.AddWordSeparator("\t");
    reader.SetAddCommandLine(false);
    reader.SetInputPath(m_inpath);
    std::string infile(m_infile);
    if (infile.find('|')!=std::string::npos)
      infile=infile.substr(0,infile.find('|'));
    reader.SetInputFile(infile+"|BEGIN_"+m_tag+"|END_"+m_tag);
    reader.AddComment("#");

    m_splitjetconts=reader.GetValue<int>("JETCONTS", 0);
    m_splitSH=reader.GetValue<int>("SPLITSH", 0);
    m_splitpm=reader.GetValue<int>("SPLITPM", 0);
    m_splitcoreprocs=reader.GetValue<int>("SPLITCOREPROCS", 0);
    m_usehepmcshort=reader.GetValue<int>("USE_HEPMC_SHORT", 0);
    if (m_usehepmcshort && m_tag!="RIVET") {
      THROW(fatal_error, "Internal error.");
    }
    m_ignorebeams=reader.GetValue<int>("IGNOREBEAMS", 0);
    m_skipweights=reader.GetValue<int>("SKIPWEIGHTS", 1);

    m_hepmcoutputprecision=reader.GetValue<int>("HEPMC_OUTPUT_PRECISION", 15);
    m_xsoutputprecision=reader.GetValue<int>("XS_OUTPUT_PRECISION", 6);

    reader.SetIgnore("");
    Log::setLevel("Rivet", reader.GetValue<int>("-l", 20));
    reader.SetUseGlobalTags(false);
    reader.VectorFromFile(m_analyses,"-a");

    // configure HepMC interface
    bool usehepmcnamedweights(false);
#if defined(HEPMC_HAS_NAMED_WEIGHTS) || defined(RIVET_ENABLE_HEPMC_3)
    usehepmcnamedweights=reader.GetValue<int>("USE_HEPMC_NAMED_WEIGHTS",1);
#endif
    bool usehepmcfullweightinfo(
      reader.GetValue<int>("USE_HEPMC_EXTENDED_WEIGHTS", 0));
    bool usehepmctreelike(
      reader.GetValue<int>("USE_HEPMC_TREE_LIKE", 0));
    bool includehepmcmeonlyvars(
      reader.GetValue<int>("INCLUDE_HEPMC_ME_ONLY_VARIATIONS", 0));
    for (size_t i=0; i<m_ignoreblobs.size(); ++i) {
      m_hepmc2.Ignore(m_ignoreblobs[i]);
    }
    m_hepmc2.SetHepMCNamedWeights(usehepmcnamedweights);
    m_hepmc2.SetHepMCExtendedWeights(usehepmcfullweightinfo);
    m_hepmc2.SetHepMCTreeLike(usehepmctreelike);
    m_hepmc2.SetHepMCIncludeMEOnlyVariations(includehepmcmeonlyvars);
  }
  return true;
}

bool Rivet_Interface::Run(ATOOLS::Blob_List *const bl)
{
  DEBUG_FUNC("");
  Particle_List pl=bl->ExtractParticles(1);
  for (Particle_List::iterator it=pl.begin(); it!=pl.end(); ++it) {
    if ((*it)->Momentum().Nan()) {
      msg_Error()<<METHOD<<" encountered NaN in momentum. Ignoring event:"
                 <<std::endl<<*bl<<std::endl;
      return true;
    }
  }

#ifndef  RIVET_ENABLE_HEPMC_3
  HepMC::GenEvent event;
#else
  std::shared_ptr<HepMC3::GenRunInfo> run_info = std::make_shared<HepMC3::GenRunInfo>();
  HepMC3::GenEvent event(run_info);
#endif
  if (m_usehepmcshort)  m_hepmc2.Sherpa2ShortHepMC(bl, event);
  else                  m_hepmc2.Sherpa2HepMC(bl, event);
  std::vector<HEPMCNS::GenEvent*> subevents(m_hepmc2.GenSubEventList());
#ifdef HEPMC_HAS_CROSS_SECTION
  // leave this, although will be overwritten later
#ifndef  RIVET_ENABLE_HEPMC_3
  HepMC::GenCrossSection xs;
  xs.set_cross_section(p_eventhandler->TotalXS(), p_eventhandler->TotalErr());
#else
  std::shared_ptr<HepMC3::GenCrossSection> xs=std::make_shared<HepMC3::GenCrossSection>();
  xs->set_cross_section(p_eventhandler->TotalXS(), p_eventhandler->TotalErr());
#endif
  event.set_cross_section(xs);
  for (size_t i(0);i<subevents.size();++i) {
    subevents[i]->set_cross_section(xs);
  }
#endif

  if (subevents.size()) {
    for (size_t i(0);i<subevents.size();++i) {
      GetRivet("",0)->analyze(*subevents[i]);
    }
    m_hepmc2.DeleteGenSubEventList();
  }
  else {
    GetRivet("",0)->analyze(event);
    Blob *sp(bl->FindFirst(btp::Signal_Process));
    size_t parts=0;
    if (sp) {
      std::string multi(sp?sp->TypeSpec():"");
      if (multi[3]=='_') multi=multi.substr(2,1);
      else multi=multi.substr(2,2);
      parts=ToType<size_t>(multi);
    }
    if (m_splitjetconts && sp) {
      GetRivet("",parts)->analyze(event);
    }
    if (m_splitcoreprocs && sp) {
      GetRivet(GetCoreProc(sp->TypeSpec()),0)->analyze(event);
      if (m_splitjetconts) {
        GetRivet(GetCoreProc(sp->TypeSpec()),parts)->analyze(event);
      }
    }
    if (m_splitSH && sp) {
      std::string typespec=sp->TypeSpec();
      typespec=typespec.substr(typespec.length()-2, 2);
      std::string type="";
      if (typespec=="+S") type="S";
      else if (typespec=="+H") type="H";
      
      if (type!="") {
        GetRivet(type,0)->analyze(event);
        if (m_splitjetconts) {
          GetRivet(type,parts)->analyze(event);
        }
      }
    }
    if (m_splitpm) {
      GetRivet(event.weights()[0]<0?"M":"P",0)->analyze(event);
    }
  }

  ++m_nevt;
  return true;
}

bool Rivet_Interface::Finish()
{
  PRINT_FUNC(m_outpath);
  GetRivet("",0)->finalize();
  const double nomsumw = GetRivet("",0)->sumW();
  const double nomxsec = p_eventhandler->TotalXS();
  const double nomxerr = p_eventhandler->TotalErr();
  // first call finalize to collapse the event group,
  // then scale the cross-section before re-finalizing
  for (Rivet_Map::iterator it=m_rivet.begin(); it!=m_rivet.end(); it++) {
    std::string out = m_outpath;
    if (it->first.first!="") out+="."+it->first.first;
    if (it->first.second!=0) out+=".j"+ToString(it->first.second);
    it->second->finalize();
    const double wgtfrac = it->second->sumW()/nomsumw;
    const double thisxs  = nomxsec*wgtfrac;
    const double thiserr = nomxerr*wgtfrac;
    it->second->setCrossSection(thisxs, thiserr, true);
    it->second->finalize();
#ifdef HAVE_LIBZ
    it->second->writeData(out+".yoda.gz");
#else
    it->second->writeData(out+".yoda");
#endif
  }
  m_finished=true;
  return true;
}

void Rivet_Interface::ShowSyntax(const int i)
{
  if (!msg_LevelIsInfo() || i==0) return;
  msg_Out()<<METHOD<<"(): {\n\n"
    <<"   RIVET: {\n\n"
    <<"     -a: [<ana_1>, <ana_2>]  # analyses to run\n"
    <<"     # optional parameters:\n"
    <<"     JETCONTS: <0|1>      # perform additional separate analyses for \n"
    <<"                          # each matrix element multiplicity\n"
    <<"     SPLITCOREPROCS: <0|1> # perform additional separate analyses for \n"
    <<"                          # each different core process\n"
    <<"     SPLITSH: <0|1>       # perform additional separate analyses for \n"
    <<"                          # S-MC@NLO S- and H- events\n"
    <<"     IGNOREBEAMS: <0|1>   # tell Rivet to ignore beam information\n"
    <<"     SKIPWEIGHTS: <0|1>   # tell Rivet to skip multi-weight information\n"
    <<"     USE_HEPMC_SHORT: <0|1> # use shortened HepMC event format\n"
    <<"     USE_HEPMC_NAMED_WEIGHTS: <true|false> # use named HepMC weights,\n"
    <<"                          # mandatory for scale variations\n"
    <<"}"<<std::endl;
}

DECLARE_GETTER(Rivet_Interface,"Rivet",
	       Analysis_Interface,Analysis_Arguments);

Analysis_Interface *ATOOLS::Getter
<Analysis_Interface,Analysis_Arguments,Rivet_Interface>::
operator()(const Analysis_Arguments &args) const
{
  std::string outpath=args.m_outpath;
  if (outpath[outpath.length()-1]=='/') {
    outpath.erase(outpath.length()-1, 1);
  }
  std::vector<btp::code> ignoreblobs;
  ignoreblobs.push_back(btp::Unspecified);
  return new Rivet_Interface
    (args.m_inpath,args.m_infile,outpath, std::vector<btp::code>(), "RIVET");
}

void ATOOLS::Getter<Analysis_Interface,Analysis_Arguments,Rivet_Interface>::
PrintInfo(std::ostream &str,const size_t width) const
{
  str<<"Rivet interface";
}


DECLARE_GETTER(RivetShower_Interface,"RivetShower",
	       Analysis_Interface,Analysis_Arguments);

Analysis_Interface *ATOOLS::Getter
<Analysis_Interface,Analysis_Arguments,RivetShower_Interface>::
operator()(const Analysis_Arguments &args) const
{
  std::string outpath=args.m_outpath;
  if (outpath[outpath.length()-1]=='/') {
    outpath.erase(outpath.length()-1, 1);
  }
  std::vector<btp::code> ignoreblobs;
  ignoreblobs.push_back(btp::Unspecified);
  ignoreblobs.push_back(btp::Fragmentation);
  ignoreblobs.push_back(btp::Hadron_Decay);
  ignoreblobs.push_back(btp::Hadron_Mixing);
  return new Rivet_Interface
    (args.m_inpath,args.m_infile,outpath+".SL", ignoreblobs, "RIVETSHOWER");
}

void ATOOLS::Getter<Analysis_Interface,Analysis_Arguments,RivetShower_Interface>::
PrintInfo(std::ostream &str,const size_t width) const
{
  str<<"Rivet interface on top of shower level events.";
}


DECLARE_GETTER(RivetME_Interface,"RivetME",
	       Analysis_Interface,Analysis_Arguments);

Analysis_Interface *ATOOLS::Getter
<Analysis_Interface,Analysis_Arguments,RivetME_Interface>::
operator()(const Analysis_Arguments &args) const
{
  std::string outpath=args.m_outpath;
  if (outpath[outpath.length()-1]=='/') {
    outpath.erase(outpath.length()-1, 1);
  }
  std::vector<btp::code> ignoreblobs;
  ignoreblobs.push_back(btp::Unspecified);
  ignoreblobs.push_back(btp::Fragmentation);
  ignoreblobs.push_back(btp::Hadron_Decay);
  ignoreblobs.push_back(btp::Hadron_Mixing);
  ignoreblobs.push_back(btp::Shower);
  ignoreblobs.push_back(btp::Hadron_To_Parton);
  ignoreblobs.push_back(btp::Hard_Collision);
  ignoreblobs.push_back(btp::QED_Radiation);
  ignoreblobs.push_back(btp::Soft_Collision);
  return new Rivet_Interface
    (args.m_inpath,args.m_infile,outpath+".ME", ignoreblobs, "RIVETME");
}

void ATOOLS::Getter<Analysis_Interface,Analysis_Arguments,RivetME_Interface>::
PrintInfo(std::ostream &str,const size_t width) const
{
  str<<"Rivet interface on top of ME level events.";
}

#endif
// end of Rivet_Interface for Rivet3











#ifdef USING__RIVET2
#include "SHERPA/Tools/HepMC2_Interface.H"
#include "HepMC/GenEvent.h"
#include "HepMC/GenCrossSection.h"
#include "HepMC/WeightContainer.h"
#ifdef USING__HEPMC2__DEFS
#include "HepMC/HepMCDefs.h"
#endif
#include "Rivet/AnalysisHandler.hh"
#include "Rivet/Tools/Logging.hh"

namespace SHERPARIVET {
  typedef std::pair<std::string, int> RivetMapKey;
  typedef std::map<RivetMapKey, Rivet::AnalysisHandler*> Rivet_Map;

  class Rivet_Scale_Variation {
  private:
    std::string m_name;
    Rivet_Map   m_rivetmap;
    double      m_wgt,m_n,m_sum,m_sum2,m_tempn,m_tempsum;
    std::vector<double> m_rswgts;
  public:
    Rivet_Scale_Variation(std::string name="");
    ~Rivet_Scale_Variation();

    void   AddPoint(const double& wgt, const double& n, size_t xsmode=0);
    void   SynchroniseCrossSection();
    double TotalXS()  const;
    double TotalVar() const;
    double TotalErr() const;

    inline Rivet_Map&  RivetMap() { return m_rivetmap; }

    inline std::string Name()         const { return m_name; }
    inline double      Weight()       const { return m_wgt; }
    inline double      Weight(size_t i) const { return m_rswgts[i]; }
    inline double      SumOfWeights() const { return m_sum; }

    inline void ResetRSWeights() { m_rswgts.clear(); }
  };

  typedef std::map<std::string, Rivet_Scale_Variation *> RivetScaleVariationMap;

  class Rivet_Interface: public SHERPA::Analysis_Interface {
  private:

    std::string m_inpath, m_infile, m_outpath, m_tag;
    std::vector<std::string> m_analyses;

    size_t m_nevt;
    bool   m_finished;
    bool   m_splitjetconts, m_splitSH, m_splitpm,
           m_splitcoreprocs, m_splitvariations,
           m_ignorebeams, m_usehepmcshort,
           m_printsummary,
           m_evtbyevtxs;
    size_t m_nin;
    size_t m_hepmcoutputprecision, m_xsoutputprecision;

    RivetScaleVariationMap         m_rivet;
    SHERPA::HepMC2_Interface       m_hepmc2;
    std::vector<ATOOLS::btp::code> m_ignoreblobs;
    std::map<std::string,size_t>   m_weightidxmap;

    void ExtractVariations(const HepMC::GenEvent& evt,
                           const std::vector<HepMC::GenEvent*>& subevents);
    void ExtractVariations(const HepMC::GenEvent& evt);
    void SetEventWeight(const Rivet_Scale_Variation* rsv,
                        HepMC::GenEvent& evt,
                        const int& idx=-1);
    void ResetRivetScaleVariationMapRSWeights();

    Rivet::AnalysisHandler* GetRivet(Rivet_Map& rm, std::string proc,
                                     int jetcont);
    std::string             GetCoreProc(const std::string& proc);

  public:
    Rivet_Interface(const std::string &inpath,
                    const std::string &infile,
                    const std::string &outpath,
                    const std::vector<ATOOLS::btp::code> &ignoreblobs,
                    const std::string &tag);
    ~Rivet_Interface();

    bool Init();
    bool Run(ATOOLS::Blob_List *const bl);
    bool Finish();

    void ShowSyntax(const int i);
  };

  class RivetShower_Interface: public Rivet_Interface {};
  class RivetME_Interface: public Rivet_Interface {};
}


using namespace SHERPARIVET;
using namespace SHERPA;
using namespace ATOOLS;
using namespace Rivet;


Rivet_Scale_Variation::Rivet_Scale_Variation(std::string name) :
  m_name(name), m_rivetmap(),
  m_wgt(0.), m_n(0.), m_sum(0.), m_sum2(0.), m_tempn(0.), m_tempsum(0.),
  m_rswgts(0,0.)
{
}

Rivet_Scale_Variation::~Rivet_Scale_Variation()
{
  for (Rivet_Map::iterator it=m_rivetmap.begin();it!=m_rivetmap.end();++it) {
    delete it->second;
  }
}

void Rivet_Scale_Variation::AddPoint(const double& wgt, const double& n,
                                     size_t xsmode)
{
  DEBUG_FUNC("wgt="<<wgt<<", n="<<n<<", mode="<<xsmode);
  if      (xsmode==0) {
    m_wgt=wgt;
    m_n+=n;
    m_sum+=wgt;
    m_sum2+=ATOOLS::sqr(wgt);
  }
  else if (xsmode==1) {
    if (m_tempn>0 && m_tempn!=n) THROW(fatal_error,"Inconsistent ntrial.");
    m_rswgts.push_back(wgt);
    m_tempn=n;
    m_tempsum+=wgt;
  }
  else THROW(fatal_error,"Unknown xs-mode.");
}

void Rivet_Scale_Variation::SynchroniseCrossSection()
{
  m_n+=m_tempn;
  m_sum+=m_tempsum;
  m_sum2+=ATOOLS::sqr(m_tempsum);
  m_tempn=m_tempsum=0.;
}

double Rivet_Scale_Variation::TotalXS() const
{
  if (m_n==0.) return 0.;
  return m_sum/m_n;
}

double Rivet_Scale_Variation::TotalVar() const
{
  if (m_n<=1.) return ATOOLS::sqr(TotalXS());
  return (m_sum2-m_sum*m_sum/m_n)/(m_n-1.);
}

double Rivet_Scale_Variation::TotalErr() const
{
  if (m_n<=1.) return TotalXS();
  if (ATOOLS::IsEqual
      (m_sum2*m_n,m_sum*m_sum,1.0e-6)) return 0.;
  return sqrt((m_sum2-m_sum*m_sum/m_n)/(m_n-1.)/m_n);
}

Rivet_Interface::Rivet_Interface(const std::string &inpath,
                                 const std::string &infile,
                                 const std::string &outpath,
                                 const std::vector<btp::code> &ignoreblobs,
                                 const std::string &tag) :
  Analysis_Interface("Rivet"),
  m_inpath(inpath), m_infile(infile), m_outpath(outpath), m_tag(tag),
  m_nevt(0), m_finished(false),
  m_splitjetconts(false), m_splitSH(false), m_splitpm(false),
  m_splitcoreprocs(false), m_splitvariations(true),
  m_ignoreblobs(ignoreblobs),
  m_printsummary(true), m_evtbyevtxs(false), m_nin(2),
  m_hepmcoutputprecision(15), m_xsoutputprecision(6)
{
  if (m_outpath[m_outpath.size()-1]=='/')
    m_outpath=m_outpath.substr(0,m_outpath.size()-1);
#ifdef USING__MPI
  if (mpi->Rank()==0) {
#endif
    if (m_outpath.rfind('/')!=std::string::npos)
      MakeDir(m_outpath.substr(0,m_outpath.rfind('/')));
#ifdef USING__MPI
  }
  if (mpi->Size()>1) {
    m_outpath.insert(m_outpath.length(),"_"+rpa->gen.Variable("RNG_SEED"));
  }
#endif
}

Rivet_Interface::~Rivet_Interface()
{
  if (!m_finished) Finish();
  for (RivetScaleVariationMap::iterator it(m_rivet.begin());
       it!=m_rivet.end();++it) {
    delete it->second;
  }
  m_rivet.clear();
}

void Rivet_Interface::ExtractVariations
(const HepMC::GenEvent& evt,const std::vector<HepMC::GenEvent*>& subevents)
{
  DEBUG_FUNC("# of subevts: "<<subevents.size());
  if (subevents.size()) {
    for (size_t i(0);i<subevents.size();++i) {
      ExtractVariations(*subevents[i]);
    }
  }
  else ExtractVariations(evt);
}

void Rivet_Interface::ExtractVariations(const HepMC::GenEvent& evt)
{
  DEBUG_FUNC("");
  const HepMC::WeightContainer& wc(evt.weights());
  std::map<std::string,double> wgtmap;
  double ntrials(1.);
  size_t xstype(0);
#ifdef HEPMC_HAS_NAMED_WEIGHTS
#ifdef HEPMC_HAS_WORKING_NAMED_WEIGHTS // replace by final HepMC-2.07 variable
  std::vector<std::string> keys(wc.keys());
  msg_Debugging()<<keys<<std::endl;
  for (size_t i(0);i<keys.size();++i) {
    std::string cur(keys[i]);
    if (m_splitvariations && cur.find("MUR")!=std::string::npos &&
                             cur.find("MUF")!=std::string::npos &&
                             cur.find("PDF")!=std::string::npos) {
      wgtmap[cur]=wc[cur];
    }
    else if (cur=="Weight")  wgtmap["nominal"]=wc[cur];
    else if (cur=="NTrials") ntrials=wc[cur];
    else if (cur=="Reweight_Type" && wc[cur]&64) xstype=1;
  }
#else
  // lookup all evt-wgts with name "MUR<fac>_MUF<fac>_PDF<id>"
  // at the moment the only way to do that is to filter the printout
  // accuracy limited to print out accu of 6 digits, must suffice
  MyStrStream str;
  str.precision(m_hepmcoutputprecision);
  wc.print(str);

  // need a temp object first, as we need to get ntrials first
  while (str) {
    double wgt(0.);
    std::string cur("");
    str>>cur;
    if (cur.length()==0) continue;
    // weight is between "," and trailing bracket
    // name is between leading bracket and ","
    wgt=ToType<double>(cur.substr(cur.find(",")+1,cur.find(")")-1));
    cur=cur.substr(1,cur.find(",")-1);
    if (m_splitvariations && cur.find("MUR")!=std::string::npos &&
                             cur.find("MUF")!=std::string::npos &&
                             cur.find("PDF")!=std::string::npos) {
      wgtmap[cur]=wgt;
    }
    else if (cur=="Weight")  wgtmap["nominal"]=wgt;
    else if (cur=="NTrials") ntrials=wgt;
    else if (cur=="Reweight_Type" && ((int)wgt)&64) xstype=1;
  }
#endif /* HEPMC_HAS_WORKING_NAMED_WEIGHTS */
#else
  wgtmap["nominal"]=wc[0];
  ntrials=wc[3];
  xstype=(((wc.size()==5&&((int)wc[4]&64))||(wc.size()==11&&((int)wc[10]&64)))?1:0);
#endif /* HEPMC_HAS_NAMED_WEIGHTS */
  if (msg_LevelIsDebugging()) {
    for (std::map<std::string,double>::iterator wit(wgtmap.begin());
         wit!=wgtmap.end();++wit)
      msg_Out()<<wit->first<<" : "<<wit->second<<std::endl;
  }
  // now construct or fill into the scale variation map
  for (std::map<std::string,double>::iterator wit(wgtmap.begin());
       wit!=wgtmap.end();++wit) {
    RivetScaleVariationMap::iterator rit=m_rivet.find(wit->first);
    if (rit==m_rivet.end()) {
      msg_Debugging()<<"creating new entry in m_rivet"<<std::endl;
      m_rivet[wit->first]=new Rivet_Scale_Variation(wit->first);
      m_rivet[wit->first]->AddPoint(wit->second,ntrials,xstype);
    }
    else rit->second->AddPoint(wit->second,ntrials,xstype);
  }
  if (msg_LevelIsDebugging()) {
    for (RivetScaleVariationMap::iterator rit(m_rivet.begin());
         rit!=m_rivet.end();++rit)
      msg_Out()<<rit->first<<" : "<<rit->second->Weight()<<std::endl;
  }
}

void Rivet_Interface::SetEventWeight(const Rivet_Scale_Variation* rsv,
                                     HepMC::GenEvent& evt, const int& idx)
{
  double wgt(idx<0?rsv->Weight():rsv->Weight(idx));
  DEBUG_FUNC(rsv->Name()<<": "<<wgt);
  evt.weights()[0]=wgt;
#ifdef HEPMC_HAS_CROSS_SECTION
  evt.cross_section()->set_cross_section(rsv->TotalXS(),rsv->TotalErr());
#endif
}

void Rivet_Interface::ResetRivetScaleVariationMapRSWeights()
{
  for (RivetScaleVariationMap::iterator rit(m_rivet.begin());
       rit!=m_rivet.end();++rit) rit->second->ResetRSWeights();
}

AnalysisHandler* Rivet_Interface::GetRivet(Rivet_Map& rm, std::string proc,
                                           int jetcont)
{
  DEBUG_FUNC(proc<<" "<<jetcont);
  RivetMapKey key = std::make_pair(proc, jetcont);
  Rivet_Map::iterator it=rm.find(key);
  if (it!=rm.end()) {
    msg_Debugging()<<"found "<<key.first<<" "<<key.second<<std::endl;
    return it->second;
  }
  else {
    msg_Debugging()<<"create new "<<key.first<<" "<<key.second<<std::endl;
    AnalysisHandler* rivet(new AnalysisHandler());
    rivet->setIgnoreBeams(m_ignorebeams);
    rivet->addAnalyses(m_analyses);
    rm.insert(std::make_pair(key, rivet));
    msg_Debugging()<<"now "<<rm.size()<<" in "
                   <<key.first<<" "<<key.second<<std::endl;
    return rivet;
  }
}

std::string Rivet_Interface::GetCoreProc(const std::string& proc)
{
  DEBUG_FUNC(proc);
  size_t idx=5;
  std::vector<ATOOLS::Flavour> flavs;
  while (idx<proc.size()) {
    std::string fl(1, proc[idx]);
    if (fl=="_") {
      ++idx;
      continue;
    }
    for (++idx; idx<proc.size(); ++idx) {
      if (proc[idx]=='_') break;
      fl+=proc[idx];
    }
    bool bar(false);
    if (fl.length()>1) {
      if (fl[fl.length()-1]=='b') {
        fl.erase(fl.length()-1,1);
        bar=true;
      }
      else if ((fl[0]=='W' || fl[0]=='H')) {
        if (fl[fl.length()-1]=='-') {
          fl[fl.length()-1]='+';
          bar=true;
        }
      }
      else if (fl[fl.length()-1]=='+') {
        fl[fl.length()-1]='-';
        bar=true;
      }
    }
    Flavour flav(s_kftable.KFFromIDName(fl));
    if (bar) flav=flav.Bar();
    flavs.push_back(flav);
  }

  std::vector<Flavour> nojetflavs;
  for (size_t i=2; i<flavs.size(); ++i) {
    if (!Flavour(kf_jet).Includes(flavs[i])) nojetflavs.push_back(flavs[i]);
  }

  std::vector<Flavour> noresflavs;
  for (size_t i=0; i<nojetflavs.size(); ++i) {
    if (!Flavour(kf_resummed).Includes(nojetflavs[i])) noresflavs.push_back(nojetflavs[i]);
  }

  std::vector<Flavour> finalflavs;
  // start with initial state
  for (size_t i=0; i<2; ++i) {
    if (Flavour(kf_jet).Includes(flavs[i]))
      finalflavs.push_back(Flavour(kf_jet));
    else if (Flavour(kf_resummed).Includes(flavs[i]))
      finalflavs.push_back(Flavour(kf_resummed));
    else
      finalflavs.push_back(flavs[i]);
  }
  // add all non-jet and non-resummed particles
  for (size_t i=0; i<noresflavs.size(); ++i) {
    finalflavs.push_back(noresflavs[i]);
  }
  // add all resummed particles
  for (size_t i=0; i<nojetflavs.size()-noresflavs.size(); ++i) {
    if (finalflavs.size()>3) break;
    finalflavs.push_back(Flavour(kf_resummed));
  }
  // add all jet particles
  for (size_t i=0; i<flavs.size()-2-nojetflavs.size(); ++i) {
    if (finalflavs.size()>3) break;
    finalflavs.push_back(Flavour(kf_jet));
  }

  std::string ret;
  for (size_t i=0; i<finalflavs.size(); ++i) {
    ret+=finalflavs[i].IDName();
    ret+="__";
  }
  while (ret[ret.length()-1]=='_') {
    ret.erase(ret.length()-1, 1);
  }

  DEBUG_VAR(ret);
  return ret;
}

bool Rivet_Interface::Init()
{
  if (m_nevt==0) {
    Data_Reader reader(" ",";","//");
    reader.AddWordSeparator("\t");
    reader.SetAddCommandLine(false);
    reader.SetInputPath(m_inpath);
    std::string infile(m_infile);
    if (infile.find('|')!=std::string::npos)
      infile=infile.substr(0,infile.find('|'));
    reader.SetInputFile(infile+"|BEGIN_"+m_tag+"|END_"+m_tag);
    reader.AddComment("#");

    m_splitjetconts=reader.GetValue<int>("JETCONTS", 0);
    m_splitSH=reader.GetValue<int>("SPLITSH", 0);
    m_splitpm=reader.GetValue<int>("SPLITPM", 0);
    m_splitcoreprocs=reader.GetValue<int>("SPLITCOREPROCS", 0);
    m_splitvariations=reader.GetValue<int>("SPLITVARIATIONS", 1);
    m_usehepmcshort=reader.GetValue<int>("USE_HEPMC_SHORT", 0);
    if (m_usehepmcshort && m_tag!="RIVET") {
      THROW(fatal_error, "Internal error.");
    }
    m_printsummary=reader.GetValue<int>("PRINT_SUMMARY",1);
    m_evtbyevtxs=reader.GetValue<int>("EVENTBYEVENTXS",0);
    m_ignorebeams=reader.GetValue<int>("IGNOREBEAMS", 0);

    m_hepmcoutputprecision=reader.GetValue<int>("HEPMC_OUTPUT_PRECISION", 15);
    m_xsoutputprecision=reader.GetValue<int>("XS_OUTPUT_PRECISION", 6);

    reader.SetIgnore("");
    Log::setLevel("Rivet", reader.GetValue<int>("-l", 20));
    reader.SetUseGlobalTags(false);
    reader.VectorFromFile(m_analyses,"-a");
    for (size_t i(0);i<m_analyses.size();++i) {
      if (m_analyses[i]==std::string("MC_XS")) break;
      if (i==m_analyses.size()-1) m_analyses.push_back(std::string("MC_XS"));
    }

    // configure HepMC interface
    bool usehepmcnamedweights(false);
#ifdef HEPMC_HAS_NAMED_WEIGHTS
    usehepmcnamedweights=reader.GetValue<int>("USE_HEPMC_NAMED_WEIGHTS",1);
#endif
    bool usehepmcfullweightinfo(
      reader.GetValue<int>("USE_HEPMC_EXTENDED_WEIGHTS", 0));
    bool usehepmctreelike(
      reader.GetValue<int>("USE_HEPMC_TREE_LIKE", 0));
    bool includehepmcmeonlyvars(
      reader.GetValue<int>("INCLUDE_HEPMC_ME_ONLY_VARIATIONS", 0));
    for (size_t i=0; i<m_ignoreblobs.size(); ++i) {
      m_hepmc2.Ignore(m_ignoreblobs[i]);
    }
    m_hepmc2.SetHepMCNamedWeights(usehepmcnamedweights);
    m_hepmc2.SetHepMCExtendedWeights(usehepmcfullweightinfo);
    m_hepmc2.SetHepMCTreeLike(usehepmctreelike);
    m_hepmc2.SetHepMCIncludeMEOnlyVariations(includehepmcmeonlyvars);
  }
  return true;
}

bool Rivet_Interface::Run(ATOOLS::Blob_List *const bl)
{
  DEBUG_FUNC("");
  Blob *sp(bl->FindFirst(btp::Signal_Process));
  if (sp) m_nin=sp->NInP();
  Particle_List pl=bl->ExtractParticles(1);
  for (Particle_List::iterator it=pl.begin(); it!=pl.end(); ++it) {
    if ((*it)->Momentum().Nan()) {
      msg_Error()<<METHOD<<" encountered NaN in momentum. Ignoring event:"
                 <<endl<<*bl<<endl;
      return true;
    }
  }

  double weight(bl->Weight());
  HepMC::GenEvent event;
  if (m_usehepmcshort)  m_hepmc2.Sherpa2ShortHepMC(bl, event, weight);
  else                  m_hepmc2.Sherpa2HepMC(bl, event, weight);
  std::vector<HepMC::GenEvent*> subevents(m_hepmc2.GenSubEventList());
#ifdef HEPMC_HAS_CROSS_SECTION
  // leave this, although will be overwritten later
  HepMC::GenCrossSection xs;
  xs.set_cross_section(p_eventhandler->TotalXS(), p_eventhandler->TotalErr());
  event.set_cross_section(xs);
  for (size_t i(0);i<subevents.size();++i) {
    subevents[i]->set_cross_section(xs);
  }
#endif

  // 1st event build index map, thereafter only lookup
  ExtractVariations(event,subevents);
  for (RivetScaleVariationMap::iterator it(m_rivet.begin());
       it!=m_rivet.end();++it) {
    msg_Debugging()<<"Running rivet for "<<it->first<<" with "
                   <<it->second->RivetMap().size()<<" histograms."<<std::endl;
    Rivet_Map& rivetmap(it->second->RivetMap());
    if (subevents.size()) {
      it->second->SynchroniseCrossSection();
      for (size_t i(0);i<subevents.size();++i) {
        SetEventWeight(it->second,*subevents[i],i);
        GetRivet(rivetmap,"", 0)->analyze(*subevents[i]);
      }
    }
    else {
      SetEventWeight(it->second,event);
      GetRivet(rivetmap,"",0)->analyze(event);
      Blob *sp(bl->FindFirst(btp::Signal_Process));
      size_t parts=0;
      if (sp) {
        std::string multi(sp?sp->TypeSpec():"");
        if (multi[3]=='_') multi=multi.substr(2,1);
        else multi=multi.substr(2,2);
        parts=ToType<size_t>(multi);
      }
      if (m_splitjetconts && sp) {
        GetRivet(rivetmap,"",parts)->analyze(event);
      }
      if (m_splitcoreprocs && sp) {
        GetRivet(rivetmap,GetCoreProc(sp->TypeSpec()),0)
            ->analyze(event);
        if (m_splitjetconts) {
          GetRivet(rivetmap,GetCoreProc(sp->TypeSpec()),parts)
              ->analyze(event);
        }
      }
      if (m_splitSH && sp) {
        std::string typespec=sp->TypeSpec();
        typespec=typespec.substr(typespec.length()-2, 2);
        std::string type="";
        if (typespec=="+S") type="S";
        else if (typespec=="+H") type="H";

        if (type!="") {
          GetRivet(rivetmap,type,0)->analyze(event);
          if (m_splitjetconts) {
            GetRivet(rivetmap,type,parts)->analyze(event);
          }
        }
      }
      if (m_splitpm) {
        GetRivet(rivetmap,event.weights()[0]<0?"M":"P",0)->analyze(event);
      }
    }
  }
  if (subevents.size()) {
    ResetRivetScaleVariationMapRSWeights();
    m_hepmc2.DeleteGenSubEventList();
  }

  ++m_nevt;
  for (RivetScaleVariationMap::iterator it(m_rivet.begin());
       it!=m_rivet.end();++it) {
    msg_Debugging()<<"Checking rivet for "<<it->first<<" with "
                   <<it->second->RivetMap().size()<<" histograms."<<std::endl;
  }
  if (m_evtbyevtxs) {
    for (RivetScaleVariationMap::iterator mit(m_rivet.begin());
         mit!=m_rivet.end();++mit) {
      std::string out=m_outpath;
      if (mit->first!="" && mit->first!="nominal") out += "."+mit->first;
      std::string namestr(m_rivet.size()>1?" for "+mit->first:"");
      std::string output(std::string("**  Total ")
                         +std::string((m_nin==1?"Width":"XS"))+namestr
                         +std::string(" = ( ")
                         +ToString(mit->second->TotalXS(),m_xsoutputprecision)
                         +std::string(" +- ")
                         +ToString(mit->second->TotalErr(),m_xsoutputprecision)
                         +std::string(" ) ")
                         +std::string((m_nin==1?"GeV":"pb"))
                         +std::string(" **"));
      std::string astline(output.size(),'*');
      msg_Info()<<astline<<"\n"<<output<<"\n"<<astline<<std::endl;
    }
  }
  return true;
}

bool Rivet_Interface::Finish()
{
  std::string ending("");
  ending=".yoda";
  for (RivetScaleVariationMap::iterator mit(m_rivet.begin());
       mit!=m_rivet.end();++mit) {
    std::string out=m_outpath;
    if (mit->first!="" && mit->first!="nominal") out += "."+mit->first;
    PRINT_FUNC(out+ending);
    if (m_printsummary) {
      std::string namestr(m_rivet.size()>1?" for "+mit->first:"");
      std::string output(std::string("**  Total ")
                         +std::string((m_nin==1?"Width":"XS"))+namestr
                         +std::string(" = ( ")
                         +ToString(mit->second->TotalXS(),m_xsoutputprecision)
                         +std::string(" +- ")
                         +ToString(mit->second->TotalErr(),m_xsoutputprecision)
                         +std::string(" ) ")
                         +std::string((m_nin==1?"GeV":"pb"))
                         +std::string(" **"));
      std::string astline(output.size(),'*');
      msg_Info()<<astline<<"\n"<<output<<"\n"<<astline<<std::endl;
    }
    for (Rivet_Map::iterator it=mit->second->RivetMap().begin();
         it!=mit->second->RivetMap().end(); ++it) {
      const double wgtfrac = it->second->sumOfWeights()/mit->second->SumOfWeights();
      const double totalxs = it->second->crossSection();
      const double thisxs  = totalxs*wgtfrac;
      it->second->setCrossSection(thisxs);
      std::string jout=out;
      if (it->first.first!="") jout+="."+it->first.first;
      if (it->first.second!=0) jout+=".j"+ToString(it->first.second);
      it->second->finalize();
      it->second->writeData(jout+ending);
    }
  }
  m_finished=true;
  return true;
}

void Rivet_Interface::ShowSyntax(const int i)
{
  if (!msg_LevelIsInfo() || i==0) return;
  msg_Out()<<METHOD<<"(): {\n\n"
    <<"   BEGIN_RIVET {\n\n"
    <<"     <Option> <Value>     optional parameters\n"
    <<"     -a <ana_1> <ana_2>   analyses to run\n"
    <<"\n   } END_RIVET\n\n"
    <<"   Options:\n"
    <<"     JETCONTS <0|1>       perform additional separate analyses for \n"
    <<"                          each matrix element multiplicity\n"
    <<"     SPLITCOREPROCS <0|1> perform additional separate analyses for \n"
    <<"                          each different core process\n"
    <<"     SPLITSH <0|1>        perform additional separate analyses for \n"
    <<"                          S-MC@NLO S- and H- events\n"
    <<"     IGNOREBEAMS <0|1>    tell Rivet to ignore beam information\n"
    <<"     USE_HEPMC_SHORT <0|1> use shortened HepMC event format\n"
    <<"     USE_HEPMC_NAMED WEIGHTS <1|0> use named HepMC weights,\n"
    <<"                          mandatory for scale variations\n"
    <<"     HEPMC_OUTPUT_PRECISION <val> precision of intermediate hepmc output \n"
    <<"                          defaults to 15\n"
    <<"     PRINT_SUMMARY <1|0>  print cross section summary at the end\n"
    <<"     EVENTBYEVENTXS <0|1> print cross section event-by-event\n"
    <<"}"<<std::endl;
}

DECLARE_GETTER(Rivet_Interface,"Rivet",
	       Analysis_Interface,Analysis_Arguments);

Analysis_Interface *ATOOLS::Getter
<Analysis_Interface,Analysis_Arguments,Rivet_Interface>::
operator()(const Analysis_Arguments &args) const
{
  std::string outpath=args.m_outpath;
  if (outpath[outpath.length()-1]=='/') {
    outpath.erase(outpath.length()-1, 1);
  }
  return new Rivet_Interface
    (args.m_inpath,args.m_infile,outpath, std::vector<btp::code>(), "RIVET");
}

void ATOOLS::Getter<Analysis_Interface,Analysis_Arguments,Rivet_Interface>::
PrintInfo(std::ostream &str,const size_t width) const
{
  str<<"Rivet interface";
}


DECLARE_GETTER(RivetShower_Interface,"RivetShower",
	       Analysis_Interface,Analysis_Arguments);

Analysis_Interface *ATOOLS::Getter
<Analysis_Interface,Analysis_Arguments,RivetShower_Interface>::
operator()(const Analysis_Arguments &args) const
{
  std::string outpath=args.m_outpath;
  if (outpath[outpath.length()-1]=='/') {
    outpath.erase(outpath.length()-1, 1);
  }
  std::vector<btp::code> ignoreblobs;
  ignoreblobs.push_back(btp::Fragmentation);
  ignoreblobs.push_back(btp::Hadron_Decay);
  ignoreblobs.push_back(btp::Hadron_Mixing);
  return new Rivet_Interface
    (args.m_inpath,args.m_infile,outpath+".SL", ignoreblobs, "RIVETSHOWER");
}

void ATOOLS::Getter<Analysis_Interface,Analysis_Arguments,RivetShower_Interface>::
PrintInfo(std::ostream &str,const size_t width) const
{
  str<<"Rivet interface on top of shower level events.";
}


DECLARE_GETTER(RivetME_Interface,"RivetME",
	       Analysis_Interface,Analysis_Arguments);

Analysis_Interface *ATOOLS::Getter
<Analysis_Interface,Analysis_Arguments,RivetME_Interface>::
operator()(const Analysis_Arguments &args) const
{
  std::string outpath=args.m_outpath;
  if (outpath[outpath.length()-1]=='/') {
    outpath.erase(outpath.length()-1, 1);
  }
  std::vector<btp::code> ignoreblobs;
  ignoreblobs.push_back(btp::Fragmentation);
  ignoreblobs.push_back(btp::Hadron_Decay);
  ignoreblobs.push_back(btp::Hadron_Mixing);
  ignoreblobs.push_back(btp::Shower);
  ignoreblobs.push_back(btp::Hadron_To_Parton);
  ignoreblobs.push_back(btp::Hard_Collision);
  ignoreblobs.push_back(btp::QED_Radiation);
  ignoreblobs.push_back(btp::Soft_Collision);
  return new Rivet_Interface
    (args.m_inpath,args.m_infile,outpath+".ME", ignoreblobs, "RIVETME");
}

void ATOOLS::Getter<Analysis_Interface,Analysis_Arguments,RivetME_Interface>::
PrintInfo(std::ostream &str,const size_t width) const
{
  str<<"Rivet interface on top of ME level events.";
}

#endif
