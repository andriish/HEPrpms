#include "SHERPA/PerturbativePhysics/Matrix_Element_Handler.H"

#include "ATOOLS/Org/Message.H"
#include "ATOOLS/Math/Random.H"
#include "ATOOLS/Org/Exception.H"
#include "METOOLS/Main/Spin_Structure.H"
#include "ATOOLS/Org/Run_Parameter.H"
#include "ATOOLS/Org/Data_Reader.H"
#include "ATOOLS/Org/MyStrStream.H"
#include "ATOOLS/Org/Shell_Tools.H"
#include "ATOOLS/Org/Return_Value.H"
#include "SHERPA/PerturbativePhysics/Shower_Handler.H"
#include "SHERPA/Tools/Variations.H"
#include "ATOOLS/Org/CXXFLAGS.H"
#include "PDF/Main/Shower_Base.H"
#include "PDF/Main/NLOMC_Base.H"
#include "ATOOLS/Phys/Cluster_Amplitude.H"
#include "PHASIC++/Process/MCatNLO_Process.H"
#include "PHASIC++/Process/ME_Generator_Base.H"
#include "PHASIC++/Main/Process_Integrator.H"
#include "PHASIC++/Main/Phase_Space_Handler.H"
#include "ATOOLS/Org/My_MPI.H"
#include "ATOOLS/Org/RUsage.H"
#ifdef USING__GZIP
#include "ATOOLS/Org/Gzip_Stream.H"
#endif

#include <cassert>
#include <unistd.h>

using namespace SHERPA;
using namespace PHASIC;
using namespace PDF;
using namespace ATOOLS;

Matrix_Element_Handler::Matrix_Element_Handler
(const std::string &dir,const std::string &file,
 const std::string &processfile,const std::string &selectorfile):
  m_gens(dir, file),
  p_proc(NULL), p_beam(NULL), p_isr(NULL), p_model(NULL),
  m_path(dir), m_file(file), m_processfile(processfile),
  m_selectorfile(selectorfile), m_eventmode(0), m_hasnlo(0),
  p_shower(NULL), p_nlomc(NULL), p_variationweights(NULL),
  m_sum(0.0), m_globalnlomode(0),
  m_ranidx(0), m_fosettings(0), p_ranin(NULL), p_ranout(NULL)
{
  Data_Reader read(" ",";","!","=");
  read.AddComment("#");
  read.SetInputPath(m_path);
  read.SetInputFile(m_file);
  if (!read.ReadFromFile(m_respath,"RESULT_DIRECTORY")) m_respath="Results";
  m_respath=ShortenPathName(m_respath);
  if (m_respath[0]!='/' && rpa->gen.Variable("PATH_PIECE")!="")
    m_respath=rpa->gen.Variable("PATH_PIECE")+"/"+m_respath;
  std::string evtm;
  if (!read.ReadFromFile(evtm,"EVENT_GENERATION_MODE")) evtm="PartiallyUnweighted";
  if (evtm=="Unweighted" || evtm=="U") m_eventmode=1;
  else if (evtm=="PartiallyUnweighted" || evtm=="P") m_eventmode=2;
  else m_eventmode=0;
  //need for LHE-output
  rpa->gen.SetVariable("EVENT_GENERATION_MODE",ToString(m_eventmode));
  if (!read.ReadFromFile(m_ovwth,"OVERWEIGHT_THRESHOLD")) m_ovwth=1e12;
  if (!read.ReadFromFile(m_seedmode,"EVENT_SEED_MODE")) m_seedmode=0;
  else msg_Info()<<METHOD<<"(): Set seed mode "<<m_seedmode<<"."<<std::endl;
  if (!read.ReadFromFile(m_rsadd,"MEH_RSADD")) m_rsadd=1;
  else msg_Info()<<METHOD<<"(): Set RS add mode "<<m_rsadd<<"."<<std::endl;
  std::string seedfile;
  if (!read.ReadFromFile(seedfile,"EVENT_SEED_FILE")) 
    seedfile="ran.stat."+rpa->gen.Variable("RANDOM_SEED");
  else msg_Info()<<METHOD<<"(): Set seed file "<<seedfile<<"."<<std::endl;
#ifdef USING__GZIP
  seedfile+=".gz";
#endif
  if (m_seedmode==1) {
#ifdef USING__GZIP
    p_ranin = new ATOOLS::igzstream(seedfile.c_str());
#else
    p_ranin = new std::ifstream(seedfile.c_str());
#endif
    if (p_ranin!=NULL && !p_ranin->good()) THROW
      (fatal_error,"Cannot initialize random generator status file");
  }
  else if (m_seedmode==2) {
#ifdef USING__GZIP
    p_ranout = new ATOOLS::ogzstream(seedfile.c_str());
#else
    p_ranout = new std::ofstream(seedfile.c_str());
#endif
    if (p_ranout!=NULL && !p_ranout->good()) THROW
      (fatal_error,"Cannot initialize random generator status file");
  }
  else if (m_seedmode==3) {
    ran->SetSeedStorageIncrement(read.GetValue("EVENT_SEED_INCREMENT",1));
    msg_Info()<<METHOD<<"(): Set seed increment to "
              <<read.GetValue("EVENT_SEED_INCREMENT",1)<<std::endl;
  }
  m_pilotrunrequired = false;
  m_haspilotscale = false;
  m_pilotrunenabled = ran->CanRestoreStatus() && (m_eventmode != 0);
  msg_Info()<<METHOD<<"(): Set pilot run mode to "<<m_pilotrunenabled<<".\n";
  std::string nlomodestring("");
  if (!read.ReadFromFile(nlomodestring,"NLO_Mode")) m_globalnlomode=0;
  else {
    if (nlomodestring=="MC@NLO" || nlomodestring=="MENLOPS" ||
        nlomodestring=="MEPS@NLO") m_globalnlomode=3;
    else if (nlomodestring=="Fixed_Order") m_globalnlomode=1;
    else m_globalnlomode=ToType<size_t>(nlomodestring);
  }
  msg_Debugging()<<METHOD<<"(): NLO_Mode = "<<m_globalnlomode<<std::endl;
  if (m_globalnlomode!=0 && m_globalnlomode!=1 && m_globalnlomode !=3)
    THROW(fatal_error,"Unknown NLO_Mode="+nlomodestring);
  if (!read.ReadFromFile(m_recalculate_zeros, "PILOT_RUN_RECALCULATE_ZEROS"))
    m_recalculate_zeros = 1;
}

Matrix_Element_Handler::~Matrix_Element_Handler()
{
  if (p_ranin) delete p_ranin;
  if (p_ranout) delete p_ranout;
  for (size_t i=0;i<m_pmaps.size();++i) {
    for (std::map<nlo_type::code,StringProcess_Map*>::const_iterator
	   pmit(m_pmaps[i]->begin());pmit!=m_pmaps[i]->end();++pmit)
      delete pmit->second;
    delete m_pmaps[i];
  }
  for (size_t i=0; i<m_procs.size(); ++i)
    if (dynamic_cast<MCatNLO_Process*>(m_procs[i])) delete m_procs[i];
  if (p_nlomc) delete p_nlomc;
}

void Matrix_Element_Handler::InitNLOMC()
{
  Data_Reader read(" ",";","!","=");
  read.AddComment("#");
  read.SetInputPath(m_path);
  read.SetInputFile(m_file);
  std::string nlomc((m_globalnlomode&2)?"MC@NLO":"");
  nlomc+="_"+read.GetValue<std::string>("NLOMC_GENERATOR","CSS");
  p_nlomc = NLOMC_Getter::GetObject(nlomc,NLOMC_Key(p_model,p_isr,&read));
}

bool Matrix_Element_Handler::CalculateTotalXSecs() 
{
  int storeresults = Data_Reader(" ",";","!","=").GetValue("GENERATE_RESULT_DIRECTORY", 1);
  if (storeresults<0) return true;
  if (storeresults) {
    My_In_File::OpenDB(m_respath+"/");
    My_In_File::ExecDB(m_respath+"/","PRAGMA cache_size = 100000");
  }
  rpa->gen.SetPilotRun(true);
  bool okay(true);
  for (size_t i=0;i<m_procs.size();++i) {
    m_procs[i]->SetLookUp(true);
    if (!m_procs[i]->CalculateTotalXSec(m_respath,false)) okay=false;
    m_procs[i]->SetLookUp(false);
    m_procs[i]->Integrator()->SetUpEnhance();
  }
  if (storeresults) My_In_File::CloseDB(m_respath+"/");
  rpa->gen.SetPilotRun(false);
  return okay;
}

void Matrix_Element_Handler::SetRandomSeed()
{
  if (m_seedmode==1) {
    m_ranidx=ran->ReadInStatus(*p_ranin,m_ranidx);
    if (m_ranidx==std::string::npos) {
      msg_Error()<<METHOD<<"(): Status file read error. Abort."<<std::endl;
      abort();
    }
  }
  else if (m_seedmode==2) {
    m_ranidx=ran->WriteOutStatus(*p_ranout,m_ranidx);
    if (m_ranidx==std::string::npos) {
      msg_Error()<<METHOD<<"(): Status file write error. Abort."<<std::endl;
      abort();
    }
  }
}

bool Matrix_Element_Handler::GenerateOneEvent() 
{
  DEBUG_FUNC("");
  Return_Value::IncCall(METHOD);
  p_proc=NULL;
  if (m_seedmode!=3) SetRandomSeed();
  p_isr->SetPDFMember();
  m_sum=0.0;
  for (size_t i(0);i<m_procs.size();++i)
    m_sum+=m_procs[i]->Integrator()->SelectionWeight(m_eventmode);
  for (size_t n(1);true;++n) {
    if (m_seedmode==3 && rpa->gen.NumberOfGeneratedEvents())
      ran->ResetToLastIncrementedSeed();
    double disc(m_sum*ran->Get()), csum(0.0);
    Process_Base *proc(NULL);
    for (size_t i(0);i<m_procs.size();++i) {
      if ((csum+=m_procs[i]->Integrator()->SelectionWeight(m_eventmode))>=disc) {
        proc=m_procs[i];
        break;
      }
    }
    if (proc==NULL) THROW(fatal_error,"No process selected");
    p_variationweights->Reset();
    proc->SetVariationWeights(NULL);
    const bool hasvars(
        p_variationweights->GetVariations()->GetParametersVector()->empty()
        == false);
    const double unweighting_r = ran->Get();
    if (m_pilotrunenabled && (hasvars || m_pilotrunrequired)) {
      // in pilot run mode, calculate nominal only, and prepare to restore the
      // rng to re-run with variations after unweighting
      ran->SaveStatus();
      rpa->gen.SetPilotRun(true);
      msg_Debugging()<<"Will do pilot run.\n";
    } else if (hasvars) {
      // in normal run mode, we immediately calculate all variations
      proc->SetVariationWeights(p_variationweights);
    }
    ATOOLS::Weight_Info *info=proc->OneEvent(m_eventmode);
    bool skip_rerun {false};
    if (!rpa->gen.PilotRun() && !hasvars) {
      // the process has opted out of the pilot run, so we can safely skip the
      // re-run (if any), as long as there are no variations to calculate in a
      // re-run
      if (m_pilotrunenabled && (hasvars || m_pilotrunrequired)) {
        msg_Debugging()<<"Pilot run has opted out of re-run.\n";
      }
      skip_rerun = true;
    }
    proc->SetVariationWeights(NULL);
    p_proc=proc->Selected();
    if (p_proc->Generator()==NULL)
      THROW(fatal_error,"No generator for process '"+p_proc->Name()+"'");
    if (p_proc->Generator()->MassMode()!=0)
      THROW(fatal_error,"Invalid mass mode. Check your PS interface.");
    double sw(p_proc->Integrator()->SelectionWeight(m_eventmode)/m_sum);
    if (info==NULL) {
      if (m_recalculate_zeros && !skip_rerun && m_haspilotscale) {
        // A pilot run with a PILOT scale has been vetoed. We can however not
        // conclude, that this also happens for the normal scale. Hence, we
        // repeat the calculation in non-pilot mode.
        ran->RestoreStatus();
        rpa->gen.SetPilotRun(false);
        msg_Debugging()<<"Pilot run has been vetoed. Re-calculate with normal settings.\n";
        info=proc->OneEvent(m_eventmode);
        p_proc=proc->Selected();
        if (!info) {
          // If also the normal scale leads to a rejection, we can safely go to
          // the next trial event.
          continue;
        }
        m_evtinfo=*info;
        // At this point, we only need another normal run below, after
        // unweighting, if there are any variations to evaluate. Otherwise, we
        // are done with the event generation, and need not other pass.
        if (!hasvars)
          skip_rerun = true;
      } else {
        continue;
      }
    } else {
      m_evtinfo=*info;
    }
    delete info;
    double enhance = p_proc->Integrator()->PSHandler()->Enhance();
    double wf((p_proc->NIn()==1?1.:rpa->Picobarn())/sw/enhance);
    if (m_eventmode!=0) {
      const double max = p_proc->Integrator()->Max();
      const double disc = max * unweighting_r;
      const double abswgt = std::abs(m_evtinfo.m_weight);
      if (abswgt < disc)
        continue;
      if (abswgt > max * m_ovwth) {
        Return_Value::IncWarning(METHOD);
        msg_Info() << METHOD<<"(): Point for '" << p_proc->Name()
                   << "' exceeds maximum by "
                   << abswgt / max - 1.0 << "." << std::endl;
        m_weightfactor = m_ovwth;
        wf *= max * m_ovwth / abswgt;
      } else {
        m_weightfactor = abswgt / max;
        wf /= Min(1.0, m_weightfactor);
      }
      if (skip_rerun) {
        msg_Debugging()<<"Pilot run has been accepted. Skip re-run.\n";
      }
      if (!skip_rerun && m_pilotrunenabled && (hasvars || m_pilotrunrequired)) {
        // re-run with same rng state and include the calculation of
        // variations this time
        ran->RestoreStatus();
        rpa->gen.SetPilotRun(false);
        msg_Debugging()<<"Pilot run has been accepted. Re-calculate with normal settings.\n";
        if (hasvars)
          proc->SetVariationWeights(p_variationweights);
        ATOOLS::Weight_Info *info=proc->OneEvent(m_eventmode);
        p_proc=proc->Selected();
        // if we have indeed used the same statistics for the (accepted) pilot
        // run, info must be non-null and it must contain the same results
        // compared to the pilot run (caveat: this is not necessarily
        // true, cases where the results might differ are the use of a
        // Pilot_Loop_Generator which does not exactly use the same EW
        // parameters, or the use of a Pilot Scale Setter)
        if (!info) {
          continue;
        }
        m_pilotweightfactor = info->m_weight/m_evtinfo.m_weight;
        m_evtinfo=*info;
        delete info;
        wf *= m_pilotweightfactor;
	m_evtinfo.m_weight /= m_pilotweightfactor;
        proc->SetVariationWeights(NULL);
      }
    }
    if (!hasvars) {
      p_variationweights->InitialiseWeights();
    }
    m_evtinfo.m_weight*=wf;
    if (p_proc->GetSubevtList()) {
      (*p_proc->GetSubevtList())*=wf;
      p_proc->GetSubevtList()->MultMEwgt(wf);
    }
    if (p_proc->GetMEwgtinfo()) (*p_proc->GetMEwgtinfo())*=wf;
    (*p_variationweights)*=wf;
    m_evtinfo.m_ntrial=n;
    return true;
  }
  return false;
}

std::vector<Process_Base*> Matrix_Element_Handler::InitializeProcess
(const Process_Info &pi,NLOTypeStringProcessMap_Map *&pmap)
{
  Process_Info cpi(pi);
  std::set<Process_Info> trials;
  std::vector<Process_Base*> procs;
  std::vector<Flavour_Vector> fls(pi.ExtractMPL());
  std::vector<int> fid(fls.size(),0);
  Flavour_Vector fl(fls.size());
  for (size_t i(0);i<fid.size();++i) fl[i]=fls[i][0];
  for (size_t hc(fid.size()-1);fid[0]<fls[0].size();) {
    if(fid[hc]==fls[hc].size()){fid[hc--]=0;++fid[hc];continue;}
    fl[hc]=fls[hc][fid[hc]];if(hc<fid.size()-1){++hc;continue;}
    Flavour_Vector cfl(fl);
    size_t n(0);
    cpi.m_ii.SetExternal(cfl,n);
    cpi.m_fi.SetExternal(cfl,n);
    Process_Base::SortFlavours(cpi,0);
    if (trials.find(cpi)==trials.end()) {
      trials.insert(cpi);
      std::vector<Process_Base*> cp=InitializeSingleProcess(cpi,pmap);
      procs.insert(procs.end(),cp.begin(),cp.end());
    }
    ++fid[hc];
  }
  return procs;
}

std::vector<Process_Base*> Matrix_Element_Handler::InitializeSingleProcess
(const Process_Info &pi,NLOTypeStringProcessMap_Map *&pmap)
{
  std::vector<Process_Base*> procs;
  if (pi.m_fi.NLOType()==nlo_type::lo) {
    Process_Base *proc(m_gens.InitializeProcess(pi, true));
    if (proc) {
      m_procs.push_back(proc);
      procs.push_back(proc);
      if (pmap==NULL) {
	m_pmaps.push_back(new NLOTypeStringProcessMap_Map());
	pmap=m_pmaps.back();
      }
      m_procs.back()->FillProcessMap(pmap);
    }
    return procs;
  }
  else {
    if (m_globalnlomode==3) {
      m_hasnlo=3;
      if (p_nlomc==NULL) InitNLOMC();
      if (pmap==NULL) {
	m_pmaps.push_back(new NLOTypeStringProcessMap_Map());
	pmap=m_pmaps.back();
      }
      MCatNLO_Process *proc=new MCatNLO_Process(m_gens,pmap);
      proc->Init(pi,p_beam,p_isr);
      if (!p_shower->GetShower())
        THROW(fatal_error,"Shower needs to be set for MC@NLO");
      proc->SetShower(p_shower->GetShower());
      proc->SetMCatNLO(p_nlomc);
      m_procs.push_back(proc);
      procs.push_back(proc);
      return procs;
    }
    else if (m_globalnlomode==1) {
      m_hasnlo=1;
      if (pi.m_fi.NLOType()&(nlo_type::vsub|nlo_type::loop|nlo_type::born)) {
	Process_Info rpi(pi);
	rpi.m_fi.SetNLOType(pi.m_fi.NLOType()&(nlo_type::vsub|nlo_type::loop|
					       nlo_type::born));
	procs.push_back(m_gens.InitializeProcess(rpi,true));
	if (procs.back()==NULL) {
	  msg_Error()<<"No such process:\n"<<rpi<<std::endl;
	  THROW(critical_error,"Failed to intialize process");
	}
      }
      if (pi.m_fi.NLOType()&nlo_type::real || pi.m_fi.NLOType()&nlo_type::rsub){
        // if real or rsub is requested, the extra jet is not yet contained
        // in the process info, but has to be added here
        Process_Info rpi(pi);
	rpi.m_fi.SetNLOType(pi.m_fi.NLOType()&(nlo_type::real|nlo_type::rsub));
	rpi.m_integrator=rpi.m_rsintegrator;
	rpi.m_megenerator=rpi.m_rsmegenerator;
	rpi.m_itmin=rpi.m_rsitmin;
	if (m_rsadd) {
	  if (pi.m_fi.m_nloqcdtype==nlo_type::lo) {
	    rpi.m_fi.m_ps.push_back(Subprocess_Info(kf_photon,"",""));
	  }
	  else if (pi.m_fi.m_nloewtype==nlo_type::lo) {
	    rpi.m_fi.m_ps.push_back(Subprocess_Info(kf_jet,"",""));
	  }
	}
        procs.push_back(m_gens.InitializeProcess(rpi,true));
	if (procs.back()==NULL) {
	  msg_Error()<<"No such process:\n"<<rpi<<std::endl;
	  THROW(critical_error,"Failed to intialize process");
	}
      }
      if (pmap==NULL) {
	m_pmaps.push_back(new NLOTypeStringProcessMap_Map());
	pmap=m_pmaps.back();
      }
      for (size_t i(0);i<procs.size();i++) {
	m_procs.push_back(procs[i]);
	m_procs.back()->FillProcessMap(pmap);
      }
      if (m_fosettings==0) {
	m_fosettings=1;
	if (p_shower->GetShower())
	  p_shower->GetShower()->SetOn(false);
	Read_Write_Base::AddCommandLine("FRAGMENTATION Off;");
	Read_Write_Base::AddCommandLine("ME_QED Off;");
	Read_Write_Base::AddCommandLine("MI_HANDLER None;");
	Data_Reader read(" ",";","!","=");
	read.AddComment("#");
	read.AddWordSeparator("\t");
	read.SetInputPath(m_path);
	read.SetInputFile(m_file);
	if (read.GetValue<int>("BEAM_REMNANTS",-1)==-1)
	  Read_Write_Base::AddCommandLine("BEAM_REMNANTS 0;");
	else {
	  Read_Write_Base::AddCommandLine("K_PERP_MEAN_1 0;");
	  Read_Write_Base::AddCommandLine("K_PERP_MEAN_2 0;");
	  Read_Write_Base::AddCommandLine("K_PERP_SIGMA_1 0;");
	  Read_Write_Base::AddCommandLine("K_PERP_SIGMA_2 0;");
	}
      }
    }
  }
  return procs;
}

int Matrix_Element_Handler::InitializeProcesses
(MODEL::Model_Base *model,
 BEAM::Beam_Spectra_Handler *beam,PDF::ISR_Handler *isr)
{
  /*
    This is the basis for all CKKW and process interplay.
    Don't even try to think about modifying 
    either this routine or any of its dependencies !!!
  */
  p_beam=beam; p_isr=isr; p_model=model;
  if (!m_gens.InitializeGenerators(model,beam,isr)) return false;
  Data_Reader reader(" ",";","!","=");
  int initonly=reader.GetValue<int>("INIT_ONLY",0);
  if (initonly&4) return 1;
  double rbtime(ATOOLS::rpa->gen.Timer().RealTime());
  double btime(ATOOLS::rpa->gen.Timer().UserTime());
#ifdef USING__MPI
  if (mpi->Rank()==0)
#endif
  MakeDir(rpa->gen.Variable("SHERPA_CPP_PATH")+"/Process",true);
  My_In_File::OpenDB(rpa->gen.Variable("SHERPA_CPP_PATH")+"/Process/Sherpa/");
  BuildProcesses();
  My_In_File::CloseDB(rpa->gen.Variable("SHERPA_CPP_PATH")+"/Process/Sherpa/");
  if (msg_LevelIsTracking()) msg_Info()<<"Process initialization";
  double retime(ATOOLS::rpa->gen.Timer().RealTime());
  double etime(ATOOLS::rpa->gen.Timer().UserTime());
  size_t rss(GetCurrentRSS());
  msg_Info()<<" done ( "<<rss/(1<<20)<<" MB, "
	    <<FormatTime(size_t(retime-rbtime))<<" / "
	    <<FormatTime(size_t(etime-btime))<<" )."<<std::endl;
  if (m_procs.empty() && m_gens.size()>0)
    THROW(normal_exit,"No hard process found");
  if (m_gens.NewLibraries())
    THROW(normal_exit,"Source code created. Run './makelibs' to compile.");
  msg_Info()<<METHOD<<"(): Performing tests "<<std::flush;
  rbtime=retime;
  btime=etime;
  int res(m_gens.PerformTests());
  retime=ATOOLS::rpa->gen.Timer().RealTime();
  etime=ATOOLS::rpa->gen.Timer().UserTime();
  rss=GetCurrentRSS();
  msg_Info()<<" done ( "<<rss/(1<<20)<<" MB, "
	    <<FormatTime(size_t(retime-rbtime))<<" / "
	    <<FormatTime(size_t(etime-btime))<<" )."<<std::endl;
  msg_Debugging()<<METHOD<<"(): Processes {\n";
  msg_Debugging()<<"  m_procs:\n";
  for (size_t i(0);i<m_procs.size();++i) 
    msg_Debugging()<<"    "<<m_procs[i]->Name()<<" -> "<<m_procs[i]<<"\n";
  msg_Debugging()<<"}\n";
  return res;
}

void Matrix_Element_Handler::BuildProcesses()
{
  Data_Reader read(" ",";","!","=");
  read.AddComment("#");
  read.AddWordSeparator("\t");
  read.SetInputPath(m_path);
  read.SetInputFile(m_file);
  // set color scheme
  read.SetTags(cls::ColorSchemeTags());
  cls::scheme cls((cls::scheme)read.GetValue<int>("COLOUR_SCHEME",0));
  read.SetTags(std::map<std::string,std::string>());
  // set helicity scheme
  read.SetTags(hls::HelicitySchemeTags());
  hls::scheme hls((hls::scheme)read.GetValue<int>("HELICITY_SCHEME",1));
  read.SetTags(std::map<std::string,std::string>());
  // set kfactor scheme
  std::string kfactor=read.GetValue<std::string>("KFACTOR","NO");
  // set scale scheme
  std::string scale=read.GetValue<std::string>
    ("SCALES","STRICT_METS{MU_F2}{MU_R2}{MU_Q2}");
  std::vector<std::string> helpsv;
  if (!read.VectorFromFile(helpsv,"COUPLINGS"))
    helpsv.push_back("Alpha_QCD 1");
  std::string coupling(MakeString(helpsv,0));
  if (scale.find("PILOT") == 0) {
    m_haspilotscale = true;
    m_pilotrunrequired = true;
  }
  // init processes
  msg_Info()<<METHOD<<"(): Looking for processes "<<std::flush;
  if (msg_LevelIsTracking()) msg_Info()<<"\n";
  std::vector<std::vector<std::string> > procdata;
  Data_Reader pread(" ",";","%",":");
  pread.AddComment("#");
  pread.AddWordSeparator("\t");
  pread.SetAddCommandLine(false);
  pread.SetInputPath(m_path);
  pread.SetInputFile(m_processfile);
  if (m_gens.size()>0 && !pread.MatrixFromFile(procdata,""))
    THROW(missing_input,"No data in "+m_path+m_processfile+"'.");
  for (size_t nf(0);nf<procdata.size();++nf) {
    std::vector<std::string> &cur(procdata[nf]);
    if(cur.size()==0) continue;
    if(cur[0].find("Order(")!=std::string::npos)
      THROW(fatal_error,
	    std::string("Syntax error in coupling order specification: '"+cur[0]+"'\n")
	    +"   Whitespace between 'Order' and brackets is mandatory");
    if (cur.size()<2) continue;
    if (cur[0]=="Process") {
      Process_Info pi;
      pi.m_scale=scale;
      pi.m_coupling=coupling;
      pi.m_kfactor=kfactor;
      pi.m_cls=cls;
      pi.m_hls=hls;
      std::string proc(MakeString(cur,1));
      size_t pos(proc.find("->"));
      if (pos==std::string::npos) continue;
      Processblock_Info pbi;
      std::string ini(proc.substr(0,pos));
      std::string fin(proc.substr(pos+2));
      std::vector<std::string> dectags;
      for (size_t ng(nf);ng<procdata.size();++ng) {
	std::vector<std::string> &cur(procdata[ng]);
	if (cur.size()<2) continue;
	if (cur[0]=="Decay") dectags.push_back(MakeString(cur,1));
	if (cur[0]=="DecayOS") dectags.push_back("Z"+MakeString(cur,1));
	if (cur[0]=="No_Decay")
	  for (size_t i(1);i<cur.size();++i) {
	    long int kfc(ToType<long int>(cur[i]));
	    pi.m_nodecs.push_back(Flavour(std::abs(kfc),kfc<0));
	  }
	if (cur[0]=="Order") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vcpl,nf);
        }
	if (cur[0]=="Max_Order") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vmaxcpl,nf);
	}
	if (cur[0]=="Min_Order") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vmincpl,nf);
	}
	if (cur[0]=="Amplitude_Order") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vacpl,nf);
        }
	if (cur[0]=="Max_Amplitude_Order") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vmaxacpl,nf);
	}
	if (cur[0]=="Min_Amplitude_Order") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vminacpl,nf);
	}
	if (cur[0]=="Order_EW") {
          THROW(fatal_error,
                std::string("You are using the obsolete setting:\n")
                +"    'Order_EW "+MakeString(cur,1)+"'\n"+
                +"  Please refer to the Sherpa manual for how to transition "
                +"to the new 'Order (<qcd>, <ew>[, ...])' syntax, e.g. using:\n"
                +"    'Order (*,"+MakeString(cur,1)+")'");
        }
	if (cur[0]=="Max_Order_EW" ||
            cur[0]=="Max_Order_QCD" ||
            cur[0]=="Order_QCD") {
          THROW(fatal_error,
                std::string("You are using an obsolete (Max_)Order_* setting) ")
                +"in your processes section. Please refer to the Sherpa manual "
                +"for how to transition to the new "
                +"'Order (<qcd>, <ew>[, ...])' syntax.");
        }
	if (cur[0]=="Cut_Core") pbi.m_cutcore=ToType<int>(cur[1]);
	if (cur[0]=="CKKW") {
	  if (p_shower==NULL || p_shower->GetShower()==NULL)
	    THROW(fatal_error,"Invalid shower generator");
	  pi.m_ckkw=1;
	  pbi.m_gycut=cur[1];
	}
	if (cur[0]=="Scales") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vscale,nf);
          if (cb.find("PILOT") == 0) {
            m_haspilotscale = true;
            m_pilotrunrequired = true;
          }
	}
	if (cur[0]=="Couplings") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vcoupl,nf);
	}
	if (cur[0]=="KFactor") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vkfac,nf);
	}
	if (cur[0]=="Y_Cut") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vycut,nf);
	}
	if (cur[0]=="Selector_File") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vsfile,nf);
	}
	if (cur[0]=="Min_N_Quarks") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vnminq,nf);
	}
	if (cur[0]=="Max_N_Quarks") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vnmaxq,nf);
	}
	if (cur[0]=="Print_Graphs") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vgpath,nf);
	}
	if (cur[0]=="Name_Suffix") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vaddname,nf);
	}
	if (cur[0]=="Enable_MHV") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vamegicmhv,nf);
	}
	if (cur[0]=="Min_N_TChannels") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vntchanmin,nf);
	}
	if (cur[0]=="Max_N_TChannels") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vntchanmax,nf);
	}
	if (cur[0]=="Integration_Error") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vmaxerr,nf);
	}
	if (cur[0]=="Max_Epsilon") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vmaxeps,nf);
	}
	if (cur[0]=="RS_Enhance_Factor") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vrsefac,nf);
	}
	if (cur[0]=="Enhance_Factor") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vefac,nf);
	}
	if (cur[0]=="Enhance_Function") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vefunc,nf);
	}
	if (cur[0]=="Enhance_Observable") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_veobs,nf);
	}
	if (cur[0]=="NLO_QCD_Mode") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vnloqcdmode,nf);
	}
	if (cur[0]=="NLO_QCD_Part") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vnloqcdpart,nf);
	}
	if (cur[0]=="NLO_EW_Mode") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vnloewmode,nf);
	}
	if (cur[0]=="NLO_EW_Part") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vnloewpart,nf);
	}
	if (cur[0]=="Subdivide_Virtual") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vnlosubv,nf);
	}
	if (cur[0]=="Associated_Contributions") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vasscontribs,nf);
	}
	if (cur[0]=="ME_Generator") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vmegen,nf);
	}
	if (cur[0]=="RS_ME_Generator") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vrsmegen,nf);
	}
	if (cur[0]=="Pilot_Loop_Generator") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vloopgen_unwt,nf);
          m_pilotrunrequired = true;
        }
	if (cur[0]=="Loop_Generator") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vloopgen,nf);
	}
	if (cur[0]=="Integrator") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vint,nf);
	}
	if (cur[0]=="RS_Integrator") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vrsint,nf);
	}
	if (cur[0]=="PSI_ItMin") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vitmin,nf);
        }
	if (cur[0]=="RS_PSI_ItMin") {
	  std::string cb(MakeString(cur,1));
	  ExtractMPvalues(cb,pbi.m_vrsitmin,nf);
        }
        pi.p_gens=&m_gens;
	if (cur[0]=="End" && cur[1]=="process") break;
      }
      BuildSingleProcessList(pi,pbi,ini,fin,dectags);
      if (msg_LevelIsDebugging()) {
        msg_Indentation(4);
        msg_Out()<<m_procs.size()<<" process(es) found ..."<<std::endl;
        for (unsigned int i=0; i<m_procs.size(); ++i) {
          msg_Out()<<m_procs[i]->Name();
          if (m_procs[i]->IsGroup())
            msg_Out()<<" has subprocesses ...";
          msg_Out()<<std::endl;
        }
      }
    }
  }
  if (m_pilotrunrequired && !m_pilotrunenabled) {
    msg_Out()
        << "WARNING: The use of a PILOT scale setting or an Unweighting Loop\n"
        << "Generator requires the use of a pilot run, but it could not be\n"
        << "enabled, either because the RNG does not support resetting its\n"
        << "state, or because a weighted event generation mode is being used.\n";
  }
}

void Matrix_Element_Handler::BuildDecays
(Subprocess_Info &ACFS,const std::vector<std::string> &dectags)
{
  for (size_t i(0);i<dectags.size();++i) {
    std::string dec(dectags[i]);
    int osf=0;
    if (dec[0]=='Z') {
      dec=dec.substr(1);
      osf=1;
    }
    size_t pos(dec.find("->"));
    if (pos==std::string::npos) continue;
    Subprocess_Info ACDIS, ACDFS;
    std::string ini(dec.substr(0,pos));
    std::string fin(dec.substr(pos+2));
    ExtractFlavours(ACDIS,ini);
    ExtractFlavours(ACDFS,fin);
    if (ACDIS.m_ps.empty() || ACDFS.m_ps.empty())
      THROW(fatal_error,"Wrong decay specification");
    Subprocess_Info &CIS(ACDIS.m_ps.front());
    size_t oldsize(ACFS.m_ps.size()), cdsize(ACDFS.m_ps.size());
    ACFS.m_ps.resize(oldsize*cdsize);
    for (size_t cfss(1);cfss<cdsize;++cfss)
      for (size_t acfsi(0);acfsi<oldsize;++acfsi)
	ACFS.m_ps[cfss*oldsize+acfsi]=
	  ACFS.m_ps[(cfss-1)*oldsize+acfsi];
    for (size_t acfsi(0);acfsi<oldsize;++acfsi) {
      for (size_t cfss(0);cfss<cdsize;++cfss) {
	Subprocess_Info &CFS(ACDFS.m_ps[cfss]);
	msg_Debugging()<<METHOD<<"(): Init decay {\n"<<CIS<<CFS<<"}\n";
	if (CIS.NExternal()!=1 || CFS.NExternal()<2)
	  THROW(fatal_error,"Wrong number of particles in decay");
	if (!ACFS.m_ps[cfss*oldsize+acfsi].AddDecay(CIS,CFS,osf))
	  THROW(fatal_error,"No match for decay "+dec);
      }
    }
  }
}

void Matrix_Element_Handler::LimitCouplings
(MPSV_Map &pbi,const size_t &nfs,const std::string &pnid,
 std::vector<double> &mincpl,std::vector<double> &maxcpl,const int mode)
{
  std::string ds;
  if (!GetMPvalue(pbi,nfs,pnid,ds)) return;
  while (ds.find("*")!=std::string::npos) ds.replace(ds.find("*"),1,"-1");
  Data_Reader read(",",";",")","(");
  read.SetString(ds);
  std::vector<double> cpl;
  read.VectorFromString(cpl,"");
  if (mode&1) {
    if (cpl.size()>mincpl.size()) mincpl.resize(cpl.size(),0);
    for (size_t i(0);i<mincpl.size();++i)
      if (cpl[i]>=0 && cpl[i]>mincpl[i]) mincpl[i]=cpl[i];
  }
  if (mode&2) {
    if (cpl.size()>maxcpl.size()) maxcpl.resize(cpl.size(),99);
    for (size_t i(0);i<maxcpl.size();++i)
      if (cpl[i]>=0 && cpl[i]<maxcpl[i]) maxcpl[i]=cpl[i];
  }
}

void Matrix_Element_Handler::BuildSingleProcessList
(Process_Info &pi,Processblock_Info &pbi,
 const std::string &ini,const std::string &fin,
 const std::vector<std::string> &dectags)
{
  Subprocess_Info AIS, AFS;
  ExtractFlavours(AIS,ini);
  ExtractFlavours(AFS,fin);
  std::vector<Process_Base*> procs;
  NLOTypeStringProcessMap_Map *pmap(NULL);
  for (size_t fss(0);fss<AFS.m_ps.size();++fss) {
    Subprocess_Info ACFS;
    ACFS.m_ps.push_back(AFS.m_ps[fss]);
    BuildDecays(ACFS,dectags);
    for (size_t afsi(0);afsi<ACFS.m_ps.size();++afsi) {
      msg_Debugging()<<METHOD<<"(): Check N_max ("
		     <<fss<<"): {\n"<<ACFS.m_ps[afsi]<<"}\n";
      pi.m_fi.GetNMax(ACFS.m_ps[afsi]);
    }
  }
  for (size_t iss(0);iss<AIS.m_ps.size();++iss) {
    Subprocess_Info &IS(AIS.m_ps[iss]);
    for (size_t fss(0);fss<AFS.m_ps.size();++fss) {
      Subprocess_Info &FS(AFS.m_ps[fss]);
      msg_Debugging()<<METHOD<<"(): Init core ("<<iss<<","
		     <<fss<<"): {\n"<<IS<<FS<<"}\n";
      std::vector<Flavour> flavs;
      IS.GetExternal(flavs);
      if (flavs.size()>1) {
        if (!p_isr->CheckConsistency(&flavs.front())) {
          msg_Error()<<METHOD<<"(): Error in initialising ISR ("
                     <<p_isr->Flav(0)<<" -> "<<flavs[0]<<") x ("
                     <<p_isr->Flav(1)<<" -> "<<flavs[1]
                     <<"). Ignoring process."<<std::endl;
          continue;
        }
      }
      Subprocess_Info ACFS;
      ACFS.m_ps.push_back(FS);
      BuildDecays(ACFS,dectags);
      for (size_t afsi(0);afsi<ACFS.m_ps.size();++afsi) {
	Subprocess_Info &CFS(ACFS.m_ps[afsi]);
	msg_Debugging()<<METHOD<<"(): Init process ("<<iss<<","
		       <<fss<<"): {\n"<<IS<<CFS<<"}\n";
	std::vector<Flavour> flavs;
	IS.GetExternal(flavs);
	CFS.GetExternal(flavs);
	size_t nis(IS.NExternal()), nfs(CFS.NExternal());
	double inisum=0.0, finsum=0.0, dd(0.0);
	for (size_t i(0);i<nis;++i) inisum+=flavs[i].Mass();
	for (size_t i(0);i<nfs;++i) finsum+=flavs[i+nis].Mass();
	if (inisum>rpa->gen.Ecms() || finsum>rpa->gen.Ecms()) continue;
	std::string pnid(CFS.MultiplicityTag()), ds;
	int di;
	Process_Info cpi(pi);
	cpi.m_ii=IS;
	cpi.m_fi=CFS;
	cpi.m_fi.m_nloqcdtype=pi.m_fi.m_nloqcdtype;
	cpi.m_fi.m_nloewtype=pi.m_fi.m_nloewtype;
	cpi.m_fi.SetNMax(pi.m_fi);
	LimitCouplings(pbi.m_vmincpl,nfs,pnid,cpi.m_mincpl,cpi.m_maxcpl,1);
	LimitCouplings(pbi.m_vmaxcpl,nfs,pnid,cpi.m_mincpl,cpi.m_maxcpl,2);
	LimitCouplings(pbi.m_vcpl,nfs,pnid,cpi.m_mincpl,cpi.m_maxcpl,3);
	LimitCouplings(pbi.m_vminacpl,nfs,pnid,cpi.m_minacpl,cpi.m_maxacpl,1);
	LimitCouplings(pbi.m_vmaxacpl,nfs,pnid,cpi.m_minacpl,cpi.m_maxacpl,2);
	LimitCouplings(pbi.m_vacpl,nfs,pnid,cpi.m_minacpl,cpi.m_maxacpl,3);
	size_t maxsize(Min(cpi.m_mincpl.size(),cpi.m_maxcpl.size()));
	for (size_t i(0);i<maxsize;++i)
	  if (cpi.m_mincpl[i]>cpi.m_maxcpl[i]) {
	    msg_Error()<<METHOD<<"(): Invalid coupling orders: "
		       <<cpi.m_mincpl<<" .. "<<cpi.m_maxcpl<<"\n";
	    THROW(inconsistent_option,"Please correct coupling orders");
	  }
	if (GetMPvalue(pbi.m_vscale,nfs,pnid,ds)) cpi.m_scale=ds;
	if (GetMPvalue(pbi.m_vcoupl,nfs,pnid,ds)) cpi.m_coupling=ds;
	if (GetMPvalue(pbi.m_vkfac,nfs,pnid,ds)) cpi.m_kfactor=ds;
	if (GetMPvalue(pbi.m_vsfile,nfs,pnid,ds)) cpi.m_selectorfile=ds;
	if (GetMPvalue(pbi.m_vnmaxq,nfs,pnid,di)) cpi.m_nmaxq=di;
	if (GetMPvalue(pbi.m_vnminq,nfs,pnid,di)) cpi.m_nminq=di;
	if (GetMPvalue(pbi.m_vamegicmhv,nfs,pnid,di)) cpi.m_amegicmhv=di;
	if (GetMPvalue(pbi.m_vntchanmin,nfs,pnid,di)) cpi.m_ntchanmin=di;
	if (GetMPvalue(pbi.m_vntchanmax,nfs,pnid,di)) cpi.m_ntchanmax=di;
	if (GetMPvalue(pbi.m_vgpath,nfs,pnid,ds)) cpi.m_gpath=ds;
	if (GetMPvalue(pbi.m_vaddname,nfs,pnid,ds)) cpi.m_addname=ds;
	if (GetMPvalue(pbi.m_vnloqcdmode,nfs,pnid,ds)) {
	  if      (ds=="Fixed_Order" || ds=="1") pi.m_nlomode=cpi.m_nlomode=1;
	  else if (ds=="MC@NLO"      || ds=="3") pi.m_nlomode=cpi.m_nlomode=3;
	  else THROW(fatal_error,"Unknown NLO_QCD_Mode "+ds+" {"+pnid+"}");
	  cpi.m_fi.m_nloqcdtype=ToType<nlo_type::code>("BVIRS");
	  if (!m_globalnlomode) m_globalnlomode=cpi.m_nlomode;
	  if (cpi.m_nlomode!=m_globalnlomode)
	    THROW(fatal_error,"Unable to process multiple NLO modes at the "
			      "same time");
	}
	if (GetMPvalue(pbi.m_vnloqcdpart,nfs,pnid,ds)) {
	  cpi.m_fi.m_nloqcdtype=ToType<nlo_type::code>(ds);
	  if (cpi.m_nlomode==0) pi.m_nlomode=cpi.m_nlomode=m_globalnlomode;
	}
	if (GetMPvalue(pbi.m_vnloewmode,nfs,pnid,ds)) {
	  if      (ds=="Fixed_Order" || ds=="1") pi.m_nlomode=cpi.m_nlomode=1;
	  else if (ds=="MC@NLO"      || ds=="3") pi.m_nlomode=cpi.m_nlomode=3;
	  else THROW(fatal_error,"Unknown NLO_EW_Mode "+ds+" {"+pnid+"}");
	  cpi.m_fi.m_nloewtype=ToType<nlo_type::code>("BVIRS");
	  if (!m_globalnlomode) m_globalnlomode=cpi.m_nlomode;
	  if (cpi.m_nlomode!=m_globalnlomode)
	    THROW(fatal_error,"Unable to process multiple NLO modes at the "
			      "same time");
	}
	if (GetMPvalue(pbi.m_vnloewpart,nfs,pnid,ds)) {
	  cpi.m_fi.m_nloewtype=ToType<nlo_type::code>(ds);
	  if (cpi.m_nlomode==0) pi.m_nlomode=cpi.m_nlomode=m_globalnlomode;
	}
	if (GetMPvalue(pbi.m_vnlosubv,nfs,pnid,ds)) cpi.m_fi.m_sv=ds;
	if (GetMPvalue(pbi.m_vasscontribs,nfs,pnid,ds))
	  cpi.m_fi.m_asscontribs=ToType<asscontrib::type>(ds);
	if (GetMPvalue(pbi.m_vmegen,nfs,pnid,ds)) cpi.m_megenerator=ds;
	if (GetMPvalue(pbi.m_vrsmegen,nfs,pnid,ds)) cpi.m_rsmegenerator=ds;
	else cpi.m_rsmegenerator=cpi.m_megenerator;
	if (GetMPvalue(pbi.m_vloopgen,nfs,pnid,ds)) {
	  m_gens.LoadGenerator(ds);
	  cpi.m_loopgenerator=ds;
	}
	if (GetMPvalue(pbi.m_vloopgen_unwt,nfs,pnid,ds)) {
	  m_gens.LoadGenerator(ds);
	  cpi.m_loopgenerator_unwt=ds;
	}
  else cpi.m_loopgenerator_unwt = cpi.m_loopgenerator;
	if (GetMPvalue(pbi.m_vint,nfs,pnid,ds)) cpi.m_integrator=ds;
	if (GetMPvalue(pbi.m_vrsint,nfs,pnid,ds)) cpi.m_rsintegrator=ds;
	else cpi.m_rsintegrator=cpi.m_integrator;
	if (GetMPvalue(pbi.m_vitmin,nfs,pnid,di)) cpi.m_itmin=di;
	if (GetMPvalue(pbi.m_vrsitmin,nfs,pnid,di)) cpi.m_rsitmin=di;
	else cpi.m_rsitmin=cpi.m_itmin;
	std::vector<Process_Base*> proc=InitializeProcess(cpi,pmap);
	for (size_t i(0);i<proc.size();i++) {
	  if (proc[i]==NULL)
	    msg_Error()<<METHOD<<"(): No process for {\n"
		       <<cpi<<"\n}"<<std::endl;
	  procs.push_back(proc[i]);
	  proc[i]->Integrator()->
	    SetISRThreshold(ATOOLS::Max(inisum,finsum));
	  if (GetMPvalue(pbi.m_vefac,nfs,pnid,dd))
	    proc[i]->Integrator()->SetEnhanceFactor(dd);
	  if (GetMPvalue(pbi.m_vmaxeps,nfs,pnid,dd))
	    proc[i]->Integrator()->SetMaxEpsilon(dd);
	  else proc[i]->Integrator()->SetMaxEpsilon(1.0e-3);
	  if (GetMPvalue(pbi.m_vrsefac,nfs,pnid,dd))
	    proc[i]->Integrator()->SetRSEnhanceFactor(dd);
	  double maxerr(-1.0);
	  std::string eobs, efunc;
	  if (GetMPvalue(pbi.m_vmaxerr,nfs,pnid,dd)) maxerr=dd;
	  if (GetMPvalue(pbi.m_veobs,nfs,pnid,ds)) eobs=ds;
	  if (GetMPvalue(pbi.m_vefunc,nfs,pnid,ds)) efunc=ds;
	  proc[i]->InitPSHandler(maxerr,eobs,efunc);
	  proc[i]->SetShower(p_shower->GetShower());
	}
      }
    }
  }
  for (size_t i(0);i<procs.size();++i) {
    Process_Info &cpi(procs[i]->Info());
    Selector_Key skey(NULL,new Data_Reader(),true);
    std::string sfile(cpi.m_selectorfile!=""?
		      cpi.m_selectorfile:m_selectorfile);
    size_t ftp(sfile.find("*"));
    if (ftp!=std::string::npos) sfile.replace
      (ftp,1,m_processfile.substr(0,m_processfile.find('|')));
    skey.ReadData(m_path,sfile);
    if (pi.m_ckkw&1) {
      std::vector<std::string> jfargs(1,pbi.m_gycut);
      GetMPvalue(pbi.m_vycut,cpi.m_fi.NExternal(),
		 cpi.m_fi.MultiplicityTag(),jfargs[0]);
      if (i==0) {
	jfargs.push_back("LO");
	if (pbi.m_cutcore==true) {
	  jfargs.push_back("CUT");
	}
      }
      skey.SetData("METS",jfargs);
    }
    procs[i]->SetSelector(skey);
    procs[i]->SetScale
      (Scale_Setter_Arguments(p_model,cpi.m_scale,cpi.m_coupling));
    procs[i]->SetKFactor
      (KFactor_Setter_Arguments(cpi.m_kfactor));
  }
}

size_t Matrix_Element_Handler::ExtractFlavours(Subprocess_Info &info,std::string buffer)
{
  info.m_ps.resize(1);
  info.m_ps.front().m_ps.clear();
  while(true) {
    while (buffer.length()>0 && 
	   (buffer[0]==' ' || buffer[0]=='\t')) buffer.erase(0,1);
    if (buffer.length()==0) break;
    size_t pos(Min(buffer.find(' '),buffer.length()));
    std::string cur(buffer.substr(0,pos));
    buffer=buffer.substr(pos);
    pos=cur.find('(');
    std::string polid, mpl, rem;
    if (pos!=std::string::npos) {
      polid=cur.substr(pos);
      rem=polid.substr(polid.find(')')+1);
      cur=cur.substr(0,pos);
      polid=polid.substr(1,polid.find(')')-1);
    }
    if (cur.length()==0 && polid.length()) {
      cur="0"+rem;
      mpl=polid;
      polid="";
    }
    pos=cur.find('[');
    std::string decid;
    if (pos!=std::string::npos) {
      decid=cur.substr(pos);
      cur=cur.substr(0,pos);
      decid=decid.substr(1,decid.find(']')-1);
    }
    int n(-1);
    pos=cur.find('{');
    if (pos!=std::string::npos) {
      std::string nid(cur.substr(pos));
      cur=cur.substr(0,pos);
      n=ToType<size_t>(nid.substr(1,nid.find('}')-1));
    }
    int kfc(ToType<int>(cur));
    Flavour cfl((kf_code)abs(kfc));
    if (kfc<0) cfl=cfl.Bar();
    if (n==-1) {
      for (size_t i(0);i<info.m_ps.size();++i)
	info.m_ps[i].m_ps.push_back(Subprocess_Info(cfl,decid,polid,mpl));
    }
    else {
      size_t oldsize(info.m_ps.size());
      info.m_ps.resize(oldsize*(n+1));
      for (int j(1);j<=n;++j) {
	for (size_t i(0);i<oldsize;++i) {
	  info.m_ps[j*oldsize+i]=info.m_ps[(j-1)*oldsize+i];
	  info.m_ps[j*oldsize+i].m_ps.push_back(Subprocess_Info(cfl,"",polid,mpl));
	}
      }
    }
  }
  return info.m_ps.back().m_ps.size();
}

namespace SHERPA {

  template <> int Matrix_Element_Handler::ExtractMPvalue(const std::string& str)
  {
    return ToType<int>(str);
  }

  template <> double Matrix_Element_Handler::ExtractMPvalue(const std::string& str)
  {
    Algebra_Interpreter inter;
    inter.AddTag("E_CMS",ToString(rpa->gen.Ecms()));
    return ToType<double>(inter.Interprete(str));
  }

  template <> std::string Matrix_Element_Handler::ExtractMPvalue(const std::string& str)
  {
    return str;
  }

  template <typename Type>
  void Matrix_Element_Handler::AddMPvalue(std::string lstr,std::string rstr,const Type &val,
			  std::map<std::string,std::pair<int,Type> >& dv,
			  const int nfs,const int &priority)
  {
    if (rstr.length()==0) {
      if (nfs==0 && 
	  (dv.find(lstr)==dv.end() || dv[lstr].first>priority)) {
	msg_Debugging()<<METHOD<<"(): adding '"<<val
		       <<"' {"<<lstr<<"}("<<priority<<")\n";
	dv[lstr]=std::pair<int,Type>(priority,val);
      }
      return;
    }
    size_t pos(rstr.find('-')), ltp(rstr.find('['));
    if (pos==std::string::npos || ltp<pos-1) {
      if (ltp!=std::string::npos) {
	size_t rtp(rstr.find(']',ltp));
	AddMPvalue(lstr+rstr.substr(0,rtp+1),rstr.substr(rtp+1),val,dv,
		   nfs-ToType<int>(rstr.substr(ltp+1,rtp-ltp-1)),priority);
	return;
      }
      AddMPvalue(lstr+rstr,"",val,dv,nfs-ToType<int>(rstr),priority);
      return;
    }
    std::string rlstr(rstr.substr(0,pos)), rrstr(rstr.substr(pos+1)), rmstr;
    if (pos>0 && ltp==pos-1) {
      rmstr="]";
      rrstr=rrstr.substr(1);
    }
    for (int i(0);i<=nfs;++i)
      AddMPvalue(lstr+rlstr+ToString(i)+rmstr,rrstr,val,dv,nfs-i,priority);
  }

  template void Matrix_Element_Handler::AddMPvalue
  (std::string lstr,std::string rstr,const double &val,
   std::map<std::string,std::pair<int,double> >& dv,
   const int nfs,const int &priority);
  template void Matrix_Element_Handler::AddMPvalue
  (std::string lstr,std::string rstr,const std::string &val,
   std::map<std::string,std::pair<int,std::string> >& dv,
   const int nfs,const int &priority);

  template <typename Type>
  bool Matrix_Element_Handler::GetMPvalue
  (std::map<std::string,std::pair<int,Type> >& dv,
   const int nfs,const std::string &pnid,Type &rv)
  {
    std::map<std::string,std::pair<int,Type> > cdv(dv);
    for (typename std::map<std::string,std::pair<int,Type> >::const_iterator 
	   dit(dv.begin());dit!=dv.end();++dit) { 
      AddMPvalue<Type>("",dit->first,dit->second.second,
		       dv,nfs,dit->second.first);
    }
    if (dv.find(pnid)!=dv.end()) {
      rv=dv[pnid].second;
      return true;
    }
    std::string nfstag(ToString(nfs));
    if (dv.find(nfstag)!=dv.end()) {
      rv=dv[nfstag].second;
      return true;
    }
    return false;
  }

  template bool Matrix_Element_Handler::GetMPvalue
  (std::map<std::string,std::pair<int,int> >& dv,
   const int nfs,const std::string &pnid,int &rv);
  template bool Matrix_Element_Handler::GetMPvalue
  (std::map<std::string,std::pair<int,double> >& dv,
   const int nfs,const std::string &pnid,double &rv);
  template bool Matrix_Element_Handler::GetMPvalue
  (std::map<std::string,std::pair<int,std::string> >& dv,
   const int nfs,const std::string &pnid,std::string &rv);

  template <typename Type>
  void Matrix_Element_Handler::ExtractMPvalues(std::string& str,std::map
			       <std::string,std::pair<int,Type> >& dv,
			       const int &priority)
  {
    int position;
    position = str.find("{");
    while (position>0 && str[position-1]!=' ' && str[position-1]!='\t')
      position=str.find('{',position+1);
    if (position==-1) {
      dv["-"]=std::pair<int,Type>(priority,ExtractMPvalue<Type>(str));
      msg_Debugging()<<METHOD<<"(): adding '"<<str<<"'("<<dv["-"].second
		     <<") {-}("<<priority<<")\n";
      return;
    }
    std::string hstr = str.substr(0,position);
    for (size_t hl=hstr.length();hl && 
	   (hstr[hl-1]==' ' || hstr[hl-1]=='\t');
	 hl=hstr.length()) hstr.erase(hl-1);
    Type value = ExtractMPvalue<Type>(hstr);
    str = str.substr(position+1,str.length()-position-2);
    do {
      position = str.find(",");
      if (position>-1) {
	hstr = str.substr(0,position);
	str = str.substr(position+1);
      }
      else hstr=str;
      if (hstr.length()>0) {
	dv[hstr]=std::pair<int,Type>(priority,value);
	msg_Debugging()<<METHOD<<"(): adding '"<<value<<"'("<<dv[hstr].second
		       <<") {"<<hstr<<"}("<<priority<<")\n";
      }
    } while (position>-1);
  }

  template void Matrix_Element_Handler::ExtractMPvalues
  (std::string& str,std::map<std::string,std::pair<int,double> >& dv,
   const int &priority);
  template void Matrix_Element_Handler::ExtractMPvalues
  (std::string& str,std::map<std::string,std::pair<int,std::string> >& dv,
   const int &priority);

}

std::string Matrix_Element_Handler::MakeString
(const std::vector<std::string> &in,const size_t &first)
{
  std::string out(in.size()>first?in[first]:"");
  for (size_t i(first+1);i<in.size();++i) out+=" "+in[i];
  return out;
}

double Matrix_Element_Handler::GetWeight
(const Cluster_Amplitude &ampl,
 const nlo_type::code type,const int mode) const
{
  std::string name(Process_Base::GenerateName(&ampl));
  for (int i(0);i<m_pmaps.size();++i) {
    StringProcess_Map::const_iterator pit
      (m_pmaps[i]->find(type)->second->find(name));
    if(pit==m_pmaps[i]->find(type)->second->end()) continue;
    SP(Color_Integrator) ci(pit->second->Integrator()->ColorIntegrator());
    if (ci!=NULL) {
      ci->GeneratePoint();
      for (size_t j(0);j<ampl.Legs().size();++j)
	ampl.Leg(j)->SetCol(ColorID(ci->I()[j],ci->J()[j]));
      if (mode&1) ci->SetWOn(false);
      double res(pit->second->Differential(ampl));
      ci->SetWOn(true);
      return res;
    }
  }
  return 0.0;
}

