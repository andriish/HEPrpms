/*
   MC_Initialize, MC_Analyze and MC_Finalize implementation
*/
#include "Generate.h"
#include "Setup.H"
#include "TDecayMode.H"
#include "UserTreeAnalysis.H"
#include "HEPParticle.H"
#include "HEPEvent.H"
#include "HEPEVTEvent.H"
#include "PYJETSEvent.H"
#include "LUJETSEvent.H"
#include "HerwigEvent.H"
#include "GenerationDescription.H"
#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <unistd.h>
#include <assert.h>
#include "MCTesterEvent.H"
#include <THEPParticle.H>

#include "UserEventAnalysis.H"
#include "UserTreeAnalysis.H"
// ROOT-specific include files

#include "TFile.h"
#include "TInterpreter.h"
#include "TROOT.h"
#include "TMethodCall.h"
#include "TSystem.h"

// global variables
char myfname[2048];
char decaymodes[2000][100];
int n_decaymodes=0;


GenerationDescription *setup_copy=0;


TMethodCall* userTreeAnalysis=0;


void MC_Initialize()
{
    
    

    if (!gInterpreter) {
	printf("FATAL ERROR: root not initialized\n");
	exit(-1);
    }

    if (Setup::stage==0) Setup::stage=1;

    // now try to open SETUP file.
    FILE *f=fopen("SETUP.C","r");
    if (f) {
	fclose(f);
	gInterpreter->ExecuteMacro("SETUP.C");
    } else {
	printf("\n\n");
	printf("	#############################\n");
	printf("	#                           #\n");
	printf("	# WARNING! NO SETUP.C file. #\n");
	printf("	#                           #\n");
	printf("	#############################\n");
	printf("\n");
    }

    printf("\n");
    printf(" *************************************\n");
    printf(" *        MC-TESTER v1.25.1          *\n");
    printf(" *-----------------------------------*\n");
    printf(" *    Testing decays of: %.8s        *\n",HEPParticle::GetParticleName(Setup::decay_particle));
    printf(" *                                   *\n");
    printf(" *   (c) Nadia    Davidson,   (1,2)  *\n");
    printf(" *       Piotr    Golonka,    (3)    *\n");
    printf(" *       Tomasz   Przedzinski,(4)    *\n");
    printf(" *       Tomasz   Pierzchala, (5)    *\n");
    printf(" *       Zbigniew Was         (2,3)  *\n");
    printf(" *                                   *\n");
    printf(" *  1) Unimelb, Melbourne, Australia *\n");
    printf(" *  2)     INP, Krakow, Poland       *\n");
    printf(" *  3)    CERN, Geneva, Switzerland  *\n");
    printf(" *  4)      UJ, Krakow, Poland       *\n");
    printf(" *  5)    until version 1.112        *\n");
    printf(" *************************************\n\n");


    if (Setup::EVENT == 0) {
        printf(" -> No event record type is set.\n");
	printf(" -> Will use HepMC or HEPEVT.\n");
	Setup::EVENT=&HEPEVT;
    }
    else if (Setup::EVENT== &HEPEVT) 
	printf(" -> Event record format: HEPEVT\n");
    else if (Setup::EVENT== &LUJETS) 
	printf(" -> Event record format: LUJETS\n");
    else if (Setup::EVENT== &PYJETS) 
	printf(" -> Event record format: PYJETS\n");
    else if (Setup::EVENT== &HerwigEVT) 
	printf(" -> Event record format: HerwigEVT\n");
    else if (Setup::EVENT== &MCTEVT) 
	printf(" -> Event record format: MCTEVT\n");
    else
	printf(" -> Event record format unknown \n");
	
    if (Setup::user_event_analysis) {
	printf(" -> Using User Event Analysis Code from object:%s\n",
			Setup::user_event_analysis->GetName());
    }


    if (Setup::UserTreeAnalysis) {
	// setup user tree analysis...
      printf(" -> User Tree Analysis routine: %s\n",Setup::UserTreeAnalysis);
      if(std::strcmp(Setup::UserTreeAnalysis,"UserTreeAnalysis") !=0) 
      {
        std::string bufline=".L ./";
        bufline += Setup::UserTreeAnalysis;
        bufline += ".C+";
        gSystem->AddIncludePath("-I${MCTESTERLOCATION}/include/");
        gSystem->AddLinkedLibs("${MCTESTERLOCATION}/lib/libMCTester.so");
        gSystem->AddLinkedLibs("${MCTESTERLOCATION}/lib/libHEPEvent.so");
        gSystem->AddLinkedLibs("${MCTESTERLOCATION}/lib/libHepMCEvent.so");
        gSystem->AddLinkedLibs("-L${HEPMCLOCATION}/lib -lHepMC");
        gROOT->ProcessLine(bufline.c_str());

        userTreeAnalysis=new TMethodCall(Setup::UserTreeAnalysis,
        "Setup::UTA_particle,Setup::UTA_partlist,Setup::UTA_nparams, Setup::UTA_params");
	if( !userTreeAnalysis->IsValid() )
        {
          printf("\nMC_Initialize: error processing user analysis script %s.C\n",Setup::UserTreeAnalysis);
          exit(-1);
        }
      }
    }

    char default_result[] = "mc-tester.root";

    if (Setup::stage==1) {
	if (Setup::result1_path==0 || strlen(Setup::result1_path)==0)
	    Setup::result1_path=default_result;

	Setup::ResolvePath(Setup::result1_path,myfname);
        printf(" -> results from stage1 goes to:\n     %s\n",myfname);

    } else {
	if (Setup::result2_path==0 || strlen(Setup::result2_path)==0)
	    Setup::result2_path=default_result;

	Setup::ResolvePath(Setup::result2_path,myfname);
        printf(" -> results from stage2 goes to:\n     %s\n",myfname);	

    }
    
    printf("\n");
	setup_copy=new GenerationDescription(Setup::setup);
	getcwd(setup_copy->gen_path,128);


    if (Setup::stage==2) {
	sprintf(setup_copy->gen_desc_1,"%s",Setup::gen2_desc_1);
	sprintf(setup_copy->gen_desc_2,"%s",Setup::gen2_desc_2);
	sprintf(setup_copy->gen_desc_3,"%s",Setup::gen2_desc_3);
    } else {
	sprintf(setup_copy->gen_desc_1,"%s",Setup::gen1_desc_1);
	sprintf(setup_copy->gen_desc_2,"%s",Setup::gen1_desc_2);
	sprintf(setup_copy->gen_desc_3,"%s",Setup::gen1_desc_3);
    }

}


void MC_Finalize()
{

  TDirectory *gdirectory_save= gDirectory;

  TFile *file=TFile::Open(myfname,"RECREATE");
  
  if (!file) {
    printf(" ! ERROR: cannot open output file: %s\n",myfname);
    exit(-1);
  }
  
  file->cd();

  setup_copy->Write("GenerationDescription");

  //sorting decay modes
  TDecayMode::DecayModes->Sort();
  char* new_title = new char[n_decaymodes+50]; //probably not the nicest way to do this
  
  TIter next(TDecayMode::DecayModes);
  for(int i = 0; TDecayMode *dm=(TDecayMode*)next(); i++) {
    //probably not the nicest way to do this
    sprintf(new_title,"DecayMode%03i",i);
    dm->SetTitle(new_title);//retitle directory so that modes are ordered
    printf("  %s  (%li entries)\n",dm->GetName(),dm->GetNEntries());
    
    // Create directory for each decay mode and add histograms to output file
    file->cd();
    TDirectory *dir = file->mkdir(dm->GetName(),dm->GetTitle());
    dir->cd();
    dm->Write(dm->GetName());
    for(int j=0;j<dm->histograms->GetEntries();j++){
      dm->histograms->At(j)->Write(dm->histograms->At(j)->GetName());
    }
  }
  
  // append user-mode histograms if any...
  
  if (Setup::user_histograms->GetEntries()){
    TDirectory *dir=file->mkdir("USER_HISTOGRAMS");
    dir->cd();
  }
  for(int i=0; i<Setup::user_histograms->GetEntries();i++){
    Setup::user_histograms->At(i)->Write(Setup::user_histograms->At(i)->GetName());
  }
  
  printf("-------------END OF MC-TESTER RUN-------------------\n");
  
  int nchannels=TDecayMode::DecayModes->GetEntriesFast();
  printf("Total: %i channels found, %li events analyzed\n",nchannels, Setup::events_cnt);
  
  printf("\n Total entries: %li\n\n",TDecayMode::NFills);

  file->Close();
  
  gDirectory = gdirectory_save;
  gDirectory->cd();
}


void MC_Analyze()
{
  MC_Analyze(Setup::decay_particle);
}

void MC_Analyze(HEPEvent * event, double weight)
{
  MC_Analyze(Setup::decay_particle, weight, event);
}

void MC_Analyze(int particle_PDG, double weight, HEPEvent * event)
{

  if(particle_PDG!=Setup::decay_particle){ //print error message is 
    std::cout << "MC-TESTER ERROR: Decay particle pdg id has change since initialization. Please check that" << std::endl;
    std::cout << "Setup::decay_particle is set in SETUP.C and is consistent with the pdg id given to MC_Analyze()" << std::endl;
    exit(-1);
  }

  Setup::event_weight = weight;
#define MCDEBUG(statement) if (Setup::debug_mode) {std::cout<<"DEBUG: Event "<<EVENT->GetEventNumber()<<" ["<<	Setup::events_cnt <<"]: "<<statement<<std::endl;}
#define MCDEBUGPART(statement,particle) if (Setup::debug_mode) {std::cout<<"DEBUG: Event "<<EVENT->GetEventNumber()<<" ["<<	Setup::events_cnt <<"] :"<<statement<<std::endl; std::cout<<"      ";particle->ls();}


    int ndecayproducts=0;
/*
    int myParticles[10000];
    int nparticles=0;
    int i=0;
    int children[10000];
    int nchildren=0;
    int decayproducts[1000];
    HEPParticle *p=0;
*/

    if(event)
      Setup::EVENT=event;

    HEPEvent *EVENT=Setup::EVENT;
    HEPEvent *EVENT_ORIG=Setup::EVENT;

    if (Setup::user_event_analysis) {
	UserEventAnalysis &a=*(Setup::user_event_analysis);
	a.SaveOriginalEvent(EVENT);
	EVENT=a.ModifyEvent(EVENT);
	Setup::EVENT=EVENT;
    }

    //    std::cout << "MC-TESTER sees the event:"<<std::endl;
    //    EVENT->ls();

    Setup::events_cnt++;

    // firstly: find all particles of that kind
    HEPParticleList plist;
    EVENT->FindParticle(particle_PDG,&plist);
    if (plist.empty()){
	
	MCDEBUG("no "<< HEPParticle::GetParticleName(particle_PDG)<<" particle found");
	return;
    } else {
	MCDEBUG(plist.size()<<" particles found");
    }
/*
    for (i=1; i<=EVENT->GetNumOfParticles();i++) {
	p=EVENT->GetParticle(i);
	// Check if it decays:
	if (! p->Decays() ) continue;
	if ( p->GetPDGId() == particle_PDG ) {
	    // protection for PYTHIA-like loops:
	    if (p->GetFirstDaughter() < p->GetId()) continue;
	    if (p->GetLastDaughter() - p->GetFirstDaughter() <=0)  continue;
	    //printf("analysis have found particle:");
	    //p->ls();
	    myParticles[nparticles]=i;
	    nparticles++;
	}
    } 

    if (nparticles<1) return;
*/
    
    HEPParticleList* stableChildren=new HEPParticleList;
    
    HEPParticleListIterator particles(plist);

    for (HEPParticle *part=particles.first(); part!=0; part=particles.next() ) {
	// exclude the ones which status is wrong.
	if (! part->Decays() ) {
	    MCDEBUGPART("Particle does not decay [status code = "<<part->GetStatus()<<"]",part)
	    continue;
	}
	// find all stable children;
	
	// at first - create the list of all daughters: intermediate and final
	HEPParticleList daughterlist;
	part->GetDaughterList(&daughterlist);
	if (daughterlist.empty()) {
	    
	    MCDEBUGPART("DAUGHTERLIST EMPTY",part);

	    //EVENT->ls();
	    printf("daughterlist is empty! decaying particle is:\n");
	    //part->ls();
	    exit(-1);
	}

	MCDEBUGPART("Particle OK ",part)
	
	// now dynamicaly append daughters' daughters etc.
	HEPParticleListIterator daughters(daughterlist);
	
	for (HEPParticle *d = daughters.first(); d!=0; d=daughters.next() ) {
	    
	    // only if status OK.
	    if (d->Decays() ) {
	    
		// but only if decay is not suppressed!
		if (Setup::IsSuppressed(d->GetPDGId())) continue;
		
		d->GetDaughterList(&daughterlist); // the list modifies itself in recurrece.
	    }
	}

	ndecayproducts=0;    
	stableChildren->clear();

	// select only the stable ones and fill decayproducts array:
	for (HEPParticle *d = daughters.first(); d!=0; d=daughters.next() ) {
	    if (d->IsStable() || (Setup::IsSuppressed(d->GetPDGId())) ){	

		stableChildren->push_back(d);
		//decayproducts[ndecayproducts]=d->GetId();
		ndecayproducts++;
		//d->ls();
	    }

	}

       // apply user's tree analysis:
	//stableChildren->ls();
	//printf("is empty:\n");
	//printf("%i\n",stableChildren->empty());
	//printf("stable children size:%i\n",stableChildren->size());

	// Do not automatically add histograms declared in UserTreeAnalysis
	TH1::AddDirectory(kFALSE);

	if (userTreeAnalysis || Setup::UserTreeAnalysis) {
	    Setup::UTA_particle=part;
	    Setup::UTA_partlist=stableChildren;
	    if(userTreeAnalysis)
	       userTreeAnalysis->Execute("Setup::UTA_particle,Setup::UTA_partlist,Setup::UTA_nparams, Setup::UTA_params");
	    else
	       UserTreeAnalysis(Setup::UTA_particle,Setup::UTA_partlist,Setup::UTA_nparams,Setup::UTA_params);
            if(stableChildren->empty()) continue;
	    //if(!stableChildren->size()==1) event->ls(); ***** need to look at extra entries.
        }

	TH1::AddDirectory(kTRUE);
//	exit(-1);
    
//	TDecayMode *mode = TDecayMode::CheckMode( part->GetPDGId(),ndecayproducts,decayproducts,1, Setup::order_matters);
	TDecayMode *mode = TDecayMode::CheckMode( part->GetPDGId(),stableChildren,1, Setup::order_matters);
	// (Hint: no need to check if the routine returns valid pointer! )

	// Fill TDecayMode object with the data describing decay products:
	
	if (Setup::debug_mode) { 
	    if (mode->fill_histos)
		std::cout<<"###DM:";
	    else
		std::cout<<"###DM[NOHISTOS]:";
		
	    mode->ls();
	    std::cout<<std::endl;
	};
	
//	mode->Fill(ndecayproducts,decayproducts);
	mode->Fill(stableChildren,weight);

    }    
/*    
    // Now we process them one by one.
    
    // find all stable children:
    for (i=0; i<nparticles;i++) {
	nchildren=0;
	p=EVENT->GetParticle(myParticles[i]);

	// record all direct daughters:
	for (int j=p->GetFirstDaughter(); j<=p->GetLastDaughter();j++) {
	    children[nchildren]=j;
	    nchildren++;	    	
	}
	
	// and daughters' daughters...
	int k=0;
	while (k<nchildren) {
	    HEPParticle *d=EVENT->GetParticle(children[k]);

	    // if unstable - append to the end of list,
	    // so it will finally be processed...
	    if ( d->Decays() ) {
		// but only if decay is not suppressed!
		if (Setup::IsSuppressed(d->GetPDGId())) {
		    //printf("Decay Suppressed:");
		    //d->ls();
		} else {
		    for (int l=d->GetFirstDaughter();l<=d->GetLastDaughter();l++) {
			children[nchildren]=l;
			nchildren++;
		    }
		}
	    }
	    
	    k++;
	    
	} // end of while(k<nchildren)
	

	// We have a list of all decays products,
	// both: stable and unstable.
	//
	// we choose only final, stable products and fill
	// ndecayproducts table.
	
	ndecayproducts=0;
	for (int m=0; m<nchildren;m++){
	    HEPParticle *particle=EVENT->GetParticle(children[m]);
	    if (particle->IsStable() || (Setup::IsSuppressed(particle->GetPDGId())) ){	
		decayproducts[ndecayproducts]=particle->GetId();
		ndecayproducts++;
	    }
	}

	//printf("we look for decay mode with %i decay prods\n",ndecayproducts);

	// Let's find out whether we already have a TDecayMode object
	// that coresponds to our list of decay products.
	// If it did not exist, it will be created automatically for us.

	TDecayMode *mode = TDecayMode::CheckMode( p->GetPDGId(),ndecayproducts,decayproducts,1, Setup::order_matters);
	// (Hint: no need to check if the routine returns valid pointer! )

	// Fill TDecayMode object with the data describing decay products:
	
	mode->Fill(ndecayproducts,decayproducts);
    }
*/    

    if (Setup::user_event_analysis) {
	UserEventAnalysis &a=*(Setup::user_event_analysis);
	a.RestoreOriginalEvent(EVENT);
	Setup::EVENT=EVENT_ORIG;
    }


    delete stableChildren;

}

void PrintAnalysedEvent()
{
    if (Setup::user_event_analysis) {
	printf("event as modified by the user analysis code:\n");
	Setup::user_event_analysis->SavedEvent()->ls();
    }

}


void MC_FillUserHistogram(char *name, double value, double weight)
{
    TH1D *h=(TH1D*)(Setup::user_histograms->FindObject(name));
    assert(h);
    h->Fill(value,weight);

}



#ifdef USE_MC_FINALIZE_AT_EXIT

// Option 'MC_FinalizeAtExit' is not supported by all platforms
// and may be unsafe. That is why it is turned off by default

#include <execinfo.h>
#include <signal.h>

bool MC_Finalize_already_invoked = false;

inline void MC_FinalizeAtExit_call(int signal)
{
  if(MC_Finalize_already_invoked) exit(signal);
  else                            MC_Finalize_already_invoked = true;

  printf("\n"); 

  switch(signal)
  {
    case 0      : break;
    case SIGINT : printf("MC_FinalizeAtExit: Interrupted!\n");             break;
    case SIGSEGV: printf("MC_FinalizeAtExit: ***Segmentation fault***\n"); break;
    default:      printf("MC_FinalizeAtExit: Caught signal: %i\n",signal); break;
  }

  if( signal!=0 ) printf("                   Attempting to save partial results...\n\n");
  
  MC_Finalize();
  
  if(signal==SIGSEGV)
  {
    printf("\n");
    printf("***         Segmentation fault occured during program execution         ***\n");
    printf("*** Keep in mind that MC_FinalizeAtExit disables core dump or backtrace ***\n");
  }

  exit(signal);
}

inline void MC_FinalizeAtExit_call()
{
  if(!MC_Finalize_already_invoked) MC_FinalizeAtExit_call(0);
}

void MC_FinalizeAtExit()
{
  // Invoke MC_Finalize at exit
  atexit(MC_FinalizeAtExit_call);
  
  struct sigaction action;
  sigemptyset(&action.sa_mask);
  action.sa_flags   = 0;
  action.sa_handler = MC_FinalizeAtExit_call;
  
  // Invoke MC_Finalize at segmentation fault, interruption, when aborted or terminated
  sigaction(SIGSEGV, &action, 0);
  sigaction(SIGINT,  &action, 0);
  sigaction(SIGABRT, &action, 0);
  sigaction(SIGTERM, &action, 0);
}

#else

void MC_FinalizeAtExit()
{
  printf("MC_FinalizeAtExit: This feature is not supported by your MC-TESTER installation.\n");
  printf("                   Use 'MC_Finalize()' at the end of main program or recompile\n");
  printf("                   MC-TESTER uncommenting option at the end of 'make.inc'\n");
  printf("                   (Not supported by all platforms)\n");
  exit(-1);
}

#endif
