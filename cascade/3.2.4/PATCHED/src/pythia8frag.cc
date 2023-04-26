#include <stdio.h>
#include <string.h>
#include <cstring>
#include <iostream>
#ifdef PYTHIA6_USE_HEPMC2
#include "HepMC/PythiaWrapper.h"
#include "HepMC/IO_HEPEVT.h"
#include "HepMC/IO_GenEvent.h"
#include "HepMC/GenEvent.h"
#include "HepMC/IO_AsciiParticles.h"
#include "HepMC/HEPEVT_Wrapper.h"
#include "PythiaHelper.h"
//#include "boost/algorithm/string/trim.hpp"
//#include "HepMC/WeightContainer.h"
#include "Pythia8Plugins/HepMC2.h"
extern void hepmc2_gWriters_set_event(const int & position, HepMC::GenEvent* e );
#else

// PYTHIA Common Block Declarations

const int pyjets_maxn =4000;
extern "C" {
    extern struct {
        int n, npad, k[5][pyjets_maxn];
        double p[5][pyjets_maxn], v[5][pyjets_maxn];
    } pyjets_;
}
#define pyjets pyjets_
#include "HepMC3/GenEvent.h"
#include "PythiaHelper.h"
#include "Pythia8Plugins/HepMC3.h"
extern void hepmc3_gWriters_set_event(const int & position, HepMC3::GenEvent* e );
#endif


#include <unistd.h>

using namespace std;


#include <string>
#include <iostream>

#include "Pythia8/Pythia.h"
#include "Pythia8/LesHouches.h"
using namespace Pythia8;

extern "C" {
    extern struct casconvhepmc{
      double w_scale[100], w_pdf[200];
      int nhard, nw_scale, nw_pdf, id_scale[100], id_pdf[200];
    } casconvhepmc_;
}
#define casconvhepmc casconvhepmc_

extern "C" {
    extern struct steerhm{
      char hmfilnam[4096];
      int hmfilfor;
    } steerhm_;
}
#define steerhm steerhm_

extern "C" {   

  int ncountp8 =0 ;
  int ncount = 0;   
  static Pythia pythia;
  Event& event      = pythia.event;
#ifdef PYTHIA6_USE_HEPMC2
  HepMC::GenCrossSection cross;
  static HepMC::Pythia8ToHepMC ToHepMC;
#else
  //HepMC3::GenCrossSection cross;
  // Interface for conversion from Pythia8::Event to HepMC one.
  static  HepMC3::Pythia8ToHepMC3 toHepMC;

#endif


void pythia8frag_(){
    
  // cout << " pythia8frag " << endl;
  // Generator; shorthand for event and particleData.
  //static Pythia pythia;
  //Event& event      = pythia.event;
  static ParticleData& pdt = pythia.particleData;
  // Set true to also see space-time information in event listings.
  static int type=0;
  static bool showScaleAndVertex = (type == 0) ? true : false;

  if (ncountp8 == 0 ) {   
     pythia.readString("Random:setSeed = on");
     char * pseed ;
     if(getenv("CASEED")) { 
        pseed = getenv ("CASEED");
        cout << " caseed set " << pseed << endl; }
        else { pseed = "12314"; 
         cout << " caseed NOT set " << pseed << endl ;}
     // cout << " after getenv " << pseed << endl;
     char * rand = "Random:seed = "  ;
     string seed1 = rand;
     string seed2 = pseed ;
     cout << seed1 + seed2 << endl;
     pythia.readString(seed1+seed2);
     // Load configuration file
     pythia.readFile("steering-P8.cmnd");
     cout << " hannes: after IO " << endl;
#ifdef PYTHIA6_USE_HEPMC2
     //ToHepMC.set_print_inconsistency(false);
     ToHepMC.set_free_parton_exception(false);
     // Do not store cross section information, as this will be done manually.
     ToHepMC.set_store_pdf(false);
     ToHepMC.set_store_proc(false);
     ToHepMC.set_store_xsec(false);
#else
     toHepMC.set_print_inconsistency(false);
     toHepMC.set_free_parton_warnings(false);
     // Do not store cross section information, as this will be done manually.
     toHepMC.set_store_pdf(false);
     toHepMC.set_store_proc(false);
     toHepMC.set_store_xsec(false);

#endif

     // Reset event record to allow for new event.
     // Initialize.
     pythia.init();
  }
  
  event.reset();
  
  // cout<< " PYJETS "<< pyjets.n<< endl;
  // event.list();
   
  int icol_index = 0 ;
  int Istr = 0 ;
  int icol1 = 0 ;
  int icol2 = 0;
  
  int mother1 = 0;
  int mother2 = 0;
  int daughter1 = 0; 
  int daughter2 = 0;
   
  for ( int i=0; i<= pyjets.n - 1 ; i++ ){
  int iorig = pyjets.k[2][i] ;
  int iorig2 = 0 ;
  int iflavor = pyjets.k[1][i] ;
  int istat = pyjets.k[0][i] ;
  int istat_old = pyjets.k[0][i-1] ;
  
  //cout << " filling event record " << i << " " <<pyjets.n  << " iflavor = " << iflavor << endl;

  //cout << " i = " << i << " istat = " << istat << " istat_old " << istat_old << " iflavor = " << iflavor << " iorig = "<< iorig << endl;
//  cout << " i " << i << " k4 " << pyjets.k[3][i] << " k5 " << pyjets.k[4][i] << endl;
  int istat_p8 =istat ;
  
  icol1 = 0 ;
  icol2 = 0;
  mother1 = 0; 
  mother2 = 0;
  daughter1 = 0; 
  daughter2 = 0;
  //istat_p8 = 71;
  istat_p8 = 23;
  if (istat == 21 ) { istat_p8 = -201 ;}
  else if(istat == 1 ) {istat_p8 = 23; }  
  else if(istat == 11 ) {istat_p8 = -201; }
  else if(istat == 13 ) {istat_p8 = -201; }
  else if(istat == 14 ) {istat_p8 = -201; }
  else if(istat == -1) {istat_p8 = -299; }  // treat old continuation lines

  icol1 = pyjets.k[3][i] ;
  icol2 = pyjets.k[4][i] ;

  // change P6: (CM-shower) to P8: (system)
  if (iflavor == 94 ) iflavor = 90 ;

  if( i == 0 || i == 1 ) { istat_p8 = -12 ; daughter1=3+i; daughter2 = 0; }
  if ( i > 1 ) { mother1 = 1 ; mother2 = 2 ;}
 
  if(iflavor != 0 ) { 
   event.append( iflavor, istat_p8, mother1, mother2, daughter1, daughter2, icol1, icol2 , pyjets.p[0][i], pyjets.p[1][i],  pyjets.p[2][i], pyjets.p[3][i], pyjets.p[4][i] );} 

  }
  if (!pythia.next() ) {
     cout << "Had Error " << ncountp8 <<   endl ; 
     event.list();
     }
  else { // cout << " after had - no error " << endl;
  //cout << " pythia next = " << pythia.next() << endl;
  // event.list();
 
  } 
   ++ncountp8  ; 
   if ( ncountp8 < 5) event.list() ;    
   // cout << " in pythia8 frag " << endl;
   

   }
   
#ifdef PYTHIA6_USE_HEPMC2
int hepmc_convert_eventp8_(const int & position)
{
      HepMC::GenEvent* hepmcevt = new HepMC::GenEvent();
      ToHepMC.fill_next_event( pythia, hepmcevt );
      hepmcevt->use_units(HepMC::Units::GEV, HepMC::Units::MM);
      hepmc2_gWriters_set_event(position,hepmcevt);
      return 0;
}

void convhepmcp8_(int & ievent, int & iproc, double & weight, double & xsec, double & xsece, int & id1pdf, int & id2pdf, double & x1pdf, double & x2pdf, double & QFac, double & pdf1, double & pdf2 ){
      
       string scale[]={"scale_variation1","scale_variation2","scale_variation3","scale_variation4","scale_variation5","scale_variation6","scale_variation7","scale_variation8","scale_variation9","scale_variation10"};
       string tmdpdf[]={"pdf_variation1","pdf_variation2","pdf_variation3","pdf_variation4","pdf_variation5","pdf_variation6","pdf_variation7","pdf_variation8","pdf_variation9","pdf_variation10",
                        "pdf_variation11","pdf_variation12","pdf_variation13","pdf_variation14","pdf_variation15","pdf_variation16","pdf_variation17","pdf_variation18","pdf_variation19","pdf_variation20",
                        "pdf_variation21","pdf_variation22","pdf_variation23","pdf_variation24","pdf_variation25","pdf_variation26","pdf_variation27","pdf_variation28","pdf_variation29","pdf_variation30",
                        "pdf_variation31","pdf_variation32","pdf_variation33","pdf_variation34","pdf_variation35","pdf_variation36","pdf_variation37","pdf_variation38","pdf_variation39","pdf_variation40"
                       };
       

       // with the static command, called only once
       //static char * outfile = cahepmcout.hepmcout+'\0' ;
       static char * outfile = steerhm.hmfilnam+'\0' ;
       static HepMC::IO_GenEvent ascii_io(outfile ,std::ios::out); 

      
//       cout << " convhepmc=" << outfile << "$end"<<endl;
      if ( ncount < 10) {
        if (outfile!=NULL) { cout << " convhepmcP8: filename = " <<  outfile << endl;}
          else { cout << "  convhepmcP8: NO filename set " <<endl; 
          return ;}
          ++ncount;
      }
      

   // use Pythia8 method to fill hepmc
      HepMC::GenEvent* hepmcevt = new HepMC::GenEvent();

      ToHepMC.fill_next_event( pythia, hepmcevt );

      hepmcevt->use_units(HepMC::Units::GEV, HepMC::Units::MM);
      hepmcevt->set_event_number(ievent);
      hepmcevt->set_signal_process_id(iproc);
      hepmcevt->weights().push_back(weight);
      
      
      // cout <<" convhepmc: weight = " << weight << " Event Nr " << ievent << endl;
      // Convert weight names in MadGraph5 convention to the convention outlined
      // in https://arxiv.org/pdf/1405.1067.pdf, page  162ff.
      if( casconvhepmc.nw_scale > 0) {
          string name ;       
          for (int jscale=0; jscale<casconvhepmc.nw_scale; jscale++){
          int id = casconvhepmc.id_scale[jscale] ;

            if (id==1001) name="MUR1.0_MUF1.0";
            if (id==1002) name="MUR1.0_MUF2.0";
            if (id==1003) name="MUR1.0_MUF0.5";
            if (id==1004) name="MUR2.0_MUF1.0";
            if (id==1005) name="MUR2.0_MUF2.0";
            if (id==1006) name="MUR2.0_MUF0.5";
            if (id==1007) name="MUR0.5_MUF1.0";
            if (id==1008) name="MUR0.5_MUF2.0";
            if (id==1009) name="MUR0.5_MUF0.5";

	   hepmcevt->weights()[name] = casconvhepmc.w_scale[jscale];
         //cout << " convhepmc: scalenumber " << jscale << " id = " << id << endl;
         // cout << " convhepmc: scale name = " << name << " weight = " << casconvhepmc.w_scale[jscale] << endl;
         }
      } 
      if( casconvhepmc.nw_pdf > 0) {
      // Convert PDF weight names to the convention outlined
      // in https://arxiv.org/pdf/1405.1067.pdf, page  162ff.
      // but indicating that it is the TMDlib numbering 
         for (int jpdf=0; jpdf<casconvhepmc.nw_pdf; jpdf++){
         int id = casconvhepmc.id_pdf[jpdf] ;
         string name_pdf = "TMD"+ std::to_string(id);
	   hepmcevt->weights()[name_pdf] = casconvhepmc.w_pdf[jpdf];
         //cout << " convhepmc: pdfnumber " << jpdf << endl;
         //cout << " convhepmc: pdf name = " << name_pdf << " weight = " << casconvhepmc.w_pdf[jpdf] << endl;
         }
      } 
      //      std::cout << " ievent " << ievent << " iproc " << iproc << " xsec " <<xsec<< std::endl;
      const double xsecval = xsec;
      const double xsecerr = xsece ;
      cross.set_cross_section( xsecval, xsecerr );
	hepmcevt->set_cross_section( cross );
      // Store PDF information.
      HepMC::PdfInfo pdf( id1pdf, id2pdf, x1pdf, x2pdf, QFac, pdf1, pdf2, 230, 230);
      hepmcevt->set_pdf_info(pdf);      // write the event out to the ascii file
	ascii_io << hepmcevt;

   delete hepmcevt;
 
   }
   
#else
#include "HepMC3/Print.h"
using namespace HepMC3;  

int hepmc_convert_eventp8_(const int & position)
{
      shared_ptr<HepMC3::GenRunInfo> genRunInfo;
      genRunInfo = make_shared<HepMC3::GenRunInfo>();
      HepMC3::GenEvent* hepmcevt = new  HepMC3::GenEvent(genRunInfo);
      toHepMC.fill_next_event( pythia, hepmcevt );
      hepmc3_gWriters_set_event(position,hepmcevt);
      return 0;
}    
void convhepmcp8_(int & ievent, int & iproc, double & weight, double & xsec, double & xsece, int & id1pdf, int & id2pdf, double & x1pdf, double & x2pdf, double & QFac, double & pdf1, double & pdf2 ){
//       string scale[]={"scale1","scale2","scale3","scale4","scale5","scale6","scale7","scale8","scale9","scale10"};
       string scale[]={"scale_variation1","scale_variation2","scale_variation3","scale_variation4","scale_variation5","scale_variation6","scale_variation7","scale_variation8","scale_variation9","scale_variation10"};
       string tmdpdf[]={"pdf_variation1","pdf_variation2","pdf_variation3","pdf_variation4","pdf_variation5","pdf_variation6","pdf_variation7","pdf_variation8","pdf_variation9","pdf_variation10",
                        "pdf_variation11","pdf_variation12","pdf_variation13","pdf_variation14","pdf_variation15","pdf_variation16","pdf_variation17","pdf_variation18","pdf_variation19","pdf_variation20",
                        "pdf_variation21","pdf_variation22","pdf_variation23","pdf_variation24","pdf_variation25","pdf_variation26","pdf_variation27","pdf_variation28","pdf_variation29","pdf_variation30",
                        "pdf_variation31","pdf_variation32","pdf_variation33","pdf_variation34","pdf_variation35","pdf_variation36","pdf_variation37","pdf_variation38","pdf_variation39","pdf_variation40"
                       };
       

       static char * outfile = steerhm.hmfilnam+'\0' ;
       // Specify file where HepMC events will be stored.
       // with the static command, called only once
       static HepMC3::WriterAscii ascii_io(outfile);
       
      
//       cout << " convhepmc=" << outfile << "$end"<<endl;
      if ( ncount < 10) {
        if (outfile!=NULL) { cout << " convhepmcP8: filename = " <<  outfile << endl;}
          else { cout << "  convhepmcP8: NO filename set " <<endl; 
          return ;}
          ++ncount;
      }

      shared_ptr<HepMC3::GenRunInfo> genRunInfo;
      genRunInfo = make_shared<HepMC3::GenRunInfo>();
      HepMC3::GenEvent hepmcevt(genRunInfo);

      // Construct new empty HepMC event.
      //HepMC3::GenEvent hepmcevt(HepMC3::Units::GEV,HepMC3::Units::MM);

       toHepMC.fill_next_event( pythia, &hepmcevt );
    
      
      // cout <<" convhepmc: weight = " << weight << " Event Nr " << ievent << endl;
      // Convert weight names in MadGraph5 convention to the convention outlined
      // in https://arxiv.org/pdf/1405.1067.pdf, page  162ff.
      vector<string> weight_names ;
      weight_names.push_back("0"); // The first weight is always the
                                // default weight with name "0".
      // set the weight by hand, since pythia gives weight = 1. by default.                          
      hepmcevt.weights()[0]=weight ;                          
      if( casconvhepmc.nw_scale > 0) {
          string name ;       
          //cout << " convhepmc: nw_scale " << casconvhepmc.nw_scale << endl;
          for (int jscale=0; jscale<casconvhepmc.nw_scale; jscale++){
          int id = casconvhepmc.id_scale[jscale] ;

            if (id==1001) name="MUR1.0_MUF1.0";
            if (id==1002) name="MUR1.0_MUF2.0";
            if (id==1003) name="MUR1.0_MUF0.5";
            if (id==1004) name="MUR2.0_MUF1.0";
            if (id==1005) name="MUR2.0_MUF2.0";
            if (id==1006) name="MUR2.0_MUF0.5";
            if (id==1007) name="MUR0.5_MUF1.0";
            if (id==1008) name="MUR0.5_MUF2.0";
            if (id==1009) name="MUR0.5_MUF0.5";
            weight_names.push_back(name);
            hepmcevt.weights().push_back(casconvhepmc.w_scale[jscale]);
         //cout << " convhepmc: scalenumber " << jscale << " id = " << id << endl;
         //cout << " convhepmc: scale name = " << name << " weight = " << casconvhepmc.w_scale[jscale] << endl;
         }
      //genRunInfo->set_weight_names(weight_names);
      // Set event weight
      //hepmcevt.weights().push_back(evtweight*normhepmc);
      } 
      if( casconvhepmc.nw_pdf > 0) {
      // Convert PDF weight names to the convention outlined
      // in https://arxiv.org/pdf/1405.1067.pdf, page  162ff.
      // but indicating that it is the TMDlib numbering 
         for (int jpdf=0; jpdf<casconvhepmc.nw_pdf; jpdf++){
         int id = casconvhepmc.id_pdf[jpdf] ;
         string name_pdf = "TMD"+ std::to_string(id);
         weight_names.push_back(name_pdf);
         hepmcevt.weights().push_back(casconvhepmc.w_pdf[jpdf]);
       //  cout << " convhepmc: pdfnumber " << jpdf << endl;
       //  cout << " convhepmc: pdf name = " << name_pdf << " weight = " << casconvhepmc.w_pdf[jpdf] << endl;
         }
      } 
      genRunInfo->set_weight_names(weight_names);
     // HepMC3::GenEvent hepmcevt(genRunInfo);
      // Fill HepMC event
      //cout << " before tohepmc " << endl ;
      //event.list();
	//evt->weights()["test3"] = 1.9999;

      //      std::cout << " ievent " << ievent << " iproc " << iproc << " xsec " <<xsec<< std::endl;
      const double xsecval = xsec;
      const double xsecerr = xsece ;
      
      // Report cross section to hepmc.
      shared_ptr<HepMC3::GenCrossSection> xsection;
      xsection = make_shared<HepMC3::GenCrossSection>();
      xsection->set_cross_section( xsecval, xsecerr );
      hepmcevt.set_cross_section( xsection );

      // Store PDF information.
      shared_ptr<HepMC3::GenPdfInfo> pdf_info = make_shared<HepMC3::GenPdfInfo>();
      hepmcevt.add_attribute("GenPdfInfo",pdf_info);
      pdf_info->set(id1pdf, id2pdf, x1pdf, x2pdf, QFac, pdf1, pdf2, 230, 230);


      //toHepMC.fill_next_event( pythia, &hepmcevt );

      hepmcevt.set_event_number(ievent);
      //hepmcevt->set_event_number(ievent);
      //hepmcevt.set_signal_process_id(iproc);
      // Set event weight
      // hepmcevt.weights().push_back(30.);


      // Unset GenRunInfo object of writer so that event run info is used.
      ascii_io.set_run_info(0);
      
//            std::cout << " event: " << std::endl;
//            Print::listing(hepmcevt);
            //Print::content(hepmcevt);
      
      //cout<< " before ascii write " << endl;
      ascii_io.write_event(hepmcevt);

 
   }

#endif   

   
}
