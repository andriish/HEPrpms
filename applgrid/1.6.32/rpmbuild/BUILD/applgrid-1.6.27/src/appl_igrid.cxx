/**
 **     @file    appl_igrid.cxx
 **
 **     @brief   grid class - all the functions needed to create and 
 **              fill the grid from an NLO calculation program. 
 **
 **     @author  mark sutton
 **     @date    2007/10/16 17:01:39 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: appl_igrid.cxx, v1.00 2007/10/16 17:01:39 sutt $
 **
 **/

#include <stdlib.h>
#include <iostream>
#include <iomanip>
#include <cmath>

#include "amconfig.h"

#include "appl_igrid.h"
#include "appl_grid/appl_grid.h"
#include "appl_grid/appl_pdf.h"
#include "appl_grid/appl_timer.h"
#include "appl_grid/vector_stream.h"
#include "appl_grid/stream_grid.h"

#include "hoppet_init.h"
#include "threadManager.h"

#ifdef USEROOT
#include "TFile.h"
#include "TObject.h"
#include "TObjString.h"
#include "TH3D.h"
#include "TVectorT.h"
#include "TFileString.h"
#endif

#include "Splitting.h"


#include "appl_grid/lumi_pdf.h"

#include "appl_grid/stream_vector.h"

// splitting function code


// pdf reweighting
// bool   igrid::m_reweight   = false;
// bool   igrid::m_symmetrise = false;

// variable tranformation parameters
double appl::igrid::transvar = 5;
bool   appl::igrid::threads_disabled = true;

double appl::igrid::lambda = 0.0625;


#if __cplusplus <= 199711L
// #warning Not using C++11 compilation - using substitute functions
std::string std::to_string( int i );
std::string std::to_string( double f );
#endif


static int ithread = 0;

std::string label( int i ) { 
  char lab[64];
  std::sprintf( lab, "thread-%d", i );
  return lab;
}


double (*fuserscale)( double ) = 0;


// static bool dbg = true;

appl::igrid::igrid() : 
  threadManager( label(ithread++) ),
  mfy(0),   mfx(0),
  m_parent(0),
  m_Ny1(0),   m_y1min(0),   m_y1max(0),   m_deltay1(0),
  m_Ny2(0),   m_y2min(0),   m_y2max(0),   m_deltay2(0),
  m_yorder(0),   
  m_Ntau(0), m_taumin(0), m_taumax(0), m_deltatau(0),   m_tauorder(0), 
  m_Nproc(0),
  m_transform(""), 
  m_qtransform(""), 
  m_transvar(transvar),
  m_lambda(lambda),
  m_reweight(false),
  m_symmetrise(false),
  m_optimised(false),
  m_weight(0),
  m_fg1(0),     m_fg2(0),
  m_fsplit1(0), m_fsplit2(0),
  m_fsplit12(0), m_fsplit22(0),
  m_alphas(0),
  m_taufilledmin(-1),  m_taufilledmax(-1),
  m_y1filledmin(-1),   m_y1filledmax(-1),
  m_y2filledmin(-1),   m_y2filledmax(-1),
  m_partons(13)
{ 
  init_fmap();
  //  std::string qtransform = "h0";
  //  if ( m_lambda==0.25 ) qtransform = "h1";
  set_transforms( m_transform, mfx, mfy );
  set_transforms( m_qtransform,  mfQ2, mftau );

  // std::cout << "igrid() (default) Ntau=" << m_Ntau << "\t" << fQ2(m_taumin) << " - " << fQ2(m_taumax) << std::endl;

} 





// standard constructor
appl::igrid::igrid(int NQ2, double Q2min, double Q2max, int Q2order, 
		   int Nx,  double xmin,  double xmax,  int xorder,  
		   std::string transform, std::string qtransform,  
		   int Nproc, bool disflag ):
  threadManager( label(ithread++) ),
  mfy(0),   mfx(0),
  m_parent(0),
  m_Ny1(Nx),   m_Ny2( disflag ? 1 : Nx ),  m_yorder(xorder), 
  m_Ntau(NQ2), m_tauorder(Q2order), 
  m_Nproc(Nproc), 
  m_transform(transform), 
  m_qtransform(qtransform), 
  m_transvar(transvar),
  m_lambda(lambda),
  m_reweight(false),
  m_symmetrise(false), 
  m_optimised(false),
  m_weight(0),
  m_fg1(0),     m_fg2(0),  
  m_fsplit1(0), m_fsplit2(0),
  m_fsplit12(0), m_fsplit22(0),
  m_alphas(0),
  m_DISgrid(disflag),   
  m_taufilledmin(-1),  m_taufilledmax(-1),
  m_y1filledmin(-1),   m_y1filledmax(-1),
  m_y2filledmin(-1),   m_y2filledmax(-1),
  m_partons(13)
{
  //  std::cout << "igrid::igrid() transform=" << m_transform << std::endl;
  init_fmap();

  //  std::string qtransform = "h0";
  //  if ( m_lambda==0.25 ) qtransform = "h1";
  set_transforms( m_transform,   mfx,  mfy );
  set_transforms( m_qtransform,  mfQ2, mftau );


  double ymin1 = fy(xmax);
  double ymax1 = fy(xmin);

  m_y1min  = ymin1 ;
  m_y1max  = ymax1 ;
  m_y2min  = ymin1 ;
  m_y2max  = ymax1 ;

  if ( m_DISgrid ) m_y2min = m_y2max = 1;

  if ( m_Ny1>1 ) m_deltay1 = (m_y1max-m_y1min)/(m_Ny1-1);
  else           m_deltay1 = 0;
  
  if ( m_Ny2>1 ) m_deltay2 = (m_y2max-m_y2min)/(m_Ny2-1);
  else           m_deltay2 = 0;
  
  double taumin=ftau(Q2min);
  double taumax=ftau(Q2max);
  m_taumin   = taumin;
  m_taumax   = taumax;
  if ( m_Ntau>1 ) m_deltatau = (taumax-taumin)/(m_Ntau-1);
  else            m_deltatau = 0;

  if ( m_Ny1-1<m_yorder ) { 
    std::cerr << "igrid() not enough nodes for this interpolation order Ny1=" << m_Ny1 
	      << "\tyorder=" << m_yorder << std::endl;
 
    while ( m_Ny1-1<m_yorder ) m_yorder--;
  } 

  if ( !m_DISgrid ) { 
    if ( m_Ny2-1<m_yorder ) { 
      std::cerr << "igrid() not enough nodes for this interpolation order Ny2=" << m_Ny2 
		<< "\tyorder=" << m_yorder << std::endl;
      
      while ( m_Ny2-1<m_yorder ) m_yorder--;
    } 
  }
  
  if ( m_Ntau-1<m_tauorder ) { 
    std::cerr << "igrid() not enough nodes for this interpolation order Ntau=" << m_Ntau 
	      << "\ttauorder=" << m_tauorder << std::endl;
    
    while ( m_Ntau-1<m_tauorder ) m_tauorder--;
  } 
  
  m_weight = new SparseMatrix3d*[m_Nproc];
  construct();

  if ( !threads_disabled ) this->start_thread();
}





// copy constructor
appl::igrid::igrid(const appl::igrid& g) :
  threadManager( label(ithread++) ), 
  mfy(0),  mfx(0),  
  m_parent(0),
  m_Ny1(g.m_Ny1),     
  m_y1min(g.m_y1min),     m_y1max(g.m_y1max),     m_deltay1(g.m_deltay1),   
  m_Ny2(g.m_Ny2),     
  m_y2min(g.m_y2min),     m_y2max(g.m_y2max),     m_deltay2(g.m_deltay2),   
  m_yorder(g.m_yorder),   
  m_Ntau(g.m_Ntau), 
  m_taumin(g.m_taumin), m_taumax(g.m_taumax), m_deltatau(g.m_deltatau), m_tauorder(g.m_tauorder), 
  m_Nproc(g.m_Nproc),
  m_transform(g.m_transform), 
  m_qtransform(g.m_qtransform), 
  m_transvar(g.m_transvar),
  m_lambda(g.m_lambda),
  m_reweight(g.m_reweight),
  m_symmetrise(g.m_symmetrise),
  m_optimised(g.m_optimised),
  m_weight(0),
  m_fg1(0),     m_fg2(0),
  m_fsplit1(0), m_fsplit2(0),
  m_fsplit12(0), m_fsplit22(0),
  m_alphas(0),
  m_taufilledmin(g.m_taufilledmin),  m_taufilledmax(g.m_taufilledmax),
  m_y1filledmin(g.m_y1filledmin),    m_y1filledmax(g.m_y1filledmax),
  m_y2filledmin(g.m_y2filledmin),    m_y2filledmax(g.m_y2filledmax),
  m_partons(g.m_partons)
{
  init_fmap();

  //  std::string qtransform = "h0";
  //  if ( m_lambda==0.25 ) qtransform = "h1";
  set_transforms( m_transform, mfx, mfy );
  set_transforms( m_qtransform,  mfQ2, mftau );


  m_weight = new SparseMatrix3d*[m_Nproc];


  for( int ip=0 ; ip<m_Nproc ; ip++ )   m_weight[ip] = new SparseMatrix3d(*g.m_weight[ip]);
  //  construct();

  if ( !threads_disabled ) this->start_thread();
}




void _setlimits( int& _min, int& _max, const int _mint, const int _maxt ) {  
  if ( _mint<=_maxt ) { 
    if ( _min==-1 || _min>_mint ) _min = _mint;
    if ( _max==-1 || _max<_maxt ) _max = _maxt;
  }
}




#ifdef USEROOT
// read from a file 
appl::igrid::igrid(TFile& f, const std::string& s) :
  threadManager( label(ithread++) ), 
  mfy(0),  mfx(0),  
  m_parent(0),
  m_Ny1(0),   m_y1min(0),   m_y1max(0),   m_deltay1(0),   
  m_Ny2(0),   m_y2min(0),   m_y2max(0),   m_deltay2(0),   
  m_yorder(0),   
  m_Ntau(0), m_taumin(0), m_taumax(0), m_deltatau(0), m_tauorder(0), 
  m_Nproc(0),
  m_transform(""), 
  m_qtransform(""), 
  m_transvar(transvar),
  m_lambda(lambda),
  m_reweight(false),
  m_symmetrise(false),
  m_optimised(false),
  m_weight(0), 
  m_fg1(0),     m_fg2(0),
  m_fsplit1(0), m_fsplit2(0),    
  m_fsplit12(0), m_fsplit22(0),    
  m_alphas(0), 
  m_taufilledmin(-1),  m_taufilledmax(-1),
  m_y1filledmin(-1),   m_y1filledmax(-1),
  m_y2filledmin(-1),   m_y2filledmax(-1),
  m_partons(13)
{ 
  //  std::cout << "igrid::igrid()" << std::endl;
  
  // get the name of the transform pair
  TFileString _tag = *(TFileString*)f.Get((s+"/Transform").c_str());
  m_transform = _tag[0];

  TFileString* _qtag = (TFileString*)f.Get((s+"/QTransform").c_str());

  if ( _qtag != 0 ) m_qtransform = _qtag->at(0);
  else              m_qtransform = "h0";


  init_fmap();

  //  std::string qtransform = "h0";
  //  TFileString* _qtag = (TFileString*)f.Get((s+"/TransformQ").c_str());
  //  if ( _qtag ) qtransform = (*_qtag)[0];
  
  //  if ( m_lambda==0.25 ) qtransform = "h1";
  set_transforms( m_transform, mfx, mfy );
  set_transforms( m_qtransform,  mfQ2, mftau );


  // delete _transform;
  
  // retrieve setup parameters 
  TVectorT<double>* setup=(TVectorT<double>*)f.Get((s+"/Parameters").c_str());
  //  f.GetObject((s+"/Parameters").c_str(), setup);

  // NB: round integer variables to nearest integer 
  //     in case (unlikely) truncation error during 
  //     conversion to double when they were stored
  m_Ny1      = int((*setup)(0)+0.5);  
  m_y1min    = (*setup)(1);
  m_y1max    = (*setup)(2);

  m_Ny2      = int((*setup)(3)+0.5);  
  m_y2min    = (*setup)(4);
  m_y2max    = (*setup)(5);

  m_yorder   = int((*setup)(6)+0.5);

  m_Ntau     = int((*setup)(7)+0.5);
  m_taumin   = (*setup)(8);
  m_taumax   = (*setup)(9);
  m_tauorder = int((*setup)(10)+0.5);

  m_transvar = (*setup)(11);

  m_Nproc    = int((*setup)(12)+0.5);

  m_reweight   = ((*setup)(13)!=0 ? true : false );
  m_symmetrise = ((*setup)(14)!=0 ? true : false );
  m_optimised  = ((*setup)(15)!=0 ? true : false );
  m_DISgrid    =  ( setup->GetNoElements()>16 && (*setup)[16]!=0 ) ;

  m_lambda = (*setup)(17); 

  if ( m_lambda==0 ) m_lambda = lambda;

  delete setup;

  //  std::cout << "igrid::igrid() read setup" << std::endl;

  // create grids
  m_deltay1   = (m_y1max-m_y1min)/(m_Ny1-1);
  m_deltay2   = (m_y2max-m_y2min)/(m_Ny2-1);

  m_deltatau = (m_taumax-m_taumin)/(m_Ntau-1);
  
  //  int rawsize=0;
  //  int trimsize=0;

  m_weight = new SparseMatrix3d*[m_Nproc];

  for( int ip=0 ; ip<m_Nproc ; ip++ ) {
    char _name[128];  sprintf(_name,"/weight[%i]", ip );
    // get storage histogram
    TH3D* htmp = (TH3D*)f.Get((s+_name).c_str()); 
 
    //    std::cout << "igrid::igrid() read " << _name << std::endl;

    // create grid
    m_weight[ip]=new SparseMatrix3d(htmp);

    // save some space
    m_weight[ip]->trim();

    // delete storage histogram
    delete htmp;

    // DON'T trim on reading unless the user wants it!!
    // he can trim himself if need be!! 
    // rawsize += m_weight[ip]->size();
    // m_weight[ip]->trim(); // trim the grid and do some book keeping
    // trimsize += m_weight[ip]->size();
  }

  /// now calculate the actual limits of this grid
  
  //  static double _ctime = 0;

  setlimits();

  if ( !threads_disabled ) this->start_thread();

}
#endif






// read from a file 
appl::igrid::igrid( appl::file& file, const std::string& s) :
  threadManager( label(ithread++) ), 
  mfy(0),  mfx(0),  
  m_parent(0),
  m_Ny1(0),   m_y1min(0),   m_y1max(0),   m_deltay1(0),   
  m_Ny2(0),   m_y2min(0),   m_y2max(0),   m_deltay2(0),   
  m_yorder(0),   
  m_Ntau(0), m_taumin(0), m_taumax(0), m_deltatau(0), m_tauorder(0), 
  m_Nproc(0),
  m_transform(""), 
  m_qtransform(""), 
  m_transvar(transvar),
  m_lambda(lambda),
  m_reweight(false),
  m_symmetrise(false),
  m_optimised(false),
  m_weight(0), 
  m_fg1(0),     m_fg2(0),
  m_fsplit1(0), m_fsplit2(0),    
  m_fsplit12(0), m_fsplit22(0),    
  m_alphas(0), 
  m_taufilledmin(-1),  m_taufilledmax(-1),
  m_y1filledmin(-1),   m_y1filledmax(-1),
  m_y2filledmin(-1),   m_y2filledmax(-1),
  m_partons(13)
{ 
  // get the name of the transform pair
  
  stream_vector<std::string> tag = file.Read<stream_vector<std::string> >( s+"/Transform" );
  // stream_vector<std::string> tag; file.Read(tag);

  m_transform = tag[0];

  stream_vector<std::string> qtag = file.Read<stream_vector<std::string> >( s+"/QTransform" );
  // stream_vector<std::string> qtag; file.Read( qtag );

  if ( qtag.size()>0 ) m_qtransform = qtag.at(0);
  else                 m_qtransform = "h0";

  init_fmap();
  
  //  if ( m_lambda==0.25 ) qtransform = "h1";
  set_transforms( m_transform,  mfx,  mfy );
  set_transforms( m_qtransform, mfQ2, mftau );


  // delete _transform;
  
  // retrieve setup parameters 

  stream_vector<double> setup = file.Read<stream_vector<double> >( s+"/Parameters" );
  // stream_vector<double> setup; file.Read( setup );
  
  // NB: round integer variables to nearest integer 
  //     in case (unlikely) truncation error during 
  //     conversion to double when they were stored
  m_Ny1      = int(setup[0]+0.5);  
  m_y1min    = setup[1];
  m_y1max    = setup[2];
  
  m_Ny2      = int(setup[3]+0.5);  
  m_y2min    = setup[4];
  m_y2max    = setup[5];
  
  m_yorder   = int(setup[6]+0.5);
  
  m_Ntau     = int(setup[7]+0.5);
  m_taumin   = setup[8];
  m_taumax   = setup[9];
  m_tauorder = int(setup[10]+0.5);
  
  m_transvar = setup[11];
  
  m_Nproc    = int(setup[12]+0.5);
  
  m_reweight   = (setup[13]!=0 ? true : false );
  m_symmetrise = (setup[14]!=0 ? true : false );
  m_optimised  = (setup[15]!=0 ? true : false );
  m_DISgrid    = ( setup.size()>16 && setup[16]!=0 ) ;
  
  m_lambda = setup[17]; 
  
  if ( m_lambda==0 ) m_lambda = lambda;
  
  //  std::cout << "igrid::igrid() read setup" << std::endl;
  
  // create grids
  m_deltay1   = (m_y1max-m_y1min)/(m_Ny1-1);
  m_deltay2   = (m_y2max-m_y2min)/(m_Ny2-1);
  
  m_deltatau = (m_taumax-m_taumin)/(m_Ntau-1);
  
  //  int rawsize=0;
  //  int trimsize=0;
 
  m_weight = new SparseMatrix3d*[m_Nproc];
  
  for( int ip=0 ; ip<m_Nproc ; ip++ ) {
    char _name[128];  sprintf(_name,"%s/weight[%i]", s.c_str(), ip );

    // get storage histogram
 
    stream_grid htmp = file.Read<stream_grid>( _name );
    // stream_grid htmp; file.Read( htmp );

    // create grid
    m_weight[ip]=new SparseMatrix3d(htmp);

    // save some space
    m_weight[ip]->trim();

    // delete storage histogram

    // DON'T trim on reading unless the user wants it!!
    // he can trim himself if need be!! 
    // rawsize += m_weight[ip]->size();
    // m_weight[ip]->trim(); // trim the grid and do some book keeping
    // trimsize += m_weight[ip]->size();
  }

  /// now calculate the actual limits of this grid
  
  //  static double _ctime = 0;

  setlimits();

  if ( !threads_disabled ) this->start_thread();


}







void appl::igrid::setlimits() { 
  if ( m_weight ) { 
    //   struct timeval _tstart = appl_timer_start(); 
    for ( int i=0 ; i<m_Nproc ; i++ ) {
      const SparseMatrix3d* _weight = m_weight[i];
      if ( _weight ) { 
	if ( _weight->empty() ) continue;
	_setlimits( m_taufilledmin, m_taufilledmax,  _weight->xmin(),  _weight->xmax() ); 
	_setlimits( m_y1filledmin,  m_y1filledmax,   _weight->ymin(),  _weight->ymax() ); 
	_setlimits( m_y2filledmin,  m_y2filledmax,   _weight->zmin(),  _weight->zmax() ); 
      }
    }
  }
}    



// constructor common internals 
void appl::igrid::construct() 
{
  // Initialize histograms representing the weight grid
  for( int ip=0 ; ip<m_Nproc ; ip++ ) {
    m_weight[ip] = new SparseMatrix3d(m_Ntau, m_taumin,   m_taumax,    
    				      m_Ny1,  m_y1min,    m_y1max, 
    				      m_Ny2,  m_y2min,    m_y2max ); 
    
  }  
}


// destructor
appl::igrid::~igrid() {
  deletepdftable();
  deleteweights();
}




double*** delete_array( double*** a, unsigned Nx, unsigned Ny ) { 
  if ( a ) { 
    for ( unsigned i=0 ; i<Nx ; i++ ) {
      if ( a[i] ) for ( unsigned j=0 ; j<Ny ; j++ )  delete[] a[i][j];
      delete[] a[i];
    }
    delete[] a;
  }
  return 0;
}


// clean up internal pdf table (could be moved to local variable)
void appl::igrid::deletepdftable() { 

}





// delete internal grids
void appl::igrid::deleteweights() { 
  if ( m_weight ) { 
    for ( int ip=0 ; ip<m_Nproc ; ip++ ) if (m_weight[ip]) delete m_weight[ip];
    delete[] m_weight;
    // m_weight=0;
  }
}



#ifdef USEROOT

// write to file
void appl::igrid::write( const std::string& _name ) { 
  Directory d(_name);
  d.push();

  // using a TH1D to store the transform pair tag since I don't know how 
  // to write a TString to a root file
  // TH1D* _transform = new TH1D("Transform", m_transform.c_str(), 1, 0, 1);
  //  _transform->Write();

  // write the name of the transform pair
  TFileString("Transform", m_transform).Write();
  TFileString("QTransform", m_qtransform).Write();


  TVectorT<double>* setup=new TVectorT<double>(20); // a few spare

  (*setup)(0)  = m_Ny1;
  (*setup)(1)  = m_y1min;
  (*setup)(2)  = m_y1max;

  (*setup)(3)  = m_Ny2;
  (*setup)(4)  = m_y2min;
  (*setup)(5)  = m_y2max;

  (*setup)(6)  = m_yorder;

  (*setup)(7)  = m_Ntau;
  (*setup)(8)  = m_taumin;
  (*setup)(9)  = m_taumax;
  (*setup)(10) = m_tauorder;

  (*setup)(11) = m_transvar;

  (*setup)(12) = m_Nproc;
 
  (*setup)(13) = ( m_reweight   ? 1 : 0 );
  (*setup)(14) = ( m_symmetrise ? 1 : 0 );
  (*setup)(15) = ( m_optimised  ? 1 : 0 );
  (*setup)(16) = ( m_DISgrid    ? 1 : 0 );

  (*setup)(17) = m_lambda;

  setup->Write("Parameters");

  int igridsize     = 0;
  int igridtrimsize = 0;

  for ( int ip=0 ; ip<m_Nproc ; ip++ ) { 

    char hname[128];
    //    sprintf(hname,"%s[%d]", _name.c_str(), ip);
    sprintf(hname,"weight[%d]", ip);

    //    int oldsize = m_weight[ip]->size();

    igridsize += m_weight[ip]->size();
 
    // trim it so that it's quicker to copy into the TH3D
    m_weight[ip]->trim();

    igridtrimsize += m_weight[ip]->size();

    TH3D* h=m_weight[ip]->getTH3D(hname);
    h->SetDirectory(0);
    h->Write();
    delete h; // is this dengerous??? will root try to delete it later? I guess not if we SetDirectory(0)
  }

#if 0
  //    std::cout << _name << " trimmed" << std::endl;
  std::cout << _name << "\tsize=" << igridsize << "\t-> " << igridtrimsize;
  if ( igridsize ) std::cout << "\t( " << igridtrimsize*100/igridsize << "% )";
  std::cout << std::endl;
#endif

  d.pop();
}

#endif





// write to file
void appl::igrid::write( appl::file& _file, const std::string& _name) { 

  // write the name of the transform pair

  _file.Write( stream_vector<std::string>( _name+"/Transform",  std::vector<std::string>(1,m_transform) ) );
  _file.Write( stream_vector<std::string>( _name+"/QTransform", std::vector<std::string>(1,m_qtransform) ) );

  std::vector<double> setup(20); // a few spare

  setup[0]  = m_Ny1;
  setup[1]  = m_y1min;
  setup[2]  = m_y1max;

  setup[3] = m_Ny2;
  setup[4]  = m_y2min;
  setup[5]  = m_y2max;

  setup[6]  = m_yorder;

  setup[7]  = m_Ntau;
  setup[8]  = m_taumin;
  setup[9]  = m_taumax;
  setup[10] = m_tauorder;

  setup[11] = m_transvar;

  setup[12] = m_Nproc;
 
  setup[13] = ( m_reweight   ? 1 : 0 );
  setup[14] = ( m_symmetrise ? 1 : 0 );
  setup[15] = ( m_optimised  ? 1 : 0 );
  setup[16] = ( m_DISgrid    ? 1 : 0 );

  setup[17] = m_lambda;

  _file.Write( stream_vector<double>( _name+"/Parameters", setup ) );

  int igridsize     = 0;
  int igridtrimsize = 0;

  for ( int ip=0 ; ip<m_Nproc ; ip++ ) { 

    char hname[128];
    //    sprintf(hname,"%s[%d]", _name.c_str(), ip);
    sprintf(hname,"%s/weight[%d]", _name.c_str(), ip);

    //    int oldsize = m_weight[ip]->size();

    igridsize += m_weight[ip]->size();
 
    // trim it so that it's quicker to copy into the TH3D
    m_weight[ip]->trim();

    igridtrimsize += m_weight[ip]->size();

    stream_grid* h = m_weight[ip]->get(hname);

    //    std::cout << *h << std::endl;

    
    _file.Write( *h );

    delete h; 
  }

#if 0
  //    std::cout << _name << " trimmed" << std::endl;
  std::cout << _name << "\tsize=" << igridsize << "\t-> " << igridtrimsize;
  if ( igridsize ) std::cout << "\t( " << igridtrimsize*100/igridsize << "% )";
  std::cout << std::endl;
#endif

  //  d.pop();


}





void appl::igrid::fill_DIS(const double x1, const double Q2, const double* weight) 
{  

  // find preferred node for low end of interpolation range
  int k1=fk1(x1);
  int k3=fkappa(Q2);
  
  double u_y1  = ( fy(x1)-gety1(k1) )/deltay1();
  double u_tau = ( ftau(Q2)-gettau(k3))/deltatau();
  
  double fillweight, fI_factor;
  
  // cache the interpolation coefficients so only 
  // have to calculate each one once 
  // NB: a (very naughty) hardcoded maximum interpolation order 
  //     of 16 for code simplicity.
  double _fI1[16];
  double _fI3[16];
  
  for( int i1=0 ; i1<=m_yorder   ; i1++ )  _fI1[i1] = fI(i1, m_yorder, u_y1);
  for( int i3=0 ; i3<=m_tauorder ; i3++ )  _fI3[i3] = fI(i3, m_tauorder, u_tau);
  
  double invwfun = 1/(weightfun(x1));
	
  //  double fI_tmp0 = 0;
  //  double fI_tmp1 = 0;

  for( int i3=0 ; i3<=m_tauorder ; i3++ ) { // "interpolation" loop in Q2

    //	  (*m_weight[ip])(k3+i3, k1+i1, k2+i2) += fillweight;	  

    //    if ( m_reweight ) fI_tmp0 = _fI3[i3]*invwfun;
    //    else              fI_tmp0 = _fI3[i3];

    for( int i1=0 ; i1<=m_yorder ; i1++ )   { // "interpolation" loop in x1      

      //      fI_tmp1 = _fI1[i1] * fI_tmp0;
      
      fI_factor = _fI1[i1] * _fI3[i3];

      if ( m_reweight ) fI_factor *= invwfun;
						 						 
      for( int ip=0 ; ip<m_Nproc ; ip++ ) {
	  
	fillweight = weight[ip] * fI_factor;

	// this method only works for grids where the x1, x2 axes are identical 
	// ranges and binning 
	//  if ( m_symmetrise ) { 
	//	// symmetrise the grid
	//    if ( abs(k1+i1)<abs(k2+i2) )  (*m_weight[ip])(k3+i3, k2+i2, k1+i1) += fillweight;   
	//    else                          (*m_weight[ip])(k3+i3, k1+i1, k2+i2) += fillweight;  
	//  } 
	
	//	  std::cout << " (" << (*m_weight[ip])(k3+i3, k1+i1, k2+i2) << " -> ";
	
	(*m_weight[ip])(k3+i3, k1+i1, 0) += fillweight;	  
	
	//	  std::cout << (*m_weight[ip])(k3+i3, k1+i1, k2+i2) << ")";
	
	//	  m_weight[ip]->print();	  
	//	  m_weight[ip]->fill_fast(k3+i3, k1+i1, k2+i2) += fillweight/wfun;	  
      }
      
    }
  }
}





void appl::igrid::fill(const double x1, const double x2, const double Q2, const double* weight) 
{  
  if ( isDISgrid() ) return fill_DIS( x1, Q2, weight );

  // find preferred node for low end of interpolation range
  int k1=fk1(x1);
  int k2=fk2(x2);
  int k3=fkappa(Q2);
  
  double u_y1  = ( fy(x1)-gety1(k1) )/deltay1();
  double u_y2  = ( fy(x2)-gety2(k2) )/deltay2();
  double u_tau = ( ftau(Q2)-gettau(k3))/deltatau();
  
  double fillweight, fI_factor;
  
  // cache the interpolation coefficients so only 
  // have to calculate each one once 
  // NB: a (very naughty) hardcoded maximum interpolation order 
  //     of 16 for code simplicity.
  double _fI1[16];
  double _fI2[16];
  double _fI3[16];
  
  for( int i1=0 ; i1<=m_yorder   ; i1++ )  _fI1[i1] = fI(i1, m_yorder, u_y1);
  for( int i2=0 ; i2<=m_yorder   ; i2++ )  _fI2[i2] = fI(i2, m_yorder, u_y2);
  for( int i3=0 ; i3<=m_tauorder ; i3++ )  _fI3[i3] = fI(i3, m_tauorder, u_tau);
  
  double invwfun = 1/(weightfun(x1)*weightfun(x2));
	
  //  double fI_tmp0 = 0;
  //  double fI_tmp1 = 0;

  for( int i3=0 ; i3<=m_tauorder ; i3++ ) { // "interpolation" loop in Q2

    //	  (*m_weight[ip])(k3+i3, k1+i1, k2+i2) += fillweight;	  

    //    if ( m_reweight ) fI_tmp0 = _fI3[i3]*invwfun;
    //    else              fI_tmp0 = _fI3[i3];

    for( int i1=0 ; i1<=m_yorder ; i1++ )   { // "interpolation" loop in x1      

      //      fI_tmp1 = _fI1[i1] * fI_tmp0;
      
      for( int i2=0 ; i2<=m_yorder ; i2++ ) { // "interpolation" loop in x2
	
	// fI_factor = fI_tmp1 * _fI2[i2];

	fI_factor = _fI1[i1] * _fI2[i2] * _fI3[i3];

	if ( m_reweight ) fI_factor *= invwfun;
						 						 
	for( int ip=0 ; ip<m_Nproc ; ip++ ) {
	  
	  fillweight = weight[ip] * fI_factor;

	  // this method only works for grids where the x1, x2 axes are identical 
	  // ranges and binning 
	  //  if ( m_symmetrise ) { 
	  //	// symmetrise the grid
	  //    if ( abs(k1+i1)<abs(k2+i2) )  (*m_weight[ip])(k3+i3, k2+i2, k1+i1) += fillweight;   
	  //    else                          (*m_weight[ip])(k3+i3, k1+i1, k2+i2) += fillweight;  
	  //  } 

	  //	  std::cout << " (" << (*m_weight[ip])(k3+i3, k1+i1, k2+i2) << " -> ";

	  (*m_weight[ip])(k3+i3, k1+i1, k2+i2) += fillweight;	  
	 	  
	  //	  std::cout << (*m_weight[ip])(k3+i3, k1+i1, k2+i2) << ")";

	  //	  m_weight[ip]->print();	  
	  //	  m_weight[ip]->fill_fast(k3+i3, k1+i1, k2+i2) += fillweight/wfun;	  
	}

	//	std::cout << std::endl;
      }
    }
  }
}


void appl::igrid::fill_phasespace(const double x1, const double x2, const double Q2, const double* weight) { 

  if ( isDISgrid() ) return fill_DIS_phasespace( x1, Q2, weight ); 

  int k1=fk1(x1);
  int k2=fk2(x2);
  int k3=fkappa(Q2);

  //  std::cout << "appl::igrid::fill_phasespace() k: " << k1 << " " << k2 << " " << k3 << std::endl;
  //  std::cout << "\tweights: ";

  //  for ( int ip=0 ; ip<m_Nproc ; ip++ ) std::cout << " " << weight[ip];
  //  std::cout << std::endl;

  for ( int ip=0 ; ip<m_Nproc ; ip++ ) (*m_weight[ip])(k3, k1, k2) += weight[ip];

  //  for ( int ip=0 ; ip<m_Nproc ; ip++ ) std::cout << "\t" << ip << "\tsparse size " << m_weight[ip]->size() << std::endl; 

  //  for ( int ip=0 ; ip<m_Nproc ; ip++ ) std::cout << "\t" << ip << "\tweight " << (*m_weight[ip])(k3, k1, k2) << std::endl; 

} 



void appl::igrid::fill_DIS_phasespace(const double x1, const double Q2, const double* weight) { 

  int k1=fk1(x1);
  int k3=fkappa(Q2);

  //  std::cout << "appl::igrid::fill_phasespace() k: " << k1 << " " << k2 << " " << k3 << std::endl;
  //  std::cout << "\tweights: ";

  //  for ( int ip=0 ; ip<m_Nproc ; ip++ ) std::cout << " " << weight[ip];
  //  std::cout << std::endl;

  for ( int ip=0 ; ip<m_Nproc ; ip++ ) (*m_weight[ip])(k3, k1, 0) += weight[ip];

  //  for ( int ip=0 ; ip<m_Nproc ; ip++ ) std::cout << "\t" << ip << "\tsparse size " << m_weight[ip]->size() << std::endl; 

  //  for ( int ip=0 ; ip<m_Nproc ; ip++ ) std::cout << "\t" << ip << "\tweight " << (*m_weight[ip])(k3, k1, k2) << std::endl; 

} 





void appl::igrid::fill_index(const int ix1, const int ix2, const int iQ2, const double* weight) { 

  //  int k1=ix1;
  //  int k2=ix2;
  //  int k3=iQ2;

  //  for ( int ip=0 ; ip<m_Nproc ; ip++ ) (*m_weight[ip])(i3, k1, k2) += weight[ip];

  for ( int ip=0 ; ip<m_Nproc ; ip++ ) (*m_weight[ip])(iQ2, ix1, ix2) += weight[ip];

} 




void appl::igrid::setuppdf(double (*alphas)(const double&),
			   NodeCache* pdf0,
			   NodeCache* pdf1,
			   int _nloop,
			   double rscale_factor,
			   double fscale_factor,
			   double beam_scale0, 
			   double beam_scale1 ) 
{

  /// if this igrid is empty, don't do anything 

  //  std::cout << m_taufilledmin << " " << m_taufilledmax << " " 
  //	        << m_y1filledmin  << " " << m_y1filledmax  << " " 
  //	        << m_y2filledmin  << " " << m_y2filledmax  << std::endl;

  if ( ( m_taufilledmin==-1 || m_taufilledmax==-1 ) ||
       (  m_y1filledmin==-1 ||  m_y1filledmax==-1 ) ) return;

  if ( !isDISgrid() && (  m_y2filledmin==-1 ||  m_y2filledmax==-1 ) ) return; 
  

  int nloop = std::fabs(_nloop);

  if ( pdf1==0 ) pdf1 = pdf0;

  bool initialise_hoppet = false;


  bool use_split = (  ( fscale_factor != 1 )  || fuserscale );

#ifdef HAVE_HOPPET
  // check if we need to use the splitting function, and if so see if we 
  // need to initialise it again, and do so if required ???
  if ( use_split ) {
    if ( pdf1 && pdf1!=pdf0 ) initialise_hoppet = true;
  }
#endif


  void (*splitting)(const double& , const double&, double*, int ) = Splitting;
		     
  //  static const double twopi = 2*M_PI;

  // static const int nc = 3;
  //// TC   const int nf = 6;
  //  static const int nf = 5;
  //  static double beta0=(11.*nc - 2.*nf)/(6*twopi);
  //  static double beta0=(11.*nc - 2.*nf)/6.;
  
  /// not needed here ?? 
  //  static double beta1=(34.*nc*nc - 3.*(nc-1./nc)*nf - 10.*nc*nf)/(6.*twopi);

  double logf_save = std::log( fscale_factor*fscale_factor );
  //  double logr = std::log( rscale_factor*rscale_factor );

  double logf = logf_save;

  // pdf table

  const double invtwopi = 0.5/(M_PI);

  m_alphas = std::vector<double>( Ntau(), 0 );
  
  m_fg1 = std::vector< std::vector< std::vector<double> > >( Ntau(), std::vector<std::vector<double> >(Ny1(), std::vector<double>(14,0) ) );  
  if ( !isSymmetric() && !isDISgrid() ) { 
    m_fg2 = std::vector< std::vector< std::vector<double> > >( Ntau(), std::vector<std::vector<double> >(Ny2(), std::vector<double>(14,0) ) );  
  }
  
  if ( nloop>=1 && use_split ) { 
    m_fsplit1 = std::vector< std::vector< std::vector<double> > >( Ntau(), std::vector<std::vector<double> >(Ny1(), std::vector<double>(14,0) ) );  
    if ( !isSymmetric() && !isDISgrid() ) { 
      m_fsplit2 = std::vector< std::vector< std::vector<double> > >( Ntau(), std::vector<std::vector<double> >(Ny2(), std::vector<double>(14,0) ) );  
    }
    if ( nloop==2 ) { 
      m_fsplit12 = std::vector< std::vector< std::vector<double> > >( Ntau(), std::vector<std::vector<double> >(Ny1(), std::vector<double>(14,0) ) );  
      if ( !isSymmetric() && !isDISgrid() ) { 
	m_fsplit22 = std::vector< std::vector< std::vector<double> > >( Ntau(), std::vector<std::vector<double> >(Ny2(), std::vector<double>(14,0) ) );  
      }
    }
  }


  const int n_y1  = Ny1();
  const int n_y2  = Ny2();

  
  bool scale_beams = false;
  if ( beam_scale0!=1 || beam_scale1!=1 ) scale_beams = true;

  if ( initialise_hoppet ) hoppet_init::assign( pdf0->pdf() );
  
  // set up pdf grid, splitting function grid 
  // and alpha_s grid
  //  for ( int itau=m_Ntau ; itau-- ; ) {
  for ( int itau=0 ; itau<m_Ntau ; itau++  ) {
    
    if ( itau<m_taufilledmin || itau>m_taufilledmax ) continue;

    double tau = gettau(itau);
    double Q2  = fQ2(tau);
    double fQ  = std::sqrt(Q2); 
    
    double Q = fQ;

#if 1
    /// not iplemented yet ...
    if ( fuserscale ) { 
      Q = fuserscale(fQ);
      double scale_factor = Q/fQ;
      logf = logf_save + 2*std::log(scale_factor);  
    }
#endif

    // alpha_s table 
    m_alphas[itau] = alphas(rscale_factor*Q)*invtwopi;

#if 0

    std::cout << itau << "\ttau " << tau 
	      << "\tQ2 " << Q2 << "\tQ " << Q 
	      << "\talphas " << m_alphas[itau] << std::endl; 


    int iymin1 = m_weight[0]->ymin();
    int iymax1 = m_weight[0]->ymax();
    
    if ( isSymmetric() ) { 
      int iymin2 = iymin1;
      int iymax2 = iymax1;
    }
    else {
      int iymin2 = m_weight[0]->zmin();
      int iymax2 = m_weight[0]->zmax();
    }  
#endif

    // grid not filled for iy<iymin || iy>iymax so no need to 
    // consider outside this range
    
    // y1 tables
    for ( int iy=n_y1 ; iy-- ;  ) { 
      
      if ( iy<m_y1filledmin || iy>m_y1filledmax ) continue;

      double y = gety1(iy);
      double x = fx(y);
      double fun = 1;

#if 0      
      static bool ff = true;

      if ( ff ) { std::cout << "ff: " << m_reweight << " " << fun << std::endl; ff = false; } 

      if ( itau==0 ) { 
	std::cout << iy << "  y: " << y << "  x: " << x << std::endl; 
      }
#endif

      if ( scale_beams ) x *= beam_scale0;

      if ( x>=1 ) { 
	for ( int ip=0 ; ip<m_partons ; ip++ ) m_fg1[itau][iy][ip]=0; 
	if ( nloop>=1 && use_split ) { 
	  for ( int ip=0 ; ip<m_partons ; ip++ ) m_fsplit1[itau][iy][ip] = 0;
	  if (nloop==2)
	    for ( int ip=0 ; ip<m_partons ; ip++ ) m_fsplit12[itau][iy][ip] = 0;
	}
	continue; 
      }    

      if ( m_reweight ) fun = weightfun(x);

      pdf0->evaluate(x, fscale_factor*Q, m_fg1[itau][iy]);
      
      //      std::cout << "PDF1: itau " << itau << "   Q: " << Q << " ( " << fscale_factor*Q << " )    iy1: " << iy << "   x: " << x << "  g-pdf: " << m_fg1[itau][iy][6] << " - " << (m_fg1[itau][iy][6]/x) << std::endl; 
     
      double invx = 1/x;
      for ( int ip=0 ; ip<m_partons ; ip++ ) m_fg1[itau][iy][ip] *= invx;
      if ( m_reweight ) for ( int ip=0 ; ip<m_partons ; ip++ ) m_fg1[itau][iy][ip] *= fun;
      
      // splitting function table
      if ( nloop>=1 && use_split ) { 

	double PLO[14]; for ( int ip=14 ; ip-- ; ) PLO[ip] = 0;  

	splitting( x, fscale_factor*Q, PLO, 1 ); /// PLO x pdf	

	for ( int ip=m_partons ; ip-- ; ) m_fsplit1[itau][iy][ip] = PLO[ip]*invx;

	if ( m_reweight ) for ( int ip=m_partons ; ip-- ; ) m_fsplit1[itau][iy][ip] *= fun;

	if (nloop>1) {
	  //	splitting( x, fscale_factor*Q, m_fsplit12[itau][iy], 1 ); placeholder for hoppet call

	  double P2LO[14]; for ( int ip=14 ; ip-- ; ) P2LO[ip] = 0;  

	  double PNLO[14]; for ( int ip=14 ; ip-- ; ) PNLO[ip] = 0;  

	  splitting( x, fscale_factor*Q, P2LO, 11 ); /// PLO x PLO x pdf
	  splitting( x, fscale_factor*Q, PNLO, 2 );  /// PNLO x pdf

	  //	  for ( int ip=0 ; ip<13 ; ip++ ) m_fsplit12[itau][iy][ip] = logf*(beta0*(0.5*logf - logr)*PLO[ip] + 0.5*logf*P2LO[ip] - PNLO[ip] )*invx ;
	  //	  for ( int ip=0 ; ip<13 ; ip++ ) m_fsplit12[itau][iy][ip] = logf*(beta0*(0.5*logf - logr)*PLO[ip] + 0.5*logf*P2LO[ip] - PNLO[ip] )*invx ;

	  for ( int ip=0 ; ip<m_partons ; ip++ ) m_fsplit12[itau][iy][ip] = logf*( 0.5*logf*P2LO[ip] - PNLO[ip] )*invx ;

	  if ( m_reweight ) for ( int ip=0 ; ip<m_partons ; ip++ ) m_fsplit12[itau][iy][ip] *= fun;

	}
      }
    }
  }

  if ( initialise_hoppet ) hoppet_init::assign( pdf1->pdf() );
  
  if ( ( !isSymmetric() && !isDISgrid() ) || ( pdf1 && pdf1!=pdf0 ) ) {
    
    for ( int itau=0 ; itau<m_Ntau ; itau++  ) {
 
      if ( itau<m_taufilledmin || itau>m_taufilledmax ) continue;

      double tau = gettau(itau);
      double Q2  = fQ2(tau);
      double fQ  = std::sqrt(Q2); 
    
      double Q = fQ;

#if 1     
      /// not iplemented yet ...
      if ( fuserscale ) { 
	Q = fuserscale(fQ);
	double scale_factor = Q/fQ;
	logf = logf_save + 2*std::log(scale_factor);  
      }
#endif     
 
      /// alpha_s table has already been filled  
      /// m_alphas[itau] = alphas(rscale_factor*Q)*invtwopi;
      
      // y2 tables
      //    for ( int iy=iymin2 ; iy<=iymax2 ; iy++ ) { 
      for ( int iy=n_y2 ; iy-- ;  ) { 

	if ( iy<m_y2filledmin || iy>m_y2filledmax ) continue;
		
	double y = gety2(iy);
	double x = fx(y);
	double fun = 1;

	if ( scale_beams ) x *= beam_scale1;
	
	if ( x>=1 ) { 
	  for ( int ip=0 ; ip<m_partons ; ip++ ) m_fg2[itau][iy][ip]=0; 
	  if ( nloop>=1 && use_split ) { 
	    for ( int ip=0 ; ip<m_partons ; ip++ ) m_fsplit2[itau][iy][ip] = 0;
	    if (nloop==2)
	      for ( int ip=0 ; ip<m_partons ; ip++ ) m_fsplit22[itau][iy][ip] = 0;
	  }
	  continue; 
	}
      
	if (m_reweight) fun = weightfun(x);
	      
	pdf1->evaluate(x, fscale_factor*Q, m_fg2[itau][iy]);

	//	std::cout << "PDF2: itau " << itau << "   Q: " << Q << " ( " << fscale_factor*Q << " )    iy1: " << iy << "   x: " << x << "  g-pdf: " << m_fg2[itau][iy][6] << " - " << (m_fg2[itau][iy][6]/x) << std::endl; 
	
	double invx = 1/x;
	for ( int ip=0 ; ip<m_partons ; ip++ ) m_fg2[itau][iy][ip] *= invx;
	if ( m_reweight ) for ( int ip=0 ; ip<m_partons ; ip++ ) m_fg2[itau][iy][ip] *= fun;      
	
	// splitting functions
	if ( nloop>=1 && use_split ) { 

	  double PLO[14]; for ( int ip=14 ; ip-- ; ) PLO[ip] = 0;  
	  splitting( x, fscale_factor*Q, PLO, 1 ); /// PLO x pdf
	  for ( int ip=m_partons ; ip-- ; ) m_fsplit2[itau][iy][ip] = PLO[ip]*invx;
	  if ( m_reweight ) for ( int ip=m_partons ; ip-- ; ) m_fsplit2[itau][iy][ip] *= fun;
	  
	  if (nloop > 1) {
	    //splitting( x, fscale_factor*Q, m_fsplit22[itau][iy], 1 );
 
	    double P2LO[14]; for ( int ip=14 ; ip-- ; ) P2LO[ip] = 0;  
	    double PNLO[14]; for ( int ip=14 ; ip-- ; ) PNLO[ip] = 0;  
	    
	    splitting( x, fscale_factor*Q, P2LO, 11 ); /// PLO x PLO x pdf
	    splitting( x, fscale_factor*Q, PNLO, 2 );  /// PNLO x pdf
	    
	    //	    for ( int ip=0 ; ip<13 ; ip++ ) m_fsplit22[itau][iy][ip] = 0.5*logf*(beta0*(logf - 2*logr)*PLO[ip] + logf*P2LO[ip] - 2*PNLO[ip] )*invx ;

  	    for ( int ip=0 ; ip<m_partons ; ip++ ) m_fsplit22[itau][iy][ip] = logf*( 0.5*logf*P2LO[ip] - PNLO[ip] )*invx ;
 
	    
	    if ( m_reweight ) for ( int ip=0 ; ip<m_partons ; ip++ ) m_fsplit22[itau][iy][ip] *= fun;

	  }
	}
      }


    } // loop over itau

  } // isSymmetric()

  if ( isSymmetric() && !isDISgrid() ) { 
    m_fg2      = m_fg1;
    m_fsplit2  = m_fsplit1;
    m_fsplit22 = m_fsplit12;
  }

}




#if 0
void igrid::pdfinterp(double x, double Q2, double* f)
{
  int k1=fk1(x);
  int k3=fkappa(Q2);

  double u_y1  = ( fy(x)-gety1(k1) )/deltay1();
  double u_tau = ( ftau(Q2)-gettau(k3))/deltatau();

  //  double rint = 0.;

  for ( int i=0 ; i<14 ; i++ ) f[i]=0.;

  double fI_factor;

  for( int i3=0 ; i3<=m_tauorder ; i3++ ) { // interpolation loop in Q2
   
    //    double fI3 = fI(i3, m_tauorder, u_tau);

    for( int i1=0 ; i1<=m_yorder ; i1++ ) { // interpolation loop in x1
      
      //      double fI1 = fI(i1, m_yorder, u_y1); 

      fI_factor=fI(i1, m_yorder, u_y1) * fI(i3, m_tauorder, u_tau);
                    
      for ( int ip1=0 ; ip1<14 ; ip1++ ) { 
	f[ip1] += fI_factor*m_fg1[k3+i3][std::abs(k1+i1)][ip1];     
      }
    }
  }
   
  double fun = weightfun(x); 

  if ( m_reweight ) for ( int ip1=0 ; ip1<14 ; ip1++ ) f[ip1] /= fun;

}
#endif




// takes pdf as the pdf lib wrapper for the pdf set for the convolution.
// takes genpdf as a function to form the generalised parton distribution.
// alphas is a function for the calculation of alpha_s (surprisingly)
// lo_order is the order of the calculation, ie for inclusive, or two jets, 
// it's O(alpha_s) (ie lo_order=1) and for 3 jet it would be O(alpha_s^2)
// (ie lo_order=2)
// nloop is the number of loops, ie for the leading order component it is
// 0, for the nlo component, interestingly it is also 0, since the leading order
// grids are seperate from the nlo grids, if nloop=1 then we must be calculating 
// {r,f}scale_factor ie f*mu then *scale_factor=f
// splitting, is the splitting function 

double appl::igrid::convolute(NodeCache* pdf0,
			      NodeCache* pdf1,
			      appl_pdf*  genpdf,
			      double (*alphas)(const double& ), 
			      int     lo_order,  
			      int     _nloop, 
			      double  rscale_factor,
			      double  fscale_factor,
			      double Escale0, 
			      double Escale1 ) 
{ 

  //  double Escale = Escale0;

  //  m_transvar = m_transvarlocal;

  if ( pdf1==0 ) pdf1 = pdf0; 


  m_conv_param.pdf0 = pdf0;
  m_conv_param.pdf1 = pdf1;
  m_conv_param.alphas = alphas;


  m_conv_param.lo_order =  lo_order;
  m_conv_param._nloop   = _nloop;
  m_conv_param.Escale0   =  Escale0;
  m_conv_param.Escale1   =  Escale1;

  m_conv_param.rscale_factor = rscale_factor;
  m_conv_param.fscale_factor = fscale_factor;

  m_conv_param.genpdf = genpdf;

  m_conv_param.dsigma     = 0;
  m_conv_param.dsigmaNLO  = 0;
  m_conv_param.dsigmaNNLO = 0;

  double dsigma     = 0; 
  //  double dsigmaNLO  = 0; 
  //  double dsigmaNNLO = 0; 

  // std::cout << name() << "\n" << *dynamic_cast<const lumi_pdf*>(genpdf) << std::endl;
  
#ifndef PDFTHREAD

  // is the grid empty
  int size=0;
  for ( int ip=0 ; ip<m_Nproc ; ip++ ) {
    //    std::cout << "\t" << name() << " " << ip << " :: " << m_weight[ip]->empty() << std::endl; 
    if ( m_weight[ip]->empty() ) continue;
    if ( !m_weight[ip]->trimmed() ) m_weight[ip]->trim();
    //    size += m_weight[ip]->xmax() - m_weight[ip]->xmin() + 1;
    size++;
  }

  //  std::cout << "\t" << name() << "  :: size: " << size << std::endl; 
    
  // grid is empty
  if ( size==0 )  return 0;

  int nloop = std::fabs(_nloop); 

  setuppdf( alphas, pdf0, pdf1, nloop, rscale_factor, fscale_factor, Escale0, Escale1 );

#endif

  if ( threads_disabled ) convolute_internal();
  else                    process();            

  dsigma     = m_conv_param.dsigma; 
  //  dsigmaNLO  = m_conv_param.dsigmaNLO; 
  //  dsigmaNNLO = m_conv_param.dsigmaNNLO; 

  return dsigma;
}







void appl::igrid::convolute_internal() { 

  //  struct timeval mytimer = appl_timer_start();

  int     lo_order = m_conv_param.lo_order;  
  int     _nloop   = m_conv_param._nloop;  

  int      nloop = std::fabs(_nloop); 

  double  rscale_factor = m_conv_param.rscale_factor;  
  double  fscale_factor = m_conv_param.fscale_factor;  

  appl_pdf* genpdf = m_conv_param.genpdf;

  //  static const double twopi = 2*M_PI;
  //  static const int nc = 3;
  //  //TC   const int nf = 6;
  //  static const int nf = 5;
  //  static double beta0=(11.*nc-2.*nf)/(6.*twopi);
  //  const bool debug=false;  


  double dsigma     = 0.; //, xsigma = 0.;
  double dsigmaNLO  = 0.; //, xsigma = 0.;
  double dsigmaNNLO = 0.; //, xsigma = 0.;

  int size=0;
  for ( int ip=0 ; ip<m_Nproc ; ip++ ) {
    if ( m_weight[ip]->empty() ) continue;
    if ( !m_weight[ip]->trimmed() ) m_weight[ip]->trim();
    //    size += m_weight[ip]->xmax() - m_weight[ip]->xmin() + 1;
    size++;
  }

  // grid is empty
  if ( size==0 )  return;

#ifdef PDFTHREAD

  double (*alphas)(const double& ) = m_conv_param.alphas;   

  NodeCache* pdf0 = m_conv_param.pdf0;   
  NodeCache* pdf1 = m_conv_param.pdf1;   
  
  double   Escale0 =  m_conv_param.Escale0;   
  double   Escale1 =  m_conv_param.Escale1;   

  //  double _alphasn = 1.;
  //  double  alphanplus1 = 0.;
  // do the convolution  
  // if (debug) std::cout<<name<<" nloop= "<<nloop<<endl;
  //  std::cout << "\torder=" << lo_order << "\tnloop=" << nloop << std::endl;
  // is the grid empty

  int size=0;
  for ( int ip=0 ; ip<m_Nproc ; ip++ ) {
    
    if ( m_weight[ip]->empty() ) continue;

    if ( !m_weight[ip]->trimmed() )  {
      /// std::cerr << "igrid::convolute() naughty, naughty!" << std::endl;
      m_weight[ip]->trim();
    }
    size += m_weight[ip]->xmax() - m_weight[ip]->xmin() + 1;
  }

  //  std::cout << "\t" << name() << " " << ip << " :: size " << size << std::endl; 

  // grid is empty
  if ( size==0 )  return;


  setuppdf( alphas, pdf0, pdf1, nloop, rscale_factor, fscale_factor, Escale0, Escale1 );


#endif

  double* sig = new double[m_Nproc];  // weights from grid

  double* H   = new double[m_Nproc];  // generalised pdf  

  double* HA  = 0;  // generalised splitting functions
  double* HB  = 0;  // generalised splitting functions

  double* HA2 = 0;  // generalised splitting functions at NNLO
  double* HB2 = 0;  // generalised splitting functions at NNLO
  double* HAB = 0;  // generalised splitting functions at NNLO

  if ( nloop>=1 && fscale_factor!=1 ) { 
    HA  = new double[m_Nproc];  // generalised splitting functions
    HB  = new double[m_Nproc];  // generalised splitting functions
    if (nloop > 1 ) {
      HA2  = new double[m_Nproc];  // generalised splitting functions at NNLO
      HB2  = new double[m_Nproc];  // generalised splitting functions at NNLO
      HAB  = new double[m_Nproc];  // generalised splitting functions at NNLO
    }
  }
  
  // cross section for this igrid  

  //char name[]="appl_grid:igrid::convolute(): ";
  //  static const double twopi = 2*M_PI;
  static const int nc = 3;
  //TC   const int nf = 6;
  static const int nf = 5;
  //  static double beta0=(11.*nc - 2.*nf)/(6.*twopi);
  //  static double beta0=(11.*nc - 2.*nf)/(6.*twopi);
  static double beta0_twopi=(11.*nc - 2.*nf)/6;

  //  static double beta1=(34.*nc*nc - 3.*(nc-1./nc)*nf - 10.*nc*nf)/(12.*twopi*twopi);  
  static double beta1_twopi_twopi=(34.*nc*nc - 3.*(nc-1./nc)*nf - 10.*nc*nf)/12.;  
 

  double alphas_tmp = 0.;  

  double _alphasn = 1.;
  double  alphanplus1 = 0.;
  //  double  alphanplus2 = 0.;


  m_conv_param.dsigma     = 0;
  m_conv_param.dsigmaNLO  = 0;
  m_conv_param.dsigmaNNLO = 0;


  // loop over the grid 
  // 

  double logr2 = std::log(rscale_factor*rscale_factor);
  double logf2 = std::log(fscale_factor*fscale_factor);

  //  std::vector<int>    ccks( m_Nproc, 0 );


  double rlocal = rscale_factor;
  double flocal = fscale_factor;

  if ( genpdf->size() != m_Nproc ) { 
    throw exception( "igrid::convolute() parton luminosity mismatch: "+
		     std::to_string(genpdf->size())+" "+
		     std::to_string(m_Nproc) );
  }
  
  for ( int itau=0 ; itau<Ntau() ; itau++  ) {

    _alphasn  = 1;    

    alphas_tmp = m_alphas[itau];

    for ( int iorder=0 ; iorder<lo_order ; iorder++ ) _alphasn *= alphas_tmp;
    alphanplus1 = _alphasn*alphas_tmp;
    //    alphanplus2 =  alphanplus1*alphas_tmp;

#if 1
    if ( fuserscale ) { 

      double tau = gettau(itau);
      double Q2  = fQ2(tau);
      double Q   = std::sqrt(Q2); 
    
      double fQ = fuserscale(Q);
      
      double scale_factor = fQ/Q;
      
      logr2 = 2*std::log(scale_factor) + std::log(rscale_factor*rscale_factor);
      logf2 = 2*std::log(scale_factor) + std::log(fscale_factor*fscale_factor);

      rlocal = scale_factor * rscale_factor;
      flocal = scale_factor * fscale_factor;

    }
#endif

    //    for ( int iy1=0 ; iy1<Ny1() ; iy1++ ) {            
    //      for ( int iy2=0 ; iy2<Ny2() ; iy2++ ) { 
    for ( int iy1=Ny1() ; iy1-- ;  ) {            

      for ( int iy2=Ny2() ; iy2-- ;  ) { 
 	// test if this element is actually filled
	// if ( !m_weight[0]->trimmed(itau,iy1,iy2) ) continue; 
	bool nonzero = false;
	// basic convolution order component for either the born level
	// or the convolution of the nlo grid with the pdf 
	
	for ( int ip=0 ; ip<m_Nproc ; ip++ ) {
	  if ( (sig[ip] = (*(const SparseMatrix3d*)m_weight[ip])(itau,iy1,iy2)) ) nonzero = true;
	}
	
	if ( nonzero ) { 	
	  // build the generalised pdfs from the actual pdfs

	  if ( !isDISgrid() ) genpdf->evaluate( &m_fg1[itau][iy1][0],  &m_fg2[itau][iy2][0], H );
	  else                genpdf->evaluate( &m_fg1[itau][iy1][0],  0, H );

	  //	  std::cout << "genpdf: " << *genpdf << "\tH: " << H[0] << " " << H[1] <<  " " << H[2] << std::endl; 

	  //	  std::cout << iy1 << " " << iy2 << " " << itau << " " << std::endl;
	  
	  //	  for ( int ip=0 ; ip<m_Nproc ; ip++ ) std::cout << "\t" << "  ip " << ip << "  y1 " << iy1 << "  y2 " << iy2 << "  t " << itau << "  s " << sig[ip]*H[ip] << " : " <<  sig[ip] << " " << H[ip] << std::endl;

	  //	  std::cout << "pdf1: " << m_fg1[itau][iy1][6] << "   pdf2: " << m_fg2[itau][iy2][6] << "   H: " << H[0] << std::endl;

	  //	  std::cout << *dynamic_cast<const lumi_pdf*>(genpdf) << std::endl;
	  
	  // do the convolution
          double xsigma=0.;

	  if ( m_parent && m_parent->subproc()!=-1 ) { 
	    int ip=m_parent->subproc();
	    xsigma+= sig[ip]*H[ip];
	  }
	  else { 
	    for ( int ip=0 ; ip<m_Nproc ; ip++ ) xsigma+= sig[ip]*H[ip];
	    //  for ( int ip=0 ; ip<m_Nproc ; ip++ ) std::cout << "\t" << name() << "  ip " << ip << "  y1 " << iy1 << "  y2 " << iy2 << "  t " << itau << "  s " << sig[ip]*H[ip] << " : " <<  sig[ip] << " " << H[ip] << std::endl;
	    //  for ( int ip=0 ; ip<1 ; ip++ ) xsigma+= sig[ip]*H[ip];
	    //  for ( int ip=0 ; ip<1 ; ip++ ) std::cout << "\t" << name() << "  ip " << ip << "  y1 " << iy1 << "  y2 " << iy2 << "  t " << itau << "  s " << sig[ip]*H[ip] << " : " <<  sig[ip] << " " << H[ip] << std::endl;
	    //	for ( int ip=0 ; ip<1 ; ip++ ) std::cout << "\t" << name() << "  ip " << ip << " " << iy1 << " " << iy2 << " " << itau << "  s " << sig[ip] << " : H " <<  H[ip] << "  sH " << sig[ip]*H[ip] << std::endl;
	  }

	  /// if want NLO or NNLO parts only, don't add in the born term
	  if ( _nloop >= 0 ) dsigma += _alphasn*xsigma;

	  //	  std::cout << "\t\tdsigma: " << dsigma << "   ( alphasn: " << _alphasn << " )" << std::endl;


	  // now do the convolution for the variation of factorisation and 
	  // renormalisation scales, proportional to the leading order weights
	  if ( nloop>=1 ) { 

	    // renormalisation scale dependent bit
	    
	    if ( rlocal!=1 ) { 
	      // nlo relative ln mu_R^2 term 

	      double fac = alphanplus1*lo_order*logr2*xsigma; 
	      
	      dsigmaNLO += fac*beta0_twopi;
	   
	      if (nloop>1) { 
		dsigmaNNLO += fac*alphas_tmp*( beta1_twopi_twopi + 0.5*beta0_twopi*beta0_twopi*(1+lo_order)*logr2 );
	      }

  	    }

	    
	    // factorisation scale dependent bit
            
	    // nlo relative ln mu_F^2 term 
	    if ( flocal!=1 ) {

	      if ( !isDISgrid() ) { 
		   genpdf->evaluate(     &m_fg1[itau][iy1][0],  &m_fsplit2[itau][iy2][0], HA);
		   genpdf->evaluate( &m_fsplit1[itau][iy1][0],      &m_fg2[itau][iy2][0], HB);
	      }
	      else { 
		   for ( int ip=0 ; ip<m_Nproc ; ip++ ) HA[ip] = 0;
		   genpdf->evaluate( &m_fsplit1[itau][iy1][0],  0, HB);
	      }

	      xsigma=0.; /// setting this to 0 ??? maybe this is ok because we have already used it for the rscale part

	      if ( m_parent && m_parent->subproc()!=-1 ) { 
		int ip=m_parent->subproc();
		xsigma += sig[ip]*(HA[ip]+HB[ip]);
	      }
	      else { 
		for ( int ip=0 ; ip<m_Nproc ; ip++ ) xsigma += sig[ip]*(HA[ip]+HB[ip]);
	      }

	      double fac = alphanplus1*logf2*xsigma;

	      dsigmaNLO -= fac;

	      if ( nloop>1 ) {

		/// this is most strange - NNLOJET needs logr2*(lo_order+1)
		/// whereas an additional calculation suggests it should be 
		/// logr2*lo_order
		/// hmmm
		dsigmaNNLO += fac*alphas_tmp*beta0_twopi*( 0.5*logf2 - logr2*(lo_order+1) );

		/// plus the double splitting function components ...
		/// PDFA2 PDFB0 alphas2 +PDFA1 PDFB1 alphas2 + PDFA0 PDFB2 alphas2 

		if ( !isDISgrid() ) { 
		  genpdf->evaluate( &m_fsplit12[itau][iy1][0],        &m_fg2[itau][iy2][0], HA2 );
		  genpdf->evaluate(      &m_fg1[itau][iy1][0],   &m_fsplit22[itau][iy2][0], HB2 );
		  genpdf->evaluate(  &m_fsplit1[itau][iy1][0],    &m_fsplit2[itau][iy2][0], HAB );
		}
		else { 
		  for ( int ip=0 ; ip<m_Nproc ; ip++ ) { HAB[ip] = HB2[ip] = 0; }
		  genpdf->evaluate( &m_fsplit12[itau][iy1][0],   0, HA2 );
		}

		double fac2 = 0;
		double fac3 = 0;

	        if ( m_parent && m_parent->subproc()!=-1 ) { 
		    int ip=m_parent->subproc();
		    fac2 += sig[ip]*( HA2[ip] + HB2[ip] );
		    fac3 += sig[ip]*( HAB[ip] );
		}
		else { 
		  for ( int ip=0 ; ip<m_Nproc ; ip++ ) { 
		    fac2 += sig[ip]*( HA2[ip] + HB2[ip] );
		    fac3 += sig[ip]*( HAB[ip] );
		  }
		}
		 
		dsigmaNNLO += alphanplus1*alphas_tmp*(logf2*logf2*fac3 + fac2); 

	      }

	      //if (debug) 
              //cout <<name<<" fscale= " << fscale_factor << " dsigma= "<<dsigma << std::endl;
	    }
	  }
	}  // nonzero
      }  // iy2
    }  // iy1

  }  // itau
  
  //  if ( dbg ) std::cout << name() << "\tccks: " << ccks << std::endl; 

  //if (debug)  std::cout << name<<"     convoluted dsigma=" << dsigma << std::endl; 

  delete[] sig;
  delete[] H;
  delete[] HA;
  delete[] HB;

  if ( HA2 ) { 
    delete[] HA2;
    delete[] HB2;
    delete[] HAB;
  }    

  deletepdftable();
  
  // NB!!! the return value dsigma must be scaled by Escale*Escale which 
  // is done in grid::vconvolute. It would be better here, but is reduces 
  // the number of operations if in grid. 

  m_conv_param.dsigma     = dsigma;
  m_conv_param.dsigmaNLO  = dsigmaNLO;
  m_conv_param.dsigmaNNLO = dsigmaNNLO;

  //  double mytime = appl_timer_stop(mytimer);

  //  std::printf("thread done: param: %lf internal time %lf ms\n", m_conv_param.dsigma, mytime );

}







/// this is the convolute routine for the amcatnlo convolution - essentially it 
/// is the same as for the standard calculation, but the amcatnlo calculation
/// stores weights for the NLO born contribution, and counterterms, so we need
/// more grids than the usual two, 
double appl::igrid::amc_convolute(NodeCache* pdf0,
				  NodeCache* pdf1,
				  appl_pdf*  genpdf,
				  double (*alphas)(const double& ), 
				  int     lo_order,  
				  int     nloop, 
				  double  rscale_factor,
				  double  fscale_factor,
				  double  Escale0, 
				  double  Escale1) 
{ 

  m_conv_param.pdf0 = pdf0;
  m_conv_param.pdf1 = pdf1;

  m_conv_param.alphas = alphas;

  m_conv_param.lo_order =  lo_order;

  //  m_conv_param._nloop   =  nloop;
  //  m_conv_param.Escale   =  Escale;

  //  m_conv_param.rscale_factor = rscale_factor;
  //  m_conv_param.fscale_factor = fscale_factor;

  m_conv_param.genpdf = genpdf;

  m_conv_param.dsigma = 0;

  // is the grid empty
  int size=0;
   for ( int ip=0 ; ip<m_Nproc ; ip++ ) { 
    if ( !m_weight[ip]->trimmed() )  {
      //  std::cout << "igrid::convolute() naughty, naughty!" << std::endl;
      m_weight[ip]->trim();
    }
    size += m_weight[ip]->xmax() - m_weight[ip]->xmin() + 1;
  }

  // grid is empty
  if ( size==0 )  return 0;

  setuppdf( alphas, pdf0, pdf1, nloop, rscale_factor, fscale_factor, Escale0, Escale1 );  

  if ( threads_disabled ) amc_convolute_internal();
  else                    process();

  return m_conv_param.dsigma;
}







void appl::igrid::amc_convolute_internal() { 
  
  int     lo_order = m_conv_param.lo_order;  

  //  int     _nloop    = m_conv_param._nloop;  
  
  //  int      nloop = std::fabs(_nloop); 
  
  //  double  rscale_factor = m_conv_param.rscale_factor;  
  //  double  fscale_factor = m_conv_param.fscale_factor;  
  
  appl_pdf* genpdf = m_conv_param.genpdf;
  
  static const double eightpisquared = 8*M_PI*M_PI;
  
  double alphas_tmp = 0.;  
  double dsigma  = 0.; //, xsigma = 0.;
  double _alphasn = 1.;

  double* sig = new double[m_Nproc];  // weights from grid
  double* H   = new double[m_Nproc];  // generalised pdf  
  double* HA  = 0;  // generalised splitting functions
  double* HB  = 0;  // generalised splitting functions
  //  if ( nloop==1 && fscale_factor!=1 ) { 
  //    HA  = new double[m_Nproc];  // generalised splitting functions
  //    HB  = new double[m_Nproc];  // generalised splitting functions
  //  }

  // cross section for this igrid  

  // loop over the grid 
  // 

  for ( int itau=0 ; itau<Ntau() ; itau++  ) {
    _alphasn  = 1;    
    alphas_tmp = m_alphas[itau]*eightpisquared;
    for ( int iorder=0 ; iorder<lo_order ; iorder++ ) _alphasn *= alphas_tmp;
    //   alphanplus1 = _alphasn*alphas_tmp;

    for ( int iy1=Ny1() ; iy1-- ;  ) {            
      for ( int iy2=Ny2() ; iy2-- ;  ) { 
 	// test if this element is actually filled
	// if ( !m_weight[0]->trimmed(itau,iy1,iy2) ) continue; 
	bool nonzero = false;
	// basic convolution order component for either the born level
	// or the convolution of the nlo grid with the pdf 
	for ( int ip=0 ; ip<m_Nproc ; ip++ ) {
	  if ( (sig[ip] = (*(const SparseMatrix3d*)m_weight[ip])(itau,iy1,iy2)) ) nonzero = true;
	}
	
	//	for ( int ip=0 ; ip<m_Nproc ; ip++ ) std::cout << "\t" << sig[ip]; 
	//	std::cout << std::endl;

	if ( nonzero ) { 	

	  // build the generalised pdfs from the actual pdfs
	  genpdf->evaluate( &m_fg1[itau][iy1][0],  &m_fg2[itau][iy2][0], H );

	  // do the convolution

          double xsigma=0.;
	  for ( int ip=0 ; ip<m_Nproc ; ip++ ) xsigma+=sig[ip]*H[ip];
	  dsigma += _alphasn*xsigma;

	}  // nonzero
      }  // iy2
    }  // iy1
  }  // itau
  
  //if (debug)  std::cout << name<<"     convoluted dsigma=" << dsigma << std::endl; 
  
  delete[] sig;
  delete[] H;
  delete[] HA;
  delete[] HB;
  
  deletepdftable();
  
  //  std::cout << "dsigma " << dsigma << std::endl;

  m_conv_param.dsigma = dsigma;

  // NB!!! the return value dsigma must be scaled by Escale*Escale which 
  // is done in grid::vconvolute. It would be better here, but is reduces 
  // the number of operations if done in grid. 
  return; 
}






bool appl::igrid::shrink( const std::vector<int>& keep ) {
 
  /// save the old grids
  int          Nproc = m_Nproc;
  SparseMatrix3d** w = m_weight;

  /// resize
  m_Nproc  = keep.size();
  m_weight = new SparseMatrix3d*[m_Nproc];

  /// copy across the grids we want to save
  for ( unsigned ip=0 ; ip<keep.size() ; ip++ ) {
 
    //    std::cout << "\tcopy " << keep[ip] << " -> " << ip << std::endl; 

    m_weight[ip] = w[keep[ip]]; /// move across the processes we want to keep
    w[keep[ip]] = 0; /// now flag as 0 
  }

  for ( int ip=0 ; ip<Nproc ; ip++ ) if ( w[ip]!=0 ) delete w[ip];

  delete[] w; 
  
  return true;
}




bool appl::igrid::combine_proc( int i, int j ) { 
  (*m_weight[i]) += (*m_weight[j]);  
  return true;
}


/// rebuild the weight grids, but eliminating the 
/// the reqested subprocess

bool appl::igrid::remove( int i ) {

  if ( i>=m_Nproc ) return false; 

  SparseMatrix3d** mw0 = m_weight;
  SparseMatrix3d** mw  = new SparseMatrix3d*[m_Nproc-1];

  int ir=0;
  int ip0=0;
  for ( int ip=0 ; ip<m_Nproc ; ip++ ) { 
    if ( ip==i ) { 
      delete m_weight[ip];
      ir++;
    }
    else { 
      mw[ip0++] = m_weight[ip];
    }
  }

  m_Nproc -= ir; 
  
  m_weight = mw;

  delete mw0;

  return true;
}





// FIXME: this has lots of legacy code and stuff commented which
// is no longer needed or even correct, so it should be tidied.
// the point of the algorithm here is to find the extent of the filled
// bines + 1 on either side and create a new grid with these limits
   
void appl::igrid::optimise(int NQ2, int Nx1, int Nx2, int extrabins) {     

  //  std::cout << "\tsize(untrimmed)=" << m_weight[0]->size();

  //  std::cout << "ymin=" << gety(0) << "\tymax=" << gety(m_Ny-1) 
  //       << "\txmin=" << fx(gety(m_Ny-1)) << "\txmax=" << fx(gety(0)) << std::endl;

  trim();

  //  std::cout << "\tsize(trimmed)=" << m_weight[0]->size() << std::endl;

#if 1

  // overall igrid optimisation limits

  int _y1setmin = Ny1();
  int _y1setmax = -1;
  
  int _y2setmin = Ny2();
  int _y2setmax = -1;
  
  int _tausetmin = Ntau(); 
  int _tausetmax = -1; 
  
  //  double oldy1min = m_y1min;
  //  double oldy1max = m_y1max;
  
  //  double oldy2min = m_y2min;
  //  double oldy2max = m_y2max;

  // go through all the subprocess to get the limits
  // FIXME: this is actually redundant, at the moment, it assumes all the subprocesses 
  //        occupy the same phase space, so only the first need be tested, and if
  //        they don;t all occupy the same phase space, then they should each be
  //        optimised seperately

  // find limits for this grid
  // initialise to original grid limits
  int y1setmin = _y1setmin;
  int y1setmax = _y1setmax;
  
  int y2setmin = _y2setmin;
  int y2setmax = _y2setmax;
  
  int tausetmin = _tausetmin; 
  int tausetmax = _tausetmax; 
  
  bool firstdebug = true;
  bool firstempty = true;

  for ( int ip=0 ; ip<m_Nproc ; ip++ ) { 
    
    if ( firstdebug ) std::cout << "optimise() proc " << ip 
				<< "\tfilled Q2 weights " << m_weight[ip]->xmax() << " - " << m_weight[ip]->xmin()
				<< "\tx1 " << m_weight[ip]->ymax() << " - " << m_weight[ip]->ymin()
				<< "\tx2 " << m_weight[ip]->zmax() << " - " << m_weight[ip]->zmin();

    // is it empty?
    if ( m_weight[ip]->xmax()-m_weight[ip]->xmin()+1 == 0 ) { 
      if ( firstdebug && firstempty ) std::cout << "\tempty" << std::endl;
      firstempty = false;
      firstdebug = false;
      continue;
    }
    

    firstdebug = false;

    //    m_weight[ip]->print();

    //    m_weight[ip]->yaxis().print(fx);

    //    std::cout << "initial ip=" << ip 
    //	       << "\tm_y2min=" << m_y2min << "\tm_y2max=" << m_y2max
    //         << "\tm_y1min=" << m_y1min << "\tm_y1max=" << m_y1max  
    //         << std::endl;  

    //    m_weight[ip]->print();

    // find actual limits

    // y1 optimisation
    int ymin1 = m_weight[ip]->ymin();
    int ymax1 = m_weight[ip]->ymax();
    
    if ( ymin1<y1setmin )                 y1setmin = ymin1;
    if ( ymin1<=ymax1 && y1setmax<ymax1 ) y1setmax = ymax1; 

    //    std::cout << "ip=" << ip << std::endl; // << "\tymin1=" << ymin1 << "\tymax1=" << ymax1 << std::endl;
    
    // y2 optimisation
    int ymin2 = m_weight[ip]->zmin();    
    int ymax2 = m_weight[ip]->zmax();

    if ( ymin2<y2setmin )                 y2setmin = ymin2;
    if ( ymin2<=ymax2 && y2setmax<ymax2 ) y2setmax = ymax2; 
    
    // tau optimisation
    int taumin = m_weight[ip]->xmin();    
    int taumax = m_weight[ip]->xmax();

    if ( taumin<tausetmin )                   tausetmin = taumin;
    if ( taumin<=taumax && taumax>tausetmax ) tausetmax = taumax;

  }
  
  // if grid is empty, do "nothing" ie create the grid with the same
  // limits as before but with the new required number of bins 
  if (  y1setmax==-1 || y2setmax==-1 || tausetmax==-1 ) { 
    m_Ny1  = Nx1;
    m_Ny2  = Nx2;
    m_Ntau = NQ2;
    m_deltay1  = (m_y1max-m_y1min)/(m_Ny1-1);
    m_deltay2  = (m_y2max-m_y2min)/(m_Ny2-1);
    m_deltatau = (m_taumax-m_taumin)/(m_Ntau-1);
  }
  else { 
 

    
    // y1 optimisation
    //    double oldy1min = m_y1min;
    //    double oldy1max = m_y1max;
   

    if ( isOptimised() ) { 
      // add a bit on each side
      y1setmin -= extrabins;
      y1setmax += extrabins;
    }
    else { 
      // not optimised yet so filled with phase space only so add 
      // the order to the max filled grid element position also 
      y1setmin -= extrabins;
      y1setmax += m_yorder+extrabins;
    }
    
    if ( y1setmin<0 )       y1setmin = 0;
    if ( y1setmax>m_Ny1-1 ) y1setmax = m_Ny1-1;
    
    
    double _min = gety1(y1setmin); 
    double _max = gety1(y1setmax); 
    
    m_Ny1     = Nx1;
    m_y1min   = _min;
    m_y1max   = _max;
    m_deltay1 = (m_y1max-m_y1min)/(m_Ny1-1);
    
    
    
    // y2 optimisation
    //   double oldy2min = m_y2min;
    //   double oldy2max = m_y2max;
    
    if ( isOptimised() ) { 
      // add a bit on each side
      y2setmin -= extrabins;
      y2setmax += extrabins;
    }
    else { 
      // not optimised yet so add the order to the  
      // max filled grid element position also 
      y2setmin -= extrabins;
      y2setmax += m_yorder+extrabins;
    }
    
    if ( y2setmin<0 )      y2setmin = 0;
    if ( y2setmax>=m_Ny2 ) y2setmax = m_Ny2-1; 


    //  m_Ny2 =  y2setmax-y2setmin+1;
    _min = gety2(y2setmin); 
    _max = gety2(y2setmax); 
    
    m_Ny2     = Nx2;
    m_y2min   = _min;
    m_y2max   = _max;
    m_deltay2 = (m_y2max-m_y2min)/(m_Ny2-1);
    
    std::cout << "\t-> " 
	      << y1setmin  << " - " << y1setmax << " : " 
	      << y2setmin  << " - " << y2setmax 
	      << std::endl;
    
    // tau optimisation
    //   double oldtaumin = m_taumin;
    //   double oldtaumax = m_taumax;
    
    // add a bit on each side
    if ( isOptimised() ) { 
      // add a bit on each side
      tausetmin--;
      tausetmax++;
    }
    else { 
      // not optimised yet so add the order to the  
      // max filled grid element position also 
      tausetmax += m_tauorder+1;
      tausetmin -= 1;
    }  

    if ( tausetmin<0 )         tausetmin = 0;
    if ( tausetmax>=m_Ntau-1 ) tausetmax = m_Ntau-1;

    
    _min   = gettau(tausetmin); 
    _max   = gettau(tausetmax);
    
    m_Ntau     = NQ2;
    m_taumin   = _min;
    m_taumax   = _max;
    m_deltatau = (m_taumax-m_taumin)/(m_Ntau-1);
    
    //    std::cout << "done ip=" << ip << std::endl; 
    
  }
  
#endif

  // now create the new subprocess grids with optimised limits
  for ( int ip=0 ; ip<m_Nproc ; ip++ ) { 
    delete m_weight[ip];
    m_weight[ip] = new SparseMatrix3d(m_Ntau, m_taumin,   m_taumax,    
				      m_Ny1,   m_y1min,   m_y1max, 
				      m_Ny2,   m_y2min,   m_y2max ); 
  }   
  
  m_optimised = true;
}





// numerical operators
appl::igrid& appl::igrid::operator=(const appl::igrid& g) { 
  m_Ny1     = g.m_Ny1;
  m_y1min   = g.m_y1min;
  m_y1max   = g.m_y1max;
  m_deltay1 = g.m_deltay1;

  m_Ny2     = g.m_Ny2;
  m_y2min   = g.m_y2min;
  m_y2max   = g.m_y2max;
  m_deltay2 = g.m_deltay2;

  m_yorder = g.m_yorder;
   
  m_Ntau     = g.m_Ntau;
  m_taumin   = g.m_taumin;
  m_taumax   = g.m_taumax;
  m_deltatau = g.m_deltatau;

  m_tauorder = g.m_tauorder;


  m_fg1.clear();
  m_fg2.clear();

  //  construct();

  //  m_weight = new SparseMatrix3d*[m_Nproc];

  for( int ip=0 ; ip<m_Nproc ; ip++ ) { 
    // delete old sparse matrix
    delete m_weight[ip];
    // create new
    m_weight[ip] = new SparseMatrix3d(*g.m_weight[ip]);
  }

  return *this;
}


// should really check all the limits and *everything* is the same
appl::igrid& appl::igrid::operator+=(const igrid& g) { 
  for ( int ip=0 ; ip<m_Nproc ; ip++ ) {
    if ( m_weight[ip] && g.m_weight[ip] ) { 
      /// this is a complicated conditional:
      /// do nothing if the grid to be added is empty ...
      if ( !g.m_weight[ip]->empty() ) { 
	if (  m_weight[ip]->empty() ) { 
	  delete m_weight[ip];
	  m_weight[ip] = new SparseMatrix3d( *g.m_weight[ip] );
	}
	else if ( m_weight[ip]->compare_axes( *g.m_weight[ip] ) ) (*m_weight[ip]) += (*g.m_weight[ip]); 
	else { 
	  throw exception("igrid::operator+=() grids do not match");
	}
      }
    }
  }
  return *this;
} 



void appl::igrid::add( igrid* g ) { 

    int nx1 = g->Ny1();
    int nx2 = g->Ny2();
    int nQ2 = g->Ntau();

    /// map nx, ny and ny to x1, x2 and Q2, then just 
    /// call fill for each !!!

    igrid* ig[2] = { this, g };

    std::cout << "appl::grid::add() combining bins: " << std::endl; 

    for ( int i=0 ; i<2 ; i++ ) std::cout << i << "\t" << *ig[i] << std::endl;

    //    for ( int i=0 ; i<m_Nproc ; i++ ) g->m_weight[i]->untrim();

    std::vector<SparseMatrix3d*> w(m_Nproc);

    for ( int i=0 ; i<m_Nproc ; i++ ) w[i] = g->m_weight[i];
 
    for ( int i1=0 ; i1<nx1 ; i1++ ) { 
      
      double y1 = g->gety1(i1);
      double x1 = g->fx(y1);

      for ( int i2=0 ; i2<nx2 ; i2++ ) { 

	double y2 = g->gety2(i2);
	double x2 = g->fx(y2);

	for ( int it=0 ; it<nQ2 ; it++ ) { 
	  //	  std::cout <<  i1 << " " << i2 << " " << it << " : " << (*m_weight[i])(i1,i2,it) << std::endl; 

	  std::vector<double> weights(m_Nproc,0);
	  
	  double nuffin = 0;
	  
	  for ( int i=0 ; i<m_Nproc ; i++ ) nuffin += weights[i] = (*w[i])(it,i1,i2);

	  if ( nuffin ) { 
	    //	    std::cout << "weight: " << i1 << " " << i2 << " " << it << " : " << (*w[0])(it,i1,i2) << std::endl; 

	    /// need fo fill this one and we're done !!!, but better 
	    /// to loop over Nproc here to avoid doing the 
	    /// interpolations many times ?

	    /// now get x1, x2 and Q2 

	    /// rememebr to use the correct x and Q2 transforms for the grid we 
	    /// are remapping ...
	    double tau = g->gettau(it);
	    double Q2  = g->fQ2(tau);
	    
	    /// now fill this grid !!!
	    fill( x1, x2, Q2, &weights[0] ); 

	  } 
	
	} /// tau
      } /// y2      
    } /// y1
        
}

#include <cstdlib>

void nodes( const std::string& s, double a, double b, int n ) { 
  double d = (b-a)/n;
  std::printf( "%s\t", s.c_str() );
  for ( int i=0 ; i<n+1 && i<6 ; i++ ) std::printf( "\t%8.6lf", a+i*d );
  std::printf("\n");
}



void appl::igrid::remap( appl::igrid* g ) const {  

  std::cout << "remap() in" << std::endl;
 
  std::cout << "Ntau: " << Ntau() << "\tNx: " << Ny1() << " " << Ny2() << "\tNproc: " << m_Nproc << std::endl;  

  std::vector<double> sig(m_Nproc);  

  for ( int itau=0 ; itau<Ntau() ; itau++  ) {

    double Q2 = fQ2(gettau(itau));

    for ( int iy1=Ny1() ; iy1-- ;  ) {                  
      
      double x1 = fx(gety1(iy1));

      for ( int iy2=Ny2() ; iy2-- ;  ) { 
	bool nonzero = false;
	for ( int ip=0 ; ip<m_Nproc ; ip++ ) {
	  if ( (sig[ip] = (*(const SparseMatrix3d*)m_weight[ip])(itau,iy1,iy2)) ) nonzero = true;
	}
	if ( nonzero ) { 
	  
	  double x2 = fx(gety2(iy2));

	  //	  std::cout << "Q2: " << Q2 << "\tx: " << x1 << "\t" << x2 << std::endl;

	  if ( !isDISgrid() ) g->fill( x1, x2, Q2, &sig[0] );
	  else                g->fill_DIS( getx1(iy1), getQ2(itau), &sig[0] );
	}
      }
    }
  }

  std::cout << "remap() out" << std::endl;

}




void appl::igrid::merge( igrid* g ) { 


  for ( int ip=0 ; ip<m_Nproc ; ip++ ) std::cout << "grids: " << m_weight[ip] << std::endl;

  /// map nx, ny and ny to x1, x2 and Q2, then just 
    /// call fill for each !!!
    
    igrid* ig[2] = { this, g };

    igrid* g0 = this;
    igrid* g1 = g;

    std::cout << *g0 << std::endl;
    std::cout << *g1 << std::endl;


    std::cout << "appl::grid::add() combining bins: " << std::endl; 


    int nx1[2];
    int nx2[2];
    int nQ2[2];
    
    double y1[2];
    double y1m[2];
    double dx1[2]; 
    
    double y2[2]; 
    double y2m[2];
    double dx2[2]; 
    
    double tau[2]; 
    double taum[2];
    double dt[2];  
    
    for ( int i=0 ; i<2 ; i++ ) { 

      //      std::cout << i << "\t" << *ig[i] << std::endl;

      igrid* g = ig[i];

      nx1[i] = g->Ny1();
      nx2[i] = g->Ny2();
      nQ2[i] = g->Ntau();

      y1[i]  = g->gety1(0);
      y1m[i] = g->gety1(nx1[i]-1);
      dx1[i]  = (y1m[i]-y1[i])/nx1[i];

      y2[i]  = g->gety2(0);
      y2m[i] = g->gety2(nx2[i]-1);
      dx2[i]  = (y2m[i]-y2[i])/nx2[i];

      tau[i]  = g->gettau(0);
      taum[i] = g->gettau(nQ2[i]-1);
      dt[i]   = (taum[i]-tau[i])/nQ2[i];

      std::cout << "grid:" << i;
      std::cout << "\t" << nx1[i] << "\tx1: " << "\t" << y1[i] << "\t " << y1m[i];
      std::cout << "\t" << nx2[i] << "\tx2: " << "\t" << y2[i] << "\t " << y2m[i]; 
      std::cout << "\t" << nQ2[i] << "\tQ2: " << "\t" << tau[i] << "\t " << taum[i];
      std::cout << std::endl; 
 	
      nodes( "y1:  ", y1[i], y1m[i], nx1[i] );
      // nodes( "y2:  ", y2[i], y2m[i], nx2[i] );
      // nodes( "tau: ", tau[i], taum[i], nQ2[i] );

    }
    
    bool rebuild1 = false; 
    bool rebuild2 = false; 
    bool rebuildt = false; 

    if (  y1[1]<y1[0]  ) rebuild1 = true;
    if ( y1m[1]>y1m[0] )  rebuild1 = true;

    if (  y2[1]<y2[0]  )  rebuild2 = true;
    if ( y2m[1]>y2m[0] )  rebuild2 = true;

    if (  tau[1]<tau[0]  )  rebuildt = true;
    if ( taum[1]>taum[0] )  rebuildt = true;
    
    bool rebuild = rebuild1 || rebuild2 || rebuildt;

    /// gt limits, now create the new igrid

    std::cout << "rebuild: " << rebuild << std::endl;

    m_yorder = 5;
    m_tauorder = 5;

    if ( rebuild ) { 

      m_y1min = std::min(  y1[0],  y1[1] );
      m_y1max = std::max( y1m[0], y1m[1] );

      double d1 = std::min( dx1[0], dx1[1] ); 

      m_y2min = std::min(  y2[0],  y2[1] );
      m_y2max = std::max( y2m[0], y2m[1] );

      double d2 = std::min( dx2[0], dx2[1] ); 
      
      m_taumin = std::min(  tau[0],  tau[1] );
      m_taumax = std::max( taum[0], taum[1] );

      double ndt = std::min( dt[0], dt[1] ); 

      std::cout << "d: " << ndt    << "\t" << d1    << "\t" << d2    << std::endl;
      std::cout << "N: " << m_Ntau << "\t" << m_Ny1 << "\t" << m_Ny2 << std::endl;

      if ( rebuildt ) m_Ntau = int((m_taumax-m_taumin)/ndt + 1);
      if ( rebuild1 ) m_Ny1  = int((m_y1max-m_y1min)/d1 + 1);
      if ( rebuild2 ) m_Ny2  = int((m_y2max-m_y2min)/d2 + 1);


      nodes( "y1 ", m_y1min, m_y1max, m_Ny1 ); 
      // nodes( "y2 ", m_y2min, m_y2max, m_Ny2 ); 
      // nodes( "tau", m_taumin, m_taumax, m_Ntau ); 

      std::cout << "N: " << m_Ntau << "\t" << m_Ny1 << "\t" << m_Ny2 << std::endl;
      
      std::cout << "xorder: " << m_yorder << "\tQorder: " << m_tauorder << std::endl;

      std::cout << "copy grid" << std::endl;

      igrid* gsave = new igrid( *g0 );

      std::cout << "new grids" << std::endl;
      
      // now create the new subprocess grids with optimised limits
      for ( int ip=0 ; ip<m_Nproc ; ip++ ) {

	std::cout << "out with the old ... " << m_weight[ip] << std::endl;          
	
	delete m_weight[ip];
	
	std::cout << "and in with the new" << std::endl;          
	
	m_weight[ip] = new SparseMatrix3d( m_Ntau, m_taumin,   m_taumax,    
					   m_Ny1,   m_y1min,   m_y1max, 
					   m_Ny2,   m_y2min,   m_y2max ); 
      }   
      
      std::cout << "add saved" << std::endl;
      add( gsave );

      delete gsave;

    }
    
    std::cout << "add next" << std::endl;
    //    add( g1 );
    
    

#if 0


    //    for ( int i=0 ; i<m_Nproc ; i++ ) g->m_weight[i]->untrim();

    std::vector<SparseMatrix3d*> w(m_Nproc);

    for ( int i=0 ; i<m_Nproc ; i++ ) w[i] = g->m_weight[i];
 
    for ( int i1=0 ; i1<nx1 ; i1++ ) { 
      
      double y1 = g->gety1(i1);
      double x1 = g->fx(y1);

      for ( int i2=0 ; i2<nx2 ; i2++ ) { 

	double y2 = g->gety2(i2);
	double x2 = g->fx(y2);

	for ( int it=0 ; it<nQ2 ; it++ ) { 
	  //	  std::cout <<  i1 << " " << i2 << " " << it << " : " << (*m_weight[i])(i1,i2,it) << std::endl; 

	  std::vector<double> weights(m_Nproc,0);
	  
	  double nuffin = 0;
	  
	  for ( int i=0 ; i<m_Nproc ; i++ ) nuffin += weights[i] = (*w[i])(it,i1,i2);

	  if ( nuffin ) { 
	    //	    std::cout << "weight: " << i1 << " " << i2 << " " << it << " : " << (*w[0])(it,i1,i2) << std::endl; 

	    /// need fo fill this one and we're done !!!, but better 
	    /// to loop over Nproc here to avoid doing the 
	    /// interpolations many times ?

	    /// now get x1, x2 and Q2 

	    /// rememebr to use the correct x and Q2 transforms for the grid we 
	    /// are remapping ...
	    double tau = g->gettau(it);
	    double Q2  = g->fQ2(tau);
	    
	    /// now fill this grid !!!
	    fill( x1, x2, Q2, &weights[0] ); 

	  } 
	
	} /// tau
      } /// y2      
    } /// y1

#endif
        
}






// debug print
std::ostream&  appl::igrid::debug(std::ostream& s) const {
    header(std::cout);
    for ( int i=0 ; i<m_Nproc ; i++ ) { 
      s << "sub process " << i << std::endl;
      s << "Nx1: " << Ny1() << "\tNx2: " << Ny2() << "\tNs: " << Ntau() << std::endl;        

      double xmin = m_weight[i]->xaxis().min();
      double xmax = m_weight[i]->xaxis().max();

      double ymin = m_weight[i]->yaxis().min();
      double ymax = m_weight[i]->yaxis().max();

      double zmin = m_weight[i]->zaxis().min();
      double zmax = m_weight[i]->zaxis().max();

      std::cout << "ranges: Q2  : " << fQ2(xmin) << " " << fQ2(xmax) << "   : " << xmin << " " << xmax << std::endl;
      std::cout << "ranges: x1  : " <<  fx(ymin) << " " <<  fx(ymax) << "   : " << ymin << " " << ymax << std::endl;
      std::cout << "ranges: x2  : " <<  fx(zmin) << " " <<  fx(zmax) << "   : " << zmin << " " << zmax << std::endl;

    }
    return s;
}
  

std::ostream& appl::igrid::header(std::ostream& s) const { 
  //  s << "interpolation orders: x=" << g.yorder() << "\tQ2=" << g.tauorder() << std::endl;  
  s << "\t x:  [ "  << std::setw(2) 
    //    << Ny1() << " ;\t" << std::setw(7) << fx(y1max())  << " - " << std::setw(7) << std::setprecision(6) << fx(y1min()) << "\t : " 
    //    << Ny2() << " ;\t" << std::setw(7) << fx(y2max())  << " - " << std::setw(7) << std::setprecision(6) << fx(y2min()) 
    << Ny1() << " :\t " << std::setw(7) << std::setprecision(6) << fx(y1max())  << " - " << std::setw(7) << std::setprecision(6) << fx(y1min()) << "\t : " 
    << Ny2() << " :\t " << std::setw(7) << std::setprecision(6) << fx(y2max())  << " - " << std::setw(7) << std::setprecision(6) << fx(y2min()) 
    << "\t : " << "\t( order=" << yorder()   << " ) ]"; 
  s << "\t Q2: [ " 
    << Ntau() << " :\t "  << std::setw(7) << std::setprecision(6) << fQ2(taumin()) << " - " << std::setw(7) << std::setprecision(6) << fQ2(taumax()) 
    << "\t( order=" << tauorder() << "  - reweight " << ( m_reweight ? "on " : "off" ) << ") ]";
  return s;
}


std::ostream& operator<<(std::ostream& s, const appl::igrid& g) {
  g.header(s);
  //  g.print(s);
  return s;
}




void appl::igrid::run_thread() {
 
  lock_proc();
  mprocessing = false;
  unlock_proc();


  while( true ) { 

    //    std::cout << "thread " << this << " " << mname << " waiting ... " << std::endl; 

    /// supend immediately
    suspend();

    if ( mterminate ) break;

    //    std::cout << "thread " << this << " " << mname << " running ... " << std::endl; 

    //    struct timeval mytimer = appl_timer_start();

    /// put the actual convoluting steps in here  
    /// once round the colbolution step, it puts itself to 
    /// sleep again
    
    if ( parent()->calculation()==grid::AMCATNLO )  amc_convolute_internal();
    else                                            convolute_internal();

    //    std::printf("thread done: param: %lf internal\n", m_conv_param.dsigma );
 
    /// after starting the processing step, the calling thread must 
    /// wait for all the threads to finish

    //    double mytime = appl_timer_stop(mytimer);

    //    std::printf("thread done: param: %lf internal time %lf ms\n", m_conv_param.dsigma, mytime );

    /// signal computation has finished

    //    std::cout << "thread " << this << " " << mname << " done [" << mytime << " ms" << "]" << std::endl; 
  }

  suspend(false);
  
}


