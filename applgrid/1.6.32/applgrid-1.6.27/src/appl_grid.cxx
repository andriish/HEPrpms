/**
 **     @file    appl_grid.cxx
 **
 **     @brief   grid class - all the functions needed to create and 
 **              fill the grid from an NLO calculation program. 
 **
 **     @author  mark sutton
 **     @date    2007/10/16 17:01:39 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: appl_grid.cxx, v1.00 2007/10/16 17:01:39 sutt $
 **
 **/

#include <cstdlib>
#include <stdio.h>
#include <sys/stat.h>

#include <vector>
#include <map>
#include <set>
#include <iostream>
#include <fstream>
#include <iomanip>
#include <cmath>

#include "appl_grid/appl_pdf.h"
#include "appl_grid/appl_timer.h"
#include "appl_grid/Directory.h"
#include "appl_grid/appl_grid.h"
#include "appl_grid/appl_file.h"
#include "appl_grid/vector_stream.h"
#include "appl_grid/stream_vector.h"

#include "appl_grid/generic_pdf.h"
#include "appl_grid/lumi_pdf.h"

#include "appl_igrid.h"
#include "Cache.h"

#ifdef USEROOT

#include "TFileString.h"
#include "TFileVector.h"

#include "TFile.h"
#include "TObjString.h"
#include "TVectorT.h"

#endif

#include "amconfig.h"

#include "Splitting.h"

#include "appl_grid/appl_TH1D.h"

/// in case of non c++11 compliant compilation, define
/// some c++11 type replacemant functions

#if __cplusplus <= 199711L
// #warning Not using C++11 compilation - using substitute functions
// Fixme: check whether this still works any more - many exception handling changes
// are not compatible between versions, also need to check other c++11 extensions 

namespace std {

int stoi( const std::string s ) { return atoi(s.c_str()); }

std::string to_string( int i ) {
    char a[16];
    sprintf( a, "%d", i );
    return a;
}


std::string to_string( double f ) {
    char a[178];
    sprintf( a, "%lf", f );
    return a;
}

}

#endif




static bool appl_first = true;


/// this is a compatability flag for persistent versions 
/// of the grid
/// NB: ONLY change the major version if the persistent 
///     class changes in a non-backwards compatible way.

// const std::string appl::grid::m_version = "version-3.3";
const std::string appl::grid::m_version = PACKAGE_VERSION;

std::string appl::grid::appl_version() const { return PACKAGE_VERSION; }

#include "hoppet_init.h"

#ifdef HAVE_HOPPET

#include "hoppet_v1.h"

// include hoppet splitting function code

static appl::hoppet_init* hoppet = 0;

void Splitting(const double& x, const double& Q, double* xf, int nLoops) {
  //  static const int nLoops    = 1;
  static const int nFlavours = 5;
  hoppetevalsplit_( x, Q, nLoops, nFlavours, xf); 
  return;
}

#else

void Splitting(const double& , const double& , double* , int ) {
  throw appl::grid::exception( "hoppet library not included - cannot call splitting function"  ); 
  return; // technically, don't need this - should throw an exception
}

#endif


#define appl_line() std::cout << __FILE__ << " " << __LINE__ << std::endl;



/// helper function
static bool contains(const std::string& s, const std::string& reg ) { 
  return s.find(reg)!=std::string::npos;
}

/// make sure pdf std::map is initialised
// bool pdf_ready = appl::appl_pdf::create_map(); 

std::string appl::compiled() {  return __DATE__; }
std::string appl::version()  { return VERSION; }


/// current time
std::string appl::date() { 
  time_t _t;
  time(&_t);
  std::string a = ctime(&_t);
  std::string b = "";
  for ( unsigned i=0 ; i<a.size()-1 ; i++ ) b+=a[i];
  return b;
}


/// simple test if a file exists
bool appl::file_exists(const std::string& s) {
  struct stat sb;
  if ( stat( s.c_str(), &sb)==0 ) return true; // && S_ISREG(sb.st_mode ))
  else return false;
}




appl::grid::grid(int NQ2, double Q2min, double Q2max, int Q2order, 
		 int Nx,  double xmin,  double xmax,  int xorder,
		 int Nobs,  double obsmin, double obsmax, 
		 std::string genpdfname,
		 int leading_order, int _nloops, 
		 std::string transform,
		 std::string qtransform ) :
  m_leading_order(leading_order), m_order(_nloops+1), 
  m_run(0), m_optimised(false), m_trimmed(false), m_normalised(false), m_symmetrise(false), 
  m_transform(transform), m_qtransform(qtransform), 
  m_genpdfname(genpdfname), m_cmsScale(0), m_dynamicScale(0),
  m_applyCorrections(false),
  m_documentation(""),
  m_type(STANDARD),
  m_read(false),
  m_subproc(-1),
  m_bin(-1),
  m_genwithpdf(""),
  m_photon(false),
  m_isDIS(false),
  m_use_duplicates(false)
{
  // Initialize histogram that saves the correspondence obsvalue<->obsbin
  m_ref=new appl::TH1D("referenceInternal","Bin-Info for Observable", Nobs, obsmin, obsmax);
  m_ref->SetDirectory(0);
  m_ref->Sumw2(); /// grrr root is so rubbish - not scaling errors properly

  m_ref_combined = m_ref;

  /// check to see if we require a generic pdf from a text file, and 
  /// and if so, create the required generic pdf
  //  if      ( m_genpdfname.find(".dat")!=std::string::npos ) addpdf(m_genpdfname);
  //  else if ( m_genpdfname.find(".dat")!=std::string::npos ) addpdf(m_genpdfname);
  if ( contains(m_genpdfname, ".dat") ||  contains(m_genpdfname, ".config") ) addpdf(m_genpdfname);
  findgenpdf( m_genpdfname );

  construct(Nobs, NQ2, Q2min, Q2max, Q2order, Nx, xmin, xmax, xorder, m_order, m_transform, m_qtransform ); 
}




appl::grid::grid(int Nobs, const double* obsbins, 
		 int NQ2,  double Q2min, double Q2max, int Q2order, 
		 int Nx,   double xmin, double xmax,   int xorder, 
		 std::string genpdfname, 
		 int leading_order, int _nloops, 
		 std::string transform,
		 std::string qtransform,
		 bool isdis ) :
  m_leading_order(leading_order), m_order(_nloops+1), 
  m_run(0), m_optimised(false), m_trimmed(false),  m_normalised(false), m_symmetrise(false),
  m_transform(transform),  m_qtransform(qtransform), 
  m_genpdfname(genpdfname), m_cmsScale(0), m_dynamicScale(0),
  m_applyCorrections(false),
  m_documentation(""),
  m_type(STANDARD),
  m_read(false),
  m_subproc(-1),
  m_bin(-1),
  m_genwithpdf(""),
  m_photon(false),
  m_isDIS(isdis),
  m_use_duplicates(false)
{
  
  // Initialize histogram that saves the correspondence obsvalue<->obsbin
  m_ref=new appl::TH1D("referenceInternal","Bin-Info for Observable", Nobs, obsbins);
  m_ref->SetDirectory(0);
  m_ref->Sumw2(); /// grrr root is so rubbish - not scaling errors properly

  m_ref_combined = m_ref;

  /// check to see if we require a generic pdf from a text file, and 
  /// and if so, create the required generic pdf
  //  if ( m_genpdfname.find(".dat")!=std::string::npos ) addpdf(m_genpdfname);
  if ( contains(m_genpdfname, ".dat") ||  contains(m_genpdfname, ".config") ) addpdf(m_genpdfname);

  findgenpdf( m_genpdfname );

  construct(Nobs, NQ2, Q2min, Q2max, Q2order, Nx, xmin, xmax, xorder, m_order, m_transform, m_qtransform, m_isDIS );
}




appl::grid::grid(const std::vector<double>& obs, 
		 int NQ2, double Q2min, double Q2max, int Q2order,
		 int Nx,  double xmin,  double xmax,  int xorder, 
		 std::string genpdfname,
		 int leading_order, int _nloops, 
		 std::string transform,
		 std::string qtransform ) :
  m_leading_order(leading_order), m_order(_nloops+1), 
  m_run(0), m_optimised(false), m_trimmed(false), m_normalised(false), m_symmetrise(false),  
  m_transform(transform),  m_qtransform(qtransform), 
  m_genpdfname(genpdfname), m_cmsScale(0), m_dynamicScale(0),
  m_applyCorrections(false),
  m_documentation(""),
  m_type(STANDARD),
  m_read(false),
  m_subproc(-1),
  m_bin(-1),
  m_genwithpdf(""),
  m_photon(false),
  m_isDIS(false),
  m_use_duplicates(false)
{
  
  if ( obs.size()==0 ) { 
    std::cerr << "grid::not enough bins in observable" << std::endl;
    std::exit(0);
  } 
  
  //  double* obsbins = new double[obs.size()];  
  //  for ( unsigned i=0 ; i<obs.size() ; i++ ) obsbins[i] = obs[i];
  //  int Nobs = obs.size()-1;

  // Initialize histogram that saves the correspondence obsvalue<->obsbin
  m_ref=new appl::TH1D("referenceInternal","Bin-Info for Observable", obs.size()-1, &obs[0] );
  m_ref->SetDirectory(0);
  m_ref->Sumw2(); /// grrr root is so rubbish - not scaling errors properly
  //  delete[] obsbins;

  m_ref_combined = m_ref;

  /// check to see if we require a generic pdf from a text file, and 
  /// and if so, create the required generic pdf
  //   if ( m_genpdfname.find(".dat")!=std::string::npos ) addpdf(m_genpdfname);
  if ( contains(m_genpdfname, ".dat") ||  contains(m_genpdfname, ".config") ) addpdf(m_genpdfname);
  findgenpdf( m_genpdfname );

  construct( obs.size()-1, NQ2, Q2min, Q2max, Q2order, Nx, xmin, xmax, xorder, m_order, m_transform, m_qtransform ); 
}



appl::grid::grid(const std::vector<double>& obs, 
		 std::string genpdfname,
		 int leading_order, int _nloops, 
		 std::string transform,
		 std::string qtransform )  :
  m_leading_order(leading_order), m_order(_nloops+1), 
  m_run(0), m_optimised(false), m_trimmed(false), m_normalised(false), m_symmetrise(false),  
  m_transform(transform),  m_qtransform(qtransform), 
  m_genpdfname(genpdfname), m_cmsScale(0), m_dynamicScale(0),
  m_applyCorrections(false),  
  m_documentation(""),
  m_type(STANDARD),
  m_read(false),
  m_subproc(-1),
  m_bin(-1),
  m_genwithpdf(""),
  m_photon(false),
  m_isDIS(false),
  m_use_duplicates(false)
{ 
  if ( obs.size()==0 ) { 
    std::cerr << "grid::not enough bins in observable" << std::endl;
    std::exit(0);
  } 
  
  //  double* obsbins = new double[obs.size()];  
  //  for ( unsigned i=0 ; i<obs.size() ; i++ ) obsbins[i] = obs[i];
  //  int Nobs = obs.size()-1;

  // Initialize histogram that saves the correspondence obsvalue<->obsbin
  m_ref=new appl::TH1D("referenceInternal","Bin-Info for Observable", obs.size()-1, &obs[0] );
  m_ref->SetDirectory(0);
  m_ref->Sumw2(); /// grrr root is so rubbish - not scaling errors properly
  //  delete[] obsbins;

  m_ref_combined = m_ref;

  /// check to see if we require a generic pdf from a text file, and 
  /// and if so, create the required generic pdf
  //  if ( m_genpdfname.find(".dat")!=std::string::npos ) addpdf(m_genpdfname);
  if ( contains(m_genpdfname, ".dat") ||  contains(m_genpdfname, ".config") ) addpdf(m_genpdfname);
  findgenpdf( m_genpdfname );

  //  for ( int iorder=0 ; iorder<m_order ; iorder++ ) m_grids[iorder] = new igrid*[obs.size()-1];
  for ( int iorder=0 ; iorder<m_order ; iorder++ ) m_grids[iorder].resize(obs.size());

}



#ifdef USEROOT
appl::grid::grid( const std::string& filename, const std::string& dirname ) : 
#else
appl::grid::grid( const std::string& filename, const std::string& /* dirname */ ) : 
#endif
  m_leading_order(0),  m_order(0),
  m_optimised(false),  m_trimmed(false), 
  m_normalised(false),
  m_symmetrise(false), m_transform(""),  m_qtransform(""), 
  m_dynamicScale(0),
  m_applyCorrections(false),
  m_documentation(""),
  m_type(STANDARD),
  m_read(false),
  m_subproc(-1),
  m_bin(-1),
  m_genwithpdf(""),
  m_photon(false),
  m_isDIS(false),
  m_use_duplicates(false)
{

  if ( filename.find(".root")==(filename.size()-5) ) {
#   ifdef USEROOT
    construct_root( filename, dirname );
#   else
    std::cerr << "appl::grid() root not compiled in - cannot read root files" << std::endl;
    std::exit(-1);
#   endif
  }
  else construct_appl( filename );

}


#ifdef USEROOT
void appl::grid::construct_root( const std::string& filename, const std::string& dirname ) { 

  if ( appl_first ) { 
    appl_first = false;
    std::cout << "appl::grid() version " << VERSION << " compiled " << __DATE__ << std::endl;
  }
  
  m_ref_combined = m_ref = 0;

  struct timeval tstart = appl_timer_start();
  
  struct stat _fileinfo;
  if ( stat(filename.c_str(),&_fileinfo) )   {    
    throw exception( std::string("grid::grid() cannot find file ") + filename  ); 
  }

  std::cout << "appl::grid() reading grid from file " << filename;
  
  TFile* gridfilep = TFile::Open(filename.c_str());

  if (gridfilep==0 ) {
    std::cout << std::endl;
    throw exception( std::string("grid::grid() cannot open root file: ") + filename );
  }

  if (gridfilep->IsZombie()) {
    std::cout << std::endl;
    delete gridfilep;
    throw exception( std::string("grid::grid() cannot open root file: zombie ") + filename );
  }

  // TFile gridfile(filename.c_str());
  
  //  gDirectory->cd(dirname.c_str());

  //  std::cout << "pwd=" << gDirectory->GetName() << std::endl;

  //  gDirectory->cd(dirname.c_str());

  //  std::cout << "pwd=" << gDirectory->GetName() << std::endl;

  //  Directory d(dirname);
  //  d.push();

  TFileString* _tagsp = (TFileString*)gridfilep->Get((dirname+"/Tags").c_str());  

  if ( _tagsp==0 ) { 
    std::cout << std::endl;
    throw exception( std::string("grid::grid() cannot get tags: ") + filename );
  }

  TFileString _tags = *_tagsp;

  m_transform  = _tags[0];
  m_genpdfname = _tags[1];
  std::string _version = _tags[2];


  /// ah ha !! this is messed up, in the older version, 
  /// documentation only gets added if it was a non-empty string
  /// and similarly with the pdf of the calculation, 
  /// so this pdf might be at position 3 if there is no 
  /// documentation, so then when reading back, the pdf
  /// would go into the documentation and the version would 
  /// be undefined. 
  ///
  /// In this version, the documntation is always 
  /// added, even if it is an empty string  

  //  std::cout << "tags:: transform: " << m_transform << "\tpdfname: " << m_genpdfname << "\tdoc: " << m_documentation << std::endl;  

  if ( _tags.size()>3 ) m_documentation = _tags[3];

  std::cout << "\t:  applgrid version " << _version;
  if ( _version != m_version ) std::cout  << " (transformed to " << m_version << ")";

  /// encoded pdfset used for the generation 
  if ( _tags.size()>4 ) { 
    std::string tmp = _tags[4];
    if ( tmp.find(":")!=std::string::npos ) {  
      m_genwithpdf  = tmp.substr(0,tmp.find(":"));
      m_genwithipdf = std::stoi(tmp.substr(tmp.find(":")+1,std::string::npos));
    }
    else { 
      m_genwithpdf  = tmp;
      m_genwithipdf = 0;
    }
    std::cout << "\t:  grid generated with " << m_genwithpdf << " : set " << m_genwithipdf;
  }

  std::cout << std::endl;

  //  std::cout << "trying qtransform" << std::endl;

  TFileString* _qtags = (TFileString*)gridfilep->Get((dirname+"/QTags").c_str());  

  if ( _qtags==0 ) m_qtransform = "h0"; 
  else             m_qtransform = _qtags->at(0);   


  /// clean up - delete 0 works fine, no need to check
  delete _tagsp;
  delete _qtags;

  // check it has the correct version
  // if ( _version > m_version ) { 
  //      throw exception(cerr << "incorrect version " << _version << " expected " << m_version ); 
  // }
  //  m_version = _version;
  

  if ( getDocumentation()!="" ) std::cout << getDocumentation() << std::endl; 
  
  bool added = false;

  if ( m_genpdfname=="basic" ) { 
    added = true;
    m_genpdfname = "basic.config"; 
    if ( contains(m_genpdfname, ".dat") ||  contains(m_genpdfname, ".config") ) addpdf(m_genpdfname);
    findgenpdf( m_genpdfname );
  }

  //  std::cout << "Tags=" << _tags << std::endl;

  //  std::cout << "grid::grid() use transform " << m_transform << std::endl;

  // read state information
  // hmmm, have to use TVectorT<double> since TVector<int> 
  // apparently has no constructor (???)
  TVectorT<double>* setup=(TVectorT<double>*)gridfilep->Get((dirname+"/State").c_str());
 
  m_run        = (*setup)(0);
  m_optimised  = ( (*setup)(1)!=0 ? true : false );
  m_symmetrise = ( (*setup)(2)!=0 ? true : false );  

  m_leading_order = int((*setup)(3)+0.5);  
  m_order         = int((*setup)(4)+0.5);  

  if ( setup->GetNoElements()>5 ) m_cmsScale = (*setup)(5);
  else                            m_cmsScale = 0;
 
  if ( setup->GetNoElements()>6 ) m_normalised = ( (*setup)(6)!=0 ? true : false );
  else                            m_normalised = true;

  if ( setup->GetNoElements()>7 ) m_applyCorrections = ( (*setup)(7)!=0 ? true : false );
  else                            m_applyCorrections = false;

  int n_userdata = 0;
  if ( setup->GetNoElements()>10 ) n_userdata = int((*setup)(10)+0.5);

  if ( setup->GetNoElements()>11 ) m_use_duplicates = ( int((*setup)(11)+0.5) ? true : false );


  //  std::vector<double> _ckmsum;
  std::vector<std::vector<double> > _ckm2;
  std::vector<std::vector<double> > _ckm;

  /// check whether we need to read in the ckm matrices

  if ( setup->GetNoElements()>8 && (*setup)(8)!=0 ) {

    std::cout << "grid::grid() read ckm matrices" << std::endl;
    
    /// try 14 x 14 squared ckm matrix 

    TVectorT<double>* ckm2flat=(TVectorT<double>*)gridfilep->Get((dirname+"/CKM2").c_str());

    if ( ckm2flat ) { 
      int Nrows = 13;
      if ( ckm2flat->GetNrows()>169 ) Nrows = 14; 
      if ( ckm2flat->GetNrows()>0 ) { 
	_ckm2 = std::vector<std::vector<double> >(Nrows, std::vector<double>(Nrows) );
      
	for ( int ic=0 ; ic<Nrows ; ic++ ) { 
	  for ( int id=0 ; id<Nrows ; id++ ) _ckm2[ic][id] = (*ckm2flat)(ic*Nrows+id); 
	}

	if ( Nrows==14 ) {
	  /// photons ??
	  for ( int ic=0 ; ic<14 ; ic++ ) _ckm2[13][ic] = 0; 
	  for ( int ic=0 ; ic<14 ; ic++ ) _ckm2[ic][13] = 0; 
	}

      }  
    }

    /// now try usual 3x3 matrix

    TVectorT<double>* ckmflat=(TVectorT<double>*)gridfilep->Get((dirname+"/CKM").c_str());

    if ( ckmflat ) { 
      if ( ckmflat->GetNrows()>0 ) { 
	_ckm = std::vector<std::vector<double> >(3, std::vector<double>(3) );
	
	for ( int ic=0 ; ic<3 ; ic++ ) { 
	  for ( int id=0 ; id<3 ; id++ ) _ckm[ic][id] = (*ckmflat)(ic*3+id); 
	}
      }  
    }

  }

  if ( setup->GetNoElements()>9 ) m_type = (CALCULATION)int( (*setup)(9)+0.5 );
  else                            m_type = STANDARD;

  //  std::cout << "appl::grid() reading grid calculation type: " << _calculation(m_type) << std::endl;

  //  std::cout << "appl::grid() normalised: " << getNormalised() << std::endl;

  /// check to see if we require a generic pdf from a text file, and 
  /// and if so, create the required generic pdf (or lumi_pdf for amcatnlo)
  //  if ( m_genpdfname.find(".dat")!=std::string::npos ) addpdf(m_genpdfname);

  //  std::cout << "appl::grid() requested pdf combination " << m_genpdfname << std::endl;
  

  if ( !added && contains(m_genpdfname, ".config") ) { 
    /// decode the pdf combination if appropriate

    // find out if we have one combination per order or one overall
    std::vector<std::string> namevec = parse( m_genpdfname, ":" );

    std::string label = dirname+"/Combinations";

    for ( unsigned i=0 ; i<namevec.size() ; i++ ) { 

      /// again have to use TVectorT<double> because TVectorT<int> has no constructor!!!
      /// I ask you!! what's the point of a template if it doesn't actually instantiate
      /// it's pathetic!

      TVectorT<double>* _combinations = (TVectorT<double>*)gridfilep->Get( label.c_str() );

      label += "N"; /// add an N for each order, N-LO, NN-LO etc

      if ( _combinations==0 ) throw exception( std::string( "grid::grid() cannot read pdf combination " ) + namevec[i] );

      std::vector<int> combinations(_combinations->GetNoElements());

      for ( unsigned ic=0 ; ic<combinations.size() ; ic++ )  combinations[ic] = int((*_combinations)(ic)); 
    
      //      addpdf(m_genpdfname, combinations);
      addpdf( namevec[i], combinations );

    }
  }
  else { 
    /// of just create the generic from the file
    if ( contains(m_genpdfname, ".dat") ) addpdf(m_genpdfname);
  }
  
  /// retrieve the pdf routine 
  findgenpdf( m_genpdfname );

  m_photon = false;

  /// check whether there are any photon contributions in this grid ...
 
  for ( int iord=0 ; iord<m_order ; iord++ ) {
    const lumi_pdf* lpdf = dynamic_cast<const lumi_pdf*>(m_genpdf[iord]);
    if ( lpdf && ( lpdf->contains(7) || lpdf->contains(22) )  ) m_photon = true;
  }

  //  std::cout << "grid::grid() read " << m_genpdfname << " " << m_genpdf[0]->getckmsum().size() << std::endl; 
  //  std::cout << "grid::grid() read " << *dynamic_cast<const lumi_pdf*>(m_genpdf[0]) << std::endl; 
  //  std::cout << "grid::grid() read " << *dynamic_cast<const lumi_pdf*>(m_genpdf[1]) << std::endl; 
  //  std::cout << "grid::grid() read " << *dynamic_cast<const lumi_pdf*>(m_genpdf[2]) << std::endl; 

  // set the ckm matrices 
  if      ( _ckm.size()>0 )  setckm( _ckm );
  else if ( _ckm2.size()>0 ) setckm2( _ckm2 );

  delete setup;

  //  std::cout << "grid::grid() read setup" << std::endl;

  // Read observable bins information
  //  gridfile.GetObject("obs_bins", m_ref );

  ::TH1D* m_ref_t = (::TH1D*)gridfilep->Get((dirname+"/reference_internal").c_str());
  
  if ( m_ref_t ) { 
    m_ref = convert( m_ref_t );
    m_ref_t->SetDirectory(0);
    delete m_ref_t;

    ::TH1D* m_ref_combined_t = (::TH1D*)gridfilep->Get((dirname+"/reference").c_str());
    if ( m_ref_combined_t ) { 
      m_ref_combined = convert( m_ref_combined_t );
      m_ref_combined->SetDirectory(0);
      if ( run() ) m_ref_combined->Scale(run());

      m_ref_combined_t->SetDirectory(0);
      delete m_ref_combined_t;
      /// for ( int i=0 ; i<m_ref_combined->GetNbinsX ; i++ ) m_ref_combined->SetBinContent( i+1, m_ref_combined->GetBinContent( i+1, run(i) ) );
    }
  }
  else { 
    m_ref_t = (::TH1D*)gridfilep->Get((dirname+"/reference").c_str());
    if ( m_ref_t ) { 
      m_ref = convert( m_ref_t );
      m_ref_combined = m_ref;

      m_ref_t->SetDirectory(0);
      delete m_ref_t;
    }
    else { 
      throw exception( "cannot read reference histogram" );
    }
  }

  m_ref->SetDirectory(0);
  if ( run() ) m_ref->Scale(run());
  /// for ( int i=0 ; i<m_ref->GetNbinsX ; i++ ) m_ref->SetBinContent( i+1, m_ref->GetBinContent( i+1, run(i) ) );

  m_ref->SetName("referenceInternal");
  if ( m_normalised && m_optimised ) m_read = true;

  ///  std::cout << "grid::grid() read obs bins" << std::endl;

  /// also calculate maximum, values for Q2 and y=ln(1/x)
  /// for hoppet initialisation

  double _Q2max = 0;
  double _xmin  = 2; // always bigger than 1

  for( int iorder=0 ; iorder<m_order ; iorder++ ) {
    //    std::cout << "grid::grid() iorder=" << iorder << std::endl;
    //    m_grids[iorder] = new igrid*[Nobs_internal()];  
    m_grids[iorder].resize(Nobs_internal());  
    for( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) {
      char name[128];  sprintf(name, (dirname+"/weight[alpha-%d][%03d]").c_str(), iorder, iobs);
      //   std::cout << "grid::grid() reading " << name << "\tiobs=" << iobs << std::endl;

      m_grids[iorder][iobs] = new igrid(*gridfilep, name);
      m_grids[iorder][iobs]->setparent( this ); 

      if (  !m_grids[iorder][iobs]->empty() ) { 

	double x1  = m_grids[iorder][iobs]->x1filledmin();
	double x2  = m_grids[iorder][iobs]->x2filledmin();	
	double Q2  = m_grids[iorder][iobs]->Q2filledmax();
	double Q2i = m_grids[iorder][iobs]->Q2filledmin();

	if ( Q2i>Q2 ) Q2 = Q2i;

	if ( m_grids[iorder][iobs]->isDISgrid() ) m_isDIS = true;

	if (  Q2>_Q2max ) _Q2max = Q2;
	if (  x1<_xmin )   _xmin = x1;
	if (  x2<_xmin )   _xmin = x2;
      }

      //    _size += m_grids[iorder][iobs]->size();
      //      std::cout << "grid::grid() done" << std::endl;
    }
  }

  if ( _Q2max>0 )           m_Qmax = std::sqrt(_Q2max);
  if ( _xmin>0 && _xmin<2 ) m_ymax = std::log(1/_xmin);

  //  std::cout << "Qmax " << m_Qmax << std::endl; 
  //  std::cout << "ymax " << m_ymax << std::endl;

  //  d.pop();

  /// bin-by-bin correction labels                                       
  TFileString* correctionLabels = (TFileString*)gridfilep->Get((dirname+"/CorrectionLabels").c_str());  
  if ( correctionLabels ) { 
    for ( unsigned i=0 ; i<correctionLabels->size() ; i++ ) {
      m_correctionLabels.push_back( (*correctionLabels)[i] ); // copy the correction label
    }
  }

#if 0
  /// bin-by-bin correction values
  TFileVector* corrections = (TFileVector*)gridfilep->Get((dirname+"/Corrections").c_str());  
  if ( corrections ) { 
    for ( unsigned i=0 ; i<corrections->size() ; i++ ) {
      m_corrections.push_back( (*corrections)[i] ); // copy the correction histograms
      m_applyCorrection.push_back(false);
    }
  }
#else

  //  std::cout << "run: " << m_run << std::endl; 

  TFileVector* corrections = (TFileVector*)gridfilep->Get((dirname+"/Corrections").c_str());  
  if ( corrections ) { 
    for ( unsigned i=0 ; i<corrections->size() ; i++ ) {
      m_corrections.push_back( *getReference() );
      m_corrections.back().name() = m_correctionLabels[i];
      m_corrections.back().set( (*corrections)[i] );
      m_applyCorrection.push_back(false);

      std::cout << "correction: " <<  m_corrections.back().name() << std::endl; 
      //      std::cout << m_corrections.back() << std::endl;

    }
  }

#endif

  /// now read in vector of bins to be combined if present
  TVectorT<double>* _combine = (TVectorT<double>*)gridfilep->Get((dirname+"/CombineBins").c_str());

  if ( _combine!=0 ) { 
    //    std::cout << "read in " << _combine->GetNrows() << " entries in combine" << std::endl;
    m_combine = std::vector<int>(_combine->GetNrows(),0);
    for ( unsigned i=_combine->GetNrows() ; i-- ; ) m_combine[i] = int((*_combine)(i));

    if ( m_combine.size() ) combineReference();
  }

  //  std::cout << "grid::grid() read from file Nobs = " << Nobs_internal() << std::endl;

  //  std::cout << "read grid" << std::endl;

  /// read in the user data
  if ( n_userdata ) { 
    TVectorT<double>* userdata=(TVectorT<double>*)gridfilep->Get((dirname+"/UserData").c_str());
    m_userdata.clear();
    for ( int i=0 ; i<n_userdata ; i++ ) m_userdata.push_back( (*userdata)(i) );
  }

  gridfilep->Close();
  delete gridfilep;

  double tstop = appl_timer_stop( tstart );

  unsigned usize = size();

  struct timeval tstart2 =  appl_timer_start();

  trim();

  double tstop2 = appl_timer_stop( tstart2 );

  std::cout << "appl::grid() read grid, size ";
  if ( usize>1024*10 ) std::cout << usize/1024./1024. << " MB";
  else                 std::cout << usize/1024.       << " kB";

  //  unsigned nusize = size();

  //  std::cout << " \t-> ";

  //  if ( nusize>1024*10 ) std::cout << nusize/1024./1024. << " MB";
  //  else                  std::cout << nusize/1024.       << " kB";

  if ( m_photon ) include_photon( true );

 
  std::cout << "\tin " << tstop << " ms";

  std::cout << "\ttrim in " << tstop2 << " ms" << std::endl;

  //  std::cout << "appl::grid::weights " << run() << std::endl;

}
#endif


void appl::grid::construct_appl(const std::string& filename )  { 

  if ( appl_first ) { 
    appl_first = false;
    std::cout << "appl::grid() version " << VERSION << " compiled " << __DATE__ << std::endl;
  }
  
  m_ref_combined = m_ref = 0;

  struct timeval tstart = appl_timer_start();
  
  struct stat _fileinfo;
  if ( stat(filename.c_str(),&_fileinfo) )   {    
    throw exception( std::string("grid::grid() cannot find file ") + filename  ); 
  }

  std::cout << "appl::grid() reading grid from file " << filename;
  

  appl::file gridfile( filename, "r" );

  if ( !gridfile.isopen() ) {
    std::cout << std::endl;
    throw exception( std::string("grid::grid() cannot open file: ") + filename );
  }

  
  stream_vector<std::string> tags = gridfile.Read<stream_vector<std::string> >( "Tags" );

  if ( tags.size()<3 ) { 
    std::cout << std::endl;
    throw exception( std::string("grid::grid() cannot get tags: ") + filename );
  }

  m_transform  = tags[0];
  m_genpdfname = tags[1];
  std::string _version = tags[2];


  /// ah ha !! this is messed up, in the older version, 
  /// documentation only gets added if it was a non-empty string
  /// and similarly with the pdf of the calculation, 
  /// so this pdf might be at position 3 if there is no 
  /// documentation, so then when reading back, the pdf
  /// would go into the documentation and the version would 
  /// be undefined. 
  ///
  /// In this version, the documntation is always 
  /// added, even if it is an empty string  

  if ( tags.size()>3 ) m_documentation = tags[3];

  std::cout << "\t:  applgrid version " << _version;
  if ( _version != m_version ) std::cout  << " (transformed to " << m_version << ")";

  /// encoded pdfset used for the generation 
  if ( tags.size()>4 ) { 
    std::string tmp = tags[4];
    if ( tmp.find(":")!=std::string::npos ) {  
      m_genwithpdf  = tmp.substr(0,tmp.find(":"));
      m_genwithipdf = std::stoi(tmp.substr(tmp.find(":")+1,std::string::npos));
    }
    else { 
      m_genwithpdf  = tmp;
      m_genwithipdf = 0;
    }
    std::cout << "\t:  grid generated with " << m_genwithpdf << " : set " << m_genwithipdf;
  }

  std::cout << std::endl;

  //  std::cout << "trying qtransform" << std::endl;

  stream_vector<std::string> qtags = gridfile.Read<stream_vector<std::string> >( "QTags" ); 

  if ( qtags.size() ) m_qtransform = "h0"; 
  else                m_qtransform = qtags[0];   

  if ( getDocumentation()!="" ) std::cout << getDocumentation() << std::endl; 
  
  bool added = false;

  if ( m_genpdfname=="basic" ) { 
    added = true;
    m_genpdfname = "basic.config"; 
    if ( contains(m_genpdfname, ".dat") ||  contains(m_genpdfname, ".config") ) addpdf(m_genpdfname);
    findgenpdf( m_genpdfname );
  }


  stream_vector<double> setup = gridfile.Read<stream_vector<double> >( "State" );

  m_run        = setup[0];
  m_optimised  = ( setup[1]!=0 ? true : false );
  m_symmetrise = ( setup[2]!=0 ? true : false );  

  m_leading_order = int(setup[3]+0.5);  
  m_order         = int(setup[4]+0.5);  

  if ( setup.size()>5 ) m_cmsScale = setup[5];
  else                  m_cmsScale = 0;
 
  if ( setup.size()>6 ) m_normalised = ( setup[6]!=0 ? true : false );
  else                  m_normalised = true;

  if ( setup.size()>7 ) m_applyCorrections = ( setup[7]!=0 ? true : false );
  else                  m_applyCorrections = false;

  int n_userdata = 0;
  if ( setup.size()>10 ) { 
    n_userdata = int(setup[10]+0.5);
    // std::cout << "appl::grid userdata: " << n_userdata << std::endl;
  }


  if ( setup.size()>11 ) m_use_duplicates = ( int(setup[11]+0.5) ? true : false );

  std::vector<std::vector<double> > _ckm2;
  std::vector<std::vector<double> > _ckm;


  /// check whether we need to read in the ckm matrices
 
  if ( setup.size()>8 && setup[8]!=0 ) {

    //    std::cout << "grid::grid() read ckm matrices" << std::endl;
    
    /// now try usual 3x3 matrix

    if ( gridfile.index().find( "CKM" ).size > 0 ) { 
      
      stream_vector<double> ckmflat = gridfile.Read< stream_vector<double> >( "CKM" );

      if ( ckmflat.size()>0 ) { 
	_ckm = std::vector<std::vector<double> >(3, std::vector<double>(3) );
	
	for ( int ic=0 ; ic<3 ; ic++ ) { 
	  for ( int id=0 ; id<3 ; id++ ) _ckm[ic][id] = ckmflat[ic*3+id]; 
	}
      }  
    }
    else { 

      /// try 14 x 14 squared ckm matrix 

      if ( gridfile.index().find( "CKM2" ).size > 0 ) { 
	
	stream_vector<double> ckm2flat = gridfile.Read< stream_vector<double> >( "CKM2" );
	
	if ( ckm2flat.size()>0 ) { 
	  int Nrows = 13;
	  if ( ckm2flat.size()>169 ) Nrows = 14; 
	  if ( ckm2flat.size()>0 ) { 
	    _ckm2 = std::vector<std::vector<double> >(Nrows, std::vector<double>(Nrows) );
	    
	    for ( int ic=0 ; ic<Nrows ; ic++ ) { 
	      for ( int id=0 ; id<Nrows ; id++ ) _ckm2[ic][id] = ckm2flat[ic*Nrows+id]; 
	    }
	    
	    if ( Nrows==14 ) { 
	      /// photons ??
	      for ( int ic=0 ; ic<14 ; ic++ ) _ckm2[13][ic] = 0; 
	      for ( int ic=0 ; ic<14 ; ic++ ) _ckm2[ic][13] = 0; 
	    } 
	    
	  }
	}
      }
      
    }

  }

  if ( setup.size()>9 ) m_type = (CALCULATION)int( setup[9]+0.5 );
  else                  m_type = STANDARD;

  //  std::cout << "appl::grid() reading grid calculation type: " << _calculation(m_type) << std::endl;
  //  std::cout << "appl::grid() normalised: " << getNormalised() << std::endl;

  /// check to see if we require a generic pdf from a text file, and 
  /// and if so, create the required generic pdf (or lumi_pdf for amcatnlo)
  //  if ( m_genpdfname.find(".dat")!=std::string::npos ) addpdf(m_genpdfname);

  //  std::cout << "appl::grid() requested pdf combination " << m_genpdfname << std::endl;
  

  if ( !added && contains(m_genpdfname, ".config") ) { 
    /// decode the pdf combination if appropriate

    // find out if we have one combination per order or one overall
    std::vector<std::string> namevec = parse( m_genpdfname, ":" );

    std::string label = "Combinations";

    for ( unsigned i=0 ; i<namevec.size() ; i++ ) { 

      /// again have to use TVectorT<double> because TVectorT<int> has no constructor!!!
      /// I ask you!! what's the point of a template if it doesn't actually instantiate
      /// it's pathetic!

      stream_vector<double> _combinations = gridfile.Read<stream_vector<double> >( label );

      //   std::cout << "combinations: " << label << " " << _combinations << std::endl;

      label += "N"; /// add an N for each order, N-LO, NN-LO etc

      if ( _combinations.size()==0 ) throw exception( std::string( "grid::grid() cannot read pdf combination " ) + namevec[i] );

      std::vector<int> combinations(_combinations.size());

      for ( unsigned ic=0 ; ic<combinations.size() ; ic++ )  combinations[ic] = int(_combinations[ic]); 
    
      //      std::cout << "combinations: " << label << " " << combinations << std::endl;

      //      addpdf(m_genpdfname, combinations);
      addpdf( namevec[i], combinations );

    }
  }
  else { 
    /// of just create the generic from the file
    if ( contains(m_genpdfname, ".dat") ) addpdf(m_genpdfname);
  }
  
  /// retrieve the pdf routine 
  findgenpdf( m_genpdfname );

  m_photon = false;

  /// check whether there are any photon contributions in this grid ...
 
  for ( int iord=0 ; iord<m_order ; iord++ ) {
    const lumi_pdf* lpdf = dynamic_cast<const lumi_pdf*>(m_genpdf[iord]);
    if ( lpdf && ( lpdf->contains(7) || lpdf->contains(22) )  ) m_photon = true;
  }

  //  std::cout << "grid::grid() read " << m_genpdfname << " " << m_genpdf[0]->getckmsum().size() << std::endl; 
  //  std::cout << "grid::grid() read " << *dynamic_cast<const lumi_pdf*>(m_genpdf[0]) << std::endl; 
  //  std::cout << "grid::grid() read " << *dynamic_cast<const lumi_pdf*>(m_genpdf[1]) << std::endl; 
  //  std::cout << "grid::grid() read " << *dynamic_cast<const lumi_pdf*>(m_genpdf[2]) << std::endl; 
  
  // set the ckm matrices 
  if      ( _ckm.size()>0 )  setckm( _ckm );
  else if ( _ckm2.size()>0 ) setckm2( _ckm2 );

  if ( gridfile.index().find( "reference_internal" ).size > 0 ) { 
    m_ref = new appl::TH1D( gridfile.Read<appl::TH1D>( "reference_internal" ) );
    if ( gridfile.index().find( "reference" ).size > 0 ) { 
      m_ref_combined = new appl::TH1D( gridfile.Read<appl::TH1D>( "reference" ) );
      if ( run() ) m_ref_combined->Scale(run());
    }
    else { 
      throw exception( "cannot read reference histogram" );
    }
  }
  else { 
    if ( gridfile.index().find( "reference" ).size > 0 ) { 
      m_ref = new appl::TH1D( gridfile.Read<appl::TH1D>( "reference" ) );
      m_ref_combined = m_ref;
    }
    else { 
      throw exception( "cannot read reference histogram" );
    }
  }

  if ( run() ) m_ref->Scale(run());

  m_ref->SetName("referenceInternal");
  if ( m_normalised && m_optimised ) m_read = true;

  /// also calculate maximum, values for Q2 and y=ln(1/x)
  /// for hoppet initialisation

  double _Q2max = 0;
  double _xmin  = 2; // always bigger than 1

  for( int iorder=0 ; iorder<m_order ; iorder++ ) {
    // m_grids[iorder] = new igrid*[Nobs_internal()];  
    m_grids[iorder].resize(Nobs_internal());  
    for( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) {

      char name[128];  sprintf(name, "weight[alpha-%d][%03d]", iorder, iobs);

      m_grids[iorder][iobs] = new igrid( gridfile, name);
      m_grids[iorder][iobs]->setparent( this ); 

      if (  !m_grids[iorder][iobs]->empty() ) { 

	double x1 = m_grids[iorder][iobs]->x1filledmin();
	double x2 = m_grids[iorder][iobs]->x2filledmin();	
	double Q2 = m_grids[iorder][iobs]->Q2filledmax();
	double Q2i = m_grids[iorder][iobs]->Q2filledmin();
	
	if ( Q2i>Q2 ) Q2 = Q2i;

	if ( m_grids[iorder][iobs]->isDISgrid() ) m_isDIS = true;

	if (  Q2>_Q2max ) _Q2max = Q2;
	if (  x1<_xmin )   _xmin = x1;
	if (  x2<_xmin )   _xmin = x2;
      }

      //    _size += m_grids[iorder][iobs]->size();
      //      std::cout << "grid::grid() done" << std::endl;
    }
  }

  if ( _Q2max>0 )           m_Qmax = std::sqrt(_Q2max);
  if ( _xmin>0 && _xmin<2 ) m_ymax = std::log(1/_xmin);
  
  //  std::cout << "Qmax " << m_Qmax << std::endl; 
  //  std::cout << "ymax " << m_ymax << std::endl;

  if ( gridfile.index().find("Corrections").size > 0 ) { 
    
    m_correctionLabels = gridfile.Read<stream_vector<std::string> >( "CorrectionLabels" ).payload();
    
    std::vector<std::vector<double> > corrections = gridfile.Read< stream_vector<std::vector<double> > >( "Corrections" ).payload();
    
    m_applyCorrection = std::vector<bool>( corrections.size(), false );
    
    m_corrections.clear();
    for ( size_t i=0 ; i<corrections.size() ; i++ ) { 
      m_corrections.push_back( *getReference() );
      m_corrections.back().name() = m_correctionLabels[i];
      m_corrections.back().set( corrections[i] );
      std::cout << "correction: " <<  m_corrections.back().name() << std::endl; 
      /// std::cout << m_corrections.back() << std::endl;
    }

  }


  /// now write out vector of bins to be combined if this has been set
  if ( gridfile.index().find("CombineBins").size > 0 ) {
    std::vector<double> combine = gridfile.Read<stream_vector<double> >( "CombineBins" ).payload(); 
    m_combine.clear();
    m_combine.resize(combine.size());
    for ( unsigned i=m_combine.size() ; i-- ; ) m_combine[i] = combine[i]; 

    if ( m_combine.size() ) combineReference();
  }


  m_userdata = gridfile.Read<stream_vector<double> >("UserData").payload();

  gridfile.Close();

  double tstop = appl_timer_stop( tstart );

  unsigned usize = size();

  struct timeval tstart2 =  appl_timer_start();

  trim();

  double tstop2 = appl_timer_stop( tstart2 );

  std::cout << "appl::grid() read grid, size ";
  if ( usize>1024*10 ) std::cout << usize/1024./1024. << " MB";
  else                 std::cout << usize/1024.       << " kB";

  //  std::cout << " -> ";

  //  unsigned nusize = size();

  //  if ( nusize>1024*10 ) std::cout << nusize/1024./1024. << " MB";
  //  else                  std::cout << nusize/1024.       << " kB";

  if ( m_photon ) include_photon( true );

  std::cout << "\tin " << tstop << " ms";

  std::cout << "\ttrim in " << tstop2 << " ms" << std::endl;

  //  std::cout << "appl::grid::weights " << run() << std::endl;
  
}



appl::grid::grid(const grid& g) : 
  m_ref(new appl::TH1D(*g.m_ref)), 
  m_leading_order(g.m_leading_order), m_order(g.m_order), 
  m_run(g.m_run), m_optimised(g.m_optimised), m_trimmed(g.m_trimmed), 
  m_normalised(g.m_normalised),
  m_symmetrise(g.m_symmetrise),
  m_transform(g.m_transform),
  m_qtransform(g.m_qtransform),
  m_genpdfname(g.m_genpdfname), 
  m_cmsScale(g.m_cmsScale),
  m_dynamicScale(g.m_dynamicScale),
  m_applyCorrections(g.m_applyCorrections), 
  m_documentation(g.m_documentation),
  m_ckmsum(g.m_ckmsum), /// need a deep copy of the contents
  m_ckm2(g.m_ckm2),     /// need a deep copy of the contents
  m_ckm(g.m_ckm),       /// need a deep copy of the contents
  m_type(g.m_type),
  m_read(g.m_read),
  m_bin(-1),
  m_genwithpdf(""),
  m_photon(g.m_photon),
  m_isDIS(g.m_isDIS),
  m_use_duplicates(g.m_use_duplicates)
{
  m_ref->SetDirectory(0);
  m_ref->Sumw2();

  m_ref_combined = m_ref;
  if ( g.m_ref_combined != g.m_ref) { 
    m_ref_combined = new appl::TH1D(*g.m_ref_combined); 
    m_ref_combined->SetDirectory(0);
  }

  /// check to see if we require a generic pdf from a text file, and 
  /// and if so, create the required generic pdf
  //  if ( m_genpdfname.find(".dat")!=std::string::npos ) addpdf(m_genpdfname);
  if ( contains(m_genpdfname, ".dat") ||  contains(m_genpdfname, ".config") ) addpdf(m_genpdfname);
  findgenpdf( m_genpdfname );

  for ( int iorder=0 ; iorder<m_order ; iorder++ ) { 
    //    m_grids[iorder] = new igrid*[Nobs_internal()];
    m_grids[iorder].resize(Nobs_internal());
    for ( int iobs=0 ; iobs<Nobs_internal() ; iobs++ )  { 
      m_grids[iorder][iobs] = new igrid(*g.m_grids[iorder][iobs]);
      m_grids[iorder][iobs]->setparent( this ); 
    }
  }
} 


// Initialize histogram that saves the correspondence obsvalue<->obsbin

// constructor common internals 
void appl::grid::construct(int Nobs, 
			   int NQ2,  double Q2min, double Q2max, int Q2order,
			   int Nx,   double xmin,  double xmax,  int xorder, 
			   int order, 
			   std::string transform, 
			   std::string qtransform, 
			   bool isdis ) { 
  
  //  std::cout << "appl::grid::construct() m_order " << m_order << "\tNobs " << Nobs << std::endl; 

  if ( m_order!=order ) std::cerr << "appl::grid::construct() order mismatch" << std::endl;

  //  for ( int iorder=0 ; iorder<m_order ; iorder++ ) m_grids[iorder] = 0;
  for ( int iorder=0 ; iorder<m_order ; iorder++ ) m_grids[iorder].clear();

  for ( int iorder=0 ; iorder<m_order ; iorder++ ) { 
    //    m_grids[iorder] = new igrid*[Nobs];
    m_grids[iorder].resize(Nobs);
    for ( int iobs=0 ; iobs<Nobs ; iobs++ ) {
      m_grids[iorder][iobs] = new igrid(NQ2, Q2min, Q2max, Q2order, 
					Nx,   xmin,  xmax,  xorder,  transform, qtransform, m_genpdf[iorder]->Nproc(), isdis);
      m_grids[iorder][iobs]->setparent( this ); 
    }
  }
  //  std::cout << "appl::grid::construct() return" << std::endl; 
}




  // number of subprocesses 
int appl::grid::subProcesses(int i) const { 
  if ( i<0 || i>=m_order ) { 
    std::stringstream s;
    s << "grid::subProcess(int i) " << i << " out of range [0-" << m_order-1 << "]";
    throw exception( s.str() );
  }
  return m_grids[i][0]->SubProcesses();     
}  


/// access the transform functions for the appl::igrid so that the 
/// igrid can be hidden 
double appl::grid::transformvar()         { return igrid::transformvar(); }
double appl::grid::transformvar(double v) { return igrid::transformvar(v); }

// add a single grid
void appl::grid::add_igrid(int bin, int order, igrid* g) { 

  if ( !(order>=0 && order<m_order) ) { 
    std::cerr << "grid::add_igrid() order out of range " << order << std::endl; 
    return;
  } 

  if ( !(bin>=0 && bin<Nobs_internal() ) ) {
    //    std::cerr << "grid::add_igrid() observable bin out of range " << bin << std::endl; 
    return;
  }

  m_grids[order][bin] = g;
  m_grids[order][bin]->setparent(this);

  if ( g->transform()!=m_transform ) { 
    std::cerr << "grid::add_igrid() transform " << m_transform 
	      << " does not match that from added igrid, " << g->transform() << std::endl;
    m_transform = g->transform();
  }

  if ( g->transform()!=m_transform ) { 
    std::cerr << "grid::add_igrid() transform " << m_transform 
	      << " does not match that from added igrid, " << g->transform() << std::endl;
    m_transform = g->transform();
  }

}




appl::grid::~grid() {

  for( int iorder=0 ; iorder<m_order ; iorder++ ) {  
    if( m_grids[iorder].size() ) { 
      for ( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) { 
	delete m_grids[iorder][iobs];
      }
      //      delete[] m_grids[iorder];
      //      m_grids[iorder] = 0;
      m_grids[iorder].clear();
    }
  }

  if (m_ref_combined) {
    if ( m_ref_combined!=m_ref) delete m_ref_combined;
  }

  if (m_ref) delete m_ref;
  m_ref=0;
  m_ref_combined = 0;

#ifdef HAVE_HOPPET
  if ( hoppet ) delete hoppet; 
  hoppet=0; 
#endif

}




// algebraic operators

appl::grid& appl::grid::operator=(const appl::grid& g) { 
  // clear out the old...
  if ( m_ref_combined!=m_ref ) delete m_ref_combined;
  delete m_ref;
  for ( int iorder=0 ; iorder<m_order ; iorder++ ) { 
    for ( int iobs=0 ; iobs<Nobs_internal() ; iobs++ )  delete m_grids[iorder][iobs];
    // delete m_grids[iorder];
    m_grids[iorder].clear();
  }
  
  // copy the new
  m_ref = new appl::TH1D(*g.m_ref);
  m_ref->SetDirectory(0);
  m_ref->Sumw2();

  m_ref_combined = m_ref;

  // copy the state
  m_leading_order = g.m_leading_order;
  m_order         = g.m_order;

  m_run       = g.m_run;
  m_optimised = g.m_optimised;
  m_trimmed   = g.m_trimmed;

  for ( int iorder=0 ; iorder<m_order ; iorder++ ) { 
    /// m_grids[iorder] = new igrid*[Nobs_internal()];
    m_grids[iorder].resize(Nobs_internal());
    for ( int iobs=0 ; iobs<Nobs_internal() ; iobs++ )  { 
      m_grids[iorder][iobs] = new igrid(*g.m_grids[iorder][iobs]);
      m_grids[iorder][iobs]->setparent( this ); 
    }
  }
  return *this;
} 
  


appl::grid& appl::grid::operator*=(const std::vector<std::vector<double> >& vv ) {

  if ( vv.size()<unsigned(m_order) ) return *this; 

  for( int iorder=0 ; iorder<m_order ; iorder++ ) {
    /// don't do anything if the scale vector is not the correct size
    if ( vv[iorder].size() != size_t(Nobs_internal()) ) return *this;
  }

  for( int iorder=0 ; iorder<m_order ; iorder++ ) {
    for( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) (*m_grids[iorder][iobs])*=vv[iorder][iobs]; 
  }

#if 0   
  for ( int i=0 ; i<getReference()->GetNbinsX() ; i++ ) {
    int ih=i+1;
    getReference()->SetBinContent( ih,  getReference()->GetBinContent(ih)*v[i] );
    getReference()->SetBinError(   ih,  getReference()->GetBinError(ih)*v[i] );
  }

  for ( int i=0 ; i<getReference_internal()->GetNbinsX() ; i++ ) {
    int ih=i+1;
    //   std::cout << "v[" << i << "] = " << v[i] << std::endl;
    getReference_internal()->SetBinContent( ih,  getReference_internal()->GetBinContent(ih)*v[i] );
    getReference_internal()->SetBinError(   ih,  getReference_internal()->GetBinError(ih)*v[i] );
  }

  combineReference(true);

#endif

  return *this;
}




double integral( appl::TH1D* h ) { 
  double d = 0;
  for ( int i=0 ; i<h->GetNbinsX() ; i++ ) d += h->GetBinContent(i+1);
  return d;
}


appl::grid& appl::grid::operator*=(const double& d) { 
  for( int iorder=0 ; iorder<m_order ; iorder++ ) {
    for( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) (*m_grids[iorder][iobs]) *= d; 
  }
  getReference()->Scale( d );
  if(getReference()!=getReference_internal()) getReference_internal()->Scale( d );
  combineReference(true);

  return *this;
}


void hprint( appl::TH1D* h ) { 
  for ( int i=1 ; i<=h->GetNbinsX() ; i++ ) std::cout << h->GetBinContent(i) << " ";
  std::cout << std::endl;
}




/// this is messed up - how can we apply "per bin" normalisations
/// it we have specified more actual bins and are combining them 
/// a-posteriori ? 
/// Fixme: at some point *remove* the bin combining functionality
///        it is far too much of a pain to maintain

appl::grid& appl::grid::operator*=(const std::vector<double>& v) {

  /// don't do anything if the scale vector is not the correct size
  if ( v.size() != size_t(Nobs_internal()) ) return *this;

  for( int iorder=0 ; iorder<m_order ; iorder++ ) {
    for( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) (*m_grids[iorder][iobs])*=v[iobs]; 
  }

#if 0   
  for ( int i=0 ; i<getReference()->GetNbinsX() ; i++ ) {
    int ih=i+1;
    getReference()->SetBinContent( ih,  getReference()->GetBinContent(ih)*v[i] );
    getReference()->SetBinError(   ih,  getReference()->GetBinError(ih)*v[i] );
  }
#endif

  for ( int i=0 ; i<getReference_internal()->GetNbinsX() ; i++ ) {
    int ih=i+1;
    //   std::cout << "v[" << i << "] = " << v[i] << std::endl;
    getReference_internal()->SetBinContent( ih,  getReference_internal()->GetBinContent(ih)*v[i] );
    getReference_internal()->SetBinError(   ih,  getReference_internal()->GetBinError(ih)*v[i] );
  }

  combineReference(true);

  return *this;
}



double appl::grid::fx(double x) const { 
  if ( m_order>0 && Nobs_internal()>0 ) return m_grids[0][0]->fx(x);
  else return 0;
}

double appl::grid::fy(double x) const { 
  if ( m_order>0 && Nobs_internal()>0 ) return m_grids[0][0]->fy(x);
  else return 0;
}


appl::grid& appl::grid::operator+=(const appl::grid& g) {
  m_run      += g.m_run;
  m_optimised = g.m_optimised;
  m_trimmed   = g.m_trimmed;
  if ( Nobs_internal()!=g.Nobs_internal() ) throw exception("grid::operator+ Nobs bin mismatch");
  if ( m_order!=g.m_order )                 throw exception("grid::operator+ different order grids");
  if ( m_leading_order!=g.m_leading_order ) throw exception("grid::operator+ different order processes in grids");
  for( int iorder=0 ; iorder<m_order ; iorder++ ) {
    for( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) { 
      (*m_grids[iorder][iobs]) += (*g.m_grids[iorder][iobs]); 
    }
  }

  /// grrr use root TH1::Add() even though I don't like it. 
  //  getReference()->Add( g.getReference() );
  *(getReference_internal()) += *(g.getReference_internal());

  combineReference(true);

  return *this;
}




/// check grids match
bool appl::grid::operator==(const appl::grid& g) const {
  if ( Nobs_internal()!=g.Nobs_internal() ) return false;
  if ( m_order!=g.m_order ) return false;
  if ( m_leading_order!=g.m_leading_order ) return false;
  for( int iorder=0 ; iorder<m_order ; iorder++ ) {
    //    for( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) match &= ( (*m_grids[iorder][iobs]) == (*g.m_grids[iorder][iobs]) ); 
    for( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) { 
      if ( ! m_grids[iorder][iobs]->compare_axes( *g.m_grids[iorder][iobs] ) ) return false; 
    }
  }
  return true;
}



// fill the appropriate igrid with these weights
void appl::grid::fill(const double x1, const double x2, const double Q2, 
		      const double obs, 
		      const double* weight, const int iorder)  {  
  int iobs = m_ref->FindBin(obs)-1;

  //  std::cout << "grid::fill() iobs " << iobs << "\tobs=" << obs << std::endl;

  if ( iobs<0 || iobs>=Nobs_internal() ) {
    //    cerr << "grid::fill() iobs out of range " << iobs << "\tobs=" << obs << std::endl;
    //    cerr << "obs=" << obs << "\tobsmin=" << obsmin() << "\tobsmax=" << obsmax() << std::endl;
    return;
  }
  
  //  std::cout << "iobs=" << iobs << "\tobs=" << obs;
  //  for ( int i=0 ; i<subProcesses(iorder) ; i++ ) std::cout << "\t" << weight[i];
  //  std::cout << std::endl;

  //  std::cout << "\tiobs=" << iobs << std::endl;
  if ( m_symmetrise && x2<x1 )  m_grids[iorder][iobs]->fill(x2, x1, Q2, weight);
  else                          m_grids[iorder][iobs]->fill(x1, x2, Q2, weight);
}


// fast fill pre-optimisation don't perform the interpolation and so on
void appl::grid::fill_phasespace(const double x1, const double x2, const double Q2, 
				 const double obs, 
				 const double* weight, const int iorder) {
  int iobs = m_ref->FindBin(obs)-1;

  //  std::cout << "grid::fill_phasespace() iobs " << iobs << "\tobs=" << obs << std::endl;

  if ( iobs<0 || iobs>=Nobs_internal() ) {
    // std::cerr << "grid::fill() iobs out of range " << iobs << "\tobs=" << obs << std::endl;
    // std::cerr << "obs=" << obs << "\tobsmin=" << obsmin() << "\tobsmax=" << obsmax() << std::endl;
    return;
  }
  if ( m_symmetrise && x2<x1 )  m_grids[iorder][iobs]->fill_phasespace(x2, x1, Q2, weight);
  else                          m_grids[iorder][iobs]->fill_phasespace(x1, x2, Q2, weight);
}




// fast fill pre-optimisation don't perform the interpolation and so on
void appl::grid::fill_index(const int ix1, const int ix2, const int iQ2, 
			    const int iobs, 
			    const double* weight, const int iorder) {

  if ( iobs<0 || iobs>=Nobs_internal() ) {
    //  cerr << "grid::fill() iobs out of range " << iobs << "\tobs=" << obs << std::endl;
    //  cerr << "obs=" << obs << "\tobsmin=" << obsmin() << "\tobsmax=" << obsmax() << std::endl;
    return;
  }
  if ( m_symmetrise && ix2<ix1 )  m_grids[iorder][iobs]->fill_index(ix2, ix1, iQ2, weight);
  else                            m_grids[iorder][iobs]->fill_index(ix1, ix2, iQ2, weight);
}


void appl::grid::trim(int iorder) {
  if ( iorder>=0 ) { 
    if ( iorder<m_order ) for( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) m_grids[iorder][iobs]->trim(); 
  }
  else { 
    m_trimmed = true;
    for( int iord=0 ; iord<m_order ; iord++ ) {
      for( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) m_grids[iord][iobs]->trim(); 
    }
  }
}

void appl::grid::untrim(int iorder) {
  if ( iorder>=0 ) { 
    if ( iorder<m_order ) for( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) m_grids[iorder][iobs]->untrim(); 
  }
  else { 
    m_trimmed = false;
    for( int iord=0 ; iord<m_order ; iord++ ) {
      for( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) m_grids[iord][iobs]->untrim(); 
    }
  }
}

std::ostream& appl::grid::print(std::ostream& s) const {
  for( int iorder=0 ; iorder<m_order ; iorder++ ) {
    for( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) {     
      s << iobs << "\t" 
	<< std::setprecision(5) << std::setw(6) << getReference()->GetBinLowEdge(iobs+1) << "\t- " 
	<< std::setprecision(5) << std::setw(6) << getReference()->GetBinLowEdge(iobs+2) << "\t"; 
      m_grids[iorder][iobs]->print(s);       
    }
  }
  return s;
}


std::ostream& appl::grid::printpdf(std::ostream& s) const {
  for( int i=0 ; i<m_order ; i++ ) { 
    if ( m_genpdf[i] ) s << "order: " << i << " " << *m_genpdf[i] << "\n";
  }
  return s;
}



std::ostream& appl::grid::debug(std::ostream& s) const {
  for( int iorder=0 ; iorder<m_order ; iorder++ ) {
    for( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) {     
      s << iobs << "\t" 
	<< std::setprecision(5) << std::setw(6) << getReference()->GetBinLowEdge(iobs+1) << "\t- " 
	<< std::setprecision(5) << std::setw(6) << getReference()->GetBinLowEdge(iobs+2) << "\t"; 
      m_grids[iorder][iobs]->debug(s);       
    }
  }
  return s;
}




 /// get the required pdf combinations from those registered   
void appl::grid::findgenpdf( std::string s ) { 
    std::vector<std::string> names = parse( s, ":" );
    if ( names.size()==unsigned(m_order) ) for ( int i=0 ; i<m_order ; i++ ) m_genpdf[i] = appl_pdf::getpdf( names[i] );
    else  if ( names.size()==1 )           for ( int i=0 ; i<m_order ; i++ ) m_genpdf[i] = appl_pdf::getpdf( names[0] );
    else  { 
      std::stringstream s_;
      s_ << "requested " << m_order << " pdf combination but given " << names.size();
      throw exception( s_.str() );
    }
}


void appl::grid::setGenpdf( const std::string& genpdfname ) {
   
   m_genpdfname = genpdfname;
   addpdf( m_genpdfname );
   findgenpdf( genpdfname );
 
   std::cout << "appl::grid::shrink()  new " 
 	    << "\t" << m_genpdf[0]->name() << ":" << m_genpdf[0]->size() 
 	    << "\t" << m_genpdf[1]->name() << ":" << m_genpdf[1]->size() 
	    <<  std::endl;
   std::cout << "appl::grid::setGenpdf()  "; 
   for ( int i=0 ; i<MAXGRIDS ; i++ ) {
     if ( m_genpdf[i] ) std::cout << "\t" << m_genpdf[1]->name() << ":" << m_genpdf[i]->size(); 
   }
   std::cout <<  std::endl;
 
}


void appl::grid::addpdf( const std::string& s, const std::vector<int>& combinations ) {

  //  std::cout << "addpdf() in " << std::endl;

    /// parse names, if they contain .dat, then create the new generic pdfs
    /// they will be added to the pdf std::map automatically 
    std::vector<std::string> names = parse( s, ":" );

    unsigned imax = unsigned(m_order); 

    /// check to see whether we have a different pdf for each order
    if ( names.size()!=imax ) { 
      if ( names.size()==1 ) imax = 1;
      else { 
	std::stringstream s_;
	s_ << "requested " << m_order << " pdf combination but given " << names.size();
	throw exception( s_.str() );
      }
    }

    //    std::cout << "imax " << imax << std::endl; 

    /// loop through all the required pdfs checking whether they exist already,
    /// if not (from thrown exception) then create it, otherwise, don't need to 
    /// do anything 
    for ( unsigned i=0 ; i<imax ; i++ ) { 

      if ( names[i].find(".dat")!=std::string::npos ) {

	if ( appl_pdf::getpdf(names[i])==0 ) { 
 	  std::cout << "appl::grid::addpdf() creating new generic_pdf " << names[i] << std::endl;
	  new generic_pdf(names[i]);
	}

	// 	try {
	// 	  appl_pdf::getpdf(names[i]); // , false);
	// 	}
	// 	catch ( appl_pdf::exception e ) { 
	// 	  std::cout << "creating new generic_pdf " << names[i] << std::endl;
	// 	  new generic_pdf(names[i]);
	// 	}

      }
      else if ( names[i].find(".config")!=std::string::npos ) { 

	if ( appl_pdf::getpdf(names[i])==0 ) { 
	  //	  std::cout << "appl::grid::addpdf() creating new lumi_pdf " << names[i] << std::endl;
	  lumi_pdf* lp = new lumi_pdf(names[i], combinations);
	  //  latex( *lp );
	  if ( !m_use_duplicates ) lp->removeDuplicates();
	  //	  latex( *lp, "duff" );
	}

	// 	try {
	// 	  appl_pdf::getpdf(names[i]); // , false);
	// 	}
	// 	catch ( appl_pdf::exception e ) { 
	// 	  std::cout << "creating new lumi_pdf " << names[i] << std::endl;
	// 	  new lumi_pdf(names[i], combinations);
	// 	  //	  std::cout << "created" << names[i] << std::endl;
	
	// 	}

      }

    }

}



void appl::grid::setckm2( const std::vector<std::vector<double> >& ckm2 ) { 
  for ( int i=0 ; i<m_order ; i++ ) m_genpdf[i]->setckm2(ckm2);
}


void appl::grid::setckm( const std::vector<std::vector<double> >& ckm ) { 
  for ( int i=0 ; i<m_order ; i++ ) m_genpdf[i]->setckm(ckm);
}


void appl::grid::setckm( const std::vector<double>& ckm ) {
  std::vector<std::vector<double> > _ckm(3, std::vector<double>(3,0) );
  for ( unsigned i=0 ; i<ckm.size() && i<9 ; i++ ) _ckm[i/3][i%3] = ckm[i]; 
  for ( int i=0 ; i<m_order ; i++ ) m_genpdf[i]->setckm(_ckm);
}

void appl::grid::setckm( const double* ckm ) { 
  std::vector<std::vector<double> > _ckm(3, std::vector<double>(3,0) );
  for ( unsigned i=0 ; i<9 ; i++ ) _ckm[i/3][i%3] = ckm[i]; 
  for ( int i=0 ; i<m_order ; i++ ) m_genpdf[i]->setckm(_ckm);
}


const std::vector<std::vector<double> >& appl::grid::getckm()  const { return m_genpdf[0]->getckm(); }  

const std::vector<std::vector<double> >& appl::grid::getckm2() const { return m_genpdf[0]->getckm2(); }  


// void appl::grid::setuppdf(void (*pdf)(const double&, const double&, double* ) )  {  }
// void grid::pdfinterp(double x, double Q2, double* f) {  }



// set the rewight flag of the internal grids
bool appl::grid::reweight(bool t) { 
  for( int iorder=0 ; iorder<m_order ; iorder++ ) {
    for( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) m_grids[iorder][iobs]->reweight(t);       
  }
  return t;
}


bool exists( const std::string& filename ) { 
  struct stat sb;
  if ( stat( filename.c_str(), &sb)==0 ) return true; // && S_ISREG(sb.st_mode ))
  else return false;
}


void appl::grid::Write(const std::string& filename, 
		       const std::string& dirname, 
		       const std::string& pdfname) {
  std::cout << "Write(): filename:  " << filename << std::endl;
  if ( filename.find(".root")==filename.size()-5 ) { 
#   ifdef USEROOT
    Write_root( filename, dirname, pdfname );
#   else    
    std::cerr << "appl::grid::Write() root file requested, but root not compiled in" << std::endl;
    std::exit(-1);
#   endif
  }
  else  Write_appl( filename, dirname, pdfname );
}
  

#ifdef USEROOT
// dump to file
void appl::grid::Write_root(const std::string& filename, 
			    const std::string& dirname, 
			    const std::string& pdfname) 
{ 

  std::cout << "appl::grid::Write() " << filename << "\tdirname " << dirname << "\tpdfname " << pdfname << std::endl; 

  if ( exists( filename ) ) { 
    std::string filename_save = filename + "-save";
    if ( std::rename( filename.c_str(), filename_save.c_str() ) ) std::cerr << "could not rename grid file " << filename << std::endl;
  } 

  if ( pdfname!="" ) shrink( pdfname, m_genpdf[0]->getckmcharge() );

  //  std::cout << "grid::Write() writing to file " << filename << std::endl;
  TFile rootfile(filename.c_str(),"recreate");

  //  std::cout << "pwd=" << gDirectory->GetName() << std::endl;

  Directory d(dirname);
  d.push();
  
  //  std::cout << "pwd=" << gDirectory->GetName() << std::endl;

  // write the name of the transform pair and the
  // generalised pdf
  TFileString _tags("Tags");
  _tags.add(m_transform);
  _tags.add(m_genpdfname);
  _tags.add(m_version);
  //  if ( m_documentation!="" ) 
  _tags.add( m_documentation );
  if ( m_genwithpdf!="" )    _tags.add( m_genwithpdf + ":"+std::to_string(m_genwithipdf) );
  _tags.Write();

  TFileString _qtags("QTags");
  _qtags.add(m_qtransform);
  _qtags.Write();

  //  std::cout << "genwith pdf " << m_genwithpdf << " : " << m_genwithipdf << std::endl; 

  //  appl::TH1D* _transform = new appl::TH1D("Transform", m_transform.c_str(), 1, 0, 1);  
  //  _transform->Write();

  //  appl::TH1D* _genpdfname = new appl::TH1D("GenPDF", m_genpdfname.c_str(), 1, 0, 1);
  //  _genpdfname->Write();


  //  std::cout << "state std::vector=" << std::endl;

  // state information
  TVectorT<double>* setup=new TVectorT<double>(12); // add a few extra just in case 
  (*setup)(0) = m_run;
  (*setup)(1) = ( m_optimised  ? 1 : 0 );
  (*setup)(2) = ( m_symmetrise ? 1 : 0 );
  (*setup)(3) =   m_leading_order ;
  (*setup)(4) =   m_order ;
  (*setup)(5) =   m_cmsScale ;
  (*setup)(6) = ( m_normalised ? 1 : 0 );
  (*setup)(7) = ( m_applyCorrections ? 1 : 0 );

  if ( m_genpdf[0]->getckmsum().size()==0 ) (*setup)(8) = 0;
  else                                      (*setup)(8) = 1;

  (*setup)(9) = (int)m_type;

  (*setup)(10) = m_userdata.size();

  /// by default we do not want to use duplicates, 
  /// so we want them removed
  if ( m_use_duplicates ) (*setup)(11) = 1;

  setup->Write("State");

  if ( (*setup)(8) == 1 ) { 
    
    const std::vector<std::vector<double> >& _ckm = m_genpdf[0]->getckm();

    bool useckm = false;

    if ( _ckm.size()>0 ) { 
      /// no longer write out squared ckm matrix - just use the 3x3
      TVectorT<double>* ckmflat = new TVectorT<double>(9);
      
      for ( int ic=0 ; ic<3 ; ic++ ) { 
	for ( int id=0 ; id<3 ; id++ ) { (*ckmflat)(ic*3+id) = _ckm[ic][id]; if ( _ckm[ic][id]!=0 ) useckm=true; } 
      }

      if ( useckm )  ckmflat->Write("CKM");
      else delete ckmflat;
    }

    if ( !useckm ) { 

      const std::vector<std::vector<double> >& _ckm2 = m_genpdf[0]->getckm2();

      if ( _ckm2.size()>0 ) { 
	/// no longer write out squared ckm matrix - just use the 3x3 unless it is an OLD, grid
	
	int nrows = _ckm2.size();
	
	TVectorT<double>* ckm2flat = new TVectorT<double>(nrows*nrows);
	
	for ( int ic=0 ; ic<nrows ; ic++ ) { 
	  for ( int id=0 ; id<nrows ; id++ ) (*ckm2flat)(ic*nrows+id) = _ckm2[ic][id];
	}

       	ckm2flat->Write("CKM2");
      }
    }      

  }


  /// encode the pdf combination if appropriate
  
  if ( contains( m_genpdfname, ".config" ) ) { 
    
    // find out if we have one combination per order or one overall
    
    std::vector<std::string> namevec = parse( m_genpdfname, ":" );
    
    /// now write out all the ones we need
    
    std::string label = "Combinations";
    
    for ( unsigned i=0 ; i<namevec.size() && i<unsigned(m_order) ; i++ ) {  

      //      dynamic_cast<lumi_pdf*>(m_genpdf[i])->restoreDuplicates();

      lumi_pdf* lpdf = dynamic_cast<lumi_pdf*>(m_genpdf[i]);

      std::cout << "lumi_pdf:   " << lpdf->name() << "\t processes " << lpdf->Nproc() << "\n";

      //      std::cout << "lumi pdf: " << *lpdf << std::endl;

      lumi_pdf lpdf_tmp(*lpdf);

      lpdf_tmp.restoreDuplicates();

      //      std::cout << "lumi pdf: " << *lpdf     << std::endl;
      //      std::cout << "lumi pdf: " <<  lpdf_tmp << std::endl;

      //      std::vector<int>   combinations = dynamic_cast<lumi_pdf*>(m_genpdf[i])->serialise();
      std::vector<int>   combinations = lpdf_tmp.serialise();
      TVectorT<double>* _combinations = new TVectorT<double>(combinations.size());
      for ( unsigned ic=0 ; ic<combinations.size() ; ic++ ) { 
	if ( combinations[ic]<0 ) (*_combinations)(ic) = double(combinations[ic]-0.5);
	else                      (*_combinations)(ic) = double(combinations[ic]+0.5);
      }
      
      _combinations->Write( label.c_str() );
      
      //      std::cout << "writing " << m_genpdf[i]->name() << std::endl;
      
      label += "N";  /// add an N for each order, N-LO, NN-LO etc !!!! ARGH!!! Need to be more general !!!
    }
  } 
  
  //  std::cout << "grids Nobs = " << Nobs_internal() << std::endl;
  
  untrim();
  int untrim_size = size();
  trim();
  int trim_size = size();
  std::cout <<"grid::Write() "
	    << "size(untrimmed)=" << untrim_size/1024 << " kB"
	    << "\tsize(trimmed)=" <<   trim_size/1024 << " kB"
	    << " (" << 0.1*int(trim_size*1000./untrim_size) << "%)" << std::endl;
  

  // internal grids
  for( int iorder=0 ; iorder<m_order ; iorder++ ) {
    for( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) {
      char name[128];  sprintf(name, "weight[alpha-%d][%03d]", iorder, iobs);
      // std::cout << "writing grid " << name << std::endl;
      //   _size += m_grids[iorder][iobs]->size();
      m_grids[iorder][iobs]->write(name);
      //   trim_size += m_grids[iorder][iobs]->size();
    }
  }
 
  //  d.pop();

  //  std::cout << "reference" << std::endl;

  appl::TH1D* reference          = 0;
  appl::TH1D* reference_internal = 0;

  //  std::cout << "m_ref_combined: " << m_ref_combined << std::endl;
  //  std::cout << "m_ref:          " << m_ref << std::endl;

  if ( m_ref_combined != m_ref )  { 
    reference = (appl::TH1D*)m_ref_combined->Clone("reference");
    reference->SetDirectory(0);

    reference_internal = (appl::TH1D*)m_ref->Clone("reference_internal");
    reference_internal->SetDirectory(0);
  }
  else { 
    reference = (appl::TH1D*)m_ref->Clone("reference");
    reference->SetDirectory(0);
  }

  /// grrr, what is this? This whole normalisation issue is a real pain - 
  /// we need to address this - but how to do it in a backwards compatible way?
  /// will need to check the grid version and do things differently probably

  //  std::cout << "normalised() " << getNormalised() << "\tread " << m_read << std::endl; 
  
  //  if ( !getNormalised() || m_read )  if ( run() ) reference->Scale(1/double(run()));
  if ( run() ) { 
    reference->Scale(1/double(run()));
    if ( reference_internal ) reference_internal->Scale(1/double(run()));
  }

  // if ( run() ) reference->Scale(1/double(run()));

#if 0

  double hmin = reference->GetBinContent(1); 
  double hmax = reference->GetBinContent(1); 
  for ( int i=1 ; i<=reference->GetNbinsX() ; i++ ) { 
    std::cout << "reference " << i << " " << reference->GetBinContent(i) << std::endl;
    if ( hmin<reference->GetBinContent(i) ) hmin = reference->GetBinContent(i); 
    if ( hmax>reference->GetBinContent(i) ) hmax = reference->GetBinContent(i); 
  }

  reference->SetMinimum(hmin);
  reference->SetMaximum(hmax);

#endif

  reference->Write();

  delete reference;

  if ( reference_internal ) { 
    reference_internal->Write();
    delete reference_internal;
  }

  //  std::cout << "corrections" << std::endl;

  /// correction histograms

  if ( m_corrections.size()>0 ) {

    /// Fixme: should add the labels to the actual corrections rather than save separately
    /// write labels
    TFileVector* corrections = new TFileVector("Corrections");
    for ( unsigned i=0 ; i<m_corrections.size() ; i++ )  corrections->add( m_corrections[i].y() );    
    corrections->Write("Corrections");

    /// write actual corrections
    TFileString correctionLabels("CorrectionLabels");
    for ( unsigned i=0 ; i<m_correctionLabels.size() ; i++ )  correctionLabels.add( m_correctionLabels[i] );
    correctionLabels.Write("CorrectionLabels");

  }

  /// now write out vector of bins to be combined if this has been set
  if ( m_combine.size()>0 ) { 
    TVectorT<double>* _combine = new TVectorT<double>(m_combine.size()); 
    for ( unsigned i=m_combine.size() ; i-- ; ) (*_combine)(i) = m_combine[i]+0.5; /// NB: add 0.5 to prevent root double -> int rounding errors
    _combine->Write( "CombineBins" );
  }


  /// now write out the user data

  if ( m_userdata.size() ) { 
    TVectorT<double>* userdata=new TVectorT<double>(m_userdata.size()); // add a few extra just in case 
    for ( unsigned i=0 ; i<m_userdata.size() ; i++ ) (*userdata)(i) = m_userdata[i];
    userdata->Write("UserData");
  }

  d.pop();
  rootfile.Close();

}
#endif






// dump to file
void appl::grid::Write_appl(const std::string& filename, 
			    const std::string& dirname, 
			    const std::string& pdfname) 
{ 

  std::cout << "appl::grid::Write() " << filename << "\tdirname " << dirname << "\tpdfname " << pdfname << std::endl; 

  if ( exists( filename ) ) { 
    std::string filename_save = filename + "-save";
    if ( std::rename( filename.c_str(), filename_save.c_str() ) ) std::cerr << "could not rename grid file " << filename << std::endl;
  } 

  if ( pdfname!="" ) shrink( pdfname, m_genpdf[0]->getckmcharge() );

  //  std::cout << "grid::Write() writing to file " << filename << std::endl;

  appl::file gridfile( filename.c_str(), "w" );

  // write the name of the transform pair and the
  // generalised pdf

  stream_vector<std::string> tags( "Tags" );
  
  tags.add(m_transform);
  tags.add(m_genpdfname);
  tags.add(m_version);
  tags.add(m_documentation);
  if ( m_genwithpdf!="" ) tags.add( m_genwithpdf + ":" + std::to_string(m_genwithipdf) );

  gridfile.Write( tags );


  gridfile.Write( stream_vector<std::string>( "QTags", std::vector<std::string>( 1, m_qtransform ) ) );

  //  std::cout << "genwith pdf " << m_genwithpdf << " : " << m_genwithipdf << std::endl; 

  stream_vector<double> setup( "State" );

  // state information
  setup.add( m_run );
  setup.add( m_optimised  ? 1 : 0 );
  setup.add( m_symmetrise ? 1 : 0 );
  setup.add( m_leading_order );
  setup.add( m_order );
  setup.add( m_cmsScale );
  setup.add( m_normalised ? 1 : 0 );
  setup.add( m_applyCorrections ? 1 : 0 );

  if ( m_genpdf[0]->getckmsum().size()==0 ) setup.add(0);
  else                                      setup.add(1);

  setup.add((int)m_type);

  setup.add(m_userdata.size());

  /// by default we do not want to use duplicates, 
  /// so we want them removed
  if ( m_use_duplicates ) setup.add(1);

  gridfile.Write( setup );

  if ( setup[8] == 1 ) { 
    
    //    std::cout << "ckm: size: " << m_genpdf[0]->getckm().size() << std::endl;

    const std::vector<std::vector<double> >& ckm = m_genpdf[0]->getckm();

    /// no longer write out squared ckm matrix - just use the 3x3 - unless it is an OLD, OLD grid

    std::vector<double> ckmflat(9);

    bool useckm = false;

    if ( ckm.size() ) {  
      for ( int ic=0 ; ic<3 ; ic++ ) { 
	for ( int id=0 ; id<3 ; id++ ) { ckmflat[ic*3+id] = ckm[ic][id]; if ( ckm[ic][id]!=0 ) useckm=true; }
      }
    }
    
    if ( useckm ) { 
      gridfile.Write(  stream_vector<double>( "CKM", ckmflat ) );
    }
    else { 
      
      const std::vector<std::vector<double> >& _ckm2 = m_genpdf[0]->getckm2();
   
      if ( _ckm2.size()>0 ) { 
	/// no longer write out squared ckm matrix - just use the 3x3 - unless it is an OLD, OLD grid

	int nrows = _ckm2.size();
	
	std::vector<double> ckm2flat(nrows*nrows);
	
	for ( int ic=0 ; ic<nrows ; ic++ ) { 
	  for ( int id=0 ; id<nrows ; id++ ) ckm2flat[ic*nrows+id] = _ckm2[ic][id];
	}

	gridfile.Write(  stream_vector<double>( "CKM2", ckm2flat ) );
      }
    }

  }

  /// encode the pdf combination if appropriate
  
  if ( contains( m_genpdfname, ".config" ) ) { 
    
    // find out if we have one combination per order or one overall
    
    std::vector<std::string> namevec = parse( m_genpdfname, ":" );
    
    /// now write out all the ones we need
    
    std::string label = "Combinations";
    
    for ( unsigned i=0 ; i<namevec.size() && i<unsigned(m_order) ; i++ ) {  

      //      dynamic_cast<lumi_pdf*>(m_genpdf[i])->restoreDuplicates();

      lumi_pdf* lpdf = dynamic_cast<lumi_pdf*>(m_genpdf[i]);

      std::cout << "lumi_pdf:   " << lpdf->name() << "\t processes " << lpdf->Nproc() << "\n";

      //      std::cout << "lumi pdf: " << *lpdf << std::endl;

      lumi_pdf lpdf_tmp(*lpdf);

      lpdf_tmp.restoreDuplicates();

      //      std::cout << "lumi pdf: " << *lpdf     << std::endl;
      //      std::cout << "lumi pdf: " <<  lpdf_tmp << std::endl;

      //      std::vector<int>   combinations = dynamic_cast<lumi_pdf*>(m_genpdf[i])->serialise();
      std::vector<int>     combinations = lpdf_tmp.serialise();
      std::vector<double> _combinations(combinations.size());
      for ( size_t ic=0 ; ic<_combinations.size() ; ic++ ) { 
	if ( combinations[ic]<0 ) _combinations[ic] = double(combinations[ic]-0.5);
	else                      _combinations[ic] = double(combinations[ic]+0.5);
      }

      gridfile.Write( stream_vector<double>( label, _combinations ) );

      //      std::cout << "writing " << m_genpdf[i]->name() << std::endl;
      
      label += "N";  /// add an N for each order, N-LO, NN-LO etc !!!! ARGH!!! Need to be more general !!!
    }
  } 
   
  //  std::cout << "reference" << std::endl;

  /// grrr only have to use pointers for the root backewards compatability 
  /// how I hate root
  appl::TH1D* reference          = 0;
  appl::TH1D* reference_internal = 0;

  //  std::cout << "m_ref_combined: " << m_ref_combined << std::endl;
  //  std::cout << "m_ref:          " << m_ref << std::endl;

  if ( m_ref_combined != m_ref )  { 
    reference = (appl::TH1D*)m_ref_combined->Clone("reference");
    reference_internal = (appl::TH1D*)m_ref->Clone("reference_internal");
  }
  else { 
    reference = (appl::TH1D*)m_ref->Clone("reference");
  }

  /// grrr, what is this? This whole normalisation issue is a real pain - 
  /// we need to address this - but how to do it in a backwards compatible way?
  /// will need to check the grid version and do things differently probably

  //  std::cout << "normalised() " << getNormalised() << "\tread " << m_read << std::endl; 
  
  //  if ( !getNormalised() || m_read )  if ( run() ) reference->Scale(1/double(run()));
  if ( run() ) { 
    reference->Scale(1/double(run()));
    if ( reference_internal ) reference_internal->Scale(1/double(run()));
  }

  // if ( run() ) reference->Scale(1/double(run()));

  gridfile.Write( *reference );
  delete reference;

  if ( reference_internal ) { 
    gridfile.Write( *reference_internal );
    delete reference_internal;
  }

  
  //  std::cout << "grids Nobs = " << Nobs_internal() << std::endl;
  
  untrim();
  int untrim_size = size();
  trim();
  int trim_size = size();

  std::cout <<"grid::Write() "
	    << "size(untrimmed)=" << untrim_size/1024 << " kB"
	    << "\tsize(trimmed)=" <<   trim_size/1024 << " kB"
	    << " (" << 0.1*int(trim_size*1000./untrim_size) << "%)" << std::endl;
  

  // internal grids
  for( int iorder=0 ; iorder<m_order ; iorder++ ) {
    for( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) {
      char name[128];  sprintf(name, "weight[alpha-%d][%03d]", iorder, iobs);
      // std::cout << "writing grid " << name << std::endl;
      m_grids[iorder][iobs]->write( gridfile, name);
    }
  }



  //  std::cout << "corrections" << std::endl;

  /// correction histograms

  if ( m_corrections.size()>0 ) {

    std::vector<std::string> correctionLabels(m_corrections.size());
    for ( size_t i=0 ; i<m_corrections.size() ; i++ ) correctionLabels[i] = m_correctionLabels[i];
    for ( size_t i=0 ; i<m_corrections.size() ; i++ ) { 
      std::cout << "labels : " << m_correctionLabels[i] << "\t" << m_correctionLabels[i] << std::endl;
    }
    gridfile.Write( "CorrectionLabels", correctionLabels );

    std::vector<std::vector<double> > corrections(m_corrections.size());
    for ( size_t i=0 ; i<m_corrections.size() ; i++ ) corrections[i] = m_corrections[i].y();
    gridfile.Write( "Corrections", corrections );
 
  }
   

  /// now write out vector of bins to be combined if this has been set

  if ( m_combine.size()>0 ) { 

    std::vector<double> combine(m_combine.size()); 
    for ( unsigned i=m_combine.size() ; i-- ; ) combine[i] = m_combine[i]; /// NB: add 0.5 to prevent root double -> int rounding errors
    gridfile.Write( "CombineBins", combine );

  }


  /// now write out the user data

  gridfile.Write( "UserData", m_userdata );


  //  d.pop();
  gridfile.Close();

}




void appl::grid::disable_threads(bool b) { appl::igrid::disable_threads(b); }


bool duff() { 
  appl::grid::disable_threads(true);
  return true;
}


// takes pdf as the pdf lib wrapper for the pdf set for the convolution.
// type specifies which sort of partons should be included:

std::vector<double> appl::grid::vconvolute(void (*pdf)(const double& , const double&, double* ), 
					   double (*alphas)(const double& ), 
					   int     _nloops, 
					   double  rscale_factor,
					   double  fscale_factor,
					   double Escale )
{ 
  return vconvolute( pdf, 0, alphas, _nloops, rscale_factor, fscale_factor, Escale, Escale );
}



std::vector<double> appl::grid::vconvolute(void (*pdf1)(const double& , const double&, double* ), 
					   void (*pdf2)(const double& , const double&, double* ), 
					   double (*alphas)(const double& ), 
					   int     _nloops, 
					   double  rscale_factor,
					   double  fscale_factor,
					   double  Escale1,
					   double  Escale2 )
{ 
 
  NodeCache cache1( pdf1 );
  NodeCache cache2;

 
  cache1.reset();


  NodeCache* _pdf1 = &cache1;
  NodeCache* _pdf2 = 0;
  
  if ( !isDIS() && pdf2!=0 && pdf1!=pdf2 ) { 
    cache2 = NodeCache( pdf2 );
    cache2.reset();  
    _pdf2    = &cache2;
  }


  //  struct timeval _ctimer = appl_timer_start();
  
  double EscaleSQ = 1;
 
  if ( Escale1!=1 || Escale2!=1 ) EscaleSQ = Escale1*Escale2;
  
  std::vector<double> hvec(Nobs_internal(), 0);
  //  hvec.clear(); /// explicit clear just in case

  double invNruns = 1;
  if ( (!m_normalised) && run() ) invNruns /= double(run());

  //  appl::TH1D* h = new appl::TH1D(*m_ref);
  //  h->SetName("xsec");

  //  std::cout << "hvec: " << hvec.size() << " : " << hvec << std::endl;


#ifdef HAVE_HOPPET
  // check if we need to use the splitting function, and if so see if we 
  // need to initialise it again, and do so if required


  if ( fscale_factor!=1 || m_dynamicScale ) {

    /// should initialise *especially* if requesting multiple PDFs  
    //    if ( pdf2==0 || pdf1==pdf2 ) { 
   
      /// fixed and shared between 
      /// all instances of the grid

      static double Qmax = 26000;  /// should be generally ok
      static double ymax = 12; 

      //      char* Qenv =  getenv("QMAX"); 
      
      //      if ( Qenv != 0 ) Qmax = std::atof( Qenv );

      //      double _xmin = xmin();
      //      double _ymax = std::log(1/_xmin);

      ///     grr - always use 12, even though a better max was computed ??
      //      double _ymax = ymax;

      double _ymax = m_ymax;

      bool restart_hoppet = false;

      /// Qmax limit needs to be recalulated
      /// used to use (m_cmsScale>Qmax) but some grid 
      /// generators use ludicrously high scales 
      if ( fscale_factor*m_Qmax>Qmax ) { 
	if ( hoppet ) restart_hoppet = true;
	Qmax = m_Qmax*fscale_factor;
      }

      /// ymax limit needs to be recalculated
      if ( _ymax>ymax ) { 
	if ( hoppet ) restart_hoppet = true;

	double dymax = (_ymax - ymax);

	/// NB: this 0.1 is just dy from hoppet init - would be better to 
	//      caluclate the values and set this in hoppet init
	double iy = dymax/0.1; 

	//	std::cout << "appl::grid changing hoppet yrange " << ymax << "\t" << iy << "\t" << (0.1+int(iy)*0.1) << std::endl;

	/// make sure that we are on a dy integer multiple
	if ( int(iy)!=iy )  ymax += 0.1+int(iy)*0.1;
	else                ymax +=     int(iy)*0.1;
      }

      if ( hoppet && restart_hoppet ) { 
	std::cout << "appl::grid deleting old hoppet" << std::endl;
	delete hoppet;
	hoppet = 0;
      }  

      if ( hoppet == 0 ) {  
	std::cout << "appl::grid initialising hoppet" << std::endl;
	//	double Qmax = 15000;
	//	double ymax
	hoppet = new appl::hoppet_init( Qmax, ymax );	
      } 

      bool newpdf = hoppet->compareCache( pdf1 );
      
      if ( newpdf ) hoppet->fillCache( pdf1 );

      //    }

  }
#endif                                   


  std::string label;

  if ( _nloops>=m_order ) { 
    std::cerr << "too many loops for grid: " << _nloops << " > " << nloops() << " " << std::endl;   
    return hvec;
  } 
  
  /// NB: in the following code, the return values is stored in a class variable that 
  ///     must be accessed when each convolution has finished since the convolution 
  ///     itself might not be happening in this thread


  if ( m_type==STANDARD ) { 

    static bool first = true;

    if ( first && m_dynamicScale ) std::cout << "** grid::vconvolute() emulating dynamic scale **" << std::endl;

    //    std::cout << "standard convolution" << std::endl;

    if      ( _nloops==0  ) label = "lo        ";
    else if ( _nloops==1  ) label = "nlo       ";
    else if ( _nloops==2  ) label = "nnlo      ";
    else if ( _nloops==-1 ) label = "nlo only  ";
    else if ( _nloops==-2 ) label = "nnlo only ";
    

    //    std::exit(0);
    
    for ( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) {  
      // for ( int iobs=0 ; iobs<1 ; iobs++ ) {  

      /// here we see whether we need to emulate a dynamic scale by simply 
      /// changing the renormalisation and factorisation scale terms to give 
      /// what a dynamic scale would be in this bin
 
      //      if ( m_bin!=-1 && iobs!=m_bin ) continue;


      ////  std::cout << "bin: " << iobs << std::endl;

      double dynamic_factor = 1;


      if ( m_dynamicScale ) {
	double var = m_ref->GetBinCenter(iobs+1);
	dynamic_factor = var/m_dynamicScale;
	//	if ( first ) std::cout << "grid::vconvolute() bin " << iobs << "\tscale " << var << "\tdynamicScale " << m_dynamicScale << "\t scale factor " << dynamic_factor << std::endl;
      } 

      /// now do the convolution proper

      //      std::cout << iobs << "\tnloops " << _nloops << std::endl;


      if ( _nloops==0 ) {
	/// leading order cross section
	if ( subproc()==-1 ) {  
	  m_grids[0][iobs]->convolute( _pdf1, _pdf2, m_genpdf[0], alphas, m_leading_order, 0, dynamic_factor*rscale_factor, dynamic_factor*fscale_factor, Escale1, Escale2 );
	}
	else {

 
	  /// fixme: for the subproceses, this is technically incorrect - the "LO" contribution 
	  ///        includes the scale dependent "NLO" terms that are proportional to the LO 
	  ///        coefficient functions - it could use the strict "LO" part without the scale 
	  ///        dependent parts using order "0" rather than order "1" but see the comment 
	  ///        for the "NLO only" contribution. The reason for this is that the subprocesses
	  ///        can be different for LO and NLO, so this NLO part with "LO coefficients", 
	  ///        can have different subprocesses from the actual NLO part, so the correct 
	  ///        LO/NLO separation is only guaranteed for the full convolution, and not by 
	  ///        subprocess
	  m_grids[0][iobs]->convolute( _pdf1, _pdf2, m_genpdf[0], alphas, m_leading_order, 1, dynamic_factor*rscale_factor, dynamic_factor*fscale_factor, Escale1, Escale2 );
	}

      }
      else if ( _nloops==1 ) {


	// next to leading order cross section
	// std::cout << "convolute() nloop=1" << std::endl;
	// leading order contribution and scale dependent born dependent terms
	m_grids[0][iobs]->convolute( _pdf1, _pdf2, m_genpdf[0], alphas, m_leading_order, 1, dynamic_factor*rscale_factor, dynamic_factor*fscale_factor, Escale1, Escale2 );
	// std::cout << "dsigma_lo=" << dsigma_lo << std::endl;
	// next to leading order contribution
	//      double dsigma_nlo = m_grids[1][iobs]->convolute(pdf, m_genpdf, alphas, m_leading_order+1, 0);
	// GPS: the NLO piece must use the same rscale_factor and fscale_factor as
	//      the LO piece -- that's the convention that defines how NLO calculations
	//      are done.
	m_grids[1][iobs]->convolute( _pdf1, _pdf2, m_genpdf[1], alphas, m_leading_order+1, 0, dynamic_factor*rscale_factor, dynamic_factor*fscale_factor, Escale1, Escale2 );
	// std::cout << "dsigma_nlo=" << dsigma_nlo << std::endl;


      }
      else if ( _nloops==-1 ) {
	label = "nlo only";

	// nlo contribution only 
	if ( subproc()==-1 ) { 
	  m_grids[0][iobs]->convolute( _pdf1, _pdf2, m_genpdf[0], alphas, m_leading_order,   -1, dynamic_factor*rscale_factor, dynamic_factor*fscale_factor, Escale1, Escale2 );
	  m_grids[1][iobs]->convolute( _pdf1, _pdf2, m_genpdf[1], alphas, m_leading_order+1,  0, dynamic_factor*rscale_factor, dynamic_factor*fscale_factor, Escale1, Escale2 );
	}
	else { 
	  /// fixme: this is technically incorrect - the "LO" component contains the 
	  ///        scale dependent NLO contribution dependent on the LO coefficient
	  ///        functions. This part is difficult to include for individual 
	  ///        subprocesses, since different subprocesses can be present 
	  ///        at LO and NLO, so speifying subprocess X at NLO does not 
	  ///        neccessarily correspond to subprocess X at LO, so adding the 
	  ///        subprocesses - so these terms are only strict LO And NLO when 
	  ///        *not* specifying subprocess
	  m_grids[1][iobs]->convolute( _pdf1, _pdf2, m_genpdf[1], alphas, m_leading_order+1,  0, dynamic_factor*rscale_factor, dynamic_factor*fscale_factor, Escale1, Escale2 );
	}

      } 
      else if ( _nloops==2 ) {
	label = "nnlo    ";
	// next to next to leading order contribution 
	// std::cout << " order: " << 0 << std::endl;
	m_grids[0][iobs]->convolute( _pdf1, _pdf2, m_genpdf[0], alphas, m_leading_order, 2,   dynamic_factor*rscale_factor, dynamic_factor*fscale_factor, Escale1, Escale2  );
	// next to leading order contribution      
	// std::cout << " order: " << 1 << std::endl;
	m_grids[1][iobs]->convolute( _pdf1, _pdf2, m_genpdf[1], alphas, m_leading_order+1, 1, dynamic_factor*rscale_factor, dynamic_factor*fscale_factor, Escale1, Escale2  );
	// next to next to leading order contribution
	// std::cout << " order: " << 2 << std::endl;
	m_grids[2][iobs]->convolute( _pdf1, _pdf2, m_genpdf[2], alphas, m_leading_order+2, 0, dynamic_factor*rscale_factor, dynamic_factor*fscale_factor, Escale1, Escale2  );
      }
      else if ( _nloops==-2 ) {
	// next to next to leading order contribution only
	m_grids[2][iobs]->convolute( _pdf1, _pdf2, m_genpdf[2], alphas, m_leading_order+2, 0, dynamic_factor*rscale_factor, dynamic_factor*fscale_factor, Escale1, Escale2  );
      }
      else { 
	std::stringstream s_;
	s_ << "invalid value for nloops " << _nloops; 
	throw grid::exception( s_.str() );
      }

      //      double deltaobs = m_ref->GetBinLowEdge(iobs+2)-m_ref->GetBinLowEdge(iobs+1);      
      //      hvec.push_back( invNruns*Escale2*dsigma/deltaobs );

    }


    /// wait on convolution results and combine the values from the different igrids
   
    //    std::cout << "vconvolute:: nloops " << _nloops << std::endl; 

    if ( _nloops==0 ) { 
      /// LO only 
      for ( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) {
	double dsigma  = 0;
	if ( m_grids[0][iobs]->ready() ) dsigma  = m_grids[0][iobs]->xsec(); 
	double deltaobs = m_ref->GetBinLowEdge(iobs+2)-m_ref->GetBinLowEdge(iobs+1);      
	hvec[iobs] = invNruns*EscaleSQ*dsigma/deltaobs;
	//      printf("iobs %d   xsec raw: %lf   %lf \n",  iobs, dsigma, invNruns*EscaleSQ*dsigma/deltaobs );     
      }    
    }
    else if ( _nloops==1 || ( _nloops==-1 && subproc()==-1 ) ) { 
      /// NLO only or overall NLO only part
      for ( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) {
	double dsigLO  = 0;
	double dsigNLO = 0;
	if ( m_grids[0][iobs]->ready() ) { 
	  dsigLO  = m_grids[0][iobs]->xsec(); 
	  dsigNLO = m_grids[0][iobs]->xsecNLO(); 
	} 
	if ( m_grids[1][iobs]->ready() ) dsigNLO += m_grids[1][iobs]->xsec(); 
	double dsigma = dsigLO + dsigNLO;
	double deltaobs = m_ref->GetBinLowEdge(iobs+2)-m_ref->GetBinLowEdge(iobs+1);      
	hvec[iobs] = invNruns*EscaleSQ*dsigma/deltaobs;
	//      printf("iobs %d   xsec raw: %lf   %lf \n",  iobs, dsigma, invNruns*EscaleSQ*dsigma/deltaobs );     
      }    
    }
    else if ( _nloops==-1 && subproc()!=-1 ) { 
      /// NLO only suboprocess only
      for ( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) {
	double dsigma = 0;
	if ( m_grids[1][iobs]->ready() ) dsigma = m_grids[1][iobs]->xsec(); 
	double deltaobs = m_ref->GetBinLowEdge(iobs+2)-m_ref->GetBinLowEdge(iobs+1);      
	hvec[iobs] = invNruns*EscaleSQ*dsigma/deltaobs;
      }
    }
    else if ( _nloops==2 ) { 
      /// NNLO only suboprocess only
      for ( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) {
	// for ( int iobs=0 ; iobs<1 ; iobs++ ) {

	double dsigLO  = 0;
	if ( m_grids[0][iobs]->ready() ) dsigLO    = m_grids[0][iobs]->xsec(); 

	double dsigNLO = 0;
	if ( m_grids[0][iobs]->ready() ) dsigNLO   = m_grids[0][iobs]->xsecNLO(); 
	if ( m_grids[1][iobs]->ready() ) dsigNLO  += m_grids[1][iobs]->xsec(); 

	double dsigNNLO = 0;
	if ( m_grids[0][iobs]->ready() ) dsigNNLO  = m_grids[0][iobs]->xsecNNLO(); 
	if ( m_grids[1][iobs]->ready() ) dsigNNLO += m_grids[1][iobs]->xsecNLO(); 
	if ( m_grids[2][iobs]->ready() ) dsigNNLO += m_grids[2][iobs]->xsec(); 

#if 0
	std::cout << "iobs: " << iobs << "  dsig: " << dsigLO << " " << dsigNLO << " " << dsigNNLO << std::endl;
	std::cout << "          dsig: " << m_grids[0][iobs]->xsec() << " " << m_grids[1][iobs]->xsec() << " " << m_grids[2][iobs]->xsec() << std::endl;  
	std::cout << "          dsig: " << m_grids[0][iobs]->xsec() << " " << m_grids[0][iobs]->xsecNLO() << " " << m_grids[0][iobs]->xsecNNLO() << " (LO)" << std::endl;  
#endif
	double dsigma = dsigLO + dsigNLO  + dsigNNLO;
	// if ( mnobinwidth ) deltaobs = 1;
	double deltaobs = m_ref->GetBinLowEdge(iobs+2)-m_ref->GetBinLowEdge(iobs+1);      
	hvec[iobs] = invNruns*EscaleSQ*dsigma/deltaobs;
	// hvec[iobs] = invNruns*EscaleSQ*dsigma; /// don't divide by the bin width to compare with stupid fastnlo

	//      printf("iobs %d   xsec raw: %lf   %lf \n",  iobs, dsigma, invNruns*EscaleSQ*dsigma/deltaobs );     
      }    

      ///      std::cout << "hvec: " << hvec.size() << " : " << hvec << "  (2)" << std::endl;

    }
    else if ( _nloops==-2 ) {
      /// NNLO only
      for ( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) {
	double dsigma = 0;
	if ( m_grids[2][iobs]->ready() ) dsigma = m_grids[2][iobs]->xsec(); 
	double deltaobs = m_ref->GetBinLowEdge(iobs+2)-m_ref->GetBinLowEdge(iobs+1);      
	//  hvec.push_back( invNruns*EscaleSQ*dsigma/deltaobs );
	hvec[iobs] = invNruns*EscaleSQ*dsigma/deltaobs;
      }
    }

    first = false;
    

  }
  else if ( m_type==AMCATNLO ) {  

    //    std::cout << "amc@NLO convolution " << _nloops << std::endl;

    if      ( _nloops==0  )  label = "lo      ";
    else if ( _nloops==1  )  label = "nlo     ";
    else if ( _nloops==-1 )  label = "nlo only";
    else if ( _nloops==-2 )  label = "nlo_w0  ";
    else if ( _nloops==-3 )  label = "nlo_wR  ";
    else if ( _nloops==-4 )  label = "nlo_wF  ";
    
    for ( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) {  

      if ( _nloops==0 ) {
	/// this is the amcatnlo LO calculation (without FKS shower)
	/// work out how to call from the igrid - maybe just implement additional 
	m_grids[3][iobs]->amc_convolute( _pdf1, _pdf2, m_genpdf[3], alphas, m_leading_order,   0, rscale_factor, fscale_factor,  Escale1, Escale2 );
      }
      else if ( _nloops==1 || _nloops==-1 ) {
	  /// this is the amcatnlo NLO calculation (without FKS shower)
	  /// Next-to-leading order contribution

	  // Scale independent contribution
	m_grids[0][iobs]->amc_convolute( _pdf1, _pdf2, m_genpdf[0], alphas, m_leading_order+1, 0,  rscale_factor, fscale_factor,  Escale1, Escale2 );

	  // Renormalization scale dependent contribution
	  if ( rscale_factor!=1 ) { 
	    m_grids[1][iobs]->amc_convolute( _pdf1, _pdf2, m_genpdf[1], alphas, m_leading_order+1, 0,  rscale_factor, fscale_factor,  Escale1, Escale2 );
	  }

	  // Factorization scale dependent contribution
	  if ( fscale_factor!=1 ) { 
	    m_grids[2][iobs]->amc_convolute( _pdf1, _pdf2, m_genpdf[2], alphas, m_leading_order+1, 0,  rscale_factor, fscale_factor,  Escale1, Escale2 );
	  }
      
	  /// Add the LO contribution if we want full NLO 
	  /// rather than specific NLO contribution only  
	  if ( _nloops==1 ) { 
	    /// this is the amcatnlo NLO calculation (without FKS shower)
	    /// work out how to call from the igrid - maybe just implement additional 
	    /// convolution routines and call them here
	    m_grids[3][iobs]->amc_convolute( _pdf1, _pdf2, m_genpdf[3], alphas, m_leading_order,   0,  rscale_factor, fscale_factor,  Escale1, Escale2 );	
	  }
      }
      else if ( _nloops==-2 ) { 
        /// Only the convolution from the W0 grid
	m_grids[0][iobs]->amc_convolute( _pdf1, _pdf2, m_genpdf[0], alphas, m_leading_order+1, 0,  rscale_factor, fscale_factor,  Escale1, Escale2 );
      }
      else if ( _nloops==-3 ) {
	/// Only the convolution from the WR grid
	m_grids[1][iobs]->amc_convolute( _pdf1, _pdf2, m_genpdf[1], alphas, m_leading_order+1, 0, rscale_factor, fscale_factor,  Escale1, Escale2 );
      }
      else if ( _nloops==-4 ) { 
        /// Only the convolution from the WF grid
	m_grids[2][iobs]->amc_convolute( _pdf1, _pdf2, m_genpdf[2], alphas, m_leading_order+1, 0,  rscale_factor, fscale_factor,  Escale1, Escale2 );
      }
      else { 
	std::stringstream s_;
	s_ << "invalid value for nloops " << _nloops; 
	throw grid::exception( s_.str() );
      }

      //  double deltaobs = m_ref->GetBinLowEdge(iobs+2)-m_ref->GetBinLowEdge(iobs+1);      
      //  hvec.push_back( invNruns*Escale2*dsigma/deltaobs );
    }

    /// wait on convolutions

    if ( _nloops==0 ) { 
      /// LO only 
      for ( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) {
	double dsigma  = 0;
	if ( m_grids[3][iobs]->ready() ) dsigma  = m_grids[3][iobs]->xsec(); 
	double deltaobs = m_ref->GetBinLowEdge(iobs+2)-m_ref->GetBinLowEdge(iobs+1);      
	// hvec.push_back( invNruns*EscaleSQ*dsigma/deltaobs );
	hvec[iobs] = invNruns*EscaleSQ*dsigma/deltaobs;
	//      printf("iobs %d   xsec raw: %lf   %lf \n",  iobs, dsigma, invNruns*EscaleSQ*dsigma/deltaobs );     
      }    
    }
    else if ( _nloops==1 || _nloops==-1 ) { 
      /// NLO only or overall NLO only part
	
      double logF2 = 0;
      double logR2 = 0;
   
      if ( rscale_factor!=1 ) logR2 = std::log(rscale_factor*rscale_factor);
      if ( fscale_factor!=1 ) logF2 = std::log(fscale_factor*fscale_factor);
     
      for ( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) {

	double dsig0 = 0;
	double dsigF = 0;
	double dsigR = 0;
	double dsigB = 0;

	if ( m_grids[0][iobs]->ready() ) dsig0  = m_grids[0][iobs]->xsec(); 
	double dsigma = dsig0;

	if ( _nloops==1 ) { 
	  if ( m_grids[3][iobs]->ready() ) dsigB  = m_grids[3][iobs]->xsec(); 
	  dsigma += dsigB;
	}
	
	if ( rscale_factor!=1 ) { 
	  if ( m_grids[1][iobs]->ready() ) dsigR  = m_grids[1][iobs]->xsec(); 
	  dsigma += dsigR*logR2;
	}

	if ( fscale_factor!=1 ) { 
	  if ( m_grids[2][iobs]->ready() ) dsigF  = m_grids[2][iobs]->xsec(); 
	  dsigma += dsigF*logF2;
	}

	double deltaobs = m_ref->GetBinLowEdge(iobs+2)-m_ref->GetBinLowEdge(iobs+1);      
	//	hvec.push_back( invNruns*EscaleSQ*dsigma/deltaobs );
	hvec[iobs] = invNruns*EscaleSQ*dsigma/deltaobs;
	//      printf("iobs %d   xsec raw: %lf   %lf \n",  iobs, dsigma, invNruns*EscaleSQ*dsigma/deltaobs );     
      }         
    }
    else if ( _nloops==-2) { 
      /// NLO only suboprocess only
      for ( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) {
	double dsigma = 0;
	if ( m_grids[0][iobs]->ready() ) dsigma = m_grids[0][iobs]->xsec(); 
	double deltaobs = m_ref->GetBinLowEdge(iobs+2)-m_ref->GetBinLowEdge(iobs+1);      
	//	hvec.push_back( invNruns*EscaleSQ*dsigma/deltaobs );
	hvec[iobs] = invNruns*EscaleSQ*dsigma/deltaobs; 
      }
    }
    else if ( _nloops==-3) { 
      /// NLO only suboprocess only

      double logR2 = 0;
      if ( rscale_factor!=1 ) logR2 = std::log(rscale_factor*rscale_factor);

      for ( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) {
	double dsigma = 0;
	if ( m_grids[1][iobs]->ready() ) dsigma = m_grids[1][iobs]->xsec(); 
	dsigma  *= logR2;
	double deltaobs = m_ref->GetBinLowEdge(iobs+2)-m_ref->GetBinLowEdge(iobs+1);      
	// hvec.push_back( invNruns*EscaleSQ*dsigma/deltaobs );
	hvec[iobs] = invNruns*EscaleSQ*dsigma/deltaobs;
      }
    }
    else if ( _nloops==-4) { 
      /// NLO only suboprocess only

      double logF2 = 0;
      if ( fscale_factor!=1 ) logF2 = std::log(fscale_factor*fscale_factor);

      for ( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) {
	double dsigma = 0;
	if ( m_grids[2][iobs]->ready() ) dsigma = m_grids[2][iobs]->xsec(); 
	dsigma  *= logF2;
	double deltaobs = m_ref->GetBinLowEdge(iobs+2)-m_ref->GetBinLowEdge(iobs+1);      
	// hvec.push_back( invNruns*EscaleSQ*dsigma/deltaobs );
	hvec[iobs] = invNruns*EscaleSQ*dsigma/deltaobs;
      }
    }

  }   /// end amc@nlo
  else if ( m_type == SHERPA ) { 
    
    //    std::cout << "sherpa convolution" << std::endl;

    if      ( _nloops==0  ) label = "lo      ";
    else if ( _nloops==1  ) label = "nlo     ";
    else if ( _nloops==-1 ) label = "nlo only";

    for ( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) {  
    
      if ( _nloops==0 ) {
	label = "lo      ";
	// leading order cross section
	m_grids[0][iobs]->convolute( _pdf1, _pdf2, m_genpdf[0], alphas, m_leading_order, 0, 1, 1, Escale1, Escale2);
      }
      else if ( _nloops==1 ) { 
	label = "nlo     ";
	// next to leading order cross section
	// leading order contribution and scale dependent born dependent terms
	// will eventually add the other nlo terms ...
	m_grids[0][iobs]->convolute( _pdf1, _pdf2, m_genpdf[0], alphas, m_leading_order,   0, rscale_factor, fscale_factor, Escale1, Escale2 );
	m_grids[1][iobs]->convolute( _pdf1, _pdf2, m_genpdf[1], alphas, m_leading_order+1, 0, rscale_factor, fscale_factor, Escale1, Escale2 );
      }
      else if ( _nloops==-1 ) { 
	label = "nlo only";
	// next to leading order cross section
	// leading order contribution and scale dependent born dependent terms
	m_grids[1][iobs]->convolute( _pdf1, _pdf2, m_genpdf[1], alphas, m_leading_order+1, 0, rscale_factor, fscale_factor, Escale1, Escale2 );
      }

    }

    /// wait on convolutions

    if    ( _nloops==0 ) { 
      for ( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) {
	double dsigma = 0;
	if ( m_grids[0][iobs]->ready() ) dsigma = m_grids[0][iobs]->xsec(); 
	double deltaobs = m_ref->GetBinLowEdge(iobs+2)-m_ref->GetBinLowEdge(iobs+1);      
	// hvec.push_back( invNruns*EscaleSQ*dsigma/deltaobs );
	hvec[iobs] = invNruns*EscaleSQ*dsigma/deltaobs; 
     }
    }
    else if ( _nloops==1 ) { 
      for ( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) {
	double dsigLO = 0;
	double dsigNLO = 0;
	if ( m_grids[0][iobs]->ready() ) dsigLO  = m_grids[0][iobs]->xsec(); 
	if ( m_grids[1][iobs]->ready() ) dsigNLO = m_grids[1][iobs]->xsec(); 
	double dsigma  = dsigLO + dsigNLO;
	double deltaobs = m_ref->GetBinLowEdge(iobs+2)-m_ref->GetBinLowEdge(iobs+1);      
	//	hvec.push_back( invNruns*EscaleSQ*dsigma/deltaobs );
	hvec[iobs] = invNruns*EscaleSQ*dsigma/deltaobs;
      }
    }
    else if ( _nloops==-1 ) { 
      for ( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) {
	double dsigma = 0;
	if ( m_grids[1][iobs]->ready() ) dsigma = m_grids[1][iobs]->xsec(); 
	double deltaobs = m_ref->GetBinLowEdge(iobs+2)-m_ref->GetBinLowEdge(iobs+1);      
	//  hvec.push_back( invNruns*EscaleSQ*dsigma/deltaobs );
	hvec[iobs] = invNruns*EscaleSQ*dsigma/deltaobs;
      }
    }


  }  /// end SHERPA


  /// now combine bins if required ...

  std::vector<bool> applied(m_corrections.size(),false);

  /// apply corrrections on the *uncombined* bins
  if ( getApplyCorrections() ) applyCorrections(hvec, applied);
  else { 
    for ( unsigned i=m_corrections.size() ; i-- ; ) if ( getApplyCorrection(i) ) applied[i] = applyCorrection(i,hvec);
  }


  /// combine bins if required
  if ( m_combine.size()!=0 ) { 
    combineBins( hvec );

    /// apply additional corrrections on the *combined* bins
    /// NB: Only apply those that have not already been applied
    if ( getApplyCorrections() ) applyCorrections(hvec, applied);
    else { 
      for ( unsigned i=m_corrections.size() ; i-- ; ) if ( getApplyCorrection(i) && !applied[i] ) applied[i] = applyCorrection(i,hvec);
    }
  }


  /// check all corrections have been applied correctly
  if ( getApplyCorrections() ) { 
    unsigned appliedcorrections = 0;
    for ( unsigned i=applied.size() ; i-- ; ) if ( applied[i] ) appliedcorrections++;
    if ( appliedcorrections!=applied.size() ) throw grid::exception( "correction vector size does not match data "  ); 
  }
  else { 
    unsigned Ncorrections = 0;
    unsigned appliedcorrections = 0;
    for ( unsigned i=m_corrections.size() ; i-- ; ) { 
      if ( getApplyCorrection(i) ) { 
	Ncorrections++;
	if ( applied[i] ) appliedcorrections++;
      }
    }
    if ( appliedcorrections!=Ncorrections ) throw grid::exception( "correction vector size does not match data "  ); 
  }

  //  double _ctime = appl_timer_stop(_ctimer);
  //  std::cout << "grid::convolute() " << label << " convolution time=" << _ctime << " ms" << std::endl;

  
  cache1.stats();
  if ( cache2.ncalls() ) cache2.stats();
  
  //  std::cout << "hvec: " << hvec.size() << " : " << hvec << "  (3)" << std::endl;


  return hvec;
}




appl::TH1D* appl::grid::aconvolute(void (*pdf)(const double& , const double&, double* ), 
			    double (*alphas)(const double& ), 
			    int     _nloops, 
			    double  rscale_factor,
			    double  fscale_factor,
			    double Escale )
{
  return aconvolute( pdf, 0, alphas, _nloops, rscale_factor, fscale_factor, Escale, Escale );
}




/// a dirty hack to tell the sub grid it should only 
/// use a single subprocess

std::vector<double> appl::grid::vconvolute_subproc(int subproc,
						   void (*pdf)(const double& , const double&, double* ), 
						   double (*alphas)(const double& ), 
						   int     _nloops, 
						   double  rscale_factor, double Escale ) 
{ 
  /// set the subprocess index - this is tested by the 
  /// igrid convolution
  m_subproc = subproc;
  std::vector<double> xsec = vconvolute( pdf, 0, alphas, _nloops, rscale_factor, rscale_factor, Escale, Escale ); 
  m_subproc = -1;

  return xsec;

}



double appl::grid::vconvolute_bin(int bin,
				  void (*pdf)(const double& , const double&, double* ), 
				  double (*alphas)(const double&) ) {
  /// set the subprocess index - this is tested by the 
  /// igrid convolution
  m_bin = bin;
  std::vector<double> xsec = vconvolute( pdf, alphas );
  m_bin = -1;
  return xsec[bin];
}





appl::TH1D* appl::grid::aconvolute(void (*pdf1)(const double& , const double&, double* ), 
			    void (*pdf2)(const double& , const double&, double* ), 
			    double (*alphas)(const double& ), 
			    int     _nloops, 
			    double  rscale_factor,
			    double  fscale_factor,
			    double Escale1,
			    double Escale2 ) 
{
  
  if ( isDIS() ) pdf2 = 0;
  
  //  int nbins = m_ref->GetNbinsX();

  appl::TH1D* h = 0;
  if ( m_combine.size() ) { 
    
    //    nbins = m_combine.size();

    std::vector<double> limits(m_combine.size()+1);
    
    int i=0;
    limits[0] = m_ref->GetBinLowEdge(i+1);
    for ( unsigned ib=0 ; ib<m_combine.size() ; ib++) { 
      i += m_combine[ib]; 
      limits[ib+1] = m_ref->GetBinLowEdge(i+1);
    }

    h = new appl::TH1D("xsec", "xsec", m_combine.size(), &limits[0] );
    h->SetDirectory(0);
  }
  else { 
    h = new appl::TH1D(*m_ref);
    h->SetName("xsec");
    h->SetDirectory(0);
  }

  std::vector<double> dvec = vconvolute( pdf1, pdf2, alphas, _nloops, rscale_factor, fscale_factor, Escale1, Escale2 );
 
  for ( unsigned i=0 ; i<dvec.size() ; i++ ) { 
      h->SetBinContent( i+1, dvec[i] );
      h->SetBinError( i+1, 0 );

      //      std::cout << "bin " << i << " " << h->GetBinContent( i+1 );
      
  }
  
  return h;
  
}





appl::TH1D* appl::grid::aconvolute_subproc(int subproc,
				    void (*pdf)(const double& , const double&, double* ), 
				    double (*alphas)(const double& ), 
				    int     _nloops, 
				    double  rscale_factor, double Escale ) {
  
    appl::TH1D* h = new appl::TH1D(*m_ref);
    h->SetName("xsec");
    
    std::vector<double> dvec = vconvolute_subproc( subproc, pdf, alphas, _nloops, rscale_factor, Escale );
    
    for ( unsigned i=0 ; i<dvec.size() ; i++ ) { 
      h->SetBinContent( i+1, dvec[i] );
      h->SetBinError( i+1, 0 );
    }
  
    return h;
  
}





void appl::grid::optimise(bool force, int extrabins ) {
  if ( !force && m_optimised ) return;
  m_optimised = true;
  m_read = false;
  std::cout << "grid::optimise() " << std::endl;
  for ( int iorder=0 ; iorder<m_order ; iorder++ ) { 
    for ( int iobs=0 ; iobs<Nobs_internal() ; iobs++ )  { 
      m_grids[iorder][iobs]->optimise(extrabins);
    }
  }
  m_ref->Reset();
}



void appl::grid::optimise(int NQ2, int Nx) {  optimise(NQ2, Nx, Nx);  }



void appl::grid::optimise(int NQ2, int Nx1, int Nx2) {
  m_optimised = true;
  m_read = false;
  for ( int iorder=0 ; iorder<m_order ; iorder++ ) { 
    for ( int iobs=0 ; iobs<Nobs_internal() ; iobs++ )  { 
      std::cout << "grid::optimise() bin " << iobs << "\t";
      m_grids[iorder][iobs]->optimise(NQ2, Nx1, Nx2);
    }
  }
  m_ref->Reset();
}




// redefine the limits by hand
void appl::grid::redefine(int iobs, int iorder,
			  int NQ2, double Q2min, double Q2max, 
			  int Nx,  double  xmin, double  xmax ) 
{ 
  
  if  ( iorder>=m_order ) { 
    std::cerr << "grid does not extend to this order" << std::endl;
    return;
  }
  
  if ( iobs<0 || iobs>=Nobs_internal() ) { 
    std::cerr << "observable bin out of range" << std::endl;
    return;
  }
  
  if ( iorder==0 ) { 
    std::cout << "grid::redefine() iobs=" << iobs 
	      << "NQ2="  << NQ2 << "\tQmin=" << std::sqrt(Q2min) << "\tQmax=" << std::sqrt(Q2max) 
	      << "\tNx=" << Nx  << "\txmin=" <<            xmin  << "\txmax=" <<            xmax << std::endl; 
  }
  
  igrid* oldgrid = m_grids[iorder][iobs];
  
  //  m_grids[iorder][iobs]->redefine(NQ2, Q2min, Q2max, Nx, xmin, xmax);

  m_grids[iorder][iobs] = new igrid(NQ2, Q2min, Q2max, oldgrid->tauorder(),
				    Nx,  xmin,  xmax,  oldgrid->yorder(), 
				    oldgrid->transform(), 
				    oldgrid->qtransform(), 
				    m_genpdf[iorder]->Nproc());

  m_grids[iorder][iobs]->setparent( this ); 

  delete oldgrid;
}
  


void appl::grid::setBinRange(int ilower, int iupper, double xScaleFactor) { 
  if ( ilower>=0 && iupper <Nobs_internal() ) {  
    double lower = getReference()->GetBinLowEdge(ilower+1);
    double upper = getReference()->GetBinLowEdge(iupper+2); 
    setRange( lower, upper, xScaleFactor );
  }
}



/// Fixme: need to fix this so that if the m_combine vector has values 
///        then the range of this vector is also modified
///        and then the combined reference recalculated:
///        at the moment, if the range is changed, the combine vector
///        needs to be recalculated and reset by hand

void appl::grid::setRange(double lower, double upper, double xScaleFactor) { 
  
  std::cout << "grid::SetRange() " << lower << " " << upper << std::endl; 

  std::vector<bool>   used;
  std::vector<double> limits;
  std::vector<double> contents;
  std::vector<double> errors;

  m_combine = std::vector<int>(0);

  /// get the occupied bins
  //  int Nbins = 0;
  double last = 0;
  for ( int i=1 ; i<=m_ref->GetNbinsX() ; i++ ) { 
    double bin = m_ref->GetBinCenter(i);
    if ( bin>lower && bin<upper ) { 
      limits.push_back( m_ref->GetBinLowEdge(i) );
      contents.push_back( m_ref->GetBinContent(i) );
      errors.push_back( m_ref->GetBinError(i) );
      //  std::cout << "keep bin " << i << " : " << m_ref->GetBinLowEdge(i) << " - " << m_ref->GetBinLowEdge(i+1) << std::endl;
      last = m_ref->GetBinLowEdge(i+1);
      used.push_back(true);
    }
    else { 
      // std::cout << "skip bin " << i << " : " << m_ref->GetBinLowEdge(i) << " - " << m_ref->GetBinLowEdge(i+1) << std::endl;
      used.push_back(false);
    }
  }

  int firstbin = 0;
  int lastbin  = 0;

  for ( unsigned i=0 ; i<used.size() ; i++ ) { 
    if ( used[i] ) { 
      firstbin = i;
      break;
    }
  }
  
  for ( unsigned i=used.size() ; i-- ; ) { 
    if ( used[i] ) { 
      lastbin = i;
      break;
    }
  }

  std::cout << "grid::SetRange() bins chosen " << firstbin << " - " << lastbin << std::endl;

  std::cout << "before setrange: " << m_ref->GetNbinsX() << std::endl;


  /// copy the range of the reference histogram
  if ( limits.size()>0 ) limits.push_back( last );
  else { 
    throw grid::exception( "new range does not include any bins"  ); 
  }

  if ( xScaleFactor!=1 ) { 
    for ( unsigned i=0 ; i<limits.size(); i++ ) limits[i] *= xScaleFactor;
  }

  /// delete combined reference if it exists

  if ( m_ref_combined != m_ref ) delete m_ref_combined;
  

  /// save bins somewhere so can overwrite them 
  appl::TH1D* h = m_ref;
  
  m_ref = new appl::TH1D( h->GetTitle(), h->GetName(), limits.size()-1, &(limits[0]) );
  
  for ( int i=0 ; i<m_ref->GetNbinsX() ; i++ ) { 
    m_ref->SetBinContent( i+1, contents[i] );
    m_ref->SetBinError( i+1, errors[i] );
  }

  std::cout << "after combine:  " << m_ref->GetNbinsX() << std::endl;

  /// copy the igrids for the observable bins in the range 

  std::vector<igrid*> grids[appl::MAXGRIDS];

  /// save old grids - this is not a deep copy
  for ( int iorder=0 ; iorder<m_order ; iorder++ ) grids[iorder] = m_grids[iorder];
  
  int _Nobs = m_ref->size();

  for ( int iorder=0 ; iorder<m_order ; iorder++ ) { 
    m_grids[iorder] = std::vector<igrid*>(_Nobs);
    int iobs = 0;
    for ( int ig=0 ; ig<h->GetNbinsX() ; ig++ ) {
      if ( used[ig] ) m_grids[iorder][iobs++] = grids[iorder][ig];
      else            delete grids[iorder][ig];                           
    }
  }
  
  m_ref_combined = m_ref; 

  delete h;

}




/// Fixme: need to fix this so that if the m_combine vector has values 
///        then the range of this vector is also modified
///        and then the combined reference recalculated:
///        at the moment, if the range is changed, the combine vector
///        needs to be recalculated and reset by hand


void appl::grid::merge( int i, int j ) {
  
  for ( int iorder=0 ; iorder<m_order ; iorder++ ) { 
    std::cout << "order: " << iorder << std::endl;
    
    igrid* g0 = m_grids[iorder][i];

    for ( int k=i+1; k<=j ; k++ ) {
      std::cout << "add grid: " << k << std::endl; 
      //    if ( m_ref->size()!=m_grids[iorder].size() ) matched = false;
      igrid* g1 = m_grids[iorder][i+1];    
      std::cout << "call igrid::merge()" << std::endl;
      g0->merge( g1 );
      m_grids[iorder].erase( m_grids[iorder].begin()+i+1 );
    }
  }
  
  std::cout << "merging reference..." << std::endl;
  
  for ( int k=i+1; k<=j ; k++ ) m_ref->merge_bins( i );

}







appl::igrid* newgrid( appl::igrid* g0, appl::igrid* g1 ) {
  

  g0->nodesQ2();
  g1->nodesQ2();

  std::cout << "newgrid() in" << std::endl;

  int NQ2 = g0->getNQ2();
 
  //  double q2min = std::min( g0->getQ2min(), g1->getQ2min() ); 
  //  double q2max = std::max( g0->getQ2max(), g1->getQ2max() ); 

  //  double _taumin = std::min( g0->taumin(), g1->taumin() ); 
  //  double _taumax = std::max( g0->taumax(), g1->taumax() ); 
  
  
  //   double dtau0 = g0->taumax() - g0->taumin(); 
  //   double dtau1 = g1->taumax() - g1->taumin(); 
  
  //   if ( g0->taumin() < g1->taumin() ) { 
  //   }
  
  int nt = (g1->taumax()-g0->taumin())/g0->deltatau() + 2; 
  // double nt = g1->getNQ2();
  

  std::cout << "newgrid: " << g0->deltatau() << "  "
	    << (g0->taumax()-g0->taumin())/g0->deltatau() << "  " << g0->getNQ2() << "  "     
	    << (g0->taumax()-g0->taumin())/(g0->getNQ2()-1)   << std::endl;
  
  double _taumin = g0->taumin();
  double _taumax = g0->taumin() + (nt-1)*g0->deltatau();  
  // double _taumax = g0->taumin() + g0->getNQ2()*g0->deltatau();  
  
  double q2min = g1->fQ2( _taumin );
  double q2max = g0->fQ2( _taumax );
  
  //  NQ2 = (_taumax - _taumin)/(g0->deltatau()-1);  
  NQ2 = nt;

  //  if ( delta1<delta0 ) delta = delta1;
  
  //  NQ2 = int( (std::log(q2max)-std::log(q2min))/delta + 0.99 );
  
  std::cout << "new Q2: " << NQ2 << "\t" << q2min << " " << q2max << std::endl;
  
  

  int Nx1 = g0->getNx1();
  
  double x1min = g0->getx1min();
  double x1max = g0->getx1max();
  
  {
    double delta0 = (std::log(g0->getx1max()) - std::log(g0->getx1min()))/(g0->getNx1()-1); 
    double delta1 = (std::log(g1->getx1max()) - std::log(g1->getx1min()))/(g1->getNx1()-1); 
    
    double delta = delta0;
  
    if ( delta1<delta0 ) delta = delta1;
    
    Nx1 = int( (std::log(x1max)-std::log(x1min))/delta + 1.5 );

    std::cout << "new x1: " << Nx1 << std::endl;

  }


  int Nx2 = 0;

  double x2min = std::min( g0->getx2min(), g1->getx2min() ); 
  double x2max = std::max( g0->getx2max(), g1->getx2max() ); 

  {
    double delta0 = (std::log(g0->getx2max()) - std::log(g0->getx2min()))/(g0->getNx2()-1); 
    double delta1 = (std::log(g1->getx2max()) - std::log(g1->getx2min()))/(g1->getNx2()-1); 
    
    double delta = delta0;
    
    if ( delta1<delta0 ) delta = delta1;
    
    Nx2 = int( (std::log(x2max)-std::log(x2min))/delta + 1.5 );

    std::cout << "new x2: " << Nx2 << std::endl;

  }


  int Nx = Nx1;
  double xmin = x1min;
  double xmax = x1max;


  if ( Nx2>Nx1 ) Nx = Nx1;
  if ( x1min>x2min ) xmin = x2min;
  if ( x1max<x2max ) xmax = x2max;

  std::cout << "Q2 " << NQ2 << " " << q2min << " " <<  q2max << " " <<  g0->tauorder() << " " 
	    << Nx << " " << xmin << " " <<  xmax << "  "
	    << g0->yorder() << " " 
	    << g0->transform() << " "  
	    << g0->qtransform() << " "  
	    << g0->SubProcesses() << " " << g0->isDISgrid() << std::endl;

#if 0
  appl::igrid* ig = new appl::igrid( NQ2, q2min, q2max, g0->tauorder(), 
				     Nx, xmin, xmax, g0->yorder(), 
				     g0->transform(), g0->qtransform(),
				     g0->SubProcesses(), g0->isDISgrid() );
#endif 

  appl::igrid* ig = new appl::igrid( NQ2, q2min, q2max, 5, 
				     Nx, xmin, xmax, 5, 
				     g0->transform(), g0->qtransform(),
				     g0->SubProcesses(), g0->isDISgrid() );

  ig->nodesQ2();


  std::cout << "new grid: " << ig << std::endl;

  std::cout << "newgrid() out" << std::endl;


  return ig;

} 




#if 0



appl::igrid* newgrid( appl::igrid* g0, appl::igrid* g1 ) {
  
  std::cout << "newgrid() in" << std::endl;

  int NQ2 = 0;
 
  double q2min = std::min( g0->getQ2min(), g1->getQ2min() ); 
  double q2max = std::max( g0->getQ2max(), g1->getQ2max() ); 

  //  double taumin = std::min( g0->gettaumin(), g1->gettaumin() ); 
  //  double taumax = std::max( g0->gettaumax(), g1->gettaumax() ); 
  
  {  
    double delta0 = (std::log(g0->getQ2max()) - std::log(g0->getQ2min()))/g0->getNQ2(); 
    double delta1 = (std::log(g1->getQ2max()) - std::log(g1->getQ2min()))/g1->getNQ2(); 
    
    double delta = delta0;
    
    if ( delta1<delta0 ) delta = delta1;
    
    NQ2 = int( (std::log(q2max)-std::log(q2min))/delta + 0.99 );

    std::cout << "new Q2: " << NQ2 << std::endl;

  }
  

  int Nx1 = 0;
  
  double x1min = std::min( g0->getx1min(), g1->getx1min() ); 
  double x1max = std::max( g0->getx1max(), g1->getx1max() ); 
  
  {
    double delta0 = (std::log(g0->getx1max()) - std::log(g0->getx1min()))/g0->getNx1(); 
    double delta1 = (std::log(g1->getx1max()) - std::log(g1->getx1min()))/g1->getNx1(); 
    
    double delta = delta0;
  
    if ( delta1<delta0 ) delta = delta1;
    
    Nx1 = int( (std::log(x1max)-std::log(x1min))/delta + 0.99 );

    std::cout << "new x1: " << Nx1 << std::endl;

  }


  int Nx2 = 0;

  double x2min = std::min( g0->getx2min(), g1->getx2min() ); 
  double x2max = std::max( g0->getx2max(), g1->getx2max() ); 

  {
    double delta0 = (std::log(g0->getx2max()) - std::log(g0->getx2min()))/g0->getNx2(); 
    double delta1 = (std::log(g1->getx2max()) - std::log(g1->getx2min()))/g1->getNx2(); 
    
    double delta = delta0;
    
    if ( delta1<delta0 ) delta = delta1;
    
    Nx2 = int( (std::log(x2max)-std::log(x2min))/delta + 0.99 );

    std::cout << "new x2: " << Nx2 << std::endl;

  }


  int Nx = Nx1;
  double xmin = x1min;
  double xmax = x1max;


  if ( Nx2>Nx1 ) Nx = Nx1;
  if ( x1min>x2min ) xmin = x2min;
  if ( x1max<x2max ) xmax = x2max;

  std::cout << "Q2 " << NQ2 << " " << q2min << " " <<  q2max << " " <<  g0->tauorder() << " " 
	    << Nx << " " << xmin << " " <<  xmax << "  "
	    << g0->yorder() << " " 
	    << g0->transform() << " "  
	    << g0->qtransform() << " "  
	    << g0->SubProcesses() << " " << g0->isDISgrid() << std::endl;

#if 0
  appl::igrid* ig = new appl::igrid( NQ2, q2min, q2max, g0->tauorder(), 
				     Nx, xmin, xmax, g0->yorder(), 
				     g0->transform(), g0->qtransform(),
				     g0->SubProcesses(), g0->isDISgrid() );
#endif 

  appl::igrid* ig = new appl::igrid( 2*NQ2, q2min, q2max, 5, 
				     2*Nx, xmin, xmax, 5, 
				     g0->transform(), g0->qtransform(),
				     g0->SubProcesses(), g0->isDISgrid() );

  std::cout << "new grid: " << ig << std::endl;

  std::cout << "newgrid() out" << std::endl;


  return ig;

} 



appl::igrid* limits( appl::igrid* g0, appl::igrid* g1 ) { 


  double q2min = std::min( g0->getQ2min(), g1->getQ2min() ); 
  double q2max = std::max( g0->getQ2max(), g1->getQ2max() ); 
    
  double x1min = std::min( g0->getx1min(), g1->getx1min() ); 
  double x1max = std::max( g0->getx1max(), g1->getx1max() ); 
  
  double x2min = std::min( g0->getx2min(), g1->getx2min() ); 
  double x2max = std::max( g0->getx2max(), g1->getx2max() ); 
  

  double nQ2 = newN( q2min, q2max,  = (std::log10(hi)-std::log10(lo))/N;
  
  std::cout << "delta: " << delta << std::endl;
    



    std::cout << "\t" << q2min << " " << q2max << "\t::\t" 
	      << x1min << " " << x1max << " :: "  
	      << x2min << " " << x2max << std::endl;  

    std::cout << "delta: " << (std::log10(q2max)-std::log10(q2min))/g0->getNQ2() << std::endl;
    std::cout << "delta: " << (std::log10(q2max)-std::log10(q2min))/delta << std::endl;

    double ndelta = (std::log10(q2max)-std::log10(q2min))/delta;
    
    int ndel = int(ndelta+0.99);
      
    if ( ndel>2*g0->getNQ2() ) ndel = int( (ndel+1)*0.5 );


}

#endif



void appl::grid::merge_bins( size_t i ) { 
  
  std::cout << "grid::merge_bins()`" << std::endl; 

  int n = m_ref->size();

  if ( n<2 ) return;

  m_combine.clear();

  std::cout << "ref::size() " << m_ref->size() << " " << m_ref_combined->size() << std::endl;

  //  bool matched = true;

  for ( int iorder=0 ; iorder<m_order ; iorder++ ) { 

    //    if ( m_ref->size()!=m_grids[iorder].size() ) matched = false;

    std::cout << "limits\n";

    igrid* g0 = m_grids[iorder][i];
    igrid* g1 = m_grids[iorder][i+1];    

    std::cout << "g0: \t";
    std::cout << g0->SubProcesses() << "\t::\t";  
    std::cout << g0->getNQ2() << "\t";  
    std::cout << g0->getQ2min() << "\t";  
    std::cout << g0->getQ2max() << "\t::\t";  

    std::cout << g0->getNx1() << "\t";  
    std::cout << g0->getx1min() << "\t";  
    std::cout << g0->getx1max() << "\t";  

    std::cout << g0->getNx2() << "\t";  
    std::cout << g0->getx2min() << "\t";  
    std::cout << g0->getx2max() << "\n";  

    std::cout << "g1: \t";
    std::cout << g1->SubProcesses() << "\t::\t";  
    std::cout << g1->getNQ2() << "\t";  
    std::cout << g1->getQ2min() << "\t";  
    std::cout << g1->getQ2max() << "\t::\t";  

    std::cout << g1->getNx1() << "\t";  
    std::cout << g1->getx1min() << "\t";  
    std::cout << g1->getx1max() << "\t";  

    std::cout << g1->getNx2() << "\t";  
    std::cout << g1->getx2min() << "\t";  
    std::cout << g1->getx2max() << "\n";  

    double delta = (std::log10(g0->getQ2max())-std::log10(g0->getQ2min()))/g0->getNQ2();
    
    std::cout << "delta: " << delta << std::endl;
    
    double q2min = std::min( g0->getQ2min(), g1->getQ2min() ); 
    double q2max = std::max( g0->getQ2max(), g1->getQ2max() ); 

    double x1min = std::min( g0->getx1min(), g1->getx1min() ); 
    double x1max = std::max( g0->getx1max(), g1->getx1max() ); 

    double x2min = std::min( g0->getx2min(), g1->getx2min() ); 
    double x2max = std::max( g0->getx2max(), g1->getx2max() ); 

    std::cout << "ranges:\t" << q2min << " " << q2max << "\t::\t" 
	      << x1min << " " << x1max << " :: "  
	      << x2min << " " << x2max << std::endl;  

    std::cout << "g0: " << *g0 << std::endl;
    std::cout << "g1: " << *g1 << std::endl;

#if 0
    g0->nodesQ2();
    g1->nodesQ2();

    g0->nodesx1();
    g1->nodesx1();

    g0->nodesx2();
    g1->nodesx2();
#endif

    appl::igrid* ng  = newgrid( g0, g1 ); 
    appl::igrid* ng1 = newgrid( g0, g1 ); 
    
    std::cout << "new grid: \nng: " << *ng << std::endl;  

    g0->remap( ng );
    g1->remap( ng1 );

    m_grids[iorder][i]   = ng;
    m_grids[iorder][i+1] = ng1;
    
    // no serialise grid g0 and g1 onto the new gerid

    delete g0; 
    delete g1; 

    // m_grids[iorder].erase( m_grids[iorder].begin()+i+1 );
  }

#if 0

    return;

    //    g0->add( g1 );
    //    delete g1;
    /// Fixme: should really get the superset of the x and Q2 ranges, 
    ///        and then remap both grids on to that
    g1->add( g0 );
    delete g0;
    m_grids[iorder][i] = g1;
    m_grids[iorder].erase( m_grids[iorder].begin()+i+1 );
  }
#endif

  //  std::cout << "merge_bins:: don't match - need to handle combinations" << std::endl;

  //  m_ref->merge_bins( i );

}





/// methods to handle the documentation
void appl::grid::setDocumentation(const std::string& s) { m_documentation = s; }
void appl::grid::addDocumentation(const std::string& s) {   
  if ( m_documentation.size() ) m_documentation += s;
  else                          setDocumentation(s);    
}



void appl::grid::include_photon( bool b ) {
  m_photon = b;
  for ( int iorder=0 ; iorder<m_order ; iorder++ ) { 
    for ( int iobs=0 ; iobs<Nobs() ; iobs++ ) {
      m_grids[iorder][iobs]->include_photon( b ); 
    }
  }
}


/// methods to handle bin-by-bin corrections

/// add a correction as a std::vector
void appl::grid::addCorrection( std::vector<double>& v, const std::string& label, bool ) {
  //  std::cout << "addCorrections(vector) " << v.size() << " " << m_ref->GetNbinsX() << std::endl;
  if ( v.size()==unsigned(m_ref->GetNbinsX()) || v.size()==unsigned(m_ref_combined->GetNbinsX()) ) {
    m_corrections.push_back( *getReference());
    m_corrections.back().name() = label;
    m_corrections.back().set( v );
    m_correctionLabels.push_back(label);
    m_applyCorrection.push_back(false);
    ///  std::cout << "appl::grid::addCorrection(vector) now " << m_corrections.size() << " corrections" << std::endl;
  }
}


/// add a correction by histogram
void appl::grid::addCorrection(appl::TH1D* h, const std::string& label, double scale, bool ) {
  
  appl::TH1D* hobs = 0;
  
  if      ( h->GetNbinsX()==m_ref->GetNbinsX() )          hobs = m_ref;
  else if ( h->GetNbinsX()==m_ref_combined->GetNbinsX() ) hobs = m_ref_combined;


  if ( hobs ) { 
    for ( int i=1 ; i<=h->GetNbinsX()+1 ; i++ ) { 
      if ( std::fabs(h->GetBinLowEdge(i+1)*scale-hobs->GetBinLowEdge(i+1))>1e-10 ) { 
	std::cerr << "bins " << h->GetBinLowEdge(i+1) << " " << hobs->GetBinLowEdge(i+1) << std::endl; 
	std::cerr << "grid::addCorrection(appl::TH1D* h): bin mismatch, not adding correction" << std::endl;
	return;
      }
    }

    std::vector<double> v(h->GetNbinsX());
    for ( int i=0 ; i<h->GetNbinsX() ; i++ ) v[i] = h->GetBinContent(i+1);
    if ( label=="" ) addCorrection(v, h->GetName());
    else             addCorrection(v, label);
  }
  else { 
    std::cerr << "grid::addCorrection(appl::TH1D* h): bin mismatch, not adding correction" << std::endl;
  }
  
}



// find the number of words used for storage
int appl::grid::size() const { 
    int _size = 0;
    for( int iorder=0 ; iorder<m_order ; iorder++ ) {
      for( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) _size += m_grids[iorder][iobs]->size();
    }
    return _size;
}


/// apply corrections to a std::vector
void appl::grid::applyCorrections(std::vector<double>& v, std::vector<bool>& applied) {
 
  if ( applied.size()!=m_corrections.size() ) throw grid::exception( "wrong number of corrections expected" ); 
 
  for ( unsigned i=m_corrections.size() ; i-- ; ) { 
 
    const std::vector<double>& corry = m_corrections[i].y();
    
    if ( applied[i] || v.size()!=corry.size() ) continue; /// correction applied already or wrong size     

    for ( unsigned j=v.size() ; j-- ; ) v[j] *= corry[j];
    applied[i] = true;
  }
  //  std::cout << "grid::applyCorrections(vector) done" << std::endl;
}




/// apply correction to a std::vector
bool appl::grid::applyCorrection(unsigned i, std::vector<double>& v) {

  if ( i>=m_corrections.size() ) throw grid::exception( "correction index out of range"  ); 
 
  const std::vector<double>& correction = m_corrections[i].y();

  if ( v.size()!=correction.size() ) return false; /// wrong size

  for ( unsigned k=v.size() ; k-- ; ) v[k] *= correction[k];
  return true;
}





/// do a deep comparison of all the different sub processes - if any 
/// are the same, then remove them
void appl::grid::shrink(const std::string& name, int ckmcharge) { 

  std::cout << "appl::grid::shrink()" << std::endl;

  std::string label[3] = { "LO", "NLO", "NNLO" };

  std::string genpdfname="";

  std::vector<std::vector<int> > keep(m_order);

  bool found = false;


  /// loop over orders 
  for( int iorder=0 ; iorder<m_order ; iorder++ ) {

    std::cout << "appl::grid::shrink() order " << iorder << std::endl;
    
    std::vector< std::vector<int> > pdf_combinations;
    pdf_combinations.reserve( Nobs_internal() );
    
    //    std::cout << "appl::grid::shrink() observable bins " << Nobs_internal() << std::endl;

    /// ... for each observable bin ...
 
    for( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) { 
      
      //      std::cout << "shrink() order: " << iorder << "\t obs: " << iobs;
      
      igrid* ig = m_grids[iorder][iobs];
    
      std::map< int, std::vector<int> > same;

      std::set<int> duplicates;

      std::set<int> empty;

      for ( int i=0 ; i<ig->SubProcesses() ; i++ ) { 

	if ( duplicates.find(i)!=duplicates.end() ) continue;

	int isize = ig->weightgrid(i)->size();

	if ( isize==0 ) { 
	  empty.insert(i); 
	  continue;
	}

	//	std::cout << i << " : ";

	keep[iorder].push_back( i );

	std::vector<int> vec; 
	
	found = true;

	for ( int j=i+1 ; j<ig->SubProcesses() ; j++ ) {

	  if ( duplicates.find(j)!=duplicates.end() ) continue;
	  if ( empty.find(j)!=empty.end() ) continue;

	  int jsize = ig->weightgrid(j)->size();
	  
	  if ( jsize==0 ) { 
	    empty.insert(j); 
	    continue;
	  }
	  
	  // std::cout << "\t" << j << ":" << jsize;
	   
	  if ( (*ig->weightgrid(i)) == (*ig->weightgrid(j)) ) { 
	    //  std::cout << "!"; 
	    vec.push_back(j);
	    duplicates.insert(j);
	  }

	}
	//	std::cout << std::endl;

	same.insert( std::map< int, std::vector<int> >::value_type( i, vec ) );

      }


      if ( same.empty() ) continue;

      //      for ( int ik=0 ; ik<m_order ; ik++ ) std::cout << m_genpdf[iorder]->name() << std::endl;

      lumi_pdf*  _pdf = 0;
      if ( m_genpdf[iorder]->name().find(".config")!=std::string::npos ){ 
	_pdf = dynamic_cast<lumi_pdf*>(m_genpdf[iorder]);
	if ( _pdf ) std::cout << "read lumi pdf: " << *_pdf << std::endl; 
      }
      else { 
	std::cerr << __FUNCTION__ << "\tpdf not found:" << m_genpdf[iorder]->name() << std::endl;
	return;
      }

      std::vector<combination> combinations;

      int i=0;

      std::map< int, std::vector<int> >::iterator itr  = same.begin();
      std::map< int, std::vector<int> >::iterator iend = same.end();

      while ( itr!=iend ) { 

	std::vector<int>& v = itr->second;
	//	std::cout << "\t" << i++ << "\tproc: " << itr->first << ":" << sizeitr->second << "\t" << itr->second << " ( " << (*_pdf)[itr->first] << " )" << std::endl;
	//	std::cout << "\t" << i << "\tproc: " << itr->first << ":" << sizeitr->second << "\t" << (*_pdf)[itr->first] << std::endl;
	//	for ( unsigned iv=0 ; iv<v.size() ; iv++ ) std::cout << "\t\t\t" << v[iv] << "\t" << (*_pdf)[v[iv]] << std::endl;

	std::vector<int> c(2);
	c[0] = i;
	c[1] = 0;

	const combination& comb = (*_pdf)[itr->first]; 

	for ( unsigned ic=0 ; ic<comb.size() ; ic++ ) { 
	  c[1]++;
	  c.push_back( comb[ic].first );
	  c.push_back( comb[ic].second );
	}

	for ( unsigned iv=0 ; iv<v.size() ; iv++ ) { 

	  const combination& comb = (*_pdf)[v[iv]]; 
	  
	  for ( unsigned ic=0 ; ic<comb.size() ; ic++ ) { 
	    c[1]++;
	    c.push_back( comb[ic].first );
	    c.push_back( comb[ic].second );
	  }
	}

	combinations.push_back( combination( c ) );

	i++;
	itr++;
      }

      //      std::set< int>::iterator eitr  = empty.begin();
      //      std::set< int>::iterator eend  = empty.end();

      //      std::cout << "\tempty: " << empty.size() << " sub-processes";
      //      while ( eitr!=eend ) std::cout << " " << (*eitr++);
      //      std::cout << std::endl; 
     
      lumi_pdf newpdf( "newpdf.config", combinations, 0 );

      pdf_combinations.push_back( newpdf.serialise() );
      
      //      std::cout << newpdf << std::endl;

      if ( found ) break;

    }

    bool common = true;
    for ( unsigned ipdf=1 ; ipdf<pdf_combinations.size() ; ipdf++ ) { 
    
      if ( pdf_combinations[ipdf].size()>0 ) {
	if ( pdf_combinations[ipdf]!=pdf_combinations[ipdf-1] ) { 
	  std::cout << "pdfs " << ipdf << " and " << ipdf-1 << " don't match" << std::endl; 
	  // << lumi_pdf("duff.config", pdf_combinations[ipdf]) 
	  common = false;
	}  
      } 
    }      
    
    /// create the actual lumi_pdfs from these combinations

    if ( common && pdf_combinations.size()>0 ) { 
      pdf_combinations[0].push_back(ckmcharge);

      lumi_pdf lpdf( name+label[iorder]+".config",   pdf_combinations[0]);

      std::cout << lpdf.name() << std::endl;

      lpdf.write( lpdf.name() );

      if ( genpdfname.size() ) genpdfname += ":";
      genpdfname += name+label[iorder]+".config";
    }  
  }

  std::cout << "appl::grid::shrink() genpdfname " << genpdfname << std::endl;

  //  if ( addpdf(m_genpdfname) )  findgenpdf( m_genpdfname );

  /// horray!! here we have the optimised pdf combinations written out, so now we 
  /// to delete the duplicated (or empty) grids ...

  /// loop over the igrids, telling each grid which processes to keep
  
  for( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) { 
    for( int iorder=0 ; iorder<m_order ; iorder++ ) {
      //      std::cout << "appl::grid::shrink()  obs " << iobs << "\torder " << iorder << std::endl;       
      m_grids[iorder][iobs]->shrink( keep[iorder] );
    }
  }

  /// ... and need to replace the genpdf with these new ones
  
  m_genpdfname = genpdfname;
  addpdf( m_genpdfname );
  findgenpdf( genpdfname );

  std::cout << "appl::grid::shrink()  new " 
	    << "\t" << m_genpdf[0]->name() << ":" << m_genpdf[0]->size() 
	    << "\t" << m_genpdf[1]->name() << ":" << m_genpdf[1]->size() 
	    <<  std::endl;

}







/// do a deep comparison of all the different sub processes - if any 
/// are the same, then remove them
void appl::grid::fixbroken(const std::string& name ) { 

  std::string label[3] = { "LO", "NLO", "NNLO" };

  std::string genpdfname="";

  std::cout << "appl::grid::fixbroken(): " << name << std::endl; 

  std::vector<std::vector<int> > keep(m_order);


  const lumi_pdf* _pdf[3];

  for ( int iorder=0 ; iorder<m_order ; iorder++ ) { 
   
    const lumi_pdf*& pdf = _pdf[iorder];

    pdf = dynamic_cast<const lumi_pdf*>( genpdf(iorder) );

    //    std::cout << "\tdetermined duplicate sets :  order: " << iorder << "\tnum lumi: " << pdf->size() << std::endl;
        
    std::vector<combination> c(pdf->size());
    std::vector<double>      sums(pdf->size());

    for ( unsigned ic=0 ; ic<pdf->size() ; ic++ ) c[ic] = pdf->at(ic);  

    std::vector<unsigned> togo;

    for ( unsigned ic=0 ; ic<pdf->size() ; ic++ ) {
      c[ic] = pdf->at(ic);  

      combination& cc = c[ic];

      std::vector<unsigned> to(cc.size(),99999);

      if ( cc.size()>1 ) { 
	std::cout << "combination: " << cc;
	
	unsigned copy_count = 0;
 
	for ( unsigned i=0 ; i<cc.size() ; i++ ) {
	  for ( unsigned ic0=ic+1 ; ic0<pdf->size() ; ic0++ ) {
	    if ( c[ic0].size()<2 && c[ic0].contains( cc[i] ) ) { 
	      std::cout << "\n\t adding " << cc[i] << " from " << ic << " to combination " << ic0 << " " << c[ic0];
	      copy_count++;
	      to[i] = ic0;
	      break;
	    }
	  }
	}
	
	bool keep_combination = true;
	if ( copy_count == cc.size() ) keep_combination = false;

	if ( !keep_combination ) std::cout << "\twill delete this combination";
	std::cout << std::endl;

	if ( !keep_combination ) { 
	  togo.push_back(ic);
	  for( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) { 	    
	    for ( unsigned ti=0 ; ti<to.size() ; ti++ ) { 
	      if ( to[ti]!=99999 ) m_grids[iorder][iobs]->combine_proc( to[ti], ic ); 
	    }
	  }
	}

      } // if ( cc.size()>1 )

    } // loop over parton luminosities


    /// work out which parton luminosities are actually duplicates

    std::vector< std::vector<int> > realc( pdf->size(), std::vector<int>(0) );

    std::vector<bool> duplicate( pdf->size(), false );

    for ( unsigned i=0 ; i<pdf->size() ; i++ ) { 

      bool ok=true;
      for ( unsigned ik=togo.size() ; ik-- ; ) if ( i==togo[ik] ) ok=false;
      if ( !ok ) continue;

      if ( duplicate[i] ) continue;
      
      for ( unsigned j=i+1 ; j<pdf->size() ; j++ ) {
	if ( i==j ) continue;
	
        if ( c[i] == c[j] ) { 
	  std::cout << "combination " << i << " " << j << " are identical" 
		    << "\t\tsums: " << sums[i] << "  " << sums[j] << "\t( " << (sums[i]-sums[j]) << " )" << std::endl; 
	  ok = false;
	  duplicate[j] = true;
	  realc[i].push_back(j);
	}
      }
    }


    std::vector<combination> actual_combinations;
    std::vector<int> keep;

    for ( unsigned i=0 ; i<pdf->size() ; i++ ) { 

      bool ok=true;
      for ( unsigned ik=togo.size() ; ik-- ; ) if ( i==togo[ik] ) ok=false;
      if ( !ok ) continue;

      if ( !duplicate[i] ) { 
	actual_combinations.push_back(c[i]);
	keep.push_back(i);
      }
    }  


    /// label for the new lumi_pdf

    std::string newconfig = label[iorder]+"-"+pdf->name();

    lumi_pdf lpdf( newconfig, actual_combinations, pdf->getckmcharge() ); 

    lpdf.write( lpdf.name() );

    if ( genpdfname.size() ) genpdfname += ":";
    genpdfname += newconfig;

    /// actually combine the grids   

    for( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) { 
   
      for ( unsigned ti=0 ; ti<keep.size() ; ti++ ) { 

	int i = keep[ti];

	for ( unsigned tj=0 ; tj<realc[i].size() ; tj++ ) { 
	  
	  int j = realc[i][tj];

	  //	  std::cout << "\t\t combine " << i << " " << j << "\t\t" 
	  //	    <<  (*m_grids[iorder][iobs]->weightgrid(i))(5,5,5) << " + " 
	  //	    <<  (*m_grids[iorder][iobs]->weightgrid(j))(5,5,5) << std::endl;

	  m_grids[iorder][iobs]->combine_proc( i, j ); 

	  //	  std::cout << "\t\t combine " << i << " " << j << "\t\t" <<  (*m_grids[iorder][iobs]->weightgrid(i))(5,5,5) << std::endl;

	}
      }
     

      m_grids[iorder][iobs]->shrink( keep ); 

    }
  }
    
 
  m_genpdfname = genpdfname;
  addpdf( m_genpdfname );
  findgenpdf( m_genpdfname );

  std::cout << "appl::grid::fixbroken()  new " 
	    << "\t" << m_genpdf[0]->name() << ":" << m_genpdf[0]->size(); 
  if (m_order>1) std::cout << "\t" << m_genpdf[1]->name() << ":" << m_genpdf[1]->size();
  if (m_order>2) std::cout << "\t" << m_genpdf[2]->name() << ":" << m_genpdf[2]->size();
  std::cout <<  std::endl;

}




/// add the weights from a src subprocess to some number of other 
/// destination processes and delete the original

void appl::grid::move( int iorder, int isrc, std::vector<int>& idest ) { 

  lumi_pdf* pdf = dynamic_cast<lumi_pdf*>( genpdf(iorder) );
  
  if ( size_t(isrc)>=pdf->size() ) return; 

  int ic = isrc;

  std::cout << "appl::grid::move(): " << std::endl;

  bool ok = false;
  for( int iobs=0 ; iobs<Nobs_internal() ; iobs++ ) { 	    
    for ( size_t it=0 ; it<idest.size() ; it++ ) { 
      m_grids[iorder][iobs]->combine_proc( idest[it], ic ); 
      ok = true;
    }
    if ( ok ) m_grids[iorder][iobs]->remove( ic );
  }

  pdf->remove( ic );

  std::cout << "appl::grid::move() "; 
  for ( int i=0 ; i<m_order ; i++ ) std::cout  << "\t" << m_genpdf[i]->name() << ":" << m_genpdf[i]->size(); 
  std::cout << std::endl;

}














void appl::grid::combineReference(bool force) { 
  //  std::cout << "grid::combineReference() " << m_ref->GetNbinsX();

  if ( m_combine.empty() ) return;

  if ( force ) { 
    if ( m_ref_combined ) { 
      if ( m_ref_combined!=m_ref ) delete m_ref_combined;
      m_ref_combined = 0;
    }
  }

  //  std::cout << "\tcombine " << m_ref_combined << " " << m_ref << std::endl;; 

  if ( m_ref_combined && m_ref_combined!=m_ref) return; 

  std::vector<double> hvec(  m_ref->GetNbinsX(), 0 );
  std::vector<double> hvece( m_ref->GetNbinsX(), 0 );


  for ( int i=m_ref->GetNbinsX() ; i-- ; )  { 
    hvec[i]  = m_ref->GetBinContent( i+1 );
    hvece[i] = m_ref->GetBinError( i+1 );
  }

  combineBins( hvec );
  combineBins( hvece, 2 );

  std::vector<double> limits(m_combine.size()+1);
  
  int i=0;
  limits[0] = m_ref->GetBinLowEdge(i+1);
  for ( unsigned ib=0 ; ib<m_combine.size() ; ib++) { 
    i += m_combine[ib]; 
    limits[ib+1] = m_ref->GetBinLowEdge(i+1);
  }
  
  /// need to make this a class variable set to 0 so we don't 
  /// recalculate this every time, only if m_combine changes 
  appl::TH1D* h = new appl::TH1D("reference", "xsec", m_combine.size(), &limits[0] );
  h->SetDirectory(0);

  for ( unsigned i=0 ; i<hvec.size() ; i++ ) { 
    h->SetBinContent( i+1, hvec[i] );
    h->SetBinError( i+1, hvece[i] );
  }

  m_ref_combined = h;

  //  std::cout << " -> " << m_ref->GetNbinsX() << std::endl;
  
}



void appl::grid::combineBins(std::vector<double>& hvec, int power ) const { 
  
  /// now combine bins if required ...
  
  if ( m_combine.size()!=0 ) { 
    /// need to go through, scaling by bin width, adding and then dividing by bin width again
    /// in the appl::TH1D* version, will need to recalculate the bin limits to create the new histogram
    
    std::vector<double> _hvec(m_combine.size(),0);
    
    unsigned nbins = 0;

    unsigned i=0;

    for ( unsigned ic=0 ; ic<m_combine.size() ; ic++ ) { 

      nbins += m_combine[ic];

      if ( nbins>hvec.size() ) throw grid::exception( "too many bins specified for rebinning"  ); 

      double sigma = 0;
      double width = 0;

      for ( int ib=0 ; ib<m_combine[ic] && i<hvec.size() ; ib++, i++ ) { 
	double deltaobs = m_ref->GetBinLowEdge(i+2)-m_ref->GetBinLowEdge(i+1);
	if ( power==1 ) sigma +=  hvec[i]*deltaobs;
	if ( power==2 ) sigma += (hvec[i]*deltaobs*hvec[i]*deltaobs);
	width += deltaobs;
      }

      if ( power==1 ) _hvec[ic] = sigma/width;
      if ( power==2 ) _hvec[ic] = std::sqrt(sigma)/width;
    }

    hvec = _hvec;
  }
}


std::ostream& operator<<(std::ostream& s, const appl::grid& g) {
  
  s << "==================================================" << std::endl;
  
  //  s << "appl::grid version " << g.version() << "\t(" << g.subProcesses(0) << " initial states, " << g.Nobs_internal() << " observable bins)" << std::endl;

  std::string basis[5] = {  "-LO, ",  "-NLO, ",  "-NNLO, ", "-Xtra0", "-Xtra1" };  
  std::string order[appl::MAXGRIDS];
  for ( int i=0 ; i<appl::MAXGRIDS ; i++ ) { 
    if ( i<5) order[i] = basis[i];
    else      order[i] = "-Unknown";
  }

  s << "appl::grid version " << g.version() << "\t( "; 

  for ( int i=0 ; i<g.nloops()+1 ; i++ ) s << g.subProcesses(i) << order[i];
  
  s << "initial states, " << g.Nobs_internal() << " observable bins )" << std::endl;
  
  if ( g.isOptimised() ) s << "Optimised grid" << std::endl;
  
  if ( g.isSymmetric() ) s << "Symmetrised in x1, x2" << std::endl;
  else                   s << "Unsymmetrised in x1, x2" << std::endl;
  
  if ( g.getNormalised() ) s << "Normalised " << std::endl;
  
  s << "leading order of processes  "  << g.leadingOrder() << std::endl;
  s << "number of loops for grid    "  << g.nloops() << std::endl;   
  s << "x->y coordinate transform:  "  << g.getTransform() << std::endl;

  s << "genpdf in use: " << g.getGenpdf() << std::endl;
  s << "--------------------------------------------------" << std::endl;
  s << "Observable binning: [ " << g.Nobs_internal() 
    << " bins : " << g.obsmin() << ",  " << g.obsmax() << " ]" << std::endl;

  //  for( int iorder=0 ; iorder<g.nloops()+1 ; iorder++ ) {
  for( int iorder=0 ; iorder<g.ninternalgrids() ; iorder++ ) {
    s << "order: " << iorder << "\n";
    for( int iobs=0 ; iobs<g.Nobs_internal() ; iobs++ ) {
      s << "  " 
	<< iobs << "\t" 
	<< std::setprecision(5) << std::setw(5) << g.getReference()->GetBinLowEdge(iobs+1) << "\t- " 
	<< std::setprecision(5) << std::setw(5) << g.getReference()->GetBinLowEdge(iobs+2) << "\t"; 
      s << "   " << *(g.weightgrid(iorder,iobs)) << std::endl;
    }
  }

  s << std::endl;
  
  return s;
}


void appl::grid::replaceBin( int iobs, grid& g ) { 

  std::cout << "replace bin " << iobs << std::endl;
 
  for ( int iorder=0 ; iorder<m_order ; iorder++ ) { 
    add_igrid( iobs, iorder, g.m_grids[iorder][iobs] );
  }

#if 0
  int iorder = 2;
  //  for ( int iorder=0 ; iorder<m_order ; iorder++ ) 
  { 
    std::cout << "order: " << iorder << std::endl;
    
    std::cout <<  m_grids[iorder][iobs] << " " << g.m_grids[iorder][iobs] << std::endl;

    igrid* ig = m_grids[iorder][iobs];

    igrid* hig = new igrid( *ig );

    //    m_grids[iorder][iobs] = 
    //    m_grids[iorder][iobs]->setparent( this ); 
  }
#endif

  std::cout << "fixing reference: " << iobs << std::endl;
  getReference_internal()->SetBinContent(iobs+1, g.getReference_internal()->GetBinContent(iobs+1) );
  getReference_internal()->SetBinError(iobs+1, g.getReference_internal()->GetBinError(iobs+1) );

  /// grrr, is this correct - why, why, why, why why oh why can't we 
  /// just use the correct binning in the first place !!!
  combineReference(true); 
}


std::vector<correction> appl::grid::corrections() const { 
  std::vector<correction> corr;
  for ( size_t i=0 ; i<m_corrections.size() ; i++ ) {
    const appl::TH1D& h = m_corrections[i];
    corr.push_back( correction( h.y(), h.name() ) );
  } 
  return corr;
}


static bool  no_threads = duff();
