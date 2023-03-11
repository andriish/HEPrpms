/**
 **     @file    fappl_grid.cxx
 **
 **     @brief   fortran callable wrapper functions for the c++  
 **              appl grid project.
 **
 **     @author  mark sutton
 **     @date    Wed May 21 14:31:36 CEST 2008 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: fappl.cxx, v1.0   Wed May 21 14:31:36 CEST 2008 sutt $
 **
 **/

#include <map>
#include <iostream>
#include <sstream>

#include "appl_grid/appl_grid.h"
#include "appl_grid/fastnlo.h"


#include "fappl_grid.h"

/// externally defined alpha_s and pdf routines for fortran 
/// callable convolution wrapper
extern "C" double fnalphas_(const double& Q); 
extern "C" void   fnpdf_(const double& x, const double& Q, double* f);



static int idcounter = 0;
static std::map<int,appl::grid*> _grid;



void throw_exception( const std::string& msg, int id, const std::string& s="" ) {  
    std::stringstream s_;
    s_ << msg << id << s;
    throw appl::grid::exception( s_.str() );
}


/// grid std::map management

void ngrids_(int& n) { n=_grid.size(); }

void gridids_(int* ids) { 
  std::map<int,appl::grid*>::iterator gitr = _grid.begin();
  for ( int i=0 ; gitr!=_grid.end() ; gitr++, i++ ) ids[i] = gitr->first;
}



void bookgrid_(int& id, const int& Nobs, const double* binlims) 
{
  id = idcounter++;

  std::map<int,appl::grid*>::iterator gitr = _grid.find(id);

  if ( gitr==_grid.end() ) {
    std::cout << "bookgrid_() creating grid with id " << id << std::endl; 
    _grid.insert(  std::map<int,appl::grid*>::value_type( id, new appl::grid( Nobs, binlims,
									      2,    10, 1000, 1,
									      12,  1e-5, 1, 3, 
									      "nlojet", 1, 3, "f3") ) ) ;									 
    //  _grid->symmetrise(true);
  }
  else throw_exception( "grid with id ", id, " already exists" );
}



void readgrid_( int& id, const char* _s, int _len) {
  std::string s(_s,0,_len);
  id = idcounter++;
  std::map<int,appl::grid*>::iterator gitr = _grid.find(id);
  if ( gitr==_grid.end() ) { 
    appl::grid* g = new appl::grid(s);
    std::cout << "readgrid: " << id << " " << g->getDocumentation() << std::endl;
    _grid.insert(  std::map<int,appl::grid*>::value_type( id, g ) ); 
  }
  else throw_exception( "grid with id ", id, " already exists" ); 
}


  
void printgrid_(const int& id) { 
  std::map<int,appl::grid*>::iterator gitr = _grid.find(id);
  if ( gitr!=_grid.end() ) { 
    std::cout << "grid id " << id << "\n" << *gitr->second << std::endl;
  }
  else throw_exception( "No grid with id ", id ); 
}


void printgrids_() { 
  std::map<int,appl::grid*>::iterator gitr = _grid.begin();
  for ( ; gitr!=_grid.end() ; gitr++ ) { 
    std::cout << "grid id " << gitr->first << "\n" << *gitr->second << std::endl;
  }
}

  
void printgriddoc_(const int& id) { 
  std::map<int,appl::grid*>::iterator gitr = _grid.find(id);
  if ( gitr!=_grid.end() ) { 
    std::cout << "grid id " << id << "\n" << gitr->second->getDocumentation() << std::endl;
  }
  else throw_exception( "No grid with id ", id ); 
}


void releasegrid_(const int& id) { 
  std::map<int,appl::grid*>::iterator gitr = _grid.find(id);
  if ( gitr!=_grid.end() )     { 
    delete gitr->second; 
    _grid.erase(gitr);
  }
  else throw_exception( "No grid with id ", id ); 
}


void releasegrids_() { 
  std::map<int,appl::grid*>::iterator gitr = _grid.begin();
  for ( ; gitr!=_grid.end() ; gitr++ ) { 
    delete gitr->second; 
    _grid.erase(gitr);
  }
}

void setckm_( const int& id, const double* ckm ) { 
  std::map<int,appl::grid*>::iterator gitr = _grid.find(id);
  if ( gitr!=_grid.end() ) {
    gitr->second->setckm( ckm );
  }
  else  throw_exception( "No grid with id ", id );  
}


void getckm_( const int& id, double* ckm ) { 
  std::map<int,appl::grid*>::iterator gitr = _grid.find(id);
  if ( gitr!=_grid.end() ) { 
    std::vector<std::vector<double> > __ckm = gitr->second->getckm();
    for ( unsigned i=0 ; i<__ckm.size() ; i++ ) {
      for ( unsigned j=0 ; j<__ckm[i].size() ; j++ ) {
	ckm[i*3+j] = __ckm[i][j];
      }
    }
  }
  else  throw_exception( "No grid with id ", id );  
}



void redefine_(const int& id, 
	       const int& iobs, const int& iorder, 
	       const int& NQ2,  const double& Q2min, const double& Q2max, 
	       const int& Nx,   const double&  xmin, const double&  xmax) 
{
  std::map<int,appl::grid*>::iterator gitr = _grid.find(id);
  if ( gitr!=_grid.end() ) {
    gitr->second->redefine(iobs, iorder, 
			   NQ2, Q2min, Q2max, 
			   Nx,   xmin,  xmax); 
  }
  else  throw_exception( "No grid with id ", id );    
} 



void getreference_(const int& id, double* data, double* dataerror ) {
  std::map<int,appl::grid*>::iterator gitr = _grid.find(id);
  if ( gitr==_grid.end() ) throw_exception( "No grid with id ", id );  
  appl::TH1D* h = gitr->second->getReference();
  for ( size_t i=0 ; i<h->size() ; i++ ) { 
    data[i]      = h->y(i);
    dataerror[i] = h->ye(i);
  }
}


void getfracerror_(const int& id, double* f ) {
  std::map<int,appl::grid*>::iterator gitr = _grid.find(id);
  if ( gitr==_grid.end() ) throw_exception( "No grid with id ", id );  
  appl::TH1D* h = gitr->second->getReference();
  for ( size_t i=0 ; i<h->size() ; i++ ) { 
    if ( h->y(i)!=0 ) f[i] = h->ye(i)/h->y(i);
  }
}


int getnbins_(const int& id) { 
  std::map<int,appl::grid*>::iterator gitr = _grid.find(id);
  if ( gitr==_grid.end() ) throw_exception( "No grid with id ", id );  
  return gitr->second->Nobs();
}

int getbinnumber_(const int& id, double& x) { 
  std::map<int,appl::grid*>::iterator gitr = _grid.find(id);
  if ( gitr==_grid.end() ) throw_exception( "No grid with id ", id );  
  return gitr->second->obsbin(x);
}

double getbinlowedge_(const int& id, int& bin) { 
  std::map<int,appl::grid*>::iterator gitr = _grid.find(id);
  if ( gitr==_grid.end() ) throw_exception( "No grid with id ", id );  
  return gitr->second->obslow(bin);
}

double getbinwidth_(const int& id, const int& bin) { 
  std::map<int,appl::grid*>::iterator gitr = _grid.find(id);
  if ( gitr==_grid.end() ) throw_exception( "No grid with id ", id );
  return gitr->second->deltaobs(bin);
}




void convolute_(const int& id, double* data) { 
  convolutewrap_(id, data, fnpdf_, fnalphas_); 
}


void convolutewrap_(const int& id, double* data, 
		    void (*pdf)(const double& , const double&, double* ),  
		    double (*alphas)(const double& ) ) {  
  std::map<int,appl::grid*>::iterator gitr = _grid.find(id);
  if ( gitr!=_grid.end() ) { 
    appl::grid*    g = gitr->second;
    std::vector<double> v = g->vconvolute( pdf, alphas);
    for ( unsigned i=0 ; i<v.size() ; i++ ) data[i] = v[i];      
  }
  else throw_exception( "No grid with id ", id );  
}




/// for the pdf of an anti-particle given a pdf
void (*_pdf)(const double& x, const double& Q, double* f) = 0;

extern "C" void  antipdf(const double& x, const double& Q, double* f) { 
  if ( _pdf!=0 ) { 
    double xf[13];
    _pdf( x, Q, xf );
    for ( int i=0 ; i<13 ; i++ ) f[i] = xf[12-i]; 
  }
}



void convoluteppbar_(const int& id, double* data) { 
  convoluteppbarwrap_(id, data, fnpdf_, fnalphas_); 
}


void convoluteppbarwrap_(const int& id, double* data, 
			 void (*pdf)(const double& , const double&, double* ),  
			 double (*alphas)(const double& ) ) {  
  std::map<int,appl::grid*>::iterator gitr = _grid.find(id);
  if ( gitr!=_grid.end() ) { 
    _pdf = pdf;
    appl::grid* g = gitr->second;
    std::vector<double> v = g->vconvolute( pdf, antipdf, alphas, g->nloops() );
    for ( unsigned i=0 ; i<v.size() ; i++ ) data[i] = v[i];
    _pdf = 0;
  }
  else throw_exception( "No grid with id ", id );  
}




void convoluteorder_(const int& id, const int& nloops, double* data) { 
  std::map<int,appl::grid*>::iterator gitr = _grid.find(id);
  if ( gitr!=_grid.end() ) { 
    appl::grid*    g = gitr->second;
    std::vector<double> v = g->vconvolute(fnpdf_, fnalphas_, nloops);
    for ( unsigned i=0 ; i<v.size() ; i++ ) data[i] = v[i];      
  }
  else throw_exception( "No grid with id ", id );  
}




void fullconvolutewrap_(const int& id, double* data, 
			void (*pdf)(const double& , const double&, double* ),  
			double (*alphas)(const double& ),
			const int& nloops,
			const double& rscale, const double& fscale  ) {  
  std::map<int,appl::grid*>::iterator gitr = _grid.find(id);
  if ( gitr!=_grid.end() ) { 
    appl::grid*    g = gitr->second;
    std::vector<double> v = g->vconvolute( pdf, alphas, nloops, rscale, fscale);
    for ( unsigned i=0 ; i<v.size() ; i++ ) data[i] = v[i];      
  }
  else throw_exception( "No grid with id ", id );  
}


void fullconvolute_(const int& id, double* data, 
		    const int& nloops,
		    const double& rscale, const double& fscale  ) {  
  fullconvolutewrap_( id, data, fnpdf_, fnalphas_, nloops, rscale, fscale);
}



/// convolute functions with beam energy scaling 

void escaleconvolute_(const int& id, double* data, const double& Escale) { 
  escaleconvolutewrap_(id, data, fnpdf_, fnalphas_, Escale); 
}

void escaleconvolutewrap_(const int& id, double* data, 
			  void (*pdf)(const double& , const double&, double* ),  
			  double (*alphas)(const double& ), 
			  const double& Escale ) {  
  std::map<int,appl::grid*>::iterator gitr = _grid.find(id);
  if ( gitr!=_grid.end() ) { 
    appl::grid*    g = gitr->second;
    std::vector<double> v = g->vconvolute( pdf, alphas, g->nloops(), 1, 1, Escale );
    for ( unsigned i=0 ; i<v.size() ; i++ ) data[i] = v[i];      
  }
  else throw_exception( "No grid with id ", id );  
}



void escalefullconvolutewrap_(const int& id, double* data, 
			      void (*pdf)(const double& , const double&, double* ),  
			      double (*alphas)(const double& ),
			      const int& nloops,
			      const double& rscale, const double& fscale,
			      const double& Escale ) {  
  std::map<int,appl::grid*>::iterator gitr = _grid.find(id);
  if ( gitr!=_grid.end() ) { 
    appl::grid*    g = gitr->second;
    std::vector<double> v = g->vconvolute( pdf, alphas, nloops, rscale, fscale, Escale);
    for ( unsigned i=0 ; i<v.size() ; i++ ) data[i] = v[i];      
  }
  else throw_exception( "No grid with id ", id );  
}


void escalefullconvolute_(const int& id, double* data, 
			  const int& nloops,
			  const double& rscale, const double& fscale,
			  const double& Escale ) {  
  escalefullconvolutewrap_(id, data, 
			   fnpdf_, fnalphas_, nloops, rscale, fscale, Escale );
  
}


void writegrid_(const int& id, const char* _s, int _len) {
  std::map<int,appl::grid*>::iterator gitr = _grid.find(id);
  if ( gitr!=_grid.end() ) { 
    std::string s(_s,0,_len);
    std::cout << "writegrid_() writing " << s << "\tid " << id << std::endl;
    appl::grid* g = gitr->second;
    g->trim();
    //   g->print();
    g->Write(s);
  }
  else throw_exception( "No grid with id ", id );  
}



void fillgrid_(const int& id, 
	       const int& ix1, const int& ix2, const int& iQ,  
	       const int& iobs, 
	       const double* w, 
	       const int& iorder ) { 
  //  std::cout << "ix " << ix1 << " " << ix2 << "  iQ" << iQ << " " << iobs << "  " << iorder << std::endl;  
  std::map<int,appl::grid*>::iterator gitr = _grid.find(id);
  if ( gitr!=_grid.end() ) { 
    gitr->second->fill_index(ix1, ix2, iQ, iobs, w, iorder);
  }  
  else throw_exception( "No grid with id ", id );  
}


void readfastnlogrids_( int* ids, const char* _s, int _len ) { 

  std::string s(_s,0,_len);

  /// create the fastnlo grids
  fastnlo f(s);

  /// don't want the grids managed by the fastnlo object, 
  /// manage them in fortran with the std::map
  f.manageGrids(false);

  ///copy to the fortran accessible grid std::map
  std::vector<appl::grid*> grids = f.grids();

  //  std::cout << "hooray!" << std::endl;
  
  for ( unsigned i=0 ; i<grids.size() ; i++ ) { 
    int id = idcounter++;
    std::map<int,appl::grid*>::iterator gitr = _grid.find(id);
    if ( gitr==_grid.end() )  { 
      _grid.insert(  std::map<int,appl::grid*>::value_type( id, grids[i] ) );
      // std::cout << grids[i]->getDocumentation() << std::endl;
    }
    else throw_exception( "grid with id ", id, " already exists" );  

    ids[i] = id;
  }  

}


void getrun_( const int& id, double& run ) { 
  std::map<int,appl::grid*>::iterator gitr = _grid.find(id);
  if ( gitr!=_grid.end() ) { 
    appl::grid*    g = gitr->second;
    run = g->run();
  }
  else throw_exception( "No grid with id ", id );  
}


void lockckm_( const int& i ) { 
  if ( i==0 ) { 
    std::cout << "enable ckm overwrites" << std::endl;
    appl::appl_pdf::overwrites( true );
  }
  else { 
    std::cout << "disable ckm overrites\n" << std::endl;
    appl::appl_pdf::overwrites( false );
  }

}

  


