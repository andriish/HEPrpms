/**
 **     @file    hoppet_init.cxx
 **
 **     @author  mark sutton
 **     @date    Sat 26 Sep 2009 13:24:31 BST 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: hoppet_init.cxx, v0.0   Sat 26 Sep 2009 13:24:31 BST sutt $
 **
 **/

#include "hoppet_init.h"

#include <iostream>
#include <vector>
#include <cmath>
#include <cstdlib>

#include "amconfig.h"
#ifdef HAVE_HOPPET
#include "hoppet_v1.h"
#endif


appl::hoppet_init::hoppet_init( double _Qmax, double _ymax ) {

  double dy = 0.1;   
  int nloop = 2;         

  double Qmax = _Qmax;

  double ymax = _ymax; /// always make this an integer multiple of dy

  std::cout << "appl::hoppet_init::hoppet_init()  dy = " << dy << "\tnloop = " << nloop << "\tQmax = " << Qmax << "\tymax = " << ymax << std::endl; 
  // the internal grid spacing (smaller->higher accuarcy)
  // 0.1 should provide at least 10^{-3} accuracy
  // the number of loops to initialise (max=3!)  
  // ensure that ymax is always an integer multiple of dy

#ifdef HAVE_HOPPET
  //  hoppetstart_( dy, nloop );
  /// NB: these parameters (except Qmax) should all be the default
  ///     parameters from hoppet 
  int MSbar = 1;
  hoppetstartextended_( ymax, dy, 1.0, Qmax, 0.25*dy, nloop, -6,  MSbar );
#else
  std::cerr << "appl::hoppet_init::hoppet_init() called but hoppet not included" << std::endl;
  exit(0);
#endif
}


appl::hoppet_init::~hoppet_init() {
  //  std::cout << "appl::hoppet_init::~hoppet_init()" << std::endl;
}


void appl::hoppet_init::fillCache( void (*pdf)(const double&, const double&, double* )  ) {

  //  fill the cache

  //  hoppetassign_( pdf );

  //  std::cout << "appl::hoppet_init::fillCache()" << std::endl; 

  clear();

  for ( double lQ=1 ; lQ<=4 ; lQ+=2 ) { 
    double Q = std::pow(10,lQ);
    for ( double lx=-5 ; lx<0 ; lx+=1 ) { 
      double x = std::pow(10,lx);
      double xf[13];
      pdf( x, Q, xf ); 
      for ( int i=0 ; i<13 ; i++ ) push_back( xf[i] ); 
    }
  }  

}



bool appl::hoppet_init::compareCache( void (*pdf)(const double&, const double&, double* )  ) {
 
  // fill a seperate cache for comparison

  // flag whether the cache has changed
  bool changed = false;

  //  std::cout << "appl::hoppet_init::compareCache()" << std::endl; 


  // is the cahche empty?
  if ( size()==0 ) {
#     ifdef HAVE_HOPPET 
      hoppetassign_( pdf );
#     endif
      fillCache( pdf );
      return changed = true;  
  }

  // fill the new cache for comparison
  //  std::cout << "appl::hoppet_init::compareCache() ccache.size() = " << ccache.size() << "\tsize() = " << size() << std::endl;  

  std::vector<double> ccache;
    
  for ( double lQ=1 ; lQ<=4 ; lQ+=2 ) { 
    double Q = std::pow(10,lQ);
    for ( double lx=-5 ; lx<0 ; lx+=1 ) { 
      double x = std::pow(10,lx);
      double xf[13];
      pdf( x, Q, xf ); 
      for ( int i=0 ; i<13 ; i++ ) ccache.push_back( xf[i] ); 
    }
  }
  

  // now compare with existing cache
  
  //  std::cout << "appl::hoppet_init::compareCache() ccache.size() = " << ccache.size() << "\tsize() = " << size() << std::endl;  
  
  if ( ccache.size()!= size() ) changed = true;  

  for ( unsigned i=0 ; i<ccache.size() ; i++ ) if ( ccache[i]!=at(i) ) changed = true;
  
  // cache (and hence the pdf) has changed, so replace the old cached 
  // values with the new and reinitialise hoppet with this new pdf 
  if ( changed ) { 
    
    assign( pdf );
    (*(std::vector<double>*)this) = ccache;
  }
  
  //  std::cout << "appl::hoppet_init::compareCache() changed = " << changed << std::endl; 

  return changed;
  
}



void appl::hoppet_init::assign( void (*pdf)(const double&, const double&, double* )  ) { 
  //  std::cout << "appl::hoppet_init::assign()" << std::endl; 
#   ifdef HAVE_HOPPET 
    hoppetassign_( pdf );
#   endif
}
