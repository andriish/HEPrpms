/**
 **     @file    integral.cxx
 **
 **     @author  mark sutton
 **     @date    Mon 24 Mar 2014 13:16:48 CET 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: integral.cxx, v0.0   Mon 24 Mar 2014 13:16:48 CET sutt $
 **
 **/


#include <iostream>

#include "appl_grid/integral.h"


namespace appl { 


double integral( const std::vector<double>& d, const grid& g ) {

  if ( d.size()!=unsigned(g.Nobs()) ) { 
    std::cerr << "integral: number of bins doesn't match" << std::endl;
    return 0;
  }

  double sum = 0;
  for ( unsigned i=d.size() ; i-- ; ) {
    double delta = g.deltaobs(i);
    sum += d[i]*delta;
  }
  
  return sum;
} 

double integral( const appl::TH1D* h ) {  
  double in = 0;
  for ( size_t i=0 ; i<h->size() ; i++ ) { 
    double width = h->hi(i)-h->lo(i);
    in += h->y(i)*width;
  }
  return in;
  //  return h->Integral( 1, h->GetNbinsX(), "width" ); 
}

}
