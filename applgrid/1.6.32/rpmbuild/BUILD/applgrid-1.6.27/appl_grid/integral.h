/** emacs: this is -*- c++ -*- **/
/**
 **     @file    integral.h
 **
 **     @author  mark sutton
 **     @date    Mon 24 Mar 2014 13:17:43 CET 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: integral.h, v0.0   Mon 24 Mar 2014 13:17:43 CET sutt $
 **
 **/


#ifndef  INTEGRAL_H
#define  INTEGRAL_H

#include <vector>

#include "appl_grid/appl_grid.h"

namespace appl { 

double integral( const std::vector<double>& d, const appl::grid& g );
double integral( const appl::TH1D* h ); //, const appl::grid& g );

}

#endif  // INTEGRAL_H 










