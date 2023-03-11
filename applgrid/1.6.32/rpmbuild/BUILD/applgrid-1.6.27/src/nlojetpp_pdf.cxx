/**
 **     @file    nlojetpp_pdf.cxx
 **
 **     @brief   pdf transform function for nlojet                   
 **
 **     @author  mark sutton
 **     @date    Mon Dec 10 01:36:04 GMT 2007 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: nlojetpp_pdf.cxx, v1.0   Mon Dec 10 01:36:04 GMT 2007 sutt $
 **
 **/



#include "appl_grid/appl_pdf.h" 

#include "nlojetpp_pdf.h"


extern "C" void fnlojetpp_pdf__(const double* fA, const double* fB, double* H) { 
  static nlojetpp_pdf pdf;
  pdf.evaluate(fA, fB, H);
}




