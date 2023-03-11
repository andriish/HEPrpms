/**
 **     @file    nlojet_pdf.cxx
 **
 **     @brief   pdf transform function for nlojet                   
 **
 **     @author  mark sutton
 **     @date    Mon Dec 10 01:36:04 GMT 2007 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: nlojet_pdf.cxx, v1.0   Mon Dec 10 01:36:04 GMT 2007 sutt $
 **
 **/



#include "appl_grid/appl_pdf.h" 

#include "nlojet_pdf.h"


extern "C" void fnlojet_pdf__(const double* fA, const double* fB, double* H) { 
  static nlojet_pdf pdf;
  pdf.evaluate(fA, fB, H);
}




