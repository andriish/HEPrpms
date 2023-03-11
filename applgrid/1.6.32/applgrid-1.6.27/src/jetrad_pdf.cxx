/**
 **     @file    jetrad_pdf.cxx
 **
 **     @brief   pdf transform function for jetrad                   
 **
 **     @author  mark sutton
 **     @date    Mon Dec 10 01:36:04 GMT 2007 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: jetrad_pdf.cxx, v1.0   Mon Dec 10 01:36:04 GMT 2007 sutt $
 **
 **/



#include "appl_grid/appl_pdf.h"

#include "jetrad_pdf.h"


// fortran callable wrapper
extern "C" void fjetrad_pdf__(const double* fA, const double* fB, double* H) { 
  static jetrad_pdf pdf;
  pdf.evaluate(fA, fB, H);
}
