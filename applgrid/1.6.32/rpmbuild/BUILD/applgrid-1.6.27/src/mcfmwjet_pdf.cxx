/**
 **     @file    mcfmwjet_pdf.cxx
 **
 **     @brief   pdf transform functions                   
 **
 **     @author  mark sutton
 **     @date    Mon Dec 10 01:36:04 GMT 2007 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: mcfmw_pdf.cxx, v1.0   Mon Dec 10 01:36:04 GMT 2007 sutt $
 **
 **/



#include "appl_grid/appl_pdf.h" 

#include "mcfmwjet_pdf.h"

// fortran callable wrapper

extern "C" void fmcfmwpjet_pdf__(const double* fA, const double* fB, double* H) { 
  static mcfmwpjet_pdf pdf;
  pdf.evaluate(fA, fB, H);
}


// fortran callable wrapper

extern "C" void fmcfmwmjet_pdf__(const double* fA, const double* fB, double* H) { 
  static mcfmwmjet_pdf pdf;
  pdf.evaluate(fA, fB, H);
}




