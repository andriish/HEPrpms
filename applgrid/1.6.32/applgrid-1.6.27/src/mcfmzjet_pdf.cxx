/**
 **     @file    mcfmzjet_pdf.cxx
 **
 **     @brief   pdf transform functions for z production                  
 **
 **     @author  mark sutton
 **     @date    Mon Dec 10 01:36:04 GMT 2007 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: mcfmz_pdf.cxx, v1.0   Mon Dec 10 01:36:04 GMT 2007 sutt $
 **
 **/



#include "appl_grid/appl_pdf.h" 

#include "mcfmzjet_pdf.h"


// fortran callable wrapper
extern "C" void fmcfmzjet_pdf__(const double* fA, const double* fB, double* H) { 
  static mcfmzjet_pdf pdf;
  pdf.evaluate(fA, fB, H);
}

