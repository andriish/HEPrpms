/**
 **     @file    dis_pdf.cxx
 **
 **     @brief   pdf transform function for nlojet                   
 **
 **     @author  mark sutton
 **     @date    Sat 10 11:27:04 GMT 2009 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: dis_pdf.cxx, v1.0   Sat 10 11:27:04 GMT 2009 sutt $
 **
 **/



#include "appl_grid/appl_pdf.h" 

#include "dis_pdf.h"


extern "C" void fdis_pdf__(const double* fA, const double* fB, double* H) { 
  static dis_pdf pdf;
  pdf.evaluate(fA, fB, H);
}




