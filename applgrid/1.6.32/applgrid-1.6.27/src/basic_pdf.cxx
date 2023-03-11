/**
 **     @file    basic_pdf.cxx
 **
 **     @brief   pdf transform function for basic 121 ( 11 x 11 ) pdf combinations                   
 **
 **     @author  mark sutton
 **     @date    Mon Dec 10 01:36:04 GMT 2007 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: basic_pdf.cxx, v1.0   Mon Dec 10 01:36:04 GMT 2007 sutt $
 **
 **/



#include "appl_grid/basic_pdf.h"


// fortran callable wrapper
extern "C" void fbasic_pdf__(const double* fA, const double* fB, double* H) { 
  static basic_pdf pdf;
  pdf.evaluate(fA, fB, H);
}




void basic_pdf::evaluate(const double* _fA, const double* _fB, double* H) const {  

  // remapping from pdg -6..6 convention to u..t ubar..tbar g internal
  // basic convention

  const double* f1 = _fA+6;
  const double* f2 = _fB+6;

  ///  double H[121]; /// do not include top: 11x11 rather than 13x13
  int ih=0;
  /// from -b to b rather than -t to t
  for ( int i=-5 ; i<=5 ; i++ )  { 
    for ( int j=-5 ; j<=5 ; j++ )  { 
      /// f1 partons first !!! 
      H[ih++] = f1[i]*f2[j];
    }
  }
}
  

int  basic_pdf::decideSubProcess(const int iflav1, const int iflav2) const { 
  if ( std::fabs(iflav1)>5 || std::fabs(iflav2)>5 ) return -1;
  return (iflav1+5)*11+(iflav2+5);
}


