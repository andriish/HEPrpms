/** emacs: this is -*- c++ -*- **/
/**
 **     @file    basic_pdf.h
 **
 **     @brief   incoming parton combinations (not including a top 
 **              density) the 121 different sub processes correspond 
 **              to flavour combinations of where the first parton 
 **              if from proton 1 and the second from proton 2 of
 **
 **                0 - bbar-bbar
 **                1 - bbar-cbar
 **                2 - bbar-sbar 
 **                ...
 **                120 - b-b
 **
 **              The flavours are in the standard lhapdf convention:
 **
 **                -6 (tbar) to +6 (top) with the gluon, flavour 0  
 **
 **     @author  mark sutton
 **     @date    Mon Dec 10 01:36:04 GMT 2007 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: basic_pdf.h, v1.0   Mon Dec 10 01:36:04 GMT 2007 sutt $
 **
 **/



#ifndef BASIC_PDF_H
#define BASIC_PDF_H

#include <cmath>

#include "appl_grid/appl_pdf.h" 


// basic pdf combination class
class basic_pdf : public appl::appl_pdf {

public:

  basic_pdf() : appl::appl_pdf("basic") { m_Nproc=121; } 

  void evaluate(const double* _fA, const double* _fB, double* H) const;

  int  decideSubProcess(const int iflav1, const int iflav2) const;

};  
  


#if 0

inline void basic_pdf::evaluate(const double* _fA, const double* _fB, double* H) const {  

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
  

inline int  basic_pdf::decideSubProcess(const int iflav1, const int iflav2) const { 
  if ( std::fabs(iflav1)>5 || std::fabs(iflav2)>5 ) return -1;
  return (iflav1+5)*11+(iflav2+5);
}

#endif

// fortran callable wrapper
extern "C" void fbasic_pdf__(const double* fA, const double* fB, double* H);


#endif // BASIC_PDF_H

