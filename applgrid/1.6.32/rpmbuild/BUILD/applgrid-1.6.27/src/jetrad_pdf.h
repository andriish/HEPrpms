/** emacs: this is -*- c++ -*- **/
/**
 **     @file    jetrad_pdf.h
 **
 **     @brief   pdf transform function for jetrad                   
 **
 **     @author  mark sutton
 **     @date    Mon Dec 10 01:36:04 GMT 2007 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: jetrad_pdf.h, v1.0   Mon Dec 10 01:36:04 GMT 2007 sutt $
 **
 **/



#ifndef __JETRAD_PDF_H
#define __JETRAD_PDF_H


#include "appl_grid/appl_pdf.h" 



// jetrad pdf combination class
class jetrad_pdf : public appl::appl_pdf {

public:

  jetrad_pdf() : appl_pdf("jetrad") { m_Nproc=7; } 

  void evaluate(const double* _fA, const double* _fB, double* H) const;

};  
  


inline void jetrad_pdf::evaluate(const double* _fA, const double* _fB, double* H) const {  

  // remapping from pdg -6..6 convention to u..t ubar..tbar g internal
  // jetrad convention
  static int index_map[13] = { 2, 1, 3, 4, 5, 6, -2, -1, -3, -4, -5, -6, 0 }; 

  double f1[13];
  double f2[13];

  const double* _f1tmp = _fA+6;
  const double* _f2tmp = _fB+6;

  for ( int i=0 ; i<13 ; i++ )  f1[i] = _f1tmp[index_map[i]];
  for ( int i=0 ; i<13 ; i++ )  f2[i] = _f2tmp[index_map[i]];


  // now calculate the combinations

  double Q1=0, Q1bar=0, Q2=0, Q2bar=0, D=0, Dbar=0; 

  for ( int i=0 ; i<6 ; i++ )  {  Q1    += f1[i];   Q2    += f2[i];  }
  for ( int i=6 ; i<12 ; i++ ) {  Q1bar += f1[i];   Q2bar += f2[i];  }

  for ( int i=0 ; i<12 ; i++ ) D += f1[i]*f2[i];

  for ( int i=0 ; i<6 ; i++ ) { 
    Dbar += f1[i]*f2[i+6];  
    Dbar += f1[i+6]*f2[i]; 
  }
  
  H[0] =  D;
  H[1] =  Q1*Q2+Q1bar*Q2bar-D ;
  H[2] =  Dbar ;
  H[3] =  Q1*Q2bar+Q1bar*Q2-Dbar ;
  H[4] =  (Q1+Q1bar)*f2[12] + f1[12]*(Q2+Q2bar) ;
  H[5] =  f1[12]*f2[12];

}
  


// fortran callable wrapper
extern "C" void fjetrad_pdf__(const double* fA, const double* fB, double* H);


#endif // __JETRAD_PDF_H

