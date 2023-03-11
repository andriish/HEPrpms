/** emacs: this is -*- c++ -*- **/
/**
 **     @file    nlojet_pdf.h
 **
 **     @brief   pdf transform functions for nlojet                   
 **
 **     @author  mark sutton
 **     @date    Mon Dec 10 01:36:04 GMT 2007 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: nlojet_pdf.h, v1.0   Mon Dec 10 01:36:04 GMT 2007 sutt $
 **
 **/

#ifndef __NLOJET_PDF_H
#define __NLOJET_PDF_H

#include "appl_grid/appl_pdf.h" 



// nlojet pdf combination class 

class nlojet_pdf : public appl::appl_pdf {

public:

  nlojet_pdf() : appl_pdf("nlojet") { m_Nproc=7; } 

  void evaluate(const double* fA, const double* fB, double* H) const;

};  


inline void  nlojet_pdf::evaluate(const double* fA, const double* fB, double* H) const {  
  
  fA += 6;  // offset internal pointers so can use fA[-6]..fA[6] for simplicity
  fB += 6; 

  double Q1=0, Q1bar=0, Q2=0, Q2bar=0, D=0, Dbar=0, G1=fA[0], G2=fB[0]; 

  for ( int i=1  ; i<=6  ; i++ ) {  Q1    += fA[i];   Q2    += fB[i]; }
  for ( int i=-1 ; i>=-6 ; i-- ) {  Q1bar += fA[i];   Q2bar += fB[i]; }  

  // don't include the gluon (fA[0], fB[0])
  for ( int i=-6 ; i<=6 ; i++ ) if ( i ) D += fA[i]*fB[i];

  for ( int i=1 ; i<=6 ; i++ ) { 
    Dbar += fA[i]  * fB[-i];  
    Dbar += fA[-i] * fB[i]; 
  }
  
  H[0] =  G1*G2 ;
  H[1] =  (Q1+Q1bar)*G2 ;
  H[2] =  G1*(Q2+Q2bar) ;
  H[3] =  Q1*Q2+Q1bar*Q2bar-D ;
  H[4] =  D ; 
  H[5] =  Dbar ;
  H[6] =  Q1*Q2bar+Q1bar*Q2-Dbar ;
}


extern "C" void fnlojet_pdf__(const double* fA, const double* fB, double* H);


#endif  // __NLOJET_PDF_H



