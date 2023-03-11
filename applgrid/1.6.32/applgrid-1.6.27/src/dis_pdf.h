/** emacs: this is -*- c++ -*- **/
/**
 **     @file    dis_pdf.h
 **
 **     @brief   pdf transform functions for nlojet                   
 **
 **     @author  mark sutton
 **     @date    Mon Dec 10 01:36:04 GMT 2007 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: dis_pdf.h, v1.0   Mon Dec 10 01:36:04 GMT 2007 sutt $
 **
 **/

#ifndef __DIS_PDF_H
#define __DIS_PDF_H

#include "appl_grid/appl_pdf.h" 



// nlojet pdf combination class 

class dis_pdf : public appl::appl_pdf {

public:

  dis_pdf() : appl_pdf("dis") { m_Nproc=3; } 

  void evaluate(const double* fA, const double* fB, double* H) const;

};  


inline void  dis_pdf::evaluate(const double* fA, const double* , double* H) const {  

  fA += 6;  // offset internal pointers so can use fA[-6]..fA[6] for simplicity

  double U=0, D=0, G=fA[0]; 

  // don't include the gluon (fA[0], fB[0])

  for ( int i=1 ; i<=5 ; i+=2 ) D += fA[i]+fA[-i] ;
  for ( int i=2 ; i<=6 ; i+=2 ) U += fA[i]+fA[-i] ;

  H[0] =  G ;  
  H[1] =  D+U ;  
  H[2] =  (D+4*U)/9 ; 
}


extern "C" void fdis_pdf__(const double* fA, const double* fB, double* H);


#endif  // __DIS_PDF_H



