/** emacs: this is -*- c++ -*- **/
/**
 **     @file    vrapz_pdf.h
 **
 **     @brief   dis_pdf.h        
 **              pdf transform functions for nlojet                   
 **
 **     @author  mark sutton
 **     @date    Mon Dec 10 01:36:04 GMT 2007 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: dis_pdf.h, v1.0   Mon Dec 10 01:36:04 GMT 2007 sutt $
 **
 **/

#ifndef __VRAPZ_PDF_H
#define __VRAPZ_PDF_H

// #include "EWparams.h"

#include "appl_grid/appl_pdf.h" 



// LO vrap pdf combination class 

class vrapzLO_pdf : public appl::appl_pdf {

public:

  vrapzLO_pdf() : appl_pdf("vrapzLO") { 
    m_Nproc=2; 
  } 

  void evaluate(const double* fA, const double* fB, double* H) const ;

};  


inline void  vrapzLO_pdf::evaluate(const double* fA, const double* fB, double* H) const {  

  fA += 6;  // offset internal pointers so can use fA[-6]..fA[6] for simplicity
  fB += 6;  // offset internal pointers so can use fB[-6]..fB[6] for simplicity

  // qqbar_lumi for LO tree level contributions
  double U=0,  D=0;

  for ( int i=1 ; i<=5 ; i+=2 ) D +=  fA[i]*fB[-i] + fA[-i]*fB[i] ;
  for ( int i=2 ; i<6 ; i+=2 )  U +=  fA[i]*fB[-i] + fA[-i]*fB[i] ;

  H[0] =  D ;  
  H[1] =  U ;  

}








// NLO vrap pdf combination class 

class vrapzNLO_pdf : public appl::appl_pdf {

public:

  vrapzNLO_pdf() : appl_pdf("vrapzNLO") { 
    m_Nproc=6; 
  } 

  void evaluate(const double* fA, const double* fB, double* H) const;

};  




inline void  vrapzNLO_pdf::evaluate(const double* fA, const double* fB, double* H) const {  

  fA += 6;  // offset internal pointers so can use fA[-6]..fA[6] for simplicity
  fB += 6;  // offset internal pointers so can use fB[-6]..fB[6] for simplicity

  double GA=fA[0];
  double GB=fB[0]; 

  // qqbar_lumi for LO tree level contributions
  double U=0,  D=0;
  double UG=0, DG=0;
  double GU=0, GD=0;

  for ( int i=1 ; i<=5 ; i+=2 ) D +=  fA[i]*fB[-i] + fA[-i]*fB[i] ;
  for ( int i=2 ; i<6 ; i+=2 )  U +=  fA[i]*fB[-i] + fA[-i]*fB[i] ;

  H[0] =  D ;  
  H[1] =  U ;  

  /// qqbar_ax_lumi same as for qqbar_lumi only multiplied by aaf_d, aadf_u rather 
  /// than qpref_d and qpref_u

  /// for gq_lumi
  for ( int i=1 ; i<=5 ; i+=2 ) GD +=  GA*(fB[-i] + fB[i]) ;
  for ( int i=2 ; i<6 ; i+=2 )  GU +=  GA*(fB[-i] + fB[i]) ;

  H[2] = GD; 
  H[3] = GU; 
 
  /// for qg_lumi
  for ( int i=1 ; i<=5 ; i+=2 ) DG +=  GB*(fA[i] + fA[-i]);
  for ( int i=2 ; i<6 ; i+=2 )  UG +=  GB*(fA[i] + fA[-i]);

  /// need the vsqasq_d and vsqasq_u terms
  H[4] = DG; 
  H[5] = UG; 

#if 0
  std::cout << "appl::vrpz fA ";  for ( int i=-6 ; i<=6 ; i++ ) std::cout << "\t" << fA[i];  std::cout << std::endl;
  std::cout << "appl::vrpz fB ";  for ( int i=-6 ; i<=6 ; i++ ) std::cout << "\t" << fB[i];  std::cout << std::endl;
  std::cout << "appl::vrpz  H ";  for ( int i=0  ; i<6 ; i++ ) std::cout << "\t" << H[i];   std::cout << std::endl;
#endif


}





// vrap pdf combination class 

class vrapzNNLO_pdf : public appl::appl_pdf {

public:

  vrapzNNLO_pdf() : appl_pdf("vrapzNNLO") { 
    m_Nproc=19; 
  } 

  void evaluate(const double* fA, const double* fB, double* H) const;

};  


inline void  vrapzNNLO_pdf::evaluate(const double* fA, const double* fB, double* H) const {  

  fA += 6;  // offset internal pointers so can use fA[-6]..fA[6] for simplicity
  fB += 6;  // offset internal pointers so can use fB[-6]..fB[6] for simplicity

  double GA=fA[0];
  double GB=fB[0]; 

  // qqbar_lumi for LO tree level contributions
  double U=0,  D=0;
  double UG=0, DG=0;
  double GU=0, GD=0;

  for ( int i=1 ; i<=5 ; i+=2 ) D +=  fA[i]*fB[-i] + fA[-i]*fB[i] ;
  for ( int i=2 ; i<6 ; i+=2 )  U +=  fA[i]*fB[-i] + fA[-i]*fB[i] ;

  H[0] =  D ;  
  H[1] =  U ;  

  /// qqbar_ax_lumi same as for qqbar_lumi only multiplied by aaf_d, aadf_u rather 
  /// than qpref_d and qpref_u

  /// for gq_lumi
  for ( int i=1 ; i<=5 ; i+=2 ) GD +=  GA*(fB[-i] + fB[i]) ;
  for ( int i=2 ; i<6 ; i+=2 )  GU +=  GA*(fB[-i] + fB[i]) ;

  H[2] = GD; 
  H[3] = GU; 
 
  /// for qg_lumi
  for ( int i=1 ; i<=5 ; i+=2 ) DG +=  GB*(fA[i] + fA[-i]);
  for ( int i=2 ; i<6 ; i+=2 )  UG +=  GB*(fA[i] + fA[-i]);

  /// need the vsqasq_d and vsqasq_u terms
  H[4] = DG; 
  H[5] = UG; 

  /// qq_11_lumi
  double UQ11 = 0;
  double DQ11 = 0;

  double QB = 0;

  //  for ( int i=1 ; i<=5 ; i++ )    QB +=  fB[i] + fB[-i];
  for ( int i=1 ; i<=5 ; i+=2 ) DQ11 +=  fA[i] + fA[-i];
  for ( int i=2 ; i<=6 ; i+=2 ) UQ11 +=  fA[i] + fA[-i];
  

  /// need the vsqasq_d and vsqasq_u terms
  H[6] = DQ11 * QB;
  H[7] = UQ11 * QB;


  /// qq_22_lumi
  double UQ22 = 0;
  double DQ22 = 0;

  double QA = 0;

  for ( int i=1 ; i<=5 ; i++ )    QA +=  fA[i] + fA[-i];
  for ( int i=1 ; i<=5 ; i+=2 ) DQ22 +=  fB[i] + fB[-i];
  for ( int i=2 ; i<=6 ; i+=2 ) UQ22 +=  fB[i] + fB[-i];
  
  /// need the vsqasq_d and vsqasq_u terms
  H[8] = DQ22 * QA;
  H[9] = UQ22 * QA;


  /// qq_12_lumi NB: these *assume* that c=cbar, s=sbar etc.
  double UU12 = 0;
  double UD12 = 0;
  double DD12 = 0;


  /// need the vdvd, vuvd, vuvu terms
  H[10] = DD12 = ( fA[1] - fA[-1] ) * ( fB[1] - fB[-1] ) ; 
  H[11] = UD12 = ( fA[2] - fA[-2] ) * ( fB[1] - fB[-1] ) + ( fA[1] - fA[-1] ) * ( fB[2] - fB[-2] ) ;
  H[12] = UU12 = ( fA[2] - fA[-2] ) * ( fB[2] - fB[-2] ) ;

  /// NB: for ppbar H{10-12] -> - H[10-12] ??? 


  /// qq_12_ax_lumi
  /// need the adad, auad and auau terms
  H[13] =  ( fA[1] + fA[-1] + fA[3] + fA[-3] + fA[5] - fA[-5] ) * ( fB[1] + fB[-1] + fB[3] + fB[-3] + fB[5] - fB[-5] ); 
  H[14] =  ( fA[2] + fA[-2] + fA[4] + fA[-4] ) * ( fB[1] + fB[-1] + fB[3] + fB[-3] + fB[5] - fB[-5] ) + 
           ( fB[2] + fB[-2] + fB[4] + fB[-4] ) * ( fA[1] + fA[-1] + fA[3] + fA[-3] + fA[5] - fA[-5] ); 
  H[15] =  ( fA[2] + fA[-2] + fA[4] + fA[-4] ) * ( fB[2] + fB[-2] + fB[4] + fB[-4] ) ;


  /// qq_CE1_lumi

  double UUCE = 0;
  double DDCE = 0;

  for ( int i=1 ; i<=5 ; i+=2 )  DDCE +=  fA[i]*fB[i] + fA[-i]*fB[-i] ;
  for ( int i=2 ; i<6  ; i+=2 )  UUCE +=  fA[i]*fB[i] + fA[-i]*fB[-i] ;

  /// need the vsqasq_d and vsqasq_u terms
  H[16] = DDCE;
  H[17] = UUCE;

  /// qq_CE2_lumi for Z production is the same as qq_CE1_lumi
  /// qq_CF_lumi  for Z production is the same as qq_CE1_lumi
  
  /// qqbar_BC_lumi for Z production  is the same as 2 * qqbar_lumi 

  /// gg_lumi
  H[18] = GA*GB;

#if 0
  std::cout << "appl::vrpz fA ";  for ( int i=-6 ; i<=6 ; i++ ) std::cout << "\t" << fA[i];  std::cout << std::endl;
  std::cout << "appl::vrpz fB ";  for ( int i=-6 ; i<=6 ; i++ ) std::cout << "\t" << fB[i];  std::cout << std::endl;
  std::cout << "appl::vrpz  H ";  for ( int i=0  ; i<18 ; i++ ) std::cout << "\t" << H[i];   std::cout << std::endl;
#endif

}


extern "C" void fvrapzLO_pdf__(const double* fA, const double* fB, double* H);
extern "C" void fvrapzNLO_pdf__(const double* fA, const double* fB, double* H);
extern "C" void fvrapzNNLO_pdf__(const double* fA, const double* fB, double* H);


#endif  // __VRAPZ_PDF_H



