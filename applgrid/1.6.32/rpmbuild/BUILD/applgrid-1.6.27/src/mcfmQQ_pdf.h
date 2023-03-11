/** emacs: this is -*- c++ -*- **/
#ifndef __MCFMQQ_PDF_H
#define __MCFMQQ_PDF_H

#include <cmath>

#include "appl_grid/appl_pdf.h" 


//
// MCFM Q - Qbar production
//
class mcfmQQ_pdf : public appl::appl_pdf { 

public: 

  mcfmQQ_pdf(std::string name) : appl_pdf(name) { m_nFlavours = 5;  m_Nproc = 7 ;}
  ~mcfmQQ_pdf() { } 

  virtual void evaluate(const double* fA, const double* fB, double* H) const;

  int m_nFlavours;

};


// actual funtion to evaluate the pdf combinations 

inline  void mcfmQQ_pdf::evaluate(const double* fA, const double* fB, double* H) const 
{

  //  const int nQuark = 6;
//  const int iQuark = 5; 
  
  // offset psd ptrs so can use [-6..6] indexing rather than [nQuark+..] indexing

  fA += 6;
  fB += 6;

  //  double GA=fA[6];
  //  double GB=fB[6];
 double G1=fA[0];
 double G2=fB[0];
  
  double Q1=0, Q2=0, Q1bar=0, Q2bar=0;
  //double B1 = 0 , B2 = 0, B1bar = 0 , B2bar = 0;
  
  for ( int i=1  ; i<= m_nFlavours ; i++ ) {  Q1    += fA[i];   Q2    += fB[i]; }
  for ( int i=-1 ; i>=-m_nFlavours ; i-- ) {  Q1bar += fA[i];   Q2bar += fB[i]; }  

  //  B1 = 	fA[5]; B2 = fB[5];
  // B1bar = fA[-5]; B2bar = fB[-5];

  // don't include the gluon (fA[0], fB[0])
//  for ( int i=-6 ; i<=6 ; i++ ) if ( i ) D += fA[i]*fB[i];
//

  double QQb1 = 0, QQb2 = 0.;

  for ( int i=1 ; i<=m_nFlavours ; i++ ) { 
    QQb1 += fA[i]  * fB[-i];  
    QQb2 += fA[-i] * fB[i]; 
  }

//  double QQbHeavy = 0;
//  for ( int i=5 ; i<=6 ; i++ ) { 
//    QQbHeavy += fA[i]  * fB[-i];  
//    QQbHeavy += fA[-i] * fB[i]; 
//  }
  
  H[0] =  G1*G2 ;
  H[1] =  (Q1 )*G2 ;
  H[2] =  G1*(Q2 ) ;
  H[3] =  ( Q1bar)*G2 ;
  H[4] =  G1*(Q2bar) ;
  H[5] =  QQb1 ;
  H[6] =  QQb2 ;
//  H[4] =  QQbHeavy;
//  H[4] =  D ; 
//  H[5] =  Dbar ;
//  H[6] =  Q1*Q2bar+Q1bar*Q2-Dbar ;
    
  
  return;     
}


class mcfmTT_pdf : public mcfmQQ_pdf { 

public: 

  mcfmTT_pdf() : mcfmQQ_pdf("mcfm-TT") { m_nFlavours = 5 ;}
  ~mcfmTT_pdf() { } 

};

class mcfmBB_pdf : public mcfmQQ_pdf { 

public: 

  mcfmBB_pdf() : mcfmQQ_pdf("mcfm-BB") { m_nFlavours = 4 ;}
  ~mcfmBB_pdf() { } 

};

class mcfmCC_pdf : public mcfmQQ_pdf { 

public: 

  mcfmCC_pdf() : mcfmQQ_pdf("mcfm-CC") { m_nFlavours = 3 ;}
  ~mcfmCC_pdf() { } 

};

#endif  


