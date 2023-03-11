/** emacs: this is -*- c++ -*- **/
/**
 **     @file    mcfmwjet_pdf.h
 **
 **     @brief   pdf transform functions for W production                   
 **
 **     @author  mark sutton
 **     @date    Mon Dec 10 01:36:04 GMT 2007 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: mcfmw_pdf.h, v1.0   Mon Dec 10 01:36:04 GMT 2007 sutt $
 **
 **/



#ifndef __MCFMWJET_PDF_H
#define __MCFMWJET_PDF_H


#include "appl_grid/appl_pdf.h" 

//
// MCFM W production
//


//
//  W+
//
class mcfmwpjet_pdf : public appl::appl_pdf { 

public: 

  mcfmwpjet_pdf(const std::string& s="mcfm-wpjet") : appl_pdf(s) { 
    m_Nproc = 7; 
    make_ckm( true ); 
  } 

  ~mcfmwpjet_pdf() { }

  virtual void evaluate(const double* fA, const double* fB, double* H) const;

};



//
//  W-
//
class mcfmwmjet_pdf : public appl::appl_pdf { 

public: 

  mcfmwmjet_pdf(const std::string& s="mcfm-wmjet") : appl_pdf(s) { 
    m_Nproc = 7; 
    make_ckm( false ); 
  } 

  ~mcfmwmjet_pdf() { } 

  virtual void evaluate(const double* fA, const double* fB, double* H) const;

};


// actual funtion to evaluate the pdf combinations 
// for the W+

inline  void mcfmwpjet_pdf::evaluate(const double* fA, const double* fB, double* H) const { 
  
  const int nQuark = 6;
  const int iQuark = 5; 
  
  // offset so we can use fA[-6]
  // fA += 6;
  // fB += 6;
  
  double GA=fA[6];
  double GB=fB[6];
  double QA=0; double QB=0; double QbA=0; double QbB=0;
  
  for(int i = 1; i <= iQuark; i++) 
    {
      QA += fA[nQuark + i]*m_ckmsum[nQuark + i];
      QB += fB[nQuark + i]*m_ckmsum[nQuark + i];
    }
  for(int i = -iQuark; i < 0; i++) 
    {
      QbA += fA[nQuark + i]*m_ckmsum[nQuark + i];
      QbB += fB[nQuark + i]*m_ckmsum[nQuark + i];
    }
  
  H[2]=QbA * GB;
  H[3]= QA * GB;
  H[4]= GA * QbB;
  H[5]= GA * QB;
  H[6]= GA * GB;

  // have to zero H[0..1] first
  H[0]=H[1]=0; 
  
  for (int i1 = 3; i1 <= 5; i1 += 2)
    {
      for(int i2 = 8; i2 <= 10; i2 += 2)
	{
	  H[0] += fA[i1]*fB[i2]*m_ckm2[i1][i2];
	  H[1] += fA[i2]*fB[i1]*m_ckm2[i2][i1];
	}
    }  
}




//
// actual funtion to evaluate the pdf combinations 
//   for the W- 
//
inline  void mcfmwmjet_pdf::evaluate(const double* fA, const double* fB, double* H) const { 
  
  const int nQuark = 6;
  const int iQuark = 5; 
  
  // offset so we can use fA[-6]
  // fA += 6;
  // fB += 6;
  
  double GA=fA[6];
  double GB=fB[6];
  double QA=0; double QB=0; double QbA=0; double QbB=0;
  
  for(int i = 1; i <= iQuark; i++) 
    {
      QA += fA[nQuark + i]*m_ckmsum[nQuark + i];
      QB += fB[nQuark + i]*m_ckmsum[nQuark + i];
    }
  for(int i = -iQuark; i < 0; i++) 
    {
      QbA += fA[nQuark + i]*m_ckmsum[nQuark + i];
      QbB += fB[nQuark + i]*m_ckmsum[nQuark + i];
    }
  
  H[2]=  QA * GB;
  H[3]= QbA * GB;
  H[4]= GA  * QB;
  H[5]= GA  * QbB;
  H[6]= GA  * GB;

  // have to zero H[0..1] first
  H[0]=H[1]=0; 
  
  for (int i1 = 7; i1 <= 9; i1 += 2)
    {
      for(int i2 = 2; i2 <= 4; i2 += 2)
	{
	  H[0] += fA[i1]*fB[i2]*m_ckm2[i1][i2];
	  H[1] += fA[i2]*fB[i1]*m_ckm2[i2][i1];
	}
    }  
}
  

// fortran callable wrappers
extern "C" void fmcfmwpjet_pdf__(const double* fA, const double* fB, double* H);
extern "C" void fmcfmwmjet_pdf__(const double* fA, const double* fB, double* H);



#endif //  __MCFMWJET_PDF_H
