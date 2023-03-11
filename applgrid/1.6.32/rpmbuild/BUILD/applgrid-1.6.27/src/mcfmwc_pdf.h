/** emacs: this is -*- c++ -*- **/

#ifndef __MCFMWC_PDF_H
#define __MCFMWC_PDF_H


#include "appl_grid/appl_pdf.h" 

//
// MCFM W+Charm production
//


//
//  W+
//
class mcfmwpc_pdf : public appl::appl_pdf { 
  
public: 
  
 mcfmwpc_pdf(const std::string& s="mcfm-wpc") : appl_pdf(s) 
  { m_Nproc = 10; make_ckm( true ); } 
  
  ~mcfmwpc_pdf() { } 
  
  virtual void evaluate(const double* fA, const double* fB, double* H) const;


};



//
//  W-
//
class mcfmwmc_pdf : public appl::appl_pdf { 

public: 

  mcfmwmc_pdf(const std::string& s="mcfm-wmc") : appl_pdf(s) 
     { m_Nproc = 10; make_ckm( false ); } 

  ~mcfmwmc_pdf() {   } 

  virtual void evaluate(const double* fA, const double* fB, double* H) const;

};

// actual funtion to evaluate the pdf combinations 
// for the W+ Cbar

inline  void mcfmwpc_pdf::evaluate(const double* fA, const double* fB, double* H) const { 
  
  const int nQuark = 6;
//  const int iQuark = 5; 
  
  // offset so we can use fA[-6]
  // fA += 6;
  // fB += 6;
  
  double GA=fA[6];
  double GB=fB[6];
  double QA=0., QB=0.; 
  double DbA_c = 0., DbB_c = 0., DbADbB = 0. , UbA =0. , UbB = 0., dbAsbB = 0., sbAdbB = 0.;
  
  for(int i = -3; i <= -1; i += 2) 
    {
      DbA_c += fA[nQuark + i]*m_ckm2[nQuark + i][nQuark + 4];
      DbB_c += fB[nQuark + i]*m_ckm2[nQuark + 4][nQuark + i];
    }
 
  for(int i = 1; i <= 3; i+=1) 
    {
      QA += fA[nQuark + i];
      QB += fB[nQuark + i];
    }
  
  for(int i = -3; i <= -1; i+=2) 
    {
      DbADbB += fA[nQuark + i]*fB[nQuark + i] * m_ckm2[nQuark + i][nQuark + 4] ;
    }

  for(int i = -2; i <= -2; i +=2) 
    {
      UbA += fA[nQuark + i];
      UbB += fB[nQuark + i];
    }
  
  dbAsbB = fA[nQuark - 1]*fB[nQuark - 3];
  sbAdbB = fA[nQuark - 3]*fB[nQuark - 1];

  //LO
  H[0] = GA*DbB_c;
  H[1] = DbA_c*GB;
  //NLO
  H[2] = GA  * GB;
  
  H[3] = DbA_c    * QB  ;
  H[4] = QA     * DbB_c ;
  H[5] = UbA      * DbB_c ;
  H[6] = DbA_c    * UbB   ;
  H[7] = DbADbB   ;
  H[8] = dbAsbB   ;
  H[9] = sbAdbB   ;

}

//
// actual funtion to evaluate the pdf combinations 
//   for the W- + C 
//
inline  void mcfmwmc_pdf::evaluate(const double* fA, const double* fB, double* H) const { 
  
  const int nQuark = 6;
//  const int iQuark = 5; 
  
  // offset so we can use fA[-6]
  // fA += 6;
  // fB += 6;
  
  double GA=fA[6];
  double GB=fB[6];
  double DA_c=0., DB_c=0.;
  double QbA=0., QbB=0., UA =0. , UB = 0., DADB = 0., dAsB = 0., sAdB = 0.;
  
  for(int i = 1; i <= 3; i+=2) 
    {
      DA_c += fA[nQuark + i]*m_ckm2[nQuark + i][nQuark - 4];
      DB_c += fB[nQuark + i]*m_ckm2[nQuark - 4][nQuark + i];
    }

  for(int i = -3; i <= -1; i+=1) 
    {
      QbA += fA[nQuark + i];
      QbB += fB[nQuark + i];
    }
  
  for(int i = 1; i <= 3; i+=2) 
    {
      DADB += fA[nQuark + i]*fB[nQuark + i] * m_ckm2[nQuark + i][2] ;
    }

  for(int i = 2; i <= 2; i +=2) 
    {
      UA += fA[nQuark + i];
      UB += fB[nQuark + i];
    }
  
  dAsB = fA[nQuark + 1]*fB[nQuark + 3];
  sAdB = fA[nQuark + 3]*fB[nQuark + 1];

  //LO
  H[0] = GA*DB_c;
  H[1] = DA_c*GB;
  //NLO
  H[2] = GA  * GB;
  
  H[3] = DA_c    * QbB  ;
  H[4] = QbA     * DB_c ;
  H[5] = UA      * DB_c ;
  H[6] = DA_c    * UB   ;
  H[7] = DADB   ;
  H[8] = dAsB   ;
  H[9] = sAdB   ;

}
  

// fortran callable wrappers
extern "C" void fmcfmwpc_pdf__(const double* fA, const double* fB, double* H);
extern "C" void fmcfmwmc_pdf__(const double* fA, const double* fB, double* H);



#endif //  __MCFMWC_PDF_H
