/** emacs: this is -*- c++ -*- **/
/**
 **     @file    generic_pdf.h
 **
 **     @brief   a generic pdf type - to read in the combinations
 **              for the subprocesses from a file, with the file 
 **              format determined by Tancredi (details will be 
 **              filled in as the implementation becomes more complete)                       
 **
 **     @author  mark sutton
 **     @date    Mon 28 Jan 2013 15:41:10 GMT 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: generic_pdf.h, v0.0   Mon 28 Jan 2013 15:41:10 GMT sutt $
 **
 **/


#ifndef  GENERIC_PDF_H
#define  GENERIC_PDF_H

#include <iostream>

#include "appl_grid/appl_pdf.h" 




class generic_pdf : public appl::appl_pdf {

public:

  //  generic_pdf(const std::string& s="", const std::vector<int> combinations=std::vector<int>() );
  generic_pdf(const std::string& s="" );

  virtual ~generic_pdf() {   } 

  /// actually evaluate the 
  void evaluate(const double* _fA, const double* _fB, double* H) const;

  /// additional user defined functions to actually initialise 
  /// based on the input file

  void initialise(const std::string& filename);

  bool initialised() const { return m_initialised; }


public:

  void ReadSubprocessSteering(const std::string& fname);

  void Print_ckm();  

  void PrintSubprocess();

  int  GetSubProcessNumber(){ return procname.size(); }

  //double* GetGeneralisedPdf(const double *,const double *); 

  int  decideSubProcess(const int iflav1, const int iflav2) const;

  int  decideSubProcessSet(const int iflav1, const int iflav2) {
    int ip = decideSubProcess( iflav1, iflav2 ); 
    currentsubprocess=ip;
    return ip;
  }


  //  void SetSubCurrentProcess(int mypro) { currentsubprocess=mypro; }

  //  int GetCurrentSubProcess() { 
  //    if ( currentsubprocess==-1 ) std::cout << " generic_pdf: current subprocess not defined ! " << std::endl;
  //    return currentsubprocess;
  //  }

  std::string GetSubProcessName(int isub) {return procname[isub];};

  //  void SetCurrentProcess(int mypro){ currentprocess=mypro; }
  
  //  int GetCurrentProcess() { 
  //    if (currentprocess==-1) std::cout<<" generic_pdf: current process not defined ! "<< std::endl;
  //    return currentprocess;
  //  }
  
  int GetnQuark() { return m_nQuark; }

  void SetnQuark(int nq) { m_nQuark=nq; }

  void MakeCkm();

  void PrintFlavourMap() {
    std::cout << " generic_pdf: print out flavour std::map " << std::endl;
    std::map<int,int>::iterator imap;
    for (imap = flavourtype.begin(); imap!=flavourtype.end(); ++imap){
      std::cout<<" Flavourtype["<<imap->first<<"]= "<<imap->second<<" "<< std::endl;
    }
  }

  //int GetNProc() {return procname.size(); };

  // generic_pdf() : appl_pdf("vrapzLO") { 
  // m_Nproc=this->GetSubProcessNumber(); 
  //} 


  
private:


  /// this might eventually become a std::string encoding the grid
  std::string m_filename;  

  /// has this been initialised yet?
  bool m_initialised;

  /// ckm matrices should they be needed ...
  //std::vector<double>                m_ckmsum;
  //std::vector<std::vector<double> >  m_ckm2;

  bool    m_debug;

  int     m_nQuark; //Number of Quarks 

  //  double* m_H; // generalised PDF defined in GetSubprocess

  // std::map name to index
  // std::maps flavour type names to iflavour <-> -6...6
  std::map<std::string, int> iflavour; 
  
  // std::maps iflavour to flavour typs names -6...6 <-> name 
  std::map<int, std::string> flavname;  

  // std::maps iflavour to types -6...6 <->  -2, -1, 0, 1, 2 
  std::map<int,int> flavourtype;

  // sum of quark flavour belonging to one flavour type up,down,gluon
  std::map<int,double> pdfA; //hadron A
  std::map<int,double> pdfB; //hadron B

  
  // std::maps iflavour -2, -1, 0, 1, 2 of first or second proton to iprocess
  std::map<int,int> Flav1; 
  std::map<int,int> Flav2;

  std::vector<std::string> procname; // names of subprocesses

  int currentsubprocess;
  int currentprocess;

  bool m_ckmflag;
};



inline std::ostream& operator<<( std::ostream& s, const generic_pdf& _g ) { 
  return s << "generic_pdf " << _g.name() << " " << _g.Nproc();  
}



#endif  // GENERIC_PDF_H 












