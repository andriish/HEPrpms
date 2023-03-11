/**
 **     @file    generic_pdf.cxx
 **
 **     @brief   a generic pdf type - to read in the combinations
 **              for the subprocesses from a file, with the file 
 **              format determined by Tancredi (details will be 
 **              filled in as the implementation becomes more complete)
 **
 **     @author  mark sutton
 **     @date    Mon 28 Jan 2013 15:40:45 GMT 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: generic_pdf.cxx, v0.0   Mon 28 Jan 2013 15:40:45 GMT sutt $
 **
 **/


// #include <stdlib.h>


#include <iostream>
#include <fstream>



#include "appl_grid/generic_pdf.h"


/// don't want a fortran implementation
//  extern "C" void fgeneric_pdf__(const double* fA, const double* fB, double* H) { 
//    static generic_pdf pdf;
//    pdf.evaluate(fA, fB, H);
//  }


// generic_pdf::generic_pdf(const std::string& s, const std::vector<int> combinations)  
generic_pdf::generic_pdf(const std::string& s)
  : appl_pdf(s), 
    m_initialised(false), 
    m_ckmflag(false)
 {

   /// need to check has the appropriate form, 
   if ( s!="" ) { 
     if ( s.find(".dat")==std::string::npos ) throw exception( std::string("generic_pdf() file ") + s + " does not have .dat extension - will not be able to save this grid to file" );
     else initialise( s );
   }

   //debug=false;
   std::cout << " initialize generic pdf " << s << " debug= " << m_debug << std::endl;
} 



void generic_pdf::initialise(const std::string& filename) { 


  if ( m_initialised ) { 
    std::cerr << "generic_pdf::initialise() already initialised" << std::endl;
    return;
  }


  /// first rename me with an appropriate file name
  /// we chose the structure "filename.dat"
  /// so the grid can tell it is generic, and can also 
  /// store the filename 
  /// NB: at some point we will encode the file contents in 
  ///     the grid itself  
  rename(filename);

  /// add tancredi's code here ...

  std::cout << "generic_pdf::initialise() " << name() << std::endl; 

  m_initialised = true;


  /// tancredi's code 

  m_debug=false;
  
  currentprocess=-1;
  currentsubprocess=-1;
  m_nQuark=6; // offset to go from 0,..,14 to  -6,..,0.,..,6 (7 including photon) 

#if 0
  std::vector<std::string> names;
  names.push_back("topbar");
  names.push_back("beautybar");
  names.push_back("charmbar");
  names.push_back("strangebar");
  names.push_back("upbar");
  names.push_back("downbar");
  names.push_back("gluon");
  names.push_back("down");
  names.push_back("up");
  names.push_back("strange");
  names.push_back("charm");
  names.push_back("beauty");
  names.push_back("top");
 
  std::string names[14] = { "topbar", "beautybar", "charmbar", "strangebar", "upbar", "downbar", 
			    "gluon", 
			    "down", "up", "strange" "charm", "beauty", "top", "photon" };
#endif

  std::string _names[14] = { "tbar", "bbar", "cbar", "sbar", "ubar", "dbar", 
		             "g", "d", "u", "s", "c", "b", "t", "pht" };

 
  std::string* names = (&_names[0])+6; /// so we can use -6..6 indexing

  for (int i=-m_nQuark; i<=m_nQuark+1; i++) {
    flavname[i] = names[i];
    iflavour[names[i]]=i;    
    if (m_debug)
      std::cout<<"generic_pdf "<<" iflavour[" <<names[i] << "]= " << iflavour[names[i]] << std::endl;
  }
  
  //pdfsumtypes1.clear();
  //pdfsumtypes2.clear();
  
  Flav1.clear();
  Flav2.clear();
  
  ReadSubprocessSteering(filename);
  
  PrintSubprocess();
  
  flavourtype.clear();
  int ifltype = -99;  
  for(int ifl = -m_nQuark; ifl <= m_nQuark+1; ifl++) {
    if ((ifl== 2)||(ifl== 4)||(ifl== 6))  ifltype =  2; 
    if ((ifl==-2)||(ifl==-4)||(ifl==-6))  ifltype = -2; 
    if ((ifl== 1)||(ifl== 3)||(ifl== 5))  ifltype =  1;
    if ((ifl==-1)||(ifl==-3)||(ifl==-5))  ifltype = -1;
    
    if (ifl==0 || ifl==7 ) ifltype= 0;
    flavourtype[ifl]=ifltype;
  }

  //this->MakeCkm();
  //if (debug) PrintFlavourMap();
  
  const int nsub = GetSubProcessNumber();
  
  if (m_debug) 
    std::cout << "generic_pdf::initialize nsub = " << nsub << std::endl;

} 


void  generic_pdf::evaluate(const double* fA, const double* fB, double* H) const {  
  //  fill this in with tancredi's code ...
  if ( !m_initialised ) {
    std::cout << "  generic_pdf::evaluate not initialised " << std::endl;
    return; 
  }
  
  /// off set the input std::vectors so we can address them from -n .. n with 0 = gluon  
  //  double* fA = _fA+m_nQuark; 
  //  double* fB = _fB+m_nQuark; 
  
  fA += 6;
  fB += 6;

  if (m_debug) std::cout << "generic_pdf:evaluate " << std::endl;
  
  //this->PrintFlavourMap();
  
  /*
    if (debug) {
    for(int i = -m_nQuark; i <= m_nQuark; i++) {
    std::cout << " fA[" << i << "]= " << fA[i+m_nQuark]
    << " fB[" << i << "]= " << fB[i+m_nQuark] 
    << std::endl;
    }
    }  
  */

  std::vector<double> pdfA(14,0); //hadron A
  std::vector<double> pdfB(14,0); //hadron B


  // reset pdf sums per flavour -2,-1,0,1,2 downbar, upbar, gluon, up, down
  // these are the pdf weights x by the ckm matrix 
  //  pdfA.clear();
  //  pdfB.clear();
  
  for ( int i=-2; i<=2 ; i++ ) { 
    pdfA[i]=0.; 
    pdfB[i]=0.;
  }
  
  pdfA[0]=fA[0]; // gluon
  pdfB[0]=fB[0];
  
  /*
    std::vector<double> myckmsum = std::vector<double>(myckm2.size(),0);
    for ( unsigned i=0 ; i<myckm2.size() ; i++ ) { 
    for ( unsigned j=0 ; j<myckm2[i].size() ; j++ ) myckmsum[i] += myckm2[i][j]; 
    }
  */
  
  /// offset so can be adressed from -n .. n as with the quark ids
  const double* _ckmsum = (&m_ckmsum[0])+m_nQuark;
  
  if ( m_ckmflag ) {
    /// do we need the ckm matrix ??
    for(int i=-6; i <=7; i++) {
      int j=flavourtype.find(i)->second;
      if (j==0) continue;
      pdfA[j] += fA[i]*_ckmsum[j];
      pdfB[j] += fB[i]*_ckmsum[j];
      if (m_debug)
	std::cout << " i= " << i 
		  << " j= " << j
		  << " fA[" << i << "]= " << fA[i]
		  << " fB[" << i << "]= " << fB[i]
		  << " pdfA[" << j << "]= " << pdfA[j] << " pdfB[" << j << "]= " << pdfB[j]
		  << " m_ckmsum[" << j << "]= " << _ckmsum[j]
		  << std::endl;
    }
  }
  else { 
    for(int i=-6; i <=7; i++) {
      int j=flavourtype.find(i)->second;
      if (j==0) continue;
      pdfA[j] += fA[i];
      pdfB[j] += fB[i];
      if (m_debug)
	std::cout<<" i= "<<i<<" j= "<<j
		 <<" fA["<<i<<"]= "<<fA[i]
		 <<" fB["<<i<<"]= "<<fB[i]
		 <<" pdfA["<<j<<"]= "<<pdfA[j]<<" pdfB["<<j<<"]= "<<pdfB[j]
		 <<std::endl;
    }
  }
  
  
  for ( unsigned iproc=0 ; iproc<procname.size() ; iproc++ ) {
    int ifl1=Flav1.find(iproc)->second;  
    int ifl2=Flav2.find(iproc)->second;
    
    H[iproc]=pdfA[ifl1]*pdfB[ifl2];
    if (ifl1==ifl2)   H[iproc]*=2.; // symetric contributions are counted twice
    
    if (m_debug) std::cout<<iproc<<" ifl1= "<<ifl1<<" ifl2= "<<ifl2
			  <<" pdfA["<<ifl1<<"]= "<<pdfA[ifl1]
			  <<" pdfB["<<ifl2<<"]= "<<pdfB[ifl2]
			  <<" H= "<<H[iproc]
			  <<" name= "<<procname[iproc]
			  <<std::endl;
  }
  
  
  return;
  
}; 






void generic_pdf::ReadSubprocessSteering(const std::string& fname){
  
  // 
  // read steering deciding on subprocesses
  //
  
  if (m_debug)
    std::cout << "generic_pdf::ReadSubprocessSteering: read subprocess configuration file: " << fname << std::endl; 
  
  /// use the search path to find config files
  std::ifstream& infile = openpdf( fname );

  //  if ( !infile ) { // Check open
  //    //    std::cerr << "Can't open " << fname << std::endl;
  //    infile.close(); /// why close it if it can't open?
  //    throw exception( std::cerr << "generic_pdf() cannot open file " << fname << std::endl );  
  //  } else {
  //    std::cout <<" generic_pdf:ReadData: read data file: " << fname << std::endl;
  //  }
  
  
  char line[256];
  int iproc=0;

  while (infile.good()) {

    //if (m_debug) std::cout << " good: " << infile.good() << " eof: " << infile.eof() << std::endl;

    if (!infile.good()) break;

    infile.getline(line,sizeof(line),'\n');
    std::string mytest = std::string(line);
    //std::cout<< "sizof test= "<< mytest.size() << std::endl;
    
    //if (m_debug) std::cout<< "line= "<< line << "\n";

    if ( !m_ckmflag ) { 
      if ( mytest=="Wplus" )  {
   	std::cout << "generic_pdf::ReadSubprocessSteering() setting W+ cmk matrix" << std::endl;
	make_ckm( true );
	m_ckmflag = true;
	continue;
      }
      else if ( mytest=="Wminus" ) { 
   	std::cout << "generic_pdf::ReadSubprocessSteering() setting W- cmk matrix" << std::endl;
	make_ckm( false );
	m_ckmflag = true;
	continue;
      }
    }
    
    if(line[0] != '%'  && mytest.size()!=0) {
      char flav1[100];char flav2[100];
      sscanf(line,"%s %s",flav1,flav2);
      std::cout << " ReadSubProcesses: flav1 = " << flav1 << " flav2 = " << flav2 << std::endl;
      
      int ifl1=iflavour[flav1];
      int ifl2=iflavour[flav2];

      if (m_debug)
	std::cout<<"ReadSubProcesses: ifl1 = " << ifl1 << " ifl2 = " << ifl2 << std::endl;
  
      Flav1[iproc]=ifl1;
      Flav2[iproc]=ifl2;
      iproc++;
      
      std::string myprocname = std::string(flav1) + "-" + std::string(flav2);
      if (m_debug) std::cout << iproc << " process name = " << myprocname << std::endl;;
      procname.push_back(myprocname);

    }

  }

  m_Nproc = procname.size();

}



int generic_pdf::decideSubProcess(const int iflav1, const int iflav2) const
{
  // 
  // iflav1 change from 0 to 21 (convention for gluons in sherpa)
  // assume that ckm comes with weights

  int    iProcess = -1;
 
  if (m_debug) std::cout << "generic_pdf::decideSubProces: " << std::endl; 
  if (m_debug) std::cout << " iflav1 = " << iflav1 << " iflav2 = " << iflav2 << std::endl;
  
  std::map<int,int>::const_iterator flav1 = flavourtype.find(iflav1);
  std::map<int,int>::const_iterator flav2 = flavourtype.find(iflav2);

  if ( flav1==flavourtype.end() || flav2==flavourtype.end() ) return iProcess;

  int ifl1=flav1->second;
  int ifl2=flav2->second;
  
  for ( unsigned i=0 ; i<procname.size() ; i++ ) {
    if (iProcess!=-1) continue;
    if (m_debug) std::cout << " " << i << " name= " << procname[i]
			 << " Flav1, Flav2 = " << (Flav1.find(i)->second) << " " << (Flav1.find(i)->second)
			 << std::endl; 

    if ( (Flav1.find(i)->second)==ifl1 && (Flav2.find(i)->second)==ifl2 ) {
      iProcess=i;
      // std::cout << " iProcess found " << iProcess << std::endl;
    }
  }
  
  if (m_debug) std::cout << "generic_pdf:decideSubprocess iProcess found " << iProcess << std::endl;
  
  if ( iProcess==-1 ) { 
    std::cout << "generic_pdf:decideSubprocess " << iflav1 << " <> " << iflav2 << std::endl; 
  }
  // else
  //    std::cout << " ***decideSubProcess " << iflav1 << " <> " << iflav2 << " iProcess = " << iProcess << std::endl; 
  //  currentsubprocess=iProcess;

  return iProcess;
}



// get ckm related information

void generic_pdf::Print_ckm() {  
  //
  // print ckm matrix for W+ and W-
  // 
  
  std::cout << "generic_pdf::Print_ckm = " << std::endl;
  std::cout <<" ckm size= "<<m_ckm2.size() << std::endl;
  if (m_ckm2.size()<=0) return;

  //
  for ( int i=0 ; i<14 ; i++ ) {
    for ( int j=0 ; j<14 ; j++ ) {
     if (m_ckm2[i][j]!=0)
      std::cout << " ckm[" << i << "][" << j << "]\t =\t " << m_ckm2[i][j] << std::endl;
    }
  }
  
  return;
}


void generic_pdf::PrintSubprocess() { 
  
  std::cout << "generic_pdf::PrintSubprocess:" << std::endl;

  // std::cout<<" size of Flav1: "<<  Flav1.size()<<endl;
  // std::map<int,int>::iterator imap;
  // for (imap = Flav1.begin(); imap!=Flav1.end(); ++imap){
  //  std::cout<<" "<<imap->first
  //           <<" flav1, flav2= "<<imap->second<<" "<<imap->second<<endl;
  // }

  std::cout << "\t Number of subprocesses: " << procname.size() << std::endl;
  
  for (unsigned i=0; i< procname.size(); i++) {
    std::cout << "\t" << i << " Flav1, Flav2 = " << Flav1[i] << " " << Flav2[i]
	      << "  " << procname[i]
	      << std::endl;
  };

  return;

}






