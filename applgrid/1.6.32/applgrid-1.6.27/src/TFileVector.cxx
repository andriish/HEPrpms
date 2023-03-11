/**
 **     @file    TFileVector.cxx
 **
 **     @brief   root TObject std::string std::vector class for writing std::string std::vectors
 **              to root files               
 **
 **     @author  mark sutton
 **     @date    Sat Mar 15 19:50:15 GMT 2008 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: TFileString.cxx, v0.0   Sat Mar 15 19:50:15 GMT 2008 sutt $
 **
 **/

#include "appl_grid/appl_root.h"

#ifdef USEROOT

#include "TFileVector.h"

ClassImp(TFileVector)


std::ostream& operator<<(std::ostream& s, const TFileVector& fs) { 
  for ( unsigned i=0 ; i<fs.size() ; i++ ) { 
    for ( unsigned j=0 ; j<fs[i].size() ; j++ ) s << "\t" << fs[i][j]; 
    s << std::endl;
  }
  return s;
}

#endif


