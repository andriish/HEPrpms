/** emacs: this is -*- c++ -*- **/
/**
 **     @file    TFileVector.h
 **
 **     @brief   root TObject std::string std::vector class for writing std::string std::vectors
 **              to root files               
 **
 **     @author  mark sutton
 **     @date    Sat Mar 15 19:49:16 GMT 2008 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: TFileVector.h, v0.0   Sat Mar 15 19:49:16 GMT 2008 sutt $
 **
 **/


#ifndef  TFILESTRING_H
#define  TFILESTRING_H

#include "appl_grid/appl_root.h"

#ifdef USEROOT

#include <iostream>
#include <string>
#include <vector>

#include "TObjString.h"
#include "TObject.h"


// template<class T>
class TFileVector : public TObjString { 

public:
  
  TFileVector(const std::string& name="") : TObjString(name.c_str()) { } 

  std::vector<std::vector<double> >&       histos() { return mv; }
  const std::vector<std::vector<double> >& histos() const { return mv; }

  // get the name
  std::string  name() const { return GetName(); } 

  // get a value 
  std::vector<double>&       operator[](int i) { return mv[i]; }
  const std::vector<double>& operator[](int i) const { return mv[i]; }
  
  // get the size
  unsigned size()  const { return mv.size(); } 

  // add an element
  void add(const std::vector<double>& s) { 
    mv.push_back(s); 
  }

private:
  
  std::vector<std::vector<double> > mv;

  ClassDef(TFileVector, 1)

}; 


std::ostream& operator<<(std::ostream& s, const TFileVector& fv);


#endif

#endif  //  TFILEVECTOR_H 










