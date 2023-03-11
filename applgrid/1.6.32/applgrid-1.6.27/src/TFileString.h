/** emacs: this is -*- c++ -*- **/
/**
 **     @file    TFileString.h
 **
 **     @brief   root TObject std::string std::vector class for writing std::string std::vectors
 **              to root files               
 **
 **     @author  mark sutton
 **     @date    Sat Mar 15 19:49:16 GMT 2008 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: TFileString.h, v0.0   Sat Mar 15 19:49:16 GMT 2008 sutt $
 **
 **/


#ifndef __TFILESTRING_H
#define __TFILESTRING_H

#include "appl_grid/appl_root.h"

#ifdef USEROOT

#include <iostream>
#include <string>
#include <vector>

#include "TObjString.h"
#include "TObject.h"


class TFileString : public TObjString { 

public:
  
  TFileString(const std::string& name="") : TObjString(name.c_str()) { } 

  TFileString(const std::string& name, const std::string& tag) : 
    TObjString(name.c_str())
  { mstring.push_back(tag.c_str()); } 
 
  std::vector<std::string>&       tags()       { return mstring; }
  const std::vector<std::string>& tags() const { return mstring; }

  // get the name
  std::string  name() const { return GetName(); } 

  // get a value 
  std::string& operator[](int i)       { return mstring[i]; }
  std::string  operator[](int i) const { return mstring[i]; }

  std::string& at(int i)       { return mstring.at(i); }
  std::string  at(int i) const { return mstring.at(i); }
  
  // get the size
  size_t size()           const { return mstring.size(); } 

  // add an element
  void push_back(const std::string& s) { mstring.push_back(s); }
  void add(const std::string& s)       { mstring.push_back(s); }

private:
  
  std::vector<std::string> mstring;

  ClassDef(TFileString, 1)

}; 


std::ostream& operator<<(std::ostream& s, const TFileString& fs);


#endif

#endif  // __TFILESTRING_H 










