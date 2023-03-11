/** emacs: this is -*- c++ -*- **/
/**
 **     @file    Directory.h
 **
 **     @brief   Description:  class to keep a directory for each object 
 **              in a root sort of way, but needed to keep 
 **              the root objects out of the actual code.   
 **
 **     @author  mark sutton
 **     @date    Wed May  4 17:54:25 BST 2005
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **/


#ifndef __DIRECTORY_H
#define __DIRECTORY_H

#include "appl_grid/appl_root.h"

#ifdef USEROOT

#include "TDirectory.h"

// #include "utils.h"

class Directory {

 public:

  Directory() : mDir(NULL), mPop(NULL) { } 
  Directory(std::string n) : mDir(NULL) { 
    mPop = gDirectory;
    //    depunctuate(n);
    mDir = gDirectory->mkdir(n.c_str());
  } 

  void push() { 
    mPop = gDirectory;
    if (mDir) mDir->cd();
  } 

  void pop() { if (mPop) mPop->cd(); }

  void Write() { 
    push();
    mDir->Write();
    pop();
  } 

 private:
  
  TDirectory* mDir;
  TDirectory* mPop;

};

#endif

#endif  /* __DIRECTORY_H */










