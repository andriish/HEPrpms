/** emacs: this is -*- c++ -*- **/
/**
 **     @file    tsparse_base.h
 **
 **     @brief   base class for common range checking and utilities for generic 
 **              sparse matrix classes
 **
 **     @author  mark sutton
 **     @date    Thu Nov 22 01:28:27 GMT 2007 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: tsparse_base.h, v0.0   Thu Nov 22 01:28:27 GMT 2007 sutt $
 **
 **/


#ifndef __TSPARSE_BASE_H
#define __TSPARSE_BASE_H

#include <iostream>


#include <cmath>


class tsparse_base {

public: 

  // range exception
  class out_of_range { 
  public: 
    out_of_range(const std::string& s="out of range") { std::cerr << s << std::endl; }
  };

public:
  
  // constructors etc

  tsparse_base()       : m_Nx(0),  m_lx(1),  m_ux(0), m_empty(true)    { }
  tsparse_base(const int& nx) : m_Nx(nx), m_lx(0),  m_ux(nx-1), m_empty(true) { } 
  tsparse_base(const int& nx, const int& lx, const int& _size=1)  
    : m_Nx(nx), m_lx(lx), m_ux(lx+_size-1), m_empty(true) { } 

  virtual ~tsparse_base() { } 


  // access functions

  int Nx() const { return m_Nx; } 
  int lo() const { return m_lx; }
  int hi() const { return m_ux; }

  virtual int size() const { return m_ux-m_lx+1; } //  + 3*sizeof(int); } 

  bool trimmed(int i) const { return ( i<m_lx || i>m_ux ? false : true );  } 

  // make sure index is not out of range
  void range_check(int i) const { if ( i<0 || i>m_Nx-1 ) { 
      std::cerr << "index i=" << i << std::endl;
      throw out_of_range(); 
    }    
  } 

  bool operator==(int i) const { return size()==i; } 

  int xmin() const { return m_lx; } 
  int xmax() const { return m_ux; } 

  bool empty() const { return ( m_empty || m_ux<m_lx ); }

  // shouldn't really be in here, it's just for printing 
  static double mant(double x) { 
    return  x/std::pow(10.,int(std::log10(std::fabs(x))+128)-128);
  }

protected:

  int m_Nx; // number of elements
  int m_lx; // indices of lowest and highest occupied 
  int m_ux; // elements : (u)pper and (l)ower

  bool m_empty; /// is the grid empty - need this to test in case the grid is untrimmed

};


#endif  // __TSPARSE_BASE_H 










