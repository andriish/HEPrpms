/** emacs: this is -*- c++ -*- **/
/**
 **     @file    tsparse1d.h
 **
 **     @brief   very basic 2d sparse matrix class, needs to be improved, 
 **              and extended to 3d                   
 **
 **     @author  mark sutton
 **     @date    Fri Nov  9 00:17:31 CET 2007 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: tsparse1d.h 1784 2019-01-18 19:33:46Z sutt $
 **
 **/


#ifndef __TSPARSE1D_H
#define __TSPARSE1D_H

#include <iostream>

#include <iomanip>

#include <sstream>

#include <string>

#include "tsparse_base.h"


template<class T> 
class tsparse1d : public tsparse_base {

public: 

  tsparse1d(int nx) : tsparse_base(nx), m_v(NULL) {
    m_v = new T[m_Nx];
    for ( int i=0 ; i<Nx() ; i++ ) m_v[i]=0;
  }
  
  tsparse1d(int nx, int lx, int _size=1) 
    : tsparse_base(nx, lx, _size), m_v(NULL) {
    if ( _size ) m_v = new T[_size];
    for ( int i=0 ; i<_size ; i++ ) m_v[i]=0;
  }
  
  ~tsparse1d() { if ( m_v ) delete[] m_v; }
 
 
  T* trim() { 
    
    int xmin = m_lx;  int xmax = m_ux;
    
    for ( ; xmin<xmax+1 && m_v[xmin-m_lx]==0 ; xmin++ ) { } 
    for ( ; xmin<xmax   && m_v[xmax-m_lx]==0 ; xmax-- ) { } 

    // don't need to change anything 
    if ( xmin==m_lx && xmax==m_ux )  return m_v;    

    // it is empty now so delete everything
    if ( xmax<xmin ) {     
      m_lx = xmin;
      m_ux = xmax;      
      delete[] m_v; 
      return m_v = NULL; 
    }

    // create a new, smaller list
    T* newv = new T[xmax-xmin+1];
    T* newp = newv;
    T* mvp  = m_v+xmin-m_lx;
    for ( int j=xmin ; j<=xmax ; j++ ) (*newp++) = (*mvp++);

    m_lx = xmin; 
    m_ux = xmax;      
    
    delete[] m_v;
    m_v = newv;     
    
    return m_v;
  }
  

  void untrim() { 
    grow(0);
    grow(Nx()-1);
  }

  T& operator()(int i) {
    //    range_check(i);
    grow(i);
    return m_v[i-m_lx];
  }

  T operator()(int i) const {
    // range_check(i); 
    if ( i<m_lx || i>m_ux ) return 0;
    return m_v[i-m_lx];
  }


#if 1  
  void print() const { 
    std::cout << "   \tsize=" << size() << "\t";
    for ( int i=0 ; i<Nx() ; i++ ) { 
      if ( trimmed(i) ) std::cout << "o";
      else              std::cout << " ";
    }
    std::cout << std::endl;
  }
#else
  void print() const { 
    std::cout << "   \tsize=" << size() << "\t";
    for ( int i=0 ; i<Nx() ; i++ ) { 
      if ( trimmed(i) ) {
	cout << std::setprecision(2) << std::setw(4) << mant((*this)(i)) << "\t";
      }
      else              std::cout << "  -\t";
    }
    std::cout << std::endl;
  }
#endif

  T* v() { return m_v; }


  tsparse1d& operator*=(const double& d) { 
    for ( int i=0 ; i<Nx() ; i++ ) {     
      if ( tsparse_base::trimmed(i) ) { 
	if ( m_v[i-m_lx] ) m_v[i-m_lx]*=d;
      }
    }
    return *this;
  }
  
  tsparse1d& operator+=(const tsparse1d& t) {
    if ( Nx()!=t.Nx() ) throw out_of_range("bin mismatch");
    for ( int i=0 ; i<Nx() ; i++ ) (*this)(i)+=t(i);
    return *this;
  }

private:


  void grow(int i) { 

    // nothing needs to be done
    if ( i>=m_lx && i<=m_ux ) return;

    // is it empty?
    if ( m_lx>m_ux ) { 
      m_v = new T[1];
      (*m_v) = 0;
      m_lx = m_ux = i;
      return;
    }
    
    T* newv = new T[ ( i<m_lx ? m_ux-i+1 : i-m_lx+1 ) ];
    T* newp = newv;
    T* mvp  = m_v;

    int lx = m_lx;
    int ux = m_ux;
    for ( ; m_lx>i ; m_lx-- )  (*newp++) = 0;
    for ( ; lx<=ux ; lx++ )    (*newp++) = (*mvp++);
    for ( ; m_ux<i ; m_ux++ )  (*newp++) = 0;
    delete[] m_v;
    m_v = newv;
  }

private:

  T* m_v;

};



// stream IO template
template<class T>std::ostream& operator<<(std::ostream& s, const tsparse1d<T>& sp) { 
  for ( int i=0 ; i<sp.Nx() ; i++ ) s << "\t" << sp(i);
  return s;
}


#endif  // __TSPARSE1D_H 










