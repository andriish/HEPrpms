/** emacs: this is -*- c++ -*- **/

// emacs: this is -*- c++ -*-
//
//   axis.h        
//
//                   
// 
//   Copyright (C) 2007 M.Sutton (sutt@hep.ucl.ac.uk)    
//
//   $Id: axis.h, v   Wed Nov 14 02:13:30 GMT 2007 sutt


#ifndef __AXIS_H
#define __AXIS_H

#include <iostream>
#include <iomanip>
#include <vector>
#include <cmath>



template<class T> 
class axis {

public:

  axis() : m_N(0), m_min(0), m_max(0) { }

  axis(int N, T mn, T mx) : m_N(N), m_min(mn), m_max(mx) { 
    if ( m_N>1 )   m_delta  = (m_max-m_min)/(m_N-1);
    else           m_delta  = 0;
    if ( m_delta ) m_idelta = 1/m_delta;
    else           m_idelta = 0;
    for ( int i=0 ; i<m_N ; i++ ) {
      double x2 = 0;
      if ( m_N>1 ) x2 = ((m_N-1-i)*m_min + i*m_max)/(m_N-1);
      else         x2 = m_min;
      m_v.push_back(x2);    
    }
  }

  ~axis() {  }
  
  int N()      const { return m_N; } 
  int size()   const { return m_N; } 
  T   min()    const { return m_min; }
  T   max()    const { return m_max; }
  T   delta()  const { return m_delta; }
  T   idelta() const { return m_idelta; }

  axis(const axis& ax) 
    : m_N(ax.m_N), m_min(ax.m_min), m_max(ax.m_max), 
      m_delta(ax.m_delta), m_idelta(ax.m_idelta) { 
    for ( int i=0 ; i<m_N ; i++ ) m_v.push_back(ax.m_v[i]);    
  } 


  axis& operator=(const axis& ax) { 
    m_N   = ax.m_N;
    m_min = ax.m_min;
    m_max = ax.m_max;
    
    m_delta  = ax.m_delta;
    m_idelta = ax.m_idelta;
    
    m_v = ax.m_v;

    return *this;
  }
  
  const std::vector<T>& v() const { return m_v; }  
  
  T&       operator()(int i)       { return m_v[i]; }
  const T& operator()(int i) const { return m_v[i]; }
  
  T&       operator[](int i)       { return m_v[i]; }  
  const T& operator[](int i) const { return m_v[i]; }  
  
  //  int bin(T x) const { return (x-m_min)*m_idelta; }
  int bin(T x) const { return int((x-m_min)*m_idelta); }
  
  void print(T (*f)(T)) const {
    for ( int i=0 ; i<m_N ; i++ ) { 
      std::cout << "\t" << std::setprecision(3) << std::setw(5) << f(m_v[i]); 
    }
    std::cout << std::endl;
  }

  /// NB: to avoid rounding errors, allow differences in the bin 
  ///     limits of 1e-10 the distance between the nodes   
  bool operator==(const axis& ax) const { 
  
    //    std::cerr << "axis::operator== m_N " << m_N << "\tax.m_N " << ax.m_N 
    //		<< "\tdmin " << (m_min-ax.m_min) << " " << m_min << " " << ax.m_min 
    //		<< "\tdmax " << (m_max-ax.m_max) << " " << m_max << " " << ax.m_max << std::endl;
    
    return  ( m_N==ax.m_N 
	      && (std::fabs(m_min-ax.m_min)<=m_delta*1e-10) 
	      && (std::fabs(m_max-ax.m_max)<=m_delta*1e-10) );   
  }
  
private:
  
  int m_N;
  
  T   m_min;
  T   m_max;
  T   m_delta;
  T   m_idelta;
  
  std::vector<T>  m_v;
};


template<class T>
std::ostream& operator<<(std::ostream& s, const axis<T>& a) { 
  s << "\t[ N=" << a.N(); 
  // for ( int i=0 ; i<a.N() ; i++ )  s << "\t" << std::setprecision(3) << std::setw(5) << a[i]; 
  s << ",\t"  << std::setprecision(3) << std::setw(5) << a[0];
  s << " .. " << std::setprecision(3) << std::setw(5) << a[a.N()-1];
  s << " ]";
  return s;
} 


#endif  // __AXIS_H 










