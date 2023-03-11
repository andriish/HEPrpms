/** emacs: this is -*- c++ -*- **/
/**
 **   @file    binary.h        
 **                   
 **            fast binary search algorithm
 **   
 **   @author  sutt
 **   @date    Tue 29 Dec 2020 15:53:45 GMT
 **
 **   $Id: binary.h, v0.0   Tue 29 Dec 2020 15:53:45 GMT sutt $
 **
 **   Copyright (C) 2020 sutt (sutt@cern.ch)    
 **
 **/


#ifndef  BINARY_H
#define  BINARY_H

#include <algorithm>

inline int find_index( double x, const std::vector<double>& v ) { 

  if ( x<v[0] || x>=v.back() ) return -1;

  int a = 0;
  int b = v.size()-1;
  
  while ( b>(a+1) ) { 

    /// fast integer division by two 
    int c = ( a+b ) >> 1;
    
    if ( x<v[c] ) b = c;
    else          a = c;
   
  }

  return a;
}


/// simpler binary search std::lower_bound but it returns an iterator 
/// to the list, but we want to find the index to the bin - ie, binary search 
/// on one vector, and then access the corresponding element in a different vector
/// would be better with a vector of bins, rather than indepent vectors etc

// std::vector<double>::const_iterator find_lowerbound( double x, const std::vector<double>& v ) { 
//   return std::lower_bound<std::vector<double>::const_iterator>( v.begin(), v.end(), x );
// }


#endif  // BINARY_H 










