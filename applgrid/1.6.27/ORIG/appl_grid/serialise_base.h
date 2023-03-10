/** emacs: this is -*- c++ -*- **/
/**
 **   @file    serialise_base.h        
 **                   
 **   @author  sutt
 **   @date    Tue 29 Dec 2020 13:57:14 GMT
 **
 **   $Id: serialise_base.h, v0.0   Tue 29 Dec 2020 13:57:14 GMT sutt $
 **
 **   Copyright (C) 2020 sutt (sutt@cern.ch)    
 **
 **/

#ifndef  SERIALISE_BASE_H
#define  SERIALISE_BASE_H

#include <vector>
#include <string>
#include <iostream>

#include <zlib.h>


namespace SB { 

  /// serialisation

  const double WRITEGUARD = 1234567890123456; 

  typedef uint64_t TYPE;  

  inline void serialise( std::vector<TYPE>& strm, const std::string& s ) { 
    strm.push_back( s.size() );
    for ( size_t i=0 ; i<s.size() ; i++ ) strm.push_back( s[i] ); 
  } 

  template<typename T>
  void serialise( std::vector<TYPE>& strm, const T& t ) { 
    strm.push_back( *((SB::TYPE*)&t) );
  } 

  template<typename T>
  void serialise( std::vector<TYPE>& strm, const std::vector<T>& v ) { 
    strm.push_back( v.size() );
    for ( size_t i=0 ; i<v.size() ; i++ ) serialise( strm, v[i] );
  } 

#if 0
  /// doesn't work for some reason
  inline void serialise( std::vector<TYPE>& strm, const std::string& s ) { 
    serialise( strm, std::vector<char>(s.begin(),s.end()) );
  }
#endif


  /// deserialise 

  inline void deserialise( std::vector<TYPE>::const_iterator& itr, std::string& s ) { 
    s.clear();
    size_t n = (*itr++);
    for ( size_t i=0 ; i<n ; i++ ) s += char(*itr++);
  }

  template<typename T>
  void deserialise( std::vector<TYPE>::const_iterator& itr, T& t ) { 
    t = *((T*)&(*itr++));
  }

  template<typename T> 
  void deserialise( std::vector<TYPE>::const_iterator& itr, std::vector<T>& v ) { 
    size_t n = (*itr++);
    v.clear();
    v.resize(n);
    for ( size_t i=0 ; i<n ; i++ ) deserialise( itr, v[i] );
  }

}



#endif  // SERIALISE_BASE_H 



