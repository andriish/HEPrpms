/* emacs: this is -*- c++ -*- */
/**
 **   @file    vector_stream.h        
 **                   
 **   @author  sutt
 **   @date    Fri  1 Jan 2021 11:37:27 GMT
 **
 **   $Id: vector_stream.h, v0.0   Fri  1 Jan 2021 11:37:27 GMT sutt $
 **
 **   Copyright (C) 2021 sutt (sutt@cern.ch)    
 **
 **/


#ifndef  VECTOR_STREAM_H
#define  VECTOR_STREAM_H

#include <iostream>

template<typename T>
std::ostream& operator<<(std::ostream& s, const std::vector<T>& v) {
  for ( unsigned i=0 ; i<v.size() ; i++ ) s << "\t" << v[i];
  return s;
}

#endif  // VECTOR_STREAM_H 










