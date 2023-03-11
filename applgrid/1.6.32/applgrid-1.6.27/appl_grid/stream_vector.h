/** emacs: this is -*- c++ -*- **/
/**
 **   @file    stream_vector.h        
 **                   
 **            template class to provide a key :: vector pair 
 **            including the serialisation and deserialisation 
 **            to a vector 
 **  
 **   @author  sutt
 **   @date    Mon 28 Dec 2020 21:23:22 GMT
 **
 **   $Id: stream_vector.h, v0.0   Mon 28 Dec 2020 21:23:22 GMT sutt $
 **
 **   Copyright (C) 2020 sutt (sutt@cern.ch)    
 **
 **/


#ifndef  STREAM_VECTOR_H
#define  STREAM_VECTOR_H

#include <iostream>

#include "serialise_base.h"
#include "appl_grid/serialisable.h"


template<typename T>
class stream_vector : public serialisable {

public:

  /// constructors ...
  
  stream_vector() { }

  stream_vector( const std::string& name ) : mname(name) { }
 
  stream_vector( const std::string& name, const std::vector<T>& v ) : mname(name), mpayload(v) { } 

  /// constructer from a stream 
  stream_vector( const std::vector<SB::TYPE>& v ) {  deserialise( v ); }

  stream_vector( const stream_vector& sv ) :
    mname(sv.mname),
    mpayload(sv.mpayload) 
  { } 

  stream_vector& operator=( stream_vector&& sv ) { 
    std::swap( mname, sv.mname );
    std::swap( mpayload, sv.mpayload );
    return *this;
  }

  stream_vector& operator=( const stream_vector& sv ) { 
    mname    = sv.mname;
    mpayload = sv.mpayload;
    return *this;
  }
  

  /// destructor

  virtual ~stream_vector() { } 



  /// accessors ...

  std::string name() const { return mname; }

  size_t size() const { return mpayload.size(); }

  std::vector<T>&       payload()       { return mpayload; }
  const std::vector<T>& payload() const { return mpayload; }

  T  operator[]( int i ) const { return mpayload.at(i); }
  T& operator[]( int i )       { return mpayload.at(i); }

  T  at( int i ) const { return mpayload.at(i); }
  T& at( int i )       { return mpayload.at(i); }

  void push_back( const T& t ) { mpayload.push_back(t); }
  void add( const T& t )       { mpayload.push_back(t); }

  typename std::vector<T>::iterator begin() { return mpayload.begin(); }
  typename std::vector<T>::iterator end()   { return mpayload.end(); }

  typename std::vector<T>::const_iterator begin() const { return mpayload.begin(); }
  typename std::vector<T>::const_iterator end()   const { return mpayload.end(); }


  /// serialise ...

  void serialise_internal( std::vector<SB::TYPE>& s ) const { 
    SB::serialise( s, name() );
    SB::serialise( s, payload() );
  }


  /// deserialise ...

  void deserialise_internal( std::vector<SB::TYPE>::const_iterator& itr ) { 
    SB::deserialise( itr, mname ); 
    SB::deserialise( itr, mpayload );
  }
  
 
private:

  std::string    mname;
  std::vector<T> mpayload;

};

#include "appl_grid/vector_stream.h"


template<typename T> 
std::ostream& operator<<( std::ostream& s, const stream_vector<T>& sv ) { 
  s << "key: " << sv.name() << std::endl;
  for ( size_t i=0 ; i<sv.size() ; i++ ) s << "  " << sv[i] << std::endl;
  return s;
}


#endif  // STREAM_VECTOR_H 










