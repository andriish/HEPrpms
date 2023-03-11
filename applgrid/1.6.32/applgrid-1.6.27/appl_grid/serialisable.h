/* emacs: this is -*- c++ -*- */
/**
 **   @file    serialisable.h        
 **                   
 **   @author  sutt
 **   @date    Sat  9 Jan 2021 02:12:03 GMT
 **
 **   $Id: serialisable.h, v0.0   Sat  9 Jan 2021 02:12:03 GMT sutt $
 **
 **   Copyright (C) 2021 sutt (sutt@cern.ch)    
 **
 **/


#ifndef  SERIALISABLE_H
#define  SERIALISABLE_H

#include <iostream>

#include "appl_grid/serialise_base.h"

class serialisable {

public:

  // histogram error exception
  class exception : public std::exception { 
  public:
    exception(const std::string& s) { std::cerr << what() << " " << s << std::endl; }; 
    virtual const char* what() const throw() { return ""; }
  };

public:

  serialisable() { } 

  virtual ~serialisable() { } 

  virtual void serialise_internal( std::vector<SB::TYPE>& s ) const = 0;
 
  virtual void deserialise_internal( std::vector<SB::TYPE>::const_iterator& itr ) = 0;

  /// Serialisation and deserialisation wrappers ...

  std::vector<SB::TYPE> serialise() const { 
   
    std::vector<SB::TYPE> s;

    s.push_back( SB::WRITEGUARD );
    s.push_back(0); // overall size
    
    serialise_internal( s );
    
    s.push_back( SB::WRITEGUARD );
    s[1] = s.size();

    return s;
  }
  
  
  void deserialise( const std::vector<SB::TYPE>& v ) { 
  
    std::vector<SB::TYPE>::const_iterator itr = v.begin();    
    if ( (*itr++) != SB::WRITEGUARD ) throw exception("read error");
    (*itr++);

    deserialise_internal( itr );

    SB::TYPE var = (*itr++);
    if ( var != SB::WRITEGUARD ) throw exception("read error");   

  } 


};

inline std::ostream& operator<<( std::ostream& s, const serialisable&  ) { 
  return s;
}


#endif  // SERIALISABLE_H 










