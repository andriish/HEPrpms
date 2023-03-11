/* emacs: this is -*- c++ -*- */
/**
 **   @file    file_index.cxx        
 **                   
 **   @author  sutt
 **   @date    Thu 31 Dec 2020 14:42:12 GMT
 **
 **   $Id: file_index.cxx, v0.0   Thu 31 Dec 2020 14:42:12 GMT sutt $
 **
 **   Copyright (C) 2020 sutt (sutt@cern.ch)    
 **
 **/


#include <iostream>
#include <vector>
#include <map>
#include <string>

#include "appl_grid/file_index.h"



file_index::file_index() : mname("index"), mrunning(0) { } 

file_index::~file_index() { } 


void file_index::add( const std::string& _s, int bytes, int offset, int counter ) { 
    std::string s = _s;
    check_for_duplicates( s, counter );
    if ( counter>0 ) std::cerr << "index::add() duplicated key: " << _s << "\tusing: " << s << std::endl;  
    mkeys.push_back( s );
    mmap.insert( map_t::value_type( s, entry( bytes, mrunning ) ) );
    mrmap.insert( rmap_t::value_type( mrunning, s ) );
    if ( offset!=-1 && mrunning!=offset ) std::cerr << "index::add() offset mismatch: " << s << std::endl;
    mrunning += bytes;
} 


void file_index::check_for_duplicates( std::string& s, int& counter ) { 
    size_t pos = s.find_last_of(";");
    if ( counter==0 && pos!=std::string::npos ) check_for_duplicates( s.erase(pos), counter );      
    else if ( mmap.find(s)!=mmap.end() ) { 
      if ( counter==0 ) s += ";"+std::to_string(++counter);
      else s.replace( pos+1, s.size(), std::to_string(++counter) );
      check_for_duplicates( s, counter );
    }
}


#if 0
/// different treatment of duplicated keys and whether ";" is 
/// allowed in the names
void check_for_duplicates( std::string& s, int& counter ) { 
    if ( mmap.find(s)!=mmap.end() ) { 
      size_t pos = s.find_last_of(";");
      if ( pos==std::string::npos ) s += ";"+std::to_string(++counter);
      else s.replace( pos+1, s.size(), std::to_string(++counter) );
      check_for_duplicates( s, counter );
    }
}
  
#endif


/// serialisation and deserialisation ...

void file_index::serialise_internal( std::vector<SB::TYPE>& s ) const {  

    SB::serialise( s, name() );

    s.push_back( mkeys.size() );

    for ( size_t i=0 ; i<mkeys.size() ; i++ ) { 
      SB::serialise( s, mkeys[i] );
      entry e = find( mkeys[i] );
      s.push_back( e.size );
      s.push_back( e.offset );
    }

}


void file_index::deserialise_internal( std::vector<SB::TYPE>::const_iterator& itr ) { 

    SB::deserialise( itr, mname );

    size_t nkeys = (*itr++);

    for ( size_t i=0 ; i<nkeys ; i++ ) {
      std::string key;
      SB::deserialise( itr, key );
      SB::TYPE esize  = (*itr++);
      SB::TYPE offset = (*itr++);
      add( key, esize, offset, -1 );
    }
      
} 









