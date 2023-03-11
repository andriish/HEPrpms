/* emacs: this is -*- c++ -*- */
/**
 **   @file    file_index.h        
 **                   
 **   @author  sutt
 **   @date    Thu 31 Dec 2020 14:42:12 GMT
 **
 **   $Id: file_index.h, v0.0   Thu 31 Dec 2020 14:42:12 GMT sutt $
 **
 **   Copyright (C) 2020 sutt (sutt@cern.ch)    
 **
 **/


#ifndef  FILE_INDEX_H
#define  FILE_INDEX_H

#include <iostream>
#include <vector>
#include <map>
#include <string>

#include "appl_grid/serialise_base.h"
#include "appl_grid/serialisable.h"


class file_index : public serialisable {

public: 

  // histogram error exception
  class exception : public std::exception { 
  public:
    exception(const std::string& s) { std::cerr << what() << " " << s << std::endl; }; 
    virtual const char* what() const throw() { return ""; }
  };

  struct entry {
    entry( int sz=0, int off=0 ) : size(sz), offset(off) { } 
    double size; 
    double offset;
  };

  typedef std::map<std::string, entry> map_t;
  typedef std::map<int, std::string>   rmap_t;
 
public:

  file_index();

  virtual ~file_index();

  std::string name() const { return mname; }

  size_t size() const { return mkeys.size(); }

  std::vector<std::string>::const_iterator begin() const { return mkeys.begin(); }
  std::vector<std::string>::const_iterator   end() const { return mkeys.end(); }

  entry find( const std::string& s ) const { 
    map_t::const_iterator itr = mmap.find( s ); 
    if ( itr!=mmap.end() ) return itr->second; 
    return entry();
  }
  
  void clear() { 
    mkeys.clear();
    mmap.clear();
    mrmap.clear();
  }

  void add( const std::string& _s, int bytes, int offset=-1, int counter=0 );

protected:

  void check_for_duplicates( std::string& s, int& counter );

  /// serialisation and deserialisation ...

  virtual void serialise_internal( std::vector<SB::TYPE>& s ) const;
  
  virtual void deserialise_internal( std::vector<SB::TYPE>::const_iterator& itr );

private:
  
  std::string mname;

  std::vector<std::string> mkeys;

  map_t  mmap;
  rmap_t mrmap;

  int    mrunning;

};


inline std::ostream& operator<<( std::ostream& s, const file_index::entry& e ) { 
  std::string space = "";
  if ( e.offset<0x10000 ) space += "\t"; 
  return s << "index: 0x" << std::hex << int(e.offset) << std::dec << space << "  size: " << int(e.size);
}


inline std::ostream& operator<<( std::ostream& s, const file_index& f ) { 
  s << "file index: " << f.size() << " keys\n";
  for ( std::vector<std::string>::const_iterator itr=f.begin() ; itr!=f.end() ; itr++ ) { 
    std::string space = "";
    if ( itr->size()<31 ) space += "\t"; 
    if ( itr->size()<23 ) space += "\t"; 
    if ( itr->size()<15 ) space += "\t"; 
    if ( itr->size()<7 )  space += "\t"; 
    s << "    Key: " << *itr << space << "  " << f.find(*itr) << "\n"; 
  } 
  return s;
}


#endif  // FILE_INDEX_H 










