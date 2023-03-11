/* emacs: this is -*- c++ -*- */
/**
 **   @file    appl_file.h        
 **                   
 **   @author  sutt
 **   @date    Wed 30 Dec 2020 15:25:42 GMT
 **
 **   $Id: appl_file.h, v0.0   Wed 30 Dec 2020 15:25:42 GMT sutt $
 **
 **   Copyright (C) 2020 sutt (sutt@cern.ch)    
 **
 **/


#ifndef  APPL_FILE_H
#define  APPL_FILE_H

#include <iostream>

#include "appl_grid/serialise_base.h"
#include "appl_grid/stream_vector.h"
#include "appl_grid/file_index.h"

// #include <filesystem>

namespace appl {

class file { 

public: 

  file( const std::string& name, const std::string& option="rb" ); 

  ~file();

  void Close();

  void close() { Close(); }

  /// accessors

  std::string filename() const { return mfilename; }

  bool isopen() const { return mopen; }

  ///  provide the index

  const file_index& index() const { return mindex; } 

  /// object writing ...

  template<typename T>
  void Write( const T& t ) {
    if ( !isopen() ) return;
    std::vector<SB::TYPE> v = t.serialise();
    size_t bytes = gzwrite( mfile, (void*)(&v[0]), sizeof(SB::TYPE)*v.size() );
    if ( bytes!=sizeof(SB::TYPE)*v.size() ) std::cerr << "issue writing object " << t.name() << std::endl;
    msize += bytes;
    mindex.add( t.name(), bytes );
  }


  template<typename T>
  void Write( const std::string& name, const std::vector<T>& t ) {
    stream_vector<T> sv( name, t );
    Write( sv );
  }

  /// object reading ...

  template<typename T>
  void Read( std::vector<T>& v ) {
    if ( !isopen() ) return; 
    double vsize = 0;
    double Tsize = 0;
    gzread( mfile, &vsize, sizeof(SB::TYPE) );
    gzread( mfile, &Tsize, sizeof(SB::TYPE) );
    v.resize(vsize);
    gzread( mfile, &v[0], Tsize*vsize );
  }



  template<typename T>
  void Read( T& t ) { 
    if ( !isopen() ) return;
    std::vector<SB::TYPE> v(2);
    gzread( mfile, &v[0], sizeof(SB::TYPE)*2 );
    v.resize( v[1] );
    size_t bytes = gzread( mfile, &v[2], sizeof(SB::TYPE)*(v[1]-2) );
    if ( bytes!=(sizeof(SB::TYPE)*(v[1]-2)) ) std::cerr << "issue reading object " << t.name() << std::endl;
    t = T(v);
  }


  template<typename T>
  T Read() {
    T t; 
    Read(t);
    return t;
  }

  template<typename T> 
  T Read( const std::string& name ) {
    return Get<T>(name);
  }

  /// non-sequential access using the index ...
  
  template<typename T> 
  T Get( const std::string& name ) {
    if ( isopen() ) { 
      file_index::entry e = mindex.find( name );
      if ( e.size>0 ) { 
	// gzrewind( mfile );
	gzseek( mfile, e.offset, SEEK_SET );
	return Read<T>();
      }
    }    
    std::cerr << "WARNING: could not retrieve object: " << name << std::endl; 
    return T();
  }


private:

  /// filename
  std::string mfilename;

  /// options, read write, etc
  std::string mopt;
  
  /// the actual file
  gzFile      mfile;

  /// is is open ?
  bool        mopen;

  //the 
  size_t      msize;

  /// file index 
  file_index  mindex;
  
};

}


inline std::ostream& operator<<( std::ostream& s, const appl::file& t ) { 
  s << "file: " << t.filename() << "\t";
  if ( !t.isopen() ) s <<  " file is not open";
  else s << t.index();
  return s;
}


#endif  // APPL_FILE_H 










