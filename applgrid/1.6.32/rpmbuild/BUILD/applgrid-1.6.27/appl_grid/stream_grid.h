/* emacs: this is -*- c++ -*- */
/**
 **   @file    stream_grid.h        
 **                   
 **   @author  sutt
 **   @date    Wed 30 Dec 2020 18:06:03 GMT
 **
 **   $Id: stream_grid.h, v0.0   Wed 30 Dec 2020 18:06:03 GMT sutt $
 **
 **   Copyright (C) 2020 sutt (sutt@cern.ch)    
 **
 **/


#ifndef  STREAM_GRID_H
#define  STREAM_GRID_H

#include <iostream>
#include <vector>

#include "appl_grid/vector_stream.h"
#include "appl_grid/serialise_base.h"
#include "appl_grid/serialisable.h"



class stream_grid : public serialisable {

public:

  // grid error exception
  class exception : public std::exception { 
  public:
    exception(const std::string& s) { std::cerr << what() << " " << s << std::endl; }; 
    virtual const char* what() const throw() { return ""; }
  };

public:

  stream_grid() { }

  stream_grid( const std::string& name, 
	       const std::vector<double>& xv, 
	       const std::vector<double>& yv, 
	       const std::vector<double>& zv ) :
    mname(name),
    mxaxis( xv ), 
    myaxis( yv ), 
    mzaxis( zv ),
    mnz(zv.size()),  
    mnyz(zv.size()*yv.size())
  { 
    //    mnodes = std::vector<std::vector<std::vector<double> > >( xv.size(), std::vector<std::vector<double> >( yv.size(), std::vector<double>(zv.size(), 0 ) ) );
    mnodes = std::vector<double>(xv.size()*yv.size()*zv.size(), 0 );
  } 

  stream_grid( const stream_grid& sg ) :
    mname(sg.mname),
    mxaxis(sg.mxaxis),
    myaxis(sg.myaxis),
    mzaxis(sg.mzaxis),
    mnz(sg.mnz),
    mnyz(sg.mnyz),
    mnodes(sg.mnodes) 
  { } 


  stream_grid& operator=( stream_grid&& sg ) { 
    std::swap(mname,  sg.mname);
    std::swap(mxaxis, sg.mxaxis);
    std::swap(myaxis, sg.myaxis);
    std::swap(mzaxis, sg.mzaxis);
    std::swap(mnz,    sg.mnz);
    std::swap(mnyz,   sg.mnyz);
    std::swap(mnodes, sg.mnodes);
    return *this;
  }


  stream_grid( const std::vector<SB::TYPE>& v ) {
    deserialise( v );
    mnz  = mzaxis.size();
    mnyz = mzaxis.size()*myaxis.size();
  } 

  virtual ~stream_grid() { } 

  std::string name() const { return mname; }

  //  void set( int ix, int iy, int iz, double d ) { mnodes[ix+mnx*iy+mnxy*iz] = d; } 
  void set( int ix, int iy, int iz, double d ) { mnodes[ix*mnyz+iy*mnz+iz] = d; } 

  //  double& get( int ix, int iy, int iz )       { return mnodes[ix+mnx*iy+mnxy*iz]; } 
  //  double  get( int ix, int iy, int iz ) const { return mnodes[ix+mnx*iy+mnxy*iz]; } 

  //  double& operator()( int ix, int iy, int iz )       { return mnodes[ix+mnx*iy+mnxy*iz]; } 
  //  double  operator()( int ix, int iy, int iz ) const { return mnodes[ix+mnx*iy+mnxy*iz]; } 

  double& get( int ix, int iy, int iz )       { return mnodes[ix*mnyz+iy*mnz+iz]; }
  double  get( int ix, int iy, int iz ) const { return mnodes[ix*mnyz+iy*mnz+iz]; }

  double& operator()( int ix, int iy, int iz )       { return mnodes[ix*mnyz+iy*mnz+iz]; }
  double  operator()( int ix, int iy, int iz ) const { return mnodes[ix*mnyz+iy*mnz+iz]; }

  const std::vector<double>& xaxis() const { return mxaxis; }
  const std::vector<double>& yaxis() const { return myaxis; }
  const std::vector<double>& zaxis() const { return mzaxis; }

  const double& xaxis(int i) const { return mxaxis[i]; }
  const double& yaxis(int i) const { return myaxis[i]; }
  const double& zaxis(int i) const { return mzaxis[i]; }


  /// serialise and deserialise

  virtual void serialise_internal( std::vector<SB::TYPE>& s ) const { 
    SB::serialise( s, name() );
    SB::serialise( s, mxaxis );
    SB::serialise( s, myaxis );
    SB::serialise( s, mzaxis );
    SB::serialise( s, mnodes );
  }


  virtual void deserialise_internal( std::vector<SB::TYPE>::const_iterator& itr ) { 
    SB::deserialise( itr, mname );
    SB::deserialise( itr, mxaxis );
    SB::deserialise( itr, myaxis );
    SB::deserialise( itr, mzaxis );
    SB::deserialise( itr, mnodes );
  }


private:

  std::string mname;
  
  std::vector<double> mxaxis;
  std::vector<double> myaxis;
  std::vector<double> mzaxis;

  size_t mnz;
  size_t mnyz;

  std::vector<double> mnodes;

};


inline std::ostream& operator<<( std::ostream& s, const stream_grid& sg ) { 

  s << "[ x axis: " << sg.xaxis().size() << "\t" << sg.xaxis()[0] << " .. " << sg.xaxis()[sg.xaxis().size()-1] << " ]\n"; 
  s << "[ y axis: " << sg.yaxis().size() << "\t" << sg.yaxis()[0] << " .. " << sg.yaxis()[sg.yaxis().size()-1] << " ]\n"; 
  s << "[ z axis: " << sg.zaxis().size() << "\t" << sg.zaxis()[0] << " .. " << sg.zaxis()[sg.zaxis().size()-1] << " ]\n"; 


#if 0
"\n";
  s << "xaxis: " << sg.xaxis() << "\n";
  s << "yaxis: " << sg.yaxis() << "\n";
  s << "zaxis: " << sg.zaxis() << "\n";

  std::vector<double> x = sg.xaxis();
  std::vector<double> y = sg.yaxis();
  std::vector<double> z = sg.zaxis();

  for ( size_t iz=0 ; iz<z.size() ; iz++ ) { 
    for ( size_t iy=0 ; iy<y.size() ; iy++ ) { 
      for ( size_t ix=0 ; ix<x.size() ; ix++ ) { 
        double v = sg( ix, iy, iz );
	if ( v==0 ) s << "   ";
	else        s << int(3*v*1e-3) << " ";
      }
      std::cout << "\n";
    }
    std::cout << "\n";
  }
#endif

  return s;
}


#endif  // STREAM_GRID_H 










