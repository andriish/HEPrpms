/** emacs: this is -*- c++ -*- **/
/**
 **   @file    histogram.h        
 **                   
 **   @author  sutt
 **   @date    Mon 28 Dec 2020 19:33:12 GMT
 **
 **   $Id: histogram.h, v0.0   Mon 28 Dec 2020 19:33:12 GMT sutt $
 **
 **   Copyright (C) 2020 sutt (sutt@cern.ch)    
 **
 **/


#ifndef  HISTOGRAM_H
#define  HISTOGRAM_H

#include <string>
#include <vector>
#include <cmath>
#include <iostream>
#include <exception>


#include "binary_search.h"
#include "serialise_base.h"
#include "appl_grid/serialisable.h"



class histogram : public serialisable {

public:

  // histogram error exception
  class exception : public std::exception { 
  public:
    exception(const std::string& s) { std::cerr << what() << " " << s << std::endl; }; 
    virtual const char* what() const throw() { return ""; }
  };

public:

  histogram( const std::string& name="" );

  histogram( const std::string& name, size_t nbins, const double* limits );

  histogram( const std::string& name, const std::vector<double>& limits );

  histogram( const std::string& name, size_t nbins, double lo, double hi );

  histogram( const std::vector<SB::TYPE> stream );

  histogram( const histogram& h );

  virtual ~histogram() { } 

  histogram& operator=( histogram&& h );

  histogram& operator=( const histogram& h );

  /// accessors 

  std::string name() const { return mname; }
  std::string& name()      { return mname; }

  size_t size() const { return mx.size(); }

  double  operator[]( int i ) const { return my.at(i); }
  double& operator[]( int i )       { return my[i]; }

  double&    y( int i ) { return my.at(i);  }
  double&   ye( int i ) { return mye.at(i); }
  double& yelo( int i ) { 
    if ( myelo.size()==0 ) myelo = std::vector<double>( mye.begin(), mye.end() );
    return myelo.at(i); 
  }

  double    y( int i ) const { return my.at(i);  }
  double   ye( int i ) const { return mye.at(i); }
  double yelo( int i ) const { 
    if ( myelo.size()==0 ) return mye.at(i);
    return myelo.at(i); 
  }

  void set( const std::vector<double>& v, 
	    const std::vector<double>& ve=std::vector<double>(),
	    const std::vector<double>& velo=std::vector<double>() );

  void set_errors( const std::vector<double>& ve,
		   const std::vector<double>& velo=std::vector<double>() );

  const std::vector<double>& y() const { return my;  }


  double lo( int i ) const { return mxlimits.at(i); }
  double hi( int i ) const { return mxlimits.at(i+1); }

  const std::vector<double>& xlimits() const { return mxlimits; } 
  std::vector<double>&       xlimits()       { return mxlimits; } 
  double&                    xlimit( int i)  { return mxlimits.at(i); } 

  /// logical operators - 
  /// NB: these only test on the BIN LIMITS, NOT THE BIN CONTENTS !!
  
  bool operator==( const histogram& h ) {
    if ( size()!=h.size() ) return false;
    if ( size()==0 ) return true;
    double delta = mxlimits[1]-mxlimits[0];
    for ( size_t i=mxlimits.size() ; i-- ; ) if ( std::fabs(mxlimits[i]-h.mxlimits[i])>delta*1e-10 ) return false;
    return true;
  }

  bool operator!=( const histogram& h ) { 
    return !(operator==(h));
  }

  /// mathematical operations ...

  histogram& operator*=( double d ) { 
    for ( size_t i=size() ; i-- ; ) { 
      my[i]  *= d;
      mye[i] *= d;
    }
    if ( myelo.size()>0 ) {
      for ( size_t i=size() ; i-- ; ) mye[i] *= d;
    }

    return *this;
  }

  histogram operator*( double d ) const {
    histogram h(*this);
    return (h*=d);
  }

  histogram& operator/=( double d ) {
    return operator*=(1/d);
  }

  histogram operator/( double d ) const {
    histogram h(*this);
    return h/=d;
  }

  histogram& operator/=( const histogram& h) {
    if ( ! operator==(h) )  throw exception( "histogram: bin mismatch for operator +- " );
    for ( size_t i=size() ; i-- ; ) { 
      my[i]  /= h.my[i];
      mye[i]  = std::sqrt( h.mye[i]*h.mye[i] + mye[i]*mye[i] );
    }
    if ( myelo.size()>0 ) {  
      for ( size_t i=size() ; i-- ; ) { 
	myelo[i]  = std::sqrt( h.myelo[i]*h.myelo[i] + myelo[i]*myelo[i] );
      }
    }
    return *this;
  }
  
  histogram operator/( const histogram& h) const {
    histogram hr(*this);
    return hr/=h;
  }


  histogram& operator+=( const histogram& h ) { 
    if ( ! operator==(h) )  throw exception( "histogram: bin mismatch for operator +- " );
    for ( size_t i=size() ; i-- ; ) { 
      my[i]  += h.my[i];
      mye[i]  = std::sqrt( h.mye[i]*h.mye[i] + mye[i]*mye[i] );
    }
    if ( myelo.size()>0 ) {  
      for ( size_t i=size() ; i-- ; ) { 
	myelo[i]  = std::sqrt( h.myelo[i]*h.myelo[i] + myelo[i]*myelo[i] );
      }
    }
    return *this;
  }

  histogram operator+( histogram h ) const { 
    return ( h += (*this) );
  }


  histogram operator-=( const histogram& h ) { 
    return operator+=( h*(-1) );
  }

  histogram operator-( const histogram& h ) const { 
    histogram hi(*this);
    return hi-=h;
  }
  

  /// filling ...

  int index( double x ) const { return find_index( x, mxlimits ); }

  void fill( double x, double w=1 );  

  /// serialisation and deserialisation ...

  ///  void serialise_internal( std::vector<SB::TYPE>& s ) const;

  ///  void deserialise_internal( const std::vector<SB::TYPE>& v );

  virtual void serialise_internal( std::vector<SB::TYPE>& s ) const; 
  virtual void deserialise_internal( std::vector<SB::TYPE>::const_iterator& itr );

  /// merge bins i and bin i+1
  void merge_bins( size_t i, bool norm=true );

protected:

  void create( size_t nbins, const double* limits );
 
protected:
  
  std::string mname;              ///! serialise_var

  std::vector<double> mxlimits;  ///! serialise_var
  
  std::vector<double> mx;        
  
  std::vector<double> my;        ///! serialise_var
  std::vector<double> mye;       ///! serialise_var
  std::vector<double> myelo;     /// possible lower limit errors for asymmetric

};


inline histogram operator*( double d, histogram h ) {
  return (h*=d);
}

  


inline std::ostream& operator<<( std::ostream& s, const histogram& h ) { 
  s << "key: " << h.name() << std::endl;
  for ( size_t i=0 ; i<h.size() ; i++ ) { 
    s << "  " << h.lo(i) << "\t- " << h.hi(i) 
      << "\t: " << h.y(i) << "\t+- " 
      << h.ye(i) << "\t( " << h.yelo(i) << " )";
    // if ( h.y(i)!=0 ) s << "\t( +- " << 100*h.ye(i)/h.y(i) << " % )";
    s << std::endl; 
  }
  return s;
}


#endif  // HISTOGRAM_H 










