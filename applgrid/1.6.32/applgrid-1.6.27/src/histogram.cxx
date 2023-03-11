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


#include "appl_grid/histogram.h"

#include "appl_grid/serialise_base.h"



histogram::histogram( const std::string& name ) : mname(name) { } 


histogram::histogram( const std::string& name, size_t nbins, const double* limits ) : mname(name) {
  if ( nbins>0 ) create( nbins, limits );
  else throw exception("histogram: not enough bins creating histogram: "+name);
}


histogram::histogram( const std::string& name, const std::vector<double>& limits ) : mname(name) {
    if ( limits.size()>1 ) create( limits.size()-1, &limits[0] );
    else throw exception("histogram: not enough bin limits creating histogram: "+name);
}


histogram::histogram( const std::string& name, size_t nbins, double lo, double hi ) : mname(name) {    
    if ( nbins==0 ) throw exception("histogram: not enough bins creating histogram: "+name);
    std::vector<double> limits(nbins+1,0);
    for ( size_t i=nbins ; i-- ; ) limits[i] = (lo*(nbins-i) + hi*i)/nbins;
    /// make sure the limits at least are exact 
    limits[0]     = lo;
    limits[nbins] = hi;
    create( nbins, &limits[0] );
} 


/// constructor from stream

histogram::histogram( const std::vector<SB::TYPE> stream ) { 
    deserialise( stream );
    mx.resize(my.size());
    for ( size_t i=my.size() ; i-- ; ) mx[i] = 0.5*(mxlimits[i]+mxlimits[i+1]);
}


histogram::histogram( const histogram& h ) : 
    mname(h.mname),
    mxlimits(h.mxlimits),
    mx(h.mx),
    my(h.my),
    mye(h.mye), 
    myelo(h.myelo) 
{ }


/// merge bins i and bin i+1

void histogram::merge_bins( size_t i, bool norm ) {
 
    //    std::cout << "before:\n" << *this << std::endl;

    size_t n = my.size();

    if ( n<2 || i>=n-1 ) { 
      std::cerr << "app::grid::merge_bins() cannot merge" << std::endl;
      return; 
    }

    int j=i+1;

    double w0 = mxlimits[i+1]-mxlimits[i];
    double w1 = mxlimits[j+1]-mxlimits[j];
    double w  = mxlimits[j+1]-mxlimits[i];

    if ( !norm ) w0 = w1 = w = 1;

    mx[i]  = 0.5*(mxlimits[j+1] + mxlimits[i]);
    my[i]  = (my[i]*w0 + my[j]*w1)/w;
    mye[i] = std::sqrt( w0*w0*mye[i]*mye[i] + w1*w1*mye[j]*mye[j] )/w;

    mxlimits.erase( mxlimits.begin()+j );
    mx.erase( mx.begin()+j );
    my.erase( my.begin()+j );
    mye.erase( mye.begin()+j );

    if ( myelo.size()>0 ) { 
      myelo[i] = std::sqrt( w0*w0*myelo[i]*myelo[i] + w1*w1*myelo[j]*myelo[j] )/w;
      myelo.erase( myelo.begin()+j );
    }

    //    std::cout << "after:\n" << *this << std::endl;

}


#if 0
void histogram::merge_bin( int i ) {
 
    std::cout << "before:\n" << *this << std::endl;

    size_t n  = my.size();
    size_t nn = my.size()-1;


    double w0 = mxlimits[n-1]-mxlimits[n-2];
    double w1 = mxlimits[n]-mxlimits[n-1];

    double w = mxlimits[n]-mxlimits[n-2];

    if ( n<2 ) return;

    mxlimits[nn] = mxlimits[n];
    mxlimits.resize(n);

    mx[nn-1] = 0.5*(mxlimits[nn] - mxlimits[nn-1]);

    mx.resize(nn);
    
    my[nn-1] = (my[nn-1]*w0 + my[n-1]*w1)/w;
    my.resize(nn);

    mye[nn-1] = std::sqrt( w0*w0*mye[nn-1]*mye[nn-1] + w1*w1*mye[n-1]*mye[n-1] )/w;
    mye.resize(nn);

    if ( myelo.size()>0 ) { 
      myelo[nn-1] = std::sqrt( w0*w0*myelo[nn-1]*myelo[nn-1] + w1*w1*myelo[n-1]*myelo[n-1] )/w;
      myelo.resize(nn);
    }

    std::cout << "after:\n" << *this << std::endl;

}
#endif


histogram& histogram::operator=( histogram&& h ) {
    std::swap( mname, h.mname );
  
    std::swap( mxlimits, h.mxlimits );
    std::swap( mx, h.mx );

    std::swap( my,    h.my );
    std::swap( mye,   h.mye );
    std::swap( myelo, h.myelo );
    
    return *this;
}


histogram& histogram::operator=( const histogram& h ) {
    mname    = h.mname;
    mxlimits = h.mxlimits;
    mx       = h.mx;
    my       = h.my;
    mye      = h.mye;
    myelo    = h.myelo;
    return *this;
}


void histogram::fill( double x, double w ) {
    int ibin = index( x );
    if ( ibin != -1 ) { 
      my[ibin] += w;
      mye[ibin] = std::sqrt( mye[ibin]*mye[ibin] + w );
    } 
    if ( myelo.size()>0 ) myelo[ibin] = std::sqrt( myelo[ibin]*mye[ibin] + w );
}
  

/// serialisation and deserialisation ...

void histogram::serialise_internal( std::vector<SB::TYPE>& s ) const {    
    SB::serialise( s, name() );
    SB::serialise( s, mxlimits );
    SB::serialise( s, my );
    std::vector<double> ye(mye);
    if ( myelo.size()>0 ) ye.insert( ye.end(), myelo.begin(), myelo.end() );
    SB::serialise( s, ye );
}


void histogram::deserialise_internal( std::vector<SB::TYPE>::const_iterator& itr ) { 
    SB::deserialise( itr, mname );
    SB::deserialise( itr, mxlimits );
    SB::deserialise( itr, my );
    std::vector<double> ye;
    SB::deserialise( itr, ye );
    if ( ye.size()==my.size() ) mye = ye;
    else { 
      mye.clear();
      mye.insert( mye.begin(), ye.begin(), ye.begin()+(ye.size()/2) ); 
      myelo.insert( myelo.begin(), ye.begin()+(ye.size()/2), ye.end() ); 
    }
} 



void histogram::create( size_t nbins, const double* limits ) { 
    
    mxlimits.resize(nbins+1);
    
    for ( size_t i=nbins+1 ; i-- ; ) mxlimits[i] = limits[i];
    
    mx.resize(nbins);
    
    for ( size_t i=nbins ; i-- ; ) mx[i] = 0.5*(limits[i]+limits[i+1]);
    
    my  = std::vector<double>(nbins,0);
    mye = std::vector<double>(nbins,0);
   
}



void histogram::set( const std::vector<double>& v, 
		     const std::vector<double>& ve,
		     const std::vector<double>& velo ) 
{ 
    if ( v.size()!=size() ) throw exception( "histogram: number of histogram and value bins don't match" ); 
    my = v;
    
    if ( ve.size()!=0 ) { 
      if ( ve.size()!=size() ) throw exception( "histogram: number of histogram and value bins don't match" ); 
      mye = ve;      
    }
    else mye = std::vector<double>(v.size(),0);

    if ( velo.size()!=0 ) { 
      if ( velo.size()!=size() ) throw exception( "histogram: number of histogram and value bins don't match" ); 
      myelo = velo;      
    }
    else myelo.clear();
} 


void histogram::set_errors( const std::vector<double>& ve,
			    const std::vector<double>& velo ) 
{ 
    if ( ve.size()!=size() ) throw exception( "histogram: number of histogram and value bins don't match" ); 
    mye = ve;      
      
    if ( velo.size()!=0 ) { 
      if ( velo.size()!=size() ) throw exception( "histogram: number of histogram and value bins don't match" ); 
      myelo = velo;      
    }
    else myelo.clear();

}








