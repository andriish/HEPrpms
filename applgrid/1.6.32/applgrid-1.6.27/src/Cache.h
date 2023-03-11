/** emacs: this is -*- c++ -*- **/
/**
 **     @file    Cache.h
 **
 **     @brief   evaluate a pdf function and store values in a cache.
 **              if this x, Q2 node has been requested before then 
 **              retrieve the values and return them if not, generate 
 **              and then add to the cache and then retiurn them
 **
 **              the new LHAPDF (version 6) may implement something 
 **              like this internally, but a retieval from the cache 
 **              is still around 20-40 times faster than LHAPDF 6                   
 **
 **     @author  mark sutton
 **     @date    Sat 19 Oct 2013 14:12:42 CEST 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: Cache.h, v0.0   Sat 19 Oct 2013 14:12:42 CEST sutt $
 **
 **/


#pragma  once

#include <iostream>
#include <map>
#include <utility>

/// Pass an LHAPDF (version 5) function pointer into the cache, 
/// then call using the evaluate() method instead of the calling 
/// the pdf directly. 


#include "pthread.h"


template<typename T>
class Cache : public std::map<T, std::vector<double> > {
 
private: 

  /// actual type def of the map for convenience
  typedef std::map<T,std::vector<double> > _map;

  /// function pointer type
  typedef void (*pdffunction)(const double& , const double&, double* );

  /// for fast copy 
  struct partons { double p[14]; };

public:

  /// give it the pdf function to use
  Cache( pdffunction pdf=0, unsigned mx=20000 ) : _pdf(pdf), _max(mx), _ncalls(0), _ncached(0), _reset(0), _disabled(false), _printstats(false) {
#   ifdef PDFTHREAD
    pthread_mutex_t tmpcache_mux = PTHREAD_MUTEX_INITIALIZER;
    cache_mux = tmpcache_mux;
#   endif
  } 
  
  virtual ~Cache() { } 

  
  /// evaluate the pdf function
  /// if this node has been requested before, retrieve values 
  /// from the cache, if not, generate and then add to cache
  /// for next time 
  void evaluate( const double& x, const double& Q2, double*  xf ) { 
    
    if ( _pdf==0 ) { 
      /// should really throw an exception here
      std::cerr << "whoops, pdf cache has no pdf!!" << std::endl; 
      return; 
    }

    _ncalls++;

    /// if we don't want to use the cache for some reason (it can get quite large)     
    if ( _disabled ) { 
      //  lock_cache();
      _pdf( x, Q2, xf ); 
      //   unlock_cache();
      return;
    }

    //    _reset++;

    T t(x, Q2);

    /// find out if this x, Q2 node is in the cache ...
       
    typename _map::const_iterator itr = this->find( t );

    
    if ( itr!=this->end() ) { 
      /// in the cache, simply copy to output ...
      //  lock_cache();
      (*(partons*)xf) = (*(partons*)(&itr->second[0])); 
      _ncached++;
      //  unlock_cache();
    } 
    else { 
      /// not in cache, call pdf function 
      std::vector<double> _xf(14,0);

      _pdf( x, Q2, &_xf[0] ); 

      /// add to cache if enough room
      //    lock_cache();
      if ( this->size()<_max ) this->insert( typename _map::value_type( t, _xf ) );

      /// copy to output 
      (*(partons*)xf) = ( *((partons*)&_xf[0]) ); 
      //  unlock_cache();
    }


    //    static int i=0; 

    //    i++;

    //    if ( i>10000 ) { 
    //      std::cout << "(Cache stats : called " << ncalls() << "\tcached " << ncached() << std::endl;
    //      i=0;
    //    }
  }
  

  void evaluate( const double& x, const double& Q2, std::vector<double>&  xf ) { evaluate( x, Q2, &xf[0] ); } 


  /// print some useful stats
  void stats() const { if (_printstats) std::cout << *this << std::endl; }

  /// return the actual stored pdf - very handy 
  pdffunction pdf() { return _pdf; }

  unsigned max()        const { return _max; } 
  unsigned ncalls()     const { return _ncalls; } 
  unsigned ncached()    const { return _ncached; } 
  unsigned ngenerated() const { return _ncalls-_ncached; } 

  double   fraction()  const { 
    if ( _ncalls>0 ) return this->size()*1.0/_ncalls; 
    return 0;
  }

  void reset() { this->clear(); _reset=_ncalls=_ncached=0; }

  void disable() { _disabled = true; }
  void enable()  { _disabled = false; }


private:

  //  void (*_pdf)(const double& , const double&, double* );
  pdffunction _pdf;

  /// maximum cache size
  unsigned _max;

  /// some stats - how many nodes generated and how many from the cache
  unsigned _ncalls;
  unsigned _ncached;
  unsigned _ngenerated;

  unsigned _reset;

  bool     _disabled;

  bool     _printstats;

# ifdef PDFTHREAD
  pthread_mutex_t cache_mux;
  void lock_cache()   {  pthread_mutex_lock(&cache_mux); }
  void unlock_cache() {  pthread_mutex_unlock(&cache_mux); }
#else
  void lock_cache()   {  }
  void unlock_cache() {  }
# endif

  
};




template<typename T>
inline std::ostream& operator<<( std::ostream& s, const Cache<T>& _c ) { 
  s << "Cache:: " 
    << "\tgenerated "  << _c.ngenerated() 
    << "\tfrom cache " << _c.ncached() 
    << "\tsize "       << _c.size() <<  " ( " << int(_c.fraction()*1000)*0.1 << "% )\t"
    << " :maximum "    << _c.max();
    return s;
}

/// useful typdef
typedef Cache<std::pair<double,double> > NodeCache;









