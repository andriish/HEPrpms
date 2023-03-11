/* emacs: this is -*- c++ -*- */
/**
 **   @file    aTH1D.h
 **       
 **            wrapper around our own histogram class to make it look 
 **            and behave like the egregious root class        
 **                   
 **   @author  sutt
 **   @date    Wed 30 Dec 2020 21:12:51 GMT
 **
 **   $Id: aTH1D.h, v0.0   Wed 30 Dec 2020 21:12:51 GMT sutt $
 **
 **   Copyright (C) 2020 sutt (sutt@cern.ch)    
 **
 **/


#ifndef  ATH1D_H
#define  ATH1D_H

#include <iostream>
#include "histogram.h"

#ifdef USEROOT

#include "TH1D.h"
#include "TDirectory.h"
#endif

namespace appl { 
class TH1D;
}

#ifdef USEROOT
TH1D*       convert( const  appl::TH1D* h );
appl::TH1D* convert( const        TH1D* h );
#endif

namespace appl { 

class TH1D : public histogram {

public:

  TH1D() { }   

  TH1D( const histogram& h ) : histogram(h) { } 

  TH1D( const std::string& name, const std::string& title, int nbins, double lo, double hi ) : 
    histogram( name, nbins, lo, hi ), mtitle(title) { } 

  TH1D( const std::string& name, const std::string& title, int nbins, const double* limits ) : 
    histogram( name, nbins, limits ), mtitle(title) { } 

  virtual ~TH1D() { } 

  double GetBinContent( int i ) const { return my[i-1]; } 
  double   GetBinError( int i ) const { return mye[i-1]; } 

  void SetBinContent( int i, double d ) { my[i-1]=d; } 
  void   SetBinError( int i, double d ) { mye[i-1]=d; } 

  double GetBinLowEdge( int i ) const { return mxlimits[i-1]; }
  double  GetBinCenter( int i ) const { return mx[i-1]; }

  double GetBinWidth( int i ) const { return mxlimits[i]-mxlimits[i-1]; } 

  int GetNbinsX() const { return int(mx.size()); } 
  
  void Fill( double x, double w=1 ) { fill(x, w ); }    

  int FindBin( double x ) const { return index(x)+1; }

  void SetDirectory( int =0 ) const { } 

  TH1D* Clone( const std::string& n="" ) { 
    TH1D* h = new TH1D(*this);
    h->SetName( n );
    return h;
  }

  void Scale( double d ) { operator*=(d); }  

  void Add( const TH1D& h ) { operator+=(h); }

  void Sumw2() const { } 

  void        SetName( const std::string& n ) { mname=n; } 
  std::string GetName() const { return mname; } 

  std::string GetTitle() const { return mtitle; } 
  void        SetTitle( const std::string& s ) { mtitle=s; } 

  void Reset() { 
    size_t n = my.size();
    my  = std::vector<double>(n,0);
    mye = std::vector<double>(n,0);
  } 

#ifdef USEROOT
  void Write() const { 
    ::TH1D* h = convert( this );
    h->Write();
  } 
#endif

protected:

  std::string mtitle;
  
};

}

// convertors to and from root histograms

#ifdef USEROOT
inline appl::TH1D* convert( ::TH1D* h ) { 
  std::vector<double> limits(h->GetNbinsX()+1);
  for ( size_t i=0 ; i<limits.size() ; i++ ) limits[i] = h->GetBinLowEdge(i+1);
  appl::TH1D* hist = new appl::TH1D( h->GetName(), h->GetTitle(), limits.size()-1, &limits[0] );
  for ( size_t i=0 ; i<hist->size() ; i++ ) { 
    hist->y(i)  = h->GetBinContent(i+1);
    hist->ye(i) = h->GetBinError(i+1);
  }
  return hist;
}


inline ::TH1D* convert( const appl::TH1D* h ) { 
  ::TH1D* hist = new ::TH1D( h->name().c_str(), h->GetTitle().c_str(), h->size(), &(h->xlimits()[0]) );
  for ( size_t i=0 ; i<h->size() ; i++ ) { 
    hist->SetBinContent(i+1, h->y(i) ); 
    hist->SetBinError(i+1, h->ye(i) ); 
  }
  return hist;
}


inline ::TH1D* convert( const appl::TH1D& h ) { 
  ::TH1D* hist = new ::TH1D( h.name().c_str(), h.GetTitle().c_str(), h.size(), &(h.xlimits()[0]) );
  for ( size_t i=h.size() ; i-- ; ) { 
    hist->SetBinContent(i+1, h.y(i) ); 
    hist->SetBinError(i+1, h.ye(i) ); 
  }
  return hist;
}
#endif


inline std::ostream& operator<<( std::ostream& s, const appl::TH1D& a ) { 
  return s << *dynamic_cast<const histogram*>(&a);
}


typedef appl::TH1D aTH1D;


#endif  // ATH1D_H 










