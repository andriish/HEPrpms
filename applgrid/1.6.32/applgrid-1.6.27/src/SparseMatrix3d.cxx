/**
 **     @file    SparseMatrix3d.cxx
 **
 **     @author  mark sutton
 **     @date    Fri Nov 23 00:49:26 GMT 2007 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: SparseMatrix3d.cxx, v1.0   Fri Nov 23 00:49:26 GMT 2007 sutt $
 **
 **/


#include "SparseMatrix3d.h"

#include "appl_grid/stream_grid.h"


SparseMatrix3d::SparseMatrix3d( int Nx, double lx, double ux, 
				int Ny, double ly, double uy, 
				int Nz, double lz, double uz) :
  sparse3d(Nx, Ny, Nz), 
  m_xaxis(Nx, lx, ux),
  m_yaxis(Ny, ly, uy),
  m_zaxis(Nz, lz, uz),
  m_fastindex(NULL) { 
  setup_fast();
} 


SparseMatrix3d::SparseMatrix3d(const SparseMatrix3d& s) : 
  // sparse3d(*(sparse3d*)(&s)), 
  sparse3d(s), 
  m_xaxis(s.m_xaxis),
  m_yaxis(s.m_yaxis),
  m_zaxis(s.m_zaxis),
  m_fastindex(NULL) { 
  setup_fast();
} 


#ifdef USEROOT 
SparseMatrix3d::SparseMatrix3d(const TH3D* h) : 
  sparse3d( h->GetNbinsX(), h->GetNbinsY(), h->GetNbinsZ() ),
  m_fastindex(NULL) {
  
  TH1D* hx = (TH1D*)h->Project3D("x");
  TH1D* hy = (TH1D*)h->Project3D("y");
  TH1D* hz = (TH1D*)h->Project3D("z");
  
  m_xaxis = axis<double>(hx->GetNbinsX(), hx->GetBinCenter(1),  hx->GetBinCenter(hx->GetNbinsX() ) ); 
  m_yaxis = axis<double>(hy->GetNbinsX(), hy->GetBinCenter(1),  hy->GetBinCenter(hy->GetNbinsX() ) ); 
  m_zaxis = axis<double>(hz->GetNbinsX(), hz->GetBinCenter(1),  hz->GetBinCenter(hz->GetNbinsX() ) ); 
  
  delete hx;
  delete hy;
  delete hz;
  
  for ( int i=0 ; i<m_xaxis.N() ; i++ ) { 
    for ( int j=0 ; j<m_yaxis.N() ; j++ ) { 
      for ( int k=0 ; k<m_zaxis.N() ; k++ ) (*this)(i,j,k) = h->GetBinContent(i+1,j+1,k+1);
    }  
  }
  setup_fast();
}
#endif




 
SparseMatrix3d::SparseMatrix3d(const stream_grid& h) :
  sparse3d( h.xaxis().size(), h.yaxis().size(), h.zaxis().size() ), 
  m_fastindex(NULL) {
  
  m_xaxis = axis<double>( h.xaxis().size(), h.xaxis(0),  h.xaxis(h.xaxis().size()-1) );
  m_yaxis = axis<double>( h.yaxis().size(), h.yaxis(0),  h.yaxis(h.yaxis().size()-1) );
  m_zaxis = axis<double>( h.zaxis().size(), h.zaxis(0),  h.zaxis(h.zaxis().size()-1) );
  
  for ( int i=0 ; i<m_xaxis.N() ; i++ ) { 
    for ( int j=0 ; j<m_yaxis.N() ; j++ ) { 
      for ( int k=0 ; k<m_zaxis.N() ; k++ ) (*this)(i,j,k) = h(i,j,k);
    }  
  }

  setup_fast();
}





SparseMatrix3d::~SparseMatrix3d() { empty_fast(); } 
  

/// check if the actual contents are the same
bool SparseMatrix3d::operator==(const SparseMatrix3d& s) const { 

  /// first if the axes are different, cannot be the same
  if ( !compare_axes( s ) ) return false;

  int nvalue = 0;

  /// now do a deep comparison ...
  for ( int i=0 ; i<m_xaxis.N() ; i++ ) { 
    for ( int j=0 ; j<m_yaxis.N() ; j++ ) { 
      for ( int k=0 ; k<m_zaxis.N() ; k++ ) { 

	//	std::cout << i << " " << j << " " << k
	//		  << " : " << (*this)(i,j,k) 
	//		  << " " << s(i,j,k)  << "  : " << ((*this)(i,j,k)-s(i,j,k) ) << std::endl;

	if ( (*this)(i,j,k) != s(i,j,k) ) return false;
	if ( (*this)(i,j,k)!=0 ) nvalue++;
      }  
    }  
  }

  return true;
}



#ifdef USEROOT
// utilities for file access and storage

TH3D* SparseMatrix3d::getTH3D(const std::string& s) const { 
  
  double delx = xaxis().delta();
  double dely = yaxis().delta();
  double delz = zaxis().delta();
  
  TH3D* h=new TH3D(s.c_str(), s.c_str(), 
		   xaxis().N(),  xaxis().min()-0.5*delx, xaxis().max()+0.5*delx, 
		   yaxis().N(),  yaxis().min()-0.5*dely, yaxis().max()+0.5*dely, 
		   zaxis().N(),  zaxis().min()-0.5*delz, zaxis().max()+0.5*delz);
  
  const SparseMatrix3d& sm = (*this);
  
  int N=0;
  
  // some printout ???
  //  print();

  for ( int i=lo() ; i<=hi() ; i++ ) { 
    const sparse2d* s2d = sm[i]; 
    if ( s2d==NULL ) continue;
    for ( int j=s2d->lo() ; j<=s2d->hi() ; j++ ) { 
      const sparse1d* s1d = (*s2d)[j];
      if ( s1d==NULL ) continue;
      for ( int k=s1d->lo() ; k<=s1d->hi() ; k++ ) {
	N++;
	// std::cout << "\tsm(" << i << "\t, " << j << "\t, " << k << ")=" << (*s1d)(k) << std::endl; 
	// h->SetBinContent(i+1, j+1, k+1, sm(i,j,k) );
	h->SetBinContent(i+1, j+1, k+1, (*s1d)(k) );
      }
    }
  }
  
  //  std::cout << "SparseMatrix3d::getTH3D() s=" << s 
  //       << "\tN=" << N << "\tfilled bins" << std::endl;
  
  return h;
}
#endif



// utilities for file access and storage

stream_grid* SparseMatrix3d::get(const std::string& s) const { 

  stream_grid* weightgp = new stream_grid( s, xaxis().v(), yaxis().v(), zaxis().v() );
  
  stream_grid& weightg = *weightgp;
    
  const SparseMatrix3d& sm = (*this);
  
  int N=0;
  
  // some printout ???
  //  print();

  for ( int i=lo() ; i<=hi() ; i++ ) { 
    const sparse2d* s2d = sm[i]; 
    if ( s2d==NULL ) continue;
    for ( int j=s2d->lo() ; j<=s2d->hi() ; j++ ) { 
      const sparse1d* s1d = (*s2d)[j];
      if ( s1d==NULL ) continue;
      for ( int k=s1d->lo() ; k<=s1d->hi() ; k++ ) {
	N++;
	// std::cout << "\tsm(" << i << "\t, " << j << "\t, " << k << ")=" << (*s1d)(k) << std::endl; 
	// h->SetBinContent(i+1, j+1, k+1, sm(i,j,k) );
	weightg(i, j, k) =  (*s1d)(k);
      }
    }
  }
  
  //  std::cout << "SparseMatrix3d::getTH3D() s=" << s 
  //       << "\tN=" << N << "\tfilled bins" << std::endl;
  
  return weightgp;
}







std::ostream& operator<<(std::ostream& s, const SparseMatrix3d& sm) { 
  const tsparse3d<double>* sp = &sm;
  s << "x:" << sm.xaxis() << "\ny:" << sm.yaxis() << "\nz:" << sm.zaxis()
    << "\n" << *sp;
  return s;
}

