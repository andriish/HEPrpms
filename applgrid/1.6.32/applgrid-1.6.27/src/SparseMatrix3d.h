/** emacs: this is -*- c++ -*- **/
/**
 **     @file    SparseMatrix3d.h
 **
 **     @author  mark sutton
 **     @date    Wed Nov 14 14:23:49 GMT 2007 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: SparseMatrix3d.h, v1.0   Wed Nov 14 14:23:49 GMT 2007 sutt $
 **
 **/


#ifndef __SPARSEMATRIX3D_H
#define __SPARSEMATRIX3D_H

#include <iostream>

#include "appl_grid/appl_root.h"

#include "axis.h"
#include "sparse.h"

#ifdef USEROOT
#include "TH1D.h"
#include "TH3D.h"
#endif



#include "appl_grid/stream_grid.h"


class SparseMatrix3d : public tsparse3d<double> {

public:

  // constructors and destructor

  SparseMatrix3d( int Nx, double lx, double ux, 
		  int Ny, double ly, double uy, 
		  int Nz, double lz, double uz);

  SparseMatrix3d(const SparseMatrix3d& s); 
  
#ifdef USEROOT
  SparseMatrix3d(const TH3D* h);   
#endif

  SparseMatrix3d(const stream_grid& h);   

  ~SparseMatrix3d();
  

  // axis accessors
  const axis<double>& xaxis() const { return m_xaxis; } 
  const axis<double>& yaxis() const { return m_yaxis; } 
  const axis<double>& zaxis() const { return m_zaxis; } 


  // trim to sparse structure 
  void trim() { empty_fast(); sparse3d::trim(); }
    
  // set up fast lookup table into the (untrimmed) 3d array.
  void setup_fast() { 
    if ( m_fastindex ) return;
    m_fastindex = new double*[Nx()*Ny()*Nz()];

    for ( int i=0 ; i<Nx() ; i++ ) { 
      for ( int j=0 ; j<Ny() ; j++ ) { 
	for ( int k=0 ; k<Nz() ; k++ ) { 
	  m_fastindex[(i*Ny()+j)*Nz()+k] = &(m_v[i]->v()[j])->v()[k];
	}
      }
    }
  }
  
  // and clean up
  void empty_fast() { 
    if ( m_fastindex ) delete[] m_fastindex;
    m_fastindex = NULL;
  }

  // access using the fast (dangerous) methods
  double& fill_fast(int i, int j, int k)       { return *m_fastindex[(i*Ny()+j)*Nz()+k]; }
  double  fill_fast(int i, int j, int k) const { return *m_fastindex[(i*Ny()+j)*Nz()+k]; }

  void fill(double x, double y, double z, double w) { 

    int i=xaxis().bin(x);
    int j=yaxis().bin(y);
    int k=zaxis().bin(z);

    if ( i<0 || i>=Nx() || j<0 || j>=Ny() || k<0 || k>=Nz() ) return; 
   
    if ( m_fastindex ) fill_fast(i,j,k) += w; 
    else                 (*this)(i,j,k) += w;
  }
  

  /// print out
  void print() const {
    //sparse3d::print();
    std::cout << m_xaxis << "\n"; 
    std::cout << m_yaxis << "\n"; 
    std::cout << m_zaxis << "\n"; 
  }

  
  void print_axes() const { 
    std::cout << "SparseMatrix: x: " 
	      << "\tx " << m_xaxis 
	      << "\ty " << m_yaxis 
	      << "\tz " << m_zaxis << std::endl; 
  }


  /// check if the axes are all the same
  bool compare_axes(const SparseMatrix3d& s) const { 

#if 0

    std::cout << "\tSparseMatrix: " 
	      << "\tx " << m_xaxis 
	      << "\ty " << m_yaxis 
	      << "\tz " << m_zaxis << std::endl; 

    std::cout << "\tSparseMatrix: " 
	      << "\tx " << s.m_xaxis 
	      << "\ty " << s.m_yaxis 
	      << "\tz " << s.m_zaxis << std::endl; 

    std::cout << "\tSparseMatrix: " 
	      << "\tx " << ( m_xaxis == s.m_xaxis ) 
	      << "\ty " << ( m_yaxis == s.m_yaxis )
	      << "\tz " << ( m_zaxis == s.m_zaxis ) << std::endl; 


    std::cout << "\tSparseMatrix: " 
	      << "\tdNx " << ( m_xaxis.N() - s.m_xaxis.N() ) 
	      << "\tmin "   << m_xaxis.min()   << " (" << ( m_xaxis.min() - s.m_xaxis.min() )     << ") " 
	      << "\tmax "   << m_xaxis.max()   << " (" << ( m_xaxis.max() - s.m_xaxis.max() )    << ") " 
	      << "\tdelta " << m_xaxis.delta() << " (" << ( m_xaxis.delta() - s.m_xaxis.delta() ) << ") " 
	      << std::endl; 

#endif

    return ( m_xaxis == s.m_xaxis &&  
	     m_yaxis == s.m_yaxis &&
	     m_zaxis == s.m_zaxis );
   
  }


  /// checks if the actual contents are the same
  bool operator==(const SparseMatrix3d& s) const;

  bool operator!=(const SparseMatrix3d& s) const { 
    return !( *this == s );
  }


  // utilities for file access and storage
#ifdef USEROOT
  TH3D*    getTH3D(const std::string& s) const; 
#endif

  stream_grid* get(const std::string& s) const; 

private:

  axis<double> m_xaxis;
  axis<double> m_yaxis;
  axis<double> m_zaxis;
  
  double** m_fastindex;

};


std::ostream& operator<<(std::ostream& s, const SparseMatrix3d& sm); 

#endif  // __SPARSEMATRIX3D_H 










