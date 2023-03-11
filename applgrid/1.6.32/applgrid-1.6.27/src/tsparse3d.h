/** emacs: this is -*- c++ -*- **/
/**
 **     @file    tsparse3d.h
 **
 **     @brief   very basic 3d sparse matrix class, needs to be improved
 **              
 **
 **     @author  mark sutton
 **     @date    Fri Nov  9 00:17:31 CET 2007 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: tsparse3d.h, v   Fri Nov  9 00:17:31 CET 2007 sutt $
 **
 **/


#ifndef __TSPARSE3D_H
#define __TSPARSE3D_H

#include <iostream>

#include "tsparse2d.h"


template<class T> 
class tsparse3d : public tsparse_base {

public:

  tsparse3d(int nx, int ny, int nz) 
    : tsparse_base(nx), m_Ny(ny), m_Nz(nz), m_v(NULL), m_trimmed(false) {
    m_v = new tsparse2d<T>*[m_Nx];
    for ( int i=0 ; i<m_Nx ; i++ ) m_v[i] = new tsparse2d<T>(m_Ny, m_Nz);
    //   setup_fast();
  }

  // Fixme: need to rewrite this constructor properly, so that 
  // a trimmed matrix is copied as trimmed and not copied in full 
  // and then trimmed
  tsparse3d(const tsparse3d& t) 
    : tsparse_base(t.m_Nx), m_Ny(t.m_Ny), m_Nz(t.m_Nz), m_v(NULL), m_trimmed(t.m_trimmed) {
    m_v = new tsparse2d<T>*[m_Nx];
    for ( int i=0 ; i<m_Nx ; i++ ) m_v[i] = new tsparse2d<T>(m_Ny, m_Nz);
    //   setup_fast();
    // deep copy of all elements
    for ( int i=0 ; i<m_Nx ; i++ ) {
      for ( int j=0 ; j<m_Ny ; j++ ) {
	for ( int k=0 ; k<m_Nz ; k++ ) (*this)(i,j,k) = t(i,j,k);
      }
    }    
    if ( m_trimmed ) trim();
  }
  
  
  ~tsparse3d() { 
    if ( m_v ) { 
      for ( int i=m_ux-m_lx+1 ; i-- ;  )  if ( m_v[i] ) delete m_v[i];
      delete[] m_v;
    }
    //    empty_fast();
  }
  

  // accessors
  int Ny() const { return m_Ny; } 
  int Nz() const { return m_Nz; } 



  void trim() { 
    
    m_trimmed = true;
    
    // trim each column down 
    for ( int i=m_ux-m_lx+1 ; i-- ;  )  if ( m_v[i] ) m_v[i]->trim();

    int xmin = m_lx;
    int xmax = m_ux;
    
    // now find new limits
    for ( ; xmin<xmax+1 && (*m_v[xmin-m_lx])==0 ; xmin++ )  delete m_v[xmin-m_lx];
    for ( ; xmin<xmax   && (*m_v[xmax-m_lx])==0 ; xmax-- )  delete m_v[xmax-m_lx];

    m_empty = false;
    if ( xmax<xmin ) m_empty = true; 
   
    // don't need to change anything
    if ( xmin==m_lx && xmax==m_ux ) return; 

    // trimmed matrix is empty
    if ( xmax<xmin ) { 
      m_lx = xmin;   m_ux = xmax;
      delete[] m_v;  m_v = NULL;
      return; 
    }

    // copy to trimmed matrix (there must be a better way to do this)
    tsparse2d<T>** m_vnew = new tsparse2d<T>*[xmax-xmin+1]; 
    tsparse2d<T>** mnp = m_vnew;
    tsparse2d<T>** mvp = m_v+xmin-m_lx;
    for ( int j=xmin ; j<=xmax ; j++ )  (*mnp++) = (*mvp++);
    delete[] m_v;

    // update the sparse data and corresponding limits 
    m_lx = xmin;
    m_ux = xmax;
    m_v  = m_vnew;

    m_empty = false;
    if ( empty() ) m_empty = true;
  }


  void untrim() { 
    m_trimmed = false;
    grow(0);
    grow(Nx()-1);
    tsparse2d<T>** mvp = m_v;
    for ( int i=m_lx ; i<=m_ux ; i++ ) (*mvp++)->untrim();
  }
  
  bool trimmed() const { return m_trimmed; }

  bool trimmed(int i, int j, int k) const { 
    if ( i<m_lx || i>m_ux ) return false;
    else                    return m_v[i-m_lx]->trimmed(j,k);
  } 


  T operator()(int i, int j, int k) const {
    //  range_check(i);
    if ( i<m_lx || i>m_ux ) return 0;
    return (*((const tsparse2d<T>** const)m_v)[i-m_lx])(j,k);
  } 


  const tsparse2d<T>* operator[](int i) const {
    // range_check(i);
    if ( i<m_lx || i>m_ux ) return NULL;
    return m_v[i-m_lx];
  } 
  

  T& operator()(int i, int j, int k) {
    // range_check(i);
    grow(i);
    return (*m_v[i-m_lx])(j,k);
  } 

  int size() const { 
    int N=0;
    for ( int i=m_ux-m_lx+1 ; i-- ; )  N += m_v[i]->size();
    return N; // +3*sizeof(int);
  }

#if 0
  void setup_fast() { 
    // set up fast lookup table into the (untrimmed) 3d array.
    m_fastindex = new T*[Nx()*Ny()*Nz()];

    for ( int i=0 ; i<Nx() ; i++ ) { 
      for ( int j=0 ; j<Ny() ; j++ ) { 
	for ( int k=0 ; k<Nz() ; k++ ) { 
	  m_fastindex[(i*Ny()+j)*Nz()+k] = &(m_v[i]->v()[j])->v()[k];
	}
      }
    }
  }
  
  
  void empty_fast() { 
    if ( m_fastindex ) delete[] m_fastindex;
    m_fastindex=NULL;
  }
  
  T& fill_fast(int i, int j, int k) { 
    return *m_fastindex[(i*Ny()+j)*Nz()+k];
  }

  T fill_fast(int i, int j, int k) const { 
    return *m_fastindex[(i*Ny()+j)*Nz()+k];
  }
#endif


  void print() const {
    if ( m_ux-m_lx+1==0 ) std::cout << "-" << "\n";
    else { 
      for ( int i=0 ; i<Nx() ; i++ ) {     
	if ( tsparse_base::trimmed(i) ) { 
	  std::cout << "m_v[" << i << "]=" << m_v[i-m_lx] << "\n";
	  if ( m_v[i-m_lx] ) m_v[i-m_lx]->print();
	}
	else {
	  std::cout << "- \t"; 
	}
	std::cout << "\n";
      }
    }
  }  

  int ymin() const { 
    int minx = m_Ny;
    tsparse2d<T>** mvp = m_v;
    for ( int i=m_lx ; i<=m_ux ; i++ ) { 
      int _minx=(*mvp++)->xmin();
      if ( _minx<minx ) minx=_minx;
    }
    return minx;
  }

  int ymax() const { 
    int maxx = -1;
    tsparse2d<T>** mvp = m_v;
    for ( int i=m_lx ; i<=m_ux ; i++ ) { 
      int _minx=(*mvp)->xmin();
      int _maxx=(*mvp++)->xmax();
      if ( _minx<=_maxx && _maxx>maxx ) maxx=_maxx;
    }
    if ( maxx==-1 ) maxx=m_Ny-1;
    return maxx;
  }


  int zmin() const { 
    int minx = m_Nz;
    tsparse2d<T>** mvp = m_v;
    for ( int i=m_lx ; i<=m_ux ; i++ ) { 
      int _minx=(*mvp++)->ymin();
      if ( _minx<minx ) minx=_minx;
    }
    return minx;
  }

  int zmax() const { 
    int maxx = -1;
    tsparse2d<T>** mvp = m_v;
    for ( int i=m_lx ; i<=m_ux ; i++ ) { 
      int _minx=(*mvp)->ymin();
      int _maxx=(*mvp++)->ymax();
      if ( _minx<=_maxx && _maxx>maxx ) maxx=_maxx;
    }
    if ( maxx==-1 ) maxx=m_Nz-1;
    return maxx;
  }


  // algebraic operators

  tsparse3d& operator=(const tsparse3d& t) { 
    // clearout what is already in the matrix 
    if ( m_v ) { 
      for ( int i=m_ux-m_lx+1 ; i-- ; )  if ( m_v[i] ) delete m_v[i];
      delete[] m_v;
      m_v = NULL;
    }

    m_trimmed = t.m_trimmed;

    // now copy everything else
    m_Nx = t.m_Nx;
    m_Ny = t.m_Ny;
    m_Nz = t.m_Nz;    

    m_lx = 0;
    m_ux = m_Nx-1;
    m_v = new tsparse2d<T>*[m_Nx];
    for ( int i=0 ; i<m_Nx ; i++ ) m_v[i] = new tsparse2d<T>(m_Ny, m_Nz);
    //   setup_fast();
    // deep copy of all elements
    for ( int i=0 ; i<m_Nx ; i++ ) {
      for ( int j=0 ; j<m_Ny ; j++ ) {
	for ( int k=0 ; k<m_Nz ; k++ ) (*this)(i,j,k) = t(i,j,k);
      }
    }    
    if ( m_trimmed ) trim();
  }
  
  
  tsparse3d& operator*=(const double& d) { 
    for ( int i=0 ; i<Nx() ; i++ ) {     
      if ( tsparse_base::trimmed(i) ) { 
	if ( m_v[i-m_lx] ) (*m_v[i-m_lx])*=d;
      }
    }
    return *this;
  }
  
  
  tsparse3d& operator+=(const tsparse3d& t) {
    m_trimmed = false;
    if ( Nx()!=t.Nx() || Ny()!=t.Ny() || Nz()!=t.Nz() ) throw out_of_range("bin mismatch");
    for ( int i=0 ; i<Nx() ; i++ ) {
      for ( int j=0 ; j<Ny() ; j++ ) {
	for ( int k=0 ; k<Nz() ; k++ ) (*this)(i,j,k) += t(i,j,k);
      }
    }
    return *this;
  }
  

private:
  
  void grow(int i) { 
     
    // nothing needs to be done; 
    if ( i>=m_lx && i<=m_ux ) return;

    // is it empty? add a single row
    if ( m_lx>m_ux ) { 
      m_v    = new tsparse2d<T>*[1];
      (*m_v) = new tsparse2d<T>(m_Ny, m_Nz, m_Ny, m_Nz, 0, 0);
      m_lx = m_ux = i;
      return;
    }
    
    tsparse2d<T>** newv = new tsparse2d<T>*[ ( i<m_lx ? m_ux-i+1 : i-m_lx+1 ) ];
    tsparse2d<T>** newp = newv;
    tsparse2d<T>** mvp  = m_v;

    int lx = m_lx;
    int ux = m_ux;

    // grow at front
    for ( ; m_lx>i ; m_lx-- )  (*newp++) = new tsparse2d<T>(m_Ny, m_Nz, m_Ny, m_Nz, 0, 0);
    // copy existing
    for ( ; lx<=ux ; lx++ )    (*newp++) = (*mvp++);
    // grow at back
    for ( ; m_ux<i ; m_ux++ )  (*newp++) = new tsparse2d<T>(m_Ny, m_Nz, m_Ny, m_Nz, 0, 0);

    delete[] m_v;
    m_v = newv;
  }


protected:
  
  int m_Ny;
  int m_Nz;
  
  tsparse2d<T>**  m_v;
  
  //  T** m_fastindex;
  
  bool m_trimmed;

};


// stream IO template
template<class T> std::ostream& operator<<(std::ostream& s, const tsparse3d<T>& sp) { 
  for ( int i=0 ; i<sp.Nx() ; i++ ) { 
    for ( int j=0 ; j<sp.Ny() ; j++ ) { 
      for ( int k=0 ; k<sp.Nz() ; k++ ) { 
	s << sp(i,j,k) << "\t"; 
      }
      s << "\n";
    }
    s << "\n";
  }
  return s;
}


#endif  // __TSPARSE3D_H 










