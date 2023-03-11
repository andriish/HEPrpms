/**
 **     @file    combination.cxx
 **
 **     @brief   this is a class which allows the sum of a std::vector of 
 **              pairs of parton-parton initial states to be calculated  
 **
 **     @author  mark sutton
 **     @date    Thu  4 Jul 2013 23:20:23 CEST 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: combination.cxx, v0.0   Thu  4 Jul 2013 23:20:23 CEST sutt $
 **
 **/


/// automatically keeps count of how many subprocess combinations 
/// there have been 
 
/// read each of the pairs to be summed for a specific 
/// subprocess combination 


#include "appl_grid/appl_pdf.h"
#include "appl_grid/combination.h"


combination::combination() {} 


combination::combination(const std::vector<int>& v) { 
  //  std::cout << "combination::combination() std::vector" << std::endl;
  construct(v); 
}


combination::combination(const std::string& line) : m_size(0) { 

  //  std::cout << "combination::combination() std::string " << line << std::endl;

  std::istringstream iss(line);
  
  std::vector<int> v;
  v.reserve(11);
  
  int i;
  
  while (iss >> i) v.push_back(i);

  construct(v);
}


combination::combination(const combination& c) : 
  m_index(c.m_index), 
  m_size(c.m_size), 
  m_pairs(c.m_pairs) 
{ }



void combination::construct(const std::vector<int>& v) { // : m_index(v.at(0)), m_size(v.at(1)) { 

  //  std::cout << "combination::construct()" << std::endl;

  if ( v.size()==0 ) return; 

  //  if ( v.size()<4 || v.size()%2!=0 ) throw appl::appl_pdf::exception("not enough entries for combination");

  m_index.push_back( v[0] );
  m_size  = v[1];

  for ( size_t i=2 ; i<v.size() ; i+=2 ) { 
    int v1 = remap(v[i]);
    int v2 = remap(v[i+1]);
    if ( v2==99 ) v2 = v1;
    m_pairs.push_back( cpair( v1, v2 ) ); 
  }



  /// check there are no duplicated pairs in this combination ...

  bool duplicates = false;

  if ( m_pairs.size()>0 ) {  
    for ( unsigned i=0 ; i<m_pairs.size()-1 ; i++ ) { 
      //  std::cout << i << "\tindex " << m_index << " " << m_pairs[i] << std::endl; 
      for ( unsigned j=i+1 ; j<m_pairs.size() ; j++ ) { 
	if ( m_pairs[i]==m_pairs[j] ) { 
	  duplicates = true;
	  std::cerr << "index " << m_index << "\t duplicated entry" << m_pairs[i] << " in " << *this << std::endl;
	}
      }  
    }

    if ( duplicates ) throw appl::appl_pdf::exception("mismatch in entries for this for combination");
    
  }
  
}


/// evaluate the sum over all the (paired) subprocesses

double combination::evaluate( const double* xfA, const double* xfB,  
			      const std::vector<double>& _ckmsum, 
			      const std::vector<std::vector<double> >& _ckm2 ) const {  

  xfA += 6;  /// offset the pointer for xf1 and xf2 so can use 


  if ( xfB ) xfB += 6;  /// lhapdf code for indexing

  double H = 0; 

  if ( _ckmsum.size()==0 ) {
    /// without using the ckm matrix 
    /// for DIS pass in a null vector as the PDF components
    if ( xfB ) for ( unsigned i=size() ; i-- ; ) H += xfA[m_pairs[i].first]*xfB[m_pairs[i].second]; 
    else       for ( unsigned i=size() ; i-- ; ) H += xfA[m_pairs[i].first]; 
  }
  else {
    /// using the ckm matrix 
    if ( xfB ) { 
      for ( unsigned i=size() ; i-- ; ) { 
	if ( m_pairs[i].first!=0  ) {
	  if ( m_pairs[i].second!=0  )  H += xfA[m_pairs[i].first]*xfB[m_pairs[i].second]*_ckm2[m_pairs[i].first+6][m_pairs[i].second+6]; //q-q 
	  else 	                        H += xfA[m_pairs[i].first]*xfB[m_pairs[i].second]*_ckmsum[m_pairs[i].first+6]; /// q-gluon
	}
	else {
	  if ( m_pairs[i].second!=0  )  H += xfA[m_pairs[i].first]*xfB[m_pairs[i].second]*_ckmsum[m_pairs[i].second+6]; // gluon-q 
	  else                          H += xfA[m_pairs[i].first]*xfB[m_pairs[i].second]; /// gluon-gluon
	}
      }  /// looop over pairs
    }
    else { 
      throw appl::appl_pdf::exception("use of CKM matrix for DIS contributions not yet implemented" );
    }
  } /// use ckm matrix 
  
  return H;
}



bool combination::operator==( const combination& c ) const { 
  if ( size()!=c.size() ) return false;
  for ( int i=size() ; i-- ; ) { 
    if ( (*this)[i]!=c[i] ) return false; 
  }
  return true;
} 



std::vector<int> combination::serialise() const { 
  std::vector<int> v;
  v.push_back(size());
  for ( unsigned i=0 ; i<size() ; i++ ) { 
    v.push_back( pair(i).first );
    v.push_back( pair(i).second );
  }
  return v;
}
