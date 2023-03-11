/** emacs: this is -*- c++ -*- **/
/**
 **     @file    combination.h
 **
 **     @author  mark sutton
 **     @date    Thu  4 Jul 2013 23:20:40 CEST 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: combination.h, v0.0   Thu  4 Jul 2013 23:20:40 CEST sutt $
 **
 **/


#ifndef  COMBINATION_H
#define  COMBINATION_H

#include <iostream>


#include <iostream>
#include <sstream>
#include <fstream>
#include <string>

#include <vector>
#include <utility>



class combination { 

public:

  typedef std::pair<int,int> cpair;

public:

  combination();

  /// constructors and destructor
  combination(const std::vector<int>& v);

  combination(const std::string& line);

  combination(const combination& c);

  virtual ~combination() {}

  /// evaluate the actual combination

  double evaluate( const double* xfA, const double* xfB,
		   const std::vector<double>& ckmsum=std::vector<double>(), 
		   const std::vector<std::vector<double> >& ckm2=std::vector<std::vector<double> >()  ) const;  

  /// number of pairs
  unsigned size() const { return m_pairs.size(); }  

  /// do any of these remaining need to be public?

  /// index
  const std::vector<int>&  index()  const { return m_index; }
  std::vector<int>&        index()        { return m_index; }

  void add_index( int i )  { m_index.push_back(i); }

  /// accessors to the pairs themselves
  cpair  pair(int i) const { return m_pairs[i]; }

  const std::vector<cpair>& pairs() const { return m_pairs; }

  cpair&       operator[](int i)       { return m_pairs[i]; } 
  const cpair& operator[](int i) const { return m_pairs[i]; } 

  bool operator==( const combination& c ) const;

  /// some spurious logical operators
  bool operator<(  const combination& c ) const { return m_index[0]<c.m_index[0]; }
  bool operator>(  const combination& c ) const { return m_index[0]>c.m_index[0]; }
  bool operator<=(  const combination& c ) const { return m_index[0]<=c.m_index[0]; }

  std::vector<int> serialise() const;

  bool contains( const cpair& p ) { for ( unsigned i=0 ; i<m_pairs.size() ; i++ ) if ( m_pairs[i]==p ) return true; return false; } 

private:

  /// actually construct combination from the pair list
  void construct(const std::vector<int>& v);

  /// remap from the pdg to lhapdf code
  int remap(int i) {
    if ( i==21 ) return 0;
    if ( i==22 ) return 7; 
    return i; 
  } 

private:

  std::vector<int>   m_index;
  unsigned           m_size;
  
  std::vector<cpair> m_pairs;

  //  static int         gindex;
  
};




inline std::ostream& operator<<( std::ostream& s, const combination& c ) { 
  s << "[ (";
  for ( unsigned ic=0 ; ic<c.index().size() ; ic++ ) s << " " << c.index()[ic]; 
  s << ") : ";
  if ( c.size()>10 ) { 
    for ( int i=0 ; i<4 ; i++ ) s << "\t(" << c[i].first << ", " << c[i].second << ")";
    s << "\t  ... ";
    for ( unsigned i=c.size()-3 ; i<c.size() ; i++ ) s << "\t(" << c[i].first << ", " << c[i].second << ")";
  }
  else { 
    for ( unsigned i=0 ; i<c.size() ; i++ ) s << "\t(" << c[i].first << ", " << c[i].second << ")";
  }
  s << " ]";
  return s;
}


inline std::ostream& operator<<( std::ostream& s, const std::pair<int,int>& p ) {
  return s << "( " << p.first << " " << p.second << ")"; 
} 

#endif  // COMBINATION_H 










