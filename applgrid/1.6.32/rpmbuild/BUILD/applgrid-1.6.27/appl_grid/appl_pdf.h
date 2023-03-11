/** emacs: this is -*- c++ -*- **/
/**
 **     @file    appl_pdf.h
 **
 **     @brief   pdf transform functions header                  
 **
 **     @author  mark sutton
 **     @date    Fri Dec 21 22:19:50 GMT 2007 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: appl_pdf.h, v   Fri Dec 21 22:19:50 GMT 2007 sutt $
 **
 **/


#ifndef __APPL_PDF_H
#define __APPL_PDF_H

#include <iostream>
#include <fstream>
#include <sstream>

#include <vector> 
#include <map> 
#include <string> 

#include <exception> 

#include "vector_stream.h"

namespace appl { 


class appl_pdf;

typedef std::map<const std::string, appl_pdf*> pdfmap;


// this is a *maybe* nice class, a base class for pdf 
// functions
//
// it has a virtual evaluate() method to be definied in 
// the derived class, and a static std::map of all the names
// of instances of the derived classes
//
// when a new instance of the class is created, it 
// automatically adds it's name to the std::map, so the user 
// doesn't need to worry about consistency, and removes 
// itself when the derived instance is deleted
// consequently, if you want to use pdfs added to the database, 
// you need to make sure that they do not go out of scope 
// NB: if you try to copy an appl_pdf then there stuff will 
//     get messed up, since the names and appl_pdf pointers 
//     are in a map, and you can't have duplicate keys 
//     so things might not go as you would expect if you try 
//     and use the copy - ie it might not have been added to 
//     the map, whereas the original was added, but was then 
//     removed when it went out of scope 
 

class appl_pdf { 

public:

  // pdf error exception
  class exception : public std::exception { 
  public: 
    exception(const std::string& s="") { std::cerr << what() << " " << s << std::endl; }; 
    const char* what() const throw() { return "appl::appl_pdf::exception "; }
  };
  
public:

  /// constructor and destructor
  /// include a flag to determine whether we want this appl_pdf registered 
  /// directly, or defer, and register the lumi_pdf derived class - a needlessly 
  /// complicated mechanism but nevertheless ...
  appl_pdf(const std::string& name, bool base=false);

  virtual ~appl_pdf();

  /// retrieve an instance from the std::map 
  static appl_pdf* getpdf(const std::string& s, bool printout=true);
  
  /// print out the pdf std::map
  static void printmap(std::ostream& s=std::cout) {
    pdfmap::iterator itr = __pdfmap.begin();
    while ( itr!=__pdfmap.end() )  {
      s << "pdfmap " << itr->first << "\t\t" << itr->second << std::endl;
      itr++;
    } 
  }

  /// initialise the factory  
  static bool create_map(); 

  virtual void evaluate(const double* fA, const double* fB, double* H) const = 0; 

  virtual int decideSubProcess( const int , const int  ) const;

  std::string   name() const { return m_name;  }

  int     Nproc() const { return m_Nproc; } 
  int     size()  const { return m_Nproc; } 



  std::string rename(const std::string& name) { 
    /// remove my entry from the std::map, and add me again with my new name
    if ( __pdfmap.find(m_name)!=__pdfmap.end() ) { 
      __pdfmap.erase(__pdfmap.find(m_name));
    }
    else { 
      std::cout << "appl_pdf::rename() " << m_name << " not in std::map" << std::endl;
    }
    m_name = name;
    addtopdfmap(m_name, this);
    return m_name;
  }


  /// code to allow optional std::vector of subprocess contribution names

  const std::vector<std::string>& subnames() const { return m_subnames; }

  void addSubnames( const std::vector<std::string>& subnames ) { m_subnames = subnames; }

  void  addSubname( const std::string& subname ) { 
    if ( int(m_subnames.size())<m_Nproc-1 ) m_subnames.push_back(subname); 
  }



  /// is this a W+ or a W- pdf combination? or neither?
  int getckmcharge() const { return m_ckmcharge; }

  /// access the ckm matrices - if no matrices are required these std::vectors have 
  /// zero size

  const std::vector<double>&               getckmsum() const { return m_ckmsum; }
  const std::vector<std::vector<double> >& getckm2()   const { return m_ckm2; }
  const std::vector<std::vector<double> >& getckm()    const { return m_ckm; }
  
  /// set the ckm matrices from external values
  void setckm( const std::vector<std::vector<double> >& ckm ); 
  void setckm2( const std::vector<std::vector<double> >& ckm2 ); 

  /// code to create the ckm matrices using the hardcoded default 
  /// values if required
  /// takes a bool input - if true creates the ckm for Wplus, 
  /// false for Wminus
  void make_ckm( bool Wp=true );

  void SetNProc(int Nsub){ m_Nproc=Nsub; return;};

  /// set some useful names for the different subprocesses
  void setnames( const std::vector<std::string>& names) { m_names = names; } 
  std::vector<std::string> getnames() const  { return m_names; } 

  /// basic comparisons of two appl_pdfs - for the base class there is
  /// little to be done except test the name - really, each derived 
  /// class should implement it's own test

  virtual bool operator==( const appl_pdf& pdf ) const { return ( name() == pdf.name() );  }

  virtual bool operator!=( const appl_pdf& pdf ) const { return !operator==(pdf); }
  
  std::vector<double>               ckmsum() const { return m_ckmsum; }
  std::vector<std::vector<double> > ckm2()   const { return m_ckm2; } /// squared N x N matrix
  std::vector<std::vector<double> > ckm()    const { return m_ckm; } 

public:

  static bool overwrites() { return ALLOW_OVERWRITES; }

  static void overwrites( bool t ) { ALLOW_OVERWRITES = t; }

protected:

  /// search the path for configuration files
  static  std::ifstream& openpdf( const std::string& filename ); 

  static void addtopdfmap(const std::string& s, appl_pdf* f) { 
    if ( __pdfmap.find(s)==__pdfmap.end() ) { 
      __pdfmap.insert( pdfmap::value_type( s, f ) );
      // std::cout << "appl_pdf::addtomap() registering " << s << " in std::map addr \t" << f << std::endl;
    }
    else { 

      std::stringstream s_; 
      s_ << "appl_pdf::addtopdfmap() " << s << " already in std::map\t0x" << __pdfmap.find(s)->second;

      /// check if the pdf that is already in map is the same as the new pdf, 
      /// if it is then do nothing, but if it is different only *then* throw an 
      /// exception 
      if ( (*f) != (*__pdfmap.find(s)->second) ) {
	s_ << "\nappl_pdf::addtppdfmap() mismatch with appl_pdf in map";
        throw exception( s_.str() );
      }
    }
  }
  
protected:

  int         m_Nproc;
  std::string m_name;

  std::vector<std::string> m_subnames;

  /// ckm matrix related information 
  /// W+, W- or neither?
  int  m_ckmcharge;

  // ckm matrices
  std::vector<double>               m_ckmsum;
  std::vector<std::vector<double> > m_ckm2; /// squared N x N matrix
  std::vector<std::vector<double> > m_ckm;  /// simple 3 x 3

  /// some strings for more useful name if required
  std::vector<std::string>           m_names;

  static pdfmap                     __pdfmap;
  static std::vector<std::string>   __pdfpath;

  static bool                       ALLOW_OVERWRITES;

};

}




std::ostream& operator<<( std::ostream& s, const appl::appl_pdf& p );

#endif  // __APPL_PDF_H 










