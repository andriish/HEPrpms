/** emacs: this is -*- c++ -*- **/
/**
 **     @file    hoppet_init.h
 **
 **     @author  mark sutton
 **     @date    Sat 26 Sep 2009 13:24:26 BST 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: hoppet_init.h, v0.0   Sat 26 Sep 2009 13:24:26 BST sutt $
 **
 **/


#ifndef __HOPPETINIT_H
#define __HOPPETINIT_H

#include <iostream>
#include <vector>

namespace appl { 

class hoppet_init : public std::vector<double> {

public:

  /// default maximum scale for hoppet initialisation
  hoppet_init( double _Qmax=15000, double _ymax=12 );

  virtual ~hoppet_init();

  void fillCache( void (*pdf)(const double&, const double&, double* )  );
  
  bool compareCache( void (*pdf)(const double&, const double&, double* )  );

  static void assign( void (*pdf)(const double&, const double&, double* )  );

};

}

inline std::ostream& operator<<( std::ostream& s, const appl::hoppet_init& _h ) { 
  return s << "appl::hoppet_init size: " << _h.size();
}


#endif  // __HOPPETINIT_H 










