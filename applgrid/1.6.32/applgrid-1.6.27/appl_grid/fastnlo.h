/** emacs: this is -*- c++ -*- **/
/**
 **     @file    fastnlo.h
 **
 **     @brief   native fastnlo interface for appl_grid                    
 **
 **     @author  mark sutton
 **     @date    Thu  8 Oct 2009 23:04:35 BST 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: fastnlo.h, v0.0   Thu  8 Oct 2009 23:04:35 BST sutt $
 **
 **/


#ifndef __FASTNLO_H
#define __FASTNLO_H

#include <iostream>
#include <vector>
#include <string>

#include "appl_grid/appl_grid.h"
#include "appl_grid/appl_pdf.h"
#include "appl_grid/appl_timer.h"


class fastnlo {
  
public:
  
  fastnlo(const std::string& filename ); 
  
  virtual ~fastnlo() { 
    if ( m_manage_grids ) { 
      for ( int i=m_grid.size() ; i-- ; ) { delete m_grid[i]; m_grid[i]=NULL; } 
    }
  } 

  unsigned size() const { return m_grid.size(); }

  std::vector<appl::grid*> grids() { return m_grid; }

  const appl::grid* operator[](int i) const { return m_grid[i]; }

  void manageGrids(bool b=true) { m_manage_grids=b; }  

private:
  
  bool m_manage_grids;

  std::vector<appl::grid*> m_grid;
  
};


inline std::ostream& operator<<( std::ostream& s, const fastnlo& _f) {
  for ( unsigned i=0 ; i<_f.size() ; i++ ) s << _f[i] << "\n"; 
  return s;
}


#endif  // __FASTNLO_H 










