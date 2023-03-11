/** emacs: this is -*- c++ -*- **/
/**
 **     @file    sparse.h
 **
 **     @brief   define 1, 2 and 3d block sparse matrices for doubles                  
 **
 **     @author  mark sutton
 **     @date    Thu Nov 22 15:46:00 GMT 2007 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: sparse.h, v1.0   Thu Nov 22 15:46:00 GMT 2007 sutt $
 **
 **/


#ifndef __SPARSE_H
#define __SPARSE_H


#include "tsparse3d.h"

typedef tsparse1d<double> sparse1d;
typedef tsparse2d<double> sparse2d;
typedef tsparse3d<double> sparse3d;


#endif  // __SPARSE_H 










