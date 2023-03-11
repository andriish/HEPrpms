/* emacs: this is -*- c++ -*- */
/**
 **   @file    equals.h        
 **                   
 **   @author  sutt
 **   @date    Wed 30 Dec 2020 11:26:54 GMT
 **
 **   $Id: equals.h, v0.0   Wed 30 Dec 2020 11:26:54 GMT sutt $
 **
 **   Copyright (C) 2020 sutt (sutt@cern.ch)    
 **
 **/


#ifndef  EQUALS_H
#define  EQUALS_H


inline bool equals( double a, double b, double limit=1 ) { 
  if ( a==b ) return true; 
  if ( std::fabs(a-b)<(std::fabs(std::min(a,b))*std::numeric_limits<double>::epsilon()*limit ) ) return true; 
  if ( ( a==0 || b==0 ) && ( std::fabs(a-b)<std::numeric_limits<double>::epsilon()*limit ) ) return true;
  return false;
}


#endif  // EQUALS_H 










