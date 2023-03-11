/**
 **     @file    fappl_fitter.cxx
 **
 **     @author  mark sutton
 **     @date    Fri 25 Oct 2013 00:49:13 CEST 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: fappl_fitter.cxx, v0.0   Fri 25 Oct 2013 00:49:13 CEST sutt $
 **
 **/



#include "fappl_grid.h"


// /// externally defined alpha_s and pdf routines for fortran 
// /// callable convolution wrapper
// extern "C" double appl_fnalphas_(const double& Q);
// extern "C" void   appl_fnpdf_(const double& x, const double& Q, double* xf);

// extern "C" double fnalphas_(const double& Q) { return appl_fnalphas_( Q); }
// extern "C" void   fnpdf_(const double& x, const double& Q, double* xf) { return appl_fnpdf_( x, Q, xf); } 


/// create a grid
// extern "C" void appl_bookgrid_(int& id, const int& Nobs, const double* binlims) { bookgrid_( id, Nobs, binlims); } 

/// delete a grid
extern "C" void appl_releasegrid_(int& id) { return releasegrid_( id ); }

/// delete all grids
extern "C" void appl_releasegrids_()       { return releasegrids_(); } 

/// read a grid from a file
extern "C" void appl_readgrid_(int& id, const char* s, int _len ) { return readgrid_( id,  s, _len ); } 

/// write to a file 
extern "C" void appl_writegrid_(int& id, const char* s, int _len) { return writegrid_( id, s, _len ); }  


// /// add an entry 
// extern "C" void appl_fillgrid_(int& id, 
// 			       const int& ix1, const int& ix2, const int& iQ, 
// 			       const int& iobs, 
// 			       const double* w,
// 			       const int& iorder ) { 
//   return fillgrid_( id, ix1, ix2, iQ, iobs, w, iorder ); 
// }

// /// redefine the grid dimensions
// extern "C" void appl_redefine_(int& id, 
// 			       const int& iobs, const int& iorder, 
// 			       const int& NQ2, const double& Q2min, const double& Q2max, 
// 			       const int& Nx,  const double&  xmin, const double&  xmax) { 

//   return redefine_( id, iobs, iorder, NQ2, Q2min, Q2max, Nx, xmin, xmax ); 
// }


/// get the ckm matrix for a grid
extern "C" void appl_getckm_( int& id, double* ckm ) { return getckm_( id, ckm ); } 

/// get the ckm matrix for a grid
extern "C" void appl_setckm_( int& id, const double* ckm ) { return setckm_( id, ckm ); } 

/// get number of observable bins for a grid 
extern "C" int appl_getnbins_(int& id) { return getnbins_(id); }

/// fins a bin number based on an ordinate
extern "C" int appl_getbinnumber_(int& id, double& x) { return getbinnumber_(id,x); } 

/// get low edge for a grid/bin number
extern "C" double appl_getbinlowedge_(int& id, int& bin) { return getbinlowedge_(id,bin); }

/// get bin width for a grid/bin number
extern "C" double appl_getbinwidth_(int& id, int& bin) { return getbinwidth_(id,bin); }


/// do the convolution!! hooray!!

extern "C" void appl_convolute_(int& id, double* data) { return convolute_( id, data ); }


extern "C" void appl_convoluteorder_(int& id, int& nloops, double* data) { return convoluteorder_( id, nloops, data); }


extern "C" void appl_convolutewrap_(int& id, double* data, 
			       void (*pdf)(const double& , const double&, double* ),
			       double (*alphas)(const double& ) ) {
  return  convolutewrap_( id,  data, pdf, alphas ); 
}





extern "C" void appl_fullconvolutewrap_(int& id, double* data, 
					void (*pdf)(const double& , const double&, double* ),
					double (*alphas)(const double& ),
					int& nloops,
					double& rscale, double& fscale  ) {
  return  fullconvolutewrap_( id,  data, pdf, alphas, nloops, rscale, fscale ); 
}



extern "C" void appl_escaleconvolute_(int& id, double* data, double& Escale) { 
  return escaleconvolute_( id, data, Escale); 
}



extern "C" void appl_escaleconvolutewrap_(int& id, double* data, 
					  void (*pdf)(const double& , const double&, double* ),
					  double (*alphas)(const double& ), double& Escale ) { 
  return escaleconvolutewrap_( id, data, pdf, alphas, Escale );
}



extern "C" void appl_escalefullconvolutewrap_(int& id, double* data, 
					      void (*pdf)(const double& , const double&, double* ),
					      double (*alphas)(const double& ),
					      int& nloops,
					      double& rscale, double& fscale,
					      double& Escale ) { 
  return escalefullconvolutewrap_( id, data, pdf, alphas, nloops, rscale, fscale, Escale );
}



/// print a grid
extern "C" void appl_printgrid_(int& id) { return printgrid_( id ); }

/// print all grids
extern "C" void appl_printgrids_() { return printgrids_(); }

/// print the grid documentation
extern "C" void appl_printgriddoc_(int& id) { return printgriddoc_(id); } 

/// create grids from fastnlo
extern "C" void appl_readfastnlogrids_(  int* ids, const char* s, int _len ) { return readfastnlogrids_( ids, s, _len ); }


/// grid std::map management

extern "C" void appl_ngrids_(int& n) { return ngrids_( n ); }

extern "C" void appl_gridids_(int* ids) { return gridids_( ids ); } 

