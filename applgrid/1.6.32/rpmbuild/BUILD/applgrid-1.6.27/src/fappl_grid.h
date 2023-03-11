/** emacs: this is -*- c++ -*- **/
/**
 **     @file    fappl_grid.h
 **
 **     @author  mark sutton
 **     @date    Fri 25 Oct 2013 00:52:32 CEST 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: fappl_grid.h, v0.0   Fri 25 Oct 2013 00:52:32 CEST sutt $
 **
 **/


#pragma once


/// externally defined alpha_s and pdf routines for fortran 
/// callable convolution wrapper
extern "C" double fnalphas_(const double& Q);
extern "C" void   fnpdf_( const double& x, const double& Q, double* xf);



/// create a grid
extern "C" void bookgrid_( int& id, const int& Nobs, const double* binlims);

/// delete a grid
extern "C" void releasegrid_( const int& id );

/// delete all grids
extern "C" void releasegrids_();

/// read a grid from a file
extern "C" void readgrid_( int& id, const char* s, int _len);

/// write to a file 
extern "C" void writegrid_(const int& id, const char* s, int _len);

/// add an entry 
extern "C" void fillgrid_(const int& id, 
			  const int& ix1, const int& ix2, const int& iQ, 
			  const int& iobs, 
			  const double* w,
			  const int& iorder );  

/// redefine the grid dimensions
extern "C" void redefine_(const int& id, 
			  const int& iobs, const int& iorder, 
			  const int& NQ2,  const double& Q2min, const double& Q2max, 
			  const int& Nx,   const double&  xmin, const double&  xmax); 


/// get number of observable bins for a grid 
extern "C" int getnbins_(const int& id);

/// get the number of a bin based on the ordonate 
extern "C" int getbinnumber_(const int& id, double& x);

/// get low edge for a grid/bin number
extern "C" double getbinlowedge_(const int& id, int& bin);

/// get width for a grid/bin number
extern "C" double getbinwidth_(const int& id, const int& bin);

/// do the convolution!! hooray!!

extern "C" void convolute_(const int& id, double* data);

extern "C" void convolutewrap_(const int& id, double* data, 
			       void (*pdf)(const double& , const double&, double* ),
			       double (*alphas)(const double& ) );


extern "C" void convoluteppbar_(const int& id, double* data);

extern "C" void convoluteppbarwrap_(const int& id, double* data, 
				    void (*pdf)(const double& , const double&, double* ),
				    double (*alphas)(const double& ) );


/// allow different loop order
extern "C" void convoluteorder_(const int& id, const int& nloops, double* data);



/// allow varaiation of loop order and scales
extern "C" void fullconvolute_(const int& id, double* data, 
			       const int& nloops,
			       const double& rscale, const double& fscale  );

extern "C" void fullconvolutewrap_(const int& id, double* data, 
				   void (*pdf)(const double& , const double&, double* ),
				   double (*alphas)(const double& ),
				   const int& nloops,
				   const double& rscale, const double& fscale  );



/// allow different cms energies
extern "C" void escaleconvolute_(const int& id, double* data, const double& Escale);

extern "C" void escaleconvolutewrap_(const int& id, double* data, 
				     void (*pdf)(const double& , const double&, double* ),
				     double (*alphas)(const double& ), 
				     const double& Escale );



/// allow different cms eneregies and scale variation, and loop order
extern "C" void escalefullconvolute_(const int& id, double* data, 
				     const int& nloops,
				     const double& rscale, const double& fscale,
				     const double& Escale );

extern "C" void escalefullconvolutewrap_(const int& id, double* data, 
					 void (*pdf)(const double& , const double&, double* ),
					 double (*alphas)(const double& ),
					 const int& nloops,
					 const double& rscale, const double& fscale,
					 const double& Escale );


/// set the ckm matrix - a flat vector of 9 doubles, Vud, Vus, Vub, Vcd ...
extern "C" void setckm_( const int& id, const double* ckm );

/// get the ckm matrix - a flat vector of 9 doubles, Vud, Vus, Vub, Vcd ...
extern "C" void getckm_( const int& id, double* ckm );

extern "C" void lockckm_( const int& i ); 

/// print a grid
extern "C" void printgrid_(const int& id);

/// print all grids
extern "C" void printgrids_();

/// print the grid documentation
extern "C" void printgriddoc_(const int& id);

/// create grids from fastnlo
extern "C" void readfastnlogrids_( int* ids, const char* s, int _len );


/// grid std::map management

extern "C" void ngrids_(int& n);

extern "C" void gridids_(int* ids);










