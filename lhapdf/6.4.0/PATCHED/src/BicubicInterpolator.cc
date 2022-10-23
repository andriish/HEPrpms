// -*- C++ -*-
//
// This file is part of LHAPDF
// Copyright (C) 2012-2022 The LHAPDF collaboration (see AUTHORS for details)
//
#include "LHAPDF/BicubicInterpolator.h"
#include <iostream>

namespace LHAPDF {


  namespace { // Unnamed namespace
    struct shared_data{
      // bools to check for edges
      bool q2_lower, q2_upper;

      // common parts of the computations
      double dx, tx, dq_0, dq_1, dq_2, dq, tq;
    };

    shared_data fill(const KnotArray& grid, double x, double q2, size_t ix, size_t iq2){
      // check edges, including internal discontinuity
      shared_data shared;
      shared.q2_lower = ( (iq2 == 0) || (grid.q2s(iq2) == grid.q2s(iq2-1)));
      shared.q2_upper = ( (iq2 + 1 == grid.q2size() -1) || (grid.q2s(iq2+1) == grid.q2s(iq2+2)) );
      //const bool ix_lower = ( (ix == 0) );
      //const bool ix_upper = ( (ix == grid.xsize()) );

      // Distance parameters
      shared.dx = grid.xs(ix+1) - grid.xs(ix);
      shared.tx = (x - grid.xs(ix)) / shared.dx;
      /// @todo Only compute these if the +1 and +2 indices are guaranteed to be valid
      // i.e. check if that is in range, and there is no discontinuitie there
      if(!shared.q2_lower)
	shared.dq_0 = grid.q2s(iq2  ) - grid.q2s(iq2-1);
      shared.dq_1 = grid.q2s(iq2+1) - grid.q2s(iq2  );
      if(!shared.q2_upper)
	shared.dq_2 = grid.q2s(iq2+2) - grid.q2s(iq2+1);
      shared.dq = shared.dq_1;
      shared.tq = (q2 - grid.q2s(iq2)) / shared.dq;
      return shared;
    }

    // // One-dimensional linear interpolation for y(x)
    // inline double _interpolateLinear(double x, double xl, double xh, double yl, double yh) {
    //   assert(x >= xl);
    //   assert(xh >= x);
    //   return yl + (x - xl) / (xh - xl) * (yh - yl);
    // }

    // One-dimensional cubic interpolation for y(x)
    inline double _interpolateCubic(double T, const double *coeffs) {
      const double x = T;
      const double x2 = x*x;
      const double x3 = x2*x;
      return coeffs[0]*x3 + coeffs[1]*x2 + coeffs[2]*x + coeffs[3];
    }

    // One-dimensional cubic interpolation
    inline double _interpolateCubic(double T, double VL, double VDL, double VH, double VDH) {
      // Pre-calculate powers of T
      const double t2 = T*T;
      const double t3 = t2*T;

      // Calculate left point
      const double p0 = (2*t3 - 3*t2 + 1)*VL;
      const double m0 = (t3 - 2*t2 + T)*VDL;

      // Calculate right point
      const double p1 = (-2*t3 + 3*t2)*VH;
      const double m1 = (t3 - t2)*VDH;

      return p0 + m0 + p1 + m1;
    }

    double _interpolate(const KnotArray& grid, size_t ix, size_t iq2, int id, shared_data& _share){
      // Points in Q2
      double vl = _interpolateCubic(_share.tx, &grid.coeff(ix,iq2,id,0));
      double vh = _interpolateCubic(_share.tx, &grid.coeff(ix,iq2+1,id,0));

      // Derivatives in Q2
      double vdl, vdh;
      if (_share.q2_lower) {
	// Forward difference for lower q
	vdl = (vh - vl) / _share.dq_1;
	// Central difference for higher q
	double vhh = _interpolateCubic(_share.tx, &grid.coeff(ix,iq2+2,id,0));
	vdh = (vdl + (vhh - vh)/_share.dq_2) / 2.0;
      }
      else if (_share.q2_upper) {
	// Backward difference for higher q
	vdh = (vh - vl) / _share.dq_1;
	// Central difference for lower q
	double vll = _interpolateCubic(_share.tx, &grid.coeff(ix,iq2-1,id,0));
	vdl = (vdh + (vl - vll)/_share.dq_0) / 2.0;
      }
      else {
	// Central difference for both q
	double vll = _interpolateCubic(_share.tx, &grid.coeff(ix,iq2-1,id,0));
	vdl = ( (vh - vl)/_share.dq_1 + (vl - vll)/_share.dq_0 ) / 2.0;
	double vhh = _interpolateCubic(_share.tx, &grid.coeff(ix,iq2+2,id,0));
	vdh = ( (vh - vl)/_share.dq_1 + (vhh - vh)/_share.dq_2 ) / 2.0;
      }

      vdl *= _share.dq;
      vdh *= _share.dq;
      return _interpolateCubic(_share.tq, vl, vdl, vh, vdh);
    }

    void _checkGridSize(const KnotArray& grid, size_t ix, size_t iq2){
      // MK: if they have at least 2 knots, falls back to linear interpolator
      if (grid.xsize() < 4)
	throw GridError("PDF subgrids are required to have at least 4 x-knots for use with BicubicInterpolator");
      if (grid.q2size() < 4)
	throw GridError("PDF subgrids are required to have at least 4 Q2-knots for use with BicubicInterpolator");
    }



  }
  void BicubicInterpolator::_interpolateXQ2(const KnotArray& grid, double x, size_t ix, double q2, size_t iq2, std::vector<double>& ret) const{
    _checkGridSize(grid, ix, iq2);
    shared_data shared = fill(grid, x, q2, ix, iq2);

    ret.resize(13);
    for (int pid(-6); pid <= 6; ++pid){
      int id = grid.lookUpPid(pid + 6);
      if (id == -1){
	ret[pid + 6] = 0;
      } else {
	ret[pid + 6] = _interpolate(grid, ix, iq2, id, shared);
      }
    }
  }


  double BicubicInterpolator::_interpolateXQ2(const KnotArray& grid, double x, size_t ix, double q2, size_t iq2, int id) const {
    _checkGridSize(grid, ix, iq2);
    shared_data shared = fill(grid, x, q2, ix, iq2);

    /// @todo Allow interpolation right up to the borders of the grid in Q2 and x... the last inter-knot range is currently broken
    /// @todo Also treat the x top/bottom edges carefully, cf. the Q2 ones
    return _interpolate(grid, ix, iq2, id, shared);
  }


}
