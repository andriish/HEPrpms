// -*- C++ -*-
//
// This file is part of LHAPDF
// Copyright (C) 2012-2022 The LHAPDF collaboration (see AUTHORS for details)
//
#include "LHAPDF/LogBicubicInterpolator.h"
#include <iostream>

namespace LHAPDF {


  namespace { // Unnamed namespace
    struct shared_data{
      // Pre-calculate parameters
      double logx, logq2, dlogx_1, dlogq_0, dlogq_1,  dlogq_2, tlogq;
      double tlogx;
      // bools to find out if at grid edges
      bool q2_lower, q2_upper;
    };

    shared_data fill(const KnotArray& grid, double x, double q2, size_t ix, size_t iq2){
      shared_data shared;
      shared.logx  = std::log(x);
      shared.logq2 = std::log(q2); // only required for fallback mode

      shared.q2_lower = ( (iq2 == 0) || (grid.q2s(iq2) == grid.q2s(iq2-1)));      
      shared.q2_upper = ( (iq2+1 == grid.q2size() - 1 ) || (grid.q2s(iq2+1) == grid.q2s(iq2+2)) );

      // Pre-calculate parameters
      shared.dlogx_1 = grid.logxs(ix+1) - grid.logxs(ix);
      shared.tlogx   = (shared.logx - grid.logxs(ix)) / shared.dlogx_1;
      if(!shared.q2_lower)
	shared.dlogq_0 = 1./(grid.logq2s(iq2) - grid.logq2s(iq2-1)); // only ever need the 1/values of the differences
      shared.dlogq_1 = grid.logq2s(iq2+1) - grid.logq2s(iq2);
      if(!shared.q2_upper)
	shared.dlogq_2 = 1./(grid.logq2s(iq2+2) - grid.logq2s(iq2+1));
      shared.tlogq   = (shared.logq2 - grid.logq2s(iq2)) / shared.dlogq_1;

      return shared;
    }


    /// One-dimensional linear interpolation for y(x)
    /// @todo Expose for re-use
    inline double _interpolateLinear(double x, double xl, double xh, double yl, double yh)  {
      assert(x >= xl);
      assert(xh >= x);
      return yl + (x - xl) / (xh - xl) * (yh - yl);
    }


    /// One-dimensional cubic interpolation
    ///
    /// @arg t is the fractional distance of the evaluation x into the dx
    /// interval.  @arg vl and @arg vh are the function values at the low and
    /// high edges of the interval. @arg vdl and @arg vdh are linearly
    /// extrapolated value changes from the product of dx and the discrete low-
    /// and high-edge derivative estimates.
    ///
    /// @note See Numerical Recipes 3.6: http://www.it.uom.gr/teaching/linearalgebra/NumericalRecipiesInC/c3-6.pdf
    ///
    /// @todo Expose for re-use
    inline double _interpolateCubic(double t, double vl, double vdl, double vh, double vdh) {
      // Pre-calculate powers of t
      const double t2 = t*t;
      const double t3 = t*t2;

      // Calculate polynomial (grouped by input param rather than powers of t for efficiency)
      const double p0 = (2*t3 - 3*t2 + 1)*vl;
      const double m0 = (t3 - 2*t2 + t)*vdl;
      const double p1 = (-2*t3 + 3*t2)*vh;
      const double m1 = (t3 - t2)*vdh;

      return p0 + m0 + p1 + m1;
    }


    /// Cubic interpolation using a passed array of coefficients
    ///
    /// @todo Expose for re-use
    inline double _interpolateCubic(double t, const double* coeffs) {
      const double x = t;
      const double x2 = x*x;
      const double x3 = x2*x;
      return coeffs[0]*x3 + coeffs[1]*x2 + coeffs[2]*x + coeffs[3];
    }


    double _interpolate(const KnotArray& grid, size_t ix, size_t iq2, int id, shared_data& _share) {
      double vl = _interpolateCubic(_share.tlogx, &grid.coeff(ix,iq2,id,0));
      double vh = _interpolateCubic(_share.tlogx, &grid.coeff(ix,iq2+1,id,0));

      // Derivatives in Q2
      /// @todo Rewrite so initial vdl, vdh = (vh - vl), then update if needed: reduce duplication
      /// @todo Change derivative approximation to take knot distance into account
      double vdl, vdh;
      if (_share.q2_lower) {
        // Forward difference for lower q
        vdl = (vh - vl);
        // Central difference for higher q
        double vhh = _interpolateCubic(_share.tlogx, &grid.coeff(ix,iq2+2,id,0));
        vdh = (vdl + (vhh - vh) * _share.dlogq_1 * _share.dlogq_2) * 0.5;
      }
      else if (_share.q2_upper) {
        // Backward difference for higher q
        vdh = (vh - vl);
        // Central difference for lower q
        double vll = _interpolateCubic(_share.tlogx, &grid.coeff(ix,iq2-1,id,0));
        vdl = (vdh + (vl - vll) * _share.dlogq_1 * _share.dlogq_0) * 0.5;
      } else {
        // Central difference for both q
        /// @todo Replace with better derivative estimates?
        double vll = _interpolateCubic(_share.tlogx, &grid.coeff(ix,iq2-1,id,0));
        vdl = ( (vh - vl) + (vl - vll)*_share.dlogq_1 * _share.dlogq_0 ) * 0.5;
        double vhh = _interpolateCubic(_share.tlogx, &grid.coeff(ix,iq2+2,id,0));
        vdh = ( (vh - vl) + (vhh - vh)*_share.dlogq_1 * _share.dlogq_2 ) * 0.5;
      }

      return _interpolateCubic(_share.tlogq, vl, vdl, vh, vdh);
    }


    double _interpolateFallback(const KnotArray& grid, size_t ix, size_t iq2, int id, shared_data& _share) {
      // First interpolate in x
      const double logx0 = grid.logxs(ix);
      const double logx1 = grid.logxs(ix+1);
      const double f_ql = _interpolateLinear(_share.logx, logx0, logx1, grid.xf(ix, iq2, id),   grid.xf(ix+1, iq2, id));
      const double f_qh = _interpolateLinear(_share.logx, logx0, logx1, grid.xf(ix, iq2+1, id), grid.xf(ix+1, iq2+1, id));
      // Then interpolate in Q2, using the x-ipol results as anchor points
      return _interpolateLinear(_share.logq2, grid.logq2s(iq2), grid.logq2s(iq2+1), f_ql, f_qh);
    }


    void _checkGridSize(const KnotArray& grid, const size_t ix, const size_t iq2) {
      // Raise an error if there are too few knots even for a linear fall-back
      const size_t nxknots = grid.xsize();
      const size_t nq2knots = grid.q2size();

      /// @todo MK: do you really need different number of knots in the directions?
      ///   Probably should be <2 for both methods, and fall back to linear in both cases.
      if (nxknots < 4)
        throw GridError("PDF subgrids are required to have at least 4 x-knots for use with LogBicubicInterpolator");
      if (nq2knots < 2)
        throw GridError("PDF subgrids are required to have at least 2 Q-knots for use with LogBicubicInterpolator");

      // Check x and q index ranges -- we always need i and i+1 indices to be valid
      const size_t ixmax = nxknots - 1;
      const size_t iq2max = nq2knots - 1;
      if (ix+1 > ixmax) // also true if ix is off the end
        throw GridError("Attempting to access an x-knot index past the end of the array, in linear fallback mode");
      if (iq2+1 > iq2max) // also true if iq2 is off the end
        throw GridError("Attempting to access an Q-knot index past the end of the array, in linear fallback mode");
    }

  }


  double LogBicubicInterpolator::_interpolateXQ2(const KnotArray& grid, double x, size_t ix, double q2, size_t iq2, int id) const {
    _checkGridSize(grid, ix, iq2);
    shared_data shared = fill(grid, x, q2, ix, iq2);
    // if it is an upper and a lower edge, there can only be two notes
    //   use lineare fallback in that case, but have default be the cubic case
    if (!shared.q2_lower || !shared.q2_upper)
      return _interpolate(grid, ix, iq2, id, shared);

    // Fallback mode
    return _interpolateFallback(grid, ix, iq2, id, shared);
  }


  /// Interpolate a whole set of standard parton IDs at once, minimising recomputation
  void LogBicubicInterpolator::_interpolateXQ2(const KnotArray& grid, double x, size_t ix, double q2, size_t iq2, std::vector<double>& ret) const {
    _checkGridSize(grid, ix, iq2);
    shared_data shared = fill(grid, x, q2, ix, iq2);

    if (!shared.q2_lower || !shared.q2_upper) {
      for (int pid = -6; pid <= 6; ++pid) {
        int id = grid.lookUpPid(pid + 6);
        ret[pid + 6] = (id == -1) ? 0 : _interpolate(grid, ix, iq2, id, shared);
      }
    } else {
      for (int pid = -6; pid <= 6; ++pid) {
        const int id = grid.lookUpPid(pid + 6);
        ret[pid + 6] = (id == -1) ? 0 : _interpolateFallback(grid, ix, iq2, id, shared);
      }
    }
  }


}
