//------------------------------------------------------------------------------
/// \file BoundingConstantsBase.hpp
//
// Author(s):
//   Christian Bauer, Simone Alioli
//
// Copyright:
//   Copyright (C) 2014 LBNL, CERN
//
//   This file is part of the Geneva MC framework. Geneva is distributed under
//   the terms of the GNU General Public License version 3 (GPLv3), see the
//   COPYING file that comes with this distribution for details.
//   Please respect the academic usage guidelines in the GUIDELINES file.
//
// Description:
//   Interface of class BoundingConstantsBase
//------------------------------------------------------------------------------

#ifndef GENEVA_BOUNDING_CONSTANTS_BASE_HPP
#define GENEVA_BOUNDING_CONSTANTS_BASE_HPP

#include "Geneva/Core/Physics/Signature.hpp"
#include "Geneva/Core/UI/Log.hpp"

#include <iosfwd>
#include <vector>

#define CUTOFF 1e-10

namespace Geneva
{

//------------------------------------------------------------------------------
/**
 * \class BoundingConstantsBase
 * \ingroup Components
 *
 * \brief This is the base class that holds the information of the bounding constant.
 *
 */
//------------------------------------------------------------------------------
class BoundingConstantsBase
{
   protected:
      double _boundFactor;                  ///<Multiply the found upper-bound by a factor
      size_t _numTauNBins;                  ///< The number of bins for TauN
      double _tauNMin;                      ///< The minimum number for TauN
      double _tauNMax;                      ///< The maximum number for TauN
      std::vector<double> _binTauNEdges;    ///< The values of the bin edges for TauN
      double _tauNSampling;                 ///< The power with which the TauN value is sampled

   public:
      BoundingConstantsBase()
         : _boundFactor(std::numeric_limits< double >::quiet_NaN()), _numTauNBins(0), _tauNSampling(std::numeric_limits< double >::quiet_NaN())
      {};

      BoundingConstantsBase (size_t numTauNBins, double tauNMin, double tauNMax)
         : _boundFactor (1.0), _numTauNBins(numTauNBins), _tauNMin(tauNMin), _tauNMax(tauNMax), _tauNSampling(1.1)
      {
         _binTauNEdges.reserve(_numTauNBins + 1);
         _binTauNEdges.clear();
         for (size_t i = 0; i <= _numTauNBins; ++i) _binTauNEdges.push_back(0);
         for (size_t i = 0; i <= _numTauNBins; ++i) {
            _binTauNEdges[i] = tauNVal(static_cast<double>(i) / static_cast<double>(_numTauNBins));
         }
      }

      BoundingConstantsBase(const BoundingConstantsBase&) = delete;
      BoundingConstantsBase(BoundingConstantsBase&&) = delete;
      BoundingConstantsBase& operator=(const BoundingConstantsBase&) = delete;
      BoundingConstantsBase& operator=(BoundingConstantsBase&&) = delete;
      virtual ~BoundingConstantsBase() = default;

      void setBoundFactor(double boundFactor) { _boundFactor = boundFactor; };

      virtual bool boundExists(Signature::const_iterator mother) const = 0;

      // writes all internal data to the given stream
      virtual void write(std::ostream& outStream) const = 0;

      /// Print information about the bounding function
      virtual void printInfo() const = 0;

      double tauNVal(double t) const;
      double tVal(double tauN) const;
      size_t binTauN(double TauN) const;    ///< The bin in TauN given TauN
      std::pair<double, double> binTauNEdges(size_t tauNBin) const;

      size_t numTauNBins() { return _numTauNBins; };
};


////////////////////////////////////////////////////////////////////////////////
// Inline Definitions
////////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------------------------------
inline double BoundingConstantsBase::tauNVal(double t) const
{
   if (t > 1. - CUTOFF)
      t = 1. - CUTOFF;
   if (t < CUTOFF)
      t = CUTOFF;
   return _tauNMin * pow(_tauNMax / _tauNMin, pow(t, _tauNSampling));
}

//------------------------------------------------------------------------------
inline double BoundingConstantsBase::tVal(double tauN) const
{
   if (tauN < _tauNMin + CUTOFF)
      tauN = _tauNMin + CUTOFF;
   if (tauN > _tauNMax - CUTOFF)
      tauN = _tauNMax - CUTOFF;
   return pow(log(tauN / _tauNMin) / log(_tauNMax / _tauNMin), 1. / _tauNSampling);
}

//------------------------------------------------------------------------------
inline std::pair<double, double> BoundingConstantsBase::binTauNEdges(size_t tauNBin) const
{
   if (tauNBin >= _numTauNBins) {
      Log::error("BoundingConstantsBase::binTauNEdges") << "Bin number " << tauNBin << " is larger than the number of allowed bins numTauNBins = " << _numTauNBins << Log::end;
   }
   return std::pair<double, double>(_binTauNEdges[tauNBin], _binTauNEdges[tauNBin + 1]);
}

//------------------------------------------------------------------------------
inline size_t BoundingConstantsBase::binTauN(double tauN) const
{
   return static_cast<size_t>(tVal(tauN) * static_cast<double>(_numTauNBins));
}

} // namespace Geneva

#undef CUTOFF

#endif // GENEVA_BOUNDING_CONSTANTS_BASE_HPP
