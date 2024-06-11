//------------------------------------------------------------------------------
/// \file BoundingConstantFsr.cpp
//
// Author(s):
//    Christian Bauer, Simone Alioli
//
// Copyright:
//    Copyright (C) 2014 <author's copyright holder>
//
//    This file is part of the Geneva MC framework. Geneva is distributed under
//    the terms of the GNU General Public License version 3 (GPLv3), see the
//    COPYING file that comes with this distribution for details.
//    Please respect the academic usage guidelines in the GUIDELINES file.
//
// Description:
//    Implementation of class BoundingConstantsFSR
//------------------------------------------------------------------------------

#include "Geneva/Components/BoundingConstantsFSR.hpp"

#include "Geneva/Core/UI/Log.hpp"

#include <istream>
#include <ostream>
#include <iomanip>
#include <string>

namespace Geneva
{

//------------------------------------------------------------------------------
BoundingConstantsFSR::BoundingConstantsFSR(size_t numTauNBins, size_t numZBins, double tauNMin, double tauNMax, double zMin, double zMax)
   : BoundingConstantsBase(numTauNBins, tauNMin, tauNMax), _numZBins(numZBins), _zMin(zMin), _zMax(zMax)
{
   _binSizeZ = (_zMax - _zMin) / static_cast<double>(_numZBins);
   _binZEdges.reserve(_numZBins + 1);
   for (size_t i = 0; i <= _numZBins; ++i) {
      _binZEdges[i] = _zMin + static_cast<double>(i) * _binSizeZ;
   }
}

//------------------------------------------------------------------------------
BoundingConstantsFSR::BoundingConstantsFSR(std::istream& initStream)
{
   initStream >> _numTauNBins;
   initStream >> _tauNMin;
   initStream >> _tauNMax;
   initStream >> _tauNSampling;
   _binTauNEdges.reserve(_numTauNBins+1);
   for (size_t bin = 0; bin <= _numTauNBins; bin++) {
      initStream >> _binTauNEdges[bin];
   }

   initStream >> _numZBins;
   initStream >> _zMin;
   initStream >> _zMax;
   initStream >> _binSizeZ;
   _binZEdges.reserve(_numZBins + 1);
   for (size_t bin = 0; bin <= _numZBins; bin++) {
      initStream >> _binZEdges[bin];
   }

   while (initStream.good()) {
      //First create the EmittedPair
      std::string radType;
      int motherIDcode;
      initStream >> radType;
      if (radType == "") {
         break;
      }
      initStream >> motherIDcode;
      PdgID motherID(motherIDcode);
      if (radType == "FSR") {
         if (_bound.count(motherID)) {
            Log::fatal("BoundingConstantsFSR::BoundingConstantsFSR") << "Trying to initialize the same EmittedPair twice" << Log::end;
         } else {
            std::vector<double> arrayTauNZ(_numTauNBins * _numZBins);
            for (size_t tauNBin = 0; tauNBin < _numTauNBins; ++tauNBin) {
               for (size_t zBin = 0; zBin < _numZBins; ++zBin) {
                  initStream >> arrayTauNZ[tauNBin + _numTauNBins * zBin];
               }
            }
            _bound[motherID] = arrayTauNZ;
         }
      } else {
         Log::fatal("BoundingConstantsFSR::BoundingConstantsFSR") << "Radiation Type can only be FSR, and not " << radType << Log::end;
      }
   }
}

//------------------------------------------------------------------------------
double BoundingConstantsFSR::bound(Signature::iterator mother, size_t tauNBin, size_t zBin)
{
   if (mother->isFinal()) {
      std::map<PdgID, std::vector<double> >::iterator it = _bound.find(*mother);
      if (it != _bound.end()) {
         return _boundFactor * it->second[tauNBin + _numTauNBins * zBin];
      } else {
         Log::fatal("BoundingConstantsFSR::bound") << "FSR Bounding grid does not have entry for flavor " << *mother << Log::end;
         return 0.;
      }
   } else {
      Log::fatal("BoundingConstantsFSR::bound") << "Calling bound with an emitter that is not FSR" << Log::end;
      return 0.;
   }
}

//------------------------------------------------------------------------------
bool BoundingConstantsFSR::boundExists(Signature::iterator mother) const
{
   if (mother->isFinal()) {
      return _bound.count(*mother);
   } else {
      Log::fatal("BoundingConstantsFSR::boundExists") << "Radiation Type can only be FSR" << Log::end;
      return false;
   }
}

//------------------------------------------------------------------------------
void BoundingConstantsFSR::informOfRatio(Signature::iterator mother, size_t tauNBin, size_t zBin, double ratio)
{
   if (!std::isfinite(ratio)) {
      Log::warning("BoundingConstantsFSR::informOfRatio") << "ratio not finite" << Log::end;
      return;
   }
   //If we have not encountered a given emitted, sister pair, initialize the bouding constant to zero
   std::string rad;
   if (mother->isFinal()) {
      if (_bound.find(*mother) == _bound.end()) {
         std::vector<double> arrayTauNZ(_numTauNBins * _numZBins);
         _bound[*mother] = arrayTauNZ;
      }
      if (ratio > _bound.at(*mother)[tauNBin + _numTauNBins * zBin]) {
         _bound.at(*mother)[tauNBin + _numTauNBins * zBin] = ratio;
      }
      rad = "FSR";
   } else {
      Log::fatal("BoundingConstantsFSR::informOfRatio") << "Can not call this function for an ISR emission" << Log::end;
      rad = "";
   }
}

//------------------------------------------------------------------------------
void BoundingConstantsFSR::printInfo() const
{
   Log::debug() << "Information about the bounding constant" << Log::end;
   Log::debug() << "numTauNBins = " << _numTauNBins << Log::end;
   Log::debug() << "tauNSampling = " << _tauNSampling << Log::end;
   Log::debug() << "numZBins = " << _numZBins << Log::end;
   Log::debug() << "zMin = " << _zMin << Log::end;
   Log::debug() << "zMax = " << _zMax << Log::end;
   Log::debug() << "binSizeZ = " << _binSizeZ << Log::end;
   Log::debug() << "----------------" << Log::end;
   Log::debug() << "Bounding array in TauN and z" << Log::end;
   for (std::map<PdgID, std::vector<double> >::const_iterator it = _bound.begin(); it != _bound.end(); ++it) {
      Log::debug() << "FSR" << ", " << it->first << Log::end;
      for (size_t tauNBin = 0; tauNBin < _numTauNBins; tauNBin++) {
         for (size_t zBin = 0; zBin < _numZBins; zBin++) {
            Log::debug() << "\t" << (it->second)[tauNBin + _numTauNBins * zBin];
         }
         Log::debug() << '\n';
      }
   }
}

//------------------------------------------------------------------------------
size_t BoundingConstantsFSR::binZ(double z)
{
   // for simplicity under and overflow are included in first or last bin
   if (z < _zMin) {
      return 0;
   }
   if (z >= _zMax) {
      return _numZBins - 1;
   }
   return static_cast<size_t>((z - _zMin) / _binSizeZ);
}

//------------------------------------------------------------------------------
void BoundingConstantsFSR::write(std::ostream& outStream) const
{
   outStream << std::setprecision(12);
   outStream << _numTauNBins << ' ';
   outStream << _tauNMin << ' ';
   outStream << _tauNMax << ' ';
   outStream << _tauNSampling << ' ';
   for (size_t bin = 0; bin <= _numTauNBins; bin++) {
      outStream << _binTauNEdges[bin] << ' ';
   }
   outStream << '\n';
   outStream << _numZBins << ' ';
   outStream << _zMin << ' ';
   outStream << _zMax << ' ';
   outStream << _binSizeZ << '\n';
   for (size_t bin = 0; bin <= _numZBins; bin++) {
      outStream << _binZEdges[bin] << ' ';
   }
   outStream << '\n';
   for (std::map<PdgID, std::vector<double> >::const_iterator it = _bound.begin(); it != _bound.end(); ++it) {
      outStream << "FSR" << " " << it->first.code() << '\n';
      for (size_t tauNBin = 0; tauNBin < _numTauNBins; tauNBin++) {
         for (size_t zBin = 0; zBin < _numZBins; ++zBin) {
            outStream << _bound.at(it->first)[tauNBin + _numTauNBins * zBin] << ' ';
         }
         outStream << '\n';
      }
   }
}

//------------------------------------------------------------------------------
std::pair<double, double> BoundingConstantsFSR::binZEdges(size_t zBin)
{
   const double zlow = _zMin + static_cast<double>(zBin) / static_cast<double>(_numZBins) * (_zMax - _zMin);
   const double zhigh = _zMin + static_cast<double>(zBin + 1) / static_cast<double>(_numZBins) * (_zMax - _zMin);
   return std::pair<double, double>(zlow, zhigh);
}

} // namespace Geneva
