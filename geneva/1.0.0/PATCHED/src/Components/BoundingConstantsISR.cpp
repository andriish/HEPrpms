//------------------------------------------------------------------------------
/// \file BoundingConstantsISR.cpp
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
//    Implementation of class BoundingConstantsISR
//------------------------------------------------------------------------------

#include "Geneva/Components/BoundingConstantsISR.hpp"

#include "Geneva/Core/UI/Log.hpp"
#include "Geneva/Core/Utils/NumericsUtils.hpp"

#include <istream>
#include <ostream>
#include <iomanip>
#include <string>

namespace Geneva
{

//------------------------------------------------------------------------------
BoundingConstantsISR::BoundingConstantsISR(size_t numTauNBins, size_t numZBins, size_t numXBins, double tauNMin, double tauNMax, double zMin, double zMax, double xMin, double xMax)
  : BoundingConstantsBase(numTauNBins, tauNMin, tauNMax), _numZBins(numZBins), _numXBins(numXBins), _zMin(zMin), _zMax(zMax), _xMin(xMin), _xMax(xMax), _xSampling(2)
{}

//------------------------------------------------------------------------------
BoundingConstantsISR::BoundingConstantsISR(std::istream& initStream)
{
   initStream >> _numTauNBins;
   initStream >> _tauNMin;
   initStream >> _tauNMax;
   initStream >> _tauNSampling;
   _binTauNEdges.reserve(_numTauNBins+1);
   _binTauNEdges.clear();
   for (size_t i = 0; i <= _numTauNBins; ++i) _binTauNEdges.push_back(0);
   for (size_t bin = 0; bin <= _numTauNBins; bin++) {
      initStream >> _binTauNEdges[bin];
   }

   initStream >> _numZBins;
   initStream >> _numXBins;
   initStream >> _zMin;
   initStream >> _zMax;
   initStream >> _xMin;
   initStream >> _xMax;
   initStream >> _xSampling;

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
      if (radType == "ISRA") {
         if (_boundA.count(motherID)) {
            Log::fatal("BoundingConstantsISR::BoundingConstantsISR") << "Trying to initialize the same EmittedPair twice" << Log::end;
         } else {
            std::vector<double> arrayTauNZX(_numTauNBins* _numZBins * _numXBins);
            for (size_t tauNBin = 0; tauNBin < _numTauNBins; ++tauNBin) {
               for (size_t zBin = 0; zBin < _numZBins; ++zBin) {
                  for (size_t xBin = 0; xBin < _numXBins; ++xBin) {
                     initStream >> arrayTauNZX[tauNBin + _numTauNBins * zBin + _numZBins *_numTauNBins * xBin];
                  }
               }
            }
            _boundA[motherID] = arrayTauNZX;
         }
      } else if (radType == "ISRB") {
         if (_boundB.count(motherID)) {
               Log::fatal("BoundingConstantsISR::BoundingConstantsISR") << "Trying to initialize the same EmittedPair twice" << Log::end;
         } else {
            std::vector<double> arrayTauNZX(_numTauNBins* _numZBins * _numXBins);
            for (size_t tauNBin = 0; tauNBin < _numTauNBins; ++tauNBin) {
               for (size_t zBin = 0; zBin < _numZBins; ++zBin) {
                  for (size_t xBin = 0; xBin < _numXBins; ++xBin) {
                     initStream >> arrayTauNZX[tauNBin + _numTauNBins * zBin + _numZBins *_numTauNBins * xBin];
                  }
               }
            }
            _boundB[motherID] = arrayTauNZX;
         }
      } else {
         Log::fatal("BoundingConstantsISR::BoundingConstantsISR") << "Radiation Type can only be ISRA or ISRB, and not " << radType << Log::end;
      }
   }
}

//------------------------------------------------------------------------------
double BoundingConstantsISR::bound(Signature::iterator mother, size_t tauNBin, size_t zBin, size_t xBin)
{
   if (mother->isInitialA()) {
      std::map<PdgID, std::vector<double> >::iterator it = _boundA.find(*mother);
      if (it != _boundA.end()) {
         return _boundFactor * it->second[tauNBin + _numTauNBins * zBin + _numZBins *_numTauNBins * xBin];
      } else {
         Log::fatal("BoundingConstantsISR::bound") << "ISRA Bounding grid does not have entry for flavor " << *mother << Log::end;
         return 0.;
      }
   } else if (mother->isInitialB()) {
      std::map<PdgID, std::vector<double> >::iterator it = _boundB.find(*mother);
      if (it != _boundB.end()) {
         return _boundFactor * it->second[tauNBin + _numTauNBins * zBin + _numZBins *_numTauNBins * xBin];
      } else {
         Log::fatal("BoundingConstantsISR::bound") << "ISRB Bounding grid does not have entry for flavor " << *mother << Log::end;
         return 0.;
      }
   } else {
      Log::fatal("BoundingConstantsISR::bound") << "Radiation Type can only be ISRA or ISRB" << Log::end;
      return 0.;
   }
}

//------------------------------------------------------------------------------
bool BoundingConstantsISR::boundExists(Signature::iterator mother) const
{
   if (mother->isInitialA()) {
      return _boundA.count(*mother);
   } else if (mother->isInitialB()) {
      return _boundB.count(*mother);
   } else {
      Log::fatal("BoundingConstantsISR::boundExists") << "Radiation Type can only be ISRA or ISRB" << Log::end;
      return false;
   }
}

//------------------------------------------------------------------------------
void BoundingConstantsISR::informOfRatio(Signature::iterator mother, size_t tauNBin, size_t zBin, size_t xBin, double ratio)
{
   if (!std::isfinite(ratio)) {
      Log::warning("BoundingConstantsISR::informOfRatio") << "ratio not finite" << Log::end;
      return;
   }
   //If we have not encountered a given emitted, sister pair, initialize the bouding constant to zero
   std::string rad;
   if (mother->isInitialA()) {
      if (_boundA.find(*mother) == _boundA.end()) {
         std::vector<double> arrayTauNZX(_numTauNBins * _numZBins * _numXBins);
         _boundA[*mother] = arrayTauNZX;
      }
      if (ratio > _boundA.at(*mother)[tauNBin + _numTauNBins * zBin + _numZBins *_numTauNBins * xBin]) {
         _boundA.at(*mother)[tauNBin + _numTauNBins * zBin + _numZBins *_numTauNBins * xBin] = ratio;
      }
      rad = "ISRA";
   } else if (mother->isInitialB()) {
      if (_boundB.find(*mother) == _boundB.end()) {
         std::vector<double> arrayTauNZX(_numTauNBins * _numZBins * _numXBins);
         _boundB[*mother] = arrayTauNZX;
      }
      if (ratio > _boundB.at(*mother)[tauNBin + _numTauNBins * zBin + _numZBins *_numTauNBins * xBin]) {
         _boundB.at(*mother)[tauNBin + _numTauNBins * zBin + _numZBins *_numTauNBins * xBin] = ratio;
      }
      rad = "ISRB";
   } else {
      Log::fatal("BoundingConstantsISR::informOfRatioISR") << "Can not call this function for an FSR emission" << Log::end;
      rad = "";
   }
}

//------------------------------------------------------------------------------
void BoundingConstantsISR::printInfo() const
{
   Log::debug() << "Information about the bounding constant" << Log::end;
   Log::debug() << "numTauNBins = " << _numTauNBins << Log::end;
   Log::debug() << "tauNSampling = " << _tauNSampling << Log::end;
   Log::debug() << "numZBins = " << _numZBins << Log::end;
   Log::debug() << "numXBins (including underflow) = " << _numXBins << Log::end;
   Log::debug() << "zMin = " << _zMin << Log::end;
   Log::debug() << "zMax = " << _zMax << Log::end;
   Log::debug() << "xMin = " << _xMin << Log::end;
   Log::debug() << "xMax = " << _xMax << Log::end;
   Log::debug() << "xSampling = " << _xSampling << Log::end;
   Log::debug() << "----------------" << Log::end;
   Log::debug() << "Bounding array in in TauN,  z (V) and x (>)" << Log::end;
   Log::debug() << std::setiosflags(std::ios::fixed) << std::setprecision(5) << std::setw(22);
   for (std::map<PdgID, std::vector<double> >::const_iterator it = _boundA.begin(); it != _boundA.end(); ++it) {
      Log::debug() << "ISRA" << ", " << it->first.code() << Log::end;
      for (size_t tauNBin = 0; tauNBin < _numTauNBins; tauNBin++) {
         for (size_t zBin = 0; zBin < _numZBins; zBin++) {
            for (size_t xBin = 0; xBin < _numXBins; xBin++) {
               Log::debug() << '\t' << (it->second)[tauNBin + _numTauNBins * zBin + _numZBins *_numTauNBins * xBin];
            }
            Log::debug() << '\n';
         }
         Log::debug() << '\n';
      }
   }
   for (std::map<PdgID, std::vector<double> >::const_iterator it = _boundB.begin(); it != _boundB.end(); ++it) {
      Log::debug() << "ISRB" << ", " << it->first.code() << Log::end;
      for (size_t tauNBin = 0; tauNBin < _numTauNBins; tauNBin++) {
         for (size_t zBin = 0; zBin < _numZBins; zBin++) {
            for (size_t xBin = 0; xBin < _numXBins; xBin++) {
               Log::debug() << '\t' << (it->second)[tauNBin + _numTauNBins * zBin + _numZBins *_numTauNBins * xBin];
            }
            Log::debug() << '\n';
         }
         Log::debug() << '\n';
      }
   }
}

//------------------------------------------------------------------------------
size_t BoundingConstantsISR::binX(double x)
{
   forceInRange(x, _xMin, 1.0," BoundingConstantsISR::binX");
   return static_cast<size_t>(static_cast<double>(_numXBins) * wVal(x));
}

//------------------------------------------------------------------------------
size_t BoundingConstantsISR::binZ(double z, double x)
{
   forceInRange(z, x, _zMax, "BoundingConstantsISR::binZ");
   return static_cast<size_t>(static_cast<double>(_numZBins) * vVal(z, x));
}

//------------------------------------------------------------------------------
void BoundingConstantsISR::write(std::ostream& outStream) const
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
   outStream << _numXBins << ' ';
   outStream << _zMin << ' ';
   outStream << _zMax << ' ';
   outStream << _xMin << ' ';
   outStream << _xMax << ' ';
   outStream << _xSampling;
   outStream << '\n';
   for (std::map<PdgID, std::vector<double> >::const_iterator it = _boundA.begin(); it != _boundA.end(); ++it) {
      outStream << "ISRA" << " " << it->first.code() << '\n';
      for (size_t tauNBin = 0; tauNBin < _numTauNBins; tauNBin++) {
         for (size_t zBin = 0; zBin < _numZBins; ++zBin) {
            for (size_t xBin = 0; xBin < _numXBins; ++xBin) {
               outStream << _boundA.at(it->first)[tauNBin + _numTauNBins * zBin + _numZBins *_numTauNBins * xBin] << ' ';
            }
            outStream << '\n';
         }
         outStream << '\n';
      }
   }
   for (std::map<PdgID, std::vector<double> >::const_iterator it = _boundB.begin(); it != _boundB.end(); ++it) {
      outStream << "ISRB" << " " << it->first.code() << '\n';
      for (size_t tauNBin = 0; tauNBin < _numTauNBins; tauNBin++) {
         for (size_t zBin = 0; zBin < _numZBins; ++zBin) {
            for (size_t xBin = 0; xBin < _numXBins; ++xBin) {
               outStream << _boundB.at(it->first)[tauNBin + _numTauNBins * zBin + _numZBins *_numTauNBins * xBin] << ' ';
            }
            outStream << '\n';
         }
         outStream << '\n';
      }
   }
}

//------------------------------------------------------------------------------
std::pair<double, double> BoundingConstantsISR::binXEdges(size_t xBin) const
{
   if (xBin >= _numXBins) {
      Log::error("BoundingConstantsISR::binXEdges") << "Bin number " << xBin << " is larger than the number of allowed bins numXBins = " << _numXBins << Log::end;
   }
   const double wLow = static_cast<double>(xBin) / static_cast<double>(_numXBins);
   const double wHigh = static_cast<double>(xBin + 1) / static_cast<double>(_numXBins);
   const double xLow = xVal(wLow);
   const double xHigh = xVal(wHigh);
   return std::pair<double, double>(xLow, xHigh);
}

//------------------------------------------------------------------------------
std::pair<double, double> BoundingConstantsISR::binZEdges(size_t zBin, double x) const
{
   if (zBin >= _numZBins) {
      Log::error("BoundingConstantsISR::binZEdges") << "Bin number " << zBin << " is larger than the number of allowed bins numZBins = " << _numZBins << Log::end;
   }
   const double vLow = static_cast<double>(zBin) / static_cast<double>(_numZBins);
   const double vHigh = static_cast<double>(zBin + 1) / static_cast<double>(_numZBins);
   const double w = wVal(x);
   const double zLow = zVal(vLow, w);
   const double zHigh = zVal(vHigh, w);
   return std::pair<double, double>(zLow, zHigh);
}

//------------------------------------------------------------------------------
double BoundingConstantsISR::xVal(double w) const
{
   double x = pow(_xMin, 1. - pow(w, _xSampling));
   forceInRange(x, _xMin, _xMax, "BoundingConstantsISR::xVal");
   return x;
}

//------------------------------------------------------------------------------
double BoundingConstantsISR::zVal(double v, double w) const
{
   double z = v + pow(_xMin, 1. - pow(w, _xSampling)) * (1. - v);
   forceInRange(z, _zMin, _zMax, "BoundingConstantsISR::zVal");
   return z;
}

//------------------------------------------------------------------------------
double BoundingConstantsISR::wVal(double x) const
{
   return pow(log(x / _xMin) / log(1. / _xMin), 1. / _xSampling);
}

//------------------------------------------------------------------------------
double BoundingConstantsISR::vVal(double z, double x) const
{
   return (z - x) / (1. - x);
}

} // namespace Geneva
