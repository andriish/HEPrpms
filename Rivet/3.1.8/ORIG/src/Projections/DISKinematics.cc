// -*- C++ -*-
#include "Rivet/Projections/DISKinematics.hh"
#include "Rivet/Math/Constants.hh"

namespace Rivet {


  void DISKinematics::project(const Event& e) {

    // Find appropriate DIS leptons
    const DISLepton& dislep = applyProjection<DISLepton>(e, "Lepton");
    if ( dislep.failed() ) {
      fail();
      return;
    }
    _outLepton = dislep.out();

    // Identify beam hadron
    const ParticlePair& inc = applyProjection<Beam>(e, "Beam").beams();
    const bool firstIsHadron  = PID::isHadron(inc.first.pid());
    const bool secondIsHadron = PID::isHadron(inc.second.pid());
    if (firstIsHadron && !secondIsHadron) {
      _inHadron = inc.first;
      _inLepton = dislep.in(); // inc.second;
    } else if (!firstIsHadron && secondIsHadron) {
      _inHadron = inc.second;
      _inLepton = dislep.in(); // inc.first;
    } else {
      fail();
      return;
    }

    // Get the DIS lepton and store some of its properties
    const FourMomentum pHad = _inHadron.momentum();
    const FourMomentum pLepIn = _inLepton.momentum();
    const FourMomentum pLepOut = _outLepton.momentum();
    const FourMomentum pGamma = pLepIn - pLepOut;
    const FourMomentum tothad = pGamma + pHad;
    _theQ2 = -pGamma.mass2();
    _theW2 = tothad.mass2();

    // Compute x and y, preferrably by numerically safe expressions, neglecting masses of incoming particles
    //_theX = Q2()/(2.0*(pGamma * pHad));
    //_theY = (pGamma * pHad) / (pLepIn * pHad);
    // cout << "Old: " << x() << "," << y() << endl;
    _theX = Q2()/(Q2()+W2());
    if (_theX != 0) {
      _theY = Q2() / ( (pLepIn * pHad) * 2.0 * x());
    } else {
      _theY = (pGamma * pHad) / (pLepIn * pHad);
    }
    // cout << "New: " << x() << "," << y() << endl;

    _theS = invariant(pLepIn + pHad);
    double cosgammah =
      ( (1 - _theY) * _theX * pHad.E() - _theY * pLepIn.E() ) /
      ( (1 - _theY) * _theX * pHad.E() + _theY * pLepIn.E());
    _theGH = acos(cosgammah);

    // Calculate boost vector for boost into HCM-system
    LorentzTransform tmp;
    tmp.setBetaVec(-tothad.betaVec());

    // Rotate so the photon is in x-z plane in HCM rest frame
    FourMomentum pGammaHCM = tmp.transform(pGamma);
    tmp.preMult(Matrix3(Vector3::mkZ(), -pGammaHCM.azimuthalAngle()));
    pGammaHCM = tmp.transform(pGamma);
    assert(isZero(dot(pGammaHCM.vector3(), Vector3::mkY())));

    // Rotate so the photon is along the positive z-axis
    const double rot_angle = pGammaHCM.polarAngle() * (pGammaHCM.px() >= 0 ? -1 : 1);
    tmp.preMult(Matrix3(Vector3::mkY(), rot_angle));

    // Check that final HCM photon lies along +ve z as expected
    pGammaHCM = tmp.transform(pGamma);
    assert(isZero(dot(pGammaHCM.vector3(), Vector3::mkX()), 1e-3));
    assert(isZero(dot(pGammaHCM.vector3(), Vector3::mkY()), 1e-3));
    assert(isZero(angle(pGammaHCM.vector3(), Vector3::mkZ()), 1e-3));

    // Finally rotate so outgoing lepton at phi = 0
    FourMomentum pLepOutHCM = tmp.transform(pLepOut);
    tmp.preMult(Matrix3(Vector3::mkZ(), -pLepOutHCM.azimuthalAngle()));
    assert(isZero(tmp.transform(pLepOut).azimuthalAngle()));
    _hcm = tmp;

    // Boost to Breit frame (use opposite convention for photon --- along *minus* z)
    tmp.preMult(Matrix3(Vector3::mkX(), PI));
    const double bz = 1 - 2*x();
    _breit = LorentzTransform::mkObjTransformFromBeta(Vector3::mkZ() * bz).combine(tmp);
    assert(isZero(angle(_breit.transform(pGamma).vector3(), -Vector3::mkZ()), 1e-3));
    assert(isZero(_breit.transform(pLepOut).azimuthalAngle(), 1e-3));
  }


  CmpState DISKinematics::compare(const Projection & p) const {
    const DISKinematics& other = pcast<DISKinematics>(p);
    return mkNamedPCmp(other, "Lepton");
  }


}
