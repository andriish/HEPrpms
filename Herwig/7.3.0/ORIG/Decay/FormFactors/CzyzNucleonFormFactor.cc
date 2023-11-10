// -*- C++ -*-
//
// This is the implementation of the non-inlined, non-templated member
// functions of the CzyzNucleonFormFactor class.
//

#include "CzyzNucleonFormFactor.h"
#include "ThePEG/Interface/ClassDocumentation.h"
#include "ThePEG/EventRecord/Particle.h"
#include "ThePEG/Repository/UseRandom.h"
#include "ThePEG/Repository/EventGenerator.h"
#include "ThePEG/Utilities/DescribeClass.h"
#include "ThePEG/Persistency/PersistentOStream.h"
#include "ThePEG/Persistency/PersistentIStream.h"
#include "Herwig/Decay/ResonanceHelpers.h"

using namespace Herwig;

CzyzNucleonFormFactor::CzyzNucleonFormFactor() {
  // masses and widths of rho resonances
  rhoMasses_   = {775.49*MeV, 1465*MeV, 1720*MeV, 2.12*GeV,2.32647*GeV};
  rhoWidths_   = {149.10*MeV,  400*MeV,  250*MeV, 0.3 *GeV,0.4473 *GeV};
  // masses and width of omega resonances
  omegaMasses_ = {782.65*MeV, 1425*MeV, 1670*MeV, 2.0707 *GeV, 2.34795 *GeV};
  omegaWidths_ = {8.49  *MeV,  215*MeV,  315*MeV, 1.03535*GeV, 1.173975*GeV};
  // c_1 couplings
  c1Re_ = {1.,-0.467,-0.177, 0.301};
  c1Im_ = {0.,-0.385, 0.149, 0.264};
  // c_2 couplings
  c2Re_ = {1., 0.0521, -0.00308,-0.348};
  c2Im_ = {0.,-3.04, 2.38,-0.104};
  // c_3 couplings
  c3Re_ = {1.,-7.88,10.2};
  c3Im_ = {0., 5.67, -1.94};
  // c_4 couplings
  c4Re_ = {1.,-0.832, 0.405};
  c4Im_ = {0., 0.308,-0.25};
  // Magnetic moments
  mup_ =  2.792847356;
  mun_ = -1.9130427;
  // set up the form factors
  addFormFactor(2212,2212,2,2,2,1,1,1);
  addFormFactor(2112,2112,2,2,2,1,1,1);
  initialModes(numberOfFactors());
}

IBPtr CzyzNucleonFormFactor::clone() const {
  return new_ptr(*this);
}

IBPtr CzyzNucleonFormFactor::fullclone() const {
  return new_ptr(*this);
}

void CzyzNucleonFormFactor::doinit() {
  BaryonFormFactor::doinit();
  static const Complex ii(0.,1.);
  // calculate c_1
  c1_.clear();
  assert(c1Re_.size()==4 && c1Im_.size()==4);
  // c_1 1 -> 4
  complex<Energy2> fact(ZERO);
  for(unsigned int ix=0;ix<4;++ix) {
    c1_.push_back(c1Re_[ix]+ii*c1Im_[ix]);
    fact += c1_[ix]*sqr(omegaMasses_[ix]);
  }
  c1_.push_back(-fact/sqr(omegaMasses_[4]));
  // calculate c_2
  c2_.clear();
  assert(c2Re_.size()==4 && c2Im_.size()==4);
  // c_2 1 -> 4
  fact = ZERO;
  for(unsigned int ix=0;ix<4;++ix) {
    c2_.push_back(c2Re_[ix]+ii*c2Im_[ix]);
    fact += c2_[ix]*sqr(rhoMasses_[ix]);
  }
  c2_.push_back(-fact/sqr(rhoMasses_[4]));
  // calculate c_3
  c3_.clear();
  assert(c3Re_.size()==3 && c3Im_.size()==3);
  // c_3 1 -> 4
  fact = ZERO;
  complex<Energy4> fact2(ZERO);
  for(unsigned int ix=0;ix<3;++ix) {
    c3_.push_back(c3Re_[ix]+ii*c3Im_[ix]);
    fact += c3_[ix]*sqr(omegaMasses_[ix]);
    fact2 += c3_[ix]*sqr(omegaMasses_[ix])*
      (sqr(omegaMasses_[ix])-sqr(omegaMasses_[4])
       +ii*(omegaMasses_[4]*omegaWidths_[4]-omegaMasses_[ix]*omegaWidths_[ix]));
  }
  c3_.push_back(fact2/sqr(omegaMasses_[3])/
		(sqr(omegaMasses_[4])-sqr(omegaMasses_[3])
		 +ii*(omegaMasses_[3]*omegaWidths_[3]-omegaMasses_[4]*omegaWidths_[4])) );
  fact += c3_[3]*sqr(omegaMasses_[3]);
  c3_.push_back(-fact/sqr(omegaMasses_[4]));
  // c_4 1 -> 4
  fact = ZERO;
  fact2 = ZERO;
  for(unsigned int ix=0;ix<3;++ix) {
    c4_.push_back(c4Re_[ix]+ii*c4Im_[ix]);
    fact += c4_[ix]*sqr(rhoMasses_[ix]);
    fact2 += c4_[ix]*sqr(rhoMasses_[ix])*
      (sqr(rhoMasses_[ix])-sqr(rhoMasses_[4])
       +ii*(rhoMasses_[4]*rhoWidths_[4]-rhoMasses_[ix]*rhoWidths_[ix]));
  }
  c4_.push_back(fact2/sqr(rhoMasses_[3])/
		(sqr(rhoMasses_[4])-sqr(rhoMasses_[3])
		 +ii*(rhoMasses_[3]*rhoWidths_[3]-rhoMasses_[4]*rhoWidths_[4])) );
  fact += c4_[3]*sqr(rhoMasses_[3]);
  c4_.push_back(-fact/sqr(rhoMasses_[4]));
  // a and b parameters
  a_ = mup_-mun_-1;
  b_ = -mup_-mun_+1.;
}

void CzyzNucleonFormFactor::persistentOutput(PersistentOStream & os) const {
  os << ounit(rhoMasses_,GeV) << ounit(rhoWidths_,GeV)
     << ounit(omegaMasses_,GeV) << ounit(omegaWidths_,GeV)
     << c1Re_ << c1Im_ << c2Re_ << c2Im_
     << c3Re_ << c3Im_ << c4Re_ << c4Im_
     << c1_ << c2_ << c3_ << c4_
     <<  mup_ << mun_ << a_ << b_;
}

void CzyzNucleonFormFactor::persistentInput(PersistentIStream & is, int) {
  is >> iunit(rhoMasses_,GeV) >> iunit(rhoWidths_,GeV)
     >> iunit(omegaMasses_,GeV) >> iunit(omegaWidths_,GeV)
     >> c1Re_ >> c1Im_ >> c2Re_ >> c2Im_
     >> c3Re_ >> c3Im_ >> c4Re_ >> c4Im_
     >> c1_ >> c2_ >> c3_ >> c4_
     >> mup_ >> mun_ >> a_ >> b_;
}

// The following static variable is needed for the type
// description system in ThePEG.
DescribeClass<CzyzNucleonFormFactor,BaryonFormFactor>
describeHerwigCzyzNucleonFormFactor("Herwig::CzyzNucleonFormFactor",
				    "HwFormFactors.so");

void CzyzNucleonFormFactor::Init() {

  static ClassDocumentation<CzyzNucleonFormFactor> documentation
    ("The CzyzNucleonFormFactor class implements the model of "
     "Phys.Rev. D90 (2014) no.11, 114021 for the nucleon form factor",
     "The nucleon form factor model of \\cite{Czyz:2014sha} was used",
     "\\bibitem{Czyz:2014sha}\n"
     "H.~Czyż, J.~H.~Kühn and S.~Tracz,\n"
     "%``Nucleon form factors and final state radiative corrections to $e^+e^-  \\to p\\bar{p}γ$,''\n"
     "Phys.\\ Rev.\\ D {\\bf 90} (2014) no.11,  114021\n"
     "doi:10.1103/PhysRevD.90.114021\n"
     "[arXiv:1407.7995 [hep-ph]].\n"
     "%%CITATION = doi:10.1103/PhysRevD.90.114021;%%\n"
     "%5 citations counted in INSPIRE as of 25 Aug 2018\n");
  
  static ParVector<CzyzNucleonFormFactor,Energy> interfaceRhoMasses
    ("RhoMasses",
     "The masses of the rho mesons for the form factor",
     &CzyzNucleonFormFactor::rhoMasses_, GeV, 5, 1.0*GeV, 0.0*GeV, 10.0*GeV,
     false, false, Interface::limited);
  
  static ParVector<CzyzNucleonFormFactor,Energy> interfaceRhoWidths
    ("RhoWidths",
     "The widths of the rho mesons for the form factor",
     &CzyzNucleonFormFactor::rhoWidths_, GeV, 5, 1.0*GeV, 0.0*GeV, 10.0*GeV,
     false, false, Interface::limited);
  
  static ParVector<CzyzNucleonFormFactor,Energy> interfaceOmegaMasses
    ("OmegaMasses",
     "The masses of the omega mesons for the form factor",
     &CzyzNucleonFormFactor::omegaMasses_, GeV, 5, 1.0*GeV, 0.0*GeV, 10.0*GeV,
     false, false, Interface::limited);
  
  static ParVector<CzyzNucleonFormFactor,Energy> interfaceOmegaWidths
    ("OmegaWidths",
     "The widths of the omega mesons for the form factor",
     &CzyzNucleonFormFactor::omegaWidths_, GeV, 5, 1.0*GeV, 0.0*GeV, 10.0*GeV,
     false, false, Interface::limited);

  static ParVector<CzyzNucleonFormFactor,double> interfacec1Real
    ("c1Real",
     "The real part of the c_1 coupling",
     &CzyzNucleonFormFactor::c1Re_, 4, 1., -100., 100.0,
     false, false, Interface::limited);

  static ParVector<CzyzNucleonFormFactor,double> interfacec1Imag
    ("c1Imag",
     "The imaginary part of the c_1 coupling",
     &CzyzNucleonFormFactor::c1Im_, 4, 1., -100., 100.0,
     false, false, Interface::limited);

  static ParVector<CzyzNucleonFormFactor,double> interfacec2Real
    ("c2Real",
     "The real part of the c_2 coupling",
     &CzyzNucleonFormFactor::c2Re_, 4, 1., -100., 100.0,
     false, false, Interface::limited);

  static ParVector<CzyzNucleonFormFactor,double> interfacec2Imag
    ("c2Imag",
     "The imaginary part of the c_2 coupling",
     &CzyzNucleonFormFactor::c2Im_, 4, 1., -100., 100.0,
     false, false, Interface::limited);

  static ParVector<CzyzNucleonFormFactor,double> interfacec3Real
    ("c3Real",
     "The real part of the c_3 coupling",
     &CzyzNucleonFormFactor::c3Re_, 3, 1., -100., 100.0,
     false, false, Interface::limited);

  static ParVector<CzyzNucleonFormFactor,double> interfacec3Imag
    ("c3Imag",
     "The imaginary part of the c_3 coupling",
     &CzyzNucleonFormFactor::c3Im_, 3, 1., -100., 100.0,
     false, false, Interface::limited);

  static ParVector<CzyzNucleonFormFactor,double> interfacec4Real
    ("c4Real",
     "The real part of the c_4 coupling",
     &CzyzNucleonFormFactor::c4Re_, 3, 1., -100., 100.0,
     false, false, Interface::limited);

  static ParVector<CzyzNucleonFormFactor,double> interfacec4Imag
    ("c4Imag",
     "The imaginary part of the c_4 coupling",
     &CzyzNucleonFormFactor::c4Im_, 3, 1., -100., 100.0,
     false, false, Interface::limited);

  static Parameter<CzyzNucleonFormFactor,double> interfaceMuProton
    ("MuProton",
     "The proton magnetic moment",
     &CzyzNucleonFormFactor::mup_, 2.792, 0.0, 10.0,
     false, false, Interface::limited);

  static Parameter<CzyzNucleonFormFactor,double> interfaceMuNeutron
    ("MuNeutron",
     "The proton magnetic moment",
     &CzyzNucleonFormFactor::mun_, -1.913, 0.0, 10.0,
     false, false, Interface::limited);
}


void CzyzNucleonFormFactor::
SpinHalfSpinHalfFormFactor(Energy2 q2,int iloc, int id0,int id1,Energy,Energy,
			   Complex & f1v,Complex & f2v,Complex & f3v,
			   Complex & f1a,Complex & f2a,Complex & f3a,
			   FlavourInfo flavour,
			   Virtuality virt) {
  f1a = f2a = f3a = f1v = f2v = f3v = 0.;
  assert(abs(id0)==abs(id1));
  if(iloc==0) assert(abs(id0)==2212);
  else        assert(abs(id0)==2112);
  assert(virt==TimeLike);
  if(flavour.strange != Strangeness::Unknown and flavour.strange != Strangeness::Zero) return;
  if(flavour.charm   != Charm::Unknown       and flavour.charm   != Charm::Zero      ) return;
  if(flavour.bottom  != Beauty::Unknown      and flavour.bottom  !=Beauty::Zero      ) return;
  bool I0 = true, I1 = true;
  if(flavour.I!=IsoSpin::IUnknown) {
    if(flavour.I==IsoSpin::IZero) {
      I1=false;
      if(flavour.I3!=IsoSpin::I3Unknown and flavour.I3!=IsoSpin::I3Zero) return;
    }
    else if(flavour.I==IsoSpin::IOne) {
      I0=false;
      if(flavour.I3!=IsoSpin::I3Unknown and flavour.I3!=IsoSpin::I3Zero) return;
    }
  }
  if(flavour.I3!=IsoSpin::I3Unknown and flavour.I3!=IsoSpin::I3Zero) return;
  // calculate the form factors
  Complex F1S(0.),F1V(0.),F2S(0.),F2V(0.);
  Complex n1(0.),n2(0.),n3(0.),n4(0.);
  for(unsigned int ix=0;ix<5;++ix) {
    if(I0) {
      F1S += c1_[ix]*Resonance::BreitWignerFW(q2,omegaMasses_[ix],omegaWidths_[ix]);
      F2S += c3_[ix]*Resonance::BreitWignerFW(q2,omegaMasses_[ix],omegaWidths_[ix]);
    }
    if(I1) {
      F1V += c2_[ix]*Resonance::BreitWignerFW(q2,  rhoMasses_[ix],  rhoWidths_[ix]);
      F2V += c4_[ix]*Resonance::BreitWignerFW(q2,  rhoMasses_[ix],  rhoWidths_[ix]);
    }
    n1 += c1_[ix];
    n2 += c2_[ix];
    n3 += c3_[ix];
    n4 += c4_[ix];
  }
  F1S *=  0.5   /n1;
  F1V *=  0.5   /n2;
  F2S *= -0.5*b_/n3;
  F2V *=  0.5*a_/n4;
  if(iloc==0) {
    f1v =  F1S + F1V;
    f2v =  F2S + F2V;
  }
  else {
    f1v = F1S - F1V;
    f2v = F2S - F2V;
  }
}

void CzyzNucleonFormFactor::
dataBaseOutput(ofstream& output,bool header,
	       bool create) const {
  if(header) output << "update decayers set parameters=\"";
  if(create) output << "create Herwig::CzyzNucleonFormFactor " 
		    << name() << " \n";
  for(unsigned int ix=0;ix<5;++ix) {
    output << "newdef " << name() << ":RhoMasses " << ix << " "
	   << rhoMasses_[ix]/GeV << "\n";
    output << "newdef " << name() << ":RhoWidths " << ix << " "
	   << rhoWidths_[ix]/GeV << "\n";
    output << "newdef " << name() << ":OmegaMasses " << ix << " "
	   << omegaMasses_[ix]/GeV << "\n";
    output << "newdef " << name() << ":OmegaWidths " << ix << " "
	   << omegaWidths_[ix]/GeV << "\n";
  }
  for(unsigned int ix=0;ix<4;++ix) {
    output << "newdef " << name() << ":c1Real " << ix << " "
	   << c1Re_[ix] << "\n";
    output << "newdef " << name() << ":c1Imag " << ix << " "
	   << c1Im_[ix] << "\n";
    output << "newdef " << name() << ":c2Real " << ix << " "
	   << c2Re_[ix] << "\n";
    output << "newdef " << name() << ":c2Imag " << ix << " "
	   << c2Im_[ix] << "\n";
  }
  for(unsigned int ix=0;ix<3;++ix) {
    output << "newdef " << name() << ":c3Real " << ix << " "
	   << c3Re_[ix] << "\n";
    output << "newdef " << name() << ":c3Imag " << ix << " "
	   << c3Im_[ix] << "\n";
    output << "newdef " << name() << ":c4Real " << ix << " "
	   << c4Re_[ix] << "\n";
    output << "newdef " << name() << ":c4Imag " << ix << " "
	   << c4Im_[ix] << "\n";
  }
  output << "newdef " << name() << ":MuProton  " << mup_ << "\n";
  output << "newdef " << name() << ":MuNeutron " << mun_ << "\n";
  BaryonFormFactor::dataBaseOutput(output,false,false);
  if(header) output << "\n\" where BINARY ThePEGName=\"" 
		    << fullName() << "\";" << endl;
}
