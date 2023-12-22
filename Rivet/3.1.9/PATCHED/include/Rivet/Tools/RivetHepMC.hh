// -*- C++ -*-
#ifndef RIVET_RivetHepMC_HH
#define RIVET_RivetHepMC_HH

#include "Rivet/Config/RivetConfig.hh"
#include "Rivet/Tools/RivetSTL.hh"
#include "Rivet/Tools/Exceptions.hh"
#include <valarray>


#ifdef RIVET_ENABLE_HEPMC_3

#include "HepMC3/HepMC3.h"
#include "HepMC3/Relatives.h"
#include "HepMC3/Reader.h"

#include "HepMC3/Version.h"
#if HEPMC3_VERSION_CODE > 3002007
#include "HepMC3/ReaderFactory.h"
#else
namespace HepMC3 {
  std::shared_ptr<HepMC3::Reader> deduce_reader(const std::string &filename);
  std::shared_ptr<HepMC3::Reader> deduce_reader(std::istream &stream);
}
#endif

#ifndef HEPMC_HAS_CROSS_SECTION
#define HEPMC_HAS_CROSS_SECTION
#endif

namespace Rivet {
  namespace RivetHepMC = HepMC3;

  using RivetHepMC::ConstGenParticlePtr;
  using RivetHepMC::ConstGenVertexPtr;
  using RivetHepMC::Relatives;
  using RivetHepMC::ConstGenHeavyIonPtr;

  using HepMC_IO_type = RivetHepMC::Reader;
  using PdfInfo = RivetHepMC::GenPdfInfo;
}

#else // HEPMC_2

#include "HepMC/GenEvent.h"
#include "HepMC/GenParticle.h"
#include "HepMC/HeavyIon.h"
#include "HepMC/GenVertex.h"
#include "HepMC/Version.h"
#include "HepMC/GenRanges.h"
#include "HepMC/IO_GenEvent.h"
namespace Rivet {
  namespace RivetHepMC = HepMC;

  // HepMC 2.07 provides its own #defines
  typedef const HepMC::GenParticle* ConstGenParticlePtr;
  typedef const HepMC::GenVertex* ConstGenVertexPtr;
  typedef const HepMC::HeavyIon* ConstGenHeavyIonPtr;

  /// @brief Replicated the HepMC3 Relatives syntax using HepMC2 IteratorRanges
  /// This is necessary mainly because of capitalisation differences
  class Relatives {
  public:

    constexpr Relatives(HepMC::IteratorRange relo): _internal(relo){}

    constexpr HepMC::IteratorRange operator()() const {return _internal;}
    operator HepMC::IteratorRange() const {return _internal;}

    const static Relatives PARENTS;
    const static Relatives CHILDREN;
    const static Relatives ANCESTORS;
    const static Relatives DESCENDANTS;

    private:
    const HepMC::IteratorRange _internal;

  };

  using HepMC_IO_type = HepMC::IO_GenEvent;
  using PdfInfo = RivetHepMC::PdfInfo;
}

#endif


namespace Rivet {


  using RivetHepMC::GenEvent;
  using ConstGenEventPtr = std::shared_ptr<const GenEvent>;

  /// @todo Use MCUtils?
  namespace HepMCUtils {
    ConstGenParticlePtr              getParticlePtr(const RivetHepMC::GenParticle & gp);
    std::vector<ConstGenParticlePtr> particles(ConstGenEventPtr ge);
    std::vector<ConstGenParticlePtr> particles(const GenEvent *ge);
    std::vector<ConstGenVertexPtr>   vertices(ConstGenEventPtr ge);
    std::vector<ConstGenVertexPtr>   vertices(const GenEvent *ge);
    std::vector<ConstGenParticlePtr> particles(ConstGenVertexPtr gv, const Relatives &relo);
    std::vector<ConstGenParticlePtr> particles(ConstGenParticlePtr gp, const Relatives &relo);
    int uniqueId(ConstGenParticlePtr gp);
    int particles_size(ConstGenEventPtr ge);
    int particles_size(const GenEvent* ge);
    std::pair<ConstGenParticlePtr,ConstGenParticlePtr> beams(const GenEvent* ge);
    std::shared_ptr<HepMC_IO_type> makeReader(std::string filename, std::shared_ptr<std::istream>& istrp, std::string* errm = 0);
    bool readEvent(std::shared_ptr<HepMC_IO_type> io, std::shared_ptr<GenEvent> evt);
    void strip(GenEvent & ge, const set<long>& stripid = {1, -1, 2, -2, 3,-3, 21});
    vector<string> weightNames(const GenEvent& ge);
    std::valarray<double> weights(const GenEvent& ge);
    pair<double,double> crossSection(const GenEvent& ge, size_t index = 0);
  }


}

#endif
