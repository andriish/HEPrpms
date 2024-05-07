#ifndef RIVET_RIVETYODA_HH
#define RIVET_RIVETYODA_HH

#include "Rivet/Config/RivetCommon.hh"
#include "Rivet/Tools/TypeTraits.hh"
#include "YODA/AnalysisObject.h"
#include "YODA/Counter.h"
#include "YODA/Histo.h"
#include "YODA/Profile.h"
#include "YODA/Estimate0D.h"
#include "YODA/BinnedEstimate.h"
#include "YODA/Scatter.h"

// Use execinfo for backtrace if available
#ifdef HAVE_EXECINFO_H
#include <execinfo.h>
#endif

#include <map>
#include <unordered_map>
#include <valarray>


namespace YODA {

  template<size_t DbnN, typename ... AxisT>
  using BinnedDbnPtr = std::shared_ptr<YODA::BinnedDbn<DbnN, AxisT...>>;

  template<typename ... AxisT>
  using BinnedHistoPtr = BinnedDbnPtr<sizeof...(AxisT), AxisT...>;

  template<typename ... AxisT>
  using BinnedProfilePtr = BinnedDbnPtr<sizeof...(AxisT)+1, AxisT...>;

  template<typename ... AxisT>
  using BinnedEstimatePtr = std::shared_ptr<YODA::BinnedEstimate<AxisT...>>;

  template<size_t N>
  using ScatterNDPtr = std::shared_ptr<YODA::ScatterND<N>>;

  using AnalysisObjectPtr = std::shared_ptr<YODA::AnalysisObject>;
  using CounterPtr = std::shared_ptr<YODA::Counter>;
  using Estimate0DPtr = std::shared_ptr<YODA::Estimate0D>;
  using Histo1DPtr = BinnedHistoPtr<double>;
  using Histo2DPtr = BinnedHistoPtr<double,double>;
  using Histo3DPtr = BinnedHistoPtr<double,double,double>;
  using Profile1DPtr = BinnedProfilePtr<double>;
  using Profile2DPtr = BinnedProfilePtr<double,double>;
  using Profile3DPtr = BinnedProfilePtr<double,double,double>;
  using Estimate1DPtr = BinnedEstimatePtr<double>;
  using Estimate2DPtr = BinnedEstimatePtr<double,double>;
  using Estimate3DPtr = BinnedEstimatePtr<double,double,double>;
  using Scatter1DPtr = ScatterNDPtr<1>;
  using Scatter2DPtr = ScatterNDPtr<2>;
  using Scatter3DPtr = ScatterNDPtr<3>;

}

namespace Rivet {

  /// If @a dst is the same subclass as @a src, copy the contents of @a
  /// src into @a dst and return true. Otherwise return false.
  template <typename T>
  bool copyAO(YODA::AnalysisObjectPtr src, YODA::AnalysisObjectPtr dst, const double scale=1.0) {
    if (dst->hasAnnotation("Type") && src->type() != dst->type()) {
      throw YODA::LogicError("Operation requries types to be the same!");
    }
    for (const std::string& a : src->annotations()) {
      dst->setAnnotation(a, src->annotation(a));
    }
    shared_ptr<T> dstPtr = std::static_pointer_cast<T>(dst);
    *dstPtr = *std::static_pointer_cast<T>(src);
    if constexpr (isFillable<T>::value) { dstPtr->scaleW(scale); }
    return true;
  }


  /// @brief A polymorphic base type for the AO type handles
  struct TypeBaseHandle {

    TypeBaseHandle() = default;

    virtual ~TypeBaseHandle() { }

    virtual bool copyAO(YODA::AnalysisObjectPtr src,
                        YODA::AnalysisObjectPtr dst,
                        const double scale = 1.0) const = 0;

    virtual bool addAO(YODA::AnalysisObjectPtr src,
                       YODA::AnalysisObjectPtr& dst,
                       const double scale = 1.0) const = 0;

  };



  /// @brief The type-specific handle that can perform
  /// type-specific operations for objects of type T
  template<typename T>
  struct TypeHandle : public TypeBaseHandle {

    bool addAO(YODA::AnalysisObjectPtr src,
               YODA::AnalysisObjectPtr& dst,
               const double scale = 1.0) const {
      if constexpr (isFillable<T>::value) {
        std::shared_ptr<T> srcPtr = std::static_pointer_cast<T>(src);
        srcPtr->scaleW(scale);
        if (dst == nullptr) { dst = src; return true; }
        try { *std::static_pointer_cast<T>(dst) += *srcPtr; }
        catch (YODA::BinningError&) { return false; }
        return true;
      }
      else if (dst == nullptr) { dst = src; return true; }
      return false;
    }

    bool copyAO(YODA::AnalysisObjectPtr src,
                YODA::AnalysisObjectPtr dst,
                const double scale = 1.0) const {
      return ::Rivet::copyAO<T>(src, dst, scale);
    }

  };


  /// @defgroup AOFills Minimal objects representing AO fills,
  /// to be buffered before collapseEventGroup().
  ///
  /// @note Every object listed here needs a virtual fill method in YODA,
  /// otherwise the Tuple fakery won't work.
  ///
  /// @{

  /// Typedef for weights.
  using Weight = double;

  /// A single fill is a (FillType, Weight) pair.
  template<typename T>
  using Fill = pair<typename T::FillType, Weight>;

  /// A collection of several Fill objects.
  template<typename T>
  using Fills = vector<Fill<T>>;



  /// @brief FillCollectors which are used to temporarily cache
  /// unaggregated fills until collapsed by the Multiplexers via
  /// a call to collapseEventGroup().
  ///
  /// The specialisations of this inherit from the YODA analysis object types,
  /// and are used as such. The user-facing analysis objects in
  /// Analysis::analyze() are FillCollectors on the apparent type (accessed transparently
  /// via the dereferencing of the current Multiplexer<T>::active() pointer).
  ///
  /// @todo Do we really want this inheritance from YODA::AOs??
  template<typename T>
  class FillCollector;


  /// FillCollector specialisation for Counter
  template<>
  class FillCollector<YODA::Counter> : public YODA::Counter {
  public:

    using YAO = YODA::Counter;
    using Ptr = shared_ptr<FillCollector<YAO>>;
    using YAO::operator =;

    FillCollector() : YAO() { }

    /// Constructor
    ///
    /// The Counter isn't actually needed here:
    /// We call the YAO nullary constructor for
    /// performance reasons but still require
    /// the pointer argument to harmonise the
    /// FillCollector constructors.
    FillCollector(typename YAO::Ptr yao) : YAO(yao->path()) { }

    /// Overloaded fill method, which stores Fill info
    /// until Multiplexer<T>::collapseEventGroup() is called.
    ///
    /// @todo Do we need to deal with users using fractions directly?
    int fill(const double weight=1.0, const double fraction = 1.0) {
      (void)fraction; // suppress unused variable warning
      _fills.insert(_fills.end(), { YAO::FillType(), weight } );
      return 0;
    }

    /// Empty the subevent stack (for start of new event group).
    void reset() { _fills.clear(); }

    /// Access the fill info subevent stack.
    const Fills<YAO>& fills() const { return _fills; }

  private:

    Fills<YAO> _fills;

  };


  /// FillCollector specialisation for all BinnedDbn-like AOs
  template <size_t DbnN, typename... AxisT>
  class FillCollector<YODA::BinnedDbn<DbnN, AxisT...>>
         : public YODA::BinnedDbn<DbnN, AxisT...> {
  public:

    using YAO = YODA::BinnedDbn<DbnN, AxisT...>;
    using Ptr = shared_ptr<FillCollector<YAO>>;
    using YAO::operator =;

    FillCollector() : YAO() { }

    /// Constructor
    ///
    /// We call the cheaper constructor based on the
    /// binning to avoid copying of the bin content.
    /// The underlying binning object is still used
    /// in analize() by many routines, e.g. to query
    /// numBins() or to loop over bins() with
    /// subsequent calls to bin xMid() etc.
    FillCollector(typename YAO::Ptr yao) : YAO(yao->binning()) {
      YAO::setPath(yao->path());
    }

    /// Overloaded fill method, which stores Fill info
    /// until Multiplexer<T>::collapseEventGroup() is called.
    ///
    /// @todo Do we need to deal with users using fractions directly?
    int fill(typename YAO::FillType&& fillCoords,
             const double weight=1.0, const double fraction=1.0) {
      (void)fraction; // suppress unused variable warning
      if (YODA::containsNan(fillCoords)) {
        _fills.insert(_fills.end(), { std::move(fillCoords), weight } );
        return -1;
      }
      // Could be that DbnN > number of binned axes, so should
      // extract subset of bin coordinates to pinpoint bin
      typename YAO::BinningT::EdgeTypesTuple binCoords{};
      auto extractBinCoords = [&binCoords, &fillCoords](auto I) {
        std::get<I>(binCoords) = std::get<I>(fillCoords);
      };
      MetaUtils::staticFor<sizeof...(AxisT)>(extractBinCoords);
      _fills.insert(_fills.end(), { std::move(fillCoords), weight } );
      return (int)YAO::_binning.globalIndexAt(binCoords);
    }

    /// Empty the subevent stack (for start of new event group).
    void reset() noexcept { _fills.clear(); }

    /// Access the fill info subevent stack.
    const Fills<YAO>& fills() const { return _fills; }

  private:

    Fills<YAO> _fills;

  };
  /// FillCollector specialisation for Histo1D
  template <typename AxisT>
  class FillCollector<YODA::BinnedDbn<1, AxisT>>
         : public YODA::BinnedDbn<1, AxisT> {
  public:

    using YAO = YODA::BinnedDbn<1, AxisT>;
    using Ptr = shared_ptr<FillCollector<YAO>>;
    using YAO::operator =;

    FillCollector() : YAO() { }

    /// Constructor
    FillCollector(typename YAO::Ptr yao) : YAO(yao->binning()) {
      YAO::setPath(yao->path());
    }

    /// Overloaded fill method, which stores Fill info
    /// until Multiplexer<T>::collapseEventGroup() is called.
    ///
    /// @todo Do we need to deal with users using fractions directly?
    int fill(typename YAO::FillType&& fillCoords,
             const double weight=1.0, const double fraction=1.0) {
      (void)fraction; // suppress unused variable warning
      if (YODA::containsNan(fillCoords)) {
        _fills.insert(_fills.end(), { std::move(fillCoords), weight } );
        return -1;
      }
      // Could be that DbnN > number of binned axes, so should
      // extract subset of bin coordinates to pinpoint bin
      typename YAO::BinningT::EdgeTypesTuple binCoords{};
      auto extractBinCoords = [&binCoords, &fillCoords](auto I) {
        std::get<I>(binCoords) = std::get<I>(fillCoords);
      };
      MetaUtils::staticFor<1>(extractBinCoords);
      _fills.insert(_fills.end(), { std::move(fillCoords), weight } );
      return (int)YAO::_binning.globalIndexAt(binCoords);
    }
    //
    int fill(const AxisT x, const double weight=1.0, const double fraction=1.0) {
      return fill(typename YAO::FillType{x}, weight, fraction);
    }

    /// Empty the subevent stack (for start of new event group).
    void reset() noexcept { _fills.clear(); }

    /// Access the fill info subevent stack.
    const Fills<YAO>& fills() const { return _fills; }

  private:

    Fills<YAO> _fills;

  };
  /// FillCollector specialisation for Histo2D
  template <typename AxisT1, typename AxisT2>
  class FillCollector<YODA::BinnedDbn<2, AxisT1, AxisT2>>
         : public YODA::BinnedDbn<2, AxisT1, AxisT2> {
  public:

    using YAO = YODA::BinnedDbn<2, AxisT1, AxisT2>;
    using Ptr = shared_ptr<FillCollector<YAO>>;
    using YAO::operator =;

    FillCollector() : YAO() { }

    /// Constructor
    FillCollector(typename YAO::Ptr yao) : YAO(yao->binning()) {
      YAO::setPath(yao->path());
    }

    /// Overloaded fill method, which stores Fill info
    /// until Multiplexer<T>::collapseEventGroup() is called.
    ///
    /// @todo Do we need to deal with users using fractions directly?
    int fill(typename YAO::FillType&& fillCoords,
             const double weight=1.0, const double fraction=1.0) {
      (void)fraction; // suppress unused variable warning
      if (YODA::containsNan(fillCoords)) {
        _fills.insert(_fills.end(), { std::move(fillCoords), weight } );
        return -1;
      }
      // Could be that DbnN > number of binned axes, so should
      // extract subset of bin coordinates to pinpoint bin
      typename YAO::BinningT::EdgeTypesTuple binCoords{};
      auto extractBinCoords = [&binCoords, &fillCoords](auto I) {
        std::get<I>(binCoords) = std::get<I>(fillCoords);
      };
      MetaUtils::staticFor<1>(extractBinCoords);
      _fills.insert(_fills.end(), { std::move(fillCoords), weight } );
      return (int)YAO::_binning.globalIndexAt(binCoords);
    }
    //
    int fill(const AxisT1 x, const AxisT2 y, const double weight=1.0, const double fraction=1.0) {
      return fill(typename YAO::FillType{x,y}, weight, fraction);
    }

    /// Empty the subevent stack (for start of new event group).
    void reset() noexcept { _fills.clear(); }

    /// Access the fill info subevent stack.
    const Fills<YAO>& fills() const { return _fills; }

  private:

    Fills<YAO> _fills;

  };
  /// FillCollector specialisation for Histo3D
  template <typename AxisT1, typename AxisT2, typename AxisT3>
  class FillCollector<YODA::BinnedDbn<3, AxisT1, AxisT2, AxisT3>>
         : public YODA::BinnedDbn<3, AxisT1, AxisT2, AxisT3> {
  public:

    using YAO = YODA::BinnedDbn<3, AxisT1, AxisT2, AxisT3>;
    using Ptr = shared_ptr<FillCollector<YAO>>;
    using YAO::operator =;

    FillCollector() : YAO() { }

    /// Constructor
    FillCollector(typename YAO::Ptr yao) : YAO(yao->binning()) {
      YAO::setPath(yao->path());
    }

    /// Overloaded fill method, which stores Fill info
    /// until Multiplexer<T>::collapseEventGroup() is called.
    ///
    /// @todo Do we need to deal with users using fractions directly?
    int fill(typename YAO::FillType&& fillCoords,
             const double weight=1.0, const double fraction=1.0) {
      (void)fraction; // suppress unused variable warning
      if (YODA::containsNan(fillCoords)) {
        _fills.insert(_fills.end(), { std::move(fillCoords), weight } );
        return -1;
      }
      // Could be that DbnN > number of binned axes, so should
      // extract subset of bin coordinates to pinpoint bin
      typename YAO::BinningT::EdgeTypesTuple binCoords{};
      auto extractBinCoords = [&binCoords, &fillCoords](auto I) {
        std::get<I>(binCoords) = std::get<I>(fillCoords);
      };
      MetaUtils::staticFor<1>(extractBinCoords);
      _fills.insert(_fills.end(), { std::move(fillCoords), weight } );
      return (int)YAO::_binning.globalIndexAt(binCoords);
    }
    //
    int fill(const AxisT1 x, const AxisT2 y, const AxisT3 z, const double weight=1.0, const double fraction=1.0) {
      return fill(typename YAO::FillType{x,y,z}, weight, fraction);
    }

    /// Empty the subevent stack (for start of new event group).
    void reset() noexcept { _fills.clear(); }

    /// Access the fill info subevent stack.
    const Fills<YAO>& fills() const { return _fills; }

  private:

    Fills<YAO> _fills;

  };
  /// FillCollector specialisation for Profile1D
  template <typename AxisT>
  class FillCollector<YODA::BinnedDbn<2, AxisT>>
         : public YODA::BinnedDbn<2, AxisT> {
  public:

    using YAO = YODA::BinnedDbn<2, AxisT>;
    using Ptr = shared_ptr<FillCollector<YAO>>;
    using YAO::operator =;

    FillCollector() : YAO() { }

    /// Constructor
    FillCollector(typename YAO::Ptr yao) : YAO(yao->binning()) {
      YAO::setPath(yao->path());
    }

    /// Overloaded fill method, which stores Fill info
    /// until Multiplexer<T>::collapseEventGroup() is called.
    ///
    /// @todo Do we need to deal with users using fractions directly?
    int fill(typename YAO::FillType&& fillCoords,
             const double weight=1.0, const double fraction=1.0) {
      (void)fraction; // suppress unused variable warning
      if (YODA::containsNan(fillCoords)) {
        _fills.insert(_fills.end(), { std::move(fillCoords), weight } );
        return -1;
      }
      // Could be that DbnN > number of binned axes, so should
      // extract subset of bin coordinates to pinpoint bin
      typename YAO::BinningT::EdgeTypesTuple binCoords{};
      auto extractBinCoords = [&binCoords, &fillCoords](auto I) {
        std::get<I>(binCoords) = std::get<I>(fillCoords);
      };
      MetaUtils::staticFor<1>(extractBinCoords);
      _fills.insert(_fills.end(), { std::move(fillCoords), weight } );
      return (int)YAO::_binning.globalIndexAt(binCoords);
    }
    //
    int fill(const AxisT x, const double y, const double weight=1.0, const double fraction=1.0) {
      return fill(typename YAO::FillType{x,y}, weight, fraction);
    }

    /// Empty the subevent stack (for start of new event group).
    void reset() noexcept { _fills.clear(); }

    /// Access the fill info subevent stack.
    const Fills<YAO>& fills() const { return _fills; }

  private:

    Fills<YAO> _fills;

  };
  /// FillCollector specialisation for Histo2D
  template <typename AxisT1, typename AxisT2>
  class FillCollector<YODA::BinnedDbn<3, AxisT1, AxisT2>>
         : public YODA::BinnedDbn<3, AxisT1, AxisT2> {
  public:

    using YAO = YODA::BinnedDbn<3, AxisT1, AxisT2>;
    using Ptr = shared_ptr<FillCollector<YAO>>;
    using YAO::operator =;

    FillCollector() : YAO() { }

    /// Constructor
    FillCollector(typename YAO::Ptr yao) : YAO(yao->binning()) {
      YAO::setPath(yao->path());
    }

    /// Overloaded fill method, which stores Fill info
    /// until Multiplexer<T>::collapseEventGroup() is called.
    ///
    /// @todo Do we need to deal with users using fractions directly?
    int fill(typename YAO::FillType&& fillCoords,
             const double weight=1.0, const double fraction=1.0) {
      (void)fraction; // suppress unused variable warning
      if (YODA::containsNan(fillCoords)) {
        _fills.insert(_fills.end(), { std::move(fillCoords), weight } );
        return -1;
      }
      // Could be that DbnN > number of binned axes, so should
      // extract subset of bin coordinates to pinpoint bin
      typename YAO::BinningT::EdgeTypesTuple binCoords{};
      auto extractBinCoords = [&binCoords, &fillCoords](auto I) {
        std::get<I>(binCoords) = std::get<I>(fillCoords);
      };
      MetaUtils::staticFor<1>(extractBinCoords);
      _fills.insert(_fills.end(), { std::move(fillCoords), weight } );
      return (int)YAO::_binning.globalIndexAt(binCoords);
    }
    //
    int fill(const AxisT1 x, const AxisT2 y, const double z, const double weight=1.0, const double fraction=1.0) {
      return fill(typename YAO::FillType{x,y,z}, weight, fraction);
    }

    /// Empty the subevent stack (for start of new event group).
    void reset() noexcept { _fills.clear(); }

    /// Access the fill info subevent stack.
    const Fills<YAO>& fills() const { return _fills; }

  private:

    Fills<YAO> _fills;

  };
  /// FillCollector specialisation for Histo3D
  template <typename AxisT1, typename AxisT2, typename AxisT3>
  class FillCollector<YODA::BinnedDbn<4, AxisT1, AxisT2, AxisT3>>
         : public YODA::BinnedDbn<4, AxisT1, AxisT2, AxisT3> {
  public:

    using YAO = YODA::BinnedDbn<4, AxisT1, AxisT2, AxisT3>;
    using Ptr = shared_ptr<FillCollector<YAO>>;
    using YAO::operator =;

    FillCollector() : YAO() { }

    /// Constructor
    FillCollector(typename YAO::Ptr yao) : YAO(yao->binning()) {
      YAO::setPath(yao->path());
    }

    /// Overloaded fill method, which stores Fill info
    /// until Multiplexer<T>::collapseEventGroup() is called.
    ///
    /// @todo Do we need to deal with users using fractions directly?
    int fill(typename YAO::FillType&& fillCoords,
             const double weight=1.0, const double fraction=1.0) {
      (void)fraction; // suppress unused variable warning
      if (YODA::containsNan(fillCoords)) {
        _fills.insert(_fills.end(), { std::move(fillCoords), weight } );
        return -1;
      }
      // Could be that DbnN > number of binned axes, so should
      // extract subset of bin coordinates to pinpoint bin
      typename YAO::BinningT::EdgeTypesTuple binCoords{};
      auto extractBinCoords = [&binCoords, &fillCoords](auto I) {
        std::get<I>(binCoords) = std::get<I>(fillCoords);
      };
      MetaUtils::staticFor<1>(extractBinCoords);
      _fills.insert(_fills.end(), { std::move(fillCoords), weight } );
      return (int)YAO::_binning.globalIndexAt(binCoords);
    }
    //
    int fill(const AxisT1 x, const AxisT2 y, const AxisT3 z, const double zPlus, const double weight=1.0, const double fraction=1.0) {
      return fill(typename YAO::FillType{x,y,z,zPlus}, weight, fraction);
    }

    /// Empty the subevent stack (for start of new event group).
    void reset() noexcept { _fills.clear(); }

    /// Access the fill info subevent stack.
    const Fills<YAO>& fills() const { return _fills; }

  private:

    Fills<YAO> _fills;

  };


  /// FillCollector specialisation for Estimate
  template<>
  class FillCollector<YODA::Estimate0D> : public YODA::Estimate0D {
  public:

    using YAO = YODA::Estimate0D;
    using Ptr = shared_ptr<FillCollector<YAO>>;
    using YAO::operator =;

    FillCollector() : YAO() { }

    /// Constructor
    ///
    /// The Estimate isn't actually needed here:
    /// We call the YAO nullary constructor for
    /// performance reasons but still require
    /// the pointer argument to harmonise the
    /// FillCollector constructors.
    FillCollector(typename YAO::Ptr yao) : YAO(yao->path()) { }

  };

  /// FillCollector specialisation for BinnedEstimate
  template<typename ... AxisT>
  class FillCollector<YODA::BinnedEstimate<AxisT...>>
         : public YODA::BinnedEstimate<AxisT...> {
  public:

    using YAO = YODA::BinnedEstimate<AxisT...>;
    using Ptr = shared_ptr<FillCollector<YAO>>;
    using YAO::operator =;

    FillCollector() : YAO() { }

    /// Constructor
    ///
    /// The BinnedEstimate isn't actually needed here:
    /// We call the YAO nullary constructor for
    /// performance reasons but still require
    /// the pointer argument to harmonise the
    /// FillCollector constructors.
    FillCollector(typename YAO::Ptr yao) : YAO(yao->path()) { }

  };

  /// FillCollector specialisation for ScatterND
  template <size_t N>
  class FillCollector<YODA::ScatterND<N>> : public YODA::ScatterND<N> {
  public:

    using YAO = YODA::ScatterND<N>;
    using Ptr = shared_ptr<FillCollector<YAO>>;
    using YAO::operator =;

    FillCollector() : YAO() { }

    /// Constructor
    ///
    /// The Scatter isn't actually needed here:
    /// We call the YAO nullary constructor for
    /// performance reasons but still require
    /// the pointer argument to harmonise the
    /// FillCollector constructors.
    FillCollector(typename YAO::Ptr yao) : YAO(yao->path()) { }

  };

  /// @}

  /// Anonymous namespace to limit visibility
  namespace {

    using namespace std;

    template<typename... Args>
    double distance(const tuple<Args...>& a, const tuple<Args...>& b) {
      double rtn = 0;
      auto calculateDistance = [&](auto I) {
        if constexpr (std::is_floating_point<std::tuple_element_t<I,
                                             std::tuple<Args...>>>::value) {
          rtn += Rivet::sqr(get<I>(a) - get<I>(b));
        }
      };
      MetaUtils::staticFor<sizeof...(Args)>(calculateDistance);
      return rtn;
    }

    /// The argument @a evgroup is a vector of sub-events.
    /// Each sub-event contains a FillCollection (where each
    /// Fill is a pair of fill coordinate and weight fraction).
    ///
    /// Returns a transposed vector of fills with aligned sub-events,
    /// potentially padded with empty fills to ensure equal sizes.
    template <typename T>
    vector<Fills<T>> applyEmptyFillPaddingAndTranspose(const vector<typename FillCollector<T>::Ptr>& subevents) {

      static Fill<T> EmptyFill = { typename T::FillType{}, 0.0 };

      vector<Fills<T>> matched;
      matched.reserve(subevents.size());
      // First just copy subevents into vectors and find the longest vector.
      size_t maxFillLen = 0; // length of biggest vector
      size_t maxFillPos = 0; // index position of biggest vector
      for (const auto& subevt : subevents) {
        auto&& fills = subevt->fills();
        if (fills.size() > maxFillLen) {
          maxFillLen = fills.size();
          maxFillPos = matched.size();
        }
        matched.push_back(std::move(fills));
      }

      // Now, go through all subevents with missing fills.
      const Fills<T>& maxFill = matched[maxFillPos]; // the longest fill
      for (auto& fills : matched) {

        if (fills.size() == maxFillLen)  continue;

        /// Add empty-fill padding, such that
        /// every element has the same length
        while (fills.size() < maxFillLen) {
          fills.push_back(EmptyFill);
        }

        /// Iterate from the back and shift all fill values
        /// backwards by swapping with empty fills so that
        /// they better match the full subevent.
        for (int i = maxFillLen - 1; i >= 0; --i) {
          if (fills[i] == EmptyFill)  continue;
          int j = i;
          while (j+1 < (int)maxFillLen &&
                 fills[j + 1] == EmptyFill &&
                 distance(fills[j].first,
                          maxFill[j].first) >
                 distance(fills[j].first,
                          maxFill[j+1].first)) {
            swap(fills[j], fills[j+1]);
            ++j;
          }
        }
      }
      // finally, transpose and we're done
      vector<Fills<T>> result(maxFillLen, Fills<T>(matched.size()));
      for (size_t i = 0; i < matched.size(); ++i) {
        for (size_t j = 0; j < maxFillLen; ++j) {
          result.at(j).at(i) = std::move(matched.at(i).at(j));
        }
      }
      return result;
    }

    /// Helper struct to extract a YODA::Binning type
    /// corresponding to the axis types of the fill tuple
    template<typename T>
    struct SubwindowType;
    //
    template<typename... Args>
    struct SubwindowType<tuple<Args...>> {
      using type = YODA::Binning<std::decay_t<decltype(std::declval<YODA::Axis<Args>>())>...>;
    };

    /// Windowing with (optional) smearing of bin edges
    ///
    /// The logic here follows Appendix A of the Rivet 3 paper
    /// but generalised to N dimensions: First, construct windows
    /// around the fill coordinates, then create a YODA::Binning
    /// object where each subwindow is represented as a bin to
    /// allow looping over all subwindows (i.e. bins) in ND.
    template<typename YAO>
    vector<std::tuple<typename YAO::FillType, valarray<double>, double>>
    applyFillWindows(shared_ptr<YAO> ao, const Fills<YAO>& subevents,
                     const vector<valarray<double>>& weights, const double fsmear=0.5) {


      using SubwindowT = typename SubwindowType<typename YAO::FillType>::type;
      SubwindowT subwindows;

      const size_t nFills = subevents.size();
      vector<vector<double>> edgesLo, edgesHi;
      edgesLo.resize(YAO::FillDimension::value);
      edgesHi.resize(YAO::FillDimension::value);

      auto constructWindows = [&](auto I) {

        using FillAxisT = typename SubwindowT::template getAxisT<I>;
        using isContinuous = typename SubwindowT::template is_CAxis<I>;

        if constexpr(!isContinuous::value) { // discrete axes don't need smearing
          // use single edge axis for discrete coordinates
          subwindows.template axis<I>() = FillAxisT({ std::get<I>(subevents[0].first) });
          return;
        }
        else { // continupus axes need windowing
          edgesHi[I].resize(nFills);
          edgesLo[I].resize(nFills);

          if constexpr(I < YAO::BinningT::Dimension::value) {
            // this fill axis is binned: window sizes will depend on bin width
            const auto& axis = ao->binning().template axis<I>();
            size_t over = 0, under = 0;
            const double edgeMax = ao->template max<I>();
            const double edgeMin = ao->template min<I>();
            const size_t binLast = axis.numBins(); // index of last visible bin

            for (size_t i = 0; i < nFills; ++i) {
              const double edge = get<I>(subevents[i].first);
              size_t idx = axis.index(edge);
              if (edge >= edgeMax) {
                if (edge > edgeMax) ++over;
                idx = binLast; // cut off at highest visible bin
              }
              else if (edge < edgeMin) {
                ++under;
                idx = 1; // cut off at lowest visible bin
              }
              // find index of closest neighbouring bin
              size_t ibn = idx;
              if (edge > axis.mid(idx)) {
                if (idx != binLast) ++ibn;
              }
              else {
                if (idx != 1) --ibn;
              }

              // construct rectangular windows of with = 2*delta
              const double ibw = axis.width(idx) < axis.width(ibn)? idx : ibn;
              if ( fsmear > 0.0 ) {
                const double delta = 0.5*fsmear*axis.width(ibw);
                edgesHi[I][i] = edge + delta;
                edgesLo[I][i] = edge - delta;
              }
              else {
                const double delta = 0.5*axis.width(ibw);
                if (edge > edgeMax) {
                  edgesHi[I][i] = max(edgeMax + 2*delta, edge + delta);
                  edgesLo[I][i] = max(edgeMax, edge - delta);
                }
                else if (edge < edgeMin) {
                  edgesHi[I][i] = min(edgeMin, edge + delta);
                  edgesLo[I][i] = min(edgeMin - 2*delta, edge - delta);
                }
                else {
                  edgesHi[I][i] = axis.max(idx);
                  edgesLo[I][i] = axis.min(idx);
                }
              }
            }
            for (size_t i = 0; i < nFills; ++i) {
              const double wsize = edgesHi[I][i] - edgesLo[I][i];
              if (over == nFills && edgesLo[I][i] < edgeMax && edgesHi[I][i] > edgeMax) {
                edgesHi[I][i] = edgeMax + wsize;
                edgesLo[I][i] = edgeMax;
              }
              else if (over == 0 && edgesLo[I][i] < edgeMax && edgesHi[I][i] > edgeMax) {
                edgesLo[I][i] = edgeMax - wsize;
                edgesHi[I][i] = edgeMax;
              }
              else if (under == nFills && edgesLo[I][i] < edgeMin && edgesHi[I][i] > edgeMin) {
                edgesLo[I][i] = edgeMin - wsize;
                edgesHi[I][i] = edgeMin;
              }
              else if (under == 0 && edgesLo[I][i] < edgeMin && edgesHi[I][i] > edgeMin) {
                edgesHi[I][i] = edgeMin + wsize;
                edgesLo[I][i] = edgeMin;
              }
            }
          } // end of constexpr-check for binned axes
          else {
            // this fill axis is unbinned (e.g. in Profiles)
            for (size_t i = 0; i < nFills; ++i) {
              /// @todo What's a good reference for the window size along an unbinned axes? Is 20% of the FP edge a reasonable window size?
              /// @note No smearing needed here since there are no bin-edges along this axes
              const double edge = get<I>(subevents[i].first);
              const double delta = 0.1*fabs(edge);
              edgesHi[I][i] = edge + delta;
              edgesLo[I][i] = edge - delta;
            }
          }
          // create CAxis with subwindows from the set of window edges
          vector<double> windowEdges;
          std::copy(edgesLo[I].begin(), edgesLo[I].end(), std::back_inserter(windowEdges));
          std::copy(edgesHi[I].begin(), edgesHi[I].end(), std::back_inserter(windowEdges));
          std::sort(windowEdges.begin(), windowEdges.end());
          windowEdges.erase( std::unique(windowEdges.begin(), windowEdges.end()), windowEdges.end() );
          subwindows.template axis<I>() = FillAxisT(windowEdges);
        } // end of if constexpr isContinuous
      };
      // execute for each fill dimension
      MetaUtils::staticFor<YAO::FillDimension::value>(constructWindows);

      // Placeholder for the return type: a tuple of {FillType, multi-weights, fill fraction}
      vector<std::tuple<typename YAO::FillType, valarray<double>, double>> rtn;

      // Subwindows are given by visible bins of the
      // SubwindowT, so skip all under-/overflows
      const vector<size_t> overflows = subwindows.calcOverflowBinsIndices();
      const auto& itEnd = overflows.cend();
      for (size_t i = 0; i < subwindows.numBins(); ++i) {
        if (std::find(overflows.cbegin(), itEnd, i) != itEnd)  continue;

        const auto coords = subwindows.edgeTuple(i);
        const double subwindowArea = subwindows.dVol(i);
        size_t nSubfills = 0;
        double windowFrac = 0.;
        valarray<double> sumw(0.0, weights[0].size()); // one per multiweight
        for (size_t j = 0; j < nFills; ++j) {
          bool pass = true;
          double windowArea = 1.0;
          auto checkSubwindowOverlap = [&](auto I) {
           using isContinuous = typename SubwindowT::template is_CAxis<I>;
            if constexpr (isContinuous::value) {
              const double edge = std::get<I>(coords);
              pass &= (edgesLo[I][j] <= edge && edge <= edgesHi[I][j]);
              windowArea *= edgesHi[I][j] - edgesLo[I][j];
            }
          };
          MetaUtils::staticFor<YAO::FillDimension::value>(checkSubwindowOverlap);
          if (pass) {
            windowFrac = subwindowArea/windowArea;
            sumw += subevents[j].second * weights[j];
            ++nSubfills;
          }
        }
        if (nSubfills) {
          const double fillFrac = (double)nSubfills/(double)nFills;
          rtn.emplace_back(coords, sumw/fillFrac, fillFrac*windowFrac); // normalise to a single fill
        }
      }
      return rtn;
    } // end of applyFillWindows

  } // end of anonymous name space


  /// @name Multiplexing wrappers
  /// @{

  /// @brief Multiplexer base class
  ///
  /// Abstract interface to a set of YODA AOs corresponding
  /// to multiple weight-streams, with subevent handling.
  ///
  /// @note This abstraction is useful for looping over
  /// generic multiplexed AOs e.g. in the AnalysisHandler.
  class MultiplexedAO {
  public:

    virtual ~MultiplexedAO() { }

    /// The type being represented is a generic AO.
    using Inner = YODA::AnalysisObject;

    /// Add a new layer of subevent fill staging.
    virtual void newSubEvent() = 0;

    /// Sync the fill proxies to the persistent histogram.
    virtual void collapseEventGroup(const vector<std::valarray<double>>& weight, const double nlowfrac=0.0) = 0;

    /// Sync the persistent histograms to the final collection.
    virtual void pushToFinal() = 0;

    /// A shared pointer to the active YODA AO.
    virtual YODA::AnalysisObjectPtr activeAO() const = 0;

    /// The histogram path, without a variation suffix.
    virtual string basePath() const = 0;

    /// Access the active analysis object for function calls.
    virtual YODA::AnalysisObject* operator -> () = 0;

    /// Access the active analysis object for const function calls.
    virtual YODA::AnalysisObject* operator -> () const = 0;

    /// Access the active analysis object as a reference.
    virtual const YODA::AnalysisObject& operator * () const = 0;

    /// Set active object for analyze.
    virtual void setActiveWeightIdx(size_t iWeight) = 0;

    /// Set active object for finalize.
    virtual void setActiveFinalWeightIdx(size_t iWeight) = 0;

    /// Unset the active-object pointer.
    virtual void unsetActiveWeight() = 0;

    /// Set the size of the bootstrap vectors
    virtual void initBootstrap() = 0;

    /// Test for equality.
    bool operator == (const MultiplexedAO& p) { return (this == &p); }

    /// Test for inequality.
    bool operator != (const MultiplexedAO& p) { return (this != &p); }

    const vector<bool>& fillOutcomes() const {  return _fillOutcomes; }

    const vector<double>& fillFractions() const {  return _fillFractions; }

  protected:

    vector<bool> _fillOutcomes;

    vector<double> _fillFractions;

  };



  /// @brief Type-specific multiplexed YODA analysis object.
  ///
  /// Specialisations of this class (to each type of YODA object)
  /// are effectively the user-facing types in Rivet analyses,
  /// modulo a further wrapping via the MultplexAOPtr (which is
  /// a customised std::shared_pointer around the Multiplexer).
  ///
  /// Multiplexers instantiate one AO for each of the multiweights
  /// (x2 for pre- and post-finalize copies). They can expose either
  /// FillCollector<T> or T active pointers, for the analyze() and
  /// finalize() steps, respectively.
  ///
  /// @todo Some things are not really well-defined here.
  /// For instance: fill() in the finalize() method and
  /// integral() in the analyze() method. Should we throw?
  template<typename T>
  class Multiplexer : public MultiplexedAO {

  public:

    friend class Analysis;
    //friend class AnalysisHandler;

    /// Typedef for the YODA type being represented
    using Inner = T;
    using MultiplexedAO::_fillOutcomes;
    using MultiplexedAO::_fillFractions;

    Multiplexer() = default;

    Multiplexer(const vector<string>& weightNames, const T& p) {
      _basePath = p.path();
      _baseName = p.name();
      for (const string& weightname : weightNames) {
        _persistent.push_back(make_shared<T>(p));
        _final.push_back(make_shared<T>(p));

        typename T::Ptr obj = _persistent.back();
        obj->setPath("/RAW" + obj->path());
        typename T::Ptr final = _final.back();
        if (weightname != "") {
          obj->setPath(obj->path() + "[" + weightname + "]");
          final->setPath(final->path() + "[" + weightname + "]");
        }
      }
    }

    ~Multiplexer() = default;

    /// Get the current active analysis object
    /// (may be either persistent or final, depending on stage)
    typename T::Ptr active() const {
      if ( !_active ) {
        #ifdef HAVE_BACKTRACE
        void* buffer[4];
        backtrace(buffer, 4);
        backtrace_symbols_fd(buffer, 4 , 1);
        #endif
        assert(false && "No active pointer set. Was this object booked in init()?");
      }
      return _active;
    }

    template <typename U = T>
    auto binning() const -> std::enable_if_t<hasBinning<T>::value, const typename U::BinningT&> {
      return _persistent.back()->binning();
    }

    /// Get the AO path of the object, without variation suffix
    string basePath() const { return _basePath; }

    /// Get the AO name of the object, without variation suffix
    string baseName() const { return _baseName; }


    /// Test for object validity.
    ///
    /// @note Don't use active() here: assert will catch.
    explicit operator bool() const { return static_cast<bool>(_active); }

    /// Test for object invalidity.
    ///
    /// @note Don't use active() here: assert will catch.
    bool operator ! () const { return !_active; }


    /// Forwarding dereference-call operator.
    T* operator -> () { return active().get(); }

    /// Forwarding dereference-call operator.
    T* operator -> () const { return active().get(); }

    /// Forwarding dereference operator.
    T& operator * () { return *active(); }

    /// Forwarding dereference operator.
    const T& operator * () const { return *active(); }


    /// Equality operator
    friend bool operator == (const Multiplexer& a, const Multiplexer& b){
      if (a._persistent.size() != b._persistent.size()) {
        return false;
      }
      // also test for binning compatibility
      for (size_t i = 0; i < a._persistent.size(); ++i) {
        if (a._persistent.at(i) != b._persistent.at(i)) {
          return false;
        }
      }
      return true;
    }

    /// Inequality operator
    friend bool operator != (const Multiplexer& a, const Multiplexer& b) {
      return !(a == b);
    }

    /// Less-than operator
    friend bool operator < (const Multiplexer a, const Multiplexer& b) {
      if (a._persistent.size() >= b._persistent.size()) {
        return false;
      }
      for (size_t i = 0; i < a._persistent.size(); ++i) {
        if (*(a._persistent.at(i)) >= *(b._persistent.at(i))) {
          return false;
        }
      }
      return true;
    }


    /// @}

    /// @name Access methods
    /// @{

    /// Set the active-object pointer to point at a variation in the persistent set
    void setActiveWeightIdx(size_t iWeight) {
      _active = _persistent.at(iWeight);
    }

    /// Set the active-object pointer to point at a variation in the final set
    void setActiveFinalWeightIdx(size_t iWeight) {
      _active = _final.at(iWeight);
    }

    /// Unset the active-object pointer
    void unsetActiveWeight() { _active.reset(); }

    /// Clear the active object pointer
    void reset() { active()->reset(); }


    /// @brief Create a new FillCollector for the next sub-event.
    ///
    /// Called every sub-event by AnalysisHandler::analyze() before
    /// dispatch to Analysis::analyze(). The fill values will be
    /// redistributed over variations by collapseEventGroup().
    void newSubEvent() {
      _evgroup.emplace_back(new FillCollector<T>(_persistent[0]));
      _active = _evgroup.back();
      assert(_active);
    }

    /// Pushes the (possibly collapsed) fill(s) into the persistent objects
    void collapseEventGroup(const vector<std::valarray<double>>& weights, const double nlowfrac=0.0) {

      /// @todo If we don't multiplex (Binned)Estimates, perhaps we can get rid of this protection?
      if constexpr( isFillable<T>::value ) {

        // Should have as many subevent fills as subevent weights
        assert( _evgroup.size() == weights.size() );

        if (_evgroup.size() == 1) { // Have we had subevents at all?
          // ensure vector length matches size of multiplexed AO
          if (_fillOutcomes.empty())  initBootstrap();
          // reset fill positions
          std::fill(_fillOutcomes.begin(), _fillOutcomes.end(), false);
          std::fill(_fillFractions.begin(), _fillFractions.end(), 0.0);

          // Simple replay of all collected fills:
          // each fill is inserted into every persistent AO
          for (auto f : _evgroup[0]->fills()) {
            int pos = -1;
            double frac = 0.0;
            for (size_t m = 0; m < _persistent.size(); ++m) { //< m is the variation index
              if (!m)  frac = f.second;
              pos = _persistent[m]->fill( std::move(f.first), std::move(f.second) * weights[0][m] );
            }
            if (pos >= 0) {
              _fillOutcomes[pos] = true;
              _fillFractions[pos] += frac;
            }
          }
        }
        else {
          collapseSubevents(weights, nlowfrac);
        }
      }

      _evgroup.clear();
      _active.reset();
    }

    /// Collapse the set of FillCollectors (i.e. _evgroup)
    /// into combined fills of the persistent objects,
    /// using fractional fills if there are subevents
    void collapseSubevents(const vector<std::valarray<double>>& weights, const double nlowfrac) {
      if constexpr( isFillable<T>::value ) {
        if constexpr (!std::is_same<T, YODA::Counter>::value ) { // binned objects
          /// @note The number of fills could be different between sub-events,
          /// in which case we will first add a padding of "empty" fills.
          /// We also take the transpose of subevents vs fills.
          ///
          /// @todo Do we really need the transposing??
          for (const Fills<T>& subEvents : applyEmptyFillPaddingAndTranspose<T>(_evgroup)) {
            // construct fill windows with fractional fills
            for (const auto& f : applyFillWindows(_persistent[0], subEvents, weights, nlowfrac)) {
              for ( size_t m = 0; m < _persistent.size(); ++m ) { // for each multiweight
                _persistent[m]->fill( typename T::FillType(get<0>(f)), get<1>(f)[m], get<2>(f) );
              }
            } // end of loop over fill windows
          } // end of loop over subevents
        }
        else {
          for (size_t m = 0; m < _persistent.size(); ++m) { //< m is the variation index
            vector<double> sumfw{0.0}; // final fill weights (one per fill)
            for (size_t n = 0; n < _evgroup.size(); ++n) { //< n is the correlated sub-event index
              const auto& fills = _evgroup[n]->fills();
              // resize if this subevent has an
              // even larger number of fills
              if (fills.size() > sumfw.size()) {
                sumfw.resize(fills.size(), 0.0);
              }
              size_t fi = 0;
              for (const auto& f : fills) { // collapse sub-events and aggregate final fill weights
                sumfw[fi++] += f.second * weights[n][m]; // f.second is optional user-supplied scaling
              }
            }
            for (double fw : sumfw) { // fill persistent Counters
              _persistent[m]->fill(std::move(fw));
            }
          }
        }
      }
    }

    /// Copy all variations from the "live" persistent set
    /// to the final collection used by Analysis::finalize()
    void pushToFinal() {
      for ( size_t m = 0; m < _persistent.size(); ++m ) { //< variation weight index
        _final.at(m)->clearAnnotations(); // in case this is a repeat call
        copyAO<T>(_persistent.at(m), _final.at(m));
        // Remove the /RAW prefix, if there is one, from the final copy
        if ( _final[m]->path().substr(0,4) == "/RAW" )
          _final[m]->setPath(_final[m]->path().substr(4));
      }
    }

    /// Get the set of persistent (i.e. after whole event groups)
    /// live objects, as used by Analysis::analyze().
    const vector<typename T::Ptr>& persistent() const {
      return _persistent;
    }

    /// Direct access to the persistent object in weight stream @a iWeight
    typename T::Ptr persistent(const size_t iWeight) { return _persistent.at(iWeight); }

    /// Get the set of final analysis objects, as used
    /// by Analysis::finalize() and written out
    const vector<typename T::Ptr>& final() const {
      return _final;
    }

    /// Direct access to the finalized object in weight stream @a iWeight
    typename T::Ptr final(const size_t iWeight) { return _final.at(iWeight); }

    /// Get the currently active analysis object
    YODA::AnalysisObjectPtr activeAO() const { return _active; }

    /// @brief Helper method to resize aux vectors to AO size
    void initBootstrap() {
      if constexpr( isFillable<T>::value ) {
        size_t nPos = 1; // Counter only has a single fill position
        if constexpr (!std::is_same<T, YODA::Counter>::value ) { // binned objects
          nPos = _persistent.back()->numBins(true, true);
        }
        _fillOutcomes.resize(nPos);
        _fillFractions.resize(nPos);
      }
    }

    /// @}

  private:

    /// @name Data members
    /// @{

    /// M of these, one for each weight
    vector<typename T::Ptr> _persistent;

    /// The copy of M-entry _persistent that will be passed to finalize().
    vector<typename T::Ptr> _final;

    /// A vector of FillCollectors, one for each subevent
    vector<typename FillCollector<T>::Ptr> _evgroup;

    /// The currently active FillCollector (if in analyze)
    /// or AO (if in finalize).
    typename T::Ptr _active;

    /// The base AO path of this object,
    /// without weight-variation suffix.
    string _basePath;

    /// The base AO name, without
    /// any weight variation suffix.
    string _baseName;

    /// @}

  };


  /// Customised shared pointer of multiplexed AOs,
  /// dispatching through two layers of indirection.
  ///
  /// The customisation is needed in order to dispatch
  /// -> and * operators all the way down to the inner
  /// YODA analysis objects.
  ///
  /// @todo Provide remaining functionality that shared_ptr has (not needed right now).
  template <typename T>
  class MultiplexPtr {

  public:

    using value_type = T;

    MultiplexPtr() = default;

    MultiplexPtr(decltype(nullptr)) : _p(nullptr) { }

    /// Convenience constructor, pass through to the Multiplexer constructor
    MultiplexPtr(const vector<string>& weightNames, const typename T::Inner& p)
      : _p( make_shared<T>(weightNames, p) ) { }

    // Ensure a shared_ptr<T> can be instantiated from a shared_ptr<U>
    template <typename U, typename = decltype(shared_ptr<T>(shared_ptr<U>{}))>
    MultiplexPtr(const shared_ptr<U>& p) : _p(p) { }

    // Ensure a shared_ptr<T> can be instantiated from a shared_ptr<U>
    template <typename U, typename = decltype(shared_ptr<T>(shared_ptr<U>{}))>
    MultiplexPtr(const MultiplexPtr<U>& p) : _p(p.get()) { }

    /// Goes right through to the active Multiplexer<YODA> object's members
    T& operator -> () {
      if (_p == nullptr) {
        throw Error("Dereferencing null AnalysisObject pointer. Is there an unbooked histogram variable?");
      }
      return *_p;
    }

    template <typename U = T>
    auto binning() const -> std::enable_if_t<hasBinning<typename T::Inner>::value, const typename U::Inner::BinningT&> {
      if (_p == nullptr) {
        throw Error("Dereferencing null AnalysisObject pointer. Is there an unbooked histogram variable?");
      }
      return _p->binning();
    }


    /// Goes right through to the active Multiplexer<YODA> object's members
    const T& operator -> () const {
      if (_p == nullptr) {
        throw Error("Dereferencing null AnalysisObject pointer. Is there an unbooked histogram variable?");
      }
      return *_p;
    }

    /// The active YODA object
    typename T::Inner& operator * ()             { return **_p; }
    const typename T::Inner& operator * () const { return **_p; }

    /// Object validity check.
    explicit operator bool() const { return _p && bool(*_p); }

    /// Object invalidity check.
    bool operator ! () const { return !_p || !(*_p);   }

    /// Object validity check.
    bool operator == (const MultiplexPtr& other) const {
      return _p == other._p;
    }

    /// Object invalidity check.
    bool operator != (const MultiplexPtr& other) const {
      return _p != other._p;
    }

    /// Less-than for ptr ordering.
    bool operator < (const MultiplexPtr& other) const {
      return _p < other._p;
    }

    /// Greater-than for ptr ordering.
    bool operator > (const MultiplexPtr other) const {
      return _p > other._p;
    }

    /// Less-equals for ptr ordering.
    bool operator <= (const MultiplexPtr& other) const {
      return _p <= other._p;
    }

    /// Greater-equals for ptr ordering.
    bool operator >= (const MultiplexPtr& other) const {
      return _p >= other._p;
    }

    /// Get the internal shared ptr.
    shared_ptr<T> get() const { return _p; }

  private:

    /// The type being wrapped.
    shared_ptr<T> _p;

  };

  /// @}


  /// @name  User-facing analysis object Multiplexers
  ///
  /// @note Every object listed here needs a virtual fill() method in YODA,
  /// otherwise the FillCollector fakery won't work.
  ///
  /// @{

  using MultiplexAOPtr = MultiplexPtr<MultiplexedAO>;

  template<size_t DbnN, typename... AxisT>
  using BinnedDbnPtr = MultiplexPtr<Multiplexer<YODA::BinnedDbn<DbnN, AxisT...>>>;

  template<typename... AxisT>
  using BinnedHistoPtr = BinnedDbnPtr<sizeof...(AxisT), AxisT...>;

  template<typename... AxisT>
  using BinnedProfilePtr = BinnedDbnPtr<sizeof...(AxisT)+1, AxisT...>;

  template<typename... AxisT>
  using BinnedEstimatePtr = MultiplexPtr<Multiplexer<YODA::BinnedEstimate<AxisT...>>>;

  template<size_t N>
  using ScatterNDPtr  = MultiplexPtr<Multiplexer<YODA::ScatterND<N>>>;

  using CounterPtr    = MultiplexPtr<Multiplexer<YODA::Counter>>;
  using Estimate0DPtr = MultiplexPtr<Multiplexer<YODA::Estimate0D>>;
  using Histo1DPtr    = BinnedHistoPtr<double>;
  using Histo2DPtr    = BinnedHistoPtr<double,double>;
  using Histo3DPtr    = BinnedHistoPtr<double,double,double>;
  using Profile1DPtr  = BinnedProfilePtr<double>;
  using Profile2DPtr  = BinnedProfilePtr<double,double>;
  using Profile3DPtr  = BinnedProfilePtr<double,double,double>;
  using Estimate1DPtr = BinnedEstimatePtr<double>;
  using Estimate2DPtr = BinnedEstimatePtr<double,double>;
  using Estimate3DPtr = BinnedEstimatePtr<double,double,double>;
  using Scatter1DPtr  = ScatterNDPtr<1>;
  using Scatter2DPtr  = ScatterNDPtr<2>;
  using Scatter3DPtr  = ScatterNDPtr<3>;

  using YODA::Counter;
  using YODA::Estimate0D;
  using YODA::Histo1D;
  using YODA::Histo2D;
  using YODA::Histo3D;
  using YODA::Profile1D;
  using YODA::Profile2D;
  using YODA::Profile3D;
  using YODA::Estimate1D;
  using YODA::Estimate2D;
  using YODA::Estimate3D;
  using YODA::Scatter1D;
  using YODA::Scatter2D;
  using YODA::Scatter3D;
  using YODA::Point1D;
  using YODA::Point2D;
  using YODA::Point3D;

  ///@}


  /// @defgroup aomanip Analysis object manipulation functions
  /// @{

  inline bool isTmpPath(const std::string& path, const bool tmp_only = false) {
    if (tmp_only)  return path.find("/TMP/") != string::npos;
    return path.find("/TMP/") != string::npos || path.find("/_") != string::npos;
  }

  /// Function to get a map of all the refdata in a paper with the
  /// given @a papername.
  map<string, YODA::AnalysisObjectPtr> getRefData(const string& papername);

  /// @todo Also provide a Scatter3D getRefData() version?

  /// Get the file system path to the reference file for this paper.
  string getDatafilePath(const string& papername);


  /// Traits class to access the type of the AnalysisObject in the reference files.
  /// @todo MIGRATE TO ESTIMATES
  template<typename T>
  struct ReferenceTraits { };

  template<>
  struct ReferenceTraits<YODA::Counter> {
    using RefT = YODA::Estimate0D;
  };

  template<>
  struct ReferenceTraits<YODA::Estimate0D> {
    using RefT = YODA::Estimate0D;
  };

  template<typename... AxisT>
  struct ReferenceTraits<YODA::BinnedEstimate<AxisT...>> {
    using RefT = YODA::BinnedEstimate<AxisT...>;
  };

  template<size_t DbnN, typename... AxisT>
  struct ReferenceTraits<YODA::BinnedDbn<DbnN, AxisT...>> {
    using RefT = YODA::ScatterND<sizeof...(AxisT)+1>;
  };

  template<size_t N>
  struct ReferenceTraits<YODA::ScatterND<N>> {
    using RefT = YODA::ScatterND<N>;
  };

  /// Check if two analysis objects have the same binning or, if not
  /// binned, are in other ways compatible.
  template <typename TPtr>
  inline bool bookingCompatible(TPtr a, TPtr b) {
    return *a == *b;
  }
  //
  inline bool bookingCompatible(CounterPtr, CounterPtr) {
    return true;
  }
  //
  inline bool bookingCompatible(YODA::CounterPtr, YODA::CounterPtr) {
    return true;
  }
  //
  inline bool bookingCompatible(Estimate0DPtr, Estimate0DPtr) {
    return true;
  }
  //
  inline bool bookingCompatible(YODA::Estimate0DPtr, YODA::Estimate0DPtr) {
    return true;
  }
  //
  template<size_t N>
  inline bool bookingCompatible(ScatterNDPtr<N> a, ScatterNDPtr<N> b) {
    return a->numPoints() == b->numPoints();
  }
  //
  template<size_t N>
  inline bool bookingCompatible(YODA::ScatterNDPtr<N> a, YODA::ScatterNDPtr<N> b) {
    return a->numPoints() == b->numPoints();
  }

  inline bool beamInfoCompatible(YODA::AnalysisObjectPtr a, YODA::AnalysisObjectPtr b) {
    YODA::BinnedEstimatePtr<string> beamsA = std::dynamic_pointer_cast<YODA::BinnedEstimate<string>>(a);
    YODA::BinnedEstimatePtr<string> beamsB = std::dynamic_pointer_cast<YODA::BinnedEstimate<string>>(b);
    return  beamsA && beamsB && (*beamsA == *beamsB) && beamsA->numBins() == 2 &&
            fuzzyEquals(beamsA->bin(1).val(), beamsB->bin(1).val()) &&
            fuzzyEquals(beamsA->bin(2).val(), beamsB->bin(2).val());
  }

  /// @}



  /// Class representing a YODA path with all its components.
  class AOPath {
  public:

    /// Constructor
    AOPath(string fullpath)
      : _valid(false), _path(fullpath), _raw(false), _tmp(false), _ref(false) {
      _valid = init(fullpath);
    }

    /// The full path.
    string path() const { return _path; }

    /// The analysis name.
    string analysis() const { return _analysis; }

    /// The analysis name with options.
    string analysisWithOptions() const { return _analysis + _optionstring; }

    /// The base name of the analysis object.
    string name() const { return _name; }

    /// The weight name.
    string weight() const { return _weight; }

    /// The weight component of the path
    string weightComponent() const {
      if (_weight == "")  return _weight;
      return "[" + _weight + "]";
    }

    /// Is this a RAW (filling) object?
    bool isRaw() const { return _raw; }

    // Is this a temporary (filling) object?
    bool isTmp() const { return _tmp; }

    /// Is this a reference object?
    bool isRef() const { return _ref; }

    /// The string describing the options passed to the analysis.
    string optionString() const { return _optionstring; }

    /// Are there options passed to the analysis?
    bool hasOptions() const { return !_options.empty(); }

    /// Don't pass This optionto the analysis
    void removeOption(string opt) { _options.erase(opt); fixOptionString(); }

    /// Pass this option to the analysis.
    void setOption(string opt, string val) { _options[opt] = val; fixOptionString(); }

    /// Was This option passed to the analyisi.
    bool hasOption(string opt) const { return _options.find(opt) != _options.end(); }

    /// Get the value of this option.
    string getOption(string opt) const {
      auto it = _options.find(opt);
      if ( it != _options.end() ) return it->second;
      return "";
    }

    /// Reset the option string after changes;
    void fixOptionString();

    /// Creat a full path (and set) for this.
    string mkPath() const;
    string setPath() { return _path = mkPath(); }

    /// Print out information
    void debug() const;

    /// Make this class ordered.
    bool operator<(const AOPath & other) const {
      return _path < other._path;
    }

    /// Check if path is valid.
    bool valid() const { return _valid; };
    bool operator!() const { return !valid(); }

  private:

    /// Internal functions for disassembling a path name
    bool init(string fullpath);
    bool chopweight(string & fullpath);
    bool chopoptions(string & anal);

    bool _valid;
    string _path;
    string _analysis;
    string _optionstring;
    string _name;
    string _weight;
    bool _raw;
    bool _tmp;
    bool _ref;
    map<string,string> _options;

  };

}

#endif
