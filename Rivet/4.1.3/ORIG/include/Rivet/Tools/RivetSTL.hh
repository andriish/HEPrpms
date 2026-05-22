#ifndef RIVET_RivetSTL_HH
#define RIVET_RivetSTL_HH

#include <string>
#include <array>
#include <vector>
#include <list>
#include <set>
#include <map>
#include <memory>
#include <functional>
#include <ostream>
#include <fstream>
#include <sstream>
#include <cmath>
#include <limits>
#include <complex>
#include <cstdint>
#include <type_traits>

namespace Rivet {


  /// We implicitly use STL entities in the Rivet namespace
  // using namespace std;
  using std::string;
  using std::to_string;
  using namespace std::string_literals;

  using std::ifstream;
  using std::ofstream;

  using std::array;
  using std::vector;
  using std::list;
  using std::set;
  using std::multiset;
  using std::map;
  using std::multimap;
  using std::pair;
  using std::make_pair;

  using std::unique_ptr;
  using std::shared_ptr;
  using std::make_shared;
  using std::make_unique;
  using std::dynamic_pointer_cast;

  using std::initializer_list;

  using std::function;

  using std::isnan;

  using std::complex;

  /// @name Streaming containers as string reps
  ///
  /// @todo Make these named toStr rather than operator<<
  /// @todo Make these generic to any iterable
  /// @{

  /// Convenient function for streaming out vectors of any streamable object.
  template<typename T>
  inline std::ostream& operator << (std::ostream& os, const std::vector<T>& vec) {
    os << "[ ";
    for (size_t i=0; i<vec.size(); ++i) {
      os << vec[i] << " ";
    }
    os << "]";
    return os;
  }

  /// Convenient function for streaming out lists of any streamable object.
  template<typename T>
  inline std::ostream& operator << (std::ostream& os, const std::list<T>& vec) {
    os << "[ ";
    for (size_t i=0; i<vec.size(); ++i) {
      os << vec[i] << " ";
    }
    os << "]";
    return os;
  }

  /// @}

  /// @name Convenience container typedefs
  /// @{
  typedef vector<std::string> strings;
  typedef vector<double> doubles;
  typedef vector<float> floats;
  typedef vector<int> ints;
  /// @}


  /// @name Conversions between containers
  /// @{

  /// Convert a container CONT to a vector of T via static casting
  template <typename T, typename CONT>
  inline vector<T> mkVecOf(const CONT& c) {
    vector<T> rtn; rtn.reserve(c.size());
    for (const typename CONT::value_type& x : c) {
      rtn.push_back(static_cast<T>(x)); //< check narrowing conversion?
    }
    return rtn;
  }

  // /// Convert an initializer list of X to a vector of T via static casting
  // template <typename T, typename X>
  // inline vector<T> mkVecOf(const std::initializer_list<X>& il) {
  //   return mkVecOf<T>(vector<X>(il));
  // }


  /// Convert a container CONT to a vector of T via static casting
  //
  template <typename T, typename CONT, typename FN = std::string(*)(typename std::remove_reference_t<CONT>::value_type),
            typename = std::enable_if_t<std::is_convertible_v<
                            T, std::decay_t<std::invoke_result_t<
                            FN, typename std::remove_reference_t<CONT>::value_type>> >> >
  vector<T> mkVecOf(CONT&& c, FN&& fn) {
    vector<T> rtn;
    rtn.reserve(std::distance(std::begin(c), std::end(c)));
    for (auto&& x : std::forward<CONT>(c)) {
      rtn.push_back(std::forward<FN>(fn)(std::forward<decltype(x)>(x))); //< check narrowing conversion?
    }
    return rtn;
  }

  // template <typename CONT, typename FN = std::string(*)(typename std::remove_reference_t<CONT>::value_type),
  //           typename T = std::decay_t<std::invoke_result_t<FN, typename std::remove_reference_t<CONT>::value_type> >        >
  // vector<T> mkVecOf(CONT&& c, FN&& fn) {
  //   vector<T> rtn;
  //   rtn.reserve(std::distance(std::begin(c), std::end(c)));
  //   for (auto&& x : std::forward<CONT>(c)) {
  //     rtn.push_back(std::forward<FN>(fn)(std::forward<decltype(x)>(x)));
  //   }
  //   return rtn;
  // }


  /// Convert a container to a vector of strings
  template <typename CONT>
  inline vector<std::string>
  mkStrings(const CONT& c) { return mkVecOf<string>(c, to_string); }

  /// Convert an initializer list to a vector of strings
  template <typename X>
  inline vector<std::string>
  mkStrings(const std::initializer_list<X>& il) { return mkVecOf<string>(il, to_string); }


  /// Convert a container to a vector of doubles
  template <typename CONT>
  //inline typename std::enable_if_t<std::is_floating_point_v<typename CONT::value_type>, vector<double> >
  inline vector<double>
  mkDoubles(const CONT& c) { return mkVecOf<double>(c); }

  /// Convert an initializer list to a vector of doubles
  template <typename X>
  //inline typename std::enable_if_t<std::is_floating_point_v<X>, vector<double> >
  inline vector<double>
  mkDoubles(const std::initializer_list<X>& il) { return mkVecOf<double>(il); }


  /// Convert a container to a vector of floats
  template <typename CONT>
  inline typename std::enable_if_t<std::is_floating_point_v<typename CONT::value_type>, vector<float> >
  // inline vector<float>
  mkFloats(const CONT& c) { return mkVecOf<float>(c); }

  /// Convert an initializer list to a vector of floats
  template <typename X>
  // inline typename std::enable_if_t<std::is_floating_point_v<X>, vector<float> >
  inline vector<float>
  mkFloats(const std::initializer_list<X>& il) { return mkVecOf<float>(il); }


  /// Convert a container to a vector of ints
  template <typename CONT>
  //inline typename std::enable_if_t<std::is_arithmetic_v<typename CONT::value_type>, vector<int> >
  inline vector<int>
  mkInts(const CONT& c) { return mkVecOf<int>(c); }

  /// Convert an initializer list to a vector of ints
  template <typename X>
  //inline typename std::enable_if_t<std::is_arithmetic_v<X>, vector<int> >
  inline vector<int>
  mkInts(const std::initializer_list<X>& il) { return mkVecOf<int>(il); }

  /// @}

  
  /// @name Boolean-return container searching
  /// @{

  /// @todo Use SFINAE, Boost.Range, or other template trickery for more generic container matching?

  /// Does @a s contain @a sub as a substring?
  inline bool contains(const std::string& s, const std::string& sub) {
    return s.find(sub) != string::npos;
  }

  /// Does the init list @a il contain @a x?
  template <typename T>
  inline bool contains(const std::initializer_list<T>& il, const T& x) {
    return find(begin(il), end(il), x) != end(il);
  }

  /// Does the vector @a v contain @a x?
  template <typename T>
  inline bool contains(const std::vector<T>& v, const T& x) {
    return find(begin(v), end(v), x) != end(v);
  }

  /// Does the list @a l contain @a x?
  template <typename T>
  inline bool contains(const std::list<T>& l, const T& x) {
    return find(begin(l), end(l), x) != end(l);
  }

  /// Does the set @a s contain @a x?
  template <typename T>
  inline bool contains(const std::set<T>& s, const T& x) {
    return find(begin(s), end(s), x) != end(s);
  }

  /// Does the map @a m contain the key @a key?
  template <typename K, typename T>
  inline bool has_key(const std::map<K, T>& m, const K& key) {
    return m.find(key) != end(m);
  }

  /// Does the map @a m contain the value @a val?
  template <typename K, typename T>
  inline bool has_value(const std::map<K, T>& m, const T& val) {
    for (typename std::map<K,T>::const_iterator it = begin(m); it != end(m); ++it) {
      if (it->second == val) return true;
    }
    return false;
  }

  /// Get the value in map @a m with key @a key, or fall back to @a fallback
  template <typename K, typename T>
  inline const T& retrieve(const std::map<K, T>& m, const K& key, const T& fallback) {
    return has_key(m, key) ? m[key] : fallback;
  }

  /// Get the value in map @a m with key @a key, or fall back to @a fallback (string-value specialisation)
  template <typename K>
  inline const std::string& retrieve(const std::map<K, std::string>& m, const K& key, const std::string& fallback) {
    return has_key(m, key) ? m.find(key)->second : fallback;
  }

  /// Get the value in map @a m with key @a key, or fall back to @a fallback (string-key specialisation)
  template <typename T>
  inline const T& retrieve(const std::map<std::string, T>& m, const std::string& key, const T& fallback) {
    return has_key(m, key) ? m.find(key)->second : fallback;
  }

  /// Get the value in map @a m with key @a key, or fall back to @a fallback (string-key+value specialisation)
  inline const std::string& retrieve(const std::map<std::string, std::string>& m, const std::string& key, const std::string& fallback) {
    return has_key(m, key) ? m.find(key)->second : fallback;
  }  
  
  /// @}


}


namespace std {


  /// @name Container filling and merging
  /// @{

  /// Append a single item to vector @a v
  template <typename T>
  inline void operator += (std::vector<T>& v, const T& x) { v.push_back(x); }

  /// Append all the items from vector @a v2 to vector @a v1
  template <typename T>
  inline void operator += (std::vector<T>& v1, const std::vector<T>& v2) {
    for (const auto& x : v2) v1.push_back(x);
  }

  /// Create a new vector from the concatenated items in vectors @a v1 and @a v2
  template <typename T>
  inline std::vector<T> operator + (const std::vector<T>& v1, const std::vector<T>& v2) {
    std::vector<T> rtn(v1);
    rtn += v2;
    return rtn;
  }


  /// Merge the contents of set @a s2 into @a s1
  template <typename T>
  inline void operator += (std::set<T>& s1, const std::set<T>& s2) {
    for (const auto& x : s2) s1.insert(x);
  }

  /// Merge the contents of sets @a s1 and @a s2
  template <typename T>
  inline std::set<T> operator + (const std::set<T>& s1, const std::set<T>& s2) {
    std::set<T> rtn(s1);
    rtn += s2;
    return rtn;
  }

  /// @}


  /// @name Function helpers
  /// @{

  /// Get a function pointer / hash integer from an std::function
  template<typename T, typename... U>
  inline uintptr_t get_address(std::function<T(U...)> f) {
    typedef T(fnType)(U...);
    fnType ** fnPointer = f.template target<fnType*>();
    return (fnPointer != nullptr) ? reinterpret_cast<uintptr_t>(*fnPointer) : 0;
  }

  /// @}


}

#endif
