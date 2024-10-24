// "The two-loop helicity amplitudes for qqb' -> V1V2 -> 4 leptons"
// T. Gehrmann, A. von Manteuffel, L. Tancredi
//
// qqvvamp package
//
// qqvvamp: core evaluator class (not recommended for direct usage, see qqvvampprec)


#ifndef QQVVAMP_H
#define QQVVAMP_H

#include <stdint.h>

// The evaluation is based on a templated numeric type, for which one can
// select double, quadtype or cln::cln_F.

// high precision versions can be disabled here
// (reduces compilation times and file sizes)
//#define ARBITRARY_PRECISION
#define QUAD_PRECISION
#define DOUBLE_PRECISION

#ifdef ARBITRARY_PRECISION
#include <cln/rational.h>
#include <cln/lfloat.h>
#endif

#ifdef QUAD_PRECISION
#ifdef __ARM_ARCH 
typedef _Float128 quadtype;
#define quadmath_snprintf snprintf
#define strtoflt128  strtof128
#else
extern "C" {
#include <quadmath.h>
}
typedef __float128 quadtype;
#endif

#include <ostream>
#endif

namespace GiNaC {
  class ex;
  class numeric;
}

// IR subtraction schemes
enum IRscheme { IRschemeQt=0, IRschemeCatani };

const int nAcoeff = 10; // coefficient type {1,2,...,10}
const int nEcoeff = 9; // coefficient type {1,2,...,9}
const int ntype = 4; // coupling type {0=A, 1=B, 2=C, 3=F}
const int nloop = 3; // {0=tree, 1=1-loop, 2=2-loop}
const int nreim = 2; // {0=Re(), 1=Im()}

typedef int64_t longint; // type for numerators and denominators
// note: 31bit is not enough for amplitude expressions ! (e.g. numerator: -2167091629)


// amplitude tensor coefficients E and A for q qbar -> V V'
/* @ tree, 1-loop, 2-loop,
 * template parameter specifies numeric type to be used internally,
 * see the paper for the definition of the coefficients E and A
 */
template<class numtype>
class qqvvamp {
public:
  // construct instance
  /* Nf is number of flavors in closed loops
   * ir the IR subtraction scheme to be used for the result */
  qqvvamp(int Nf, IRscheme ir);
  ~qqvvamp();

  // evaluate amplitude coefficients
  void compute(numtype s, numtype t, numtype ma2, numtype mb2);
  
  // settings
  int Nf; // number of light flavors
  IRscheme ir; // IR subtraction scheme
  
  // result
  // A coefficients (see paper for definition)
  numtype A[nAcoeff+1][ntype][nloop][nreim]; // first index: 1..10
  // E coefficients (see paper for definition)
  numtype E[nEcoeff+1][ntype][nloop][nreim]; // first index: 1..9

  // auxiliary array used for the computation
  numtype* r;
  bool compute_gpls; // performance tweaking for precision control

  // auxiliary methods
  // we avoid an explicit template parameter in calls by defining these as class members:
  static inline numtype n(longint a); // integer prefactor
  static inline numtype n(longint a, longint b); // rational prefactor
  static numtype n(const GiNaC::ex& a); // GPL result

#include "qqvvamp.xh"
};


// specializations

#ifdef ARBITRARY_PRECISION
GiNaC::numeric tonumeric(const cln::cl_F&);
GiNaC::numeric tonumeric(const cln::cl_N&);
double todouble(const cln::cl_F&);
template<> inline cln::cl_F qqvvamp<cln::cl_F>::n(longint a) {
  return cln::cl_float(cln::cl_I(a), cln::default_float_format);
}
template<> inline cln::cl_F qqvvamp<cln::cl_F>::n(longint a, longint b) {
  return cln::cl_float(cln::cl_I(a) / cln::cl_I(b), cln::default_float_format);
}
std::ostream& operator<<(std::ostream&, const cln::cl_F&);
#endif

#ifdef QUAD_PRECISION
GiNaC::numeric tonumeric(const quadtype&);
double todouble(const quadtype&);
template<> inline quadtype qqvvamp<quadtype>::n(longint a) {
  return static_cast<quadtype>(a);
}
template<> inline quadtype qqvvamp<quadtype>::n(longint a, longint b) {
  return a / static_cast<quadtype>(b);
}
std::ostream& operator<<(std::ostream&, const quadtype&);
#endif

#ifdef DOUBLE_PRECISION
GiNaC::numeric tonumeric(const double&);
double todouble(const double& x);
template<> inline double qqvvamp<double>::n(longint a) {
  return static_cast<double>(a);
}
template<> inline double qqvvamp<double>::n(longint a, longint b) {
  return a / static_cast<double>(b);
}
#endif
#endif // QQVVAMP_H
