// "The two-loop helicity amplitudes for gg -> V1V2 -> 4 leptons"
// A. von Manteuffel, L. Tancredi
//
// ggvvamp package
//
// ggvvamp: core evaluator class (not recommended for direct usage, see ggvvampprec)


#ifndef GGVVAMP_H
#define GGVVAMP_H

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
extern "C" {
#include <quadmath.h>
}
typedef __float128 quadtype;
#endif
#include <ostream>

namespace GiNaC {
  class ex;
  class numeric;
}

// IR subtraction schemes
enum IRscheme { IRschemeQt=0, IRschemeCatani };

const int nAcoeff = 20; // coefficient type {1,...,20}
const int nloop = 2; // {0=1-loop, 1=2-loop}
const int nreim = 2; // {0=Re(), 1=Im()}

typedef int64_t longint; // type for numerators and denominators
// note: 31bit may not enough for amplitude expressions !


// amplitude tensor coefficients A for g g -> V V'
/* @ 1-loop, 2-loop,
 * template parameter specifies numeric type to be used internally,
 * see the paper for the definition of the coefficients A
 */
template<class numtype>
class ggvvamp {
public:
  // construct instance
  /* Nf is number of flavors in closed loops
   * ir the IR subtraction scheme to be used for the result */
  ggvvamp(int Nf, IRscheme ir);
  ~ggvvamp();

  // evaluate amplitude coefficients
  void compute(numtype s, numtype t, numtype ma2, numtype mb2);
  
  // settings
  int Nf; // number of light flavors
  IRscheme ir; // IR subtraction scheme
  
  // result
  // minimal subset of A_j^{[A]} coefficients (see paper for definition)
  // note: this class computes just 9 out of 20 coefficients:
  // {1, 2, 4, 5, 8, 9, 16, 17, 18}
  // the other coefficient can be obtained via crossing relations
  numtype A[nAcoeff+1][nloop][nreim]; // first index: 1..20

  // auxiliary array used for the computation
  numtype* r;
  bool compute_gpls; // performance tweaking for precision control

  // auxiliary methods
  // we avoid an explicit template parameter in calls by defining these as class members:
  static inline numtype n(longint a); // integer prefactor
  static inline numtype n(longint a, longint b); // rational prefactor
  static numtype n(const GiNaC::ex& a); // GPL result

#include "ggvvamp.xh"
};


// specializations

#ifdef ARBITRARY_PRECISION
GiNaC::numeric tonumeric(const cln::cl_F&);
GiNaC::numeric tonumeric(const cln::cl_N&);
double todouble(const cln::cl_F&);
template<> inline cln::cl_F ggvvamp<cln::cl_F>::n(longint a) {
  return cln::cl_float(cln::cl_I(a), cln::default_float_format);
}
template<> inline cln::cl_F ggvvamp<cln::cl_F>::n(longint a, longint b) {
  return cln::cl_float(cln::cl_I(a) / cln::cl_I(b), cln::default_float_format);
}
std::ostream& operator<<(std::ostream&, const cln::cl_F&);
#endif

#ifdef QUAD_PRECISION
GiNaC::numeric tonumeric(const quadtype&);
double todouble(const quadtype&);
template<> inline quadtype ggvvamp<quadtype>::n(longint a) {
  return static_cast<quadtype>(a);
}
template<> inline quadtype ggvvamp<quadtype>::n(longint a, longint b) {
  return a / static_cast<quadtype>(b);
}
std::ostream& operator<<(std::ostream&, const quadtype&);
#endif

#ifdef DOUBLE_PRECISION
GiNaC::numeric tonumeric(const double&);
double todouble(const double& x);
template<> inline double ggvvamp<double>::n(longint a) {
  return static_cast<double>(a);
}
template<> inline double ggvvamp<double>::n(longint a, longint b) {
  return a / static_cast<double>(b);
}
#endif
#endif // GGVVAMP_H
