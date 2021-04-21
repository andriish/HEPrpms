/*  Tree3.cc  */

/*  David A. Kosower, July 17, 2008  */

/*  Implementation of a cubic recursion relation for QCD amplitudes,
    based on a rewriting of light-cone Berends-Giele recursion relations.
    Uses the BlackHat momentum library.  Follows cubic.tex

*/

#define BRENDSGIELE_H_
#define BERENDSGIELE_IMPL_H_  // Don't include standard files
#include <vector>
#include <map>
#include "spinor.h"
#include "mom_conf.h"
#include "Tree.h"
#include "Tree_impl.h"
#include "particleid.h"
#include "util.h"
#define Re(v) real(v)
#define Im(v) imag(v)

#define RefTag "ref"

//#include "polylog.h"
#include <math.h>


#define BaseIndex 0 // [0] index not used in arg, helicity, or id.

#define isnt !=
#define is ==

namespace BH {
#undef MomentumConfiguration
#define MomentumConfiguration momentum_configuration<R>

#if 0
template<class T> inline T Max(T x,T y) {return (x > y ? x : y);}
inline bool IsOdd(int i) {return(i&1);}
#endif

// Returns the number of elements from start to end, understood in
// a cyclic sense.
inline int CountCyclic(const vector<int>& v,int start,int end)
{if (start <= end) return (end-start+1);
 else return(v.size()-start+end+1);}

// Returns the given index mod n
inline int IndexCyclic(const vector<int>& v,int index)
{while (index >= v.size()) index -= v.size();
 while (index < 0) index += v.size();
 return index;}

// Sums elements of the vector from the given start to the given end
// position, these boundaries understood in a cyclic sense.
inline int SumCyclic(const vector<int>& v,int start,int end)
{int sum = 0;
 if (start <= end) for (int j = start;  j <= end;  j += 1)  sum += v[j];
 else {for (int j = start;  j < v.size();  j += 1)  sum += v[j];
       for (int j = BaseIndex;  j <= end;  j += 1)  sum += v[j];}
 return sum;
}

#define GGG VType3(Gluon,Gluon,Gluon)
#define FFG VType3(Quark,Quark,Gluon)
#define FGF VType3(Quark,Gluon,Quark)
#define GFF VType3(Gluon,Quark,Quark)

// 6/4/08
#define SSG VType3(Scalar,Scalar,Gluon)
#define SGS VType3(Scalar,Gluon,Scalar)
#define GSS VType3(Gluon,Scalar,Scalar)

// 8/4/08
#define FF VType2(Quark,Quark)

#define PPP HType3(1,1,1)
#define MPP HType3(-1,1,1)
#define PMP HType3(1,-1,1)
#define PPM HType3(1,1,-1)
#define MMP HType3(-1,-1,1)
#define MPM HType3(-1,1,-1)
#define PMM HType3(1,-1,-1)
#define MMM HType3(-1,-1,-1)

// Additional "helicities" 'L' and 'R' (numerically represented by -3 & + 3)
// are used to split up the four-point vertices, see cubic.tex.
#define MPL HType3(-1,1,-3)
#define MPR HType3(-1,1,3)
#define PLM HType3(1,-3,-1)
#define PRM HType3(1,3,-1)
#define LMP HType3(-3,-1,1)
#define RMP HType3(3,-1,1)
#define PML HType3(1,-1,-3)
#define PMR HType3(1,-1,3)
#define MLP HType3(-1,-3,1)
#define MRP HType3(-1,3,1)
#define LPM HType3(-3,1,-1)
#define RPM HType3(3,1,-1)

// 8/4/08
#define PM HType2(1,-1)
#define MP HType2(-1,1)
#define PP HType2(1,1)
#define MM HType2(-1,-1)
#define PL HType2(1,-3)
#define PR HType2(1,3)
#define ML HType2(-1,-3)
#define MR HType2(-1,3)
#define LP HType2(-3,1)
#define RP HType2(3,1)
#define LM HType2(-3,-1)
#define RM HType2(3,-1)
#define LR HType2(-3,3)
#define RL HType2(3,-3)

// For fermion-electroweak vector vertices
#define MXP HType3(-1,0,1)
#define PXM HType3(1,0,-1)
#define MXL HType3(-1,0,-3)
#define MXR HType3(-1,0,3)
#define PXL HType3(1,0,-3)
#define PXR HType3(1,0,3)
#define LXM HType3(-3,0,-1)
#define RXM HType3(3,0,-1)
#define LXP HType3(-3,0,1)
#define RXP HType3(3,0,1)
#define LXR HType3(-3,0,3)
#define RXL HType3(3,0,-3)

namespace Tree {
#include "zero.h"

  int S1 = 1, S2 = 1, S3 = 1, S4 = 1, S5 = 1, S6 = 1, Sv = 1, Sv2 = 1, Sp = 1;
  void SetCSigns(int s1, int s2, int s3, int s4, int s5, int s6, int sv,
                 int sv2,
                 int sp)
{S1 = s1;  S2 = s2;  S3 = s3;  S4 = s4;  S5 = s5;  S6 = s6;
  Sv = sv; Sv2 = sv2; Sp = sp;}

  int Sf1 = 1, Sf2 = 1, Sf3 = 1, Sf4 = 1, Sf5 = 1, Sf6 = 1, Sf7 = 1;
  int Sf8 = 1, Sf9 = 1, Sf10 = 1, Sf11 = 1, Sf12 = 1, Sf13 = 1;
void SetCFSigns(int sf1, int sf2, int sf3, int sf4, int sf5, int sf6,
                int sf7, int sf8, int sf9, int sf10, int sf11, int sf12)
{Sf1 = sf1;  Sf2 = sf2;  Sf3 = sf3;  Sf4 = sf4;  Sf5 = sf5;  Sf6 = sf6;
  Sf7 = sf7; Sf8 = sf8; Sf9 = sf9;  Sf10 = sf10;  Sf11 = sf11; Sf12 = sf12;
}

// Three-point vertices, standard helicities
#define VgggPPM(k1,k2,k3) \
  -I*T(S1)*square(k.spa(ref,k3))*k.spb(k1,k2)/(k.spa(ref,k1)*k.spa(ref,k2))
#define VgggPMM(k1,k2,k3) \
  I*T(S2)*square(k.spb(ref,k1))*k.spa(k2,k3)/(k.spb(ref,k2)*k.spb(ref,k3))

// Three-point vertices, additional helicities
#define VgggMPL(k1,k2,k3) \
  -I*T(S3)*k.spa(ref,k1)*k.spb(ref,k2)*k.s(ref,k1)/              \
  (k.spb(ref,k1)*k.spa(ref,k2)*(k.s(ref,k1)+k.s(ref,k2)))
#define VgggMPR(k1,k2,k3) \
  I*T(S4)*k.spa(ref,k1)*k.spb(ref,k2)*k.s(ref,k2)/               \
  (k.spb(ref,k1)*k.spa(ref,k2)*(k.s(ref,k1)+k.s(ref,k2)))
#define VgggPML(k1,k2,k3) \
  I*T(S5)*k.spb(ref,k1)*k.spa(ref,k2)*k.s(ref,k2)/               \
  (k.spa(ref,k1)*k.spb(ref,k2)*(k.s(ref,k1)+k.s(ref,k2)))
#define VgggPMR(k1,k2,k3) \
  -I*T(S6)*k.spb(ref,k1)*k.spa(ref,k2)*k.s(ref,k1)/              \
  (k.spa(ref,k1)*k.spb(ref,k2)*(k.s(ref,k1)+k.s(ref,k2)))

// Three-gluon vertex for cubic recursion
// V_3(-K_{s1..e1}-K_{s2..e2},K_{s1..e1},K_{s2..e2})
// Lightcone means that it's the tree-gluon vertex with flatted momenta
template<class T> complex<T> inline
  VgggC(momentum_configuration<T>& k,
       int ref /* index of reference momentum */,
       int helicity0,
       const vector<int>& arg /* indices of momenta from which legs are taken */,
       int s1, int e1, int helicity1,
        int s2, int e2, int helicity2,
        const vector<int>& additionalMomenta1 = empty,
        const vector<int>& additionalMomenta2 = empty)
{//string key = GenKey("VgggC",helicity0,s1,e1,helicity1,s2,e2,helicity2,ref,arg);

 // until appropriate routine is available:
 string key = GenKey("VgggC",MakeVector(helicity0,s1,e1,helicity1,s2,e2,helicity2,
                                               ref),arg);
 complex<T> result;
 static complex<T> I(0,1); // keep macros below happy

 // cout << "ref [" << ref << "]: " << k[ref] << endl;
 // cout << helicity0 << " " << helicity1 << " " << helicity2 << ": ";
#define $_DEBUG1_ 0
#define $_DEBUG2_ 0
 if ( !k.get_value(key,result) || true ) {
   int k0 = NegativeFlatSum(k,ref,arg,s1,e1,s2,e2,
                            additionalMomenta1,additionalMomenta2);
   int k1 = FlatSum(k,ref,arg,s1,e1,additionalMomenta1);
   int k2 = FlatSum(k,ref,arg,s2,e2,additionalMomenta2);
    switch (HelicityType(helicity0,helicity1,helicity2)) {
    case MPP:
      result = VgggPPM(k1,k2,k0);  break;
    case PMP:
      result = VgggPPM(k2,k0,k1);  break;
    case PPM:
      result = VgggPPM(k0,k1,k2);  break;
    case PMM:
      result = VgggPMM(k0,k1,k2);  break;
    case MPM:
      result = VgggPMM(k1,k2,k0);  break;
    case MMP:
      result = VgggPMM(k2,k0,k1);  break;
    case MPL:
      result = VgggMPL(k0,k1,k2);  break;
    case PML:
      result = VgggPML(k0,k1,k2);  break;
    case MPR:
      result = VgggMPR(k0,k1,k2);  break;
    case PMR:
      result = VgggPMR(k0,k1,k2);  break;
    case PLM:
      result = VgggMPL(k2,k0,k1);  break;
    case MLP:
      result = VgggPML(k2,k0,k1);  break;
    case PRM:
      result = VgggMPR(k2,k0,k1);  break;
    case MRP:
      result = VgggPMR(k2,k0,k1);  break;
    case LMP:
      result = VgggMPL(k1,k2,k0);  break;
    case LPM:
      result = VgggPML(k1,k2,k0);  break;
    case RMP:
      result = VgggMPR(k1,k2,k0);  break;
    case RPM:
      result = VgggPMR(k1,k2,k0);  break;
    default:
      if (IsOdd(helicity0) and helicity0 >= -3 and helicity0 <= 3
          and IsOdd(helicity1) and helicity1 >= -3 and helicity1 <= 3
          and IsOdd(helicity2) and helicity2 >= -3 and helicity2 <= 3)
         {result = complex<T>(0,0);  break;}
      throw "Illegal helicity configuration [Vggg]";
    }
    //    cout << result << endl;
    k.put_value(key,result);}
 return result;
}

// In Blackhat, spa(q,x)/spa(q,-x) is 1 and spb(q,x)/spb(q,-x) is -1
// as opposed to the original Mathematica implementation with both being i.
// do we need to compensate somewhere???
#define VffgMPP(k1,k2n,k3) \
  -I * k.spa(ref,k1) * k.spb(k2n,k3)/(k.spa(ref,k3))

#define VffgPMP(k1n,k2,k3) \
  I * k.spb(k1n,k3) * k.spa(ref,k2)/(k.spa(ref,k3))

#define VffgMPM(k1,k2n,k3) \
  -I * k.spa(k1,k3) * k.spb(ref,k2n)/(k.spb(k3,ref))

#define VffgPMM(k1n,k2,k3) \
  I * k.spb(ref,k1n) * k.spa(k2,k3)/(k.spb(k3,ref))

#define VffgPML(k1n,k2,k3) \
  -I * T(Sf1) * k.spb(ref,k1n) * k.spa(ref,k2)/(T(1)*(k.s(ref,k2)-k.s(ref,k1n)))
#define VffgPMR(k1n,k2,k3) \
  -I * T(Sf2) * k.spb(ref,k1n) * k.spa(ref,k2)/(T(1)*(k.s(ref,k2)-k.s(ref,k1n)))
#define VffgMPL(k1,k2n,k3) \
  I * T(Sf3) * k.spa(ref,k1) * k.spb(ref,k2n)/(T(1)*(k.s(ref,k1)-k.s(ref,k2n)))
#define VffgMPR(k1,k2n,k3) \
  I * T(Sf4) * k.spa(ref,k1) * k.spb(ref,k2n)/(T(1)*(k.s(ref,k1)-k.s(ref,k2n)))

#define VffgLMP(k1n,k2,k3) \
  -I*OneOverSqrt2 * T(Sf5) * k.spa(ref,k2) * k.spb(ref,k3)/(k.spa(ref,k3)*k.spb(ref,k1n))
#define VffgRMP(k1,k2,k3) \
  -I*OneOverSqrt2 * T(Sf6) * k.spa(ref,k2) * k.spb(ref,k3)/(k.spa(ref,k3)*k.spa(ref,k1))
#define VffgLPM(k1n,k2,k3) \
  -I*OneOverSqrt2 * T(Sf7) * k.spb(ref,k2) * k.spa(ref,k3)/(k.spb(ref,k3)*k.spb(ref,k1n))
#define VffgRPM(k1,k2,k3) \
  -I*OneOverSqrt2 * T(Sf8) * k.spb(ref,k2) * k.spa(ref,k3)/(k.spb(ref,k3)*k.spa(ref,k1))


#define TrackingFermionFlow 1

  void DumpHelicities(int h0, int h1, int h2) ;

  bool debug = false;

// Light-cone fermion-fermion-gluon vertex
// Convention: fermion arrow always flows from + helicity to - helicity
// so that the + fermion's momentum needs to be negated before creating spinors
// V_3(-K_{s1..e1}-K_{s2..e2},K_{s1..e1},K_{s2..e2}) or cyclic perm
// Lightcone means that it's the tree vertex with flatted momenta
// To allow it to be used for ffg, gff, and fgf, we indicate
// a rotation of arguments rightwards by 0, 1, or 2.
template<class T> complex<T> inline
  VffgC(momentum_configuration<T>& k,
       int ref /* index of reference momentum */,
       int helicity0,
       const vector<int>& arg /* indices of momenta from which legs are taken */,
       int s1, int e1, int helicity1,
       int s2, int e2, int helicity2,
        const vector<int>& additionalMomenta1,
        const vector<int>& additionalMomenta2,
       int rotateRight)
{//string key = GenKey("VffgC",helicity0,s1,e1,helicity1,s2,e2,helicity2,ref,arg);

 // until appropriate routine is available:
 string key = GenKey("VffgC",MakeVector(helicity0,s1,e1,helicity1,s2,e2,helicity2,
                                        ref,rotateRight),arg,
                     // Because these vectors can be empty, and the need
                     // to distinguish "empty v" from "v empty", put in
                     // an additional "marker"
                     Join(additionalMomenta1,MakeVector(-1),additionalMomenta2));
 complex<T> result;
 static complex<T> I(0,1); // keep macros below happy
 static T OneOverSqrt2 = T(1)/sqrt(T(2));

#if 0
 {
   int k0 = NegativeFlatSum(k,ref,arg,s1,e1,s2,e2,
                            additionalMomenta1,additionalMomenta2);
   cout << "VffgC: k0 = " << k.mom(k0) << endl;
 }
#endif
 if ( !k.get_value(key,result) ) {
   int k0 = NegativeFlatSum(k,ref,arg,s1,e1,s2,e2,
                            additionalMomenta1,additionalMomenta2);
   int k1 = FlatSum(k,ref,arg,s1,e1,additionalMomenta1);
   int k2 = FlatSum(k,ref,arg,s2,e2,additionalMomenta2);
    int k0n, k1n;
    if (rotateRight is 2) {// fgf
      int ks = k2;  k2 = k1;  k1 = k0;  k0 = ks;
      int helicityS = helicity2;  helicity2 = helicity1;
      helicity1 = helicity0; helicity0 = helicityS;
    } else if (rotateRight is 1) {// gff
      int ks = k2;  k2 = k0;  k0 = k1;  k1 = ks;
      int helicityS = helicity2;  helicity2 = helicity0;
      helicity0 = helicity1; helicity1 = helicityS;
    }
    switch (HelicityType(helicity0,helicity1,helicity2)) {
    case MPP:
      k1n = Negative(k,k1);
      //      cout << "ffg -++" << endl;
      //      cout << "MPP [" << rotateRight << "," << additionalMomenta1.size() << "]: " <<  k.spa(ref,k0) << " " << k.spb(k1n,k2) << " " << (k.spa(ref,k2)) << endl;
      result = VffgMPP(k0,k1n,k2);  break;
    case PMP:
      k0n = Negative(k,k0);
      //      cout << "ffg +-+" << endl;
      result = VffgPMP(k0n,k1,k2);  break;
    case PMM:
      k0n = Negative(k,k0);
      //      cout << "ffg +--" << endl;
      result = VffgPMM(k0n,k1,k2);  break;
    case MPM:
      k1n = Negative(k,k1);
      //      cout << "ffg -+-" << endl;
      result = VffgMPM(k0,k1n,k2);  break;
    case PML:
      k0n = Negative(k,k0);
      result = VffgPML(k0n,k1,k2);  break;
    case PMR:
      k0n = Negative(k,k0);
      result = VffgPML(k0n,k1,k2);  break;
    case MPL:
      k1n = Negative(k,k1);
      result = VffgMPL(k0,k1n,k2);
#if 0
      if (rotateRight is 2) // Fermi minus needed???
         {result = -result; cout << "sign" << endl;}
#endif
      break;
    case MPR:
      k1n = Negative(k,k1);
      result = VffgMPR(k0,k1n,k2);
#if 0
      if (rotateRight is 2) // Fermi minus needed???
        result = -result;
#endif
      break;
    case LPM:
#if !TrackingFermionFlow
      k0n = Negative(k,k0);
#else
      k0n = k0;
#endif
      k1n = Negative(k,k1);
      result = VffgLPM(k0n,k1,k2);  break;
    case RPM:
      k1n = Negative(k,k1);
      result = VffgRPM(k0,k1,k2);  break;
    case LMP:
      k0n = Negative(k,k0);
      result = VffgLMP(k0n,k1,k2);  break;
    case RMP:
#if TrackingFermionFlow
      k0n = Negative(k,k0);
      result = VffgRMP(k0n,k1,k2);  break;
#else
      result = VffgRMP(k0,k1,k2);  break;
#endif
    case MLP:
      k1n = Negative(k,k1);
      result = -T(Sf9)*VffgLMP(k1n,k0,k2);  break;
    case MRP:
#if TrackingFermionFlow
      k1n = Negative(k,k1);
      result = -T(Sf10)*VffgRMP(k1n,k0,k2);  break;
#else
      result = -T(Sf10)*VffgRMP(k1,k0,k2);  break;
#endif
    case PLM:
      k0n = Negative(k,k0);
#if !TrackingFermionFlow
      k1n = Negative(k,k1);
#else
      k1n = k1;
#endif
      result = T(Sf11)*VffgLPM(k1n,k0n,k2);  break;
    case PRM:
      k0n = Negative(k,k0);
      result = T(Sf12)*VffgRPM(k1,k0n,k2);  break;
    case PPM:
    case MMP:
    case PPP:
    case MMM:
      result = complex<T>(0,0);  break;
    default:
      if (IsOdd(helicity0) and helicity0 >= -3 and helicity0 <= 3
          and IsOdd(helicity1) and helicity1 >= -3 and helicity1 <= 3
          and IsOdd(helicity2) and helicity2 >= -3 and helicity2 <= 3)
         {result = complex<T>(0,0);  break;}
      throw "Illegal helicity configuration [Vffg]";
    }

    // 5/20/08 New phase convention for spinors requires additional phase
    // here in order to get conventional phase for two-quark amplitudes
    result = -result;
    k.put_value(key,result);}
 if (debug) {
   cout << "VffgC: " << hex << HelicityType(helicity0,helicity1,helicity2) << dec << "; " << result << endl;
 }
 //    cout << "VffgC: " << result << endl;
 return result;
}

int SV1 = 1, SV2 = 1, SV3 = 1, SV4 = 1, SV5 = 1, SV6 = 1;
int SV7 = 1, SV8 = 1, SV9 = 1, SV10 = 1, SV11 = 1, SV12 = 1, SV13 = 1;
int SV14 = 1, SV15 = 1, SV16 = 1;

void SetCVSigns(int sv1, int sv2, int sv3, int sv4, int sv5, int sv6,
                int sv7, int sv8, int sv9, int sv10, int sv11, int sv12,
                int sv13, int sv14, int sv15, int sv16)
{SV1 = sv1;  SV2 = sv2;  SV3 = sv3;  SV4 = sv4;  SV5 = sv5;  SV6 = sv6;
  SV7 = sv7; SV8 = sv8; SV9 = sv9;  SV10 = sv10;  SV11 = sv11; SV12 = sv12;
  SV13 = sv13;  SV14 = sv14;  SV15 = sv15;  SV16 = sv16;
}

#define VffVPM(k1n,k2,pol) \
  T(SV1)*T(2)*k.spa(k2,pol)*k.spb(pol,k1n)

#define VffVMP(k1,k2n,pol) \
  T(SV2)*T(2)*k.spa(k1,pol)*k.spb(pol,k2n)

  // 9/20/08 Changed sign
#define VffVPL(k1n,k2n,pol) \
  (T(SV3)*Sqrt2*k.spa(ref,pol)*k.spb(pol,k1n)/k.spb(ref,k2n))

#define VffVML(k1,k2n,pol) \
  (-T(SV4)*Sqrt2*k.spa(k1,pol)*k.spb(pol,ref)/k.spb(ref,k2n))

#define VffVPR(k1n,k2,pol) \
  (T(SV5)*Sqrt2*k.spa(ref,pol)*k.spb(pol,k1n)/k.spa(ref,k2))

#define VffVMR(k1,k2,pol) \
  (T(SV6)*Sqrt2*k.spa(k1,pol)*k.spb(pol,ref)/k.spa(ref,k2))

#define VffVLR(k1n,k2,pol) \
  (T(SV7)*T(2)*k.spa(ref,pol)*k.spb(pol,ref)/(k.spa(ref,k2)*k.spb(ref,k1n)))

bool tagging = false;

bool fulldebug = false;
template<class T> inline complex<T>
  VffV(momentum_configuration<T>& k,
       int ref /* index of reference momentum */,
       int helicity0,
       const vector<int>& arg /* indices of momenta from which legs are taken */,
       int start, int end, int helicity1,
       int flow /* direction of fermion line: > 0 means */,
       int vectorK, int polarization,
       const vector<int>& additionalMomenta = empty)

{
 // until appropriate routine is available:
 string key = GenKey("VffV",MakeVector(helicity0,start,end,helicity1,
                                       ref,vectorK,polarization,flow),arg,
                     additionalMomenta);
 complex<T> result;
 static complex<T> I(0,1); // keep macros below happy
 static T Sqrt2 = sqrt(T(2));

#if 0
 cout << "V(" << helicity0 << " " << start << " " << end
      << " " << helicity1 << " " << vectorK << " " << polarization << " "
      << additionalMomenta.size() << ")"
      << hex << " " << HelicityType(helicity0,helicity1) << dec
      << endl;
#endif
 if ( !k.get_value(key,result) or tagging) {
   int k0 = NegativeFlatSum(k,ref,arg,start,end,vectorK,additionalMomenta);
   //   cout << "k0: " << k0 << " " << k.mom(k0) << endl;
   int k1 = FlatSum(k,ref,arg,start,end,additionalMomenta);
   int k0n, k1n;

   switch (HelicityType(helicity0,helicity1)) {
   case PM:
      k0n = Negative(k,k0);
      result = VffVPM(k0n,k1,polarization);  break;
   case MP:
      k1n = Negative(k,k1);
      result = VffVMP(k0,k1n,polarization);  break;
   case PL:
      k0n = Negative(k,k0);
#if !TrackingFermionFlow
      k1n = Negative(k,k1);
#else
      k1n = k1;
#endif
      result = VffVPL(k0n,k1n,polarization);  break;
   case PR:
      k0n = Negative(k,k0);
      result = VffVPR(k0n,k1,polarization);  break;
   case ML:
      k1n = Negative(k,k1);
      result = VffVML(k0,k1n,polarization);  break;
   case MR:
#if TrackingFermionFlow
      k1n = Negative(k,k1);
#else
      k1n = k1;
#endif
      result = -VffVMR(k0,k1n,polarization);  break;
   case LP:
#if !TrackingFermionFlow
      k0n = Negative(k,k0);
#else
      k0n = k0;
#endif
      k1n = Negative(k,k1);
      result = T(SV8)*VffVPL(k1n,k0n,polarization);  break;
   case RP:
      k1n = Negative(k,k1);
      result = T(SV9)*VffVPR(k1n,k0,polarization);  break;
   case LM:
      k0n = Negative(k,k0);
      result = T(SV10)*VffVML(k1,k0n,polarization);  break;
   case RM:
#if TrackingFermionFlow
      k0n = Negative(k,k0);
#else
      k0n = k0;
#endif
      result = -T(SV11)*VffVMR(k1,k0n,polarization);  break;
      //
      /* Note we must be careful about signs: there is an additional sign
         if the momentum after flipping the "L" helicity is going opposite
         to the net flow, which is out via k0 ??? */
   case LR:
#define SVF1 T(1)
#if TrackingFermionFlow
     if (flow > 0)
        {k1n = Negative(k,k1);
          result = -SVF1*T(SV13)*VffVLR(k0,k1n,polarization);}
     else {k0n = Negative(k,k0);
     if (fulldebug)
        {cout << "LR: " << flow << " " << k0 << " " << k1 << " " << k0n << endl;
          cout << "LR k0n: " << k.mom(k0n) << endl;
          cout << "LR k1: " << k.mom(k1) << endl;}
       // k0n = k0;
       result = -SVF1*T(SV14)*VffVLR(k0n,k1,polarization);}
     //     if (flow < 0) cout << "[VffV " << k0 << " " << k1 << "] " << result << endl;
     //     result *= T(flow);
#else
#define Explore 1
#if Explore
     k0n = k0;
#else
      k0n = Negative(k,k0);
#endif
#if _DEBUG_1
      cout << "k0 [LR]: " << k.mom(k0) << endl;
      cout << "k0n [LR]: " << k.mom(k0n) << endl;
      cout << "k1 [LR]: " << k.mom(k1) << endl;
#endif
      result = -VffVLR(k0n,k1,polarization);
#if _DEBUG_1
      cout << "result [LR]: " << result
           << "; " << k.spa(ref,polarization) << " " << k.spb(polarization,ref)
           << "| " << k.spa(ref,k1) << " " << k.spb(ref,k0n)
           << endl;
#endif
#endif
break;
   case RL:
#define SVF2 T(-1)
#if TrackingFermionFlow
     if (flow < 0)
        {k1n = Negative(k,k1);
          // k1n = k1;
     if (fulldebug)
        {cout << "RL: " << flow << " " << k0 << " " << k1 << " " << k1n << endl;
          cout << "RL k0: " << k.mom(k0) << endl;
          cout << "RL k1n: " << k.mom(k1n) << endl;}
          result = -SVF2*T(SV15)*VffVLR(k1n,k0,polarization);}
     else {k0n = Negative(k,k0);
       result = -SVF2*T(SV16)*VffVLR(k1,k0n,polarization);}
     //     if (flow < 0) cout << "[VffV " << k0 << " " << k1 << "] " << result << endl;
     //     result *= T(flow);
#else
#if Explore
     k1n = k1;
#else
      k1n = Negative(k,k1);
#endif
#if _DEBUG_1
      cout << "k0 [RL]: " << k.mom(k0) << endl;
      cout << "k1 [RL]: " << k.mom(k1) << endl;
      cout << "k1n [RL]: " << k.mom(k1n) << endl;
#endif
      //      result = -T(SV12)*VffVLR(k1n,k0,polarization);
      // Sign has to be the same for 2q 2g + ll...
      result = -VffVLR(k1n,k0,polarization);
#if _DEBUG_1
      cout << "result [RL]: " << result
           << "; " << k.spa(ref,polarization) << " " << k.spb(polarization,ref)
           << "| " << k.spa(ref,k0) << " " << k.spb(ref,k1xon)
           << endl;
#endif
#endif
break;
   default:
     if (IsOdd(helicity0) and helicity0 >= -3 and helicity0 <= 3
         and IsOdd(helicity1) and helicity1 >= -3 and helicity1 <= 3)
        {result = complex<T>(0,0);  break;}
     throw "Illegal helicity configuration [VffV]";
   }
   result = -result;
   k.put_value(key,result);}
#if 0
   cout << "VffV: "
<< hex << HelicityType(helicity0,helicity1) << dec << " " << result << endl;
#endif
  return(result);
}

// All vertices: three-point
template<class T> complex<T> static
  VertexC(momentum_configuration<T>& k,
         int ref /* index of reference momentum */,
         int helicity0,
         int id0,
         const vector<int>& arg,
         int s1, int e1, int helicity1, int id1,
          int s2, int e2, int helicity2, int id2,
          const vector<int>& additionalMomenta1 /* Of colorless objects */ = empty,
          const vector<int>& additionalMomenta2 /* Of colorless objects */ = empty)
{
#if 0
cout << hex << "VT: " << TypeName(id0) << TypeName(id1) << TypeName(id2)
      << VertexType(id0,id1,id2)
     << " [" << s1 <<":"<<e1<<","<<s2<<":"<<e2<<"]" << endl << dec;
#endif
// cout << hex << "VT: " << id0 << " " << id1 << " " << id2 << endl;
switch (VertexType(id0,id1,id2)) {
 case GGG:
   return(VgggC(k,ref,helicity0,arg,s1,e1,helicity1,s2,e2,helicity2,
                additionalMomenta1,additionalMomenta2));
 case FFG:
    return(VffgC(k,ref,helicity0,arg,s1,e1,helicity1,s2,e2,helicity2,
                 additionalMomenta1,additionalMomenta2,0));
 case FGF:
    return(VffgC(k,ref,helicity0,arg,s1,e1,helicity1,s2,e2,helicity2,
                 additionalMomenta1,additionalMomenta2,2));
 case GFF:
    return(VffgC(k,ref,helicity0,arg,s1,e1,helicity1,s2,e2,helicity2,
                 additionalMomenta1,additionalMomenta2,1));
#if 0
 // 6/4/08
 case SSG:
    return(Vssg(k,ref,helicity0,arg,s1,e1,helicity1,s2,e2,helicity2,
                 additionalMomenta1,additionalMomenta2,0));
 case SGS:
    return(Vssg(k,ref,helicity0,arg,s1,e1,helicity1,s2,e2,helicity2,
                 additionalMomenta1,additionalMomenta2,2));
 case GSS:
    return(Vssg(k,ref,helicity0,arg,s1,e1,helicity1,s2,e2,helicity2,
                 additionalMomenta1,additionalMomenta2,1));
#endif
 default:
   cout << endl;
cout << hex << "VT: " << TypeName(id0) << TypeName(id1) << TypeName(id2)
     << " [" << s1 <<":"<<e1<<","<<s2<<":"<<e2<<"]"
     << " " << additionalMomenta1.size()
     << " " << additionalMomenta2.size()
     << endl << dec;
   cout << "VertexC throw" << hex << VertexType(id0,id1,id2) << dec << endl;
   throw "Illegal vertex type [VertexC]";
}
}

template<class T> complex<T>
  VertexV(momentum_configuration<T>& k,
          int ref /* index of reference momentum */,
          int helicity0,
          int id0,
          const vector<int>& arg,
          int start, int end, int helicity,
          int flow /* direction of fermion line */,
          int vectorK /* vector's momentum */,
          int polarization,
       const vector<int>& additionalMomenta = empty)
{if (IsQuark(id0)) return(VffV(k,ref,helicity0,arg,start,end,helicity,flow,
                               vectorK,polarization,additionalMomenta));
  else throw "Illegal vertex type [VertexV]";
}



bool FlavorsOK(const ParticleID& offshell /* of the current's offshell leg */,
               const vector<ParticleID>& leg /* of remaining legs */,
               int start, int end /* indices into the vectors */,
               const vector<int>& coupleTo /* quark flavor */ = empty,
               int offshellMass = defaultMass,
               const vector<int>& massValue = empty);
bool Classify3(const ParticleID& offshell /* of the current's offshell leg */,
               const vector<ParticleID>& leg /* of remaining legs */,
               int start1, int end1 /* indices into the vectors of subsidiary current 1*/,
               int start2, int end2 /* indices into the vectors of subsidiary current 2*/,
               ParticleID& id1, ParticleID& id2 /* resulting ids */,
               int& internal1, int& internal2 /* resulting mass indices */,
                      int scalarFlavor /* of first (or second) subsidiary current,
                                          if needed to resolve ambiguities
                                          between gluons & scalars */,
               int offshellMass = defaultMass,
               const vector<int>& massValue = empty);


  void DumpHelicity(int h,string tagL = "L", string tagM = "-",
                    string tagP = "+", string tagR = "R")
{
  switch (h) {
  case -3: cout << tagL;  break;
  case -1: cout << tagM;  break;
  case 1: cout << tagP;  break;
  case 3: cout << tagR;  break;
  default: cout << "?"; break;
  }
}

void DumpHelicities(int h0, int h1, int h2)
{
  DumpHelicity(h0);
  DumpHelicity(h1);
  DumpHelicity(h2);
}

  void DumpHelicities(int h0, int h1, int h2, int h3)
{
  DumpHelicity(h0);
  DumpHelicity(h1);
  DumpHelicity(h2);
  DumpHelicity(h3);
}

void DumpVertex(int id0, int h0, int id1, int h1, int id2, int h2)
{
switch (VertexType(id0,id1,id2)) {
 case GGG: cout << "ggg";  break;
 case FFG:
   DumpHelicity(h0,"L","M","P","R");
   DumpHelicity(h1,"L","M","P","R");
   DumpHelicity(h2,"L","M","P","R");
   break;
 case GFF:
   DumpHelicity(h1,"L","M","P","R");
   DumpHelicity(h2,"L","M","P","R");
   DumpHelicity(h0,"L","M","P","R");
   break;
 case FGF:
   DumpHelicity(h2,"L","M","P","R");
   DumpHelicity(h0,"L","M","P","R");
   DumpHelicity(h1,"L","M","P","R");
   break;
 default:
   cout << "???";  break;
 }
}

void DumpTaggedHelicities(int id0, int h0,
                          int id1, int h1,
                          int id2, int h2,
                          int id3, int h3)
{ DumpHelicity(h0);
  if (IsQuark(id0)) cout << "q";
  DumpHelicity(h1);
  if (IsQuark(id1)) cout << "q";
  DumpHelicity(h2);
  if (IsQuark(id2)) cout << "q";
  DumpHelicity(h3);
  if (IsQuark(id3)) cout << "q";
}

#define hL -3
#define hR 3

int ST1 = 1, ST2 = 1, ST3 = 1, ST4 = 1, ST5 = 1;
int STv = 1;
int SThelicity1, SThelicity2;
bool verbose = false;
bool verbose2 = false;
void TagTerms(int st1, int st2, int st3, int st4, int st5,
              int helicity1, int helicity2)
{ST1 = st1; ST2 = st2; ST3 = st3;  ST4 = st4;  ST5 = st5;
  STv = 1; // reset
  SThelicity1 = helicity1;
  SThelicity2 = helicity2;
  tagging = false;
  if (st1 isnt 1 or st2 isnt 1 or st3 isnt 1
      or st4 isnt 1 or st5 isnt 1) tagging = true;
}


template<class T> complex<T>
  Vertex(momentum_configuration<T>& k,
         int ref /* index of reference momentum */,
         int helicity0,
         int id0,
         const vector<int>& arg,
         int s1, int e1, int helicity1, int id1,
         int s2, int e2, int helicity2, int id2,
         int s3, int e3, int helicity3, int id3);

/* Amputated cubic light-cone current for QCD tree amplitudes (propagator
   for off-shell leg is not included).

   Arguments are similar to J() in tree1.cc:
     a momentum configuration;
     an optional reference momentum ("-1" argument to use the default);
     a particle_ID object supplying the helicity and particle id of the
       offshell leg;

   Because the fermion propagator is linear in the momentum, while all
   momentum arguments are taken to be outgoing, there is a phase that
   arises from converting one end of the fermion line back to the direction
   of momentum flow.  By convention, we take the fermion line to flow
   from positive to negative helicity, so that there is a possible phase
   from negating the positive-helicity quark's momentum.  Inside
   diagrams, we need to keep track of the flow direction for Vqq vertices
   which may include no fermion line of positive or negative helicity,
   but only of the extra L or R helicities.

*/
#define DebugSigns 0
template<class T> complex<T>
 Jc(momentum_configuration<T>& k,
    int ref0 /* Supply negative number to use default one */,
    ParticleID offshell /* helicity, type etc */,
#if 0
    int helicityO /* helicity of offshell leg */,
    int idO /* ParticleID of offshell leg */,
#endif
    const vector<int>& arg /* momentum labels of colored arguments */,
#if 0
    const vector<int>& helicity,
    const vector<ParticleID>& id,
#endif
    const vector<ParticleID>& leg,
    int start, int end /* indices into the vectors */,
    int flow /* direction: out (+1) or in (-1) of a fermionic offshell leg
                of helicity L or R -- for helicity +/- the direction is
                deduced from the helicity */,
    // Vector bosons
    const vector<int>& vectorK /* momenta */ = empty,
    const vector<int>& polarization = empty,
    const vector<int>& coupleTo /* quark flavor */ = empty,
    int offshellMass = defaultMass,
    const vector<int>& massValue = empty)
{ // Get reference momentum
 size_t ref /* index of reference momentum */;
 const string refKey(RefTag);
 if (ref0 >= 0) ref = ref0;
 else if (not k.get_label(refKey,ref)) {
   T dummy;
   // inserting momentum<R> doesn't seem to yield correct lambdas
   momentum<complex<T> > refMom = GenerateMomentum(dummy);
   ref = k.insert(refMom);
   k.put_label(refKey,ref);
 }

   int helicityO = offshell.helicity();
   int idcodeO = ParticleCode(offshell);
#if 0
   cout << "id O: " << idcodeO << endl;
#endif

   vector<int> helicity = Helicities(leg);
   vector<int> idcode = ParticleCode(leg);

 //string key = GenKey("J",helicityO,idO,start,end,arg,helicity,id);
 // until the right routine is available:
   string key = GenKey("Jc",start,end,MakeVector(ref,helicityO,idcodeO,offshellMass,flow),
                     Join(massValue,arg),Join(helicity,idcode),
                     Join(vectorK,polarization));

 complex<T> result;

 if (debug) {cout << "D: " << helicityO;
   for (int jj = start;  jj <= end;  jj += 1)
     cout << " " << helicity[jj];
   cout << "; " << key << "; " << k.get_value(key,result);
   cout << "; " << result;
   cout << endl;}

 // cout << "G " << key << ": " << k.get_value(key,result) << endl;
 if (not k.get_value(key,result) or verbose or tagging) {
    // Must compute it
    complex<T> term;

    // Number of arguments including off-shell leg
    int n = CountCyclic(arg,start,end)+1;

 if (verbose2 and n is 2) cout << "J_2 computing..." << endl;
    //       cout << "---" << endl;
    //    cout << "J_" << n <<": " << idcodeO << "; ";
    // PrintVector(id); cout << " [" << start << ":" << end << "] " << endl;

    // First look at special cases:
    // End-points of recursion: three-point currents are just vertices

    if (n is 3 and vectorK.size() is 0)
       {result = T(Sv2)*VertexC(k,ref,helicityO,idcodeO,arg,
                        start,start,helicity[start],idcode[start],
                        end,end,helicity[end],idcode[end]);
         //       cout << "--- " << result << endl;
         //    cout << "P (1)" << key << ": " << result << endl;
        k.put_value(key,result);
        return result;}

#if 0 // Can't use this check, when using +/- 3 for special extra helicities
    // All-plus vanishes, likewise all-minus (independent of number of leptons)
    int netHelicity = helicityO+SumCyclic(helicity,start,end);

    if (netHelicity == (n=CountCyclic(arg,start,end)+1)
        || netHelicity == -n)
       {result = complex<T>(0,0);
    cout << "P (2)" << key << ": " << result << endl;
k.put_value(key,result);  return result;}
#endif

    // Small number of negative helicities, expand using MHV vertices
    // or perhaps ordinary on-shell recursion
    //else {}

    // General case: sum over currents joined to three-point and
    // four-point vertices

    if (not FlavorsOK(offshell,leg,start,end,coupleTo,
                      offshellMass,massValue))
      return(complex<T>(0,0));

    vector<bool> flavorParity;
    int flavorO = FlavorOf(offshell);

    result = complex<T>(0,0);

    // 5/20/08 New phase convention for spinors requires different phase
    // for two-particle "current"
    complex<T> externalLeg = complex<T>(0,1);

    /* If we have a fermion of the right flavor, each vector couples
       directly to it */
    if (IsQuark(offshell))
       {int flavor = FlavorOf(offshell);
         if (verbose2 and n is 2) {cout << "    cs: " << coupleTo.size();
           if (coupleTo.size() is 1) cout << ": " << coupleTo[0];
           cout << " [" << flavor << "]; ";
           cout << helicityO << " ";
           cout << helicity[start];
           cout << endl;}
         for (int v = 0;  v < coupleTo.size();  v += 1)
           if (coupleTo[v] is flavor)
              {vector<int> restK = vectorK, restPol = polarization,
                  restCouple = coupleTo;
                restK.erase(restK.begin()+v);
                restPol.erase(restPol.begin()+v);
                restCouple.erase(restCouple.begin()+v);
                int sum = MomentumSum(k,arg,start,end,restK);
                // No massive quarks yet...
                complex<T> prop;
                if (n is 2) prop = complex<T>(T(Sp),0);
                else prop = k.m2(sum);
                for (int h = -3;  h <= 3;  h += 2)
                   {if (n is 2 and h isnt helicity[start]) continue;
                    term = VertexV(k,ref,helicityO,idcodeO,arg,
                                   start,end,h,flow,
                                   vectorK[v],polarization[v],restK);
                    if (verbose2 and n is 2) cout << "    term V: " << term << " " << h
                                     << "| " << ST3 << " " << STv
                                     << " (" << ST2
                                     << "; " << SThelicity1 << " " << SThelicity2
                                     << ")" << endl;
#define Tagging 0
#if Tagging
                    if (tagging and ST3 isnt 0 and false)
                      cout << "term: " << term << endl;
                    if (tagging and n is 3) term *= T(ST1);
                    if (false and n is 3 and helicityO is hL and h is hR
                        and not IsZero(term)
                        and not IsZero(Jc(k,ref,NParticleID(-h,offshell),
                                         arg,leg,start,end,flow,
                                         restK,restPol,restCouple,
                                          offshellMass,massValue)))
                       {fulldebug = true;
                      cout << "V_{LR}:" << " " << flow
                           << " " << VertexV(k,ref,helicityO,idcodeO,arg,
                                   start,end,h,flow,
                                   vectorK[v],polarization[v],restK)
#if 0
                           << "  " << Jc(k,ref,NParticleID(-h,offshell),
                                         arg,leg,start,end,flow,
                                         restK,restPol,restCouple,
                                          offshellMass,massValue)
#endif
                           << endl;
                      fulldebug = false;}
                    if (false and n is 3 and helicityO is hR and h is hL
                        and not IsZero(term)
                        and not IsZero(Jc(k,ref,NParticleID(-h,offshell),
                                         arg,leg,start,end,flow,
                                         restK,restPol,restCouple,
                                          offshellMass,massValue)))
                       {fulldebug = true;
                      cout << "V_{RL}:" << " " << flow
                           << " " << term
#if 0
                           << "  " << Jc(k,ref,NParticleID(-h,offshell),
                                         arg,leg,start,end,flow,
                                         restK,restPol,restCouple,
                                          offshellMass,massValue)
#endif
                           << endl;
                      fulldebug = false;}
                    if (tagging and n is 2)
                       {if (verbose2 and STv is 0) cout << "elim" << endl;
                        term *= T(STv);}
                    if (tagging and (n is 3 or n is 2))
                       {if (helicityO isnt SThelicity1
                            and SThelicity1 isnt 0) term *= 0;
                        if (h isnt SThelicity2
                            and SThelicity2 isnt 0) term *= 0;
                        if (false) cout << "n: " << n << "; h: " << h << "; term: "
                             << term << endl;}
#endif
                    if (verbose2 and n is 2) cout << "    term V (2): " << term << " " << h << endl;
                     if (IsZero(term)) continue;
                     complex<T> jc;
                     debug = false;
                     if (n is 2) term *= externalLeg;
                     else term *= (jc=Jc(k,ref,NParticleID(-h,offshell),
                                         arg,leg,start,end,flow,
                                         restK,restPol,restCouple,
                                         offshellMass,massValue));
                     debug = false;
                     //                     cout << "term V 3: " << term << " " << h << endl;
                     if (h isnt hL and h isnt hR) term /= prop;
#if 0
                     if (n is 4 or (verbose and n is 3))
                       cout << "term V: " << term << " " << helicityO << " " << h << endl;
#endif
                     if (verbose2 and n is 2)
                       cout << "    term V: " << term << " " << h << endl;
                     if (n is 3 and false)
                     {complex<T> direct = k.spbb(3,polarization[v],sum,2)*k.spa(ref,1)/(prop*k.spa(ref,2));
                       cout << "V direct: " << direct << " " << direct/term << endl;
                       int sumf = FlatSum(k,ref,arg,start,end);
                       complex<T> directF = k.spbb(3,polarization[v],sumf,2)*k.spa(ref,1)/(prop*k.spa(ref,2));
                       if (h is -1) cout << "V directF: " << directF << " " << directF/term << endl;
                       complex<T> v3 = -complex<T>(0,1)*k.spb(sumf,2)*k.spa(ref,1)/k.spa(ref,2);
                       if (h is -1) debug = true;
                       complex<T> v3o = VffgC(k,ref,-h,arg,
                                              start,start,helicity[start],
                                              end,end,helicity[end],
                                              empty,empty,0);
                       debug = false;
                       int k0n = NegativeFlatSum(k,ref,arg,start,start,end,end);
                       int k0x = NegativeFlatSum(k,ref,arg,start,end);
                       if (h is -1)
                         cout << "sumf: " << k.mom(sumf) << "; " << k.m2(sumf) << endl
                              << "k0n: " << k.mom(k0n) << "; " << k.m2(k0n) << endl
                              << "k0x: " << k.mom(k0x) << "; " << k.m2(k0x) << endl;
                       complex<T> v3alt = complex<T>(0,1) * k.spb(k0n,arg[end]) * k.spa(ref,arg[start])/(k.spa(ref,arg[end]));

                       if (h is -1)
                         cout << "rat: " << k.spb(ref,arg[start])/k.spb(ref,1)
                              << " " << k.spa(ref,arg[end])/k.spa(ref,2)
                              << " " << k.spb(k0n,arg[end])/k.spb(sumf,2)
                              << endl;
                       if (h is -1) cout << "prop: " << prop << " " << k.s(1,2) << endl;
                         if (h is -1) cout << "v3 : " << v3 << " " << jc << " " << v3/jc << " " << v3o << " " << v3/v3o << " " << v3alt << " " << v3/v3alt << endl;
                       if (h is -1) cout << "adj: " << v3/jc*term << " "
                                         << directF*jc/(v3*term) << endl;
                     }
                     result -= term;}
              }
         //         if (n is 2) cout << "result V: " << result << endl;
       }

    //    if (n is 4) cout << "result V: " << result << endl;

    // Three-point vertex terms
    for (int j1 = 1;  j1 <= n-2;  j1 += 1)
       {int midL, midR; // Indices of j1th & j1+1st legs "arg"
       // Figure out internal labels (common to all helicities)
       midL=IndexCyclic(arg,start+j1-1);
       midR=IndexCyclic(arg,start+j1);
       /* Particle type of offshell leg of first daughter current should
          be the lone imbalanced fermion flavor (argument) or gluon if its
          arguments are fermion-number balanced; as the overall imbalance
          has already been checked above, this ensures correct type
          for other daughter current */
#if 0
       int imbalance = 0;
       bool skip = false;
       flavorParity = FermionParity(leg,start,midL);
#if 0
       cout << "id[" << start << ":" << midL <<"]: ";
       PrintVector(id); cout << "; p: "; PrintVector(flavorParity);
       cout << endl;
#endif
       for (int f = 0;  f < flavorParity.size();  f += 1)
         if (flavorParity[f])
           if (imbalance) {skip = true;  break;}
           else imbalance = f;  // flavor-to-be of internal leg
       // offshell is gluon -> any lone imbalance or zero allowed
       // offshell is fermion -> same fermion or gluon allowed
       if (skip or (IsQuark(offshell) and imbalance isnt 0
                    and imbalance isnt flavorO)) continue;
       ParticleID id1 = imbalance is 0 ?
         GluonID : FlavoredQuarkID(imbalance);
       ParticleID id2 = imbalance is 0 ?
         offshell : (IsQuark(offshell) ? GluonID : id1);
#else
       ParticleID id1, id2;
       int internal1, internal2;  // Internal legs' mass indices
       if (not Classify3(offshell,leg,start,midL,midR,end,id1,id2,
                         internal1,internal2,0,offshellMass,massValue))
         continue;
#endif

       // Sum over partitions of vectors onto the two subcurrents J1 & J2
        vector<int> vector1base /* Can couple only to J1 */,
           vector2base /* Can couple only to J2 */,
           polarization1base, polarization2base,
           couple1base,couple2base,
           vectorEither /* Can couple to either */,
           polarizationEither, coupleToEither;
         vector<int> count1 = FermionCount(leg,start,midL);
         for (int v = 0;  v < vectorK.size();  v += 1)
            {if (count1[coupleTo[v]] > 0)
                if (coupleTo[v] is FlavorOf(id2))
                   {vectorEither.push_back(vectorK[v]);
                     polarizationEither.push_back(polarization[v]);
                     coupleToEither.push_back(coupleTo[v]);}
                else {vector1base.push_back(vectorK[v]);
                  polarization1base.push_back(polarization[v]);
                  couple1base.push_back(coupleTo[v]);}
              /* We already know it must couple *somewhere* -- otherwise
                 FlavorsOK would have caused us to return 0 */
              else {vector2base.push_back(vectorK[v]);
                polarization2base.push_back(polarization[v]);
                couple2base.push_back(coupleTo[v]);}}
         /* Sum over all possible assignments of "vectorEither"
            by constructing the set of all assignments, a bit set;
            assume that we have no more than 8 vectors */
        for (unsigned short int inPart1 = 0;
             inPart1 <= ((unsigned short int)0xFF >> (8-vectorEither.size()));
             inPart1 += 1)
          { vector<int> vector1 = vector1base, vector2 = vector2base;
            vector<int> polarization1 = polarization1base,
              polarization2 = polarization2base;
            vector<int> couple1 = couple1base, couple2 = couple2base;
           for (int v = 0;  v < vectorEither.size();  v += 1)
             if (inPart1 & (1<<v)) {vector1.push_back(vectorEither[v]);
               polarization1.push_back(polarizationEither[v]);
               couple1.push_back(coupleToEither[v]);}
             else {vector2.push_back(vectorEither[v]);
               polarization2.push_back(polarizationEither[v]);
               couple2.push_back(coupleToEither[v]);}

#if 1
           if (verbose2) {
           if (n is 3) cout << "  ";
       cout << "@n: " << n << " (" << j1 << "," << midL << ") "
            << vector1.size() << " " << vector2.size() << endl;}
#endif
#if 0
       cout << "id: " << idcodeO << " " << ParticleCode(id1) << " " << ParticleCode(id2) << endl;
       cout << IsQuark(offshell) << "; " << imbalance << endl;
#endif

       // Sum over helicities: note that h_i is the helicity on the vertex,
        // the negative of the helicity on the current.  '-3' and '3' stand
       // for the additional 'L' and 'R' helicities respectively
        for (int h1 = -3;  h1 <= 3;  h1 += 2)
        for (int h2 = -3;  h2 <= 3;  h2 += 2)
           {complex<T> J1, J2;
#if 0
            // If not an allowed configuration, skip it
            if ((h1+h2+helicityO == 3 or h1+h2+helicityO == -3)
                and massValue.size() is 0) continue;
#endif
            /* Is it an allowed configuration?  If so, vertex is non-vanishing;
               compute it first to see... */
            term = T(Sv)*VertexC(k,ref,helicityO,idcodeO,arg,
                                 start,midL,h1,ParticleCode(id1),
                                 midR,end,h2,ParticleCode(id2),vector1,vector2);
            complex<T> vertex = term;
            if (IsZero(term)) continue;

#if 0
            if (n is 4 or debug or (n is 3 and tagging)) {
            cout << "V [";  DumpHelicities(helicityO,h1,h2);
            cout << "; " << start << ".." << midL << ";" << midR << ".." << end;
            cout << "]: " << term << "; " << endl;
            }
#endif

#if Tagging
            if (n is 4 and tagging) STv = 0;
            if (tagging and n is 3 and IsGluon(offshell))
              STv = vector1.empty() ? ST5 : ST4;

            if (n is 4 and IsQuark(id1) and IsQuark(id2))
              STv = vector1.empty() ? ST3 : ST2;

            if (verbose2 and n is 4 and tagging)
                      cout << ST1 << " " << ST2 << " " << ST3 << "  " << STv
                           << "| " << j1 << " " << helicityO
                           << " (" << ParticleCode(id1) << "," << h1 << ") "
                           << " (" << ParticleCode(id2) << "," << h2 << ") "
                           << vector1.size() << " " << vector2.size()
                           << " " << helicity[start]
                           <<endl;
#endif
            if (verbose2 and (n is 4 or n is 3))
               {if (n is 3) cout << "  ";
                cout << "term [";  DumpHelicities(helicityO,h1,h2);
                cout << "] ("
                     << idcodeO << " "
                     << ParticleCode(id1) << " "
                     << ParticleCode(id2) << " "
                     << "): "
                     << term << " " << j1 << "  "
                     << vector1.size() << "[" << vector1.empty() << "]"
                     << " " << vector2.size() << "[" << vector2.empty() << "]"
                     << endl;}
            if (IsQuark(id1) and (h1 is hL or h1 is hR))
              // Need to indicate flow direction; rely on vertex having
              // only one L or R helicity
              flow = IsQuark(offshell) ? -helicityO : -h2;
            /* For allowed configurations, vertex is always non-vanishing;
               we may want to compute shorter current first, but just do
               them in order for now. */
            if (j1 is 1 and vector1.empty()) // 2-pt current: do helicities match?
               if (helicity[start] isnt h1) continue; else J1 = externalLeg;
            else J1 = Jc(k,ref,NParticleID(-h1,id1),arg,leg,start,midL,flow,
                         vector1,polarization1,couple1,
                         internal1,massValue);
            if (verbose2 and ((n is 4 and tagging and ST3 isnt 0) or n is 3))
               {if (n is 3) cout << "  ";
                 cout << "J1: " << J1 << endl;}
            if (IsZero(J1)) continue;
            // Fermion phase convention: extra factor of -i for +;- configuration
            // Is this phase independent of the sign of the energy?
            //            if (j1 is 1 and IsQuark(id1) and h1 < 0) J1 = -J1;
            if (IsQuark(id2) and (h2 is hL or h2 is hR))
              // Need to indicate flow direction
              flow = IsQuark(offshell) ? -helicityO : -h1;
            if (j1 is n-2 and vector2.empty()) // 2-pt current: do helicities match?
               if (helicity[end] isnt h2) continue; else J2 = externalLeg;
            else J2 = Jc(k,ref,NParticleID(-h2,id2),arg,leg,midR,end,flow,
                         vector2,polarization2,couple2,
                         internal2,massValue);
#if 0
            if (n is 4 and IsZero(J2))
               {cout << "J2=0;" << h2 << endl;}
#endif
#if 0
            if (n is 5 and IsZero(J2))
               {cout << "J2=0;" << h2 << endl;}
#endif
            if (verbose2 and ((n is 4 and tagging and ST3 isnt 0) or n is 3))
               {if (n is 3) cout << "  ";
                 cout << "J2: " << J2 << "  " << (end-midR+1) << " "
                      << ParticleCode(leg[midR])
                      << endl;}
            if (IsZero(J2)) continue;
            //            if (j1 is n-2 and IsQuark(id2) and h2 < 0) J2 = -J2;
            int sum1 = MomentumSum(k,arg,start,midL,vector1);
            int sum2 = MomentumSum(k,arg,midR,end,vector2);
            // Need to get at scalar masses for massive case...
            complex<T> prop1 = k.m2(sum1);
            complex<T> prop2 = k.m2(sum2);
            complex<T> props;
            if (h1 is hL or h1 is hR) prop1 = complex<T>( T(Sp), 0 );
            if (h2 is hL or h2 is hR) prop2 = complex<T>( T(Sp), 0 );
            if (j1 is 1 and vector1.empty()) // Because n>3 here, j1 cannot be both 1 and n-2
               {// No massive quarks yet...
                 if (IsScalar(id2) and internal1 >= 0)
                   prop2 -= k.m2(internal1);
                 props = prop2;}
            else if (j1 is n-2 and vector2.empty())
               {if (IsScalar(id1) and internal2 >= 0)
                   prop1 -= k.m2(internal2);
               props = prop1;}
            else {if (IsScalar(id1) and internal1 >= 0)
                 prop1 -= k.m2(internal1);
               if (IsScalar(id2) and internal2 >= 0)
                 prop2 -= k.m2(internal2);
               props = prop1 * prop2;}
            // Phase convention for fermions?
#if 0
            cout << "h_in: ";
            //            print(helicity,start,end);
            cout << endl;
            cout << "h: " << helicityO << " " << h1 << " " << h2 << endl;
            cout << "J1: " << J1 << "; J2: " << J2;
            cout << "; V: " << VertexC(k,ref,helicityO,idO,arg,
                                      start,midL,h1,id1,  midR,end,h2,id2);
            cout << endl;
#endif
            term *= J1 * J2/props;
            // 5/20/08 New phase convention for spinors requires additional sign
#if 0
            if (IsQuark(id1) and j1 isnt 1) term = -term;
            if (IsQuark(id2) and j1 isnt n-2) term = -term;

            if (IsQuark(id1) and j1 is 1) term = -term;
            if (IsQuark(id2) and j1 is n-2) term = -term;
#endif
#if QuarkFlip
            if (IsQuark(id1)) term = -term;
            if (IsQuark(id2)) term = -term;
#endif
#if 0
            cout << "n: " << n << "; " << j1 << "; ref: " << ref << endl;
            cout << "mv: " << massValue << endl;
            cout << "h: " << helicityO << " " << h1 << " " << h2 << endl;
            cout << "J1: " << J1 << "; J2: " << J2;
            cout << "; V: " << VertexC(k,ref,helicityO,idcodeO,arg,
                                      start,midL,h1,ParticleCode(id1),
                                      midR,end,h2,ParticleCode(id2));
            cout << "; props: " << props;
            cout << endl;
            cout << "id: " << idcodeO << " " << ParticleCode(id1) << " " << ParticleCode(id2) << endl;
#endif
#if 0
            if (n is 3 and tagging and !vectorK.empty()) {
              cout << "term [";  DumpHelicities(helicityO,h1,h2);
              cout << "]: " << term << " " << j1 << "  " << props << " "
                   << vector1.size() << " " << vector2.size()
                   << "  " << k.s(1,3) << " " << k.s(2,3)
                   << endl;
              if (false and IsGluon(offshell)
                  and (IsQuark(leg[start]) or IsQuark(leg[start+1]))
                  and !vector1.empty() and j1 is 2) {
                complex<T> J1o;
                complex<T> J1alt = complex<T>(0,0);
                complex<T> J1b = complex<T>(0,0);
                vector<int> arg1(arg.size()+1);
                vector<particle_ID> leg1(leg.size()+1);
                int photon = k.Sum(vectorK);
                for (int insert = start;  insert <= midL;  insert += 1)
                   {for (int j = start;  j <= insert;  j += 1)
                        {arg1[j] = arg[j];  leg1[j] = leg[j];}
                     arg1[insert+1] = photon;  leg1[insert+1] = GluonID;
                     for (int j = insert+1;  j <= end;  j += 1)
                        {arg1[j+1] = arg[j];  leg1[j+1] = leg[j];}
                     for (int j = end+2;  j < leg1.size();  j += 1)
                       leg1[j] = GluonID;
                     complex<T> J1term;
                       J1alt += (J1term = Jc(k,ref,NParticleID(-h1,id1),arg1,leg1,
                                             start,midL+1,flow));
                       cout << "J1term: " << J1term << endl;
                   }
                {complex<T> J1x;
                  // (cycled3) "gamma" 4- 1+q 2-q
                  int k41 = k.Sum(arg[0],arg[1]);
                  int k41f = FlatSum(k,ref,MakeVector(arg[0],arg[1]),0,1);
#if 0
                  J1x = VertexV(k,ref,-h1,ParticleCode(id1),arg,0,
                    *VertexC(k,ref,-h1,ParticleCode(id1),arg,
                             0,0,helicity[0],idcode[0],
                             1,1,helicity[1],idcode[1])/k.m2(k41);
#endif
                  cout << "J1b term1: " << J1x << endl;
                  J1b += J1x;
                  int kg1 = k.Sum(photon,arg[1]);
                  J1x = k.spa(photon,ref)*k.spb(ref,arg[1])
                    *VertexC(k,ref,-h1,ParticleCode(id1),arg1,
                             0,0,helicity[0],idcode[0],
                             1,1,helicity[1],idcode[1],empty,
                             MakeVector(photon))/k.m2(kg1);
                  cout << "J1b term2: " << J1x << endl;
                  J1b += J1x;
                }
                verbose = true;
                cout << "J1o" << endl;
                J1o = Jc(k,ref,NParticleID(-h1,id1),arg,leg,start,midL,flow,
                         vector1,polarization1,couple1,
                         internal1);
                cout << "end J1o" << endl;
                verbose = false;
                //                cout << k.mom(photon) << endl;
                cout << "J1 [";   DumpHelicity(-h1);
                cout << "]: " << J1 << " vs " << J1alt << " vs " << J1b
                     << " orig: " << J1o
                     << endl;
                }
              if (!vector1.empty() and false)
                 {complex<T> direct = (T(2)*k.spb(ref,2)*k.spaa(1,polarization1[0],sum1,3)/
                                       (k.spb(ref,3)*props));
                   cout << "1 direct: "<< direct << " " << direct/term << endl;
                   int sum1f = FlatSum(k,ref,arg,start,midL,vector1);
                   complex<T> directF = (T(2)*k.spb(ref,2)*k.spaa(1,polarization1[0],sum1f,3)/
                                         (k.spb(ref,3)*props));
                   cout << "1 direct f: " << directF << "  " << directF/term << endl;
                   complex<T> directQ = (T(2)*k.spb(ref,2)*k.spaa(1,polarization1[0],ref,3)/
                                         (k.spb(ref,3)*k.s(ref,sum1f)));
                   cout << "1 direct Q: " << directQ << "  " << directQ/term << endl;
                   complex<T> v = VffV(k,ref,-h1,arg,0,0,-1,vector1[0],polarization1[0]);
                   //                   complex<T> valt = T(2)*k.spa(1,polarization1[0])*k.spb(sum1f,polarization1[0]);
                   complex<T> valt = T(2)*k.spa(1,polarization1[0])*k.spb(ref,polarization1[0])/k.spa(ref,sum1f);
                   cout << "1 v(" << vector1[0] << "," << polarization1[0] << "): " << v << " " << valt << " " << valt/v << endl;
                   complex<T> v3 = VffgC(k,ref,-1,arg,0,0,h1, 1,1,1, vector1, empty, 1);
                   cout << "1 v3: " << v3 << endl;
                   //                   cout << "k3: " << k.mom(3) << endl;
#if 0
                   complex<T> v3alt = -complex<T>(0,1)*(k.spb(ref,2)*k.spa(sum1f,3)/
                                       k.spb(ref,3));
#endif
                   complex<T> v3alt = -complex<T>(0,1)*(k.spb(ref,2)*k.spa(ref,3)/
                                                        (k.spb(ref,3)*k.spb(ref,sum1f)) );
                   cout << "1 v3 alt: " << v3alt << " " << (v3alt/v3) << endl;
                   complex<T> alt = (v*v3/props);
                   cout << "1 term alt: " << alt << " " << alt/term << endl;
                   complex<T> alt2 = (valt*v3alt/props);
                   cout << "1 term alt2: " << alt2 << " " << alt2/term << endl;
                 }
              if (!vector2.empty() and false)
                 {complex<T> direct = (T(2)*complex<T>(0,-1)*k.spa(3,1)*k.spbb(ref,sum2,polarization2[0],2)/
                                       (k.spb(ref,3)*props));
                   cout << "2 direct: " << direct << endl;
                   //                   cout << "sum2: " << k.mom(sum2) << endl;
                   int sum2f = FlatSum(k,ref,arg,midR,end,vector2);
                   complex<T> directF = (T(2)*complex<T>(0,-1)*k.spa(3,1)*k.spbb(ref,sum2f,polarization2[0],2)/
                                       (k.spb(ref,3)*props));
                   cout << "2 direct f: " << directF << " " << directF/term << endl;
#if 0
                   complex<T> directQ = (T(2)*complex<T>(0,-1)*k.spa(ref,1)*k.spbb(3,ref,polarization2[0],2)/
                                         (k.spa(ref,3)*(k.s(ref,sum2)-k.m2(sum2))));
#endif
                   complex<T> directQ = (T(2)*complex<T>(0,-1)*k.spa(ref,1)*k.spbb(3,ref,polarization2[0],2)/
                                         (k.spa(ref,3)*(k.s(ref,sum2f))));
                   cout << "2 direct q: " << directQ << " " << directQ/term << endl;
                   complex<T> v = VffV(k,ref,-h2,arg,1,1,1,vector2[0],polarization2[0]);
                   complex<T> valt = T(sqrt(2)*2)*k.spab(ref,polarization2[0],2)/
                                            k.spb(ref,sum2f);
                   complex<T> valt2 = T(sqrt(2)*2)*k.spa(ref,polarization2[0])*k.spb(polarization2[0],2)/
                                            k.spb(ref,sum2f);
                   cout << "2 v(" << vector2[0] << "," << polarization2[0] << "): " << v<< " " << valt << " " << valt/v << " " << valt2 << endl;
                   complex<T> v3 = VffgC(k,ref,-1,arg,0,0,-1, 1,1,h2, empty, vector2, 1);
                   complex<T> v3alt = complex<T>(0,-1)/sqrt(T(2))* T(Sf6)
                     * k.spa(ref,1) * k.spb(sum2f,3)/(k.spb(ref,3)*k.spb(ref,sum2f));
                   cout << "2 v3: " << v3 << " " << vertex << " " << v3alt << endl;
                   cout << "props: " << props << endl;
                   complex<T> alt = (v*v3/props);
                   cout << "2 term alt: " << alt << " " << alt/term << " " << alt/directQ << endl;
                   complex<T> alt2 = (valt*v3alt/props);
                   cout << "2 term alt2: " << alt2 << " " << alt2/term << " " << alt2/directQ << endl;
                   cout << "2 flat: "
                        << (k.spa(ref,1)*k.spba(3,sum2,ref)*k.spbb(ref,sum2,polarization2[0],2)/
                            (k.spa(ref,3)*(k.s(ref,sum2)-k.m2(sum2))*props)) << endl;
                  cout << "2 q: "
                       << (k.spa(ref,1)*k.spbb(3,ref,polarization2[0],2)/
                           (k.spa(ref,3)*(k.s(ref,sum2)-k.m2(sum2))*props)) << endl;
                 }
            }
#endif
#if DebugSigns
            if (n is 4 or debug) {//cout << endl;
            cout << "term [";  DumpHelicities(helicityO,h1,h2);
            if (h1 is hL or h1 is hR or h2 is hL or h2 is hR)
               {cout << "|";
                 if (IsQuark(id1) or IsQuark(id2))
                 DumpVertex(idcodeO,helicityO,ParticleCode(id1),h1,ParticleCode(id2),h2);
                 cout << "<>";
                 if (j1 isnt 1)
                    {cout << "1"; DumpVertex(ParticleCode(id1),-h1,
                              ParticleCode(leg[start]),helicity[start],
                                             ParticleCode(leg[midL]),helicity[midL]);}
                 else {cout << "2"; DumpVertex(ParticleCode(id2),-h2,
                              ParticleCode(leg[midR]),helicity[midR],
                                               ParticleCode(leg[end]),helicity[end]);}
                 }
            cout << "; " << start << ".." << midL << ";" << midR << ".." << end;
            cout << "]: " << term << endl;
            if (h1 is hL or h1 is hR or h2 is hL or h2 is hR)
              if (j1 isnt 1)
                cout << "Vo: " << (complex<T>(0,1)*Vertex(k,ref,helicityO,idcodeO,arg,
               start,start,leg[start].helicity(),ParticleCode(leg[start]),
               midL,midL,leg[midL].helicity(),ParticleCode(leg[midL]),
                                            end,end,leg[end].helicity(),ParticleCode(leg[end]))) << endl;
              else cout << "Vo: " << (complex<T>(0,1)*Vertex(k,ref,helicityO,idcodeO,arg,
               start,start,leg[start].helicity(),ParticleCode(leg[start]),
               midR,midR,leg[midR].helicity(),ParticleCode(leg[midR]),
                                               end,end,leg[end].helicity(),ParticleCode(leg[end]))) << endl;
            if (j1 isnt 1 and (h1 is -1 or h1 is 1))
               {complex<T> J1a = J(k,ref,NParticleID(-h1,id1),arg,leg,start,midL,internal1,massValue);
                 if (not IsZero(J1-J1a)) cout << "J1 [ " << h1 << "]: " << J1 << " vs " << J1a << endl;}
            if (j1 isnt n-2 and (h2 is -1 or h2 is 1))
               {complex<T> J2a = J(k,ref,NParticleID(-h2,id2),arg,leg,midR,end,internal2,massValue);
                 if (not IsZero(J2-J2a)) cout << "J2 [" << h2 << "]: " << J2 << " vs " << J2a << endl;}
            if (false and j1 is n-2 and (h2 is -1 or h2 is 1))
              cout << "J2 [" << h2 << "]: " << J2 << endl;
            //            cout << "props: " << props << endl;
            }
#endif
#if 0
            if (n is 6) {
              cout << "j1: " << j1 << "; ref: " << ref << endl;
              cout << "J1: " << J1 << "; J2: " << J2 << "; ";
#if 0 // ***ss
            if (j1 is 2)
               {C J2e = -1.0/(k.spa(j1+1,j1+2)*(k.s(j1+2,j1+3)-k.m2(j1+3)))*
                  (k.m2(j1+3)*k.spb(j1+1,j1+2)
                   -k.spab(ref,j1+3,j1+2)*(k.s(j1+1,j1+2,j1+3)-k.m2(j1+3))/
                   k.spa(ref,j1+1));
               cout << "J2 e: " << J2e;}
            else if (j1 is 3)
               {C J2e = -1.0/(k.spa(ref,j1+1))*k.spab(ref,j1+2,j1+1);
               cout << "J2 e(" << (j1+1) << "," << (j1+2) << "): " << J2e;}
#else // s***s
            if (j1 is 4)
               {C J1e = -1.0/(k.spa(2,3)*k.spa(3,4)*(k.s(1,2)-k.m2(1)))*
                  (k.m2(1)*(k.spbb(4,2,1,2)+k.spbb(4,3,1,2))/
                   (k.s(1,2,3)-k.m2(1))
                   -(k.spab(ref,1,2)-k.m2(1)*k.spab(ref,3,2)/
                   (k.s(1,2,3)-k.m2(1)))
                   *(k.s(1,2,3,4)-k.m2(1))/k.spa(ref,4));
               cout << "J1 e(2,3,4): " << J1e;}
            else if (j1 is 3)
               {C J1e = -1.0/(k.spa(2,3)*(k.s(1,2)-k.m2(1)))*
                  (k.m2(1)*k.spb(3,2)
                   -k.spab(ref,1,2)*(k.s(1,2,3)-k.m2(1))/
                   k.spa(ref,3));
               cout << "J1 e(2,3): " << J1e;}
            else if (j1 is 2)
               {C J1e = -1.0/(k.spa(ref,2))*k.spab(ref,1,2);
               cout << "J1 e(" << (1) << "," << (2) << "): " << J1e;}
#endif
            cout << endl;
            cout << "term: " << term << endl;
            }
#endif
            result -= term;
            if (n is 4 and false) cout << "result i: " << result << endl;}}}

    //    cout << "P (3)" << key << ": " << result << endl;
        if (not tagging)
    k.put_value(key,result);
 }
 return(result);
} // Jc

//EXPLICIT INSTANTIATION

template class complex<R>
  Jc(momentum_configuration<R>& k,
     int ref0 /* index of reference momentum */,
     ParticleID offshell /* helicity, type etc */,
     const vector<int>& arg /* indices of arguments */,
     const vector<ParticleID>& leg,
     int start, int end /* indices into the vectors */,
     int flow,
     const vector<int>& vectorK /* momenta */,
     const vector<int>& polarization ,
     const vector<int>& coupleTo /* quark flavor */ ,
     int offshellMass ,
     const vector<int>& massValue );

template class complex<RHP>
  Jc(momentum_configuration<RHP>& k,
     int ref0 /* index of reference momentum */,
     ParticleID offshell /* helicity, type etc */,
     const vector<int>& arg /* indices of arguments */,
     const vector<ParticleID>& leg,
     int start, int end /* indices into the vectors */,
     int flow,
     const vector<int>& vectorK /* momenta */ ,
     const vector<int>& polarization ,
     const vector<int>& coupleTo /* quark flavor */ ,
     int offshellMass,
     const vector<int>& massValue );

}

}
