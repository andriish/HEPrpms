/* Tree2.cc */

/*  David A. Kosower,  May 21, 2008  */

/* Implementation of on-shell recursion relations for tree amplitudes
   using the BlackHat momentum library

*/

#include <vector>
#include <map>
#include "spinor.h"
#include "mom_conf.h"
#include "Tree.h"
#include "Tree_impl.h"
#include "util.h"
#include "zero.h"

#define Re(v) real(v)
#define Im(v) imag(v)

#define isnt !=
#define is ==

#include "particleid.h"

#define _DEBUG_1 0
#define _DEBUG_2 0
#define _DEBUG_3 0
#define _DEBUG_4 0
#define _DEBUG_5 0
#define _DEBUG_6 0
#define _DEBUG_7 0
#define _DEBUG_8 0
#define _DEBUG_9 0

namespace BH {
const C I(0,1);
const particle_ID GluonID(gluon,1,1,false);

#undef MomentumConfiguration
#define MomentumConfiguration momentum_configuration<R>

#define BaseIndex 0

#define GGG VType3(Gluon,Gluon,Gluon)
#define FFG VType3(Quark,Quark,Gluon)
#define FGF VType3(Quark,Gluon,Quark)
#define GFF VType3(Gluon,Quark,Quark)

// 6/17/08
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

// 8/4/08
#define PM HType2(1,-1)
#define MP HType2(-1,1)
#define PP HType2(1,1)
#define MM HType2(-1,-1)

// To resolve 0/0 we decree a denominator 0 if according to IsZero

static double S1 = 1, S2 = 1, S3 = 1, S4 = 1, S5 = 1, S6 = 1;
void SetGGGSigns(int s1, int s2, int s3, int s4, int s5, int s6)
{S1 = s1;  S2 = s2;  S3 = s3;  S4 = s4;  S5 = s5;  S6 = s6;
}

static double FS1 = 1, FS2 = 1, FS3 = 1, FS4 = 1, FSi = 1;
void SetFFGSigns(int s1, int s2, int s3, int s4, int si)
{FS1 = s1;  FS2 = s2;  FS3 = s3;  FS4 = s4;  FSi = si;
}

static double FS5 = 1, FS6 = 1, FS7 = 1, FS8 = 1, FSi2 = 1;
void SetSSGSigns(int s1, int s2, int s3, int s4, int si)
{FS5 = s1;  FS6 = s2;  FS7 = s3;  FS8 = s4;  FSi2 = si;
}

namespace Tree {

bool fullDebug;
void FullDebug(bool status)
{fullDebug = status;}

template<class T> inline complex<T> Tggg(momentum_configuration<T>& k,
                                         int k1, int h1,
                                         int k2, int h2,
                                         int k3, int h3)
{complex<T> result;
 complex<T> denom;
#if 0
 cout << hex << HelicityType(h1,h2,h3)  << dec << endl;
 cout << hex << MPP << dec << endl;
 cout << "[12]" << k.spb(k1,k2) << endl;
 cout << "<12>" << k.spa(k1,k2) << endl;
#endif
 // cout << S1 << " " << S2 << " " << S3 << " " << S4 << " " << S5 << " "
 //      << S6 << " " << endl;
 switch (HelicityType(h1,h2,h3)) {
 case MPP:
   denom = k.spb(k1,k2)*k.spb(k2,k3)*k.spb(k3,k1);
   if (IsZero(denom)) return(0);
   result = (-S1*I*pow(k.spb(k2,k3),4)/denom);
   break;
 case PMP:
   denom = k.spb(k1,k2)*k.spb(k2,k3)*k.spb(k3,k1);
   if (IsZero(denom)) return(0);
   result = (-S2*I*pow(k.spb(k3,k1),4)/denom);
   break;
 case PPM:
   denom = k.spb(k1,k2)*k.spb(k2,k3)*k.spb(k3,k1);
   if (IsZero(denom)) return(0);
   result = (-S3*I*pow(k.spb(k1,k2),4)/denom);
   break;
 case PMM:
   denom = k.spa(k1,k2)*k.spa(k2,k3)*k.spa(k3,k1);
   if (IsZero(denom)) return(0);
   result = (S4*I*pow(k.spa(k2,k3),4)/denom);
   break;
 case MPM:
   if (fullDebug)
      {cout << "k1(" << k1 << "): " << k.L(k1) << endl;
      cout << "k2: " << k.L(k2) << endl;
      cout << "<12>: " << k.spa(k1,k2) << endl;
      cout <<  "<23>: " << k.spa(k2,k3) << endl;
      cout <<  "<31>: " << k.spa(k3,k1) << endl;}
   denom = k.spa(k1,k2)*k.spa(k2,k3)*k.spa(k3,k1);
   if (IsZero(denom)) return(0);
   result = (S5*I*pow(k.spa(k3,k1),4)/denom);
   break;
 case MMP:
   denom = k.spa(k1,k2)*k.spa(k2,k3)*k.spa(k3,k1);
   if (IsZero(denom)) return(0);
   result = (S6*I*pow(k.spa(k1,k2),4)/denom);
   break;
 case PPP:
 case MMM:
   result = (0);
   break;
 }
 if (fullDebug) cout << "ggg: " << result << endl;
 return(result);
}

template<class T> inline complex<T> Tffg(momentum_configuration<T>& k,
                                         int k1, int h1,
                                         int k2, int h2,
                                         int k3, int h3)
{complex<T> denom;
// cout << hex << HelicityType(h1,h2,h3) << dec << endl;
 // Non-standard-looking phases, but they appear to give the right answers...
 switch (HelicityType(h1,h2,h3)) {
 case MPP:
#if _DEBUG_1
   cout << "[12]: " << k.spb(k1,k2) << endl;
   cout << "<12>: " << k.spa(k1,k2) << endl;
#endif
   denom = k.spb(k1,k2)*k.spb(k2,k3)*k.spb(k3,k1);
   if (IsZero(denom)) return(0);
   return(FS1*I*pow(k.spb(k2,k3),3)*k.spb(k1,k3)/denom);
 case PMP:
   denom = k.spb(k1,k2)*k.spb(k2,k3)*k.spb(k3,k1);
   if (IsZero(denom)) return(0);
   return(-FS2*I*pow(k.spb(k3,k1),3)*k.spb(k2,k3)/denom);
 case PMM:
   denom = k.spa(k1,k2)*k.spa(k2,k3)*k.spa(k3,k1);
   if (IsZero(denom)) return(0);
   return(FS3*I*pow(k.spa(k2,k3),3)*k.spa(k1,k3)/denom);
 case MPM:
   denom = k.spa(k1,k2)*k.spa(k2,k3)*k.spa(k3,k1);
   if (IsZero(denom)) return(0);
   return(-FS4*I*pow(k.spa(k3,k1),3)*k.spa(k2,k3)/denom);
 case PPM:
 case MMP:
 case PPP:
 case MMM:
   return(0);
 }}

template<class T> inline complex<T> Tssg(momentum_configuration<T>& k,
                                         int k1, int h1,
                                         int k2, int h2,
                                         int k3, int h3)
{complex<T> denom;
// cout << hex << HelicityType(h1,h2,h3) << dec << endl;
// Maintain the convention, even in the massive case, that the scalars
// should be of opposity helicity.  In the massive case, the scalar
// momenta should be flattened
 switch (HelicityType(h1,h2,h3)) {
 case MPP:
#if _DEBUG_1
   cout << "[12]: " << k.spb(k1,k2) << endl;
   cout << "<12>: " << k.spa(k1,k2) << endl;
#endif
   denom = k.spb(k1,k2);
   if (IsZero(denom)) return(0);
   return(-FS5*I*k.spb(k2,k3)*k.spb(k3,k1)/denom);
 case PMP:
   denom = k.spb(k1,k2);
   if (IsZero(denom)) return(0);
   return(-FS6*I*k.spb(k3,k1)*k.spb(k2,k3)/denom);
 case PMM:
   denom = k.spa(k1,k2);
   if (IsZero(denom)) return(0);
   return(FS7*I*k.spa(k2,k3)*k.spa(k3,k1)/denom);
 case MPM:
   denom = k.spa(k1,k2);
   if (IsZero(denom)) return(0);
   return(FS8*I*k.spa(k3,k1)*k.spa(k2,k3)/denom);
 case PPM:
 case MMP:
 case PPP:
 case MMM:
   return(0);
 }}

template<class T> inline complex<T> TffV(momentum_configuration<T>& k,
                                         int k1, int h1,
                                         int k2, int h2,
                                         int v, int pol)
{
#if 0
cout << "1: " << k.mom(k1) << " " << k.m2(k1) << endl;
  cout << "2: " << k.mom(k2) << endl;
 cout << "e: " << k.mom(pol) <<  " " << k.m2(pol) << endl;
 cout << h1 << " " << h2 << " " << hex << HelicityType(h1,h2) << " ";
 cout << MP << dec << endl;
 cout << "pol a: " << k.L(pol) << endl;
 cout << "pol b: " << k.Lt(pol) << endl;
#endif
switch (HelicityType(h1,h2)) {
  case PM:
    return(T(-2)*I*k.spa(k2,pol)*k.spb(pol,k1));
  case MP:
    return(T(-2)*I*k.spa(k1,pol)*k.spb(pol,k2));
  case PP:
  case MM:
    return(0);
  }
}

template<class T> static complex<T> ThreePoint(momentum_configuration<T>& k,
                                               int k1, int h1, int id1,
                                               int k2, int h2, int id2,
                                               int k3, int h3, int id3)
{switch (VertexType(id1,id2,id3)) {
 case GGG:
   return(Tggg(k,k1,h1, k2,h2, k3,h3));
 case FFG:
   return(Tffg(k,k1,h1, k2,h2, k3,h3));
 case FGF:
   return(Tffg(k,k3,h3, k1,h1, k2,h2));
 case GFF:
   return(Tffg(k,k2,h2, k3,h3, k1,h1));
 case SSG:
   return(Tssg(k,k1,h1, k2,h2, k3,h3));
 case SGS:
   return(Tssg(k,k3,h3, k1,h1, k2,h2));
 case GSS:
   return(Tssg(k,k2,h2, k3,h3, k1,h1));
 default:
   throw "Illegal vertex type [ThreePoint]";
}
}

template<class T> static complex<T> ThreePointV(momentum_configuration<T>& k,
                                               int k1, int h1, int id1,
                                               int k2, int h2, int id2,
                                               int v, int pol)
{switch (VertexType(id1,id2)) {
  case FF:
    return(TffV(k,k1,h1,k2,h2,v,pol));
  default:
   throw "Illegal vertex type [ThreePointV]";
  }
}


void PrintVector(vector<bool> v);

void PrintVector(const vector<int>& v)
{cout << "{"; for (int i = BaseIndex;  i < v.size();  i += 1)
   {cout << v[i]; if (i < v.size()-1) cout << " ";}
 cout << "}";}

void PrintArguments(const vector<int>& args, const vector<particle_ID>& id)
{bool prime = false;
  cout << "{";
  for (int j = BaseIndex; j < args.size();  j += 1)
     {cout << args[j];
       cout << (id[j].helicity() > 0 ? "+" : "-");
    if (IsQuark(id[j]))
       {cout << "q"; if (prime) cout << FlavorOf(id[j]);}
    else if (IsScalar(id[j]))
       {cout << "s"; if (prime) cout << FlavorOf(id[j]);}
    if (j < args.size()-1) cout << " ";}
  cout << "}";
}

bool ScalarImbalance(const vector<ParticleID>& leg,
                     int start, int end,
                     int scalarFlavor /* to resolve ambiguous cases */,
                     int& sImbalance);

/* Decide whether a partition requires a gluon, fermion, or scalar
   for the intermediate leg (return value: true), or whether it's not
   legitimate (for example, if it's unbalanced in two different fermion
   flavors) */
inline bool Classify(const vector<ParticleID>& leg /* of one amplitude */,
                     ParticleID& id /* result for internal leg */,
                     int scalarFlavor /* hint for ambiguous cases: 0 means gluon */)
{int imbalance = 0;
 vector<bool> flavorParity = FermionParity(leg,0,leg.size()-1);
#if _DEBUG_8
 cout << "Classify: ";
 for (int j = 0;  j < leg.size();  j += 1)
    {cout << ParticleCode(leg[j]) << " ";}
 PrintVector(flavorParity);
 cout << endl;
#endif

 for (int f = 0;  f < flavorParity.size();  f += 1)
   if (flavorParity[f])
     if (imbalance) return(false);
     else imbalance = f;  // flavor-to-be of internal leg

 // Overall arguments have already been checked...
 if (imbalance) id = FlavoredQuarkID(imbalance);
 else {// Look at scalar imbalance
   int sImbalance;
   if (not ScalarImbalance(leg,0,leg.size()-1,scalarFlavor,sImbalance))
     return false;
   id = sImbalance ? FlavoredScalarID(sImbalance) : GluonID;
   if (leg.size() is 1 /* Arises with external electroweak vector */
       and sImbalance is 0) return false;
 }
 return true;
}

template<class T> /* static */ int Flat(momentum_configuration<T>& k,
                         int ref /* index of reference momentum */,int j)
{// cout << "In Flat " << start << ", " << end << endl;
 /* For massless complex momenta, we must be careful to preserve the
    separate lambda and lambda-tilde spinors, which will in general not
    be identical to those produced by the "insert" below, even if the
    momentum is the same... */
 if (IsZero(k.m2(j))) return(j);
 string key = GenKey("fl",ref,j);
 size_t index;
 if (not k.get_label(key,index)) {
    Cmom<T> flat = k[j] - (k.m2(j)/(T(2)*(k[j]*k[ref]))) * k[ref];
    index = k.insert(flat);
    k.put_label(key,index);}
 return index;
}

#define RefTag "ref"

C KnownGluonMPPM(momentum_configuration<R>& k,int ref0,
                 int i1, int i2, int i3, int i4);


vector<int> Join(const vector<int>& v1, const vector<int>& v2,
                 const vector<int>& v3, const vector<int>& v4);


/* Perform the sum over intermediate helicities in each term in the on-shell
   recursion relation */
template<class T> complex<T>
  HelicitySum(momentum_configuration<T>& k,
              const vector<int>& arg,
              const vector<particle_ID>& leg,
              int part1, int part2, int shiftVector,
              bool part1first /* Which sub-amplitude to do first */,
              int shift1, int shift2 /* Shifted legs */,
              vector<int>& partition1, vector<int>& partition2,
                 /* momentum indices in each sub-amplitude */
              vector<ParticleID>& id1, vector<ParticleID>& id2,
              const ParticleID& internalID,
              /* vector momenta, polarization vectors, and fermion flavors
                 for vectors attached to partition1 & 2 respectively: */
              const vector<int>& vector1, const vector<int>& polarization1,
              const vector<int>& couple1,
              const vector<int>& vector2, const vector<int>& polarization2,
              const vector<int>& couple2,
              const vector<int>& massValue,
              vector<int>& massValue1, vector<int>& massValue2)
{complex<T> result = complex<T>(0,0);

#if 0
  vector<ParticleID>& id1; vector<ParticleID>& id2;
  const ParticleID& internalID;
              const vector<int>& vector1; const vector<int>& polarization1;
              const vector<int>& couple1;
              const vector<int>& vector2; const vector<int>& polarization2;
              const vector<int>& couple2;
              const vector<int>& massValue;
              vector<int>& massValue1; vector<int>& massValue2;
#endif

  // Pole momentum
  int pole = k.Sum(k.Sum(partition1),arg[part1],k.Sum(vector1));
#if _DEBUG_4
  if (arg.size() is 3)
    cout << "pole index: " << pole << ", k^2: " << k.m2(pole) << endl;
#endif

  // Compute pole location in shift parameter; j is in partition1
  complex<T> z;
  int lone = part1first ? shift1 : shift2;
  if (IsScalar(internalID) and massValue.size() > 0
      and massValue[lone] > 0)
    z = (k.m2(pole)-k.m2(massValue[lone]))/
#if 0 // Without vectors, it was
      k.spab(shiftVector,partition1,shiftVector);
  else z = k.m2(pole)/k.spab(shiftVector,partition1,shiftVector);
#else
      k.spab(shiftVector,pole,shiftVector);
  else z = k.m2(pole)/k.spab(shiftVector,pole,shiftVector);
#endif
  //k.spab(arg[shift1],partition1,arg[shift2]);

  //  cout << "z: " << z << endl;
  // Shifted momenta
  int shifted1, shifted2;
  if (massValue.size() > 0 and IsScalar(leg[shift1])
      and massValue[shift1] > 0)
    shifted1 = k.insert(k[arg[shift1]]-z*k[shiftVector]);
  else shifted1 = k.insert(k.L(arg[shift1]),
                           k.Lt(arg[shift1])-z*k.Lt(shiftVector));
  if (massValue.size() > 0 and IsScalar(leg[shift2])
      and massValue[shift2] > 0)
    shifted2 = k.insert(k[arg[shift2]]+z*k[shiftVector]);
  else shifted2 = k.insert(k.Lt(arg[shift2]),
                           k.L(arg[shift2])+z*k.L(shiftVector));

  if (fullDebug)
     {cout << "<z: " << k.L(arg[shift2])+z*k.L(arg[shift1]) << endl;}

  // Note that adding these at the end relies on the fact that
  // they are at the edge of the partitions, which in turn relies on
  // the fact that the two shifted legs are neighbors; the order here
  // is needed to keep the right cyclic ordering
  int internal1, internal2;
      if (part1first)
         {partition1.push_back(shifted1);
           if (massValue1.size() > 0) massValue1.push_back(shift1);
         // leg connecting two amplitudes
           internal2 = k.Sum(partition1,vector1);
         internal1 = k.insert(-k[internal2]);
         partition1.push_back(internal1);
         if (massValue1.size() > 0) massValue1.push_back(lone);
         partition2.push_back(internal2);
         if (massValue2.size() > 0) massValue2.push_back(lone);
         partition2.push_back(shifted2);
         if (massValue2.size() > 0) massValue2.push_back(shift2);}
      else {partition2.push_back(shifted2);
         // leg connecting two amplitudes
        internal1 = k.Sum(partition2,vector2);
         internal2 = k.insert(-k[internal1]);
         partition2.push_back(internal2);
         if (massValue2.size() > 0) massValue2.push_back(lone);
         partition1.push_back(internal1);
         if (massValue1.size() > 0) massValue1.push_back(lone);
         partition1.push_back(shifted1);
         if (massValue1.size() > 0) massValue1.push_back(shift1);}
      //      cout << "IID: " << ParticleCode(internalID) << endl;

      // Sum over both helicities
      if (part1first)
         {id1.push_back(NParticleID(1,internalID));
         id2.push_back(NParticleID(-1,internalID));
         id2.push_back(leg[shift2]);}
      else {id2.push_back(NParticleID(1,internalID));
         id1.push_back(NParticleID(-1,internalID));
         id1.push_back(leg[shift1]);}

#if 0
      cout << "Internal (HS): " << ParticleCode(internalID) << endl;
      cout << "HS sizes [" << id1.size() << " " << id2.size() << "]" << endl;
      if (id1.size() is 2) cout << "id1b: " << ParticleCode(id1[0]) << " "
                                << ParticleCode(id1[1]) << endl;
      else if (id2.size() is 2) cout << "id2b: " << ParticleCode(id2[0]) << " "
                                << ParticleCode(id2[1]) << endl;
#endif
#if _DEBUG_4
      if (arg.size() is 4) {
      cout << "partition1 complete: ";  PrintVector(partition1); cout << endl;
      cout << "partition2 complete: ";  PrintVector(partition2); cout << endl;
      }
#endif
      complex<T> fac1, fac2;
      complex<T> term;
      complex<T> denom;
      denom = k.m2(pole);

#if 0
         if (arg.size() is 4)
            {
      cout << "partition1 w/internal: ";
      PrintArguments(partition1,id1);
      if (vector1.size() > 0) cout << "+" << vector1.size();
      cout << endl;
      cout << "partition2 w/internal: ";
      PrintArguments(partition2,id2);
      if (vector2.size() > 0) cout << "+" << vector2.size();
      cout << endl;
            }
#endif
      if (IsScalar(internalID) and massValue.size() > 0 and massValue[lone] > 0)
        denom -= k.m2(massValue[lone]);
      if (partition1.size() <= partition2.size())
         {fac1 = term = Aosrr(k,partition1,id1,
                              vector1,polarization1,couple1,massValue1);
         if (not IsZero(term)) term *= -I*(fac2=Aosrr(k,partition2,id2,
                                                      vector2,polarization2,couple2,massValue2))/denom;
#if 0
         if (arg.size() is 4)
         cout << "fac1: " << fac1 << "; " << "fac2: " << fac2 << endl;
#endif
}
      else
         {fac2 = term = Aosrr(k,partition2,id2,
                              vector2,polarization2,couple2,massValue2);
         if (not IsZero(term)) term *= -I*(fac1=Aosrr(k,partition1,id1,
                              vector1,polarization1,couple1,massValue1))/denom;
#if 0
         if (arg.size() is 4)
         cout << "fac1: " << fac1 << "; " << "fac2: " << fac2 << endl;
#endif
}
      if (IsQuark(internalID)) term *= FSi;
      if (IsScalar(internalID)) term *= FSi2;

      if (fullDebug) cout << arg.size() << ": " << term << " " << endl;

#if 0
      if (arg.size() is 4)
         {
           complex<T> val2 = complex<T>(0,-1)*pow(k.spb(shifted2,3),2)/k.spb(internal2,3);
           cout << "comp A: " << val2 << endl;
         }
#endif

      result += term;
#if 0
         if (arg.size() is 4)
      cout << "term 1: " << term << endl;
#endif

      //      cout << "r1: " << result << endl;
      // Undo 1st helicity, use 2nd
      if (part1first)
         {id1.pop_back();  id2.pop_back();  id2.pop_back();
         id1.push_back(NParticleID(-1,internalID));
         id2.push_back(NParticleID(1,internalID));
         id2.push_back(leg[shift2]);}
      else {id2.pop_back();  id1.pop_back();  id1.pop_back();
         id2.push_back(NParticleID(-1,internalID));
         id1.push_back(NParticleID(1,internalID));
         id1.push_back(leg[shift1]);}

#if 0
         if (arg.size() is 4)
            {      cout << "partition1 w/internal: ";
      PrintArguments(partition1,id1);
      if (vector1.size() > 0) cout << "+" << vector1.size();
      cout << endl;
      cout << "partition2 w/internal: ";
      PrintArguments(partition2,id2);
      if (vector2.size() > 0) cout << "+" << vector2.size();
      cout << endl;
            }
#endif
      if (partition1.size() < partition2.size())
         {term = Aosrr(k,partition1,id1,
                       vector1,polarization1,couple1,massValue1);
         fac1 = term;
#if 0
         cout << "fac1: " << fac1 << " ";
         PrintVector(partition1);
         cout << endl;
#endif
         if (not IsZero(term)) term *= -I*(fac2=Aosrr(k,partition2,id2,
                              vector2,polarization2,couple2,massValue2))/denom;
#if 0
         if (arg.size() is 4)
         cout << "fac1: " << fac1 << "; " << "fac2: " << fac2 << endl;
#endif
}
      else
         {term = Aosrr(k,partition2,id2,
                              vector2,polarization2,couple2,massValue2);
         fac2 = term;
#if 0
         cout << "fac2: " << fac2 << endl;
#endif
         if (not IsZero(term)) term *= -I*(fac1=Aosrr(k,partition1,id1,
                       vector1,polarization1,couple1,massValue1))/denom;
#if 0
         if (arg.size() is 4)
         cout << "fac1: " << fac1 << "; " << "fac2: " << fac2 << endl;
#endif
}
      if (IsQuark(internalID)) term *= FSi;
      if (IsScalar(internalID)) term *= FSi2;

      result += term;
#if 0
         if (arg.size() is 4)
      cout << "term 2: " << term << endl;
#endif
#if 0
      if (polarization1.size() > 0)
      {int pol = polarization1[0];
        cout << "ind: " << pol << " " << shifted1 << " " << shifted2 << endl;
        complex<T> val1 = -2.*complex<T>(0,1)*k.spa(shifted1,pol)*k.spb(pol,internal1);
        int neg = (leg[1].helicity() < 0 ? 2 : 1);
        int pos = (neg is 1 ? 2 : 1);
        complex<T> val2 = complex<T>(0,-1)*pow(k.spb(pos,shifted2),2)/k.spb(internal2,pos);
        cout << "comp A: " << val1 << " " << val2 << " " << denom
             << "  " << (val1*val2/denom) << endl;
        val1 = -2.*complex<T>(0,1)*k.spa(neg,pol)*k.spb(pol,internal1);
        val2 = complex<T>(0,-1)*pow(k.spb(pos,3),2)/k.spb(internal2,pos);
        cout << "comp B: " << val1 << " " << val2 << " " << denom
             << "  " << (val1*val2/denom) << endl;
        cout << "<neg int1>: " << k.spa(neg,internal1)
             << "; [neg int1]: " << k.spb(neg,internal1) << endl;
        val1 = -2.*complex<T>(0,1)*k.spa(neg,pol)*k.spab(neg,pole,pol)/
          k.spa(internal1,neg);
        val2 = complex<T>(0,-1)*pow(k.spb(pos,3),2)*k.spa(internal2,neg)/
          k.spab(neg,pole,pos);
        cout << "comp C: " << val1 << " " << val2 << " " << denom
             << "  " << (val1*val2/denom) << endl;
      }
#endif
      // In case there are additional terms summed (e.g. vectors coupling
      // on both sides), remove earlier appends
      if (part1first)
         {partition1.pop_back();
          if (massValue1.size() > 0) massValue1.pop_back();
          // leg connecting two amplitudes
         partition1.pop_back();
         if (massValue1.size() > 0) massValue1.pop_back();
         partition2.pop_back();
         if (massValue2.size() > 0) massValue2.pop_back();
         partition2.pop_back();
         if (massValue2.size() > 0) massValue2.pop_back();
         id1.pop_back();  id2.pop_back();  id2.pop_back();}
      else {partition2.pop_back();
         // leg connecting two amplitudes
         partition2.pop_back();
         if (massValue2.size() > 0) massValue2.pop_back();
         partition1.pop_back();
         if (massValue1.size() > 0) massValue1.pop_back();
         partition1.pop_back();
         if (massValue1.size() > 0) massValue1.pop_back();
         id2.pop_back();  id1.pop_back();  id1.pop_back();}

return result;
}

// Are the requested fermion flavors present for all vectors?
bool FermionFlavorsOK(const vector<particle_ID>& leg,
                      const vector<int>& coupleTo)
{vector<int> count = FermionCount(leg,BaseIndex,leg.size()-1);
  for (int v = 0;  v < coupleTo.size();  v += 1)
    if (count[coupleTo[v]] is 0) return(false);
  return true;
}

/* 8/26/08: Is the given index a legal place to split the arguments into
            two amplitudes?  We need at least two external colored
            legs in each partition, unless there's a vector that might attach,
            in which case we can allow a lone leg.
            We assume that the two shifted legs are always cyclicly adjacent.
            This means there are six configurations we have to consider for
            partition1 (partition2 is the complement)
      ...part1 part2...j... from j to size & BaseIndex up to & including part1
                                      (j > part2)
      ...part2 part1...j... from part1 up to & including j (j >= part1)
      ...j...part1 part2... from j up to & including part1 (j <= part1)
      ...j...part2 part1... from part1 up to size and BaseIndex up to
                               & including j (j < part2)
      part1...j...part2     from part1 up to & including j (part1 <= j < part2)
      part2...j...part1     from j up to & including part1 (part1 >= j > part2)
            (part1 = arg.size(), part2 = BaseIndex);
*/
inline bool AllowedSplit(int argSize, int part1, int part2, int j,
                         int vectorSize,
                         const ParticleID& id1 /* at part1 */,
                         const ParticleID& id2 /* at part2 */)
{if (j is part2) return false; // would leave partition2 empty
 // partition1 can contain one external leg only if a vector can attach
  //  cout << "AS: vs = " << vectorSize << ", g1? " << IsGluon(id1) << endl;
 if (j is part1) return (vectorSize isnt 0 and not IsGluon(id1));
 // partition2 can contain one external leg only if a vector can attach
 if (part2 is BaseIndex and part1 is argSize-1 and j is BaseIndex+1
     /* j is then < part1 */)
    return (vectorSize isnt 0 and not IsGluon(id2));
 if (part2 is BaseIndex and part1 < argSize-1 and j is argSize-1)
    return (vectorSize isnt 0 and not IsGluon(id2));
 if (j > part1 and j is part2+1)
    return (vectorSize isnt 0 and not IsGluon(id2));
 if (j < part1 and j is part2-1)
    return (vectorSize isnt 0 and not IsGluon(id2));
 if (j is BaseIndex and j < part1 and part2 is argSize-1)
    return (vectorSize isnt 0 and not IsGluon(id2));
 if (part1 is BaseIndex and part2 is argSize-1 and j is part2-1)
    return (vectorSize isnt 0 and not IsGluon(id2));
#if 0
  cout << "AS: " << part1 << " " << part2 << " " << argSize << " " << j
      << " " << IsGluon(id1) << " " << IsGluon(id2) << endl;
#endif
 return true;
}

/*
   Arguments k, arg, leg, same as for J() in tree1.cc.

   8/4/08: For masses, an additional argument, giving the indices of
   four-vectors for each entry (ignored for gluons) whose square is the
   mass of the corresponding leg.  Each distinct index tracks a "mass
   line" (which may implicitly be the D-dimensional component of a
   D-vector), which can flow from scalar to scalar, fermion to fermion,
   or scalar to fermion (or vice versa).  Each index must appear exactly
   twice.

   8/4/08 For lepton pairs, the vector polarization should be the lepton
   current; only include direct coupling of the vectors to the specified
   quark flavors.  For the moment, support only a single vector
*/
template <> BH::momentum<std::complex<double> > BH::Tree::GenerateMomentum<double>(  double const &  dummy) { return GenerateMomentum<double>(dummy); }
template <> BH::momentum<std::complex<dd_real> > BH::Tree::GenerateMomentum<dd_real>( dd_real const &  dummy) { return GenerateMomentum<dd_real>(dummy); }


template<class T> complex<T>
  Aosrr(momentum_configuration<T>& k,
        const vector<int>& arg /* indices of arguments */,
        const vector<particle_ID>& leg /* helicities and particle ids */,
        // Vector bosons
        const vector<int>& vectorK /* momenta */ = empty,
        const vector<int>& polarization = empty,
        const vector<int>& coupleTo /* quark flavor */ = empty,
        const vector<int>& massValue = empty)
{ // until the right routine is available:
 vector<int> helicity = Helicities(leg);
 vector<int> idcode = ParticleCode(leg);
 // Temporary hack until GenKey takes more args...
 string key = GenKey("Ao",0,0,arg,helicity,idcode,
                     Join(vectorK,polarization,coupleTo,massValue));

 const string refKey(RefTag);
 size_t ref;
 if (massValue.size() > 0 and not k.get_label(refKey,ref)) {
   R dummy;
   // inserting momentum<R> doesn't seem to yield correct lambdas
   momentum<complex<R> > refMom = GenerateMomentum(dummy);
   ref = k.insert(refMom);
   k.put_label(refKey,ref);
 }

#if _DEBUG_2
 PrintVector(arg);
 cout << " | ";
 PrintVector(helicity);
 cout << " | ";
 PrintVector(idcode);
 cout << endl;
#endif

 C result;

 if (not k.get_value(key,result)) {

#if 0
   cout << "Aosrr.s: " << arg.size() << " ";
   PrintVector(arg);  cout << endl;
#endif
   // Three-point amplitude? Terminate recursion
   if (arg.size() is 3 and vectorK.size() is 0)
      {int h1 = leg[0].helicity(), h2 = leg[1].helicity(),
         h3 = leg[2].helicity();
      int id1 = ParticleCode(leg[0]), id2 = ParticleCode(leg[1]),
        id3 = ParticleCode(leg[2]);
      // For massive case, need to flatten scalar momenta
      if (massValue.size() > 0)
         {int flat1, flat2, flat3;
           flat1 = (IsScalar(id1) and massValue[0] >= 0) ?
             Flat(k,ref,arg[0]) : arg[0];
           flat2 = (IsScalar(id2) and massValue[1] >= 0) ?
             Flat(k,ref,arg[1]) : arg[1];
           flat3 = (IsScalar(id3) and massValue[2] >= 0) ?
             Flat(k,ref,arg[2]) : arg[2];
         result = ThreePoint(k,flat1,h1,id1, flat2,h2,id2, flat3,h3,id3);}
      else result = ThreePoint(k,arg[0],h1,id1, arg[1],h2,id2, arg[2],h3,id3);
      if (fullDebug) cout << "3(" << arg[0] << ","
                          <<arg[1] << "," << arg[2] << "): " << result << endl;
#if 0
      if (fullDebug) cout << h1 << " " << h2 << " " << h3 << endl;
      if (fullDebug) cout << k[arg[0]] << endl;
      if (fullDebug) cout << k[arg[1]] << endl;
      if (fullDebug) cout << k[arg[2]] << endl;
#endif
#if _DEBUG_1
      cout << "k1: " << k[arg[0]] << endl;
      cout << "k2: " << k[arg[1]] << endl;
      cout << "k3: " << k[arg[2]] << endl;
#endif
#if _DEBUG_3
      cout << "TP: " << result << endl;
#endif
      k.put_value(key,result);
      return result;
      }
   else if (arg.size() is 2 and vectorK.size() is 1)
      {int h1 = leg[0].helicity(), h2 = leg[1].helicity();
       int id1 = ParticleCode(leg[0]), id2 = ParticleCode(leg[1]);
#if 0
       cout << arg[0] << " " << h1 << " " << id1 << endl;
       cout << arg[1] << " " << h2 << " " << id2 << endl;
       cout << coupleTo[0] << endl;
       cout << "k1: " << k.mom(arg[0]) << endl;
       cout << "k2: " << k.mom(arg[1]) << endl;
       cout << "v: " << k.mom(vectorK[0]) << endl;
       cout << "e: " << k.mom(polarization[0]) << endl;
#endif
       result = (IsQuark(leg[0]) and FlavorOf(leg[0]) is coupleTo[0]) ?
         ThreePointV(k,arg[0],h1,id1, arg[1],h2,id2,
                     vectorK[0], polarization[0]) : complex<T>(0,0);
#if 0
       cout << "result: " << result << endl;
#endif
      k.put_value(key,result);
      return result;
      }
   else if (arg.size() is 2 and vectorK.size() > 1)
     throw "Not supported yet";

   // Are the requested fermion flavors present?
   if (vectorK.size() > 0 and !FermionFlavorsOK(leg,coupleTo))
      {result = complex<T>(0,0);
      k.put_value(key,result);
      return result;
      }

   //   cout << arg.size() << "; " << FermionFlavorsOK(leg,coupleTo) << endl;

   // Find neighboring - & + helicities that are not attached to the same
   // fermion line (each fermion line assumed to have a distinct flavor here)
   bool found = false;
   int shift1, shift2;
   int part1, part2;
   {for (int j = BaseIndex;  j < arg.size();  j += 1)
      {int next = (j is arg.size()-1 ? BaseIndex : j+1);
      // scalars behave helicity-independently... so they always behave
      // as though they have the same helicity as the gluon/quark
      if (IsScalar(leg[j]) or IsScalar(leg[next])) continue;
      if (helicity[j] < 0 and helicity[next] > 0
          and (!IsQuark(leg[j]) or !IsQuark(leg[next])
               or FlavorOf(leg[j]) isnt FlavorOf(leg[next])))
         {found = true; shift1 = part1 = j;  shift2 = part2 = next;  break;}
      else if (helicity[j] > 0 and helicity[next] < 0
               and (!IsQuark(leg[j]) or !IsQuark(leg[next])
                    or FlavorOf(leg[j]) isnt FlavorOf(leg[next])))
         {found = true; shift1 = part1 = next;  shift2 = part2 = j;  break;}}}


   // Otherwise, look for like-helicity legs
   if (not found)
     for (int j = BaseIndex;  j < arg.size();  j += 1)
        {int next = (j is arg.size()-1 ? BaseIndex : j+1);
        if (((helicity[j] < 0 and helicity[next] < 0)
             or (helicity[j] > 0 and helicity[next] > 0))
            and IsGluon(leg[j]) and IsGluon(leg[next]))
           {found = true; shift1 = part1 = j;  shift2 = part2 = next;  break;}
#if 1 // For gq, be careful about which order is allowed
        else if ((helicity[j] > 0 and helicity[next] > 0
                 and IsGluon(leg[next]) and IsQuark(leg[j]))
                 or (helicity[j] < 0 and helicity[next] < 0
                     and IsGluon(leg[j]) and IsQuark(leg[next])))
           {found = true; shift1 = part1 = j;  shift2 = part2 = next;  break;}
        else if ((helicity[j] > 0 and helicity[next] > 0
                 and IsGluon(leg[j]) and IsQuark(leg[next]))
                 or (helicity[j] < 0 and helicity[next] < 0
                     and IsGluon(leg[next]) and IsQuark(leg[j])))
           {found = true; shift1 = part1 = next;  shift2 = part2 = j;  break;}
#endif
#if 1 // For qq, to be checked!
        else if ((helicity[j] > 0 and helicity[next] > 0
                 and IsQuark(leg[next]) and IsQuark(leg[j]))
                 or (helicity[j] < 0 and helicity[next] < 0
                     and IsQuark(leg[j]) and IsQuark(leg[next])))
           {found = true; shift1 = part1 = j;  shift2 = part2 = next;  break;}
#endif
        else if ((helicity[j] < 0 and IsGluon(leg[j]) and IsScalar(leg[next]))
                  or (helicity[next] > 0 and IsGluon(leg[next])
                      and IsScalar(leg[j])))
           {found = true;  shift1 = part1 = j;  shift2 = part2 = next;  break;}
        else if ((helicity[j] > 0 and IsGluon(leg[j]) and IsScalar(leg[next]))
                  or (helicity[next] < 0 and IsGluon(leg[next])
                      and IsScalar(leg[j])))
           {found = true;  shift1 = part1 = next;  shift2 = part2 = j;  break;}
        }

   if (not found)
     throw "Shift pair not found in Aosrr!";

#if _DEBUG_6
   if (arg.size() is 3)
      {cout << "p1, p2: " << part1 << " " << part2 << " ";
      cout << "[";
      if (IsQuark(leg[part1])) cout << "q";
      else if (IsScalar(leg[part1])) cout << "s";
      cout << (leg[part1].helicity() > 0 ? "+" : "-");
      cout << ",";
      if (IsQuark(leg[part2])) cout << "q";
      else if (IsScalar(leg[part2])) cout << "s";
      cout << (leg[part2].helicity() > 0 ? "+" : "-");
      cout << ">" << endl;}
#endif
   result = complex<T>(0,0);
   // Sum over all partitions of legs into sets where shift1 is in one set,
 // shift2 in the other
 for (int j = BaseIndex;  j < arg.size();  j += 1)
    {// j is the boundary of partition1
#if _DEBUG_7
      if (arg.size() is 5)
      cout << "split at " << j << endl;
#endif
     // Momenta in each set
     vector<int> partition1, partition2;
     vector<int> massValue1(0), massValue2(0);
     // and their particle IDs (including helicities)
     vector<ParticleID> id1, id2;
#if _DEBUG_9
     if (arg.size() is 4)
     cout << "j: " << j << "; part1: " << part1 << "; part2: " << part2 << endl;
#endif
      if (part2 < part1)
        {// partition1 is from j up to & including part1 if part1 > j > part2
         // (which is the case of part1 = arg.size(), part2 is BaseIndex)
         // partition1 is from part1 up to & including j if j > part1
         // partition1 is from part1 up to arg.size()-1 and
         // BaseIndex up to & including j if j < part1
         // Need at least two external legs in each partition,
         // or (8/26/08) one if there's a vector that might attach
#if 0
          if (vectorK.size() is 0
              and (j is part1 or j is part2
                   or (part2 is BaseIndex and j is BaseIndex+1)
                   or (part2 is BaseIndex and j is arg.size()-1)
                   or (j is part2-1))) continue;
#else
#if 0
          cout << (j is part1 or j is part2
                   or (part2 is BaseIndex and j is BaseIndex+1)
                   or (part2 is BaseIndex and j is arg.size()-1)
                   or (j is part2-1)) << " vs "
               <<  AllowedSplit(arg.size(),part1,part2,j,vectorK.size(),
                                leg[part1],leg[part2]) << endl;
#endif
          //          cout << "!!!1a" << endl;
          if (not AllowedSplit(arg.size(),part1,part2,j,vectorK.size(),
                               leg[part1],leg[part2])) continue;
          //          cout << "!!!1b" << endl;
#endif
#if _DEBUG_4
          if (arg.size() is 5)
          cout << "p2 [" << part2 << "] < p1 [" << part1 << "],";
#endif
          if ((j < part1 and j > part2)
              or (j is part1 and part2 < part1-1) /* for electroweak */)
             {
#if _DEBUG_4
          if (arg.size() is 5)
            cout << "j [" << j << "] < p1 & > p2" << endl;
#endif
               for (int l = j;  l < part1;  l += 1)
                  {partition1.push_back(arg[l]);  id1.push_back(leg[l]);
                   if (massValue.size() > 0) massValue1.push_back(massValue[l]);}
             for (int l = BaseIndex;  l < j;  l += 1)
               if (l isnt part2) {partition2.push_back(arg[l]); id2.push_back(leg[l]);
                 if (massValue.size() > 0) massValue2.push_back(massValue[l]);}}
          else if (j >= part1 /* = for electroweak case */)
             {
#if _DEBUG_4
          if (arg.size() is 5)
            cout << "j [" << j << "] > p1" << endl;
#endif
               for (int l = part1+1;  l <= j;  l += 1)
                  {partition1.push_back(arg[l]);  id1.push_back(leg[l]);
                   if (massValue.size() > 0) massValue1.push_back(massValue[l]);}
              for (int l = j+1;  l < arg.size();  l += 1)
                 {partition2.push_back(arg[l]);  id2.push_back(leg[l]);
                  if (massValue.size() > 0) massValue2.push_back(massValue[l]);}
              for (int l = BaseIndex;  l < part2;  l += 1)
                 {partition2.push_back(arg[l]); id2.push_back(leg[l]);
                  if (massValue.size() > 0) massValue2.push_back(massValue[l]);}}
          else if (j < part1 /* Need to check explicitly because of
                                electroweak-vector case, when j can = part1 */)
             {
#if _DEBUG_4
          if (arg.size() is 5)
            cout << "j [" << j << "] < p1" << endl;
#endif
            for (int l = part1+1;  l < arg.size();  l += 1)
                {partition1.push_back(arg[l]);  id1.push_back(leg[l]);
                 if (massValue.size() > 0) massValue1.push_back(massValue[l]);}
             for (int l = BaseIndex;  l <= j;  l += 1)
                {partition1.push_back(arg[l]);  id1.push_back(leg[l]);
                 if (massValue.size() > 0) massValue1.push_back(massValue[l]);}
             for (int l = j+1;  l < part2;  l += 1)
                {partition2.push_back(arg[l]); id2.push_back(leg[l]);
                 if (massValue.size() > 0) massValue2.push_back(massValue[l]);}}}
      else // part1 < part2
        // partition1 is from part1 up to & including j if part1 < j < part2
        // (which is the case of part1 = BaseIndex, part2 = arg.size())
        // otherwise, partition1 is from j up to & including part1 if j < part1
        // partition1 is from j up to size & 0 up to part1 if j > part1
         {// Need at least two external legs in each partition
         // or (8/26/08) one if there's a vector that might attach
#if 0
           if (vectorK.size() is 0
               and (j is part1 or j is part2
               or (part2 < arg.size()-1 and j is part2+1)
                    or (part2 is arg.size()-1 and (j is BaseIndex or j is part2-1))))
             continue;
#else
           //          cout << "!!!2a" << endl;
          if (not AllowedSplit(arg.size(),part1,part2,j,vectorK.size(),
                               leg[part1],leg[part2])) continue;
          //          cout << "!!!2b" << endl;
#endif
#if _DEBUG_4
          if (arg.size() is 5)
           cout << "p1 [" << part1 << "] < p2 [" << part2 << "], ";
#endif
          if ((j > part1 and j < part2)
              or (j is part1 and part2 > part1+1 /* for electroweak case */))
             {for (int l = part1+1;  l <= j;  l += 1)
                {partition1.push_back(arg[l]);  id1.push_back(leg[l]);
                 if (massValue.size() > 0) massValue1.push_back(massValue[l]);}
             for (int l = j+1;  l < part2;  l += 1)
                {partition2.push_back(arg[l]); id2.push_back(leg[l]);
                 if (massValue.size() > 0) massValue2.push_back(massValue[l]);}}
          else if (j < part1
                   or (j is part1 and part2 is part1+1 /* = for electroweak vector case */)) {
#if _DEBUG_4
          if (arg.size() is 5)
            cout << "j [" << j << "] < p1" << endl;
#endif
          for (int l = j;  l < part1;  l += 1)
             {partition1.push_back(arg[l]);  id1.push_back(leg[l]);
                 if (massValue.size() > 0) massValue1.push_back(massValue[l]);}
            // part2 is next, skip it until shifts are done
            for (int l = part2+1;  l < arg.size();  l += 1)
               {partition2.push_back(arg[l]); id2.push_back(leg[l]);
                 if (massValue.size() > 0) massValue2.push_back(massValue[l]);}
            for (int l = BaseIndex;  l < j;  l += 1)
               {partition2.push_back(arg[l]); id2.push_back(leg[l]);
                 if (massValue.size() > 0) massValue2.push_back(massValue[l]);}}
          else if (j > part2 /* Need to check explicitly? */)
             {
#if _DEBUG_4
          if (arg.size() is 5)
               cout << "j [" << j << "] > p1" << endl;
#endif
for (int l = part2+1;  l < j;  l += 1)
                {partition2.push_back(arg[l]); id2.push_back(leg[l]);
                 if (massValue.size() > 0) massValue2.push_back(massValue[l]);}
             for (int l = j;  l < arg.size();  l += 1)
                {partition1.push_back(arg[l]);  id1.push_back(leg[l]);
                 if (massValue.size() > 0) massValue1.push_back(massValue[l]);}
             for (int l = BaseIndex;  l < part1;  l += 1)
                {partition1.push_back(arg[l]);  id1.push_back(leg[l]);
                 if (massValue.size() > 0) massValue1.push_back(massValue[l]);}}}

#if _DEBUG_4
      if (arg.size() is 3) {
      cout << "partition1: ";  PrintVector(partition1); cout << endl;
      cout << "partition2: ";  PrintVector(partition2); cout << endl;
      cout << "part1: " << arg[part1] << endl;
      }
#endif

      bool part1first /* cyclicly */;
      part1first = (part1 < part2
                    and (part1 isnt BaseIndex or part2 isnt arg.size()-1))
        or (part1 is arg.size()-1 and part2 is BaseIndex);
      //      cout << part1 << " " << part2 << ": " << part1first << endl;

      // Compute shift vector; shift pair can be (massless,massless) or (massive,massless)
      int shiftVector;
      if (massValue.size() > 0)
         {if (IsScalar(leg[shift1]) and massValue[shift1] >= 0)
            shiftVector = k.insert(k.p(arg[shift1]).Sm()*k.Lt(arg[shift2]),
                                   k.Lt(arg[shift2]));
         else if (IsScalar(leg[shift2]) and massValue[shift2] >= 0)
            shiftVector = k.insert(k.L(arg[shift1]),k.p(arg[shift2]).Sm()*k.L(arg[shift1]));
         else shiftVector = k.insert(k.L(arg[shift1]),k.Lt(arg[shift2]));}
      else shiftVector = k.insert(k.L(arg[shift1]),k.Lt(arg[shift2]));

      // Set up particle ids of both partitions & figure out id of internal line
      if (part1first)
         id1.push_back(leg[shift1]);
      else id2.push_back(leg[shift2]);
#if 0
      // Figure out particle type of internal leg
      // Scope out fermion flavors; only one flavor can be imbalanced
      vector<bool> flavorParity =
        part1first ? FermionParity(id1,BaseIndex,id1.size()-1)
        : FermionParity(id2,BaseIndex,id2.size()-1);
      bool alreadyImbalanced = false;
      int internalFlavor;
      bool skip = false;
      //      PrintVector(FermionCount(id1,BaseIndex,id1.size()-1));
      for (int f = 0;  f < flavorParity.size();  f += 1)
        if (flavorParity[f])
          if (alreadyImbalanced) {skip = true; break;}
          else {alreadyImbalanced = true;  internalFlavor = f;}
      if (skip) continue; // No contribution here

      ParticleID internalID = alreadyImbalanced ? FlavoredQuarkID(internalFlavor) : GluonID;
#else
      ParticleID internalID;
//      cout << "Cl: " << Classify(part1first ? id1 : id2,internalID,0) << endl;
#if 0
      cout << "part 1 first? " << part1first
           << " id1.s: " << id1.size() << "; id2.s: " << id2.size()
           << endl;
#endif
      if (not Classify(part1first ? id1 : id2,internalID,0)) continue;
#if 0
      cout << "Internal: " << ParticleCode(internalID) << endl;
#endif
#endif
      // Note that adding these at the end relies on the fact that
      // they are at the edge of the partitions, which in turn relies on
      // the fact that the two shifted legs are neighbors; the order here
      // is needed to keep the right cyclic ordering
      int internal1, internal2;

      /* 8/4/08: Sum over vector being attached to either current if
         the internalID is the appropriate quark flavor, or just to one
         side or other other if it isn't */
      {// Select vectors coupling only to partition1
        vector<int> vectorFor1(0), vectorFor2(0), vectorForBoth(0);
        vector<int> polarizationFor1(0), polarizationFor2(0),
          polarizationForBoth(0);
        vector<int> coupleTo1(0), coupleTo2(0), coupleToBoth(0);
        // Need to include shifted legs in count (they might be only
        // fermions on one side)
        vector<int> count1 = FermionCount(id1,BaseIndex,id1.size()-1,
                                          leg[shift1]);
#if 0
        vector<int> count2 = FermionCount(id2,BaseIndex,id2.size()-1);
        vector<int> countA = FermionCount(leg,BaseIndex,leg.size()-1);
#endif
#if 0
        cout << "count1: ";  PrintVector(count1);  cout << endl;
        cout << "count2: ";  PrintVector(count2);  cout << endl;
        cout << "countA: ";  PrintVector(countA);  cout << endl;
#endif

        for (int v = 0;  v < vectorK.size();  v += 1)
           {if (count1[coupleTo[v]] > 0)
               if (coupleTo[v] is FlavorOf(internalID))
                  {vectorForBoth.push_back(vectorK[v]);
                    polarizationForBoth.push_back(polarization[v]);
                    coupleToBoth.push_back(coupleTo[v]);}
               else {vectorFor1.push_back(vectorK[v]);
                 polarizationFor1.push_back(polarization[v]);
                 coupleTo1.push_back(coupleTo[v]);}
             else {vectorFor2.push_back(vectorK[v]);
               polarizationFor2.push_back(polarization[v]);
               coupleTo2.push_back(coupleTo[v]);}}
#if 0
        cout << "ct1: " << coupleTo1.size() << "; ct2: " << coupleTo2.size()
             << "; ctb: " << coupleToBoth.size() << endl;
        cout << "sizes: " << partition1.size() << " ";
        PrintVector(partition1);
        cout << " " << partition2.size() << " ";
        PrintVector(partition2);  cout << endl;
#endif
        /* Sum over all possible assignments of "vectorForBoth"
           by constructing the set of all assignments, a bit set;
           assume that we have no more than 8 vectors */
        //        cout << "limit: " << ((unsigned short int)0xFF >> (8-vectorForBoth.size())) << endl;
        for (unsigned short int inPart1 = 0;
             inPart1 <= ((unsigned short int)0xFF >> (8-vectorForBoth.size()));
             inPart1 += 1)
          { vector<int> vector1 = vectorFor1, vector2 = vectorFor2;
            vector<int> polarization1 = polarizationFor1,
              polarization2 = polarizationFor2;
            vector<int> couple1 = coupleTo1, couple2 = coupleTo2;
            //            cout << "in part1: " << inPart1 << endl;
           for (int v = 0;  v < vectorForBoth.size();  v += 1)
             if (inPart1 & (1<<v)) {vector1.push_back(vectorForBoth[v]);
               polarization1.push_back(polarizationForBoth[v]);
               couple1.push_back(coupleToBoth[v]);}
             else {vector2.push_back(vectorForBoth[v]);
               polarization2.push_back(polarizationForBoth[v]);
               couple2.push_back(coupleToBoth[v]);}

           // Need at least two external legs if there's no vector coupling,
           // on each side of the internal line; note that the partitions
           // don't yet include the shift legs or the internal ones
           if ((partition1.size() > 0 or couple1.size() > 0)
               and (partition2.size() > 0 or couple2.size() > 0))
             result += HelicitySum(k,arg,leg,part1,part2,shiftVector,
                                   part1first,shift1,shift2,
                                   partition1,partition2,
                                   id1,id2,internalID,
                                   vector1,polarization1,couple1,
                                   vector2,polarization2,couple2,
                                   massValue,massValue1,massValue2);
          }
      }
    }
 k.put_value(key,result);
 }
#if _DEBUG_4
          if (arg.size() is 5)
 cout << "====" << endl;
#endif
 return result;

}

  // Explicit instantiation
template class complex<R>
  Aosrr(momentum_configuration<R>& k,
        const vector<int>& arg /* indices of arguments */,
        const vector<particle_ID>& leg /* helicities and particle ids */,
        const vector<int>& vectorK /* momenta */ ,
        const vector<int>& polarization ,
        const vector<int>& coupleTo /* quark flavor */ ,
        const vector<int>& massValue );
}}


