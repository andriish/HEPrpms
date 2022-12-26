/*  util.cc  */

/*  David A. Kosower, June 2, 2008  */

/* Utility routines for counting particle types, extracting helicities, etc.;
   extracted from tree1.cc

   7/17/08: More routines moved from tree1.cc

 */

#include <vector>
#include "spinor.h"
#include "mom_conf.h"
#include "Tree.h"
#include "Tree_impl.h"
#include "util.h"

#include "particleid.h"

#define BaseIndex 0 // Must match value in tree1.cc

#define isnt !=
#define is ==

using namespace std;
namespace BH {
#undef IsGluon
#define IsGluon(id) id.is_a(gluon)
#undef IsQuark
#define IsQuark(id) id.is_a(quark)
#undef FlavorOf
#define FlavorOf(id) id.flavor()
#undef HelicityOf
#define HelicityOf(id) id.helicity()
#define IsScalar(id) id.is_a(scalar)

/* Counts the number of fermion legs of each different flavor in the
   given range of arguments within "id".  The returned vector has length
   equal to the highest flavor number in "id" plus one (the 0 component
   is not used)
*/
vector<int> FermionCount(const vector<particle_ID>& id, int start, int end)
{int max = 0;
 for (int j = BaseIndex;  j < id.size();  j += 1)
    if (IsQuark(id[j])) max = Max(max,(int)FlavorOf(id[j]));
 vector<int> count(max+1,0); // Initialized to 0
 if (start <= end)
   {for (int j = start;  j <= end;  j += 1)
     if (IsQuark(id[j])) count[FlavorOf(id[j])] += 1;}
 else {for (int j = start;  j < id.size();  j += 1)
      if (IsQuark(id[j])) count[FlavorOf(id[j])] += 1;
    for (int j = BaseIndex;  j <= end;  j += 1)
      if (IsQuark(id[j])) count[FlavorOf(id[j])] += 1;}

 return count;
}

vector<int> FermionCount(const vector<particle_ID>& id, int start, int end,
                           const particle_ID& addedLeg)
{vector<int> count = FermionCount(id,start,end);
  int max;
  if (IsQuark(addedLeg))
     {if ((max = (int)FlavorOf(addedLeg)) >= count.size())
         count.resize(max+1,0);
       count[max] += 1;}
  return count;
}

/* Determines whether the number of fermion legs of each different
   flavor is even or not.  Return vector as in FermionCount above.
*/
vector<bool> FermionParity(const vector<particle_ID>& id, int start, int end)
{vector<int> count = FermionCount(id,start,end);
#if 0
 cout << "FP: "; PrintVector(id); cout << "[" << start << ":" << end << "]: ";
 PrintVector(count); cout << endl;
#endif
 vector<bool> parity(count.size());
 for (int j = 1;  j < count.size();  j += 1) parity[j] = IsOdd(count[j]);
 return parity;
}

/* 6/3/08: Counts the number of scalar legs of each different flavor in the
   given range of arguments within "id".  The returned vector has length
   equal to the highest flavor number in "id" plus one (the 0 component
   is not used, that "flavor" referring to gluons within the tree code)
*/
vector<int> ScalarCount(const vector<particle_ID>& id, int start, int end)
{int max = 0;
 for (int j = BaseIndex;  j < id.size();  j += 1)
    if (IsScalar(id[j])) max = Max(max,(int)FlavorOf(id[j]));
 vector<int> count(max+1,0); // Initialized to 0
 if (start <= end)
   {for (int j = start;  j <= end;  j += 1)
     if (IsScalar(id[j])) count[FlavorOf(id[j])] += 1;}
 else {for (int j = start;  j < id.size();  j += 1)
      if (IsScalar(id[j])) count[FlavorOf(id[j])] += 1;
    for (int j = BaseIndex;  j <= end;  j += 1)
      if (IsScalar(id[j])) count[FlavorOf(id[j])] += 1;}

 return count;
}

/* 6/3/08: Determines whether the number of scalar legs of each different
   flavor is even or not.  Return vector as in ScalarCount above.
*/
vector<bool> ScalarParity(const vector<particle_ID>& id, int start, int end)
{vector<int> count = ScalarCount(id,start,end);
#if 0
 cout << "SP: "; PrintVector(id); cout << "[" << start << ":" << end << "]: ";
 PrintVector(count); cout << endl;
#endif
 vector<bool> parity(count.size());
 for (int j = 1;  j < count.size();  j += 1) parity[j] = IsOdd(count[j]);
 return parity;
}

// Helicity ignored here
particle_ID FlavoredQuarkID(int flavor) {
  return particle_ID(quark,1,flavor,false);
}

particle_ID FlavoredScalarID(int flavor) {
  return particle_ID(scalar,1,flavor,false);
}

particle_ID NParticleID(int helicity,const particle_ID& base) {
  return particle_ID(base.type(),helicity,base.flavor(),false);
}

vector<particle_ID> NParticleID(const vector<int>& helicity,
                               const vector<particle_ID>& base) {
  vector<particle_ID> result(base.size());
  for (int j = 0;  j < base.size();  j += 1)
    result[j] = NParticleID(helicity[j],base[j]);
  return result;
}

vector<particle_ID> NParticleID(const vector<int>& helicity,
                               const vector<particle_ID>& base,int n) {
  size_t size = n < base.size() ? n : base.size();
  vector<particle_ID> result(size);
  for (int j = 0;  j < size;  j += 1)
    result[j] = NParticleID(helicity[j],base[j]);
  return result;
}

// returns a characteristic integer
int ParticleCode(particle_ID id) {
  if (id.is_a(gluon)) return Gluon;
  else if (id.is_a(quark)) return(FlavoredQuark(id.flavor()));
  else if (id.is_a(scalar)) return(FlavoredScalar(id.flavor()));
}

vector<int> ParticleCode(const vector<particle_ID>& id) {
  vector<int> result(id.size());
  for (int j = 0;  j < id.size();  j += 1)
    result[j] = ParticleCode(id[j]);
  return result;
}

vector<int> Helicities(const vector<particle_ID>& id) {
  vector<int> result(id.size());
  for (int j = 0;  j < id.size();  j += 1)
    result[j] = id[j].helicity();
  return result;
}

// 7/17/08
namespace Tree {
#include "zero.h"

/* 8/4/08 Counts the number of legs of each different mass-vector index
   within the given range of arguments into "massValue".  Because the indices
   can be quite large (and not contiguous), use a map rather than a vector.
   The key is the mass index, while the value is the count of how many times
   it appears. */
map<int,int> MassIndexCount(const vector<int>& massValue, int start, int end)
{map<int,int> count;
  if (massValue.size() is 0) return(count);
 if (start <= end)
   {for (int j = start;  j <= end;  j += 1)
       if (massValue[j] >= 0) count[massValue[j]] += 1;}
 else {for (int j = start;  j < massValue.size();  j += 1)
       if (massValue[j] >= 0) count[massValue[j]] += 1;
    for (int j = BaseIndex;  j <= end;  j += 1)
      if (massValue[j] >= 0) count[massValue[j]] += 1;}

 return count;
}


// Computes the sum of the momenta v[start..end], if not already
// known, and creates a new momentum entry corresponding to it.
// "start" and "end" are understood in a cyclic sense
template<class T> int
  MomentumSum(momentum_configuration<T>& k,
              const vector<int>& v, int start, int end,
              const vector<int>& extraK = empty)
{/* For massless complex momenta, we must be careful to preserve the
    separate lambda and lambda-tilde spinors, which will in general not
    be identical to those produced by the "insert" below, even if k[sum]
    is identical to the original momentum.  In addition, this is faster... */
  if (start is end and extraK.empty()) return(v[start]);
  string key = GenKey("ms",start,end,v,extraK);
// cout << "In MS " << start << ", " << end << endl;
 size_t index;
 if (not k.get_label(key,index)) {
    momentum<complex<T> > sum;  // Initialized to 0; don't use Cmom, to avoid recomputing
    // lambda & lambda-tilde at every +=
    if (start <= end)
       for (int j = start;  j <= end;  j += 1)
          {sum += k.mom(v[j]);
         //         cout << v[j] << ": " << k.mom(v[j]) << endl;
          }
    else {for (int j = start;  j < v.size();  j += 1)  sum += k.mom(v[j]);
       for (int j = BaseIndex;  j <= end;  j += 1)
         sum += k.mom(v[j]);}
 #if FIXZERO
    FixZero(sum);
 #endif
    // 8/5/08 For electroweak vectors...
    for (int j = 0;  j < extraK.size();  j += 1)
      sum += k.mom(extraK[j]);
    //    cout << "sum: " << sum << endl;
    index = k.insert(sum);
    k.put_label(key,index);}
 // cout << "sum [" << index << "]: " << k.p(index) << endl;
 return index;
}

// Computes the flatted sum of the momenta v[start..end], if not already
// known, and creates a new momentum entry corresponding to it.
// "start" and "end" are understood in a cyclic sense
template<class T> /* static */ int FlatSum(momentum_configuration<T>& k,
            int ref /* index of reference momentum */,
                                           const vector<int>& v, int start, int end,
                                           const vector<int>& extraK = empty)
{// cout << "In FS " << start << ", " << end << endl;
 /* For massless complex momenta, we must be careful to preserve the
    separate lambda and lambda-tilde spinors, which will in general not
    be identical to those produced by the "insert" below, even if k[sum]
    is identical to the original momentum. */
  if (start is end and extraK.empty() and IsZero(k.m2(v[start])))
     return(v[start]);
  int sum = MomentumSum(k,v,start,end,extraK);
  string key = GenKey("fs",MakeVector(start,end,ref),v,extraK);
 size_t index;
 if (not k.get_label(key,index)) {
    Cmom<T> flat = k[sum] - (k.m2(sum)/(T(2)*(k[sum]*k[ref]))) * k[ref];
 #if FIXZERO
    FixZero(flat);
 #endif
    index = k.insert(flat);
#if 0
    if (!IsZero(Im(k[sum].E())))
       {T dummy; momentum<complex<T> > refMom2 = GenerateMomentum(dummy);
       int ref2 = k.insert(refMom2);
       momentum<complex<T> > refMom3 = GenerateMomentum(dummy);
       int ref3 = k.insert(refMom3);
        C value1 = k.spab(ref,sum,ref2);
        C value2 = k.spab(ref,index,ref2);
        C value3 = k.spa(ref,index)*k.spb(index,ref2);
        cout << "CFS: " << value2/value1 << " "
             <<value3/value1 << endl;
        value1 = k.spab(ref3,sum,ref2);
        value2 = k.spab(ref3,index,ref2);
        value3 = k.spa(ref3,index)*k.spb(index,ref2);
        cout << "CFS a: " << value2/value1 << " "
             <<value3/value1 << endl;
        }
#endif
    k.put_label(key,index);}
 return index;
}

// Computes the flatted sum of the negative of the
//   momenta v[start1..end1], if not already
// known, and creates a new momentum entry corresponding to it.
// "start" and "end" are understood in a cyclic sense
template<class T> int
  NegativeFlatSum(momentum_configuration<T>& k,
                  int ref /* index of reference momentum */,
                  const vector<int>& v, int s1, int e1)
{// cout << "In NFS " << s1 << ", " << e1 << "; " << s2 << ", " << e2 << endl;

 string key = GenKey("nf",s1,e1,ref,v);
 size_t index;
 if (not k.get_label(key,index)) {
   /* For massless complex momenta, we must be careful to preserve the
      separate lambda and lambda-tilde spinors, which will in general not
      be identical to those produced by the "insert" after flattening below,
      even if k[sum] is identical to the original momentum. */
   if (s1 is e1 and IsZero(k.m2(v[s1])))
      {Cmom<T> negative = -k[v[s1]]; // This operation does transfer spinors properly
      index = k.insert(negative);}
   else {
     int sum = MomentumSum(k,v,s1,e1);
     // The spinor products in BlackHat alas have their branch cuts along the
     // real axis (with the old phase convention), which means we have to be
     // VERY careful to ensure that
     // Negative(FlatSum(...)) is identical -- down to the sign of
     // (infinitesimal) imaginary parts to NegativeFlatSum(...) -- hence
     // do this in two steps
     momentum<complex<T> > flat = k.mom(sum) - (k.m2(sum)/(T(2)*(k[sum]*k[ref]))) * k.mom(ref);
     flat = -flat;
 #if FIXZERO
     FixZero(flat);
 #endif
     index = k.insert(flat);
     k.put_label(key,index);}}
 return index;
}

// 8/5/08
template<class T> int
  NegativeFlatSum(momentum_configuration<T>& k,
                  int ref /* index of reference momentum */,
                  const vector<int>& v, int s1, int e1,
                  int ev, const vector<int>& extraK = empty)
// With added vector momentum index "ev"
{// cout << "In NFS " << s1 << ", " << e1 << "; " << s2 << ", " << e2 << endl;

  string key = GenKey("nf",MakeVector(s1,e1,ref,ev),v,extraK);
  size_t index;
  if (not k.get_label(key,index)) {
    /* For massless complex momenta, we must be careful to preserve the
       separate lambda and lambda-tilde spinors, which will in general not
       be identical to those produced by the "insert" after flattening below,
       even if k[sum] is identical to the original momentum. */
    int sum = MomentumSum(k,v,s1,e1,extraK);
    // The spinor products in BlackHat alas have their branch cuts along the
    // real axis (with the old phase convention), which means we have to be
    // VERY careful to ensure that
    // Negative(FlatSum(...)) is identical -- down to the sign of
    // (infinitesimal) imaginary parts to NegativeFlatSum(...) -- hence
    // do this in two steps
    momentum<complex<T> > flat =
      k.mom(sum)+k.mom(ev)
      - (k.s(sum,ev)/(T(2)*((k[sum]+k[ev])*k[ref]))) * k.mom(ref);
    flat = -flat;
#if FIXZERO
    FixZero(flat);
#endif
    index = k.insert(flat);
    k.put_label(key,index);}
  return index;
}

// Computes the flatted sum of the negative of the
//   momenta v[start1..end1] + v[start2..end2], if not already
// known, and creates a new momentum entry corresponding to it.
// "start" and "end" are understood in a cyclic sense
template<class T> int
  NegativeFlatSum(momentum_configuration<T>& k,
                  int ref /* index of reference momentum */,
                  const vector<int>& v, int s1, int e1, int s2, int e2,
                  const vector<int>& extraK1,
                  const vector<int>& extraK2)
{// cout << "In NFS " << s1 << ", " << e1 << "; " << s2 << ", " << e2 << endl;
  int sum1 = MomentumSum(k,v,s1,e1,extraK1);
  int sum2 = MomentumSum(k,v,s2,e2,extraK2);

 // string key = GenKey("nf",s1,e1,s2,e2,ref,v);
 // until appropriate routine is available:
  string key = GenKey("nf",MakeVector(s1,e1,s2,e2,ref),v,Join(extraK1,extraK2));
 size_t index;
 if (not k.get_label(key,index)) {
   // Can't use k[sum1] notation here, ugh
   momentum<complex<T> > ksum = k.mom(sum1)+k.mom(sum2);
   int sum = k.insert(ksum);
   momentum<complex<T> > flat =
     ksum - (k.m2(sum)/(T(2)*(k[sum]*k[ref]))) * k.mom(ref);
#if 0
    cout << "NF sum: " << ksum << endl;
    cout << "NF sum^2: " << k.m2(sum) << endl;
    cout << "NF ref [" << ref << "]: " << k[ref] << endl;
    cout << "NF den: " << (2.*(k[sum]*k[ref])) << endl;
    cout << "NF den alt: " << (k.s(sum,ref)-k.m2(sum)) << endl;
    cout << "NF den alt 2: " << (2.*(k[sum].E()*k[ref].E()
                                     -k[sum].X()*k[ref].X()
                                     -k[sum].Y()*k[ref].Y()
                                     -k[sum].Z()*k[ref].Z())) << endl;
#endif
    flat = -flat;
    //    cout << "NFS 1: " << flat << endl;
 #if FIXZERO
    FixZero(flat);
 #endif
#if 0
    if (Im(flat.E()) == 0) {cout << "zero" << endl;
    flat = Cmom<R>(C(Re(flat.E()),0),C(Re(flat.X()),0),C(Re(flat.Y()),0),
                C(Re(flat.Z()),0));}
    //    if (Im(flat.E()) == 0) flat.E() *= copysign(1.0,flat.E());
#endif
    //    cout << "NFS 2: " << flat << endl;
    index = k.insert(flat);
    k.put_label(key,index);}
 // cout << "NFS 2 [" << index << "]: " << k[index] << endl;
 return index;
}

template<class T> int
  NegativeFlatSum(momentum_configuration<T>& k,
                  int ref /* index of reference momentum */,
                  const vector<int>& v, int s1, int e1, int s2, int e2,
                  int s3, int e3)
{
  // cout << "In NFS " << s1 << ", " << e1 << "; " << s2 << ", " << e2
  //      << s3 << ", " << e3 << endl;
 int sum1 = MomentumSum(k,v,s1,e1);
 int sum2 = MomentumSum(k,v,s2,e2);
 int sum3 = MomentumSum(k,v,s3,e3);

 // string key = GenKey("nf",s1,e1,s2,e2,s2,e3,ref,v);
 // until appropriate routine is available:
 string key = GenKey("nf",MakeVector(s1,e1,s2,e2,s3,e3,ref),v);
 size_t index;
 if (not k.get_label(key,index)) {
   // Can't use k[sum1] notation here, ugh
    momentum<complex<T> > ksum = k.mom(sum1)+k.mom(sum2)+k.mom(sum3);
    int sum = k.insert(ksum);
    momentum<complex<T> > flat = ksum - (k.m2(sum)/(T(2)*(k[sum]*k[ref]))) * k.mom(ref);
    flat = -flat;
 #if FIXZERO
    FixZero(flat);
 #endif
    index = k.insert(flat);
    k.put_label(key,index);}
 return index;
}

template<class T> int
  Negative(momentum_configuration<T>& k, int i)
{string key = GenKey("neg",i);
 size_t index;
 if (not k.get_label(key,index)) {
   Cmom<T> kneg = -k[i]; // Note this transfers spinors properly for complex momenta
    //    cout << "N 1: " << kneg << endl;
 #if FIXZERO
    FixZero(kneg);
 #endif
    //    cout << "N 2: " << kneg << endl;
    index = k.insert(kneg);
    k.put_label(key,index);}
 return index;
}


vector<int> Join(const vector<int>& v1, const vector<int>& v2)
{vector<int> result = v1;
copy(v2.begin(), v2.end(), back_insert_iterator<vector<int> >(result));
 return result;
}

vector<int> Join(const vector<int>& v1, const vector<int>& v2,
                 const vector<int>& v3)
{vector<int> result = v1;
copy(v2.begin(), v2.end(), back_insert_iterator<vector<int> >(result));
copy(v3.begin(), v3.end(), back_insert_iterator<vector<int> >(result));
 return result;
}

vector<int> Join(const vector<int>& v1, const vector<int>& v2,
                 const vector<int>& v3, const vector<int>& v4)
{vector<int> result = v1;
copy(v2.begin(), v2.end(), back_insert_iterator<vector<int> >(result));
copy(v3.begin(), v3.end(), back_insert_iterator<vector<int> >(result));
copy(v4.begin(), v4.end(), back_insert_iterator<vector<int> >(result));
 return result;
}

//EXPLICIT INSTANTIATION
template int
  MomentumSum(momentum_configuration<R>& k,
              const vector<int>& v, int start, int end,
              const vector<int>& extraK );
template int
  MomentumSum(momentum_configuration<RHP>& k,
              const vector<int>& v, int start, int end,
              const vector<int>& extraK );
template /* static */ int FlatSum(momentum_configuration<R>& k,
            int ref /* index of reference momentum */,
                                  const vector<int>& v, int start, int end,
                                  const vector<int>& extraK );
template /* static */ int FlatSum(momentum_configuration<RHP>& k,
            int ref /* index of reference momentum */,
                                  const vector<int>& v, int start, int end,
                                  const vector<int>& extraK);

template int
  NegativeFlatSum(momentum_configuration<R>& k,
                  int ref /* index of reference momentum */,
                  const vector<int>& v, int s1, int e1);
template int
  NegativeFlatSum(momentum_configuration<RHP>& k,
                  int ref /* index of reference momentum */,
                  const vector<int>& v, int s1, int e1);

template int
  NegativeFlatSum(momentum_configuration<R>& k,
                  int ref /* index of reference momentum */,
                  const vector<int>& v, int s1, int e1,
                  int ev, const vector<int>& extraK );
template int
  NegativeFlatSum(momentum_configuration<RHP>& k,
                  int ref /* index of reference momentum */,
                  const vector<int>& v, int s1, int e1,
                  int ev, const vector<int>& extraK );

template int
  NegativeFlatSum(momentum_configuration<R>& k,
                  int ref /* index of reference momentum */,
                  const vector<int>& v, int s1, int e1, int s2, int e2,
                  const vector<int>& extraK1 ,
                  const vector<int>& extraK2 );
template int
  NegativeFlatSum(momentum_configuration<RHP>& k,
                  int ref /* index of reference momentum */,
                  const vector<int>& v, int s1, int e1, int s2, int e2,
                  const vector<int>& extraK1 ,
                  const vector<int>& extraK2 );

template int
  NegativeFlatSum(momentum_configuration<R>& k,
                  int ref /* index of reference momentum */,
                  const vector<int>& v, int s1, int e1, int s2, int e2,
                  int s3, int e3);
template int
  NegativeFlatSum(momentum_configuration<RHP>& k,
                  int ref /* index of reference momentum */,
                  const vector<int>& v, int s1, int e1, int s2, int e2,
                  int s3, int e3);

template int Negative(momentum_configuration<R>& k, int i);
template int Negative(momentum_configuration<RHP>& k, int i);
}

}

template <> BH::momentum<std::complex<double> > BH::Tree::GenerateMomentum<double>(  double const &  dummy) { return GenerateMomentum<double>(dummy); }
template <> BH::momentum<std::complex<dd_real> > BH::Tree::GenerateMomentum<dd_real>( dd_real const &  dummy) { return GenerateMomentum<dd_real>(dummy); }
