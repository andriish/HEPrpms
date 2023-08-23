/*
 * partial_amplitude.cpp
 *
 *  Created on: 22-Apr-2009
 *      Author: daniel
 */

#include "cached_OLHA.h"
#include "assembly.h"
#include <map>
#include "index_vector.h"
#include <iterator>
#include <iostream>
#include "BH_debug.h"

#define _VERBOSE 0

using namespace std;
namespace BH {

partial_amplitude_base::partial_amplitude_base(const partial_amplitude_base& PA){
	if ( PA.m_prefactor !=0 ) { m_prefactor = PA.m_prefactor->clone();} else { m_prefactor=0; };
	m_lo_or_nlo=PA.m_lo_or_nlo;
}
/*
partial_amplitude::partial_amplitude(const partial_amplitude& PA){
	if ( PA.m_prefactor !=0 ) { m_prefactor = PA.m_prefactor->clone();}else { m_prefactor=0; }
	indices=PA.indices;
	subs_indices=PA.subs_indices;
	factors=PA.factors;
	for (int j=0;j<PA.ampls.size();j++){
		ampls.push_back(new OneLoopHelAmpl(PA.ampls[j]->get_process(),PA.ampls[j]->color_struct()));
	}
	for (int j=0;j<PA._subs.size();j++){
		_subs.push_back(new subtraction(*PA._subs[j]));
	}
}

void partial_amplitude::add(const process& pro, color_structure cs, const vector<int>& ind,int num, int den){
	ampls.push_back(new OneLoopHelAmpl(pro,cs));
	factors.push_back(multi_precision_fraction(num,den));
	indices.push_back(ind);
}

void partial_amplitude::add(const subtraction& sub,const vector<int>& ind){
	_subs.push_back(new subtraction(sub));
	subs_indices.push_back(ind);
}

template <class T> SeriesC<T> partial_amplitude::eval_fn(momentum_configuration<T>& mc, const vector<int>& ind,int mu_index){
	SeriesC<T> res(-2,0);
	for (int i=0;i<ampls.size();i++){
		ampls[i]->set_mu(mu_index);
		vector<int> new_ind;
		for (int j=0;j<ind.size();j++){
			new_ind.push_back(ind[indices[i][j]-1]);
		}
		Index_Vector IV(new_ind);
		res+=complex<T>(T(factors[i].get<T>()),0)*(ampls[i]->eval(mc,IV));
	BH_DEBUG_MESSAGE4("primitive amplitude:",*ampls[i]->tree()," normalized: ",res/ampls[i]->get_tree(mc,new_ind));
		}
	for (int i=0;i<_subs.size();i++){
		vector<int> new_ind;
		for (int j=0;j<ind.size();j++){
			new_ind.push_back(ind[subs_indices[i][j]-1]);
		}
		res-=_subs[i]->eval(mc,new_ind);
	}
	if (m_prefactor != 0 ){
		complex<T> pref=m_prefactor->eval(mc);
		res=pref*res;
	}
	return res;
}

void partial_amplitude::dry_run(const vector<int>& ind){
	for (int i=0;i<ampls.size();i++){
		vector<int> new_ind;
		for (int j=0;j<ind.size();j++){
			new_ind.push_back(ind[indices[i][j]-1]);
		}
		Index_Vector IV(new_ind);
		ampls[i]->dry_run(IV);
	}
}



partial_amplitude::~partial_amplitude(){
	for (int j=0;j<ampls.size();j++){
		delete ampls[j];
	}
}
*/

namespace CachedOLHA {
partial_amplitude_cached::~partial_amplitude_cached(){
	for (int j=0;j<d_subs.size();j++){
		delete d_subs[j];
	}
}

void partial_amplitude_cached::add(const process& pro, color_structure cs, const vector<int>& ind,int num, int den){
	if(m_lo_or_nlo==nlo){
	BH_DEBUG_MESSAGE5("Adding process",pro," for color_structure ",cs," in partial_amplitude_cached.");
	d_ampls.push_back(CachedOLHA::Cached_OLHA_factory::default_COLHA->new_OLHA(pro,cs,ind));
	d_factors.push_back(multi_precision_fraction(num,den));
	d_factors_R.push_back(1);
	d_indices.push_back(ind);
	};
}

void partial_amplitude_cached::add(const process& pro, color_structure cs, const vector<int>& ind,R weight){
	if(m_lo_or_nlo==nlo){
	BH_DEBUG_MESSAGE5("Adding process",pro," for color_structure ",cs," in partial_amplitude_cached.");
	d_ampls.push_back(CachedOLHA::Cached_OLHA_factory::default_COLHA->new_OLHA(pro,cs,ind));
	d_factors.push_back(multi_precision_fraction(1,1));
	d_factors_R.push_back(weight);
	d_indices.push_back(ind);
	};
}

void partial_amplitude_cached::add_subtraction(const process& pro, const vector<int>& ind, multi_precision_fraction mpf,int order){
	if(m_lo_or_nlo==nlo){
	d_subs.push_back(new subtraction(pro,ind,mpf,order));
	d_subs_indices.push_back(ind);
	}
}

template <class T> SeriesC<T> partial_amplitude_cached::eval_fn(momentum_configuration<T>& mc, const vector<int>& ind,int mu_index){
	SeriesC<T> res(-2,0);
	for (int i=0;i<d_ampls.size();i++){
		res+=complex<T>(d_factors[i].get<T>(),0)*complex<T>(d_factors_R[i],0)*(d_ampls[i]->eval(mc,mu_index));
		/*BH_DEBUG(
			cout << "ampl " << d_ampls[i].get_process() << " ";
			const vector<int>& vec=d_ampls[i].get_index_vector();
			copy(vec.begin(),vec.end(),ostream_iterator<int>(cout," "));
			cout << " " <<  d_ampls[i].color_struct() << ":" << d_ampls[i].eval(mc,mu_index)  << endl;  ///d_ampls[i].get_tree(mc,mu_index);
		);
        */
	}
	for (int i=0;i<d_subs.size();i++){
		/*vector<int> new_ind;
		for (int j=0;j<ind.size();j++){
			new_ind.push_back(ind[d_subs_indices[i][j]-1]);
		}
		res-=d_subs[i]->eval(mc,new_ind);
		 */
		res-=d_subs[i]->eval(mc);
	}
	if (m_prefactor != 0 ){
		complex<T> pref=m_prefactor->eval(mc);
		res=pref*res;
	}
	return res;
}

void partial_amplitude_cached::dry_run(const vector<int>& ind){
	for (int i=0;i<d_ampls.size();i++){
		d_ampls[i]->dry_run();
	}
}
}

}
