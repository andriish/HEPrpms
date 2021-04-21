/*
 * cached_OLHA.cpp
 *
 *  Created on: 12-Nov-2008
 *      Author: daniel
 */

#include "cached_OLHA.h"
#include "assembly.h"
#include <map>

#include "index_vector.h"
#include "IR_checked.h"
#include "BH_debug.h"

#define _VERBOSE 0

using namespace std;

namespace BH {


namespace CachedOLHA {

//Cached_OLHA::Cached_OLHA(const process& pro,color_structure cs){
//	d_OLHA_p=new OneLoopHelAmpl(pro,cs);
//}

Cached_OLHA::Cached_OLHA(OneLoopAmplitude_base* OLHAb) : d_OLHA_p(OLHAb) {

}

Cached_OLHA::~Cached_OLHA(){
	delete d_OLHA_p;
}

void Cached_OLHA::print_stat() {
	double sum=0;
	for (int i=0;i<d_load.size();i++){
		sum+=double(d_load[i]);
	}
	double av=sum/double(d_load.size());
	_MESSAGE8(d_OLHA_p->get_process()," ",d_OLHA_p->color_struct(),": ",d_index_vectors.size()," index vectors used in average ", av ," times. Last values: ");
	for (int i=0;i<d_load.size();i++){
		_MESSAGE9(i,": ",d_index_vectors[i]," acc:",int(get_accuracy(i))," loop: ",get_value<R>(i)/(get_tree_value<R>(i)+complex<R>(0.0000000001,0))," tree:",get_tree_value<R>(i));
	}
}



void Cached_OLHA::refresh(momentum_configuration<R>& mc, int mu_index) {
	for (int i=0;i<d_load.size();i++){
		eval(i,mc,mu_index);
	}
}


size_t Cached_OLHA::add(const vector<int>& indices){
	vector<vector<int> >::iterator it=find(d_index_vectors.begin(),d_index_vectors.end(),indices);
	if ( it == d_index_vectors.end() ){
		d_index_vectors.push_back(indices);
		d_load.push_back(1);
		d_values.push_back(SeriesC<R>(-2,0));
		d_values_HP.push_back(SeriesC<RHP>(-2,0));
		d_values_VHP.push_back(SeriesC<RVHP>(-2,0));
	    //
        d_conj_values.push_back(SeriesC<R>(-2,0));
		d_conj_values_HP.push_back(SeriesC<RHP>(-2,0));
		d_conj_values_VHP.push_back(SeriesC<RVHP>(-2,0));
        //
		d_tree_values.push_back(C(0,0));
		d_tree_values_HP.push_back(CHP(0,0));
		d_tree_values_VHP.push_back(CVHP(0,0));
		d_accuracy.push_back(0.);
		d_mcIDs.push_back(0);
		d_mcIDs_HP.push_back(0);
		d_mcIDs_VHP.push_back(0);
		d_mu_index.push_back(0);
		d_mu_index_HP.push_back(0);
		d_mu_index_VHP.push_back(0);
		return d_index_vectors.size()-1;
	} else {
		size_t pos=it-d_index_vectors.begin();
		++d_load[pos];
		return pos;
	}
}

template <class T> inline void set_mu_helper(OneLoopAmplitude_base* olhap,int index);

template <> inline void set_mu_helper<R>(OneLoopAmplitude_base* olhap,int index){olhap->set_mu(index);};
template <> inline void set_mu_helper<RHP>(OneLoopAmplitude_base* olhap,int index){olhap->set_mu_HP(index);};
template <> inline void set_mu_helper<RVHP>(OneLoopAmplitude_base* olhap,int index){olhap->set_mu_VHP(index);};
#ifdef BH_USE_GMP
template <> inline void set_mu_helper<RGMP>(OneLoopAmplitude_base* olhap,int index){olhap->set_mu_GMP(index);};
#endif

SeriesC<R> Cached_OLHA::eval(size_t n, momentum_configuration<R>& mc,int mu_index){
	if (mc.get_ID() != get_mcID<R>(n) || mu_index != get_mu_index<R>(n) ){
//		d_OLHA_p->set_mu(mu_index);
		set_mu_helper<R>(d_OLHA_p,mu_index);
		set_value(n, d_OLHA_p->eval(mc,Index_Vector(d_index_vectors[n])));
		set_conj_value(n, d_OLHA_p->get_conjugate_amplitude());

		set_tree_value(n, d_OLHA_p->get_tree(mc,d_index_vectors[n]));
		set_accuracy(n, d_OLHA_p->get_accuracy());
		set_mcID<R>(n, mc.get_ID()) ;
		set_mu_index<R>(n, mu_index) ;
	}
	return get_value<R>(n);
}


SeriesC<RHP> Cached_OLHA::eval(size_t n, momentum_configuration<RHP>& mc,int mu_index){
	if (mc.get_ID() != get_mcID<RHP>(n) || mu_index != get_mu_index<RHP>(n) ){
//		d_OLHA_p->set_mu(mu_index);
		set_mu_helper<RHP>(d_OLHA_p,mu_index);
		set_value(n, d_OLHA_p->eval(mc,Index_Vector(d_index_vectors[n])));
		set_conj_value(n, d_OLHA_p->get_conjugate_amplitude_HP());

		set_tree_value(n, d_OLHA_p->get_tree(mc,d_index_vectors[n]));
		set_accuracy(n, d_OLHA_p->get_accuracy());
		set_mcID<RHP>(n, mc.get_ID()) ;
		set_mu_index<RHP>(n, mu_index) ;
	}
	return get_value<RHP>(n);
}


SeriesC<RVHP> Cached_OLHA::eval(size_t n, momentum_configuration<RVHP>& mc,int mu_index){
	if (mc.get_ID() != get_mcID<RVHP>(n) || mu_index != get_mu_index<RVHP>(n) ){
//		d_OLHA_p->set_mu(mu_index);
		set_mu_helper<RVHP>(d_OLHA_p,mu_index);
		set_value(n, d_OLHA_p->eval(mc,Index_Vector(d_index_vectors[n])));
		set_conj_value(n, d_OLHA_p->get_conjugate_amplitude_VHP());

		set_tree_value(n, d_OLHA_p->get_tree(mc,d_index_vectors[n]));
		set_accuracy(n, d_OLHA_p->get_accuracy());
		set_mcID<RVHP>(n, mc.get_ID()) ;
		set_mu_index<RVHP>(n, mu_index) ;
	}
	return get_value<RVHP>(n);
}

#ifdef BH_USE_GMP
SeriesC<RGMP> Cached_OLHA::eval(size_t n, momentum_configuration<RGMP>& mc,int mu_index){
	if (mc.get_ID() != get_mcID<RGMP>(n) || mu_index != get_mu_index<RGMP>(n) ){
//		d_OLHA_p->set_mu(mu_index);
		set_mu_helper<RGMP>(d_OLHA_p,mu_index);
		set_value(n, d_OLHA_p->eval(mc,Index_Vector(d_index_vectors[n])));
		set_conj_value(n, d_OLHA_p->get_conjugate_amplitude_GMP());

		set_tree_value(n, d_OLHA_p->get_tree(mc,d_index_vectors[n]));
		set_accuracy(n, d_OLHA_p->get_accuracy());
		set_mcID<RGMP>(n, mc.get_ID()) ;
		set_mu_index<RGMP>(n, mu_index) ;
	}
	return get_value<RGMP>(n);
}

#endif

SeriesC<R> Cached_OLHA::eval_conj(size_t n, momentum_configuration<R>& mc,int mu_index){
	if (mc.get_ID() != get_mcID<R>(n) || mu_index != get_mu_index<R>(n) ){
//		d_OLHA_p->set_mu(mu_index);
		set_mu_helper<R>(d_OLHA_p,mu_index);
		set_value(n, d_OLHA_p->eval(mc,Index_Vector(d_index_vectors[n])));
		set_conj_value(n, d_OLHA_p->get_conjugate_amplitude());

		set_tree_value(n, d_OLHA_p->get_tree(mc,d_index_vectors[n]));
		set_accuracy(n, d_OLHA_p->get_accuracy());
		set_mcID<R>(n, mc.get_ID()) ;
		set_mu_index<R>(n, mu_index) ;
	}
	return get_conj_value<R>(n);
}


SeriesC<RHP> Cached_OLHA::eval_conj(size_t n, momentum_configuration<RHP>& mc,int mu_index){
	if (mc.get_ID() != get_mcID<RHP>(n) || mu_index != get_mu_index<RHP>(n) ){
//		d_OLHA_p->set_mu(mu_index);
		set_mu_helper<RHP>(d_OLHA_p,mu_index);
		set_value(n, d_OLHA_p->eval(mc,Index_Vector(d_index_vectors[n])));
		set_conj_value(n, d_OLHA_p->get_conjugate_amplitude_HP());

		set_tree_value(n, d_OLHA_p->get_tree(mc,d_index_vectors[n]));
		set_accuracy(n, d_OLHA_p->get_accuracy());
		set_mcID<RHP>(n, mc.get_ID()) ;
		set_mu_index<RHP>(n, mu_index) ;
	}
	return get_conj_value<RHP>(n);
}

SeriesC<RVHP> Cached_OLHA::eval_conj(size_t n, momentum_configuration<RVHP>& mc,int mu_index){
	if (mc.get_ID() != get_mcID<RVHP>(n) || mu_index != get_mu_index<RVHP>(n) ){
//		d_OLHA_p->set_mu(mu_index);
		set_mu_helper<RVHP>(d_OLHA_p,mu_index);
		set_value(n, d_OLHA_p->eval(mc,Index_Vector(d_index_vectors[n])));
		set_conj_value(n, d_OLHA_p->get_conjugate_amplitude_VHP());

		set_tree_value(n, d_OLHA_p->get_tree(mc,d_index_vectors[n]));
		set_accuracy(n, d_OLHA_p->get_accuracy());
		set_mcID<RVHP>(n, mc.get_ID()) ;
		set_mu_index<RVHP>(n, mu_index) ;
	}
	return get_conj_value<RVHP>(n);
}

#ifdef BH_USE_GMP
SeriesC<RGMP> Cached_OLHA::eval_conj(size_t n, momentum_configuration<RGMP>& mc,int mu_index){
	if (mc.get_ID() != get_mcID<RGMP>(n) || mu_index != get_mu_index<RGMP>(n) ){
//		d_OLHA_p->set_mu(mu_index);
		set_mu_helper<RGMP>(d_OLHA_p,mu_index);
		set_value(n, d_OLHA_p->eval(mc,Index_Vector(d_index_vectors[n])));
		set_conj_value(n, d_OLHA_p->get_conjugate_amplitude_GMP());

		set_tree_value(n, d_OLHA_p->get_tree(mc,d_index_vectors[n]));
		set_accuracy(n, d_OLHA_p->get_accuracy());
		set_mcID<RGMP>(n, mc.get_ID()) ;
		set_mu_index<RGMP>(n, mu_index) ;
	}
	return get_conj_value<RGMP>(n);
}
#endif


template <class T> std::complex<T> Cached_OLHA::eval_tree(size_t n, momentum_configuration<T>& mc,int mu_index){
	if (mc.get_ID() != get_mcID<T>(n) || mu_index != get_mu_index<T>(n) ){
//		d_OLHA_p->set_mu(mu_index);
		set_mu_helper<T>(d_OLHA_p,mu_index);
		set_value(n, d_OLHA_p->eval(mc,Index_Vector(d_index_vectors[n])));
		set_accuracy(n, d_OLHA_p->get_accuracy());
		set_tree_value(n, d_OLHA_p->get_tree(mc,d_index_vectors[n]));
		set_mcID<T>(n, mc.get_ID()) ;
		set_mu_index<T>(n, mu_index) ;
	}
	return get_tree_value<T>(n);
}

void Cached_OLHA::dry_run(){
	for (int n=0;n<d_index_vectors.size();n++){
		d_OLHA_p->dry_run(d_index_vectors[n]);
	}
}

const process& Cached_OLHA::get_process() const {
	return d_OLHA_p->get_process();
}
color_structure Cached_OLHA::get_color_structure() const {
	return d_OLHA_p->color_struct();
}

template <class key,class T> struct do_delete_second : public std::unary_function<pair<key,T>&,void> {
	void operator()(pair<key,T>& p) { delete p.second;}
};

bool operator<(const pro_cs& pc1,const pro_cs& pc2){
	if (pc1.get_process()< pc2.get_process()) return true;
	if (pc2.get_process()< pc1.get_process()) return false;

	if (pc1.get_color_structure()< pc2.get_color_structure()) return true;
	if (pc2.get_color_structure()< pc1.get_color_structure()) return false;

return false;
}

template <class OLHA> Cached_OLHA_factory_impl<OLHA>::~Cached_OLHA_factory_impl(){
	for_each(d_amplitudes.begin(),d_amplitudes.end(),do_delete_second<const pro_cs,Cached_OLHA*>());

}

template <class OLHA > void Cached_OLHA_factory_impl<OLHA>::print_state(){
	_MESSAGE("=-=-=-=-=-=-=-=-=-=-= Cached_OLHA_factory =-=-=-=-=-=-=-=-=-=-= " );
	for ( map<const pro_cs,Cached_OLHA*>::iterator it=d_amplitudes.begin();it!=d_amplitudes.end();it++){
		(*it).second->print_stat();
	};
	_MESSAGE("=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= " );
}

template <class OLHA > void Cached_OLHA_factory_impl<OLHA>::fill_process_list(std::vector<std::pair<process,color_structure> >& li){
	for ( map<const pro_cs,Cached_OLHA*>::iterator it=d_amplitudes.begin();it!=d_amplitudes.end();it++){
		li.push_back( std::make_pair((*it).second->get_process(),(*it).second->get_color_structure() ) );
	};
}



Cached_OLHA_user_normal::~Cached_OLHA_user_normal(){};
Cached_OLHA_user_conjugate::~Cached_OLHA_user_conjugate(){};


void flip_helicity(process & pro){
       vector<particle_ID> v_pro;
       for(size_t i=0;i<pro.n();i++) v_pro.push_back(pro[i].helicity_conjugate());
       pro=process(v_pro);
       return;
}

    
    /* switch: - conj==0:  no conjugation use normal user
     *         - conj==-1: conjugation with sign, use conjugate user and inset sign
     *         - conj==1: conjugation without sign, use conjugate user no sign
     */

template <class OLHA> Cached_OLHA_user* Cached_OLHA_factory_impl<OLHA>::new_OLHA(const process& pro_in,color_structure cs,const std::vector<int>& ind, int conj){
	
    process pro(pro_in);

    //switch off conj for debugging 
    //conj=0;  
    
    bool conjQ(false);
	int sign;
	switch(conj){
         case 0: {conjQ=false; sign=0;} break;
         case 1: {conjQ=true; sign=1;} break;
         case -1: {conjQ=true; sign=-1;} break;
         default: _MESSAGE("conjugation not specified"); break;
     }

     //conjQ=false; //switch of conjugation for debugging

     //helicity conjugate particle IDs
     if(conjQ) flip_helicity(pro);
   
    
	pro_cs pc(pro,cs);
	map<pro_cs,Cached_OLHA*>::iterator it = d_amplitudes.find(pc);
	if (it == d_amplitudes.end()){
		OneLoopAmplitude_base* new_OLHAb=new OLHA(pro,cs);
		Cached_OLHA* new_COLHA = new Cached_OLHA(new_OLHAb);
		d_amplitudes.insert(pair<pro_cs,Cached_OLHA*>(pc,new_COLHA));
		if(conjQ) return new Cached_OLHA_user_conjugate(new_COLHA,new_COLHA->add(ind),sign);
        else return (Cached_OLHA_user*)(Cached_OLHA_user_normal(new_COLHA,new_COLHA->add(ind)));
	} else {
        if(conjQ) return new Cached_OLHA_user_conjugate((*it).second,(*it).second->add(ind),sign);
        else return (Cached_OLHA_user*)(new Cached_OLHA_user_normal((*it).second,(*it).second->add(ind)));
	}

}

template <> Cached_OLHA_user* Cached_OLHA_factory_impl<IR_checked_OLHA>::new_OLHA(const process& pro_in,color_structure cs,const std::vector<int>& ind, int conj){
	
    process pro(pro_in);
     
    bool conjQ(false);
     int sign;
     switch(conj){
         case 0: {conjQ=false; sign=0;} break;
         case 1: {conjQ=true; sign=1;} break;
         case -1: {conjQ=true; sign=-1;} break;
     }

     //helicity conjugate particle IDs
     if(conjQ) flip_helicity(pro);
    
	pro_cs pc(pro,cs);
	map<pro_cs,Cached_OLHA*>::iterator it = d_amplitudes.find(pc);
	if (it == d_amplitudes.end()){

		OneLoopAmplitude_base* new_OLHAb;
//		switch (settings::general::s_rat_type){
//		case settings::general::recursive:
			new_OLHAb=new IR_checked_OLHA(pro,cs);//break;
//		case settings::general::ratext:
//			new_OLHAb=new IR_checked_OLHA_Ext(pro,cs);break;
//		}

		Cached_OLHA* new_COLHA = new Cached_OLHA(new_OLHAb);
		d_amplitudes.insert(pair<pro_cs,Cached_OLHA*>(pc,new_COLHA));
/*		return Cached_OLHA_user(new_COLHA,new_COLHA->add(ind));
	} else {
		return Cached_OLHA_user((*it).second,(*it).second->add(ind));
	}
*/
		if(conjQ){return new Cached_OLHA_user_conjugate(new_COLHA,new_COLHA->add(ind),sign);}
        else{return new Cached_OLHA_user_normal(new_COLHA,new_COLHA->add(ind));}
	} else {
        if(conjQ){return new Cached_OLHA_user_conjugate((*it).second,(*it).second->add(ind),sign);}
        else{ return new Cached_OLHA_user_normal((*it).second,(*it).second->add(ind));}
	}



}


template <> Cached_OLHA_user* Cached_OLHA_factory_impl<OneLoopHelAmpl>::new_OLHA(const process& pro_in,color_structure cs,const std::vector<int>& ind, int conj){
	
    process pro(pro_in);
    
     bool conjQ(false);
     int sign;
     switch(conj){
         case 0: {conjQ=false; sign=0;} break;
         case 1: {conjQ=true; sign=1;} break;
         case -1: {conjQ=true; sign=-1;} break;
     }
    
	pro_cs pc(pro,cs);
	map<pro_cs,Cached_OLHA*>::iterator it = d_amplitudes.find(pc);
	if (it == d_amplitudes.end()){

		OneLoopAmplitude_base* new_OLHAb;

		new_OLHAb=new One_Loop_Helicity_Amplitude(pro,cs);

		Cached_OLHA* new_COLHA = new Cached_OLHA(new_OLHAb);
		d_amplitudes.insert(pair<pro_cs,Cached_OLHA*>(pc,new_COLHA));

		if(conjQ){return new Cached_OLHA_user_conjugate(new_COLHA,new_COLHA->add(ind),sign);}
        else{return new Cached_OLHA_user_normal(new_COLHA,new_COLHA->add(ind));}
	} else {
        if(conjQ){return new Cached_OLHA_user_conjugate((*it).second,(*it).second->add(ind),sign);}
        else{ return new Cached_OLHA_user_normal((*it).second,(*it).second->add(ind));}
	}
}

template <class OLHA> void Cached_OLHA_factory_impl<OLHA>::refresh(momentum_configuration<R>& mc, int mu_index){
	for ( map<const pro_cs,Cached_OLHA*>::iterator it=d_amplitudes.begin();it!=d_amplitudes.end();it++){
		(*it).second->refresh(mc,mu_index);
	};
}



Cached_OLHA_factory_impl<OneLoopHelAmpl> global_COLHA_OLHA;
template <> Cached_OLHA_factory* Cached_OLHA_factory_impl<OneLoopHelAmpl>::d_global_p= &global_COLHA_OLHA;

Cached_OLHA_factory_impl<IR_checked_OLHA> global_COLHA_IR;
template <> Cached_OLHA_factory* Cached_OLHA_factory_impl<IR_checked_OLHA>::d_global_p= &global_COLHA_IR;

template class Cached_OLHA_factory_impl<OneLoopHelAmpl>;
template class Cached_OLHA_factory_impl<IR_checked_OLHA>;

Cached_OLHA_factory* Cached_OLHA_factory::default_COLHA= &global_COLHA_OLHA;

void use_IR_checked(){
	Cached_OLHA_factory::default_COLHA= Cached_OLHA_factory_impl<IR_checked_OLHA>::d_global_p;
}
void use_OneLoopHelAmpl(){
	Cached_OLHA_factory::default_COLHA= Cached_OLHA_factory_impl<OneLoopHelAmpl>::d_global_p;
}


//template SeriesC<R> Cached_OLHA_user::eval(momentum_configuration<R>& mc,int mu_index);
//template SeriesC<RHP> Cached_OLHA_user::eval(momentum_configuration<RHP>& mc,int mu_index);
//template SeriesC<RVHP> Cached_OLHA_user::eval(momentum_configuration<RVHP>& mc,int mu_index);


}

}


