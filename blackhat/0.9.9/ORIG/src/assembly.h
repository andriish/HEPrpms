/*
 * assembly.h
 *
 *  Created on: Aug 25, 2008
 *      Author: daniel
 */

#ifndef ASSEMBLY_H_
#define ASSEMBLY_H_


#include "amplitudes.h"
#include "OneLoopHelAmpl.h"
#include "constants.h"
#include "BH_typedefs.h"
#include "cached_THA.h"
#include "cached_OLHA.h"
#include <map>
#include <algorithm>

namespace BH {

enum approx{born=1,virt};

class settings_table;
class Cached_THA_factory;
class Cached_THA_user;
class Cached_THA_user_normal;
class Cached_THA_user_conjugate;
class Cached_OLHA_factory;
class Cached_OLHA_user;

class kinematic_function {
public:
	//virtual std::complex<R> eval(momentum_configuration<R>&,const vector<int>& ind ) =0;
	//virtual std::complex<RHP> eval(momentum_configuration<RHP>&,const vector<int>& ind ) =0;
	//virtual std::complex<RVHP> eval(momentum_configuration<RVHP>&,const vector<int>& ind ) =0;
	virtual std::complex<R> eval(momentum_configuration<R>&) =0;
	virtual std::complex<RHP> eval(momentum_configuration<RHP>&) =0;
	virtual std::complex<RVHP> eval(momentum_configuration<RVHP>&) =0;
	virtual kinematic_function* clone() const = 0;
	virtual ~kinematic_function(){};
};

class constant_kinematic_function : public kinematic_function {
	multi_precision_constant m_cst;
public:
	constant_kinematic_function(multi_precision_constant cst): m_cst(cst){};
	//virtual std::complex<R> eval(momentum_configuration<R>&,const vector<int>& ind ) {return std::complex<R>(m_cst,0);};
	//virtual std::complex<RHP> eval(momentum_configuration<RHP>&,const vector<int>& ind ) {return std::complex<RHP>(m_cst,0);};
	//virtual std::complex<RVHP> eval(momentum_configuration<RVHP>&,const vector<int>& ind ){return std::complex<RVHP>(m_cst,0);};
	virtual std::complex<R> eval(momentum_configuration<R>&) {return std::complex<R>(m_cst,0);};
	virtual std::complex<RHP> eval(momentum_configuration<RHP>&) {return std::complex<RHP>(m_cst,0);};
	virtual std::complex<RVHP> eval(momentum_configuration<RVHP>&){return std::complex<RVHP>(m_cst,0);};
	virtual kinematic_function* clone() const {return new constant_kinematic_function(m_cst);};
	virtual ~constant_kinematic_function(){};
};

class prop_fn : public kinematic_function {
	multi_precision_constant m_Q;
	multi_precision_constant m_P;
	multi_precision_constant m_M;
	multi_precision_constant m_G;
	int m_i;
	int m_j;
public:
	prop_fn(multi_precision_constant Q,multi_precision_constant P,multi_precision_constant M,multi_precision_constant G,int i, int j): m_Q(Q), m_P(P),m_M(M),m_G(G), m_i(i), m_j(j) {};
	//virtual std::complex<R> eval(momentum_configuration<R>& mc,const vector<int>& ind ) {return eval_fn(mc);};
	//virtual std::complex<RHP> eval(momentum_configuration<RHP>& mc,const vector<int>& ind ) {return eval_fn(mc);};
	//virtual std::complex<RVHP> eval(momentum_configuration<RVHP>& mc,const vector<int>& ind ){return eval_fn(mc);};
	virtual std::complex<R> eval(momentum_configuration<R>& mc) {return eval_fn(mc);};
	virtual std::complex<RHP> eval(momentum_configuration<RHP>& mc) {return eval_fn(mc);};
	virtual std::complex<RVHP> eval(momentum_configuration<RVHP>& mc){return eval_fn(mc);};
	virtual kinematic_function* clone() const {return new prop_fn(m_Q, m_P, m_M, m_G, m_i, m_j);};
	virtual ~prop_fn(){};
private:
	template <class T> std::complex<T> eval_fn(momentum_configuration<T>& mc){std::complex<T> s=mc.s(m_i,m_j);return -std::complex<T>(m_Q,0)+std::complex<T>(m_P,0)*s/(s-std::complex<T>(m_M*m_M,0)+std::complex<T>(0,m_G*m_M));}
};



class prop_hel_fn : public kinematic_function {
	R m_hel_coupling;
	int m_Q; //[-1,2] for [down,up]
	int m_i;
	int m_j;
	int m_photonZW; //[0,1,2] corresponds to [photon,photon&Z,W]
	bool m_up_down_quark; //[0,1] corresponds to [up,down]
	int m_leading_vect_ax; //[0,1,2] corresponds to [leading,vectorial,axial]
	std::vector<ph_type> m_ql_ph_type;
public:
	prop_hel_fn(bool up_down_quark,int photonZW, int leading_vect_ax, int i, int j, const std::vector<ph_type> ql_ph_type);
	//virtual std::complex<R> eval(momentum_configuration<R>& mc,const vector<int>& ind ) {return eval_fn(mc,ind);};
	//virtual std::complex<RHP> eval(momentum_configuration<RHP>& mc,const vector<int>& ind ) {return eval_fn(mc,ind);};
	//virtual std::complex<RVHP> eval(momentum_configuration<RVHP>& mc,const vector<int>& ind ){return eval_fn(mc,ind);};
	virtual std::complex<R> eval(momentum_configuration<R>& mc) {return eval_fn(mc);};
	virtual std::complex<RHP> eval(momentum_configuration<RHP>& mc) {return eval_fn(mc);};
	virtual std::complex<RVHP> eval(momentum_configuration<RVHP>& mc){return eval_fn(mc);};
	virtual kinematic_function* clone() const {return new prop_hel_fn(m_up_down_quark, m_photonZW, m_leading_vect_ax, m_i, m_j, m_ql_ph_type);};
	bool selection_rule_is_zero() {return (0.==m_hel_coupling && m_photonZW!=0);};
	virtual ~prop_hel_fn(){};
private:
	template <class T> std::complex<T> eval_fn(momentum_configuration<T>& mc);
};


class prop_hel_fn_diphoton : public kinematic_function {
	int m_Q2; //[1,4] for [down^2,up^2]
public:
	prop_hel_fn_diphoton(int quark_pdb_label);
	virtual std::complex<R> eval(momentum_configuration<R>& mc) {return eval_fn(mc);};
	virtual std::complex<RHP> eval(momentum_configuration<RHP>& mc) {return eval_fn(mc);};
	virtual std::complex<RVHP> eval(momentum_configuration<RVHP>& mc){return eval_fn(mc);};
	virtual kinematic_function* clone() const {return new prop_hel_fn_diphoton(m_Q2);};
	bool selection_rule_is_zero() {return false;};
private:
	template <class T> std::complex<T> eval_fn(momentum_configuration<T>& mc);
};


/*
class subtraction {
	TreeHelAmpl* _tree;
	multi_precision_fraction _factor;
	int _order;
public:
	subtraction(const process& pro,multi_precision_fraction mpf,int order): _tree(new TreeHelAmpl(pro)), _factor(mpf), _order(order) {}
	template <class T> SeriesC<T> eval(momentum_configuration<T>& mc,const std::vector<int>& ind);
	subtraction(const subtraction& sub): _tree(new TreeHelAmpl(sub._tree->get_process())), _factor(sub._factor), _order(sub._order){};
	subtraction& operator=(const subtraction& sub);
	~subtraction(){delete _tree;};
};
*/
class subtraction {
	CachedTHA::Cached_THA_user* _tree;
	//TreeHelAmpl* _tree;
	multi_precision_fraction _factor;
	int _order;
	const std::vector<int> _ind;
public:
	subtraction(const process& pro, const std::vector<int>& ind, multi_precision_fraction mpf,int order):
					_tree((*CachedTHA::Cached_THA_factory::default_CTHA).new_THA(pro,ind)), _factor(mpf), _order(order), _ind(ind) {}
	//template <class T> SeriesC<T> eval(momentum_configuration<T>& mc,const std::vector<int>& ind);
	template <class T> SeriesC<T> eval(momentum_configuration<T>& mc);
	//subtraction(const subtraction& sub): _tree((*CachedTHA::Cached_THA_factory::default_CTHA).new_THA(sub._tree.get_process(),sub._tree.get_index_vector())), _factor(sub._factor), _order(sub._order){};
	//subtraction& operator=(const subtraction& sub);
	//~subtraction(){delete &_tree;};
	//const std::vector<int>& get_ind(){return _ind;};
	~subtraction(){ delete _tree;};
};

class partial_amplitude_base {
protected:
	kinematic_function* m_prefactor;
	QCDorder m_lo_or_nlo;
public:
	partial_amplitude_base(QCDorder lo_or_nlo=nlo) : m_prefactor(0), m_lo_or_nlo(lo_or_nlo) {};
	virtual ~partial_amplitude_base(){};
//	virtual partial_amplitude_base* clone()=0;
	virtual void add(const process& pro, color_structure cs, const std::vector<int>& ind,int num, int den)=0;
//	virtual void add(const subtraction& sub,const std::vector<int>& ind)=0;
	//virtual void add(const subtraction& sub)=0;
	virtual void add_subtraction(const process& pro, const std::vector<int>& ind, multi_precision_fraction mpf,int order)=0;
	virtual SeriesC<R> eval(momentum_configuration<R>& mc, const std::vector<int>& ind,int mu_index)=0;
	virtual SeriesC<RHP> eval(momentum_configuration<RHP>& mc, const std::vector<int>& ind,int mu_index)=0;
	virtual SeriesC<RVHP> eval(momentum_configuration<RVHP>& mc, const std::vector<int>& ind,int mu_index)=0;
	virtual void set_prefactor(const kinematic_function& kf){ delete m_prefactor;  m_prefactor=kf.clone(); };
	virtual void dry_run(const std::vector<int>&)=0;
	//the copy and assignments are expensive, we restrict their use.
private:
	partial_amplitude_base(const partial_amplitude_base&);
	partial_amplitude_base& operator=(const partial_amplitude_base&);
};
namespace CachedOLHA {

class partial_amplitude_cached : public partial_amplitude_base {
	std::vector<CachedOLHA::Cached_OLHA_user*> d_ampls;
	std::vector<multi_precision_fraction> d_factors;
	std::vector<R> d_factors_R;
	std::vector<std::vector<int> > d_indices;
	std::vector<std::vector<int> > d_subs_indices;
	std::vector<subtraction*> d_subs;
public:
	partial_amplitude_cached(QCDorder lo_or_nlo=nlo) : partial_amplitude_base(lo_or_nlo) {};
	virtual ~partial_amplitude_cached();

	virtual void add(const process& pro, color_structure cs, const std::vector<int>& ind,int num, int den);
	virtual void add(const process& pro, color_structure cs, const std::vector<int>& ind, R weight);
	virtual void add_subtraction(const process& pro, const std::vector<int>& ind, multi_precision_fraction mpf,int order);
	virtual SeriesC<R> eval(momentum_configuration<R>& mc, const std::vector<int>& ind,int mu_index){return eval_fn(mc,ind,mu_index);};
	virtual SeriesC<RHP> eval(momentum_configuration<RHP>& mc, const std::vector<int>& ind,int mu_index){return eval_fn(mc,ind,mu_index);};
	virtual SeriesC<RVHP> eval(momentum_configuration<RVHP>& mc, const std::vector<int>& ind,int mu_index){return eval_fn(mc,ind,mu_index);};
	virtual void dry_run(const std::vector<int>&);
private:
	//the copy and assignments are expensive, we restrict their use.
	partial_amplitude_cached(const partial_amplitude_cached&);
	partial_amplitude_cached& operator=(const partial_amplitude_cached&);
	template <class T> SeriesC<T> eval_fn(momentum_configuration<T>& mc, const std::vector<int>& ind,int mu_index);

};
}






/*
class partial_amplitude : public partial_amplitude_base {
	std::vector<OneLoopHelAmpl*> ampls;
	std::vector<multi_precision_fraction> factors;
	std::vector<std::vector<int> > indices;
	std::vector<std::vector<int> > subs_indices;
	std::vector<subtraction*> _subs;
public:
	partial_amplitude() : partial_amplitude_base() {};
	virtual ~partial_amplitude();
//	virtual partial_amplitude_base* clone();

	virtual void add(const process& pro, color_structure cs, const std::vector<int>& ind,int num, int den);
	virtual void add(const subtraction& sub,const std::vector<int>& ind);
	virtual SeriesC<R> eval(momentum_configuration<R>& mc, const std::vector<int>& ind,int mu_index){return eval_fn(mc,ind,mu_index);};
	virtual SeriesC<RHP> eval(momentum_configuration<RHP>& mc, const std::vector<int>& ind,int mu_index){return eval_fn(mc,ind,mu_index);};
	virtual SeriesC<RVHP> eval(momentum_configuration<RVHP>& mc, const std::vector<int>& ind,int mu_index){return eval_fn(mc,ind,mu_index);};
	virtual void dry_run(const vector<int>&);
	//the copy and assignments are expensive, we restrict their use.
private:
	partial_amplitude(const partial_amplitude&);
	partial_amplitude& operator=(const partial_amplitude&);
	template <class T> SeriesC<T> eval_fn(momentum_configuration<T>& mc, const std::vector<int>& ind,int mu_index);
};
*/

class Tree_with_prefactor : public TreeHelAmpl {
	kinematic_function* m_prefactor;
public:
	Tree_with_prefactor(const process& pro,const kinematic_function& kf): TreeHelAmpl(pro)  {m_prefactor=kf.clone();};
	virtual std::complex<R> eval(momentum_configuration<R>& mc,const std::vector<int>& ind){return m_prefactor->eval(mc)*pointee()->eval(mc,ind);};
	virtual std::complex<RHP> eval(momentum_configuration<RHP>& mc,const std::vector<int>& ind){return m_prefactor->eval(mc)*pointee()->eval(mc,ind);};
	virtual std::complex<RVHP> eval(momentum_configuration<RVHP>& mc,const std::vector<int>& ind){return m_prefactor->eval(mc)*pointee()->eval(mc,ind);};
	virtual ~Tree_with_prefactor() {delete m_prefactor;};
	Tree_with_prefactor(const Tree_with_prefactor& t): TreeHelAmpl(t.get_process()) {m_prefactor=t.m_prefactor->clone();};
	Tree_with_prefactor& operator=(const Tree_with_prefactor& t){TreeHelAmpl::operator=(t); delete m_prefactor;m_prefactor=t.m_prefactor->clone();return *this;};

};



class CTree_with_prefactor {
	CachedTHA::Cached_THA_user*  m_Cached_THA_user;
	kinematic_function* m_prefactor;
	std::vector<int> m_ind;
public:
	CTree_with_prefactor(const process& pro, const std::vector<int>& ind ,const kinematic_function& kf):
	  m_Cached_THA_user((*CachedTHA::Cached_THA_factory::default_CTHA).new_THA(pro,ind)),m_ind(ind) {m_prefactor=kf.clone();};
	CTree_with_prefactor(const process& pro, const std::vector<int>& ind);
	virtual std::complex<R> eval(momentum_configuration<R>& mc){return m_prefactor->eval(mc)*(m_Cached_THA_user->eval(mc));};
	virtual std::complex<RHP> eval(momentum_configuration<RHP>& mc){return m_prefactor->eval(mc)*(m_Cached_THA_user->eval(mc));};
	virtual std::complex<RVHP> eval(momentum_configuration<RVHP>& mc){return m_prefactor->eval(mc)*(m_Cached_THA_user->eval(mc));};
	virtual ~CTree_with_prefactor() {delete m_prefactor;delete m_Cached_THA_user;};
	const std::vector<int>& get_index_vector(){return m_ind;};
};



struct cross_term {
	size_t _ind_1;
	size_t _ind_2;
	multi_precision_fraction _factor;
	cross_term(size_t l, size_t t,const multi_precision_fraction& factor):_ind_1(l), _ind_2(t), _factor(factor){};
};

struct cross_term_md : public cross_term {
	double d_prefactor;
	cross_term_md(size_t l, size_t t,const multi_precision_fraction& factor,double d): cross_term(l,t,factor), d_prefactor(d)  {};
};


struct cached_cross_term_md : public cross_term {
	double d_prefactor;
	cached_cross_term_md(size_t l, size_t t,const multi_precision_fraction& factor,double d): cross_term(l,t,factor), d_prefactor(d)  {};
	cached_cross_term_md(size_t l, size_t t,const multi_precision_fraction& factor): cross_term(l,t,factor), d_prefactor(1.)  {};
};



struct Ampl_Info {
    std::vector<int> m_perm, m_hels;
    double* real;
    double* imag;
    Ampl_Info(const process& pro, const vector<int>& ind,double * real, double * imag);
};

class Squared_ME 
#if BH_USE_OMP
	: private Tools::HasOMPLock
#endif
{
	std::vector<partial_amplitude_base*> _one_loops;
	std::vector<TreeHelAmpl*> _trees;
	std::vector<CTree_with_prefactor*> _ctrees_with_prefactor;
	std::vector<std::vector<int> > _tree_ind;
	std::vector<cross_term> _cross_terms;
	std::vector<cross_term> _cross_terms_tree;
	std::vector<cross_term_md> _cross_terms_md;
	std::vector<cross_term_md> _cross_terms_tree_md;
	std::vector<cached_cross_term_md> _cached_cross_terms_md;
	std::vector<cached_cross_term_md> _cached_cross_terms_tree_md;
	size_t d_nbr_external;
    R _couplings;
	QCDorder d_lo_or_nlo;
public:
	Squared_ME(QCDorder lo_or_nlo = nlo): d_nbr_external(0),d_lo_or_nlo(lo_or_nlo) {};
	size_t add(partial_amplitude_base* PA){
		if(d_lo_or_nlo==nlo){_one_loops.push_back(PA);return _one_loops.size()-1;}
		else return 0;};
    //new_begin
    //virtual size_t add(const process&, const std::vector<int>&, short conjQ, kinematic_function* kf=0){return 0;};
    //virtual size_t add(const process&, const std::vector<int>&, color_structure, short conjQ, kinematic_function* kf=0){return 0;};
    virtual size_t add(const process&, const std::vector<int>&, short conjQ, std::vector<kinematic_function*> kf){return 0;};
    virtual size_t add(const process&, const std::vector<int>&, color_structure, short conjQ, std::vector<kinematic_function*> kf){return 0;};
	virtual size_t add_partial(std::vector<std::pair<int,double > >& ){return 0;};
    //new_end
	size_t add(TreeHelAmpl* t,const std::vector<int>& ind){_trees.push_back(t);_tree_ind.push_back(ind);d_nbr_external=ind.size();return _trees.size()-1;}
	size_t add(Tree_with_prefactor* t,const std::vector<int>& ind){_trees.push_back(t);_tree_ind.push_back(ind);d_nbr_external=ind.size();return _trees.size()-1;}
	size_t add(CTree_with_prefactor* CTwp,const std::vector<int>& ind){_ctrees_with_prefactor.push_back(CTwp);_tree_ind.push_back(ind);d_nbr_external=ind.size();return _ctrees_with_prefactor.size()-1;}
	size_t add(CTree_with_prefactor* CTwp); //{_ctrees_with_prefactor.push_back(CTwp);_tree_ind.push_back((*CTwp).m_ind);d_nbr_external=ind.size();return _ctrees_with_prefactor.size()-1;}
	void add_loop(const cross_term& t){if(d_lo_or_nlo==nlo){_cross_terms.push_back(t);};}
	void add_loop(const cross_term_md& t){if(d_lo_or_nlo==nlo){_cross_terms_md.push_back(t);};}
	void add_loop(const cached_cross_term_md& t){if(d_lo_or_nlo==nlo){_cached_cross_terms_md.push_back(t);};}
    //new_begin
    virtual void add_tree(int,int,double){};
    virtual void complete_construction(){};
    virtual void complete_virt_construction(){};
    virtual void add_loop(int,int,double){};
    //new_end
	void add_tree(const cross_term& t){_cross_terms_tree.push_back(t);}
	void add_tree(const cross_term_md& t){_cross_terms_tree_md.push_back(t);};
	//void add_tree(const cached_cross_term_md& t){_cached_cross_terms_tree_md.push_back(t);}
	virtual void add_tree(const cached_cross_term_md& t){_cached_cross_terms_tree_md.push_back(t);};
	size_t get_nbr_external(){return d_nbr_external;};
	
    //template <class T> SeriesC<T> eval(momentum_configuration<T>& mc, const std::vector<int>& ind,int mu_index);
    virtual SeriesC<R> eval(momentum_configuration<R>& mc, const std::vector<int>& ind,int mu_index){return eval_fn(mc,ind,mu_index);};
    virtual SeriesC<RHP> eval(momentum_configuration<RHP>& mc, const std::vector<int>& ind,int mu_index){return eval_fn(mc,ind,mu_index);};
    virtual SeriesC<RVHP> eval(momentum_configuration<RVHP>& mc, const std::vector<int>& ind,int mu_index){return eval_fn(mc,ind,mu_index);};
	
    //template <class T> std::complex<T> eval_tree(momentum_configuration<T>& mc, const std::vector<int>& ind);
	virtual std::complex<R> eval_tree(momentum_configuration<R>& mc, const std::vector<int>& ind){return eval_tree_fn(mc,ind);};
	virtual std::complex<RHP> eval_tree(momentum_configuration<RHP>& mc, const std::vector<int>& ind){return eval_tree_fn(mc,ind);}
	virtual std::complex<RVHP> eval_tree(momentum_configuration<RVHP>& mc, const std::vector<int>& ind){return eval_tree_fn(mc,ind);}
	//
    virtual void set_partial_born(){return;};
    virtual void set_couplings(R couplings){_couplings=couplings;};
    virtual vector<Ampl_Info* >  get_partial_born(){ return vector<Ampl_Info* >(); };
    virtual int add(Ampl_Info* ampl_info){return 0;}
    // 
	virtual ~Squared_ME();
	void dry_run(const std::vector<int>& ind);
	//the copy and assignments are expensive, we restrict their use.
private:
	Squared_ME(const Squared_ME& sm);
	Squared_ME& operator=(const Squared_ME& sm);
protected:	
    template <class T> std::complex<T> eval_tree_fn(momentum_configuration<T>& mc, const std::vector<int>& ind);
    template <class T> SeriesC<T> eval_fn(momentum_configuration<T>& mc, const std::vector<int>& ind,int mu_index);
};

/*
struct Tree_and_prefactor{
    CachedTHA::Cached_THA_user* m_C_THA_user;
    kinematic_function* m_kin_fn;
    Tree_and_prefactor(CachedTHA::Cached_THA_user* C_THA_user,kinematic_function* kin_fn): m_C_THA_user(C_THA_user), m_kin_fn(kin_fn){};
};
*/


class Squared_ME_Struct: public Squared_ME {
   
    //------------------------------
    //for born matrix element
    std::map< std::pair<process,std::vector<int> >, int > map_tree_users;
    std::vector<std::complex<R> > m_tree_vals;
    //std::vector<std::complex<R>* > m_ptr_tree_vals;
    std::complex<R>** m_ptr_tree_vals;
    std::vector<CachedTHA::Cached_THA_user* > m_tree_users;
    
    std::map<kinematic_function*,int > map_kin_fn; 
    std::vector<kinematic_function*> m_kin_fn; 
    std::vector<std::complex<R> > m_kin_fn_vals;
    
    //vector< pair< pos-m_kin_fn_vals, pos-m_tree_vals > >
    std::vector<std::pair<std::vector<int>,int> > m_trees; 
    //std::map<std::pair< kn_fn, kn_fn >, std::map<tree-left  ,std::map<tree-right,pre-factor> > > m_cross_terms;
    std::map<std::pair<vector<int>,vector<int> >, std::map<int,std::map<int,double> > > m_cross_terms;
    //iterators
    std::map<std::pair<vector<int>,vector<int> >, std::map<int,std::map<int,double> > >::const_iterator it_ct;
    std::map<int,std::map<int,double> >::const_iterator it_lt;
    std::map<int,double>::const_iterator it_rtp;
    //
    //rewrite into arrays
    std::vector<std::vector<std::complex<R>*> > v_kin_fn_vals_l;
    std::vector<std::vector<std::complex<R>*> > v_kin_fn_vals_r;
    std::vector<std::vector<std::complex<R>* > > v_tree_vals_l;
    std::vector<std::vector<std::vector<double> > > v_color_factor;
    std::vector<std::vector<std::vector<std::complex<R>* > > > v_tree_vals_r;

    std::vector<std::complex<R>* >::const_iterator it_kin_fn_vals_l;
    std::vector<std::complex<R>* >::const_iterator it_kin_fn_vals_r;
    std::vector<std::complex<R>* >::const_iterator it_tree_vals_l;
    std::vector<std::complex<R>* >::const_iterator it_tree_vals_r;
    std::vector<double >::const_iterator it_color_factor;

    std::vector<double *> intfer_matrix;

    //------------------------------
    //for virtual matrix element
    std::map< std::pair< CachedOLHA::pro_cs, std::vector<int> >, int > map_loop_users;
    //std::map<   std::pair<color_structure, std::pair<process,std::vector<int> > >, int > map_loop_users;
    //std::map< std::vector<std::pair<int,double> >, int > map_loops_users_coeffs_in_partial;
    //void print();
    std::vector<std::complex<R> > m_loop_finite;
    std::vector<std::complex<R> > m_loop_single;
    std::vector<std::complex<R> > m_loop_double;
    std::vector<CachedOLHA::Cached_OLHA_user* > m_loop_users;
    //std::map<kinematic_function*,int > map_kin_fn; 
    //std::vector<kinematic_function*> m_kin_fn; 
    //std::vector<std::complex<R> > m_kin_fn_vals;
    //vector< pair< pos-m_kin_fn_vals, pos-m_tree_vals > >
    std::vector<std::pair<std::vector<int>,int> > m_loops; 
    //std::map<std::pair< kn_fn, kn_fn >, std::map<tree-left  ,std::map<tree-right,pre-factor> > > m_cross_terms;
    std::map<std::vector< std::pair<int,double> >, int > map_partial_ampls;
    std::vector<std::vector< std::pair<int,double> > > m_partial_ampls;
    std::map<std::pair<std::vector<int>,std::vector<int> >, std::map<int,std::map<int,double> > > m_loop_cross_terms;
    //iterators
    std::map<std::pair<int,int >, std::map<int,std::map<int,double> > >::const_iterator it_lct;//loop cross term
    std::map<int,std::map<int,double> >::const_iterator it_ll;//left loop
    std::map<int,double>::const_iterator it_rvtp;//right virtual tree prefactor

/////HERERER
    //rewrite into arrays
    std::vector<std::vector<std::complex<R>* > > vv_kin_fn_vals_l;
    std::vector<std::vector<std::complex<R>* > > vv_kin_fn_vals_r;
    std::vector<std::vector<std::complex<R>* > > vv_loop_finite_vals_l;
    std::vector<std::vector<std::complex<R>* > > vv_loop_single_vals_l;
    std::vector<std::vector<std::complex<R>* > > vv_loop_double_vals_l;
    std::vector<std::vector<std::vector<double> > > vv_color_factor;
    std::vector<std::vector<std::vector<std::complex<R>* > > > vv_tree_vals_r;

    std::vector<std::complex<R>* >::const_iterator itv_kin_fn_vals_l;
    std::vector<std::complex<R>* >::const_iterator itv_kin_fn_vals_r;
    std::vector<std::complex<R>* >::const_iterator itv_loop_finite_vals_l;
    std::vector<std::complex<R>* >::const_iterator itv_loop_single_vals_l;
    std::vector<std::complex<R>* >::const_iterator itv_loop_double_vals_l;
    std::vector<std::complex<R>* >::const_iterator itv_tree_vals_r;
    std::vector<double >::const_iterator itv_color_factor;

//    std::vector<double *> intfer_matrix;


    //------------------------------
    //for dipole subtraction terms
    std::vector<Ampl_Info* > partial_tree_ampls;
    std::vector<double > m_tree_vals_re;
    std::vector<double > m_tree_vals_im;
    //
    R m_couplings;
    int m_symmetry_factor;
    //m_kin_ps_tree_val[tree_pos]=kin_pos;
	std::vector<std::vector<int> > m_kin_ps_tree_val;

    
    public:
	Squared_ME_Struct(QCDorder lo_or_nlo = nlo): Squared_ME(lo_or_nlo), m_symmetry_factor(1) {}; 
    //virtual size_t add(const process&, const std::vector<int>&, short conjQ,kinematic_function* kf=0);
    virtual size_t add(const process&, const std::vector<int>&, short conjQ, std::vector<kinematic_function*> kf);
   
    virtual void add_tree(int,int,double);
    virtual void complete_construction();
    virtual void complete_virt_construction();
 
    virtual size_t add(const process&, const std::vector<int>&, color_structure, short conjQ, std::vector<kinematic_function*> kf);
	virtual size_t add_partial(std::vector< std::pair<int,double> >& );
    virtual void add_loop(int,int,double);
    
    
    //------------------------------
    // virtual eval functions
    virtual SeriesC<R> eval(momentum_configuration<R>& mc, const std::vector<int>& ind,int mu_index){return eval_fn(mc,ind,mu_index);};
    virtual SeriesC<RHP> eval(momentum_configuration<RHP>& mc, const std::vector<int>& ind,int mu_index){return eval_fn(mc,ind,mu_index);};
    virtual SeriesC<RVHP> eval(momentum_configuration<RVHP>& mc, const std::vector<int>& ind,int mu_index){return eval_fn(mc,ind,mu_index);};

    virtual std::complex<R> eval_tree(momentum_configuration<R>& mc, const std::vector<int>& ind){return eval_tree_fn(mc,ind);};
	virtual std::complex<RHP> eval_tree(momentum_configuration<RHP>& mc, const std::vector<int>& ind){return eval_tree_fn(mc,ind);}
	virtual std::complex<RVHP> eval_tree(momentum_configuration<RVHP>& mc, const std::vector<int>& ind){return eval_tree_fn(mc,ind);}
	
    
    //------------------------------
    //for controlling subtraction terms
    virtual void set_partial_born();
    virtual void set_couplings(R couplings){m_couplings=couplings;};
    virtual vector<Ampl_Info* >  get_partial_born(){
        return partial_tree_ampls ;
    };
    virtual int add(Ampl_Info* ampl_info){
        partial_tree_ampls.push_back(ampl_info); 
        return partial_tree_ampls.size();
    };
    //virtual vector<Ampl_Info* >  get_partial_born();
    //virtual int add(Ampl_Info* ampl_info); 
    //
    virtual ~Squared_ME_Struct();
   
    
    private:
    //------------------------------
    // eval functions
    template <class T> std::complex<T> eval_tree_fn(momentum_configuration<T>& mc, const std::vector<int>& ind);
    template <class T> SeriesC<T> eval_fn(momentum_configuration<T>& mc, const std::vector<int>& ind,int mu_index);
    template <class T> std::complex<T> eval_tree_vect_fn(momentum_configuration<T>& mc, const std::vector<int>& ind);

    CachedTHA::Cached_TA* d_Cached_TA;

};


class Virtual_SME {
	std::vector<Squared_ME*> _MEs;
	std::vector<int> d_index_vector;
    std::vector<Ampl_Info* > partial_tree_ampls;
    int m_order_qcd;
    int m_order_qed;
public:
	Virtual_SME(){};
	void add(Squared_ME* sm);
	virtual SeriesC<R> eval(momentum_configuration<R>& mc,R mu){ return eval_fn(mc,mu);};
	virtual SeriesC<RHP> eval(momentum_configuration<RHP>& mc,RHP mu){ return eval_fn(mc,mu);};
	virtual SeriesC<RVHP> eval(momentum_configuration<RVHP>& mc,RVHP mu){ return eval_fn(mc,mu);};
	virtual std::complex<R> eval_tree(momentum_configuration<R>& mc){ return eval_tree_fn(mc);};
	virtual std::complex<RHP> eval_tree(momentum_configuration<RHP>& mc){ return eval_tree_fn(mc);};
	virtual std::complex<RVHP> eval_tree(momentum_configuration<RVHP>& mc){ return eval_tree_fn(mc);};
    virtual void set_partial_born();
    virtual void set_couplings(R couplings);
    virtual void set_order(int order_qcd, int order_qed){m_order_qcd=order_qcd;m_order_qed=order_qed;};
    virtual int get_order_qcd(){return m_order_qcd;};
    virtual int get_order_qed(){return m_order_qed;};
    virtual void get_partial_born_map(
        vector<vector<int> >& permutation,
        vector<vector<int> >& helicity);
    virtual void get_vals_partial_born(
                vector<double* >& re_partial_born,
                vector<double* >& im_partial_born);
	virtual ~Virtual_SME();
	void dry_run();
//the copy and assignments are expensive, we restrict their use.
private:
	Virtual_SME(const Virtual_SME& sm);
	Virtual_SME& operator=(const Virtual_SME& sm);
protected:
	template <class T> SeriesC<T> eval_fn(momentum_configuration<T>& mc,T mu);
	template <class T> std::complex<T> eval_tree_fn(momentum_configuration<T>& mc);
};

class Virtual_SME_with_prefactor : public Virtual_SME {
	kinematic_function* d_prefactor_p;
public:
	Virtual_SME_with_prefactor(){};
	void add_amplitude_prefactor(kinematic_function* kin);//this factor multiplies VSM as a norm squared
	virtual SeriesC<R> eval(momentum_configuration<R>& mc,R mu){ return eval_fn(mc,mu);};
	virtual SeriesC<RHP> eval(momentum_configuration<RHP>& mc,RHP mu){ return eval_fn(mc,mu);};
	virtual SeriesC<RVHP> eval(momentum_configuration<RVHP>& mc,RVHP mu){ return eval_fn(mc,mu);};
	virtual std::complex<R> eval_tree(momentum_configuration<R>& mc){ return eval_tree_fn(mc);};
	virtual std::complex<RHP> eval_tree(momentum_configuration<RHP>& mc){ return eval_tree_fn(mc);};
	virtual std::complex<RVHP> eval_tree(momentum_configuration<RVHP>& mc){ return eval_tree_fn(mc);};
	virtual ~Virtual_SME_with_prefactor();
//the copy and assignments are expensive, we restrict their use.
private:
	template <class T> SeriesC<T> eval_fn(momentum_configuration<T>& mc,T mu);
	template <class T> std::complex<T> eval_tree_fn(momentum_configuration<T>& mc);
	Virtual_SME_with_prefactor(const Virtual_SME_with_prefactor& sm);
	Virtual_SME_with_prefactor& operator=(const Virtual_SME_with_prefactor& sm);
};

}


#endif /* ASSEMBLY_H_ */
