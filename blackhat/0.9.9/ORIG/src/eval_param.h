/*
 * eval_param.h
 *
 *  Created on: Apr 3, 2009
 *      Author: Darren Forde
 */

#ifndef EVAL_PARAM_H_
#define EVAL_PARAM_H_

#include "spinor.h"
#include "mom_conf.h"
#include "process.h"
#include "index_vector.h"
namespace BH {

// A class for storing information about the masses of particles
class mass_param{
private:
	C _original, _original_sqr;
	CHP _original_HP, _original_sqr_HP;
	CVHP _original_VHP, _original_sqr_VHP;
	C _actual, _actual_sqr;
	CHP _actual_HP, _actual_sqr_HP;
	CVHP _actual_VHP, _actual_sqr_VHP;
#ifdef BH_USE_GMP
	mutable CGMP _original_GMP, _original_sqr_GMP;
	mutable CGMP _actual_GMP, _actual_sqr_GMP;
#endif


	int _label;
	static int _next_mass_label; // A central location to give the next available mass label

	friend std::ostream& operator<<(std::ostream& ,const mass_param& );
public:
	mass_param();
	mass_param(const C mass, const CHP mass_HP, const CVHP mass_VHP);
	mass_param(const mass_param& mp);
	~mass_param() {};

	mass_param& operator=(const mass_param& mp);

	void set_dynamic(const CVHP& dyn) {_actual_VHP=_original_VHP+dyn; _actual_sqr_VHP=_actual_VHP*_actual_VHP;};
	void set_dynamic(const CHP& dyn) {_actual_HP=_original_HP+dyn; _actual_sqr_HP=_actual_HP*_actual_HP;};
	void set_dynamic(const C& dyn) {_actual=_original+dyn; _actual_sqr=_actual*_actual;};
#ifdef BH_USE_GMP
	void set_dynamic(const CGMP& dyn) {_actual_GMP=_original_GMP+dyn; _actual_sqr_GMP=_actual_GMP*_actual_GMP;};
#endif

	void set_dynamic2(const CVHP& dyn) {_actual_sqr_VHP=_original_sqr_VHP+dyn; _actual_VHP=sqrt(_actual_sqr_VHP);};
	void set_dynamic2(const CHP& dyn) {_actual_sqr_HP=_original_sqr_HP+dyn; _actual_HP=sqrt(_actual_sqr_HP);};
	void set_dynamic2(const C& dyn) {_actual_sqr=_original_sqr+dyn; _actual=sqrt(_actual_sqr);};
#ifdef BH_USE_GMP
	void set_dynamic2(const CGMP& dyn) {_actual_sqr_GMP=_original_sqr_GMP+dyn; _actual_GMP=sqrt(_actual_sqr_GMP);};
#endif

	void orig_mass(C& mass) const {mass=_original;};
	void orig_mass(CHP& mass) const {mass=_original_HP;};
	void orig_mass(CVHP& mass) const {mass=_original_VHP;};
	void orig_mass2(C& mass) const {mass=_original_sqr;};
	void orig_mass2(CHP& mass) const {mass=_original_sqr_HP;};
	void orig_mass2(CVHP& mass) const {mass=_original_sqr_VHP;};

	void mass(C& mass) const {mass=_actual;};
	void mass(CHP& mass) const {mass=_actual_HP;};
	void mass(CVHP& mass) const {mass=_actual_VHP;};
	void mass2(C& mass) const {mass=_actual_sqr;};
	void mass2(CHP& mass) const {mass=_actual_sqr_HP;};
	void mass2(CVHP& mass) const {mass=_actual_sqr_VHP;};

#ifdef BH_USE_GMP
	void orig_mass(CGMP& mass) const {mass=getGMPvalue(_original_GMP);};
	void orig_mass2(CGMP& mass) const {mass=getGMPvalue(_original_sqr_GMP);};

	void mass(CGMP& mass) const {mass=getGMPvalue(_actual_GMP);};
	void mass2(CGMP& mass) const {mass=getGMPvalue(_actual_sqr_GMP);};
#endif


	template <class T> const std::complex<T>& orig_mass() const {};
	template <class T> const std::complex<T>& orig_mass2() const {};

	template <class T> const std::complex<T>& mass() const {};
	template <class T> const std::complex<T>& mass2() const {};

	void reset() {
		_actual=_original; _actual_sqr=_original_sqr; _actual_HP=_original_HP; _actual_sqr_HP=_original_sqr_HP; _actual_VHP=_original_VHP; _actual_sqr_VHP=_original_sqr_VHP;
#ifdef BH_USE_GMP
		_actual_GMP=_original_GMP; _actual_sqr_GMP=_original_sqr_GMP;
#endif
		};

	int label() const {return _label;};

	static int set_next_mass_label(int next) {return mass_param::_next_mass_label=next;}; // Keeps track of how many entries we have made so do not make the mass_param_library too large
	static int get_next_mass_label() {return mass_param::_next_mass_label;}; // Keeps track of how many entries we have made so do not make the mass_param_library too large

#ifdef BH_USE_GMP
private:
	const CGMP& getGMPvalue(CGMP& value) const;
#endif
};


template <> inline const CVHP& mass_param::orig_mass() const {return _original_VHP;};
template <> inline const CHP& mass_param::orig_mass() const {return _original_HP;};
template <> inline const C& mass_param::orig_mass() const {return _original;};

template <> inline const CVHP& mass_param::orig_mass2() const {return _original_sqr_VHP;};
template <> inline const CHP& mass_param::orig_mass2() const {return _original_sqr_HP;};
template <> inline const C& mass_param::orig_mass2() const {return _original_sqr;};

template <> inline const CVHP& mass_param::mass() const {return _actual_VHP;};
template <> inline const CHP& mass_param::mass() const {return _actual_HP;};
template <> inline const C& mass_param::mass() const {return _actual;};

template <> inline const CVHP& mass_param::mass2() const {return _actual_sqr_VHP;};
template <> inline const CHP& mass_param::mass2() const {return _actual_sqr_HP;};
template <> inline const C& mass_param::mass2() const {return _actual_sqr;};

#ifdef BH_USE_GMP
template <> inline const CGMP& mass_param::orig_mass() const {return getGMPvalue(_original_GMP);};
template <> inline const CGMP& mass_param::orig_mass2() const {return getGMPvalue(_original_sqr_GMP);};
template <> inline const CGMP& mass_param::mass() const {return getGMPvalue(_original_sqr_GMP);};
template <> inline const CGMP& mass_param::mass2() const {return getGMPvalue(_actual_sqr_GMP);};
#endif


// A class for storing the mass_params during evaluation
class mass_param_library{
private:
	std::vector<mass_param> _mass_params;

	friend std::ostream& operator<<(std::ostream& ,const mass_param_library& );
public:
	mass_param_library() {};
	mass_param_library(mass_param& mp);
	~mass_param_library() {};

	void set_dynamic(int label,const CVHP& dyn) {_mass_params[label].set_dynamic(dyn);};
	void set_dynamic(int label,const CHP& dyn) {_mass_params[label].set_dynamic(dyn);};
	void set_dynamic(int label,const C& dyn) {_mass_params[label].set_dynamic(dyn);};

	void set_dynamic2(int label,const CVHP& dyn) {_mass_params[label].set_dynamic2(dyn);};
	void set_dynamic2(int label,const CHP& dyn) {_mass_params[label].set_dynamic2(dyn);};
	void set_dynamic2(int label,const C& dyn) {_mass_params[label].set_dynamic2(dyn);};

	void orig_mass(int label,C& mass) const {_mass_params[label].orig_mass(mass);};
	void orig_mass(int label,CHP& mass) const {_mass_params[label].orig_mass(mass);};
	void orig_mass(int label,CVHP& mass) const {_mass_params[label].orig_mass(mass);};
	void orig_mass2(int label,C& mass) const {_mass_params[label].orig_mass2(mass);};
	void orig_mass2(int label,CHP& mass) const {_mass_params[label].orig_mass2(mass);};
	void orig_mass2(int label,CVHP& mass) const {_mass_params[label].orig_mass2(mass);};

	void mass(int label,C& mass) const {_mass_params[label].mass(mass);};
	void mass(int label,CHP& mass) const {_mass_params[label].mass(mass);};
	void mass(int label,CVHP& mass) const {_mass_params[label].mass(mass);};
	void mass2(int label,C& mass) const {_mass_params[label].mass2(mass);};
	void mass2(int label,CHP& mass) const {_mass_params[label].mass2(mass);};
	void mass2(int label,CVHP& mass) const {_mass_params[label].mass2(mass);};

	template <class T> const std::complex<T>& orig_mass(int label) const {return _mass_params[label].orig_mass<T>();};
	template <class T> const std::complex<T>& orig_mass2(int label) const {return _mass_params[label].orig_mass2<T>();};

	template <class T> const std::complex<T>& mass(int label) const {return _mass_params[label].mass<T>();};
	template <class T> const std::complex<T>& mass2(int label) const {return _mass_params[label].mass2<T>();};

	void add(mass_param& mp);
	mass_param& operator[] (const int i) {return _mass_params[i];};
	mass_param& p(const int i) {return _mass_params[i];};
	
	size_t size() {return _mass_params.size();};

	void reset(int label) {_mass_params[label].reset();};

#ifdef BH_USE_GMP
	void set_dynamic(int label,const CGMP& dyn) {_mass_params[label].set_dynamic(dyn);};
	void set_dynamic2(int label,const CGMP& dyn) {_mass_params[label].set_dynamic2(dyn);};
	void orig_mass(int label,CGMP& mass) const {_mass_params[label].orig_mass(mass);};
	void orig_mass2(int label,CGMP& mass) const {_mass_params[label].orig_mass2(mass);};
	void mass(int label,CGMP& mass) const {_mass_params[label].mass(mass);};
	void mass2(int label,CGMP& mass) const {_mass_params[label].mass2(mass);};
#endif


};

template <> inline const CVHP& mass_param_library::orig_mass(int label) const {return _mass_params[label].orig_mass<RVHP>();};
template <> inline const CHP& mass_param_library::orig_mass(int label) const {return _mass_params[label].orig_mass<RHP>();};
template <> inline const C& mass_param_library::orig_mass(int label) const {return _mass_params[label].orig_mass<R>();};

template <> inline const CVHP& mass_param_library::orig_mass2(int label) const {return _mass_params[label].orig_mass2<RVHP>();};
template <> inline const CHP& mass_param_library::orig_mass2(int label) const {return _mass_params[label].orig_mass2<RHP>();};
template <> inline const C& mass_param_library::orig_mass2(int label) const {return _mass_params[label].orig_mass2<R>();};

template <> inline const CVHP& mass_param_library::mass(int label) const {return _mass_params[label].mass<RVHP>();};
template <> inline const CHP& mass_param_library::mass(int label) const {return _mass_params[label].mass<RHP>();};
template <> inline const C& mass_param_library::mass(int label) const {return _mass_params[label].mass<R>();};

template <> inline const CVHP& mass_param_library::mass2(int label) const {return _mass_params[label].mass2<RVHP>();};
template <> inline const CHP& mass_param_library::mass2(int label) const {return _mass_params[label].mass2<RHP>();};
template <> inline const C& mass_param_library::mass2(int label) const {return _mass_params[label].mass2<R>();};

#ifdef BH_USE_GMP
template <> inline const CGMP& mass_param_library::orig_mass(int label) const {return _mass_params[label].orig_mass<RGMP>();};
template <> inline const CGMP& mass_param_library::orig_mass2(int label) const {return _mass_params[label].orig_mass2<RGMP>();};
template <> inline const CGMP& mass_param_library::mass(int label) const {return _mass_params[label].mass<RGMP>();};
template <> inline const CGMP& mass_param_library::mass2(int label) const {return _mass_params[label].mass2<RGMP>();};
#endif



// A class for storing the mass_params
class mass_param_coll{
private:
	int* _em;
	int _size;

	friend std::ostream& operator<<(std::ostream& ,const mass_param_coll& );
public:
	mass_param_coll(int n) {_size=n; _em=new int[n];};
	mass_param_coll(const mass_param_coll& mpc) {_size=mpc.size(); _em=new int[_size]; for(int i=0;i<_size;i++){_em[i]=mpc.p(i);}};
	mass_param_coll(const process& pro) {_size=pro.n(); _em=new int[_size]; for(int i=0;i<_size;i++){_em[i]=pro.p(i+1).mass_label();}};
	~mass_param_coll() {delete[] _em;};

	int operator[] (const int i) const {return _em[i];};
	int p(const int i) const {return _em[i];};

	void set(int pos, int mass) {_em[pos]=mass;};
	int size() const {return _size;};
private:
	mass_param_coll& operator=(const mass_param_coll&);
};

// Stores the state of the eval_param computation
class eval_param_state {
private:
	std::vector<eval_param_state*> _eps_legs;

	int _size;

	unsigned long int _ep_ID;
	static unsigned long int eval_param_next_ID;

	unsigned short _state;
	unsigned long int _full_state;

	// Use these for comparing two eval_param_states, do not use to compare the state of the eval_param_state
	friend bool operator==(const eval_param_state& eps1, const eval_param_state& eps2);
	friend bool operator!=(const eval_param_state& eps1, const eval_param_state& eps2);
public:
	eval_param_state(int n) : _ep_ID(eval_param_next_ID), _state(0), _full_state(0), _size(n) {for(int i=0;i<_size;i++) {_eps_legs.push_back(this);}; eval_param_next_ID++;};
	~eval_param_state() {};

	// If we change elements in an eval_param we need to update its ID
	void renew_ID(){_ep_ID=eval_param_next_ID;eval_param_next_ID++;};

	// Change the eval_param_states for the different legs
	void set_leg_EPS(int leg, eval_param_state* eps) {_eps_legs[leg]=eps;};
	eval_param_state* get_leg_EPS(int leg) const {return _eps_legs[leg];};

	// Gets and sets the state of the eval_param_state
	unsigned short get_state() {return _state;};
	void toggle_state();

	// Gets the full state
	unsigned long int get_full_state() {return _full_state;};

	// Use this to check the state of the eval_param_state
	bool is_match(eval_param_state* eps);

	// Just return the eval_param ID for a very simple check
	unsigned long int get_ID() const {return _ep_ID;};

private:
	eval_param_state(const eval_param_state&){};
	eval_param_state& operator=(const eval_param_state&);
};

template <class T> class eval_param;
template <class T> std::ostream& operator<<(std::ostream& ,const eval_param<T>& );

template <class T> class eval_param {
	const Cmom<T>** _em;
	int _size;

	const Cmom<T>* _ref_vector; // Stores a pointer to the current massive quark reference vector, it usual points to _ep_quark_ref

	static mass_param_library _masses;

	static const Cmom<T> _ep_quark_ref; //The standard quark reference

	eval_param_state* _epstate;
#ifndef SWIG
	friend std::ostream& operator<< <T> (std::ostream& ,const eval_param<T>& );
#endif
public:
	eval_param(int n);
	eval_param(const vector<Cmom<T> >& vec) ;
	eval_param(momentum_configuration<T>& mc, std::vector<int>& ind);
	eval_param(momentum_configuration<T>& mc, const std::vector<int>& ind);
	~eval_param() {delete[] _em; delete _epstate;};
    void update(momentum_configuration<T>& mc, const std::vector<int>& ind);
	// If we change elements in an eval_param we need to update its state
	bool is_state_match(eval_param_state* epsc) {return _epstate->is_match(epsc);};
	eval_param_state* get_state() {return _epstate;};
	eval_param_state* get_leg_state(int leg) const {return _epstate->get_leg_EPS(leg);};
	void changed() {_epstate->toggle_state();}

	unsigned long int get_ID() const {return _epstate->get_ID();};
	void renew_ID(){_epstate->renew_ID();};

	const Cmom<T>* operator[] (const int i) {return _em[i];};
	const Cmom<T>* p(const int i) const {return _em[i];};
	const Cmom<T>& cmom(const int i) const {return *_em[i];};
	const momentum<std::complex<T> >& mom(const int i) const {return _em[i]->P();};

	int size() const {return _size;};
	const Cmom<T>** begin() const {return _em;};
	const Cmom<T>** back() const {return &_em[_size-1];};
	
	const Cmom<T>* ref() const {return _ref_vector;};

	void set(int pos, const Cmom<T>* insert_momentum) {_em[pos]=insert_momentum;};
	void set(int pos, const Cmom<T>* insert_momentum, eval_param_state* eps) {_em[pos]=insert_momentum; _epstate->set_leg_EPS(pos,eps);};
	void set_ref(const Cmom<T>* ref_vector) {_ref_vector=ref_vector;};
	static const Cmom<T>& get_default_ref(){return _ep_quark_ref;};

	// Evaluation functions
	std::complex<T> spa(const int i1,const int i2) const {return _em[i1]->L()*_em[i2]->L();};
	std::complex<T> spb(const int i1,const int i2) const {return _em[i1]->Lt()*_em[i2]->Lt();};
	std::complex<T> spab(const int i1,const int i2,const int i3) const {return _em[i1]->L()*_em[i2]->Sm()*_em[i3]->Lt();};
	std::complex<T> spba(const int i1,const int i2,const int i3) const {return _em[i1]->Lt()*_em[i2]->Sm()*_em[i3]->L();};
	std::complex<T> spaa(const int i1,const int i2,const int i3,const int i4) const {return _em[i1]->L()*_em[i2]->Sm()*_em[i3]->Sm()*_em[i4]->L();};
	std::complex<T> spbb(const int i1,const int i2,const int i3,const int i4) const {return _em[i1]->Lt()*_em[i2]->Sm()*_em[i3]->Sm()*_em[i4]->Lt();};
	std::complex<T> spab(const int i1,const int i2,const int i3,const int i4,const int i5) const {return _em[i1]->L()*_em[i2]->Sm()*_em[i3]->Sm()*_em[i4]->Sm()*_em[i5]->Lt();};
	std::complex<T> spba(const int i1,const int i2,const int i3,const int i4,const int i5) const {return _em[i1]->Lt()*_em[i2]->Sm()*_em[i3]->Sm()*_em[i4]->Sm()*_em[i5]->L();};
	std::complex<T> spaa(const int i1,const int i2,const int i3,const int i4,const int i5,const int i6) const {return _em[i1]->L()*_em[i2]->Sm()*_em[i3]->Sm()*_em[i4]->Sm()*_em[i5]->Sm()*_em[i6]->L();};
	std::complex<T> spbb(const int i1,const int i2,const int i3,const int i4,const int i5,const int i6) const {return _em[i1]->Lt()*_em[i2]->Sm()*_em[i3]->Sm()*_em[i4]->Sm()*_em[i5]->Sm()*_em[i6]->Lt();};
	std::complex<T> s(const int i1, const int i2) const {momentum<std::complex<T> > p=_em[i1]->P()+_em[i2]->P(); return p*p;};
	std::complex<T> s(const int i1, const int i2, const int i3) const {momentum<std::complex<T> > p=_em[i1]->P()+_em[i2]->P()+_em[i3]->P(); return p*p;};
	std::complex<T> s(const int i1, const int i2, const int i3, const int i4) const {momentum<std::complex<T> > p=_em[i1]->P()+_em[i2]->P()+_em[i3]->P()+_em[i4]->P(); return p*p;};
	std::complex<T> s(const int i1, const int i2, const int i3, const int i4, const int i5) const {momentum<std::complex<T> > p=_em[i1]->P()+_em[i2]->P()+_em[i3]->P()+_em[i4]->P()+_em[i5]->P(); return p*p;};
	std::complex<T> sp(const int i1,const int i2) const {return _em[i1]->P()*_em[i2]->P();};

	//Mass extraction functions
	static mass_param& modify_mass_param(int mass) {return eval_param<T>::_masses[mass];};
	static const mass_param& get_mass_param(int mass) {return eval_param<T>::_masses[mass];};
	static const std::complex<T> orig_mass(int mass) {std::complex<T> retmass; eval_param<T>::_masses.orig_mass(mass,retmass); return retmass;}
	static const std::complex<T> orig_mass2(int mass) {std::complex<T> retmass; eval_param<T>::_masses.orig_mass2(mass,retmass); return retmass;}
	static const std::complex<T> mass(int mass) {std::complex<T> retmass; eval_param<T>::_masses.mass(mass,retmass); return retmass;}
	static const std::complex<T> mass2(int mass) {std::complex<T> retmass; eval_param<T>::_masses.mass2(mass,retmass); return retmass;}
	static void set_dynamic(int mass, const std::complex<T>& dyn) {eval_param<T>::_masses.set_dynamic(mass,dyn);};
	static void set_dynamic2(int mass, const std::complex<T>& dyn) {eval_param<T>::_masses.set_dynamic2(mass,dyn);};
	static size_t nbr_masses() {return eval_param<T>::_masses.size();};

	void add(mass_param& mass_add){eval_param<T>::_masses.add(mass_add);} // Adds a mass to the library of masses
private :
	eval_param(const eval_param&);
	eval_param& operator=(const eval_param&);

};

template <class T> class eval_param_with_permutation : public eval_param<T> {
	long d_perm;
	// holds a pointer to the mc in which the momenta are stored. This is important for the skeleton code
	momentum_configuration<R>* d_mc;
public:
	eval_param_with_permutation(momentum_configuration<T>& mc, std::vector<int>& ind) : eval_param<T>(mc,ind) , d_perm(compute_permutation_code(ind)) , d_mc(&mc) {};
	eval_param_with_permutation(eval_param<T>& ep, long code,momentum_configuration<T>& mc) : eval_param<T>(ep) , d_perm(code) , d_mc(&mc) {};
	long get_permutation_code() const {return d_perm;}
	momentum_configuration<R>* get_mc() const {return d_mc;}
	virtual ~eval_param_with_permutation(){};
};

//Functions for extending the momenta of an eval_param to higher precision
template <class T_low,class T_high> std::vector<Cmom<T_high> >* extend_momenta(const eval_param<T_low>& ep);

}


#endif /* EVAL_PARAM_H_ */
