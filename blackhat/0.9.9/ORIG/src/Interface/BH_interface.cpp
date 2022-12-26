/*
 * BH_interface.cpp
 *
 *  Created on: 09-Mar-2009
 *      Author: daniel
 */

#include "BH_interface.h"
#include <iostream>
#include <string>
#include "BH_Ampl.h"
#include "mom_conf.h"
#include "assembly.h"
#include "settings.h"
#include "settings_reader.h"
#include "BH_Ampl_processes.h"
#include "BH_interface_impl.h"
#include "cached_OLHA.h"
#include <time.h>
#include "BH_error.h"
#include "path.h"
#include <set>
#include "BH_debug.h"

// concerning collectPS and echo modes for BH_interface
#define STORE_ONLY_ONE_SUBPROCESS_PER_PS_POINT 1

#define _VERBOSE 0

using namespace std;

namespace BH {

bool isApproximatelyEqual(double x,double y,double tol){
		double diff=abs(x-y)/(abs(x)+abs(y));
		if ( diff < tol) { return true; } else {return false; }
}



template <class T> void BH_interface_impl::set(const std::string& name,T value){
	bool isInterfaceSetting=d_settings_p->set(name,value);
	if (isInterfaceSetting){
		update_constants();
	} else {
		// try if it is a BH setting
		stringstream bs;
		bs << name << " " << value << "\n";
		stringstream ss(bs.str());
		settings::read_from_stream(ss);
	}
}

BH_interface_impl::~BH_interface_impl(){
	if (d_settings_p){
		delete d_settings_p;
	}
	delete d_mc_p;
}


void BH_interface::operator ()(BHinput& in){
	d_impl->operator()(in);
}

BH_Ampl* BH_interface::new_ampl(const std::vector<int>& particles){
	return d_impl->new_ampl(particles, nlo);
};


BH_Ampl* BH_interface::new_tree_ampl(const std::vector<int>& particles){
	return d_impl->new_ampl(particles, lo);
};



bool BH_interface::set(const std::string& name,double value){
	d_impl->set(name,value);
}
bool BH_interface::set(const std::string& name,int value){
	d_impl->set(name,value);
}
bool BH_interface::set(const std::string& name,std::string value){
	d_impl->set(name,value);
}
bool BH_interface::set(const std::string& name,bool value){
	d_impl->set(name,value);
}

void BH_interface::print_settings(std::ostream& os){
	d_impl->getSettingsTable()->display(os);
};


template <class T> bool BH_interface::apply_setting(const std::string& name, T& param){return d_impl->apply_setting(name,param);} ;

template bool BH_interface::apply_setting(const std::string& name, int& param);
template bool BH_interface::apply_setting(const std::string& name, string& param);
template bool BH_interface::apply_setting(const std::string& name, double& param);


BH_interface_impl::BH_interface_impl() : d_settings_p(new settings_table()),d_mc_p (new momentum_configuration<R> ){
	d_settings_p->add("Z_mass",double(constants::MZ));
	d_settings_p->add("Z_width",double(constants::GZ));
	d_settings_p->add("W_mass",double(constants::MW));
	d_settings_p->add("W_width",double(constants::GW));
	d_settings_p->add("sin_th_2",constants::sin_th_2);
//	d_settings_p->add("sin_2th",0.8416650165000326);
	d_settings_p->add("Top_mass",double(constants::Mtop));
	d_settings_p->add("alpha_S",constants::alpha_S);
	d_settings_p->add("alpha_QED",constants::alpha_QED);
	//
    d_settings_p->add("G3_Lambda2",1.e5);
}


class BH_interface_impl_normal: public BH_interface_impl {
	std::vector<BH_Ampl*> d_amplitudes;
public:
	//! Return a pointer to a new BH_Ampl object.
	virtual BH_Ampl* new_ampl(const std::vector<int>&, QCDorder lo_or_nlo);
	BH_Ampl* new_ampl_fn(const std::vector<int>&, QCDorder lo_or_nlo);
	virtual void operator()(BHinput& in);
	BH_interface_impl_normal();
	virtual void remove_last(){d_amplitudes.pop_back();};
	virtual ~BH_interface_impl_normal();
};

BH_interface_impl_normal::~BH_interface_impl_normal(){
	for (int i=0;i<d_amplitudes.size();i++){
		delete d_amplitudes[i];
	}
}

class BH_interface_impl_gridWarmup : public BH_interface_impl {
	int d_ampl_index;
public:
	//! Return a pointer to a new BH_Ampl object.
	virtual BH_Ampl* new_ampl(const std::vector<int>&, QCDorder lo_or_nlo);
	virtual void operator()(BHinput& in);
	BH_interface_impl_gridWarmup();
	virtual ~BH_interface_impl_gridWarmup(){};
};

class BH_interface_impl_collectPS : public BH_interface_impl {
	// keeps track of the # of phase space called
	int d_phase_space_point;
	// an index assigned to each amplitude created by "new_ampl" and stored by the corresponding d_process_locator (so far only in 2q3g2l and 2q2Q1g2l cases)
	int d_ampl_index;
	// this keeps the info of how many PS points are stored in a single file with matrix elements
	std::ifstream d_matrix_elements;
public:
	//! Return a pointer to a new BH_Ampl object.
	virtual BH_Ampl* new_ampl(const std::vector<int>&, QCDorder lo_or_nlo);
	virtual void operator()(BHinput& in);
	BH_interface_impl_collectPS();
	virtual ~BH_interface_impl_collectPS(){};
	// output file to store amplitude called and PS points of a given run
	std::ofstream d_ps_output_file;
#if STORE_ONLY_ONE_SUBPROCESS_PER_PS_POINT
	int number_of_get_finite_calls;
#endif
};

class BH_interface_impl_cached : public BH_interface_impl {
	BH_interface_impl* d_true_impl;
	std::vector<double> d_previous_values;
	std::vector<BH_Ampl*> d_amplitudes;
public:
	//! Return a pointer to a new BH_Ampl object.
	virtual BH_Ampl* new_ampl(const std::vector<int>&, QCDorder lo_or_nlo);
	virtual void operator()(BHinput& in){ d_true_impl->operator ()(in);};
	BH_interface_impl_cached();
	virtual ~BH_interface_impl_cached();
};


class BH_interface_impl_echo : public BH_interface_impl {
	// Just to handle, print and read relevant information for Sherpa when running stored heavy matrix elements
	//
	// keeps track of the # of phase space called
	int d_phase_space_point;
	// an index assigned to each amplitude created by "new_ampl" and stored by the corresponding d_process_locator (so far only in 2q3g2l and 2q2Q1g2l cases)
	// --- it counts how many amplitudes have been build
	int d_ampl_index;
	// this keeps the info of how many PS points are stored in a single file with matrix elements
	int d_file_base;
	// extension of file to be opened next when reading matrix elements
	int d_file_extension;
	// should be the input file containing the corresponding matrix elements if run is for returning stored values
	std::ifstream d_matrix_elements;
	// output file to store PS points of a given run
	std::ofstream d_ps_output_file;
public:
	//! Return a pointer to a new BH_Ampl object.
	virtual BH_Ampl* new_ampl(const std::vector<int>&, QCDorder lo_or_nlo);
	virtual void operator()(BHinput& in);
	BH_interface_impl_echo();
	virtual ~BH_interface_impl_echo(){};
#if STORE_ONLY_ONE_SUBPROCESS_PER_PS_POINT
	// given that only one subprocess is called per PS point, this valiable stores the corresponding ME
	double ME_result;
	// last process called
	int d_last_process_called;
#endif
};



//extend momenta to obtain better precision on input PS point
//assume all states to be massless
void extend_R(mom_conf & mc, size_t n_momenta){

    vector<int> ind; for(size_t ii=1;ii<n_momenta+1;ii++) ind.push_back(ii);
    mom_conf_HP mcHP=mc.extend<RHP>(ind);
    mc.clear();
    //
    for(size_t i=1;i<n_momenta+1;i++){
        mc.insert(Cmom<double>(
                    to_double(mcHP.p(i).E().real()),
                    to_double(mcHP.p(i).X().real()),
                    to_double(mcHP.p(i).Y().real()),
                    to_double(mcHP.p(i).Z().real())));
    }
    return;
}


void BH_interface_impl_normal::operator()(BHinput& in){
	d_mc_p->clear();
	mom_conf& mc=*d_mc_p;

///////////////////////////////////////////////////////////////////
// rescaling momenta and dimensionful objects to O(1) units
	R _GeV;	_GeV=double(in.m_momenta.size())/abs(in.m_momenta[0][0]+in.m_momenta[1][0]);
//_PRINT(_GeV);
	constants::s_GeV=_GeV;
///////////////////////////////////////////////////////////////////

		// flip sign of incoming momenta (first two passed)
		mc.insert(Cmom<double>(-in.m_momenta[0][0]*_GeV,
				-in.m_momenta[0][2]*_GeV,
				-in.m_momenta[0][3]*_GeV,
				-in.m_momenta[0][1]*_GeV));
		mc.insert(Cmom<double>(-in.m_momenta[1][0]*_GeV,
				-in.m_momenta[1][2]*_GeV,
				-in.m_momenta[1][3]*_GeV,
				-in.m_momenta[1][1]*_GeV));
		for(int k=2;k<in.m_momenta.size();k++){
			mc.insert(Cmom<double>(in.m_momenta[k][0]*_GeV,
					in.m_momenta[k][2]*_GeV,
					in.m_momenta[k][3]*_GeV,
					in.m_momenta[k][1]*_GeV));
		}

    //improve precision of input on-shell PS point 
    //assume all states to be massless
    extend_R(mc,in.m_momenta.size());
	
    d_mu=in.m_mu*_GeV;

}

void BH_interface_impl_gridWarmup::operator()(BHinput& in){
	d_mc_p->clear();
	mom_conf& mc=*d_mc_p;

///////////////////////////////////////////////////////////////////
// rescaling momenta and dimensionful objects to O(1) units
	R _GeV;	_GeV=2.*double(in.m_momenta.size())/abs(in.m_momenta[0][0]+in.m_momenta[1][0]);
//_PRINT(_GeV);
	constants::s_GeV=_GeV;
///////////////////////////////////////////////////////////////////

		// flip sign of incoming momenta (first two passed)
		mc.insert(Cmom<double>(-in.m_momenta[0][0]*_GeV,
				-in.m_momenta[0][2]*_GeV,
				-in.m_momenta[0][3]*_GeV,
				-in.m_momenta[0][1]*_GeV));
		mc.insert(Cmom<double>(-in.m_momenta[1][0]*_GeV,
				-in.m_momenta[1][2]*_GeV,
				-in.m_momenta[1][3]*_GeV,
				-in.m_momenta[1][1]*_GeV));
		for(int k=2;k<in.m_momenta.size();k++){
			mc.insert(Cmom<double>(in.m_momenta[k][0]*_GeV,
					in.m_momenta[k][2]*_GeV,
					in.m_momenta[k][3]*_GeV,
					in.m_momenta[k][1]*_GeV));
		}
    
   
    //improve precision of on-shell PS point 
    //assume all states to be massless
    extend_R(mc,in.m_momenta.size());

	d_mu=in.m_mu*_GeV;
}

void BH_interface_impl_collectPS::operator()(BHinput& in){
		d_mc_p->clear();
		mom_conf& mc=*d_mc_p;
#if STORE_ONLY_ONE_SUBPROCESS_PER_PS_POINT
		number_of_get_finite_calls=0;
#endif

			// flip sign of incoming momenta (first two passed)
			mc.insert(Cmom<double>(-in.m_momenta[0][0],-in.m_momenta[0][1],-in.m_momenta[0][2],-in.m_momenta[0][3]));
			mc.insert(Cmom<double>(-in.m_momenta[1][0],-in.m_momenta[1][1],-in.m_momenta[1][2],-in.m_momenta[1][3]));
			for(int k=2;k<in.m_momenta.size();k++){
				mc.insert(Cmom<double>(in.m_momenta[k][0],in.m_momenta[k][1],in.m_momenta[k][2],in.m_momenta[k][3]));
			}

// printing of PS point have been moved to BH_Ampl_collectPS::get_finite(), to store first the subprocess of interest
#if !STORE_ONLY_ONE_SUBPROCESS_PER_PS_POINT
		d_ps_output_file<<in.m_momenta[0][0]<<" "<<in.m_momenta[0][2]<<" "<<in.m_momenta[0][3]<<" "<<in.m_momenta[0][1]<<" ";
		d_ps_output_file<<in.m_momenta[1][0]<<" "<<in.m_momenta[1][2]<<" "<<in.m_momenta[1][3]<<" "<<in.m_momenta[1][1]<<" ";
		for(int kk=2;kk<in.m_momenta.size();kk++){
			d_ps_output_file<<in.m_momenta[kk][0]<<" "<<in.m_momenta[kk][2]<<" "<<in.m_momenta[kk][3]<<" "<<in.m_momenta[kk][1]<<" ";
		}
		d_ps_output_file<<endl;
#endif

		d_mu=in.m_mu;

		d_phase_space_point++;

}

void BH_interface_impl_echo::operator()(BHinput& in){
		// notice that this works only for 7-pt, although generalization is straightforward
		const char * filename;
		if(d_phase_space_point%d_file_base==0){
			std::string sfilename=settings::BH_interface_settings::s_echo_input_filename.c_str();
			std::string s;
			std::stringstream out;
			out << d_file_extension;
			sfilename+=out.str();
			filename=sfilename.c_str();

			if(d_matrix_elements.is_open())
				d_matrix_elements.close();
			d_matrix_elements.open(filename,ios::in);

			if (!d_matrix_elements){
				_WARNING2(" BH Interface could not open expected file: ",filename);
				throw BHerror("Missing file in BH_interface_impl_echo");
			}

			d_file_extension++;
		}
		d_phase_space_point++;
#if STORE_ONLY_ONE_SUBPROCESS_PER_PS_POINT
		d_matrix_elements>>ME_result;
#else
// this part can be used if more than a subprocess per PS point is called
		d_mc_p->clear();
		for(int ii=0;ii<(d_ampl_index/4);ii++){
			double e,x,y,z;
			d_matrix_elements>>e;
			d_matrix_elements>>x;
			d_matrix_elements>>y;
			d_matrix_elements>>z;
			d_mc_p->insert(Cmom<R>(e,x,y,z));
		}
		if(d_ampl_index%4>0){
			double xx[4];
			for(int ii=0;ii<4;ii++){
				if(ii<(d_ampl_index%4))
					d_matrix_elements>>xx[ii];
				else
					xx[ii]=0.0;
			}
			d_mc_p->insert(Cmom<R>(xx[0],xx[1],xx[2],xx[3]));
		}
#endif

		d_mu=in.m_mu;

	// SANITY check
	if((d_phase_space_point)%100000==0){
		std::cout<<setprecision(16);
//		_PRINT(filename);
		_PRINT(d_phase_space_point);
		_PRINT(in.m_mu);
#if STORE_ONLY_ONE_SUBPROCESS_PER_PS_POINT
		std::cout<<"process called in previous PS point: "<<d_last_process_called<<endl;
		_PRINT(ME_result);
#else
		_PRINT(d_mc_p->p(1));
#endif
	// getting the PS points
		std::cout<<in.m_momenta[0][0]<<" "<<in.m_momenta[0][2]<<" "<<in.m_momenta[0][3]<<" "<<in.m_momenta[0][1]<<" ";
		std::cout<<in.m_momenta[1][0]<<" "<<in.m_momenta[1][2]<<" "<<in.m_momenta[1][3]<<" "<<in.m_momenta[1][1]<<" ";
		for(int kk=2;kk<d_mc_p->n();kk++){
			std::cout<<in.m_momenta[kk][0]<<" "<<in.m_momenta[kk][2]<<" "<<in.m_momenta[kk][3]<<" "<<in.m_momenta[kk][1]<<" ";
		}
		std::cout<<endl;
	}
	// end of SANITIY check
}

BH_interface_impl_normal::BH_interface_impl_normal() : BH_interface_impl() {

}

BH_interface_impl_gridWarmup::BH_interface_impl_gridWarmup() : BH_interface_impl() {

}

BH_interface_impl_collectPS::BH_interface_impl_collectPS() : d_phase_space_point(0),d_ampl_index(0)
#if STORE_ONLY_ONE_SUBPROCESS_PER_PS_POINT
		,number_of_get_finite_calls(0)
#endif
		{
	d_ps_output_file.open(settings::BH_interface_settings::s_PS_collection_filename.c_str(),ios::app);
	d_ps_output_file<<setprecision(16);
//	d_ps_output_file<<"testing open"<<endl;
}
BH_interface_impl_echo::BH_interface_impl_echo() : d_phase_space_point(0),d_ampl_index(0),
	d_file_base(settings::BH_interface_settings::s_file_base), d_file_extension(0)
#if STORE_ONLY_ONE_SUBPROCESS_PER_PS_POINT
	, ME_result(0.0), d_last_process_called(0)
#endif
	{

}

BH_Ampl* BH_interface_impl_normal::new_ampl(const std::vector<int>& labels, QCDorder lo_or_nlo){
	BH_Ampl* A=new_ampl_fn(labels,lo_or_nlo);
	d_amplitudes.push_back(A);
	return A;
}

BH_Ampl* BH_interface_impl_normal::new_ampl_fn(const std::vector<int>& labels, QCDorder lo_or_nlo){
	// gluon 21
	// photon 22
	// d 1
	// u 2
	//s c b t
	//e- 11
	//ne 12
	//mu- 13
	//nmu 14
	//tau- 15
	//ntau- 16
//_MESSAGE("BH_interface::new_ampl CALLED");
//_MESSAGE("library in ~ffebres/workspace/W3jdists/blackhat-lib-0.6.3/");

//need to make sure that zero pointers are returned for processes that do not make sense.
//sherpa askes for such processes, which have certain conditions fulfilled: like charge 
//conservation and baryon # conservation.


	// updates the values of the constant
	update_constants();


	//get particle code
	std::vector<particle> types;
	size_t nbr_gluons=0,nbr_quarks=0,nbr_leptons=0,nbr_photon=0;
	for(int i=0;i<labels.size();i++){
        switch (std::abs(labels[i])){
			case 1: case 2: case 3: case 4: case 5: types.push_back(quark); ++nbr_quarks; break;
			case 11: case 13: case 15: types.push_back(lepton); ++nbr_leptons;break;
			case 12: case 14: case 16: types.push_back(lepton); ++nbr_leptons;break;
			case 21: types.push_back(gluon);++nbr_gluons; break;
			case 22: types.push_back(photon);++nbr_photon; break;
		}
	}
	size_t pc=nbr_gluons+10*nbr_quarks+100*nbr_leptons+100000*nbr_photon;
	
	//force auto assembly for pure QCD
	bool force_autoassembly(false);
	switch(pc){
		case 4: case 5: case 6: 
		case 22: case 23: case 24: case 25: 
		case 40: case 41 :case 42: case 43:
		case 60: case 61: 
		case 223: case 241: 
		case 100022: case 100023: case 100024: case 100025: 
        case 100040: case 100041: case 100042: case 100043:
		case 100060: case 100061: 
		case 200020: case 200021: case 200022: case 200023: case 200024: case 200025: 
		case 200040: case 200042: case 200043:
		case 200060: case 200061: 
				force_autoassembly=true; break;
	};


    //force automated assembly for born only runs
    //if(lo_or_nlo==lo){
    //    force_autoassembly=true; 
    //}

// USE automated assemblies
if(BH::settings::BH_interface_settings::s_use_automated_assembly||force_autoassembly){
    BH_DEBUG_MESSAGE("Using automated assembly.");
    
    //use IR checked amplitudes 
	
	CachedOLHA::use_IR_checked();
    //set to use parent diagram files
    bool use_parent_files=BH::settings::general::s_use_parent_files;
    BH::settings::general::s_use_parent_files=true;
	
    std::vector<pair<int,int> > particle_labels;
	for(int i=0;i<2;i++){
		if((0<abs(labels[i])&&abs(labels[i])<7)||
		(10<abs(labels[i])&&abs(labels[i])<17)){particle_labels.push_back(make_pair(i+1,-labels[i]));}
		else {particle_labels.push_back(make_pair(i+1,labels[i]));};
	}
	for(int i=2;i<labels.size();i++){
		particle_labels.push_back(make_pair(i+1,labels[i]));
	}
	//NbrPowerOfAlphaS
	int pdb,partons(0),quarks(0),lept_photon(0);
	for(int i=0;i<particle_labels.size();i++){
		pdb=abs(particle_labels[i].second);
		if(pdb==21){partons++;}
		else if((pdb>0&&pdb<7)){partons++;quarks++;}
		else if(pdb==22||(pdb>10&&pdb<17)){lept_photon++;};
	}
	int Nbr_Ext_Particles(particle_labels.size());
	int NbrPowersOfAlphaS(partons-2);
	int NbrPowersOfAlphaQED(lept_photon);
	int GeVdim(2*Nbr_Ext_Particles-8);
	//scheme_shift & renomralization_shift
	double Nc(settings::BH_interface_settings::s_nc);
	double Nf(settings::BH_interface_settings::s_nf);
	double scheme_shift(0);
	double renormalization_shift(0);
	double d_lc(0);
	double d_fc(0);
	
    d_lc=Nc*(1.+partons-quarks);
    d_fc=d_lc-quarks/2.0/Nc;

    switch (settings::BH_interface_settings::s_BH_color_mode){
	     //apply full renormalization and scheme-shift to full and leading color
         //we use here the freedom to define leading color upt to 1/Nc terms
         //does not resemble the scheme and renorm. shift of the primitive amplitudes in the leading color approximation 
        case settings::BH_interface_settings::full_color:
        case settings::BH_interface_settings::leading_color: { 
            scheme_shift=-Nc*(-1./3.-quarks*(-1./4./Nc/Nc +1./12.));
    	    renormalization_shift=(partons-2.)/2.*Nc*(11./3.-2.*Nf/3./Nc);
        } break;
        case settings::BH_interface_settings::full_minus_leading_color:{
             scheme_shift=0;
             renormalization_shift=0;
        } break;
    }
    //same_helicity_projection
    //make sure that renormalization and scheme shift are only used once
    //average scheme and renorm shift over all helicity classes
    if (settings::BH_interface_settings::s_same_helicity_projection!=-1){
        double helicity_classes=(Nbr_Ext_Particles/2-1);
        scheme_shift=scheme_shift/helicity_classes;
        renormalization_shift=renormalization_shift/helicity_classes;
    }

	//approximation (should be order in Nc eventually)
    approx born_or_virt(born);
	if(lo_or_nlo==nlo){
		born_or_virt=virt;
	}

	BH_Ampl_impl* ampl=new BH_Ampl_data(
		particle_labels,
		lo_or_nlo,
		this,
		Nbr_Ext_Particles,
		NbrPowersOfAlphaS,
		NbrPowersOfAlphaQED,
		GeVdim,
		scheme_shift,
		renormalization_shift,
		d_lc,d_fc
		);
	//if no assembly file is found try to generate it and 
	//then compute again
	if(ampl->vanishing_VSM()&&BH::settings::BH_interface_settings::s_generate_assembly_files){
#ifndef BH_PUBLIC

	_MESSAGE("Generate assembly entries.");	
	ME2_factory* me2_factory;
	BH_Ampl_automated(
		particle_labels,
		me2_factory,
		born_or_virt,
		this,
		Nbr_Ext_Particles,
		NbrPowersOfAlphaS,
		NbrPowersOfAlphaQED,
		GeVdim,
		scheme_shift,
		renormalization_shift
		);
	delete me2_factory;
	_MESSAGE("Finished generating assembly entries.");	
#endif
	
	ampl=new BH_Ampl_data(
		particle_labels,
		lo_or_nlo,
		this,
		Nbr_Ext_Particles,
		NbrPowersOfAlphaS,
		NbrPowersOfAlphaQED,
		GeVdim,
		scheme_shift,
		renormalization_shift,
        d_lc,d_fc
		);
	}
 
    //set back the use parents switch
    BH::settings::general::s_use_parent_files=use_parent_files;

    //if still zero this might be because of process request which violates 
	//charge/flavor conservation and we return zero pointer.
	if(ampl->vanishing_VSM()){
		delete ampl;
       	return 0;
	}
	else{ 
		return ampl;
	}

};

BH_DEBUG_MESSAGE("Using standard assembly.");
// USE standard assemblies

	int case4q=-1;
	int case6q=-1;
	std::vector<int> fermion_ind; //Used to quark indices into cannonical from for 6-quark processes.
	bool up_down_quark=0;
	// default zero, which is for only off-shell photon
	int photonZW=0;
	// labels_flipped corrects for the all outgoing BH conventions if initial fermions are present
	std::vector<int> labels_flipped;
		if((std::abs(labels[0])>0 && std::abs(labels[0])<6) || (std::abs(labels[0])>10 && std::abs(labels[0])<17))
			labels_flipped.push_back(-labels[0]);
		else
			labels_flipped.push_back(labels[0]);
		if((std::abs(labels[1])>0 && std::abs(labels[1])<6) || (std::abs(labels[1])>10 && std::abs(labels[1])<17))
			labels_flipped.push_back(-labels[1]);
		else
			labels_flipped.push_back(labels[1]);
		for(int k=2;k<labels.size();k++)
			labels_flipped.push_back(labels[k]);
	switch(pc){
	  case 41: case 42:{
		// needs modification for W --- so far only drops the run
		int quark1=0,quark2=0,quark3=0,quark4=0;
		for(int i=0;i<labels_flipped.size();i++){
			if(labels_flipped[i]>0 && labels_flipped[i]<6 && quark1==0)
				quark1=labels_flipped[i];
			else if(labels_flipped[i]>0 && labels_flipped[i]<6 && quark2==0)
				quark2=labels_flipped[i];
			else if(labels_flipped[i]<0 && labels_flipped[i]>-6 && quark3==0)
				quark3=labels_flipped[i];
			else if(labels_flipped[i]<0 && labels_flipped[i]>-6 && quark4==0)
				quark4=labels_flipped[i];
		}
		if(quark1==0 || quark2==0 || quark3==0 || quark4==0 ){
			std::cout<<"wrong assignmnet of particle labels in 2q2Q1g amps\n";
			throw BHerror("Sorry, can't do that yet.");
		}
		//
		else if((quark1==-quark3 && quark2==-quark4) || (quark1==-quark4 && quark2==-quark3)){
			if(quark1==quark2 && quark1%2==0)
				// uu
				case4q=0;
			else if(quark1==quark2)
				// dd
				case4q=5;
			else if(std::abs(quark1-quark2)%2==0 && quark1%2==0)
				// uup
				case4q=1;
			else if(std::abs(quark1-quark2)%2==0)
				// ddp
				case4q=4;
			else if(quark1%2==0)
				// ud
				case4q=2;
			else
				// du
				case4q=3;

			}
		else{
			std::cout<<"wrong assignmnet of particle labels in 2q2Q2l amps!\n";
			throw BHerror("Sorry, can't do that yet.");
		}
	  } break;
	  case 60:{
		// needs modification for W --- so far only drops the run
		// only for distince flavor case so far
		int quark1=0,quark2=0,quark3=0,quark4=0,quark5=0,quark6=0;
		for(int i=0;i<labels_flipped.size();i++){
			if(labels_flipped[i]>0 && labels_flipped[i]<6 && quark1==0)
				quark1=labels_flipped[i];
			else if(labels_flipped[i]>0 && labels_flipped[i]<6 && quark2==0)
				quark2=labels_flipped[i];
			else if(labels_flipped[i]>0 && labels_flipped[i]<6 && quark3==0)
				quark3=labels_flipped[i];
			else if(labels_flipped[i]<0 && labels_flipped[i]>-6 && quark4==0)
				quark4=labels_flipped[i];
			else if(labels_flipped[i]<0 && labels_flipped[i]>-6 && quark5==0)
				quark5=labels_flipped[i];
			else if(labels_flipped[i]<0 && labels_flipped[i]>-6 && quark6==0)
				quark6=labels_flipped[i];
		}
		if(quark1==0 || quark2==0 || quark3==0 || quark4==0 || quark5==0 || quark6==0){
			std::cout<<"wrong assignmnet of particle labels in 2q2Q2P amps\n";
			throw BHerror("Sorry, can't do that yet.");
		}
		//
		else if((quark1==-quark4 && quark2==-quark5 && quark3==-quark6 )
			||(quark1==-quark4 && quark2==-quark6 && quark3==-quark5 )
			||(quark1==-quark6 && quark2==-quark5 && quark3==-quark4 )
			||(quark1==-quark5 && quark2==-quark4 && quark3==-quark6 )
			||(quark1==-quark5 && quark2==-quark6 && quark3==-quark4 )
			||(quark1==-quark6 && quark2==-quark4 && quark3==-quark5 ))
		{
			if(quark1==quark2 && quark2==quark3 && quark1%2==0)
				// uuu
				case6q=0;
			else if(quark1==quark2 && quark2==quark3)
				// ddd
				case6q=1;
			else if(quark1==quark2 && quark1%2==0 && quark3%2==0)
				// uuup
				case6q=2;
			else if(quark1==quark2 && quark1%2==0)
				// uud
				case6q=3;
			else if(quark1==quark3 && quark1%2==0 && quark2%2==0)
				// uupu
				case6q=4;
			else if(quark1==quark3 && quark1%2==0)
				// udu
				case6q=5;
			else if(quark3==quark2 && quark3%2==0 && quark1%2==0)
				// upuu
				case6q=6;
			else if(quark3==quark2 && quark3%2==0)
				// duu
				case6q=7;
			else if(quark1==quark2 && quark1%2==1 && quark3%2==1)
				// dddp
				case6q=8;
			else if(quark1==quark2 && quark1%2==1)
				// ddu
				case6q=9;
			else if(quark1==quark3 && quark1%2==1 && quark2%2==1)
				// ddpd
				case6q=10;
			else if(quark1==quark3 && quark1%2==1)
				// dud
				case6q=11;
			else if(quark3==quark2 && quark3%2==1 && quark1%2==1)
				// dpdd
				case6q=12;
			else if(quark3==quark2 && quark3%2==1)
				// udd
				case6q=13;
			else if(std::abs(quark1-quark2)%2==0 && quark3%2==1)
				//uupd
				case6q=14;
			else if(std::abs(quark2-quark3)%2==0 && quark1%2==1)
				//udup
				case6q=15;
			else if(std::abs(quark3-quark1)%2==0 && quark2%2==1)
				//duup
				case6q=16;
			else if(std::abs(quark1-quark2)%2==0 && std::abs(quark2-quark3)%2==0)
				//uupupp
				case6q=17;
			else if(std::abs(quark1-quark2)%2==1 && quark3%2==0)
				//ddpu
				case6q=18;
			else if(std::abs(quark2-quark3)%2==1 && quark1%2==0)
				//dudp
				case6q=19;
			else if(std::abs(quark3-quark1)%2==1 && quark2%2==0)
				//uddp
				case6q=20;
			else if(std::abs(quark1-quark2)%2==1 && std::abs(quark2-quark3)%2==1)
				//ddpdpp
				case6q=21;
			else
				 std::cout<<"wrong assignmnet of particle labels in 2q2Q2l amps!\n";
			}
		else{
			std::cout<<"wrong assignmnet of particle labels in 2q2Q2l amps!\n";
			throw BHerror("Sorry, can't do that yet.");
		}
	  } break;
	  case 220: case 221: case 222: case 223: case 224: case 225: {
		int lepton1=0,lepton2=0;
		int quark1=0,quark2=0;
		for(int i=0;i<labels_flipped.size();i++){
			if(labels_flipped[i]>10 && labels_flipped[i]<17 && lepton1==0)
				lepton1=labels_flipped[i];
			else if(labels_flipped[i]<-10 && labels_flipped[i]>-17)
				lepton2=labels_flipped[i];
			else if(labels_flipped[i]>0 && labels_flipped[i]<6 && quark1==0)
				quark1=labels_flipped[i];
			else if(labels_flipped[i]<0 && labels_flipped[i]>-6 && quark2==0)
				quark2=labels_flipped[i];
		}
		if(quark1==0 || quark2==0 || lepton1==0 || lepton2==0){
			std::cout<<"wrong assignmnet of particle labels in 2q2g2l amps\n";
			throw BHerror("Sorry, can't do that yet.");
		}
		else if(lepton1==-lepton2 && (std::abs(lepton1)==11 || std::abs(lepton1)==13 /*|| std::abs(lepton1)==15*/)){
			// gamma+Z case
			photonZW=1;
		}
		else if(lepton1==-lepton2  && /* abuse of tauon for this switch */ (std::abs(lepton1)==15)){
			// forced gamma decaying into leptons
			photonZW=0;
		}
		else if(lepton1==-lepton2  && (std::abs(lepton1)==12 || std::abs(lepton1)==14 || std::abs(lepton1)==16)){
			// pure Z case decaying into neutrinos
			photonZW=2;
		}
		else if((((lepton1==11 || lepton1==13 || lepton1==15) && (lepton1+lepton2)==-1) ||
			((lepton1==12 || lepton1==14 || lepton1==16) && (lepton1+lepton2)==1)) &&
			std::abs(quark1+quark2)%2==1 &&
			((1+std::abs(quark1))/2==(1+std::abs(quark2))/2)){
			// W case
			// diagonal CKM
			photonZW=3;
		}
		else{ return 0; //return zero pointer if process does not make sense for diagonal CKM
		}
		// W case keeps default value up_down_quark=0
		if(std::abs(quark1)==std::abs(quark2)){
			switch (std::abs(quark1)){
				// up quarks
				case 2: case 4:	up_down_quark=0; break;
				// down quarks
				case 1: case 3: case 5:up_down_quark=1; break;
			}
		}
	  } break;
	  case 240: case 241: case 242: {
		// needs modification for W --- so far only drops the run
		int lepton1=0,lepton2=0;
		int quark1=0,quark2=0,quark3=0,quark4=0;
		for(int i=0;i<labels_flipped.size();i++){
			if(std::abs(labels_flipped[i])>10 && std::abs(labels_flipped[i])<17 && lepton1==0)
				lepton1=labels_flipped[i];
			else if(std::abs(labels_flipped[i])>10 && std::abs(labels_flipped[i])<17)
				lepton2=labels_flipped[i];
			else if(labels_flipped[i]>0 && labels_flipped[i]<6 && quark1==0)
				quark1=labels_flipped[i];
			else if(labels_flipped[i]>0 && labels_flipped[i]<6 && quark2==0)
				quark2=labels_flipped[i];
			else if(labels_flipped[i]<0 && labels_flipped[i]>-6 && quark3==0)
				quark3=labels_flipped[i];
			else if(labels_flipped[i]<0 && labels_flipped[i]>-6 && quark4==0)
				quark4=labels_flipped[i];
		}
		if(quark1==0 || quark2==0 || quark3==0 || quark4==0 || lepton1==0 || lepton2==0){
			std::cout<<"wrong assignmnet of particle labels in 2q2Q2l amps\n";
			throw BHerror("Sorry, can't do that yet.");
		}
		// Z & gamma cases
		else if((quark1==-quark3 && quark2==-quark4) || (quark1==-quark4 && quark2==-quark3)){
			if(quark1==quark2 && quark1%2==0)
				// uu
				case4q=0;
			else if(quark1==quark2)
				// dd
				case4q=5;
			else if(std::abs(quark1-quark2)%2==0 && quark1%2==0)
				// uup
				case4q=1;
			else if(std::abs(quark1-quark2)%2==0)
				// ddp
				case4q=4;
			else if(quark1%2==0)
				// ud
				case4q=2;
			else
				// du
				case4q=3;

			if(lepton1==-lepton2 && (std::abs(lepton1)==11 || std::abs(lepton1)==13 /* used to switch to photon only || std::abs(lepton1)==15*/)){
				// gamma+Z case
				photonZW=1;
			}
			else if(lepton1==-lepton2  && /* abuse of tauon for this switch */ (std::abs(lepton1)==15)){
				// forced gamma decaying into leptons
				photonZW=0;
			}
			else if(lepton1==-lepton2){
				// pure Z case decaying into neutrinos
				photonZW=2;
			}
		}
		// W cases
		// diagonal CKM
		else if(quark1==-quark3 && std::abs(quark2+quark4)%2==1 
				&&((1+std::abs(quark2))/2==(1+std::abs(quark4))/2)){
			photonZW=3;
			if(quark1==quark2)	case4q=0;
			else if(quark3==quark4)	case4q=1;
			else	case4q=2;
		}
		else if(quark1==-quark4 && std::abs(quark2+quark3)%2==1 
				&& ((1+std::abs(quark2))/2==(1+std::abs(quark3))/2)){
			photonZW=3;
			if(quark1==quark2)	case4q=0;
			else if(quark3==quark4)	case4q=1;
			else	case4q=2;
		}
		else if(quark2==-quark3 && std::abs(quark1+quark4)%2==1 
				&& ((1+std::abs(quark1))/2==(1+std::abs(quark4))/2)){
			photonZW=3;
			if(quark1==quark2)	case4q=0;
			else if(quark3==quark4)	case4q=1;
			else	case4q=2;
		}
		else if(quark2==-quark4 && std::abs(quark1+quark3)%2==1 
				&& ((1+std::abs(quark1))/2==(1+std::abs(quark3))/2)){
			photonZW=3;
			if(quark1==quark2)	case4q=0;
			else if(quark3==quark4)	case4q=1;
			else	case4q=2;
		}

		else{ return 0; //return zero-pointer if proces does not make sense for diagonal CKM
		}
	  } break;
	  case 260: {
		// needs modification for W --- so far only drops the run
		int lepton(-1),leptonb(-1);
		std::vector<int> quarks;
		std::vector<int> anti_quarks;
		for(int i=0;i<labels_flipped.size();i++){
			if(labels_flipped[i]>10 && labels_flipped[i]<17)
				lepton=i;
			else if(labels_flipped[i]<-10 && labels_flipped[i]>-17)
				leptonb=i;
			else if(labels_flipped[i]>0 && labels_flipped[i]<6)
				quarks.push_back(i);
			else if(labels_flipped[i]<0 && labels_flipped[i]>-6)
				anti_quarks.push_back(i);
		}
		if(quarks.size()!=3 || anti_quarks.size()!=3 || lepton==-1 || leptonb==-1){
			std::cout<<"wrong assignmnet of particle labels in 6q2l amps\n";
			throw BHerror("Sorry, can't do that yet.");
		}
		// Z & gamma cases
		else if(labels_flipped[lepton]==-labels_flipped[leptonb]){
			// cases to consider:
			// forced gamma decaying into leptons
			// pure Z case decaying into neutrinos
		}
		// W cases
		// diagonal CKM
		else if(labels_flipped[lepton]!=-labels_flipped[leptonb]){
			photonZW=3;
			int Wpm=labels_flipped[lepton]+labels_flipped[leptonb];
			int permutations[6][3]={
				0,1,2,
				0,2,1,
				1,2,0,
				1,0,2,
				2,0,1,
				2,1,0};
			std::vector<int> ind;
			int counter(0);
			for(int i=0;i<3;i++){
			for(int j=0;j<6;j++){
				std::vector<int> quark_vector;
				if(labels_flipped[quarks[i%3]]+Wpm==-labels_flipped[anti_quarks[permutations[j][0]]]&&
				((1+std::abs(labels_flipped[quarks[i%3]]))/2==(1+std::abs(labels_flipped[anti_quarks[permutations[j][0]]]))/2)&&
				   labels_flipped[quarks[(i+1)%3]]==-labels_flipped[anti_quarks[permutations[j][1]]]&&
				   labels_flipped[quarks[(i+2)%3]]==-labels_flipped[anti_quarks[permutations[j][2]]]){
					if(counter==0){
						ind.push_back(quarks[i%3]);
						ind.push_back(anti_quarks[permutations[j][1]]);
						ind.push_back(quarks[(i+1)%3]);
						ind.push_back(anti_quarks[permutations[j][2]]);
						ind.push_back(quarks[(i+2)%3]);
						ind.push_back(anti_quarks[permutations[j][0]]);	
						ind.push_back(lepton);
						ind.push_back(leptonb);
					}
					counter++;
				}
			}
			}
			for(int i=0;i<ind.size();i++){fermion_ind.push_back(ind[i]);}//will be corrected in switch below if different order is expected
			switch(counter){
				// The notation for case6q borrowed from (2,x,xb,y,yb,-1,11,-12) case: case6q="xy".
				// the basic distinct cases are: 
				// case6q=11: (2,-1,1,-1,1,-1,11,-12),
				// case6q=22: (2,-2,2,-2,2,-1,11,-12),
				// case6q=33: (2,-3,3,-3,3,-1,11,-12),
				// case6q=21: (2,-2,2,-1,1,-1,11,-12),
				// case6q=31: (2,-3,3,-1,1,-1,11,-12),
				// case6q=23: (2,-2,2,-3,3,-1,11,-12),
				// case6q=34: (2,-3,3,-4,4,-1,11,-12).
				// We relabel to reduce all orderings of quarks to these 7 basic arrangements. 
				// This rearrangement is tuned to Wm+jets processes.
				// For (1,x,xb,y,yb,-2,-11,12) notation is missleading: case6q="11" then really means x=2,y=2.
				case 6:{
				       if(labels_flipped[ind[0]]==labels_flipped[ind[2]]) 
					       case6q=22;
				       else case6q=11;
				       } break;
				case 4:{
				       case6q=21;
				       fermion_ind=ind;
				       //If we have instead xy=12 we reshuffle to go get back xy=21.
				       if(labels_flipped[ind[0]]==labels_flipped[ind[4]]){
				       		fermion_ind[1]=ind[3];fermion_ind[2]=ind[4];fermion_ind[3]=ind[1];fermion_ind[4]=ind[2];};
				       } break;
				case 2:{
				       case6q=33;
				       if(labels_flipped[ind[0]]==labels_flipped[ind[2]]) case6q=23;
				       else if(labels_flipped[ind[0]]==labels_flipped[ind[4]]){case6q=23;fermion_ind[1]=ind[3];fermion_ind[2]=ind[4];fermion_ind[3]=ind[1];fermion_ind[4]=ind[2];}
				       else if(labels_flipped[ind[5]]==labels_flipped[ind[3]]) case6q=31;
				       else if(labels_flipped[ind[5]]==labels_flipped[ind[1]]){case6q=31;fermion_ind[1]=ind[3];fermion_ind[2]=ind[4];fermion_ind[3]=ind[1];fermion_ind[4]=ind[2];}
				       } break;
				case 1: case6q=34; break;
				case 0: return 0; break; //diagonal CKM; flavor charge non-conservation should return zero pointer.
			}
		}
	  } break;
	  case 100021: case 100022: case 100023: {
		int quark1=-1;
		for(int i=0;i<labels_flipped.size();i++){
			if(labels_flipped[i]>0 && labels_flipped[i]<6 && quark1<0)
				quark1=labels_flipped[i];
		}
                        switch (std::abs(quark1)){
		            // up quarks     
	                       case 2: case 4: up_down_quark=0; break;
		            // down quarks 
			       case 1: case 3: case 5:up_down_quark=1; break;
			}
		} break;
	  case 100040: {
		int quark1=-1,quark2=-1;
		for(int i=0;i<labels_flipped.size();i++){
			if(labels_flipped[i]>0 && labels_flipped[i]<6 && quark1<0)
				quark1=labels_flipped[i];
			else if(labels_flipped[i]>0 && labels_flipped[i]<6)
				quark2=labels_flipped[i];
		}
		if(quark1==quark2)
			case4q=0;
		else if(std::abs(quark1-quark2)%2==0)
			case4q=1;
		else
			case4q=2;
              	switch (std::abs(quark1)){
		            // up quarks     
	                       case 2: case 4: up_down_quark=0; break;
		            // down quarks 
			       case 1: case 3: case 5:up_down_quark=1; break;}
	  } break;
	  case 100041:{
		int quark1=0,quark2=0,quark3=0,quark4=0;
		for(int i=0;i<labels_flipped.size();i++){
			if(labels_flipped[i]>0 && labels_flipped[i]<6 && quark1==0)
				quark1=labels_flipped[i];
			else if(labels_flipped[i]>0 && labels_flipped[i]<6 && quark2==0)
				quark2=labels_flipped[i];
			else if(labels_flipped[i]<0 && labels_flipped[i]>-6 && quark3==0)
				quark3=labels_flipped[i];
			else if(labels_flipped[i]<0 && labels_flipped[i]>-6 && quark4==0)
				quark4=labels_flipped[i];
		}
		if(quark1==0 || quark2==0 || quark3==0 || quark4==0){
			std::cout<<"wrong assignmnet of particle labels in 2q2Q1g1y amps\n";
			throw BHerror("Sorry, can't do that yet.");
		}
		// Z & gamma cases
		else if((quark1==-quark3 && quark2==-quark4) || (quark1==-quark4 && quark2==-quark3)){
			if(quark1==quark2 && quark1%2==0)
				// uu
				case4q=0;
			else if(quark1==quark2)
				// dd
				case4q=5;
			else if(std::abs(quark1-quark2)%2==0 && quark1%2==0)
				// uup
				case4q=1;
			else if(std::abs(quark1-quark2)%2==0)
				// ddp
				case4q=4;
			else if(quark1%2==0)
				// ud
				case4q=2;
			else
				// du
				case4q=3;

			// forced gamma decaying into leptons
			photonZW=0;
			}
		else{ return 0; //return zero-pointer if proces does not make sense for diagonal CKM
		}
	  } break;
	  default: return 0; //return zero as we do not know what to do
	}
#if _VERBOSE
_PRINT(case4q);
_PRINT(case6q);
_PRINT(photonZW);
_PRINT(up_down_quark);
#endif

//_PRINT(pc);

// with this we get the correct assigment of momenta --- notice that first to momenta are assumed to be initial
	std::vector<int> mom_assg;
	switch(pc){
	  case 41:{
		int qpos=-1,qbpos=-1,q2pos=-1,qb2pos=-1,g1pos=-1;
		for(int i=0;i<labels_flipped.size();i++){
			if(labels_flipped[i]>0 && labels_flipped[i]<6 && qpos==-1){qpos=i;}
			else if(labels_flipped[i]>0 && labels_flipped[i]<6 && q2pos==-1){q2pos=i;}
			else if(labels_flipped[i]<0 && labels_flipped[i]>-6 && qbpos==-1){qbpos=i;}
			else if(labels_flipped[i]<0 && labels_flipped[i]>-6 && qb2pos==-1){qb2pos=i;}
			else if(labels_flipped[i]==21 && g1pos==-1){g1pos=i;}
		}
	// NOTICE that for BH_interface mom_assg is base 1, as it'll be passed as input ind
			switch (case4q){
				case 0: case 5: break;
				default:{
					if(labels_flipped[qpos]!=-labels_flipped[qbpos]){
						int qbp(qbpos),qb2p(qb2pos);
						qbpos=qb2p;
						qb2pos=qbp;
					}
				} break;
			}
		mom_assg.push_back(qpos+1);
		mom_assg.push_back(qb2pos+1);
		mom_assg.push_back(q2pos+1);
		mom_assg.push_back(qbpos+1);
		mom_assg.push_back(g1pos+1);
	  } break;
	  case 42:{
		int qpos=-1,qbpos=-1,q2pos=-1,qb2pos=-1,g1pos=-1,g2pos=-1;
		for(int i=0;i<labels_flipped.size();i++){
			if(labels_flipped[i]>0 && labels_flipped[i]<6 && qpos==-1){qpos=i;}
			else if(labels_flipped[i]>0 && labels_flipped[i]<6 && q2pos==-1){q2pos=i;}
			else if(labels_flipped[i]<0 && labels_flipped[i]>-6 && qbpos==-1){qbpos=i;}
			else if(labels_flipped[i]<0 && labels_flipped[i]>-6 && qb2pos==-1){qb2pos=i;}
			else if(labels_flipped[i]==21 && g1pos==-1){g1pos=i;}
			else if(labels_flipped[i]==21 && g2pos==-1){g2pos=i;}
		}
		switch (case4q){
			case 0: case 5: break;
			default:{
				if(labels_flipped[qpos]!=-labels_flipped[qbpos]){
					int qbp(qbpos),qb2p(qb2pos);
					qbpos=qb2p;
					qb2pos=qbp;
				}
			} break;
		}
// NOTICE that for BH_interface mom_assg is base 1, as it'll be passed as input ind
		mom_assg.push_back(qpos+1);
		mom_assg.push_back(qb2pos+1);
		mom_assg.push_back(q2pos+1);
		mom_assg.push_back(qbpos+1);
		mom_assg.push_back(g1pos+1);
		mom_assg.push_back(g2pos+1);
	  } break;

	  case 60:{
		//passing two identical quarks in pos1 & pos2
		int q1pos=-1,qb1pos=-1,q2pos=-1,qb2pos=-1,q3pos=-1,qb3pos=-1;
		for(int i=0;i<labels_flipped.size();i++){
			if(labels_flipped[i]>0 && labels_flipped[i]<6 && q1pos==-1){q1pos=i;}
			else if(labels_flipped[i]>0 && labels_flipped[i]<6 && q2pos==-1){q2pos=i;}
			else if(labels_flipped[i]>0 && labels_flipped[i]<6 && q3pos==-1){q3pos=i;}
		}
		switch (case6q){
			case 0: case 1:
			//uuu //ddd
			for(int i=0;i<labels_flipped.size();i++){
		if(labels_flipped[i]<0 && labels_flipped[i]>-6 && qb1pos==-1){qb1pos=i;}
		else if(labels_flipped[i]<0 && labels_flipped[i]>-6 && qb2pos==-1){qb2pos=i;}
		else if(labels_flipped[i]<0 && labels_flipped[i]>-6 && qb3pos==-1){qb3pos=i; break;}} break;
			case 2: case 3:	case 8: case 9:
			// uuup //uud //dddp //ddu
			for(int i=0;i<labels_flipped.size();i++){
		if(labels_flipped[i]<0 && labels_flipped[i]>-6 && qb1pos==-1&& labels_flipped[q1pos]==-labels_flipped[i]){qb1pos=i;}
		else if(labels_flipped[i]<0 && labels_flipped[i]>-6 && qb2pos==-1&& labels_flipped[q2pos]==-labels_flipped[i]){qb2pos=i;}
		else if(labels_flipped[i]<0 && labels_flipped[i]>-6 && qb3pos==-1){qb3pos=i; break;}} break;
			case 4: case 5: case 10: case 11:{
			// uupu //udu   //ddpd   //dud
			for(int i=0;i<labels_flipped.size();i++){
		if(labels_flipped[i]<0 && labels_flipped[i]>-6 && qb1pos==-1&& labels_flipped[q1pos]==-labels_flipped[i]){qb1pos=i;}
		else if(labels_flipped[i]<0 && labels_flipped[i]>-6 && qb3pos==-1&& labels_flipped[q3pos]==-labels_flipped[i]){qb3pos=i;}
		else if(labels_flipped[i]<0 && labels_flipped[i]>-6 && qb2pos==-1){qb2pos=i; break;}};
			int q1(q1pos),q2(q2pos),q3(q3pos),qb1(qb1pos),qb2(qb2pos),qb3(qb3pos);
			q1pos=q1;
			qb1pos=qb1;
			q2pos=q3;
                        qb2pos=qb3;
			q3pos=q2;
                        qb3pos=qb2;} break;
			case 6: case 7: case 12: case 13:{
			// upuu // duu	//dpdd   //udd
			for(int i=0;i<labels_flipped.size();i++){
		if(labels_flipped[i]<0 && labels_flipped[i]>-6 && qb2pos==-1&& labels_flipped[q2pos]==-labels_flipped[i]){qb2pos=i;}
		else if(labels_flipped[i]<0 && labels_flipped[i]>-6 && qb3pos==-1&& labels_flipped[q3pos]==-labels_flipped[i]){qb3pos=i;}
		else if(labels_flipped[i]<0 && labels_flipped[i]>-6 && qb1pos==-1){qb1pos=i; break;}};
			int q1(q1pos),q2(q2pos),q3(q3pos),qb1(qb1pos),qb2(qb2pos),qb3(qb3pos);
			q1pos=q2;
                        qb1pos=qb2;
                        q2pos=q3;
                        qb2pos=qb3;
                        q3pos=q1;
                        qb3pos=qb1;} break;
			case 14:  case 15: case 16: case 17: case 18: case 19: case 20: case 21:
			// uupd   //udup   //duup   //uupupp //ddpu   /dudp    //uddp   //ddpdpp
			for(int i=0;i<labels_flipped.size();i++){
		if(labels_flipped[i]<0 && labels_flipped[i]>-6 && qb1pos==-1&& labels_flipped[q1pos]==-labels_flipped[i]){qb1pos=i;}
		else if(labels_flipped[i]<0 && labels_flipped[i]>-6 && qb2pos==-1&& labels_flipped[q2pos]==-labels_flipped[i]){qb2pos=i;}
		else if(labels_flipped[i]<0 && labels_flipped[i]>-6 && qb3pos==-1){qb3pos=i; break;}} break;
			}

// NOTICE that for BH_interface mom_assg is base 1, as it'll be passed as input ind
		mom_assg.push_back(q1pos+1);
		mom_assg.push_back(qb2pos+1);
		mom_assg.push_back(q2pos+1);
		mom_assg.push_back(qb3pos+1);
		mom_assg.push_back(q3pos+1);
		mom_assg.push_back(qb1pos+1);
	} break;
	  case 220: case 221: case 222: case 223: case 224: case 225: {
		int qpos=-1,qbpos=-1,lpos=-1,lbpos=-1;
		std::vector<int> gpos;
		for(int i=0;i<labels_flipped.size();i++){
			if(labels_flipped[i]>10 && labels_flipped[i]<17)	{lpos=i;}
			else if(labels_flipped[i]<-10 && labels_flipped[i]>-17)	{lbpos=i;}
			else if(labels_flipped[i]==21)	{gpos.push_back(i);}
			else if(labels_flipped[i]>0 && labels_flipped[i]<6)	{qpos=i;}
			else	{qbpos=i;}
		}
		mom_assg.push_back(qpos+1);
		for(int k=0;k<gpos.size();k++){
			mom_assg.push_back(gpos[k]+1);
		}
		mom_assg.push_back(qbpos+1);
		mom_assg.push_back(lpos+1);
		mom_assg.push_back(lbpos+1);
	  } break;
	  
	case 240: case 241: case 242: {
		int qpos=-1,qbpos=-1,q2pos=-1,qb2pos=-1,lpos=-1,lbpos=-1;
		std::vector<int> gpos;
		for(int i=0;i<labels_flipped.size();i++){
			if(labels_flipped[i]>10 && labels_flipped[i]<17)	{lpos=i;}
			else if(labels_flipped[i]<-10 && labels_flipped[i]>-17)	{lbpos=i;}
			else if(labels_flipped[i]>0 && labels_flipped[i]<6 && qpos==-1){qpos=i;}
			else if(labels_flipped[i]>0 && labels_flipped[i]<6 && q2pos==-1){q2pos=i;}
			else if(labels_flipped[i]<0 && labels_flipped[i]>-6 && qbpos==-1){qbpos=i;}
			else if(labels_flipped[i]<0 && labels_flipped[i]>-6 && qb2pos==-1){qb2pos=i;}
			else if(labels_flipped[i]==21){gpos.push_back(i);}
		}
		// Z/gamma case
		if(photonZW!=3){
			switch (case4q){
				case 0: case 5: break;
				default:{
					if(labels_flipped[qpos]!=-labels_flipped[qbpos]){
						int qbp(qbpos),qb2p(qb2pos);
						qbpos=qb2p;
						qb2pos=qbp;
					}
				} break;
			}
		}
		// W case
		else{
			if(labels_flipped[qpos]==-labels_flipped[qbpos]){
				int qp(qpos),q2p(q2pos);
				int qbp(qbpos),qb2p(qb2pos);
				qpos=q2p;
				q2pos=qp;
				qbpos=qb2p;
				qb2pos=qbp;
			}
			else if(labels_flipped[qpos]==-labels_flipped[qb2pos]){
				int qp(qpos),q2p(q2pos);
				qpos=q2p;
				q2pos=qp;
			}
			else if(labels_flipped[q2pos]==-labels_flipped[qbpos]){
				int qbp(qbpos),qb2p(qb2pos);
				qbpos=qb2p;
				qb2pos=qbp;
			}
		}
		// NOTICE that for BH_interface mom_assg is base 1, as it'll be passed as input ind
		mom_assg.push_back(qpos+1);
		mom_assg.push_back(qb2pos+1);
		mom_assg.push_back(q2pos+1);
		mom_assg.push_back(qbpos+1);
		for(int k=0;k<gpos.size();k++)
			mom_assg.push_back(gpos[k]+1);
		mom_assg.push_back(lpos+1);
		mom_assg.push_back(lbpos+1);
	  } break;
	 case 260: {
		for(int k=0;k<fermion_ind.size();k++){ mom_assg.push_back(fermion_ind[k]+1);}
		} break;
	 case 100021: case 100022: case 100023:{
		int qpos=-1,qbpos=-1,ypos=-1;
		std::vector<int> gpos;
		for(int i=0;i<labels_flipped.size();i++){
			if(labels_flipped[i]==22)	{ypos=i;}
			else if(labels_flipped[i]==21)	{gpos.push_back(i);}
			else if(labels_flipped[i]>0 && labels_flipped[i]<6)	{qpos=i;}
			else	{qbpos=i;}
		}
		mom_assg.push_back(qpos+1);
		for(int k=0;k<gpos.size();k++)
			mom_assg.push_back(gpos[k]+1);
		mom_assg.push_back(qbpos+1);
		mom_assg.push_back(ypos+1);
	  } break;
	 case 100040:{
		int qpos=-1,qbpos=-1,q2pos=-1,qb2pos=-1,ypos=-1;
		std::vector<int> gpos;
		for(int i=0;i<labels_flipped.size();i++){
			if(labels_flipped[i]==22)	{ypos=i;}
			else if(labels_flipped[i]==21)	{gpos.push_back(i);}
			else if(labels_flipped[i]>0 && labels_flipped[i]<6 && qpos<0){
				qpos=i;
				for(int j=0;j<labels_flipped.size();j++){
					if(labels_flipped[i]==-labels_flipped[j]){
						qbpos=j;
						break;
					}
				}
			}
			else if(labels_flipped[i]>0 && labels_flipped[i]<6 && q2pos<0){
				q2pos=i;
				for(int j=0;j<labels_flipped.size();j++){
					if(labels_flipped[i]==-labels_flipped[j] && j!=qbpos){
						qb2pos=j;
						break;
					}
				}
			}
		}
		// NOTICE that for BH_interface mom_assg is base 1, as it'll be passed as input ind
		mom_assg.push_back(qpos+1);
		mom_assg.push_back(qb2pos+1);
		mom_assg.push_back(q2pos+1);
		mom_assg.push_back(qbpos+1);
		for(int k=0;k<gpos.size();k++)
			mom_assg.push_back(gpos[k]+1);
		mom_assg.push_back(ypos+1);
	  } break;
	case 100041:{
		int qpos=-1,qbpos=-1,q2pos=-1,qb2pos=-1, ypos=-1;
		std::vector<int> gpos;
		for(int i=0;i<labels_flipped.size();i++){
			if(labels_flipped[i]==22)	{ypos=i;}
			else if(labels_flipped[i]>0 && labels_flipped[i]<6 && qpos==-1){qpos=i;}
			else if(labels_flipped[i]>0 && labels_flipped[i]<6 && q2pos==-1){q2pos=i;}
			else if(labels_flipped[i]<0 && labels_flipped[i]>-6 && qbpos==-1){qbpos=i;}
			else if(labels_flipped[i]<0 && labels_flipped[i]>-6 && qb2pos==-1){qb2pos=i;}
			else if(labels_flipped[i]==21){gpos.push_back(i);}
		}
		// Z/gamma case
		if(photonZW!=3){
			switch (case4q){
				case 0: case 5: break;
				default:{
					if(labels_flipped[qpos]!=-labels_flipped[qbpos]){
						int qbp(qbpos),qb2p(qb2pos);
						qbpos=qb2p;
						qb2pos=qbp;
					}
				} break;
			}
		}
	// NOTICE that for BH_interface mom_assg is base 1, as it'll be passed as input ind
		mom_assg.push_back(qpos+1);
		mom_assg.push_back(qb2pos+1);
		mom_assg.push_back(q2pos+1);
		mom_assg.push_back(qbpos+1);
		for(int k=0;k<gpos.size();k++)
			mom_assg.push_back(gpos[k]+1);
		mom_assg.push_back(ypos+1);
	  } break;
	 default:{
		// NOTICE that for BH_interface mom_assg is base 1, as it'll be passed as input ind
		mom_assg.push_back(0+1);
		mom_assg.push_back(1+1);
		mom_assg.push_back(2+1);
		mom_assg.push_back(3+1);
		mom_assg.push_back(4+1);
		mom_assg.push_back(5+1);
		mom_assg.push_back(6+1);
		mom_assg.push_back(7+1);
	  } break;
	}

#if _VERBOSE
for(int kk=0;kk<mom_assg.size();kk++)
	_PRINT(mom_assg[kk]);
#endif

int color,tree_color;

switch (settings::BH_interface_settings::s_BH_color_mode)
	{
        case settings::BH_interface_settings::full_color: {color=0;tree_color=0;}; break;
        case settings::BH_interface_settings::leading_color : {color=1;
		switch (settings::BH_interface_settings::s_BH_tree_color_mode){
        		case settings::BH_interface_settings::full_color : {tree_color=0;}; break;
        		case settings::BH_interface_settings::leading_color : {tree_color=1;}; break;
			};
		}; break;
        case settings::BH_interface_settings::full_minus_leading_color : {color=2;tree_color=0;}; break;
	}


switch (pc){
case 41:{
	// to use IR_checked_OLHA's instead of OneLoopHelAmpl's
	CachedOLHA::use_IR_checked();
	return new BH_Ampl_2q2Q1g(case4q,color,tree_color,mom_assg,lo_or_nlo,this);
	} break;
case 42:{
	// to use IR_checked_OLHA's instead of OneLoopHelAmpl's
	CachedOLHA::use_IR_checked();
	return new BH_Ampl_2q2Q2g(case4q,color,tree_color,mom_assg,lo_or_nlo,this);
	} break;
case 60:{
	// to use IR_checked_OLHA's instead of OneLoopHelAmpl's
	CachedOLHA::use_IR_checked();
	return new BH_Ampl_2q2Q2P(case6q,color,tree_color,mom_assg,lo_or_nlo,this);
	} break;

//case 220:return new BH_Ampl_concrete(pc);
//case 221:return new BH_Ampl_ee3jet(mom_assg);

case 220:{
	// to use IR_checked_OLHA's instead of OneLoopHelAmpl's
	CachedOLHA::use_IR_checked();
	return new BH_Ampl_2q2l(up_down_quark,photonZW,color,tree_color,mom_assg,lo_or_nlo,this);
	} break;
case 221:{
	// to use IR_checked_OLHA's instead of OneLoopHelAmpl's
	CachedOLHA::use_IR_checked();
	return new BH_Ampl_2q1g2l(up_down_quark,photonZW,color,tree_color,mom_assg,lo_or_nlo,this);
	} break;
case 222:{
	// to use IR_checked_OLHA's instead of OneLoopHelAmpl's
	CachedOLHA::use_IR_checked();
	return new BH_Ampl_2q2g2l(up_down_quark,photonZW,color,tree_color,mom_assg,lo_or_nlo,this);
	} break;
case 223:{
	// to use IR_checked_OLHA's instead of OneLoopHelAmpl's
	CachedOLHA::use_IR_checked();
	return new BH_Ampl_2q3g2l(up_down_quark,photonZW,color,tree_color,mom_assg,lo_or_nlo,this);
	} break;
case 224:{
	// to use IR_checked_OLHA's instead of OneLoopHelAmpl's
	CachedOLHA::use_IR_checked();
	return new BH_Ampl_2q4g2l(up_down_quark,photonZW,color,tree_color,mom_assg,lo_or_nlo,this);
	} break;
case 225:{
	// to use IR_checked_OLHA's instead of OneLoopHelAmpl's
	CachedOLHA::use_IR_checked();
	return new BH_Ampl_2q5g2l(up_down_quark,photonZW,color,tree_color,mom_assg,lo_or_nlo,this);
	} break;
case 240:{
	// to use IR_checked_OLHA's instead of OneLoopHelAmpl's
	CachedOLHA::use_IR_checked();
	return new BH_Ampl_2q2Q2l(photonZW,case4q,color,tree_color,mom_assg,lo_or_nlo,this);
	 } break;
case 241:{
	// to use IR_checked_OLHA's instead of OneLoopHelAmpl's
	CachedOLHA::use_IR_checked();
	return new BH_Ampl_2q2Q1g2l(photonZW,case4q,color,tree_color,mom_assg,lo_or_nlo,this);
	} break;
case 242:{
	// to use IR_checked_OLHA's instead of OneLoopHelAmpl's
	CachedOLHA::use_IR_checked();
	return new BH_Ampl_2q2Q2g2l(photonZW,case4q,color,tree_color,mom_assg,lo_or_nlo,this);
	} break;
case 260:{
	// to use IR_checked_OLHA's instead of OneLoopHelAmpl's
	CachedOLHA::use_IR_checked();
	return new BH_Ampl_6q2l(photonZW,case6q,color,tree_color,mom_assg,lo_or_nlo,this);
	} break;
case 100021:{ 
	// to use IR_checked_OLHA's instead of OneLoopHelAmpl's
	CachedOLHA::use_IR_checked();
	return new BH_Ampl_2q1g1y(up_down_quark,color,mom_assg,lo_or_nlo,this);
	} break;
case 100022:{
	// to use IR_checked_OLHA's instead of OneLoopHelAmpl's
	CachedOLHA::use_IR_checked();
	return new BH_Ampl_2q2g1y(up_down_quark,color,tree_color,mom_assg,lo_or_nlo,this);
	} break;
case 100023:{
	// to use IR_checked_OLHA's instead of OneLoopHelAmpl's
	CachedOLHA::use_IR_checked();
	return new BH_Ampl_2q3g1y(up_down_quark,0,color,tree_color,mom_assg,lo_or_nlo,this);
	} break;
case 100040:{ 
	// to use IR_checked_OLHA's instead of OneLoopHelAmpl's
	CachedOLHA::use_IR_checked();
	return new BH_Ampl_2q2Q1y(up_down_quark,case4q,color,tree_color,mom_assg,lo_or_nlo,this);
	} break;
case 100041:{ 
	// to use IR_checked_OLHA's instead of OneLoopHelAmpl's
	CachedOLHA::use_IR_checked();
	return new BH_Ampl_2q2Q1g1y(photonZW,case4q,color,tree_color,mom_assg,lo_or_nlo,this);
	} break;
default : throw BHerror("Sorry, can't do that yet.");
}

}

class BH_Ampl_return_one : public BH_Ampl {
public:
		virtual double operator()(BHinput& in){return 1.;};
		virtual double get_single_pole(){return 1.;};
		virtual double get_double_pole(){return 1.;};
		virtual double get_finite(){return 1.;};
		virtual double get_born(){return 1.;};
		virtual ~BH_Ampl_return_one(){}
};

BH_Ampl_return_one dummy_BH2Sherpa;

BH_Ampl* BH_interface_impl_gridWarmup::new_ampl(const std::vector<int>& labels, QCDorder lo_or_nlo){
	return &dummy_BH2Sherpa;
}

class BH_Ampl_collectPS : public BH_Ampl {
	int d_process_locator;
	BH_interface_impl_collectPS* d_parent_p;
public:
		virtual double operator()(BHinput& in){};
		virtual double get_single_pole(){return 1.;};
		virtual double get_double_pole(){return 1.;};
		virtual double get_finite();
		BH_Ampl_collectPS(BH_interface_impl_collectPS* bhi,int loc):d_process_locator(loc), d_parent_p(bhi) {};
		virtual ~BH_Ampl_collectPS(){}
};

double BH_Ampl_collectPS::get_finite(){
	mom_conf& mc=*d_parent_p->get_mc();

#if STORE_ONLY_ONE_SUBPROCESS_PER_PS_POINT
	// First entry in d_ps_output_file is the subprocess called
	d_parent_p->d_ps_output_file<<d_process_locator<<" ";
	// Then print the PS point --- notice WARNING in the "else"
	if(d_parent_p->number_of_get_finite_calls==0){
		// Notice minus sign in incoming momenta, to revert BH's mom_conf convention
		d_parent_p->d_ps_output_file<<-real(mc.p(1).E())<<" "<<-real(mc.p(1).X())<<" "<<-real(mc.p(1).Y())<<" "<<-real(mc.p(1).Z())<<" ";
		d_parent_p->d_ps_output_file<<-real(mc.p(2).E())<<" "<<-real(mc.p(2).X())<<" "<<-real(mc.p(2).Y())<<" "<<-real(mc.p(2).Z())<<" ";
		for(int kk=3;kk<=mc.n();kk++){
			d_parent_p->d_ps_output_file<<real(mc.p(kk).E())<<" "<<real(mc.p(kk).X())<<" "<<real(mc.p(kk).Y())<<" "<<real(mc.p(kk).Z())<<" ";
		}
		d_parent_p->d_ps_output_file<<endl;
	}
	else{
		_WARNING("Unexpected behavior: get_finite() in BH_interface collectPS mode been called more than once ");
	}

	d_parent_p->number_of_get_finite_calls++;
#endif

	return 1.;
}

BH_Ampl* BH_interface_impl_collectPS::new_ampl(const std::vector<int>& labels, QCDorder lo_or_nlo){
	d_ampl_index++;
	ofstream logfile;
	logfile.open("collectPS.log",ios::app);
#if STORE_ONLY_ONE_SUBPROCESS_PER_PS_POINT
	if(d_ampl_index==1){
		time_t now;
		time(&now);
		logfile<<"\n// ## only one subprocess per PS point stored mode ## // run on: "<<asctime(localtime(&now))<<endl;
	}
#else
	if(d_ampl_index==1){
		time_t now;
		time(&now);
		logfile<<"\n// ## all subprocess per PS point stored mode ## // run on: "<<asctime(localtime(&now))<<endl;
	}
#endif
	logfile << "\n// process "<<d_ampl_index<<":"<<endl;
	logfile <<"	int array"<<d_ampl_index<<"[]={";
		for(int jj=0;jj<(labels.size()-1);jj++)
			logfile << labels[jj]<<",";
			logfile << labels[labels.size()-1]<<"};\n";
			logfile <<"	std::vector<int> prop_n"<<d_ampl_index<<"(array"<<d_ampl_index<<",array"<<d_ampl_index<<"+sizeof(array"<<d_ampl_index<<")/sizeof(int));\n";
			logfile <<"	processes.push_back(prop_n"<<d_ampl_index<<");\n";
	return new BH_Ampl_collectPS(this,d_ampl_index);
//	return &dummy_BH2Sherpa;
}

class BH_Ampl_echo : public BH_Ampl {
	int d_process_locator;
	BH_interface_impl_echo* d_parent_p;
	double d_last_result_2;
	double d_last_result_1;
	double d_last_result_0;
public:
		virtual double operator()(BHinput& in){};
		virtual double get_single_pole(){return 1.;};
		virtual double get_double_pole(){return 1.;};
		virtual double get_finite();
		BH_Ampl_echo(BH_interface_impl_echo* bhi,int loc):d_process_locator(loc), d_parent_p(bhi) {};
		virtual ~BH_Ampl_echo(){}
};

double BH_Ampl_echo::get_finite(){
#if STORE_ONLY_ONE_SUBPROCESS_PER_PS_POINT
	d_last_result_2 = 1.;
	d_last_result_1 = 1.;
	d_last_result_0=d_parent_p->ME_result;
	// for sanity check
	d_parent_p->d_last_process_called=d_process_locator;
	return d_last_result_0;
#else
// this part can be used if more than a subprocess per PS point is called
	mom_conf& mc=*d_parent_p->get_mc();

	int exyz,wmom;
	exyz=(d_process_locator-1)%4;
	wmom=(d_process_locator-1)/4 + 1;
	#if 0
	_PRINT(d_process_locator);
	_PRINT(exyz);
	_PRINT(wmom);
	#endif
	d_last_result_2 = 1;
	d_last_result_1 = 1;

	switch (exyz){
		case 0:{
			//_PRINT(mc.p(wmom).E());
			d_last_result_0 =mc.p(wmom).E().real();
			} break;
		case 1:{
			//_PRINT(mc.p(wmom).X());
			d_last_result_0 =mc.p(wmom).X().real();
			} break;
		case 2:{
			//_PRINT(mc.p(wmom).Y());
			d_last_result_0 =mc.p(wmom).Y().real();
			} break;
		case 3:{
			//_PRINT(mc.p(wmom).Z());
			d_last_result_0 =mc.p(wmom).Z().real();
			} break;
		default:{
			d_last_result_0 =1;
			} break;
	}
	return d_last_result_0;
#endif
}

BH_Ampl* BH_interface_impl_echo::new_ampl(const std::vector<int>& labels, QCDorder lo_or_nlo){
	d_ampl_index++;
	ofstream logfile;
	logfile.open("echo.log",ios::app);
#if STORE_ONLY_ONE_SUBPROCESS_PER_PS_POINT
	if(d_ampl_index==1){
		time_t now;
		time(&now);
		logfile<<"\n// ## only one subprocess per PS point stored mode ## // run on: "<<asctime(localtime(&now))<<endl;
	}
#else
	if(d_ampl_index==1){
		time_t now;
		time(&now);
		logfile<<"\n// ## all subprocess per PS point stored mode ## // run on: "<<asctime(localtime(&now))<<endl;
	}
#endif
	logfile<<"\n// process "<<d_ampl_index<<":"<<endl;
	logfile<<"	int array"<<d_ampl_index<<"[]={";
		for(int jj=0;jj<(labels.size()-1);jj++)
			logfile<<labels[jj]<<",";
		logfile<<labels[labels.size()-1]<<"};\n";
		logfile<<"	std::vector<int> prop_n"<<d_ampl_index<<"(array"<<d_ampl_index<<",array"<<d_ampl_index<<"+sizeof(array"<<d_ampl_index<<")/sizeof(int));\n";
		logfile <<"	processes.push_back(prop_n"<<d_ampl_index<<");\n";

	  return new BH_Ampl_echo(this,d_ampl_index);
}

BH_interface_impl_cached::BH_interface_impl_cached(){
	d_true_impl=new BH_interface_impl_normal();
//	d_true_impl->useSettingsTable(this->getSettingsTable());
//	d_settings_p=0;  // will prevents deleting
}

class cached_BH_Ampl : public BH_Ampl {
	BH_Ampl* d_ampl;
public:
	cached_BH_Ampl(BH_Ampl* ampl) : d_ampl(ampl){};	

	virtual double operator()(BHinput& in){d_ampl->operator ()(in);};
	virtual double get_single_pole(){return d_ampl->get_single_pole();};
	virtual double get_double_pole(){return d_ampl->get_double_pole();};;
	virtual double get_finite(){return d_ampl->get_finite();};;
	virtual double get_born(){return d_ampl->get_born();};
	virtual double get_single_pole_HP(){return d_ampl->get_single_pole_HP();};
	virtual double get_double_pole_HP(){return d_ampl->get_double_pole_HP();};;
	virtual double get_finite_HP(){return d_ampl->get_finite_HP();};;
	virtual double get_born_HP(){return d_ampl->get_born_HP();};
	virtual double get_single_pole_VHP(){return d_ampl->get_single_pole_VHP();};
	virtual double get_double_pole_VHP(){return d_ampl->get_double_pole_VHP();};;
	virtual double get_finite_VHP(){return d_ampl->get_finite_VHP();};;
	virtual double get_born_VHP(){return d_ampl->get_born_VHP();};
	virtual double getScaleVariationCoefficient(int logMuOrder){return d_ampl->getScaleVariationCoefficient(logMuOrder);};
    
    //subtraction terms
    virtual void set_partial_born(){return d_ampl->set_partial_born();};
    virtual void get_vals(vector<double* >& re_ampl, vector<double* >& im_ampl){ d_ampl->get_vals(re_ampl,im_ampl); return;};
    virtual void get_map(vector<vector<int> >& perm,vector<vector<int> >& hel){d_ampl->get_map(perm,hel); return;};
    virtual int get_order_qcd(){return d_ampl->get_order_qcd();};
    virtual int get_order_qed(){return d_ampl->get_order_qed();};

	virtual void dry_run(){};

	virtual ~cached_BH_Ampl(){}	;
};

BH_interface_impl_cached::~BH_interface_impl_cached(){
//	std::set<BH_Ampl*> amplitudes_sorted;
//	for (int ii=0;ii<d_amplitudes.size();ii++){
//		amplitudes_sorted.insert( d_amplitudes[ii]);
//	}
//	std::set<BH_Ampl*>::iterator it=amplitudes_sorted.begin();
//	while (it!=amplitudes_sorted.end()){
//		delete *it;
//		++it;
//	}

	//the deletion of the amplitudes is done by the true_impl interface

	delete d_true_impl;
};


BH_Ampl* BH_interface_impl_cached::new_ampl(const std::vector<int>& labels, QCDorder lo_or_nlo){
	BH_Ampl* candidate=d_true_impl->new_ampl(labels,lo_or_nlo);

	if(candidate==0) return 0;
	
	vector<vector<double> > ps;

	switch (labels.size()) {
		case 4:{
			double p_a1[] = {-2.29333281086030751,0.88305370744680835,1.96002342239748243,0.79868624301796119};
			double p_a2[] = {-2.22100900105464863,-0.83839402809274431,-1.97192484347455149,-0.584370471629133949};
			double p_a3[] = {-2.20789098262258162,1.64481285677059797,0.82065855183763165,-1.2230669640882361};
			double p_a4[] = {-2.30645082929237452,-1.60015317741653393,-0.83255997291470071,1.43738273547706334};
			vector<double> p1(p_a1,p_a1+4);
			vector<double> p2(p_a2,p_a2+4);
			vector<double> p3(p_a3,p_a3+4);
			vector<double> p4(p_a4,p_a4+4);
			ps.push_back(p1);
			ps.push_back(p2);
			ps.push_back(p3);
			ps.push_back(p4);
		}; break;
		case 5:{
			double p_a1[] = {-1.86058205404180962,-0.22322341633486098,-1.29381396176358134,-1.31832557381242467};
			double p_a2[] = {-2.22702886899219324,1.97843090981622047,-0.18090821896703733,-1.00635030417771722};
			double p_a3[] = {1.77552105445334353,-1.61888957780145734,0.46152052119123634,-0.56450895317284531};
			double p_a4[] = {-4.59937300385497873,4.17419448896838902,-0.96173464232271022,-1.67493249852413707};
			double p_a5[] = {-1.26375897363236766,-0.80009741768557219,-0.97450805959914479,-0.08523442629315951};
			vector<double> p1(p_a1,p_a1+4);
			vector<double> p2(p_a2,p_a2+4);
			vector<double> p3(p_a3,p_a3+4);
			vector<double> p4(p_a4,p_a4+4);
			vector<double> p5(p_a5,p_a5+4);
			ps.push_back(p1);
			ps.push_back(p2);
			ps.push_back(p3);
			ps.push_back(p4);
			ps.push_back(p5);
		}; break;
		case 6:{
			double p_a1[] = {-1.92955137629582831,-0.12176375318546036,-1.35203710358853989,-1.37125408757648828};
			double p_a2[] = {-2.3718946178199526,2.10154141143786404,-0.25155580416061273,-1.07057342179232001};
			double p_a3[] = {1.86478515042509431,-1.71633845344031231,0.51744205227939982,-0.51367274895690288};
			double p_a4[] = {-4.87600294030279301,3.29796758789333694,-3.20053955542535968,-1.62952785100596814};
			double p_a5[] = {-0.54186109016474053,-0.06227089168380983,0.5327311409203322,-0.07702797269268775};
			double p_a6[] = {-0.74836711407334168,0.46041941548318888,0.54677345447647505,-0.22159893671324953};
			vector<double> p1(p_a1,p_a1+4);
			vector<double> p2(p_a2,p_a2+4);
			vector<double> p3(p_a3,p_a3+4);
			vector<double> p4(p_a4,p_a4+4);
			vector<double> p5(p_a5,p_a5+4);
			vector<double> p6(p_a6,p_a6+4);
			ps.push_back(p1);
			ps.push_back(p2);
			ps.push_back(p3);
			ps.push_back(p4);
			ps.push_back(p5);
			ps.push_back(p6);
		}; break;
		case 7:{
			double p_a1[] = {-1.37845962162848452,0.899604391289195437,0.76146684945939796,0.71486439609741331};
			double p_a2[] = {-1.31550521424004713,-0.86154333881518911,-0.98581179913844106,-0.128343837855031049};
			double p_a3[] = {-0.483429888680200544,0.286893255432157653,-0.277486867061210162,-0.272759520210466919};
			double p_a4[] = {-0.321701443648813055,0.202997164217524277,-0.246144284682511691,-0.041194189924169683};
			double p_a5[] = {-0.471025810729965125,-0.286107020036279202,0.365488906741813979,-0.080162001649501396};
			double p_a6[] = {-0.337726474245189617,0.258662100312875037,-0.217145127094240915,-0.00104069546211045};
			double p_a7[] = {-1.080081218564363314,-0.42438444745227144,0.150942422417105687,0.981676965488630708};
			vector<double> p1(p_a1,p_a1+4);
			vector<double> p2(p_a2,p_a2+4);
			vector<double> p3(p_a3,p_a3+4);
			vector<double> p4(p_a4,p_a4+4);
			vector<double> p5(p_a5,p_a5+4);
			vector<double> p6(p_a6,p_a6+4);
			vector<double> p7(p_a7,p_a7+4);
			ps.push_back(p1);
			ps.push_back(p2);
			ps.push_back(p3);
			ps.push_back(p4);
			ps.push_back(p5);
			ps.push_back(p6);
			ps.push_back(p7);
		}; break;
		case 8:{
			double p_a1[] = {-0.677400017074605876,-0.250142496747423391,-0.218529533647501042,0.590376453949043703};
			double p_a2[] = {-0.890448994420059572,0.84042287009358795,0.169878789888167917,0.240270696992960786};
			double p_a3[] = {0.319059059309799145,0.282169596702112851,-0.132095668503444812,-0.068773078942627912};
			double p_a4[] = {0.591895986238114489,0.075279124768095475,-0.329199014134954976,0.486108959999209894};
			double p_a5[] = {0.695489395606314995,0.128490939776938918,0.58370742287648654,0.355641986096927112};
			double p_a6[] = {0.449781677244689127,-0.067132947945870905,0.07422384062513292,-0.438506038690458123};
			double p_a7[] = {-2.04102697550486404,0.10089801809451397,-1.68939376079989505,1.14085907352903102};
			double p_a8[] = {-1.58304815438871916,0.07057564195037425,1.44410643617734225,-0.6446837510500775};
			vector<double> p1(p_a1,p_a1+4);
			vector<double> p2(p_a2,p_a2+4);
			vector<double> p3(p_a3,p_a3+4);
			vector<double> p4(p_a4,p_a4+4);
			vector<double> p5(p_a5,p_a5+4);
			vector<double> p6(p_a6,p_a6+4);
			vector<double> p7(p_a7,p_a7+4);
			vector<double> p8(p_a8,p_a8+4);
			ps.push_back(p1);
			ps.push_back(p2);
			ps.push_back(p3);
			ps.push_back(p4);
			ps.push_back(p5);
			ps.push_back(p6);
			ps.push_back(p7);
			ps.push_back(p8);
		       }; break;
		case 9:{
			double p_a1[] = {-0.724029539784378082,-0.232674459973560293,-0.24351697489968873,0.640921877526500715};
			double p_a2[] = {-0.925896021004126494,0.86306259634180863,0.13749342014590363,0.305780895288754375};
			double p_a3[] = {0.313397289344500322,0.274286381522237675,-0.120818993354682674,-0.091583910839617861};
			double p_a4[] = {0.545232501029269833,0.061105453223224026,-0.308924051948866364,0.44509609512161666};
			double p_a5[] = {0.689905146431366202,0.111222770507794176,0.608408960360722244,0.305674898128453623};
			double p_a6[] = {0.487395926677445987,-0.078814343395845723,0.090933685373488494,-0.472307265962787639};
			double p_a7[] = {-1.81586645851486555,0.31768258179445032,-1.00852443447653242,1.47625446229554976};
			double p_a8[] = {-1.24170537476965713,0.08802623630309822,1.16631481734921562,-0.41688531550755247};
			double p_a9[] = {-0.62828459098656425,-0.14312094358671035,-0.53341353805713,-0.29954619042040699};
			vector<double> p1(p_a1,p_a1+4);
			vector<double> p2(p_a2,p_a2+4);
			vector<double> p3(p_a3,p_a3+4);
			vector<double> p4(p_a4,p_a4+4);
			vector<double> p5(p_a5,p_a5+4);
			vector<double> p6(p_a6,p_a6+4);
			vector<double> p7(p_a7,p_a7+4);
			vector<double> p8(p_a8,p_a8+4);
			vector<double> p9(p_a9,p_a9+4);
			ps.push_back(p1);
			ps.push_back(p2);
			ps.push_back(p3);
			ps.push_back(p4);
			ps.push_back(p5);
			ps.push_back(p6);
			ps.push_back(p7);
			ps.push_back(p8);
			ps.push_back(p9);
		}; break;
		default: cerr << "Caching not yet implemented for this case."; throw BHerror("Caching not yet implemented for this case.");
	}

	BHinput bhi(ps,1.);
	d_true_impl->operator ()(bhi);
	
	double res;
	switch (lo_or_nlo){
		case lo: res = candidate->get_born(); break;
		case nlo: res = candidate->get_finite(); break;		
	}
	for (int i=0;i<d_previous_values.size();i++){
		if ( isApproximatelyEqual(res,d_previous_values[i],1e-9)){
			BH_DEBUG_MESSAGE3("Reusing cached process (res=",res, ")");
			delete candidate;
			d_true_impl->remove_last();
			return new cached_BH_Ampl(d_amplitudes[i]); 	
		}
	}

// if we get there we have a new amplitude
	
	BH_DEBUG_MESSAGE3( "new amplitude (res=",res , ")" );
	d_previous_values.push_back(res);
	d_amplitudes.push_back(candidate);
	
	return new cached_BH_Ampl(candidate);
}

bool print_svn_version_from_files(std::ostream& os,const string& versionFile,const string& urlFile){
	ifstream svn_url(urlFile.c_str());
	ifstream svn_version(versionFile.c_str());

	if (!svn_url || !svn_version){
		return false;
	}
	string revision;
	svn_version >> revision;
	os << "SVN revision: " << revision << "\n";
	int pos_M = revision.find('M',0);
	if ( pos_M != string::npos ){
		os << "*******************************************\n";
		os << " WARNING: you are using a modified version!";
		os << "\n*******************************************\n";
	}
	int pos_colon = revision.find(':',0);
	if ( pos_colon != string::npos ){
		os << "*******************************************\n";
		os << " WARNING: you are using a mixed version!";
		os << "\n*******************************************\n";
	}
	string url;
	svn_url >> url; // skips "URL: "
	svn_url >> url;
	os << "SVN branch: \'" << url <<  "\'.\n" ;

	return true;
}


void print_svn_version(std::ostream& os){

	string versionFile=GetDataPath()+"/svnversion";
	string urlFile=GetDataPath()+"/svnurl";

	if (print_svn_version_from_files(os,versionFile,urlFile)){
		return;
	}
	versionFile=GetDataPath()+"/svnversion_dist";
	urlFile=GetDataPath()+"/svnurl_dist";
	if (print_svn_version_from_files(os,versionFile,urlFile)){
		return;
	}
	// this is the case when running the tests before the installation
	versionFile=GetSrcPath()+"/share/svnversion_dist";
	urlFile=GetSrcPath()+"/share/svnurl_dist";
	if (print_svn_version_from_files(os,versionFile,urlFile)){
		return;
	}

	os << "********************************************\n";
	os << "WARNING: could not find version information!";
	os << "\n********************************************\n";

}

void BH_interface::print_banner(){
	static bool alreadyDone=false;
	if (!alreadyDone){
		cout << "====================================" << endl;
		cout << " BlackHat                         " << endl;
		cout << "                                  " << endl;
		cout << " version info:                    " << endl;
		print_svn_version(cout);
		cout << "\nIf you use this program, please   " << endl;
		cout << "cite arXiv:0803.4180              " << endl;
		cout << "====================================" << endl;
		alreadyDone=true;
	}
}


BH_interface::BH_interface(){
	print_banner();
	settings::read_settings_from_file("BHsettings");
	switch (settings::BH_interface_settings::s_BH_interface_mode){
	case settings::BH_interface_settings::normal : {
		d_impl=new BH_interface_impl_normal();
		BH_DEBUG_MESSAGE("BH_interface created in normal mode");
	} break;
	case settings::BH_interface_settings::cached : {
		d_impl=new BH_interface_impl_cached();
		BH_DEBUG_MESSAGE("BH_interface created in cached mode");
	} break;

	case settings::BH_interface_settings::gridWarmup : {
		d_impl=new BH_interface_impl_gridWarmup();
		BH_DEBUG_MESSAGE("BH_interface created in gridWarmup mode");
	} break;
	case settings::BH_interface_settings::collectPS : {
		d_impl=new BH_interface_impl_collectPS();
		BH_DEBUG_MESSAGE("BH_interface created in collectPS mode");
	} break;
	case settings::BH_interface_settings::echo : {
		d_impl=new BH_interface_impl_echo();
		BH_DEBUG_MESSAGE("BH_interface created in echo mode");
	} break;
	}
}


BH_interface::BH_interface(const std::string& name){
	print_banner();
	//if empty string is passed ude again "BHsettings", which is the same as default value.
	if(name==""){settings::read_settings_from_file("BHsettings");}
	else { settings::read_settings_from_file(name);};

	switch (settings::BH_interface_settings::s_BH_interface_mode){
	case settings::BH_interface_settings::normal : {
		d_impl=new BH_interface_impl_normal();
		_MESSAGE("BH_interface created in normal mode");
	} break;
	case settings::BH_interface_settings::cached : {
		d_impl=new BH_interface_impl_cached();
		_MESSAGE("BH_interface created in cached mode");
	} break;
	case settings::BH_interface_settings::gridWarmup : {
		d_impl=new BH_interface_impl_gridWarmup();
		_MESSAGE("BH_interface created in gridWarmup mode");
	} break;
	case settings::BH_interface_settings::collectPS : {
		d_impl=new BH_interface_impl_collectPS();
		_MESSAGE("BH_interface created in collectPS mode");
	} break;
	case settings::BH_interface_settings::echo : {
		d_impl=new BH_interface_impl_echo();
		_MESSAGE("BH_interface created in echo mode");
	} break;
	}
}


BH_interface::~BH_interface(){
	delete d_impl;
}


template void BH_interface_impl::set(const std::string& name,double value);
template void BH_interface_impl::set(const std::string& name,int value);
template void BH_interface_impl::set(const std::string& name,bool value);



}


