/*
 * parent_diagrams_from_file.h
 *
 *  Created on: Oct 4, 2009
 *      Author: daniel
 */

#ifndef ME2_DIAGRAMS_FROM_FILE_H_
#define ME2_DIAGRAMS_FROM_FILE_H_

#include <vector>
#include <string>
#include <map>
#include "BH_utilities.h"
#include "assembly.h"
#include "cached_OLHA.h"
//#include "ME2.h"
using std::vector;
using std::pair;
namespace BH {

class process;
class cutD;
class plabel;
class particle_ID;
struct cross_term_entry;

std::string ME2_file_name(const std::vector<std::pair<int,int> >& particle_labels);
std::string PA_file_name(const std::vector<std::pair<int,int> >& particle_labels,bool verbose=true);
std::string GetAssemblyDataDirectory();
std::map<std::string,color_structure> init_cs_string_map();


//enum approx{born=1,virt};
//Virtual_SME* get_ME2_from_file(/*const std::string& filename,*/ const std::string& ME2_order_str, const std::vector<std::pair<int, int> >& pa_labels);
Virtual_SME* get_ME2_from_file(/*const string& filename, const std::string& ME2_order_str*/ QCDorder, const std::vector<std::pair<int, int> >&);

// puts all color structures and the corresponding processes from a file in the provided vectors
void get_color_structure_from_file(const std::string& filename,std::vector<std::string>& color_list,std::vector<std::vector<std::vector<plabel> > >& process_list);
particle_ID get_particle_ID_from_string(const std::string& str, bool cut_or_rat);

std::map<color_structure,std::string> init_cs_map();

bool is_nf_color_structure(color_structure);


void process_coupling_from_string(const std::string& process_str, 
		const std::string& coupling_str,
		std::vector<plabel>& process,
		std::vector<std::vector<std::pair<int,int> > >& coupling);

void partial_process_coupling_from_string(const std::string& process_str, 
		const std::string& coupling_str,
		std::vector<plabel>& process,
		std::vector<std::vector<std::pair<int,int> > >& coupling,
		std::vector<std::string > & color_info);
		//vector<size_t> & color_info);

bool sorted_permutation(const std::vector<int> perm, const std::vector<int> trace_begin, const std::vector<int> trace_end);
int free_orbit_size(const std::vector<int> trace_begin, const std::vector<int> trace_end);
void permutation_orbit(const std::vector< plabel>& plabels,const std::vector<int>& perm,std::vector<std::vector<int> >& perm_orbit, std::vector<std::string >& color_info);


bool compr(const cross_term_entry* cte1, const cross_term_entry* cte2);

void get_PA_from_file(CachedOLHA::partial_amplitude_cached* PA,
//void get_PA_from_file(partial_amplitude_cached* PA,
		const std::string& filename,
		const process & pro,
		const std::vector<int>& ind,
		const std::vector<std::vector<std::pair<int,int> > >& coupling,
		const std::vector<std::string >& color_info);


//CachedOLHA::partial_amplitude_cached* PA_from_file(
int PA_from_file(
//partial_amplitude_cached* PA_from_file(
	    Squared_ME* ME,
		const std::string& filename,
		std::string part_type,
		const std::vector<plabel> & pro,
		const std::vector<std::vector<pair<int,int> > >& coupling,
		const std::vector<std::string >& color_info,
		std::vector<kinematic_function*> prop_hel_fn);



struct coupling_process{
    friend  bool operator<(const coupling_process&,const coupling_process&); 
	std::vector<std::vector<std::pair<int,int> > > m_coupling;
	std::vector<std::vector<plabel> > m_plabels;
    coupling_process(std::vector<std::vector<pair<int,int> > >&, const std::vector<plabel>& );
};


struct cross_term_entry {
	std::vector<plabel> m_plabel;
	std::vector<std::vector<pair<int,int> > > m_coupling;
//	std::vector<size_t> m_color_info;
	std::vector<std::string > m_color_info;
	friend	bool operator<(const cross_term_entry& , const cross_term_entry&);
	friend	bool operator==(const cross_term_entry& , const cross_term_entry&);
	friend	bool compr(const cross_term_entry* , const cross_term_entry*);
	friend	bool equal(const cross_term_entry* , const cross_term_entry*);
	public:
	cross_term_entry(const std::vector<plabel>& labels,
			const std::vector<int>& perm,
			const std::vector<std::vector<pair<int,int> > >& coupling);	
	cross_term_entry(const std::vector<plabel>& labels,
			const std::vector<int>& perm,
			const std::vector<std::vector<pair<int,int> > >& coupling,
			//const std::vector<size_t>& color_info
			const std::vector<std::string >& color_info);	
	std::vector<kinematic_function*> m_coupling_function();
	~cross_term_entry(){};
};



class CrossTermLessThan {
    public:
        bool operator( )(const cross_term_entry* cte1, const cross_term_entry* cte2) const {
	        if(cte1==0||cte2==0) return false;
            else if ((*cte1) < (*cte2)) return(true);
            //else if ((*cte1) == (*cte2)) return(true);
            else return(false);
        }
};

struct Compr {
	bool operator()( const cross_term_entry* s1, const cross_term_entry* s2 ) const {
		return compr(s1,s2);
	}
};

	
class construction_cache{
	Squared_ME* m_ME;
	std::string m_PA_filename;
	public:
	std::map<cross_term_entry*,int,CrossTermLessThan > m_cross_terms;
	std::map<cross_term_entry*,int,CrossTermLessThan > m_cross_terms_loop;
	std::map<coupling_process,std::vector<kinematic_function*> > m_kinematic_functions;
//	std::vector<pair<cross_term_entry*,int> > m_cross_terms;
//	std::vector<pair<cross_term_entry*,int> > m_cross_terms_loop;
	int new_cross_term_entry(const std::vector<plabel>& labels,
		const std::vector<int>& perm,
		const std::vector<std::vector<pair<int,int> > >& coupling,
		const std::vector<std::string > & color_info, 
            	double & sign);
	int new_loop_cross_term_entry(const std::vector<plabel>& labels,
		const std::vector<int>& perm,
		const std::vector<std::vector<std::pair<int,int> > >& coupling,
		const std::vector<std::string >& color_info
//		const std::vector<size_t>& color_info
		);
	void add_gluon_permutations();
	construction_cache(Squared_ME* ME, const std::vector<std::pair<int,int> >& pa_labels): m_ME(ME),m_PA_filename(PA_file_name(pa_labels)){};
	~construction_cache();
};

std::vector<kinematic_function*> coupling_function(std::vector<std::vector<pair<int,int> > >& coupling, const std::vector<plabel>& labels);

kinematic_function* coupling_function_4(std::vector<pair<int,int> >& coupling, const std::vector<plabel>& labels);

}
#endif /* ME2_DIAGRAMS_FROM_FILE_H_ */
