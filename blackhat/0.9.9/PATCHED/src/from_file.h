/*
 * parent_diagrams_from_file.h
 *
 *  Created on: Oct 4, 2009
 *      Author: daniel
 */

#ifndef FROM_FILE_H_
#define FROM_FILE_H_

#include <vector>
#include <string>
#include <map>
#include "BH_utilities.h"
#include "particles.h"

namespace BH {

class process;
class cutD;
class plabel;
class particle_ID;



std::string NoSpaces(std::string s);
particle* particle_type_from_string(const std::string& s);
std::map<std::string,particle*> init_particle_map();
particle_ID get_particle_ID_from_string(const std::string& str);
plabel plabel_from_string(const std::string& s);

std::string GetParentDataDirectory();
std::string get_worker_dir(const std::string& subdir);

void read_processes(const std::string& input,	std::vector<std::vector<plabel> >& labels);



particle_ID map_massless_to_massive(particle_ID pID);

std::map<color_structure,std::string> init_cs_map();

std::string myHash(const std::string& input,int maxLength=10);


}


#endif /* PARENT_DIAGRAMS_FROM_FILE_H_ */
