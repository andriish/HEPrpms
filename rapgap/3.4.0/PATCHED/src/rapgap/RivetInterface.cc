#ifdef  DUMMYRIVETINTERFACE
extern "C" {
    int rivet_interface_version_()
    {
        return 0;
    }
    int rivet_init_(char* rname1)
    {
        return 0;
    }
    int rivet_init_first_event_()
    {
        return 0;
    }
    int rivet_run_(const int &  id)
    {
        return 0;
    }
    int rivet_add_analysis_(char* ana)
    {
        return   0;
    }
    int rivet_done_(char* filename1)
    {
        return 0;
    }
}
#else
#include <iostream>
#include <set>
#include <vector>
#include <string>
#include "Rivet/Rivet.hh"
#include "Rivet/Config/RivetConfig.hh"
#ifdef RIVET_ENABLE_HEPMC_3
#include "HepMC3/GenEvent.h"
#else
#include "HepMC/GenEvent.h"
#endif
#ifdef RIVET_ENABLE_HEPMC_3
#define   RIVET_HEPMC_VERSION   3
/**  HepMC3 event to reads from*/
HepMC3::GenEvent* rivetevent=NULL;
extern HepMC3::GenEvent* hepmc3_gWriters_get_event(const int & position);
#else
#define   RIVET_HEPMC_VERSION    2
/**  HepMC event to reads from*/
HepMC::GenEvent* rivetevent=NULL;
extern HepMC::GenEvent* hepmc2_gWriters_get_event(const int & position);
#endif

extern "C" {
    int rivet_interface_version_()
    {
        return RIVET_HEPMC_VERSION;
    }

    /**The name of the file where the histograms are dumped.*/
    std::string filename;
    /** Analyses with optional analysis parameters.*/
    std::set<std::string> analyses;
    /**The Rivet object.*/
    Rivet::AnalysisHandler * rivet=NULL;
    /** Run name*/
    std::string rname;
    /**Ignore beams flag.*/
    bool igBeam=true;

    int rivet_init_(char* rname1) {
        if ( rivet ) return 0;
        rname=std::string(rname1);
        rivet = new Rivet::AnalysisHandler(rname);
#ifdef RIVET4
        rivet->setCheckBeams(!igBeam);
#else
        rivet->setIgnoreBeams(igBeam);
#endif
        Rivet::addAnalysisLibPath(".");
        for (std::set<std::string>::iterator it = analyses.begin();
                it != analyses.end(); ++it) {
            rivet->addAnalysis(*it);
        }
        return 0;
    }
    int rivet_init_first_event_(const int &  id)
    {

#ifdef RIVET_ENABLE_HEPMC_3
        rivetevent=hepmc3_gWriters_get_event(id);
        if (!rivetevent) {
            puts("Something is wrong with the first event!");
            return 1;
        }
        rivet->init(*rivetevent);
#else
        rivetevent=hepmc2_gWriters_get_event(id);
        if (!rivetevent) {
            puts("Something is wrong with the first event!");
            return 1;
        }
        rivet->init(*rivetevent);
#endif

        return 0;
    }
    int rivet_run_(const int &  id) {
#ifdef RIVET_ENABLE_HEPMC_3
        rivetevent=hepmc3_gWriters_get_event(id);
        if (!rivetevent) {
            puts("Something is wrong with event!");
            return 1;
        }
        if (!rivetevent->particles().size()) {
            printf("Something is wrong with particles!   %i\n",id);
            return 2;
        }
#else
        rivetevent=hepmc2_gWriters_get_event(id);
        if (!rivetevent) {
            puts("Something is wrong with event!");
            return 1;
        }
        if (!rivetevent->particles_size()) {
            printf("Something is wrong with particles!   %i\n",id);
            return 2;
        }
#endif
        if (!rivetevent->cross_section()) {
            puts("Something is wrong with cross-section!");
            return 3;
        }
        rivet->analyze(*rivetevent);
        return 0;
    }
    int rivet_add_analysis_(char* ana)
    {
        if (!ana) return analyses.size();
        std::string z(ana);
        if (z.length()>0) analyses.insert(z);
        return   analyses.size();
    }
    int rivet_done_(char* filename1) {
        if ( !rivet ) return 0;
        filename=std::string(filename1);
        rivet->finalize();
        rivet->writeData(filename);
        delete rivet;
        rivet = NULL;
        return 0;
    }
}
#endif
