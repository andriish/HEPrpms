// -*- C++ -*-
//
// This file is part of HepMC3
// Copyright (C) 2014-2019 The HepMC collaboration (see AUTHORS for details)
//
#ifndef Pythia6_Pythia6ToHepMC2_H
#define Pythia6_Pythia6ToHepMC2_H
#include <sys/stat.h>
#ifdef  DUMMYPYTHIA6TOHEPMC2
extern "C" {

    int hepmc2_delete_writer_(const int & position)
    {
        return -1;
    }
    int hepmc2_convert_event_(const int & position)
    {
        return -1;
    }
    int hepmc2_write_event_(const int & position)
    {
        return -1;
    }
    int hepmc2_clear_event_(const int & position)
    {
        return -1;
    }
    int hepmc2_set_cross_section_(const int & position, const double& x, const double& xe, const int& n1, const int& n2)
    {
        return -1;
    }

    int hepmc2_set_pdf_info_(const int & position, const int& parton_id1, const int& parton_id2, const double& x1, const double& x2,
                             const double& scale_in, const double& xf1, const double& xf2,
                             const int& pdf_id1, const int& pdf_id2)
    {
        return -1;
    }
    int hepmc2_set_event_number_(const int & position,int& a)
    {
        return -1;
    }
    int hepmc2_set_hepevt_address_(int* a)
    {
        return -1;
    }
    int hepmc2_set_attribute_int_(const int & position, const int & attval, const char* attname)
    {
        return -1;
    }
    int hepmc2_set_attribute_double_(const int & position, const double & attval, const char* attname)
    {
        return -1;
    }
    int hepmc2_new_writer_(const int & position, const int & mode, const char* ffilename)
    {
        return  -1;
    }
    int hepmc2_new_weight_(const int & position, const char* name)
    {
        return -1;
    }
    int hepmc2_set_weight_by_index_(const int & position, const double& val, const int & pos)
    {
        return -1;
    }
    int hepmc2_set_weight_by_name_(const int & position, const double& val, const char* name)
    {
        return -1;
    }
}


#else
#include <iostream>
#include "HepMC/PythiaWrapper.h"
#include "HepMC/IO_HEPEVT.h"
#include "HepMC/IO_GenEvent.h"
#include "HepMC/GenEvent.h"
#include "HepMC/IO_AsciiParticles.h"
#include "HepMC/HEPEVT_Wrapper.h"
#include "PythiaHelper.h"

std::map<int, std::pair<HepMC::IO_GenEvent*, HepMC::GenEvent*> > hepmc2_gWriters;
HepMC::IO_HEPEVT  hepmc2_gHEPEVT;
HepMC::GenEvent* hepmc2_gWriters_get_event(const int & position)
{
    return    hepmc2_gWriters[position].second;
}
using namespace HepMC;
extern "C" {
    int getorig_(int &a);
    int hepmc2_delete_writer_(const int & position)
    {
        if (hepmc2_gWriters.count(position) != 0) {
            if (hepmc2_gWriters[position].second) delete hepmc2_gWriters[position].second;
            if (hepmc2_gWriters[position].first) delete hepmc2_gWriters[position].first;
            hepmc2_gWriters[position].second = nullptr;
            hepmc2_gWriters[position].first = nullptr;
        }
        return 0;
    }
    int hepmc2_convert_event_(const int & position)
    {
        hepmc2_gHEPEVT.set_trust_mothers_before_daughters( true );
        GenEvent* event_hepmc2 = hepmc2_gWriters[position].second;
        if (event_hepmc2) delete event_hepmc2;
        event_hepmc2 = hepmc2_gHEPEVT.read_next_event();
        /** This algorithm below removes empty particles left in the HEPEVT table by cascade 
          * The particles have pdg id=0 and status=0
          */
        std::vector<GenVertex*> v_remove;
        for ( auto part = event_hepmc2->particles_begin(); part != event_hepmc2->particles_end(); ++part ) 
        {
		 if (!(*part)) continue;
         if ((*part)->pdg_id() == 0 && (*part)->status() == 0) { 
			 GenVertex* v = (*part)->production_vertex();
			 if (v && v->particles_out_size() == 1 ) v_remove.push_back(v);
	      } 
	    }
	    for (auto v: v_remove) { if (v) { event_hepmc2->remove_vertex(v); delete v;} }
	    /** End of algorithm */
        hepmc2_gWriters[position].second  = event_hepmc2;
        event_hepmc2->use_units(HepMC::Units::GEV, HepMC::Units::MM);
        //Set beams
        event_hepmc2->barcode_to_particle(1)->set_status(4);
        event_hepmc2->barcode_to_particle(2)->set_status(4);

        return 0;
    }
    int hepmc2_write_event_(const int & position)
    {
        (*hepmc2_gWriters[position].first) << hepmc2_gWriters[position].second;
        return 0;
    }
    int hepmc2_clear_event_(const int & position)
    {
        hepmc2_gWriters[position].second->clear();
        return 0;
    }
    int hepmc2_set_cross_section_(const int & position, const double& x, const double& xe, const int& n1, const int& n2)
    {
        const double xsecval = x;
        const double xsecerr = xe ;
        HepMC::GenCrossSection cross;
        cross.set_cross_section( xsecval, xsecerr );
        hepmc2_gWriters[position].second->set_cross_section( cross );
        return 0;
    }

    int hepmc2_set_pdf_info_(const int & position, const int& parton_id1, const int& parton_id2, const double& x1, const double& x2,
                             const double& scale_in, const double& xf1, const double& xf2,
                             const int& pdf_id1, const int& pdf_id2)
    {
        HepMC::PdfInfo pdf( parton_id1, parton_id2, x1, x2, scale_in, xf1, xf2, pdf_id1, pdf_id2);
        hepmc2_gWriters[position].second->set_pdf_info(pdf);
        return 0;
    }
    int hepmc2_set_event_number_(const int & position,int& a)
    {
        hepmc2_gWriters[position].second->set_event_number(a);
        return 0;
    }

    int hepmc2_set_hepevt_address_(int* a)
    {
        return 0;
    }
    int hepmc2_set_attribute_int_(const int & position, const int & attval, const char* attname)
    {
        std::string sta(attname);
        GenEvent* event_hepmc2 = hepmc2_gWriters[position].second;
        if (sta == std::string("mpi")) {
            event_hepmc2->set_mpi(attval);
            return 0;
        }
        if (sta == std::string("signal_process_id")) {
            event_hepmc2->set_signal_process_id(attval);
            return 0;
        }
        if (sta.substr(0, 13) == std::string("random_states")) {
            std::vector<long> rs = event_hepmc2->random_states();    //Dummy version
            rs.push_back(attval);
            event_hepmc2->set_random_states(rs);
            return 0;
        }
        return 0;
    }
    int hepmc2_set_attribute_double_(const int & position, const double & attval, const char* attname)
    {
        std::string sta(attname);
        GenEvent* event_hepmc2 = hepmc2_gWriters[position].second;
        if (sta == std::string("alphaQED")) {
            event_hepmc2->set_alphaQED(attval);
            return 0;
        }
        if (sta == std::string("alphaQCD")) {
            event_hepmc2->set_alphaQCD(attval);
            return 0;
        }
        if (sta == std::string("event_scale")) {
            event_hepmc2->set_event_scale(attval);
            return 0;
        }
        return 0;
    }

    int hepmc2_new_writer_(const int & position, const int & mode, const char* ffilename)
    {

        int r_position = position;
        if (r_position == 0)
        {
            if (hepmc2_gWriters.size() == 0) r_position = 1;
            if (hepmc2_gWriters.size() != 0) r_position = hepmc2_gWriters.rend()->first + 1;
        }
        if (hepmc2_gWriters.count(r_position) != 0) {
            printf("Error in %s: Writer at position %i already exists\n", __FUNCTION__, r_position);
            exit(1);
        }
        
        std::ofstream * fifostream = nullptr;
#if defined(__linux__) || defined(__darwin__)|| defined(__APPLE__) || defined(__FreeBSD__) || defined(__sun)
        /// The idea below is to overwrite the output file unless it is FIFO.
        /// FIFO will be opened in "append" mode.
        struct stat   buffer;
        if (stat(ffilename, &buffer) == 0 && S_ISFIFO(buffer.st_mode)) fifostream = new std::ofstream(ffilename);
#endif 
        hepmc2_gWriters[r_position] = fifostream ? std::pair<IO_GenEvent*, GenEvent*>( new HepMC::IO_GenEvent(*fifostream), new GenEvent(Units::GEV, Units::MM)) 
                                                 : std::pair<IO_GenEvent*, GenEvent*>( new HepMC::IO_GenEvent(ffilename, std::ios::out), new GenEvent(Units::GEV, Units::MM));
        return  r_position;
    }
    int hepmc2_new_weight_(const int & position, const char* name)
    {
        if (hepmc2_gWriters.count(position) == 0) {
            printf("Warning in %s: Writer at position %i does not exist\n", __FUNCTION__, position);
            return 1;
        }
        hepmc2_gWriters[position].second->weights().push_back(1.0);
        return 0;
    }
    int hepmc2_set_weight_by_index_(const int & position, const double& val, const int & pos)
{   if (hepmc2_gWriters.count(position) == 0) {
            printf("Warning in %s: Writer at position %i does not exist\n", __FUNCTION__, position);
            return 1;
        }
        if (hepmc2_gWriters[position].second->weights().size() <= pos)  {
            printf("Out of bounds\n");
            return 1;
        }
        hepmc2_gWriters[position].second->weights()[pos] = val;
        return 0;
    }
    int hepmc2_set_weight_by_name_(const int & position, const double& val, const char* name)
    {
        if (hepmc2_gWriters.count(position) == 0) {
            printf("Warning in %s: Writer at position %i does not exist\n", __FUNCTION__, position);
            return 1;
        }
        hepmc2_gWriters[position].second->weights()[name] = val;
        return 0;
    }
}
#endif
#endif
