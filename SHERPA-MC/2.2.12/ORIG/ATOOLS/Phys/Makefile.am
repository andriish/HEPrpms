
GITTAG = ATOOLS/Phys
include ../../git.make

pkglib_LTLIBRARIES       =	libToolsPhys.la

libToolsPhys_la_SOURCES       =	\
				Git_Info.C \
				Flavour.C \
				Ordering.C \
				Particle.C \
			        Simple_Polarisation_Info.C \
				Particle_List.C \
				NLO_Subevt.C \
				Decay_Info.C \
				Cluster_Leg.C \
				Cluster_Amplitude.C \
				Blob.C \
				Blob_List.C \
				Parton_Finder.C \
				Color_Tester.C \
				Momentum_Shifter.C \
				Flow.C \
				Color.C \
				Spinor.C \
				Fastjet_Helpers.C \
				Momenta_Stretcher.C \
				Weight_Info.C

localinc_HEADERS      =	\
			Ordering.H \
			Flavour_Tags.H \
			Flavour.H \
			Particle.H \
			Simple_Polarisation_Info.H \
			Particle_List.H \
			NLO_Subevt.H \
			Decay_Info.H \
			Cluster_Leg.H \
			Cluster_Amplitude.H \
			Flow.H \
			Blob.H \
			Blob_List.H \
			Parton_Finder.H \
			Color_Tester.H \
			Momentum_Shifter.H \
			Color.H \
			Spinor.H \
			Fastjet_Helpers.H \
			Momenta_Stretcher.H \
			Weight_Info.H

libToolsPhys_la_LIBADD = @CONDITIONAL_FASTJETLIBS@

libToolsPhys_la_CPPFLAGS = $(AM_CPPFLAGS) @CONDITIONAL_FASTJETINCS@
