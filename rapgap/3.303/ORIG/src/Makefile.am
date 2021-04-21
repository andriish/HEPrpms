## Copy.configureright (C) 2006 Free Software Foundation, Inc.
## This Makefile.am is free software; the Free Software Foundation
## gives unlimited permission to copy, distribute and modify it.

# AM_FFLAGS = -I$(top_srcdir)/include -fno-automatic -finit-local-zero -fno-backslash -fno-globals -ff90 -fpic
# for gfortran
# AM_FFLAGS = -I$(top_srcdir)/include  -fno-automatic -fno-backslash -fpic
AM_FFLAGS = -I$(top_srcdir)/include -I$(PYTHIA_SOURCE) $(MYFFLAGS)
AM_CPPFLAGS = -I$(srcdir)/../include -I$(HEPMC)/include

AUTOMAKE_OPTIONS = subdir-objects

EXTRA_DIST =heracles466/flgint.forig  heracles466/hsevtg.forig heracles466/lystfu.forig \
		heracles466/flqint.forig  heracles466/hsmain.forig \
		heracles466/gadap.forig   heracles466/hsrndm.forig \
            heracles466/hsblkd.fsave \
            heracles466/README \
            sophia466/datdec.f sophia466/param_ini.f sophia466/sophia.4.6.6.forig

bin_PROGRAMS = rapgap33 rapgap_hepmc

rapgap33_SOURCES =  rapgap/rgmain.F  rapgap/drn.F  rapgap/pyr.F rapgap/pyname.F \
            rapgap/upinit.F   rapgap/upevnt.F rapgap/pydata.f

rapgap_hepmc_SOURCES =  rapgap/rgmain-hepmc.F rapgap/drn.F  rapgap/pyr.F rapgap/pyname.F \
            rapgap/upinit.F   rapgap/upevnt.F rapgap/pydata.f 

if NO_HEPMC
   conv_source=rapgap/convhepmc-dummy.cc     rapgap/convhepmc-helpers.F
else
   conv_source=rapgap/convhepmc.cc     rapgap/convhepmc-helpers.F
endif

lib_LTLIBRARIES = librapgap33.la

librapgap33_la_SOURCES = \
		rapgap/aalam.F    rapgap/lhaini.F    rapgap/rgstga.F \
		rapgap/alphas.F   rapgap/eleqcdc.F   rapgap/lmeps.F     rapgap/pytcha.F \
		rapgap/eleqq.F     rapgap/lmeps99.F   rapgap/qcdgrid.F \
		rapgap/eleqqb.F    rapgap/lprikt.F    rapgap/qcdgridi.F \
		rapgap/aricha.F   rapgap/eleqqf.F    rapgap/lremh.F     rapgap/qcdmix.F \
		rapgap/eleqqg.F    rapgap/lscale.F    rapgap/raend.F \
		rapgap/blepin.F   rapgap/eleqqgg.F   rapgap/ralmkt.F \
		rapgap/colres.F   rapgap/eleqql.F    rapgap/partdf.F    rapgap/rapcha.F \
		rapgap/cutg.F     rapgap/eleres.F    rapgap/partdh.F    rapgap/rapgap.F \
		rapgap/dfridr.F   rapgap/elqcdcnl.F  rapgap/partdi.F    rapgap/raphi.F \
		rapgap/dfun.F     rapgap/elqqfnl.F   rapgap/partdi99.F  rapgap/rastfu.F \
		rapgap/dhformf.F  rapgap/elqqlnl.F   rapgap/parths.F    rapgap/rat2di.F \
		rapgap/diffr1.F   rapgap/event.F     rapgap/parti.F     rapgap/rghaprep.F \
		rapgap/diffr2.F   rapgap/eventep.F   rapgap/pdfgrid.F    \
		rapgap/diffr3.F   rapgap/eventpp.F   rapgap/phase.F     rapgap/rgq2w2mx.F \
		rapgap/diffr30.F       rapgap/pomstr.F    rapgap/rgsatrap.F \
		rapgap/diffr4.F   rapgap/fxn1.F      rapgap/ppmeps.F    rapgap/rgsatrev.F \
		rapgap/diffr5.F   rapgap/fxnb.F      rapgap/pqcd.F      rapgap/rgupevnt.F \
		rapgap/diffr6.F   rapgap/gadap.F     rapgap/pqcdi.F     rapgap/rgupinit.F \
		rapgap/diffr7.F   rapgap/gadap2.F    rapgap/pqcdqqb.F   rapgap/rlu.F \
		rapgap/diffr8.F   rapgap/gadapf.F    rapgap/pqpm.F      rapgap/rndm.F \
		rapgap/dir1.F     rapgap/graini.F    rapgap/prodiff.F   rapgap/rystfu.F \
		rapgap/dir2.F     rapgap/grawq2.F    rapgap/ptime.F     rapgap/steer.F \
		rapgap/dir3.F     rapgap/h1rn.F      rapgap/rggamm.F    rapgap/usdiffr.F \
		rapgap/dir4.F     rapgap/hercha.F    rapgap/rgremn.F    rapgap/xpq30.F \
            rapgap/dirh0.F 	rapgap/eleh0.F	 rapgap/dirxv1.F	  rapgap/elexv1.F \
            rapgap/dirvqq.F 	rapgap/elevqq.F \
            rapgap/dirxvq.F   rapgap/elevqg.F \
            rapgap/difxvq.F   rapgap/difxv1.F \
		rapgap/dir99.F    rapgap/xpqhf.F \
		rapgap/dot.F      rapgap/rasspa.F \
		rapgap/dot1.F     rapgap/rgstfe.F    rapgap/draprn.F\
            rapgap/rapgen.F   rapgap/intga.F     rapgap/rgstfu.F \
            util/Ctq4Pdfm.F  util/Ctq6Pdfm.F   util/minuts.F \
		util/Ctq5Pdfm.F  util/getintPDF.F  util/strpriv.F \
            actw/actwfit.F  actw/evlrnfe.F  actw/pardis.F  \
		actw/ratint.F  actw/warnr.F \
		actw/evlrd.F    actw/nextun.F   actw/polint.F  actw/rdupd.F \
            gammapdf/grspar.F  gammapdf/rystga.F  gammapdf/sasgam.F \
            gluon/blueml.F  gluon/ccfmstfu.F  gluon/d_xgx.F  gluon/ktstfu.F  gluon/runiglu.F \
            gluon/ryssha.F gluon/dhfint.F \
		h1qcd/fluxh1.F  h1qcd/i_2006_fitb.F  h1qcd/i_nlo_q3g1.F  h1qcd/qcd_2006.F \
		h1qcd/h12006flux.F   h1qcd/i_lo_q3g0.F    h1qcd/i_nlo_q3g3.F  h1qcd/xyinter.F \
		h1qcd/h1qcd2006.F    h1qcd/i_lo_q3g1.F    h1qcd/pion_init.F \
		h1qcd/h1qcdfit.F     h1qcd/i_lo_q3g3.F    h1qcd/pion_xpq.F \
		h1qcd/i_2006_fita.F  h1qcd/i_nlo_q3g0.F   h1qcd/qcd_1994.F \
            hardpom/dgrv_nln.F  hardpom/f2dhw.F  hardpom/f2md.F \
		hardpom/f2blw.F     hardpom/f2mcd.F \
            nikzak/nikzak.F \
            pdiss/decdel.F  pdiss/decrho.F  pdiss/pdisdcvm.F  pdiss/rgrambo.F \
		pdiss/deceta.F  pdiss/fragpj.F  pdiss/pdismass.F   pdiss/rgrng.F \
		pdiss/decnst.F  pdiss/fragpx.F  pdiss/prodiffvm.F  pdiss/rkno.F \
		pdiss/decpi0.F  pdiss/pdisdc.F  pdiss/ranbw.F      pdiss/splitp.F \
            rho/dboost.F  rho/elerho.F  rho/gdeca3.F  rho/gensph.F  rho/p0togg.F \
            satrap/compcross.F  satrap/phifunc.F    satrap/satsigtot.F satrap/swavet.F \
		satrap/fourdot.F    satrap/ran2body.F   satrap/saturpar.F   satrap/threedot.F \
		satrap/inputkine.F  satrap/ran3body.F   satrap/sigdif.F \
		satrap/intwave.F    satrap/ranq2w2mx.F  satrap/swaveg.F \
		satrap/partkine.F   satrap/ranuni.F     satrap/swavel.F \
            sca/sca_dat.F  sca/sca_main.F  sca/sca_pdf.F  sca/scapdf.F \
		heracles466/brasse.f     heracles466/hsclm2.f  heracles466/hsgenm.f  heracles466/hssign.f \
		heracles466/d01ajf.f     heracles466/hsclm3.f  heracles466/hsglow.f  heracles466/hsspen.f \
		heracles466/dfnc00.f     heracles466/hsclm4.f  heracles466/hsgupp.f  heracles466/hssrgg.f \
		heracles466/dfncii.f     heracles466/hscln.f   heracles466/hshadq.f  heracles466/hssrgz.f \
		heracles466/dialfs.f     heracles466/hscmw.f   heracles466/hsinil.f  heracles466/hssrww.f \
		heracles466/diflop.f     heracles466/hscmwz.f  heracles466/hsinit.f  heracles466/hssrzz.f \
		heracles466/djgevt.F     heracles466/hscphl.f  heracles466/hsiwz1.f  heracles466/hsstal.f \
		heracles466/djgvar.F     heracles466/hscwll.f  heracles466/hsiwz2.f  heracles466/hsstbk.f \
		heracles466/dx1fcf.f     heracles466/hscwlr.f  heracles466/hsixy.f   heracles466/hsstdl.f \
		heracles466/f2allm.f     heracles466/hscwql.f  heracles466/hsk1k3.f  heracles466/hsstr1.f \
		heracles466/fiuser.f     heracles466/hscwqr.f  heracles466/hsk1ts.f  heracles466/hsstrf.f \
		heracles466/flgint.F     heracles466/hscxsm.f  heracles466/hsl1k3.f  heracles466/hsterm.F \
		heracles466/flintg.f     heracles466/hsd0.f    heracles466/hsl1ts.f  heracles466/hstpar.f \
		heracles466/flipol.f     heracles466/hsd13c.f  heracles466/hsloqs.f  heracles466/hstrit.f \
		heracles466/flqint.F     heracles466/hsdelo.f  heracles466/hslts1.f  heracles466/hstsk1.f \
		heracles466/fltabl.f     heracles466/hsdelx.f  heracles466/hslts2.f  heracles466/hstsk2.f \
		heracles466/fltint.f     heracles466/hsdout.f  heracles466/hslufl.f  heracles466/hsuser.F \
		heracles466/hsdqds.f  heracles466/hslzk1.f  heracles466/hswcut.F \
		heracles466/gausk1.f     heracles466/hsdqdv.f  heracles466/hslzk2.f  heracles466/hswg1l.f \
		heracles466/gausk2.f     heracles466/hsdsgq.f  heracles466/hslzk3.f  heracles466/hswg1r.f \
		heracles466/heracl466.F  heracles466/hsdtin.f  heracles466/hslzts.f  heracles466/hswg2l.f \
		heracles466/hs46ini.F    heracles466/hsduwa.f  heracles466/hswg2r.f \
		heracles466/hsacpt.F     heracles466/hsduwq.f  heracles466/hsnc22.f  heracles466/hswgtx.f \
		heracles466/hsbcga.f     heracles466/hsel22.f  heracles466/hsncg1.f  heracles466/hswpdf.f \
		heracles466/hsbkin.f     heracles466/hselco.f  heracles466/hsncg2.f  heracles466/hswrpa.f \
		heracles466/hselg1.f  heracles466/hsnrat.f  heracles466/hswrsa.f \
		heracles466/hsbrnc.f     heracles466/hselg2.f  heracles466/hsomax.f  heracles466/hsxmax.f \
		heracles466/hsbxca.f     heracles466/hselk1.f  heracles466/hsomin.f  heracles466/hsxsct.f \
		heracles466/hsbxcv.f     heracles466/hselk2.f  heracles466/hspgfx.f  heracles466/hsxsmn.f \
		heracles466/hsbxi0.f     heracles466/hsenuw.f  heracles466/hsprlg.f  heracles466/lnstrf.F \
		heracles466/hsbxi5.f     heracles466/hsestm.f  heracles466/hspver.f  heracles466/ltimex.F \
		heracles466/hscc22.f     heracles466/hsevtg.f  heracles466/hsrndm.f  heracles466/lystfu.F \
		heracles466/hsccba.f     heracles466/hsf2bk.f  heracles466/hsrnin.f  heracles466/parinv.f \
		heracles466/hsccbq.f     heracles466/hsf2dl.f  heracles466/hsrnou.f  heracles466/pepevt.F \
		heracles466/hsccg1.f     heracles466/hsfcms.f  heracles466/hsrnst.f  heracles466/rf12.f \
		heracles466/hsccg2.f     heracles466/hsfhfb.f  heracles466/hsrnte.f  heracles466/rslac.f \
		heracles466/hscckl.f     heracles466/hsfie0.f  heracles466/hssab0.f  heracles466/sfecfe.f \
		heracles466/hsccms.f     heracles466/hsfiel.f  heracles466/hssab1.f  heracles466/sigvmes.f \
		heracles466/hsccqf.f     heracles466/hsfiv1.f  heracles466/hssetf.f  heracles466/stein.f \
		heracles466/hsccqi.f     heracles466/hsfiv2.f  heracles466/hssetp.f  heracles466/strfbs.f \
		heracles466/hsccsa.f     heracles466/hsfiv3.f  heracles466/hssfgz.f  heracles466/svtot.f \
		heracles466/hsccsq.f     heracles466/hsfivc.f  heracles466/hssfww.f  heracles466/vegas.f \
		heracles466/hscir.f      heracles466/hsfivm.f  heracles466/hssfzz.f  heracles466/vmesnuc.f \
		heracles466/hsckmx.f     heracles466/hsflab.f  heracles466/hssgcc.f  heracles466/wabc.f \
		heracles466/hsclab.f     heracles466/hsflav.f  heracles466/hssgel.f \
		heracles466/hsclm1.f     heracles466/hsfone.f  heracles466/hssgnc.f \
		sophia466/breitwigner.f   sophia466/icon_sib_pdg.f  sophia466/probangle.f \
		sophia466/check_event.f   sophia466/initial.f       sophia466/proc_twopart.f \
		sophia466/crossection.f   sophia466/lund_frag.F     sophia466/res_decay3.f \
	      sophia466/lund_get.F      sophia466/dec_inter3.f    sophia466/lund_put.F \
		sophia466/dec_proc2.f     \
		sophia466/dec_res2.f      sophia466/pl.f            sophia466/scatangle.f \
		sophia466/decpar.f        sophia466/po_altra.f      sophia466/singleback.f \
		sophia466/decsib.f        sophia466/po_mshell.f      \
		sophia466/djgnoe.F        sophia466/po_rndbet.f     sophia466/sophia.f \
		sophia466/djgsph.F        sophia466/po_rndgam.f     sophia466/twoback.f \
		sophia466/ef.f            sophia466/po_selsx2.f     sophia466/valences.f \
		sophia466/gamma_h.f       sophia466/po_trans.f  \
		sophia466/gauss.f         sophia466/po_xlam.f \
		sophia466/icon_pdg_sib.f  sophia466/print_event.f \
            $(conv_source) 

#		rapgap/analys.F  rapgap/frag.F 

LDADD = ./librapgap33.la ../bases51/libbases.la \
        $(HEPMCLIB) $(ARIADNELIB) $(PYTHIALIB) $(ARIADNELIB) $(PDFLIB) ../misc/libmycern.la -lstdc++ -lm


all:
#	cp rapgap33 ../bin/. 
