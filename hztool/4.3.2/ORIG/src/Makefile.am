AUTOMAKE_OPTIONS = subdir-objects
if SPECIAL_OPT
   INCLUDE_SPECIAL = interfaces/hzhwavrg.F interfaces/hzpyavrg.F 
else
   INCLUDE_SPECIAL = 
endif


EVSHAPESSOURCE   = evshapes/deisrs1.F evshapes/devshp.F evshapes/dgeig.F \
                   evshapes/dtred2.F  evshapes/dgthru.F evshapes/dtql2.F
INTERFACESSOURCE = interfaces/hzdifkin.F interfaces/hzdjhepc.F interfaces/hzfilhep.F \
                   interfaces/hzfpomp.F  interfaces/hzgamad.F  interfaces/hzhacop.F  \
                   interfaces/hzheptop.F interfaces/hzhrwcop.F interfaces/hzhrwlop.F \
                   interfaces/hzlop.F    interfaces/hzluhepc.F interfaces/hzpycomp.F \
                   interfaces/hzluncop.F interfaces/hzparton.F interfaces/hzphocop.F \
                   interfaces/hzpycop.F  interfaces/hzpyhepc.F interfaces/pyevwt.F   \
                   interfaces/hzpytcop.F interfaces/hzpytlop.F interfaces/hzpylist.F \
                   interfaces/hzshcop.F $(INCLUDE_SPECIAL)   
JETFINDERSSOURCE = jetfinders/hzjetrad.F              jetfinders/ktclus/hzsubjm.F jetfinders/ktclus/ktvmul.F    \
                   jetfinders/hzjetshp.F              jetfinders/ktclus/ktbeam.F jetfinders/ktclus/ktwarn.F     \
                   jetfinders/hzjtfind.F              jetfinders/ktclus/ktbrei.F jetfinders/ktclus/ktwchs.F     \
                   jetfinders/hzjtname.F              jetfinders/ktclus/ktclur.F jetfinders/ktclus/ktwich.F     \
                   jetfinders/hzlijet.F               jetfinders/ktclus/ktclus.F jetfinders/ktclus/ktycut.F     \
                   jetfinders/jetprofile.F            jetfinders/ktclus/ktcopy.F jetfinders/ktclus/ktysub.F     \
                   jetfinders/deco/hedeci.F           jetfinders/ktclus/ktfram.F jetfinders/pucell/cfilpagp.F   \
                   jetfinders/deco/hedeco.F           jetfinders/ktclus/kthadr.F jetfinders/pucell/cluhad.F     \
                   jetfinders/deco/hedecx.F           jetfinders/ktclus/ktijoi.F jetfinders/pucell/convpagp.F   \
                   jetfinders/deco/hephan.F           jetfinders/ktclus/ktincl.F jetfinders/pucell/had_to_hcm.F \
                   jetfinders/deco/hgdeco.F           jetfinders/ktclus/ktinvt.F jetfinders/pucell/pacellgp.F   \
                   jetfinders/deco/qdecpm.F           jetfinders/ktclus/ktirec.F jetfinders/pxcone/pxaddv.F     \
                   jetfinders/deco/qemin2.F           jetfinders/ktclus/ktisub.F jetfinders/pxcone/pxang3.F     \
                   jetfinders/eucell/eucell.F         jetfinders/ktclus/ktjoin.F jetfinders/pxcone/pxcone.F     \
                   jetfinders/eucell/eucon1.F         jetfinders/ktclus/ktlbst.F jetfinders/pxcone/pxnew.F      \
                   jetfinders/eucell/euhepc.F         jetfinders/ktclus/ktmdpi.F jetfinders/pxcone/pxnorv.F     \
                   jetfinders/eucell/pconrad.F        jetfinders/ktclus/ktmerg.F jetfinders/pxcone/pxolap.F     \
                   jetfinders/field_stuart/jetalgo2.F jetfinders/ktclus/ktmmul.F jetfinders/pxcone/pxord.F      \
                   jetfinders/h1gpcone/h1gpcone.F     jetfinders/ktclus/ktmove.F jetfinders/pxcone/pxsame.F     \
                   jetfinders/h1qgcone/h1qgcone.F     jetfinders/ktclus/ktpair.F jetfinders/pxcone/pxsear.F     \
                   jetfinders/jade/dgfjad.F           jetfinders/ktclus/ktpmin.F jetfinders/pxcone/pxsorv.F     \
                   jetfinders/jade/dist.F             jetfinders/ktclus/ktreco.F jetfinders/pxcone/pxtry.F      \
                   jetfinders/jade/dvadd.F            jetfinders/ktclus/ktrrot.F jetfinders/pxcone/pxuvec.F     \
                   jetfinders/jade/dvcopy.F           jetfinders/ktclus/ktsing.F jetfinders/pxcone/pxzeri.F     \
                   jetfinders/jade/jadejet.F          jetfinders/ktclus/ktsmin.F jetfinders/pxcone/pxzerv.F     \
                   jetfinders/jclust/jclust.F         jetfinders/ktclus/ktunit.F
NLOSOURCE        = nlo/hzho1cop.F nlo/hzsnlo.F
UTILSOURCE       = util/ap2.F        util/hzdiskin.F util/hzhcmtol.F  util/hzlcomp.F  \
                   util/hzpt.F       util/mcdistr.F  util/getbinno.F  util/hzdot.F    \
                   util/hzhinfo.F    util/hzlihep.F  util/mcmeans.F  \
                   util/get_kd_bin.F util/hzdphi.F   util/hzhinrm.F   util/hzlulist.F \
                   util/mcpsi.F    util/h1distr.F   util/hzdr.F     \
                   util/hzhnorm.F    util/hzmeanhi.F util/hzpytnmi.F  util/mcpsifxd.F \
                   util/h1histo.F    util/hzeebeam.F util/hzhrwnmi.F  util/hzmghepc.F \
                   util/hzrap.F      util/mcpsihi.F  util/h1means.F   util/hzeegamn.F \
                   util/hznmi.F 	 util/hzibeam.F util/hzrchepc.F   util/mcrho.F    \
                   util/hzabsvec.F   util/hzeekin.F   util/hzpcomp.F  util/hzipgam.F  \
                   util/hzscaprod.F  util/mcsmhi.F   util/hzboost.F   util/hzeta.F    \
                   util/hzidelec.F   util/hzp.F      util/hzsvhepc.F  util/seterror.F \
                   util/hzbrtini.F   util/hzet.F     util/hzidntro.F  util/hzphi.F    \
                   util/hzterm2.F    util/vp2.F      util/hzbrtola.F  util/hzevent.F  \
                   util/hzinvmas.F   util/hzphmang.F util/hzterm.F    util/zspsi.F    \
                   util/hzchisqa.F   util/hzgetvar.F util/hzphmrot.F \
                   util/hztheta.F    util/zspsifxd.F util/hzchisq.F   util/hzhadgap.F \
                   util/hzipgamn.F   util/hzphokin.F util/hzvecprod.F util/zsrho.F    \
                   util/hzdeta.F     util/hzhcmini.F util/hzlchge.F   util/hzpscon.F  \
                   util/hzvers.F     util/hzppbbeam.F util/hzbrtola2.F \
                   util/hzcdrates.F	 util/hzDRates.F   util/hzcalculus.F  util/hzxgam2j.F \
                   util/hzstatus.F   util/hzfillssub.F util/hzkhisnorm.F  util/hzkhisfil.F \
                   util/hzkbins.F    util/hzkhisdef.F util/hzcmtola2.F util/get_var.F
                   

TEMPLATE_SOURCE  = hztemplate.F
EMC_SOURCE       = emc/hzc87112.F
HERA_SOURCE      = hera/hz00017.F hera/hz95036.F hera/hz97191.F  hera/hz99091.F    \
                   hera/hz00035.F hera/hz95072.F hera/hz97196.F  hera/hz99094.F    \
                   hera/hz00040.F hera/hz95084.F hera/hz97210.F  hera/hz99101.F    \
                   hera/hz00166.F hera/hz95108.F hera/hz98018.F  hera/hz99126.F    \
                   hera/hz00174.F hera/hz95115.F hera/hz98029.F  hera/hz99193.F    \
                   hera/hz00181.F hera/hz95194.F hera/hz98038.F  hera/hzh0006017.F \
                   hera/hz01064.F hera/hz95219.F hera/hz98044.F  hera/hzh0108047.F \
                   hera/hz01100.F hera/hz95221.F hera/hz98050.F  hera/hzh0302034.F \
                   hera/hz01220.F hera/hz96039.F hera/hz98076.F  hera/hzh0307071.F \
                   hera/hz01225.F hera/hz96076.F hera/hz98085.F  hera/hzh0312057.F \
                   hera/hz02023.F hera/hz96094.F hera/hz98085p.F hera/hzh0404033.F \
                   hera/hz02079.F hera/hz96122.F hera/hz98087.F  hera/hzh0408149.F \
                   hera/hz02228.F hera/hz96138.F hera/hz98092.F  hera/hzh9505001.F \
                   hera/hz03015.F hera/hz96160.F hera/hz98121.F  hera/hzh9810020.F \
                   hera/hz03094.F hera/hz96215.F hera/hz98143.F  hera/hzi02849.F   \
                   hera/hz03160.F hera/hz97098.F hera/hz98162.F  hera/hzth001.F    \
                   hera/hz97108.F hera/hz98169.F  hera/hzth002.F    \
                   hera/hz94033.F hera/hz97158.F hera/hz98204.F  hera/hzth003.F    \
                   hera/hz94176.F hera/hz97164.F hera/hz98205.F  hera/hz95007.F    \
                   hera/hz97179.F hera/hz98210.F hera/hz95033.F  hera/hz97183.F    \
                   hera/hz99057.F hera/hz00145.F hera/hz01073.F  hera/hz07062.F\
                   hera/hzh0508055.F hera/hzh0507089.F hera/hzh0603014.F hera/hzh0608042.F hera/hzh0401010.F \
                   hera/hzh0407018.F hera/hzh0512014.F \
                   hera/hzh0502010.F hera/hzh09012226.F hera/hzh0701023.F
LC_SOURCE        = lc/hznlc1.F lc/hznlc2.F
LEP_SOURCE       = lep/hzburgin1.F lep/hzc00109.F  lep/hzc98091.F lep/hzh0301013.F \
                   lep/hzburgin2.F lep/hzc96132.F  lep/hzc98113.F lep/hzpn443.F
SPS_SOURCE       = sps/hzc88172a.F sps/hzc88172b.F sps/hzc93153.F sps/hzprt154247.F
TEVATRON_SOURCE  = tevatron/hzh0307080.F  tevatron/hzh0001021.F tevatron/hzh9807014.F  \
                   tevatron/hzh9907009.F tevatron/hzf01211e.F  tevatron/hzh0010026.F  \
                   tevatron/hzh9807018.F tevatron/hzh9912022.F tevatron/hzf89201e.F   \
                   tevatron/hzh0412071.F tevatron/hzh9905024.F tevatron/hzplb435453.F \
                   tevatron/hzh0404004.F

TOOLSSOURCES = $(EVSHAPESSOURCE) $(INTERFACESSOURCE) $(JETFINDERSSOURCE) $(NLOSOURCE) $(UTILSOURCE_NORIVET) $(UTILSOURCE) 
PAPERSSOURCES = $(TEMPLATE_SOURCE) $(EMC_SOURCE) $(HERA_SOURCE) $(LC_SOURCE) $(LEP_SOURCE) $(SPS_SOURCE) $(TEVATRON_SOURCE)

lib_LTLIBRARIES = libhztool.la
AM_LDFLAGS = $(VERSIONFLAGS)
AM_FFLAGS = -I$(top_srcdir)/inc $(MYFFLAGS)
libhztool_la_SOURCES = $(TOOLSSOURCES) $(PAPERSSOURCES)
