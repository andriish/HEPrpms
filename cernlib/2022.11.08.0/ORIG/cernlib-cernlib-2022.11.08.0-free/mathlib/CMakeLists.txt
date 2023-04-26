
########################################################################
#
#  Automatically or semiautomaticall generated, do not edit.
#
########################################################################
# The following input was used
# bvsl/bvslftn/Imakefile
# gen/a/Imakefile
# gen/b/Imakefile
# gen/c/Imakefile
# gen/d/Imakefile
# gen/divon/Imakefile
# gen/e/Imakefile
# gen/f/Imakefile
# gen/g/Imakefile
# gen/h/Imakefile
# gen/j/Imakefile
# gen/m/Imakefile
# gen/s/Imakefile
# gen/u/Imakefile
# gen/v/Imakefile
# gen/x/Imakefile
# hbook/hdiff/Imakefile

########################################################################
set_package_flags(mathlib)
#mathlib/bvsl/bvslftn/Imakefile
 
#ORIGINAL #if defined(CERNLIB_SGI)
if (CERNLIB_SGI)
endif()
  set(mathlib_bvsl_bvslftn_FSRC aintgb.F alosb.F andb.F andntb.F binvec.F cntob.F cntzb.F copyb.F cprsb.F dalosb.F dcopyb.F ddotb.F dgthrb.F dmod3b.F dotb.F drangb.F dscalb.F dscttb.F dsxpyb.F dsxyb.F
 dvsetb.F dvxpyb.F dxypwzb.F dylosb.F dyloxb.F gthrb.F idlosb.F intgb.F iylosb.F iyloxb.F nandb.F norb.F notb.F oneb.F orb.F ornotb.F rangb.F rjctb.F scalb.F scttb.F smod3b.F sxpyb.F sxyb.F vsetb.F vxpyb.F
 xorb.F xypwzb.F ylosb.F yloxb.F zerob.F)
if (CERNLIB_HPUX)
endif()
  list(TRANSFORM mathlib_bvsl_bvslftn_FSRC PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/../mathlib/bvsl/bvslftn/")
 
#The original Imake file below included files:pilot.h
#Those were NOT processed.
#mathlib/gen/a/Imakefile
 
#include "pilot.h"
#ORIGINAL #if defined(CERNLIB_QF2C)
if (CERNLIB_QF2C)
  set(mathlib_gen_a_FSRC dreal.F)
#ORIGINAL #if !defined(CERNLIB_LINUX)
if (NOT CERNLIB_LINUX)
  set(mathlib_gen_a_FSRC ${SRCS_F} dcmplx.F dconjg.F dimag.F gcmplx.F gconjg.F gimag.F greal.F)
endif()
endif()
#ORIGINAL #if defined(CERNLIB_SGI)
if (CERNLIB_SGI)
endif()
  list(TRANSFORM mathlib_gen_a_FSRC PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/../mathlib/gen/a/")
 
#The original Imake file below included files:pilot.h
#Those were NOT processed.
#mathlib/gen/b/Imakefile
 
#include "pilot.h"
  set(mathlib_gen_b_FSRC asinh64.F atg.F binom64.F dplnml.F prmfct.F rplnml.F rsrtnt64.F)
#ORIGINAL #if defined(CERNLIB_DOUBLE)
if (CERNLIB_DOUBLE)
  list(APPEND mathlib_gen_b_FSRC  asinh.F binom.F rsrtnt.F)
else()
  list(APPEND mathlib_gen_b_FSRC  asinh128.F binom128.F rsrtnt128.F)
endif()
if (CERNLIB_HPUX)
endif()
#ORIGINAL #if defined(CERNLIB_SGI)
if (CERNLIB_SGI)
endif()
  list(TRANSFORM mathlib_gen_b_FSRC PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/../mathlib/gen/b/")
 
#The original Imake file below included files:pilot.h
#Those were NOT processed.
#mathlib/gen/c/Imakefile
 
#include "pilot.h"
  set(mathlib_gen_c_FSRC aslgf64.F atani64.F besi064.F besj064.F bsir364.F bsir464.F bsja64.F bzejy64.F c311bd.F cbsja64.F cdigam64.F celfun64.F celint64.F cexpin64.F cgamma64.F cgplg64.F clogam64.F clogok.F
 cpolyz64.F cpolyzd.F cpsipg.F crit.F csqrtk.F cwerf64.F cwhitm64.F dawson64.F dbska64.F dclaus64.F dilog64.F elfun64.F fconc64.F ferfr64.F freq64.F frsin64.F gagnc64.F gammf64.F gapnc64.F hypgm.F leg1.F
 legfn.F legor.F legv.F legz.F nic311.F nzerfz64.F qfunct.F reli164.F reli1c64.F reli264.F reli364.F religc64.F relikc64.F rexpin64.F rgam.F rmullz64.F rpsipg64.F rrizet64.F rrteq4.F rsnleq.F rteq3.F rteq364.F
 rteq464.F rtheta64.F sinint64.F snleq64.F strh064.F theta1.F trdz.F wpsipg.F zerox64.F)
#ORIGINAL #if defined(CERNLIB_DOUBLE)
if (CERNLIB_DOUBLE)
  list(APPEND mathlib_gen_c_FSRC  atani.F besi0.F besj0.F bsir3.F bsir4.F bsja.F cbsja.F cdigam.F celfun.F celint.F cexpin.F cgamma.F cgplg.F clogam.F cwerf.F cwhitm.F freq.F gammf.F raslgf.F rbzejy.F rclaus.F rdawsn.F rdilog.F relfun.F reli1.F reli1c.F rexpin.F rfconc.F rferfr.F rfrsin.F rgapnc.F rmullz.F rpsipg.F rrizet.F rsinin.F rstrh0.F rtheta.F rzerox.F r1dp.F r2dp.F r3dp.F r4dp.F r5dp.F r6dp.F r7dp.F r8dp.F wclbes.F)
#ORIGINAL #if defined(CERNLIB_IBM)
if (CERNLIB_IBM)
  list(APPEND mathlib_gen_c_FSRC  alogam.F)
else()
  list(APPEND mathlib_gen_c_FSRC  gamma.F algama.F)
endif()
else()
  list(APPEND mathlib_gen_c_FSRC  atani128.F aslgf128.F besi0128.F besj0128.F bsir3128.F bsir4128.F bska.F dbska.F bzejy128.F cbsja128.F cdigam128.F cexpin128.F cgplg128.F cgamma128.F clogam128.F cwerf128.F cwhitm128.F dawson.F dawson128.F dclaus.F deli1128.F dilog.F dilog128.F dpsipg128.F erf128.F fconc128.F ferdr128.F ferfr.F rexpin128.F freq128.F frsin.F frsin128.F gammf128.F gapnc.F gapnc128.F snleq128.F strh0.F strh0128.F reli1c128.F rmullz128.F rteq3128.F rtheta128.F rteq4128.F sinint.F sinint128.F welfun128.F zerox128.F)
endif()
#ORIGINAL #if defined(CERNLIB_IBMVM)
if (CERNLIB_IBMVM)
  list(APPEND mathlib_gen_c_FSRC  cbsja128.F)
endif()
#ORIGINAL #if defined(CERNLIB_SINGLE)
if (CERNLIB_SINGLE)
  list(APPEND mathlib_gen_c_FSRC  cclbes.F r1sp.F r2sp.F r3sp.F r4sp.F r5sp.F r6sp.F r7sp.F)
endif()
#ORIGINAL #if defined(CERNLIB_QUAD)
if (CERNLIB_QUAD)
  list(APPEND mathlib_gen_c_FSRC  gamma128.F algama128.F bsja128.F)
endif()
#ORIGINAL #if defined(CERNLIB_IBM)
if (CERNLIB_IBM)
  list(APPEND mathlib_gen_c_FSRC  dlogam.F)
else()
  list(APPEND mathlib_gen_c_FSRC  gamma64.F algama64.F)
endif()
#ORIGINAL #if defined(CERNLIB_HPUX)
if (CERNLIB_HPUX)
  list(APPEND mathlib_gen_c_FSRC  erf64.F erf.F)
endif()
#ORIGINAL #if defined(CERNLIB_IBMRT)
if (CERNLIB_IBMRT)
  list(APPEND mathlib_gen_c_FSRC  erf64.F erf.F)
endif()
#ORIGINAL #if defined(CERNLIB_SOLARIS)
if (CERNLIB_SOLARIS)
  list(APPEND mathlib_gen_c_FSRC  erf64.F erf.F)
endif()
#ORIGINAL #if defined(CERNLIB_WINNT)
if (CERNLIB_WINNT)
  list(APPEND mathlib_gen_c_FSRC  erf64.F erf.F)
endif()
#ORIGINAL #if defined(CERNLIB_LINUX)
if (CERNLIB_LINUX)
  list(APPEND mathlib_gen_c_FSRC  erf64.F erf.F)
endif()
#ORIGINAL #if defined(CERNLIB_VAXVMS)
if (CERNLIB_VAXVMS)
  list(APPEND mathlib_gen_c_FSRC  erf64.F erf.F)
endif()
if (CERNLIB_HPUX)
endif()
#ORIGINAL #if defined(CERNLIB_IBMRT)
if (CERNLIB_IBMRT)
endif()
#ORIGINAL #if defined(CERNLIB_QFAPOGEE)
if (CERNLIB_QFAPOGEE)
endif()
#ORIGINAL #if defined(CERNLIB_SGI)
if (CERNLIB_SGI)
endif()
#ORIGINAL #if defined(CERNLIB_QMVAOS)
if (CERNLIB_QMVAOS)
endif()
  list(TRANSFORM mathlib_gen_c_FSRC PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/../mathlib/gen/c/")
 
#The original Imake file below included files:pilot.h
#Those were NOT processed.
#mathlib/gen/d/Imakefile
 
#include "pilot.h"
  set(mathlib_gen_d_CSRC dummy.c)
  set(mathlib_gen_d_FSRC arithm.F c128.F c256.F c512.F c64.F cauchy64.F cft.F cfstft.F rfstft.F cgauss.F d107d1.F d107r1.F d501l1.F d501l2.F d501n1.F d501n2.F d501p1.F d501p2.F d501sf.F d700su.F d701bd.F
 dadapt.F deqbs64.F deqmr64.F deriv64.F dfunft.F dgs56p.F dmaxlk.F dsumsq.F elanal.F elpahy.F epdbps.F epdchk.F epdchn.F epdcj.F epde1.F epdecc.F epdfix.F epdin.F epditr.F epdje.F epdloc.F epdsrt.F epdtab.F
 errorf.F esolve.F fcn1.F fftrc.F fumili.F gauss.F i128.F i32.F i64.F linsq.F mconv.F mft.F minfc.F minsq.F monito.F newro.F radapt.F radmul.F rca.F rfrdh164.F rfrdh264.F rft.F rgquad.F rgs56p.F rgset.F
 riwiad.F riwibd.F rknys64.F rknysd.F rkstp.F rpa.F rps.F rrkstp.F rsa.F rtriint.F s128.F s256.F s512.F s64.F scal.F sgz.F simps.F synt.F traper.F triint.F vd01a.F)
#ORIGINAL #if defined(CERNLIB_DOUBLE)
if (CERNLIB_DOUBLE)
  list(APPEND mathlib_gen_d_FSRC  cgauss64.F dgquad.F dgset.F gauss64.F minfc64.F rcauch.F rdeqbs.F rdeqmr.F rderiv.F rfrdh1.F rkstp64.F simps64.F triint64.F)
else()
  list(APPEND mathlib_gen_d_FSRC  cgauss128.F cauchy.F cauchy128.F deqbs128.F deqmr128.F deriv128.F minfc128.F rfrdh128.F rkstp128.F simps128.F triint128.F)
endif()
#ORIGINAL #if defined(CERNLIB_QUAD)
if (CERNLIB_QUAD)
  list(APPEND mathlib_gen_d_FSRC  gauss128.F)
endif()
if (CERNLIB_HPUX)
endif()
#ORIGINAL #if defined(CERNLIB_SGI)
if (CERNLIB_SGI)
endif()
  list(TRANSFORM mathlib_gen_d_FSRC PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/../mathlib/gen/d/")
  list(TRANSFORM mathlib_gen_d_CSRC PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/../mathlib/gen/d/")
 
#mathlib/gen/divon/Imakefile
 
  set(mathlib_gen_divon_FSRC addbnd.F bfgs.F bndopt.F bndtst.F bounds.F bucmve.F bufopt.F delbnd.F delete.F delslv.F divon.F dvcopy.F dvdot.F dvnbkd.F dvnopt.F exmbuc.F feasmv.F feqn.F fun.F genpnt.F grdcmp.F
 intgrl.F ldlsol.F locsch.F modchl.F mulchk.F mxstep.F newptq.F nmdchl.F nocut.F nodaud.F orthvc.F partn.F quad.F quasi.F ranums.F recpar.F rlen.F settol.F shrnk.F split.F treaud.F tredmp.F tstext.F usrint.F
 usrtrm.F)
#ORIGINAL #if defined(CERNLIB_SGI)
if (CERNLIB_SGI)
endif()
  list(TRANSFORM mathlib_gen_divon_FSRC PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/../mathlib/gen/divon/")
 
#The original Imake file below included files:pilot.h
#Those were NOT processed.
#mathlib/gen/e/Imakefile
 
#include "pilot.h"
  set(mathlib_gen_e_FSRC cheb.F checf64.F chsum64.F dchpws.F decomp.F dlsqp1.F dlsqp2.F dlsqpm.F dspap1.F dspap2.F dspcd1.F dspcd2.F dspin1.F dspin2.F dspkn1.F dspkn2.F dspnb1.F dspnb2.F dspps1.F dspps2.F
 dspvd1.F dspvd2.F dtrgsm.F dummy.F lfit.F lfitw.F lkkspl.F lsqqr.F maxdze.F maxize.F ndigit.F parlsq.F proc1.F prod1.F prod2.F rchpws.F rcspln64.F rlsqp1.F rlsqp2.F rlsqpm.F rtrgsm.F slv.F splan0.F splan1.F
 splan2.F splan3.F splas1.F splas2.F trisum.F)
#ORIGINAL #if defined(CERNLIB_DOUBLE)
if (CERNLIB_DOUBLE)
  list(APPEND mathlib_gen_e_FSRC  chsum.F dchebn.F rchecf.F rcspln.F)
else()
  list(APPEND mathlib_gen_e_FSRC  chsum128.F rchebn.F rcspln128.F)
endif()
#ORIGINAL #if defined(CERNLIB_QUAD)
if (CERNLIB_QUAD)
  list(APPEND mathlib_gen_e_FSRC  checf128.F)
endif()
#ORIGINAL #if defined(CERNLIB_SGI)
if (CERNLIB_SGI)
endif()
  list(TRANSFORM mathlib_gen_e_FSRC PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/../mathlib/gen/e/")
 
#The original Imake file below included files:pilot.h
#Those were NOT processed.
#mathlib/gen/f/Imakefile
 
#include "pilot.h"
  set(mathlib_gen_f_FSRC bakvec.F balanc.F balbak.F bisect.F cbabk2.F cbal.F cinvit.F combak.F comhes.F comlr.F comlr2.F dircos.F eiscg1.F eiscg2.F eisch1.F eisch2.F eisch3.F eisch4.F eisrg1.F eisrg2.F
 eisrs1.F eisrs2.F eisrs3.F eisrs4.F eisst1.F eisst2.F eisst3.F eisst4.F elmbak.F elmhes.F eltran.F figi.F figi2.F gather.F hqr.F hqr2.F htribk.F htridi.F imtql1.F imtql2.F invit.F mxdipr.F ortbak.F orthes.F
 ortran.F polrot.F ratqr.F rlhoin.F tinvit.F tql1.F tql2.F trbak1.F tred1.F tred2.F tsturm.F )
#ORIGINAL #if defined(CERNLIB_DOUBLE)
if (CERNLIB_DOUBLE)
  list(APPEND mathlib_gen_f_FSRC  ilsum.F lihoin64.F scatter.F wheneq.F whenfge.F whenfgt.F whenfle.F whenflt.F whenige.F whenigt.F whenile.F whenilt.F whenne.F)
else()
  list(APPEND mathlib_gen_f_FSRC  lihoin128.F)
endif()
#ORIGINAL #if defined(CERNLIB_UNIX) || defined(CERNLIB_VAX)
if (CERNLIB_UNIX OR CERNLIB_VAX)
  list(APPEND mathlib_gen_f_FSRC  iilz.F)
endif()
#ORIGINAL #if defined(CERNLIB_SGI)
if (CERNLIB_SGI)
endif()
  list(TRANSFORM mathlib_gen_f_FSRC PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/../mathlib/gen/f/")
 
#The original Imake file below included files:pilot.h
#Those were NOT processed.
#mathlib/gen/g/Imakefile
 
#include "pilot.h"
  set(mathlib_gen_g_FSRC chisin.F coedin.F coedis.F denlan.F diflan.F dinvav.F dislan.F disvav.F g116f1.F gamdis.F gausin64.F prob.F probkl.F ranlan.F studin.F studis.F tkolmo.F vavden.F vavfcn.F vavran.F
 vavset.F vavzro.F vviden.F vviset.F xm1lan.F xm2lan.F)
#ORIGINAL #if defined(CERNLIB_DOUBLE)
if (CERNLIB_DOUBLE)
  list(APPEND mathlib_gen_g_FSRC  gausin.F)
else()
  list(APPEND mathlib_gen_g_FSRC  gausin128.F)
endif()
#ORIGINAL #if defined(CERNLIB_SGI)
if (CERNLIB_SGI)
endif()
  list(TRANSFORM mathlib_gen_g_FSRC PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/../mathlib/gen/g/")
 
#mathlib/gen/h/Imakefile
 
  set(mathlib_gen_h_FSRC assndx.F dsmplx.F h101s1.F h101s2.F rsmplx.F)
#ORIGINAL #if defined(CERNLIB_SGI)
if (CERNLIB_SGI)
endif()
  list(TRANSFORM mathlib_gen_h_FSRC PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/../mathlib/gen/h/")
 
#mathlib/gen/j/Imakefile
 
  set(mathlib_gen_j_FSRC conprt.F cont.F finarg.F findem.F frame.F frearg.F j509bd.F map.F names.F namezb.F ordre2.F paper.F setup.F)
#ORIGINAL #if defined(CERNLIB_SGI)
if (CERNLIB_SGI)
endif()
  list(TRANSFORM mathlib_gen_j_FSRC PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/../mathlib/gen/j/")
 
#mathlib/gen/m/Imakefile
 
  set(mathlib_gen_m_FSRC sortdq.F sortiq.F sortrq.F)
#ORIGINAL #if defined(CERNLIB_SGI)
if (CERNLIB_SGI)
endif()
  list(TRANSFORM mathlib_gen_m_FSRC PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/../mathlib/gen/m/")
 
#mathlib/gen/s/Imakefile
 
  set(mathlib_gen_s_FSRC gent.F)
#ORIGINAL #if defined(CERNLIB_SGI)
if (CERNLIB_SGI)
endif()
  list(TRANSFORM mathlib_gen_s_FSRC PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/../mathlib/gen/s/")
 
#The original Imake file below included files:pilot.h
#Those were NOT processed.
#mathlib/gen/u/Imakefile
 
#include "pilot.h"
  set(mathlib_gen_u_FSRC clebs.F clebsg.F djmnb64.F dtclgn64.F dwig3j64.F u100dv.F u100fc.F u100pm.F u100pr.F)
#ORIGINAL #if defined(CERNLIB_DOUBLE)
if (CERNLIB_DOUBLE)
  list(APPEND mathlib_gen_u_FSRC  dwig9j64.F rdjmnb.F rtclgn.F rwig3j.F)
else()
  list(APPEND mathlib_gen_u_FSRC  djmnb.F djmnb128.F dtclgn.F dwig3j.F)
endif()
#ORIGINAL #if defined(CERNLIB_SGI)
if (CERNLIB_SGI)
endif()
  list(TRANSFORM mathlib_gen_u_FSRC PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/../mathlib/gen/u/")
 
#The original Imake file below included files:pilot.h
#Those were NOT processed.
#mathlib/gen/v/Imakefile
 
#include "pilot.h"
  set(mathlib_gen_v_FSRC binomi.F corgen.F corset.F funlux.F funpre.F funran.F funzer.F graph.F hisran.F iget.F munomi.F normco.F permu.F pgraph.F poissn.F ran3d.F rangam.F ranlux.F rg32.F rm48.F rnbnml.F
 rndm2.F rngama.F rnhran.F rnmnml.F rnorml.F rnormx.F rnpssn.F rn3dim.F rvnspc64.F trevni.F tup.F)
#ORIGINAL #if defined(CERNLIB_DOUBLE)
if (CERNLIB_DOUBLE)
  list(APPEND mathlib_gen_v_FSRC  rvnspc.F)
else()
  list(APPEND mathlib_gen_v_FSRC  dvnspc128.F)
endif()
#ORIGINAL #if defined(CERNLIB_VAX)
if (CERNLIB_VAX)
  list(APPEND mathlib_gen_v_FSRC  rnorthvx.F)
endif()
#ORIGINAL #if defined(CERNLIB_UNIX)
if (CERNLIB_UNIX)
  list(APPEND mathlib_gen_v_FSRC  rnorthux.F)
endif()
#ORIGINAL #if defined(CERNLIB_VAXVMS) && !defined(CERNLIB_FORTRAN)
if (CERNLIB_VAXVMS AND NOT CERNLIB_FORTRAN)
  set(mathlib_gen_v_SSRC norranvx.s nranvx.s rn32ml.s)
endif()
#ORIGINAL #if !defined(CERNLIB_VAX) || defined(CERNLIB_FORTRAN)
if (NOT CERNLIB_VAX OR CERNLIB_FORTRAN)
  list(APPEND mathlib_gen_v_FSRC  nranf.F rn32.F)
endif()
#ORIGINAL #if defined(CERNLIB_UNIX) || defined(CERNLIB_QMALPH)
if (CERNLIB_UNIX OR CERNLIB_QMALPH)
  list(APPEND mathlib_gen_v_FSRC  norranux.F)
endif()
#ORIGINAL #if defined(CERNLIB_SGI)
if (CERNLIB_SGI)
endif()
#ORIGINAL #if defined(CERNLIB_SUN) || defined(CERNLIB_QMVAOS)
if (CERNLIB_SUN OR CERNLIB_QMVAOS)
endif()
#ORIGINAL #if defined(CERNLIB_QMALPH)
if (CERNLIB_QMALPH)
endif()
  list(TRANSFORM mathlib_gen_v_FSRC PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/../mathlib/gen/v/")
  list(TRANSFORM mathlib_gen_v_SSRC PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/../mathlib/gen/v/")
 
#mathlib/gen/x/Imakefile
 
  set(mathlib_gen_x_FSRC erprip.F pripar.F strag.F vomas.F)
#ORIGINAL #if defined(CERNLIB_SGI)
if (CERNLIB_SGI)
endif()
  list(TRANSFORM mathlib_gen_x_FSRC PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/../mathlib/gen/x/")
 
#mathlib/hbook/hdiff/Imakefile
 
  set(mathlib_hbook_hdiff_FSRC hbnsum.F hdbaop.F hdbcop.F hdbini.F hdbprf.F hdbsop.F hdiff.F hdiffb.F hgcont.F hlbino.F hlnfct.F)
  list(TRANSFORM mathlib_hbook_hdiff_FSRC PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/../mathlib/hbook/hdiff/")
 
set(mathlib_esources )
if (CERNLIB_BUILD_STATIC)
add_library(mathlib_static STATIC ${mathlib_esources}
                             ${mathlib_bvsl_bvslftn_FSRC} 
                             ${mathlib_gen_a_FSRC} 
                             ${mathlib_gen_b_FSRC} 
                             ${mathlib_gen_c_FSRC} 
                             ${mathlib_gen_d_FSRC} 
                             ${mathlib_gen_d_CSRC} 
                             ${mathlib_gen_divon_FSRC} 
                             ${mathlib_gen_e_FSRC} 
                             ${mathlib_gen_f_FSRC} 
                             ${mathlib_gen_g_FSRC} 
                             ${mathlib_gen_h_FSRC} 
                             ${mathlib_gen_j_FSRC} 
                             ${mathlib_gen_m_FSRC} 
                             ${mathlib_gen_s_FSRC} 
                             ${mathlib_gen_u_FSRC} 
                             ${mathlib_gen_v_FSRC} 
                             ${mathlib_gen_v_SSRC} 
                             ${mathlib_gen_x_FSRC} 
                             ${mathlib_hbook_hdiff_FSRC} 
)
target_include_directories(mathlib_static PRIVATE ${PROJECT_SOURCE_DIR}/include)
target_include_directories(mathlib_static PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
target_include_directories(mathlib_static PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/gen/")
target_include_directories(mathlib_static PRIVATE ${FREETYPE_INCLUDE_DIRS})
install(TARGETS mathlib_static DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT libs)
set_target_properties(mathlib_static PROPERTIES POSITION_INDEPENDENT_CODE ${CERNLIB_POSITION_INDEPENDENT_CODE} OUTPUT_NAME mathlib)
endif()
if (CERNLIB_BUILD_SHARED)
add_library(mathlib SHARED ${mathlib_esources}
                             ${mathlib_bvsl_bvslftn_FSRC} 
                             ${mathlib_gen_a_FSRC} 
                             ${mathlib_gen_b_FSRC} 
                             ${mathlib_gen_c_FSRC} 
                             ${mathlib_gen_d_FSRC} 
                             ${mathlib_gen_d_CSRC} 
                             ${mathlib_gen_divon_FSRC} 
                             ${mathlib_gen_e_FSRC} 
                             ${mathlib_gen_f_FSRC} 
                             ${mathlib_gen_g_FSRC} 
                             ${mathlib_gen_h_FSRC} 
                             ${mathlib_gen_j_FSRC} 
                             ${mathlib_gen_m_FSRC} 
                             ${mathlib_gen_s_FSRC} 
                             ${mathlib_gen_u_FSRC} 
                             ${mathlib_gen_v_FSRC} 
                             ${mathlib_gen_v_SSRC} 
                             ${mathlib_gen_x_FSRC} 
                             ${mathlib_hbook_hdiff_FSRC} 
)
target_include_directories(mathlib PRIVATE ${PROJECT_SOURCE_DIR}/include)
target_include_directories(mathlib PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
target_include_directories(mathlib PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/gen/")
target_include_directories(mathlib PRIVATE ${FREETYPE_INCLUDE_DIRS})
install(TARGETS mathlib DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT libs)
set_target_properties(mathlib        PROPERTIES POSITION_INDEPENDENT_CODE ON OUTPUT_NAME mathlib SOVERSION 2_${COMPSUFFIX}.${CERNLIB_VERSION_MAJOR})
install_symlink(libmathlib.so.2_${COMPSUFFIX}.${CERNLIB_VERSION_MAJOR} ${CMAKE_INSTALL_LIBDIR}/libmathlib.so.2_${COMPSUFFIX})
endif()
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/gen/gen DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}  COMPONENT devel  PATTERN "*makefile*" EXCLUDE  PATTERN "*\.c" EXCLUDE )
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/hbook/hbook DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}  COMPONENT devel  PATTERN "*makefile*" EXCLUDE  PATTERN "*\.c" EXCLUDE )

if(CERNLIB_ENABLE_TEST)
  ENABLE_TESTING()
  add_subdirectory(bvsl/test)
  add_subdirectory(gen/tests)
endif()
