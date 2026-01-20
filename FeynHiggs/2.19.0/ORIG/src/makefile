default: frontend mma$(ML)

all: frontend mma1


lib: libFH.a CFeynHiggs.h CSLHA.h fcc

frontend: lib FeynHiggs$(EXE) table$(EXE)

mma0:

mma1: lib MFeynHiggs$(EXE)


MAIN = SetFlags.o RetrieveFlags.o \
  SetSMPara.o RetrieveSMPara.o GetSMPara.o \
  SetPara.o RetrievePara.o RetrieveOSPara.o \
	GetPara.o GetTLPara.o GetQuarkMasses.o \
  SetFV.o RetrieveFV.o GetFV.o SetSLHA.o \
  Para.o Sfermions.o dZHfin.o TBconv.o TBrepara.o SetDebug.o \
  SetExt.o GetSelf.o GetSelfgl.o HiggsCorr.o Uncertainties.o \
  Couplings.o SelectUZ.o SelectIpol.o \
  Constraints.o EWPO.o Flavour.o \
  HiggsProd.o CalcSE.o CalcSEgl.o AddSelf.o AddExt.o Error.o \
  DRbartoOS.o DRbartoMDRbar.o DeltaMT.o AlphaS.o MDcorr.o Deltab2L.o \
  Output.o OutputSLHA.o
#MAIN += gghNLO.o
#MAIN += DRbar2OS_t.o

DR = Deltar.o DrMSSM1LChaNeu.o DrhoMSSM1LSf.o DrhoSUSY2LGlu.o

REC = RecordIndex.o ClearRecord.o \
  ReadRecord.o NextToken.o SLHARecord.o \
  LoopRecord.o SetRecord.o RetrieveRecord.o \
  LoadTable.o TableRecord.o

LT = cache.o ini.o spence.o eta.o A0.o \
  Bfpv.o B0.o B1.o B00.o B11.o DB0.o DB1.o DB00.o DB11.o \
  C0.o Cget.o CDz.o \
  A0del.o B0del.o myG.o TF.o T134.o T134p.o Tred.o Ored.o
#LT += B001.o B111.o DB001.o D0.o Dget.o

B0.o B1.o B00.o B11.o DB0.o DB1.o DB00.o DB11.o B001.o B111.o DB001.o: Bcoeff.F

T134.o TF.o: T134.h

UTIL = Gauss.o ODESolve.o EigenSf.o SfEigensystem.o \
  HEigensystem.o SEigensystem.o TakagiFactor.o SVD.o \
  LU.o FindMinimum.o Interpolate.o extself.o \
  ODESolveRK.o ODESolveStiff.o ODESolveODEPACK.o \
  odepack.o odepack_sub1.o odepack_sub2.o

ODESolveODEPACK.o: odepack.o odepack_sub1.o odepack_sub2.o
ODESolve.o: ODESolveRK.o ODESolveStiff.o ODESolveODEPACK.o

SLHA = SLHAClear.o SLHARead.o SLHAWrite.o \
  SLHAGetInfo.o SLHAPutInfo.o SLHACopyString.o \
  SLHANewDecay.o SLHAFindDecay.o SLHAAddDecay.o SLHAGetDecay.o \
  SLHADecayTable.o SLHAValid.o SLHAExist.o SLHAPDGName.o

ONELOOP = \
  se_h0h0_mfv.o se_HHHH_mfv.o se_A0A0_mfv.o se_HmHp_mfv.o \
  se_h0HH_mfv.o se_h0A0_mfv.o se_HHA0_mfv.o \
  se_G0G0_mfv.o se_h0G0_mfv.o se_HHG0_mfv.o se_A0G0_mfv.o \
  se_GmGp_mfv.o se_HmGp_mfv.o se_ZZ_mfv.o se_WW_mfv.o \
  td_h0_mfv.o td_HH_mfv.o td_A0_mfv.o \
  dse_h0h0_mfv.o dse_HHHH_mfv.o dse_A0A0_mfv.o dse_HmHp_mfv.o \
  dse_h0HH_mfv.o dse_h0A0_mfv.o dse_HHA0_mfv.o \
  dse_G0G0_mfv.o dse_h0G0_mfv.o dse_HHG0_mfv.o dse_A0G0_mfv.o \
  dse_GmGp_mfv.o dse_HmGp_mfv.o \
  dse_ZZ_mfv.o dse_WW_mfv.o \
  dZ_h0h0_mfv.o dZ_HHHH_mfv.o \
  se_h0h0_nmfv.o se_HHHH_nmfv.o se_A0A0_nmfv.o se_HmHp_nmfv.o \
  se_h0HH_nmfv.o se_h0A0_nmfv.o se_HHA0_nmfv.o \
  se_G0G0_nmfv.o se_h0G0_nmfv.o se_HHG0_nmfv.o se_A0G0_nmfv.o \
  se_GmGp_nmfv.o se_HmGp_nmfv.o se_ZZ_nmfv.o se_WW_nmfv.o \
  td_h0_nmfv.o td_HH_nmfv.o td_A0_nmfv.o \
  dse_h0h0_nmfv.o dse_HHHH_nmfv.o dse_A0A0_nmfv.o dse_HmHp_nmfv.o \
  dse_h0HH_nmfv.o dse_h0A0_nmfv.o dse_HHA0_nmfv.o \
  dse_G0G0_nmfv.o dse_h0G0_nmfv.o dse_HHG0_nmfv.o dse_A0G0_nmfv.o \
  dse_GmGp_nmfv.o dse_HmGp_nmfv.o \
  dse_ZZ_nmfv.o dse_WW_nmfv.o \
  dZ_h0h0_nmfv.o dZ_HHHH_nmfv.o \
  se_h0h0_gl.o se_HHHH_gl.o se_A0A0_gl.o se_HmHp_gl.o \
  se_h0HH_gl.o se_h0A0_gl.o se_HHA0_gl.o \
  se_G0G0_gl.o se_h0G0_gl.o se_HHG0_gl.o se_A0G0_gl.o \
  se_GmGp_gl.o se_HmGp_gl.o \
  td_h0_gl.o td_HH_gl.o td_A0_gl.o \
  dse_h0h0_gl.o dse_HHHH_gl.o dse_A0A0_gl.o dse_HmHp_gl.o \
  dse_h0HH_gl.o dse_h0A0_gl.o dse_HHA0_gl.o \
  dse_G0G0_gl.o dse_h0G0_gl.o dse_HHG0_gl.o dse_A0G0_gl.o \
  dse_GmGp_gl.o dse_HmGp_gl.o \
  dZ_h0h0_gl.o dZ_HHHH_gl.o dZ_h0HH_gl.o \
  se_h0h0_exp.o se_HHHH_exp.o se_A0A0_exp.o se_HmHp_exp.o \
  se_h0HH_exp.o se_h0A0_exp.o se_HHA0_exp.o \
  se_G0G0_exp.o se_h0G0_exp.o se_HHG0_exp.o se_A0G0_exp.o \
  se_GmGp_exp.o se_HmGp_exp.o se_ZZ_exp.o se_WW_exp.o \
  td_h0_exp.o td_HH_exp.o td_A0_exp.o \
  dse_h0h0_exp.o dse_HHHH_exp.o dse_A0A0_exp.o dse_HmHp_exp.o \
  dse_h0HH_exp.o dse_h0A0_exp.o dse_HHA0_exp.o \
  dse_G0G0_exp.o dse_h0G0_exp.o dse_HHG0_exp.o dse_A0G0_exp.o \
  dse_GmGp_exp.o dse_HmGp_exp.o \
  dse_ZZ_exp.o dse_WW_exp.o \
  dZ_h0h0_exp.o dZ_HHHH_exp.o

CalcSE.o: CalcSE_mfv.h CalcSE_nmfv.h CalcSE_exp.h

CalcSEgl.o: CalcSE_gl.h

TLPS = TLps.o TLps_asat.o TLps_asab.o TLps_atab.o TLps_atat.o \
  Pi11t.o Pi12t.o Pi22t.o Pi11b.o Pi12b.o Pi22b.o Pitt.o \

TLHR = TLhr.o TLhr_abbr_r.o TLhr_rcs.o \
  TLhr_abbr_c1.o TLhr_abbr_c2.o TLhr_self_c.o \
  TLhr_abbr_d1.o TLhr_abbr_d2.o TLhr_abbr_d3.o TLhr_abbr_d4.o \
  TLhr_abbr_d5.o TLhr_abbr_d6.o TLhr_abbr_d7.o TLhr_abbr_d8.o \
  TLhr_abbr_d9.o TLhr_abbr_d10.o TLhr_abbr_d11.o TLhr_abbr_d12.o \
  TLhr_abbr_d13.o TLhr_self_d.o \
  TLhr_abbr_e1.o TLhr_abbr_e2.o TLhr_abbr_e3.o TLhr_self_e.o\
  TLhr_abbr_o1.o TLhr_abbr_o2.o TLhr_abbr_o3.o TLhr_self_o.o

TLSP = TLsp.o TLsp_atat_c1.o TLsp_atat_c2.o \
  TLsp_atat_e1.o TLsp_atat_e2.o TLsp_atat_e3.o \
  TLsp_atat_o1.o TLsp_atat_o2.o

TLAQAS = TLaqas.o TLaqas_coupl.o TLaqas_renconst_1L.o \
  TLaqas_renconst_1L_div.o TLaqas_renconst_1L_Q.o \
  TLaqas_renconst_1L_DR.o TLaqas_renconst_1L_MDR.o \
  TLaqas_renconst_1L_OS.o TLaqas_renconst_1L_MS.o \
  TLaqas_renconst_1L_scheme.o TLaqas_renconst_1L_dep.o \
  TLaqas_renconst_2L.o TLaqas_se_abas_hjhk_susy.o \
  TLaqas_se_abas_hjhk_thdm.o TLaqas_se_atas_hjhk_susy.o \
  TLaqas_se_atas_hjhk_thdm.o TLaqas_sect_abas_hjhk_susy.o \
  TLaqas_sect_abas_hjhk_thdm.o TLaqas_sect_atas_hjhk_susy.o \
  TLaqas_sect_atas_hjhk_thdm.o TLaqas_sect_HmHp_susy.o \
  TLaqas_sect_HmHp_thdm.o TLaqas_se_HmHp_susy.o \
  TLaqas_se_HmHp_thdm.o TLaqas_tad_abas_hj_susy.o \
  TLaqas_tad_abas_hj_thdm.o TLaqas_tad_atas_hj_susy.o \
  TLaqas_tad_atas_hj_thdm.o TLaqas_tadct_abas_hj_susy.o \
  TLaqas_tadct_abas_hj_thdm.o TLaqas_tadct_atas_hj_susy.o \
  TLaqas_tadct_atas_hj_thdm.o

TLAQAQ = TLaqaq.o TLaqaq_coupl.o TLaqaq_renconst_1L.o \
  TLaqaq_renconst_1L_div.o TLaqaq_renconst_1L_Q.o \
  TLaqaq_renconst_1L_DR.o TLaqaq_renconst_1L_OS.o \
  TLaqaq_renconst_1L_MS.o TLaqaq_renconst_1L_scheme.o \
  TLaqaq_renconst_1L_dep.o TLaqaq_renconst_2L.o \
  TLaqaq_se_hjhk_susy.o TLaqaq_se_hjhk_thdm.o \
  TLaqaq_sect_hjhk_susy.o TLaqaq_sect_hjhk_thdm.o \
  TLaqaq_se_HmHp_susy.o TLaqaq_se_HmHp_thdm.o \
  TLaqaq_sect_HmHp_susy.o TLaqaq_sect_HmHp_thdm.o \
  TLaqaq_tad_hj_susy.o TLaqaq_tad_hj_thdm.o \
  TLaqaq_tadct_hj_susy.o TLaqaq_tadct_hj_thdm.o

TWOLOOP = $(TLPS) $(TLHR) $(TLSP) $(TLAQAS) $(TLAQAQ) \
  CalcRenSETL.o InvTri.o Phi.o \
  DRbar2LShifts.o dZHfin2LShifts.o MTrepara.o

EFT = Resum4H.o Resum4H1.o \
  Resum4H23.o Resum4H4r.o Resum4H4c.o \
  beta4H.o beta4H4r.o beta4H4c.o \
  eftloopfun.o runStopPara.o \
  TLthresholdasat.o TLthresholdatat.o \
  TLthresholds4asat.o TLthresholds4atat.o \
  PhiQuad.o spenceQuad.o

PhiQuad.o: Phi.F

spenceQuad.o: spence.F

$(EFT): Resum4H.h

TLthresholdasat.o: Resum4H.h
TLthresholdatat.o: TLthresholdatat.h Resum4H.h

Resum4H.o: Resum4H.h trigfun.h
Resum4H1.o: Resum4H.h
Resum4H23.o: FOSubtract.h  para23.h Resum4H.h \
  TLthresholdasat.o TLthresholdatat.o dmz2.h trigfun.h
Resum4H4r.o: FOSubtract4r.h  para4r.h Resum4H.h \
  TLthresholds4asat.o TLthresholds4atat.o dmw2.h dmz2.h
Resum4H4c.o: FOSubtract4c.h  para4c.h Resum4H.h \
  TLthresholds4asat.o TLthresholds4atat.o dmw2.h dmz2.h trigfun.h

GM2_2L_0 = gm2_2L_1.o gm2_2L_2.o

GM2_2L_FULL = gm2_2L_full1.o gm2_2L_full2.o gm2_2L_full3.o \
  gm2_2L_full4.o gm2_2L_full5.o gm2_2L_full6.o gm2_2L_full7.o \
  gm2_2L_full8.o gm2_2L_full9.o gm2_2L_full10.o gm2_2L_full11.o

GM2_2L_1 = $(GM2_2L_0) $(GM2_2L_FULL)

GM2 = gm2_1L_1.o $(GM2_2L_$(GM2FULL))

CONSTRAINTS = $(GM2) EDM.o Bsgamma.o DeltaMs.o Bsll.o

Bsgamma.o DeltaMs.o Bsll.o: debug.h

HGAGA = hgaga-SquaredME.o \
  hgaga-abbr1s.o hgaga-abbr1h.o hgaga-v1.o \
  hgaga-v1_Cha4.o hgaga-v1_Gen4.o \
  hgaga-v1_All4_Ind1_Ind2.o

HGAGASM = hgagaSM-SquaredME.o \
  hgagaSM-abbr1s.o hgagaSM-abbr1h.o \
  hgagaSM-v1.o hgagaSM-v1_Gen4.o

HGAZ = hgaZ-SquaredME.o hgaZ-RenConst.o \
  hgaZ-abbr1s.o hgaZ-abbr1h.o \
  hgaZ-v1.o hgaZ-v1_Gen4.o \
  hgaZ-v1_All4_All5_Ind1_Ind2_Ind3.o \
  hgaZ-v1_Cha4_Cha5.o

HGAZSM = hgaZSM-SquaredME.o hgaZSM-RenConst.o \
  hgaZSM-abbr1s.o hgaZSM-abbr1h.o \
  hgaZSM-v1.o hgaZSM-v1_Gen4.o

HGG = hgg-SquaredME.o \
  hgg-abbr1s.o hgg-abbr1h.o \
  hgg-v1_Gen4.o hgg-v1_All4_Ind1_Ind2.o

HGGSM = hggSM-SquaredME.o \
  hggSM-abbr1s.o hggSM-abbr1h.o \
  hggSM-v1_Gen4.o

HEE = hee-SquaredME.o hee-RenConst.o \
  hee-abbr0s.o hee-abbr0h.o \
  hee-abbr1s.o hee-abbr1h.o \
  hee-v0.o hee-v1.o \
  hee-v1_All4_All5_Ind1_Ind2_Ind3.o \
  hee-v1_All4_All5_Ind1_Ind2_Ind3_Ind4.o \
  hee-v1_All4_All5_Ind1_Ind2_Neu4.o \
  hee-v1_All4_Ind1.o \
  hee-v1_All4_Neu4_Neu5.o \
  hee-v1_Cha4.o \
  hee-v1_Cha4_Cha5.o \
  hee-v1_Gen4.o \
  hee-v1_Neu4_Neu5.o

HEESM = heeSM-SquaredME.o heeSM-RenConst.o \
  heeSM-abbr0h.o \
  heeSM-abbr1s.o heeSM-abbr1h.o \
  heeSM-v0.o heeSM-v1.o \

HUU = huu-SquaredME.o huu-RenConst.o \
  huu-abbr0s.o huu-abbr0h.o \
  huu-abbr1s.o huu-abbr1h.o \
  huu-v0.o huu-v1.o \
  huu-v1_All4_All5_Cha4_Ind1_Ind2_Ind3_Ind4.o \
  huu-v1_All4_All5_Ind1_Ind2.o \
  huu-v1_All4_All5_Ind1_Ind2_Ind3.o \
  huu-v1_All4_All5_Ind1_Ind2_Ind3_Ind4.o \
  huu-v1_All4_All5_Ind1_Ind2_Neu4.o \
  huu-v1_All4_Cha4_Cha5_Ind1_Ind2.o \
  huu-v1_All4_Ind1.o \
  huu-v1_All4_Neu4_Neu5.o \
  huu-v1_Cha4_Cha5.o \
  huu-v1_Gen4.o \
  huu-v1_Neu4_Neu5.o

HUUSM = huuSM-SquaredME.o huuSM-RenConst.o \
  huuSM-abbr0s.o huuSM-abbr0h.o \
  huuSM-abbr1s.o huuSM-abbr1h.o \
  huuSM-v0.o huuSM-v1.o huuSM-v1_Gen4.o

HDD = hdd-SquaredME.o hdd-RenConst.o \
  hdd-abbr0s.o hdd-abbr0h.o \
  hdd-abbr1s.o hdd-abbr1h.o \
  hdd-v0.o hdd-v1.o \
  hdd-v1_All4_All5_Cha4_Ind1_Ind2_Ind3_Ind4.o \
  hdd-v1_All4_All5_Ind1_Ind2.o \
  hdd-v1_All4_All5_Ind1_Ind2_Ind3.o \
  hdd-v1_All4_All5_Ind1_Ind2_Ind3_Ind4.o \
  hdd-v1_All4_All5_Ind1_Ind2_Neu4.o \
  hdd-v1_All4_Cha4_Cha5_Ind1_Ind2.o \
  hdd-v1_All4_Ind1.o \
  hdd-v1_All4_Neu4_Neu5.o \
  hdd-v1_Cha4_Cha5.o \
  hdd-v1_Gen4.o \
  hdd-v1_Neu4_Neu5.o

HDDSM = hddSM-SquaredME.o hddSM-RenConst.o \
  hddSM-abbr0s.o hddSM-abbr0h.o \
  hddSM-abbr1s.o hddSM-abbr1h.o \
  hddSM-v0.o hddSM-v1.o hddSM-v1_Gen4.o

fvHHH = hhh-RenConst.o hhh-SquaredME.o \
  hhh-abbr0s.o hhh-abbr1s1.o hhh-abbr1s2.o \
  hhh-v0.o \
  hhh-v1_All4_All5_All6_Ind1_Ind2_Ind3_Ind4_Ind5_Ind6.o \
  hhh-v1_All4_All5_Ind1_Ind2_Ind3.o hhh-v1_All4_All5_Ind1_Ind2_Ind3_Ind4.o \
  hhh-v1_All4_Ind1.o \
  hhh-v1_Cha4_Cha5_Cha6.o hhh-v1_Cha4_Cha5.o hhh-v1.o hhh-v1_Gen4.o \
  hhh-v1_Gen4_Sfe4.o \
  hhh-v1_Gen4_Sfe4_Sfe5.o \
  hhh-v1_Gen4_Sfe4_Sfe5_Sfe6.o \
  hhh-v1_Neu4_Neu5.o \
  hhh-v1_Neu4_Neu5_Neu6.o

HHH = hhh-RenConst.o hhh-SquaredME.o \
  hhh-abbr0s.o hhh-abbr1s1.o hhh-abbr1s2.o \
  hhh-v0.o hhh-v1.o hhh-v1_Cha4_Cha5.o \
  hhh-v1_Cha4_Cha5_Cha6.o hhh-v1_Gen4.o \
  hhh-v1_Gen4_Sfe4.o hhh-v1_Gen4_Sfe4_Sfe5.o \
  hhh-v1_Gen4_Sfe4_Sfe5_Sfe6.o hhh-v1_Neu4_Neu5.o \
  hhh-v1_Neu4_Neu5_Neu6.o

DECAYS = VecSet.o \
  $(HGAGA) $(HGAGASM) \
  $(HGAZ) $(HGAZSM) \
  $(HGG) $(HGGSM) \
  $(HEE) $(HEESM) \
  $(HUU) $(HUUSM) \
  $(HDD) $(HDDSM) \
  $(HHH)

MOST = $(MAIN) $(ONELOOP) $(TWOLOOP) $(LT) $(REC) $(UTIL) \
  $(EFT) $(DECAYS) $(CONSTRAINTS) $(DR)

ALL = $(MOST) $(SLHA)


libFH.a: $(ALL)
	ar cru libFH.a $?
	-ranlib libFH.a


$(ALL): externals.h types.h

$(MOST) EigenSf.o LoopRecord.o ReadRecord.o TableRecord.o: FH.h const.h

$(REC) SetSMPara.o Para.o SetSLHA.o SetNMFV.o: FHRecord.h

EigenSf.o ReadRecord.o LoopRecord.o SetRecord.o LoadTable.o TableRecord.o: debug.h

LoopRecord.o SetRecord.o: Defaults.h

SetSLHA.o: SLHA.h SLHADefs.h

$(SLHA): SLHADefs.h

SLHARead.o SLHAWrite.o SLHAPDGName.o: PDG.h

SLHARead.o: SLHAReadBlocks.h

SLHAWrite.o: SLHAWriteBlocks.h

SLHARead.o SLHAWrite.o SLHANewDecay.o SLHAFindDecay.o \
  SLHAAddDecay.o SLHAGetDecay.o SLHADecayTable.o: SLHAIntern.h

SLHARecord.o OutputSLHA.o: DeltaAlfaTop.h

SetFlags.o OutputSLHA.o: version.h

SetFlags.o RetrieveFlags.o SetSMPara.o RetrieveSMPara.o \
  RetrievePara.o SetSLHA.o SetCKM.o RetrieveCKM.o SetNMFV.o \
  RetrieveNMFV.o Para.o GetPara.o GetSelf.o CalcSE.o CalcSEgl.o \
  HiggsCorr.o Uncertainties.o Couplings.o SelectUZ.o \
  HiggsProd.o Flavour.o EWPO.o Constraints.o \
  DRbartoOS.o Error.o: debug.h

Couplings.o HiggsProd.o: FHCouplings.h

Couplings.o: NHiggsDecayFitsLO.f NHiggsDecayFitsNLO.f

HiggsProd.o: gghcTev.f gghkTev.f NHiggsProdFits-Tev.f NHiggsProdFits-LHC.f \
  gghcLHC.f gghkLHC.f NHiggsProdFits-LHC7.f NHiggsProdFits-LHC8.f \
  NHiggsProdFits-LHC14.f CHiggsProdFits-LHC8.f CHiggsProdFits-LHC13.f \
  CHiggsProdFits-LHC14.f

Para.o DRbartoOS.o: looptools.h ltcache.h

$(LT): lt.h

Cget.o Dget.o: ltcache.h

HEigensystem.o SEigensystem.o TakagiFactor.o SVD.o Gauss.o LU.o: Util.h

$(ONELOOP): looptools.h debug.h

$(TLHR) MTrepara.o: TLhr.h TLhrvars.h TL.h FH.h looptools.h debug.h

$(TLPS) CalcRenSETL.o: TLps.h TLpsvars.h TL.h FH.h looptools.h debug.h

$(TLSP): TLsp.h TLspvars.h TL.h FH.h looptools.h debug.h

$(TLAQAS): TLaqas.h TLaqasvars.h TL.h FH.h looptools.h debug.h

$(TLAQAQ): TLaqaq.h TLaqaqvars.h TL.h FH.h looptools.h debug.h

HiggsCorr.o AddSelf.o CalcRenSETL.o TLsp.o: HiggsRot.h

MTrepara.o: MTrepara.h

dZHfin2LShifts.o: looptools.h ltcache.h

Constraints.o $(GM2): gm2.h

$(GM2_2L_0): gm2_2L.h

$(GM2_2L_FULL): gm2_2L_full.h

DeltaRho_1L.o $(GM2) $(DR): looptools.h ltcache.h

Bsgamma.o: bsgamma.h bsglu.h looptools.h ltcache.h

DeltaMs.o: deltaMs.h looptools.h ltcache.h

Bsll.o: bsll.h looptools.h ltcache.h

Deltar.o: Deltar.h DrSM1L.h DrMSSM1LVH.h DrMSSM1LSf.h

DrMSSM1LChaNeu.o: Deltar.h DrMSSM1LChaNeu.h

DrhoSUSY2LGlu.o: Deltar.h DrhoSUSY2LdMSb12.h DrhoSUSY2LGluino.h

$(HGAGA): hgaga-vars.h

$(HGAGASM): hgagaSM-vars.h

$(HGAZ): hgaZ-vars.h hgaZ-RenConst.h.F

$(HGAZSM): hgaZSM-vars.h hgaZSM-RenConst.h.F

$(HGG): hgg-vars.h

$(HGGSM): hggSM-vars.h

$(HEE): hee-vars.h hee-RenConst.h.F

$(HEESM): heeSM-vars.h heeSM-RenConst.h.F

$(HUU): huu-vars.h huu-RenConst.h.F

$(HUUSM): huuSM-vars.h huuSM-RenConst.h.F

$(HDD): hdd-vars.h hdd-RenConst.h.F

$(HDDSM): hddSM-vars.h hddSM-RenConst.h.F

$(HHH): hhh-vars.h hhh-RenConst.h.F

$(DECAYS) Couplings.o: Decay.h inline.h looptools.h ltcache.h debug.h

VecSet.o: Decay.h


FHRecord.h: FHRecord.h.in RecordIndices.h
	cat RecordIndices.h FHRecord.h.in > FHRecord.h

CFeynHiggs.h: CFeynHiggs.h.in RecordIndices.h ftypes.h
	sed "s:NOUNDERSCORE:$(NOUNDERSCORE):" ftypes.h RecordIndices.h CFeynHiggs.h.in > CFeynHiggs.h

CSLHA.h: CSLHA.h.in ftypes.h
	sed "s:NOUNDERSCORE:$(NOUNDERSCORE):" ftypes.h CSLHA.h.in > CSLHA.h


FE = FeynHiggs.o pager.o

FeynHiggs$(EXE): $(FE) libFH.a
	$(FC) $(FFLAGS) -o FeynHiggs$(EXE) $(FE) libFH.a -lstdc++

MFeynHiggs$(EXE): MFeynHiggs$(QUADSUFFIX).tm CFeynHiggs.h CSLHA.h fcc fortranflush.o libFH.a
	test -d =. || ln -s . =.
	NM="$(NM)" DLLTOOL="$(DLLTOOL)" \
	CC="./fcc" REALCC="$(CC) $(CFLAGS)" \
	CXX="./f++" REALCXX="$(CXX) $(CXXFLAGS)" \
	PATH="$$PATH:." \
	  $(MCC) -o MFeynHiggs$(EXE) $(MCFLAGS) MFeynHiggs$(QUADSUFFIX).tm \
	    fortranflush.o libFH.a
	rm -f MFeynHiggs$(QUAD).tm.*

MFeynHiggs-quad.tm: MFeynHiggs.tm
	sed '/:Begin:/,/:End:/ s/Real/Real128/g' MFeynHiggs.tm > MFeynHiggs-quad.tm


SetFlags.o FeynHiggs.o: flags.h

FeynHiggs.o: version.h FHRecord.h SLHA.h


fcc: fcc.in makefile
	sed "s|^fldflags=[\"']*\([^\"']*\).*$$|fldflags='$(LDFLAGS) \1'|" fcc.in > fcc
	chmod 755 fcc
	rm -f f++
	ln -s fcc f++

table$(EXE): table.c
	$(CC) $(CFLAGS) -o table$(EXE) table.c
