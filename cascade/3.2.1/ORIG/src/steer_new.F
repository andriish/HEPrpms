      subroutine steer_new

      implicit none
#include "casparas.inc"
#include "casluco.inc"
#include "caspara5.inc"
#include "caspart.inc"
#include "casvalues.inc"
#include "casshwr.inc"
#include "casprkt.inc"
#include "casscal.inc"
#include "casglu.inc"
#include "casmult.inc"
#include "casherup.inc"
#include "caspsipol.inc"
#include "caslhe.inc"
#include "casevent.inc"
#include "casparam.inc"
      Integer Iremn
      common/casflags/Iremn	
      
#include "casMERGING.inc"

      character*512 PDFPATH
      common/CASPDF/PDFPATH
      Integer ICOLORA,IRESPRO,IRPA,IRPB,IRPC,IRPD,IRPE,IRPF,IRPG
      COMMON /CASCOL/ ICOLORA,IRESPRO,IRPA,IRPB,IRPC,IRPD,IRPE,IRPF,IRPG

      Integer IINT,NCB
      Double Precision ACC1,ACC2
      COMMON /INTEGR/ ACC1,ACC2,IINT,NCB
#include "casiglu.inc"
      Integer EVENT_NPRIN
      Common/EVENT_INFO/EVENT_NPRIN

#include "casMLM.inc"
#include "casfNLO.inc"

      Integer NrEvents,Process_Id,Hadronisation,
     & SpaceShower,SpaceShowerOrderAlphas,TimeLikeSplittingInSpaceShower,
     & TimeShower,ScaleTimeShower,
     & PartonEvolution,
     & PartonDensity, PartonDensity2,
     & EnergyShareRemnant,Remnant,
     & lheHasOnShellPartons,lheReweightTMD,lheScale,lheWeightId,lheNBornpart,
     & pythia6, Imerge, MinJetsMerge, MaxJetsMerge, NqmaxMerge,Nevent_print,Uncertainty_TMD,fNLO,
     & UsePrivateSetting
      Double Precision ScaleFactorFinalShower,ScaleFactorMatchingScale
      Double Precision MaxFactor
      character *512 TMDDensityPath,lheInput
      
C
#include "HepMCSteering.inc"
      LOGICAL HepMCOUTPUT
      character *132 HepMCFile  
      integer HepMCFormat   
C
      namelist/CASCADE_input/
     & NrEvents,Process_Id,Hadronisation,
     & SpaceShower,SpaceShowerOrderAlphas,TimeLikeSplittingInSpaceShower,
     & TimeShower,ScaleTimeShower,ScaleFactorFinalShower,
     & PartonEvolution,
     & PartonDensity, PartonDensity2,
     & EnergyShareRemnant,Remnant,
     & lheHasOnShellPartons,lheReweightTMD,lheWeightId,lheScale,lheNBornpart,
     & Nevent_print,
     & ScaleFactorMatchingScale,MaxFactor,
     & pythia6,
     & TMDDensityPath,lheInput,Uncertainty_TMD,fNLO,UsePrivateSetting,
     & HepMCFile,
     & HepMCOUTPUT,
     & HepMCFormat
      
      Double precision rclus, etclus, etaclmax
      Integer iexcl,iMLM, LHE_tmd
      namelist/CASCADE_MLM/rclus, etclus, etaclmax,    
     & Imerge, MinJetsMerge, MaxJetsMerge, iMLM, LHE_tmd,NqmaxMerge
      
      NrEvents=Nevent
      Process_Id  =Ipro 
      Hadronisation=Nfrag
      
      if(IFPS.eq.1.or.IFPS.eq.3) Spaceshower=1
      if(IFPS.eq.2.or.IFPS.eq.3) Timeshower=1
      IFPS = 0
      SpaceShowerOrderAlphas = IordasPS
c scalfa = scale factor for alphas
c scalfaf = scale factor for final state scale
c ifin = defintion of scale for final state shower
      ScaleTimeShower=ifinal
      PartonEvolution=Iccfm
      EnergyShareRemnant=IRspl
      Remnant=IRemn
      PartonDensity = IGLU
      lheHasOnShellPartons=ILHE
      lheReweightTMD=iTMDw
      lheWeightId=lheWid
      lheScale=Iscale
      lheNBornpart = lheNBorn
      ScaleFactorFinalShower=Scalfaf
      TMDDensityPath=PDFPATH
      lheInput=lhefile
      pythia6=p6frag
      ScaleFactorMatchingScale=ScaleFactorMatching
      MaxFactor=Scalfac
      Uncertainty_TMD=TMDuncertainty
      UsePrivateSetting = 0
      fNLO=ifixed_NLO
      TimeLikeSplittingInSpaceShower = ITIMSHR
c      read (5,NML=CASCADE_input,END=141,ERR=42)
      read (5,NML=CASCADE_input,END=141)
      WRITE ( *, CASCADE_input ) 
       hmOUT=HepMCOUTPUT
       hmfilnam=HepMCFile
       hmfilfor=HepMCFormat
      
      Nevent = NrEvents
      EVENT_NPRIN = Nevent_print
      Ipro = Process_Id      
      Nfrag = Hadronisation     
      if(SpaceShower.eq.1) IFPS = 1
      if(TimeShower.eq.1) IFPS = 2
      if(SpaceShower.eq.1.and.TimeShower.eq.1) IFPS = 3
      Ifinal = ScaleTimeShower
      IRspl = EnergyShareRemnant
      IRemn = Remnant
      IGLU = PartonDensity 
      IGLU2 = IGLU
      if(PartonDensity2.ne.0) IGLU2 = PartonDensity2 
      if(UsePrivateSetting.eq.0) then 
c        if(iglu.gt. 102100 .and.Iglu.le.102290 ) IordasPS = 2
        IordasPS = SpaceShowerOrderAlphas
c        write(6,*)  ' before call casTMDlib_param ',iglu
        if(iglu.gt.10000) call cas_getTMDlib_param
        Iccfm = 2
      else
        IordasPS = SpaceShowerOrderAlphas
        Iccfm = PartonEvolution
      endif
      
      ILHE = lheHasOnShellPartons
      iTMDw = lheReweightTMD
      lheWid = lheWeightId
      Iscale = lheScale
      p6frag = pythia6
      
      Scalfaf = ScaleFactorFinalShower
      
      PDFPATH = trim(TMDDensityPath)
      lhefile = lheInput
      lheNBorn = lheNBornpart
      ScaleFactorMatching=ScaleFactorMatchingScale
      if(MaxFactor.gt.0) ScalFac=MaxFactor
      TMDuncertainty=Uncertainty_TMD
      ifixed_NLO=fNLO
      ITIMSHR=TimeLikeSplittingInSpaceShower

c      Collfile = 

141   continue
      
      rewind 5
      

c  inout for MLM merging      
      rclus = RCLUS_MLM
      etclus = ETCLUS_MLM
      etaclmax = ETACLMAX_MLM
      Imerge=IMultiJetMerge
      MinJetsMerge = MinJets
      MaxJetsMerge = MaxJets
c armando
      NqmaxMerge = Nqmax
cend armando
      iMLM = iMLMold
      LHE_tmd = iLHE_TMD
      read (5,NML=CASCADE_MLM,END=142)
      WRITE ( *, CASCADE_MLM ) 
      IMultiJetMerge = Imerge
      MinJets = MinJetsMerge
      MaxJets = MaxJetsMerge
c armando
      Nqmax = NqmaxMerge
cend armando
      RCLUS_MLM = rclus
      ETCLUS_MLM = etclus
      ETACLMAX_MLM = etaclmax
      iMLMold = iMLM
      iLHE_tmd = LHE_TMD
      
 142  continue
      
      rewind (5)
      return
      
c 141  continue
c      close (5)

c 142  continue
c      close (5)
      
      return
      
 42   continue
      print '(''Error reading namelist &CASCADE_input, STOP'')'
      write( *, CASCADE_input ) 
      STOP
      end       
      
