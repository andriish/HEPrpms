      PROGRAM RGMAIN
	Implicit None
#include "rgfull.inc"
#include "rgdisdif.inc"
#include "rgluco.inc"
#include "rgludat1.inc"
#include "rgpara.inc"
#include "rgpypars.inc"
#include "rgdiffr.inc"
#include "rgscq2.inc"
#include "rglq2.inc"
#include "rghsunts.inc"
#include "rghsoptn.inc"
#include "rghscuts.inc"
#include "rghsvglp.inc"
#include "rgeffic.inc"
#include "rgpart.inc"
#include "rgstru.inc"
#include "rgrapgki.inc"

      character*512 hepmcout
      common/RAHEPMCOUT/hepmcout
      integer iev,isub
      double precision xsec,xsece

	Integer Nevent
	Common/steer1/Nevent
	Integer K1,K2,LUX

	real timeleft
      Integer Minuts
      External Minuts
	External pydata
      double precision x1,x2,q2pdfeval,xf1mom,xf2mom,getq2q
      integer pdf1,pdf2

      character *512 hepmcout_env
      Integer I,J,ISEED
      character*255 raseed
      character*255 test

      Integer ICHNN
      COMMON /HSCHNN/ ICHNN

      INTEGER NMXHEP
      PARAMETER (NMXHEP=4000)
      COMMON /HEPEVT/  NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &                 JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),
     &                 VHEP(4,NMXHEP)
      INTEGER          NEVHEP,NHEP,ISTHEP,IDHEP,JMOHEP,JDAHEP
      DOUBLE PRECISION PHEP,VHEP

include "HepMCSteering.inc"
include "RivetInterface.inc"
include "Pythia6ToHepMC3.inc"
include "Pythia6ToHepMC2.inc"
      INTEGER OID3, HM3STATUS, OID2, HM2STATUS, rvtHM, RIVETSTATUS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      
      call dummysub(pydata)
      
C---initialise ARIADNE parameters, now done via block data
C---initialise PYTHIA 6 parameters, via pythia block data 
C     initialize random number generator
c      ISEED = 213123
c      ISEED = Iabs(MINUTS())
c      ISEED = 44788029
      
      CALL GETENV("RASEED", raseed)
c note RASEED can have only 10 digits
c i.e   export RASEED='1234567890
      read(raseed,'(I30)') iseed
      if(RASEED.eq.' ') then
         write(6,*) ' RASEED set = 12345 '
         iseed = 12345
      else
         read(raseed,'(I30)') iseed  
         write(6,*) ' RASEED from environment variable ', iseed
      endif
      WRITE (6,*) ' test raseed:',Iseed
      LUX = 4
      K1=0
      K2=0
            
      CALL RLUXGO(LUX,ISEED,K1,K2)
C---initialise RAPGAP parameters
      CALL GRAINI
C-- read in parameters from file 
      Call Steer
C-- Init HepMC2      
      if (hm2OUT) then      
      OID2=HepMC2_new_writer(0,0,trim(hm2filnam)//char(0))
      endif
C-- Init HepMC3      
      if (hm3OUT) then
      OID3=HepMC3_new_writer(0,hm3filfor,trim(hm3filnam)//char(0))
      HM3STATUS=HepMC3_new_weight(OID3,'Default'//char(0))
      NEVHEP=-123456
      HM3STATUS=HepMC3_set_hepevt_address(NEVHEPL)
      endif
C-- Init Rivet
      if (rvtOUT) then
      rvtHM=rivet_interface_version()
      do i=1,Nrvtana
      RIVETSTATUS=rivet_add_analysis(trim(rvtana(i))//char(0))
      end do
      if (len(rvtrun).GT. 0) then
      RIVETSTATUS=rivet_init(trim(rvtrun)//char(0))
      end if
      endif
C-- change standard parameters of RAPGAP 	
	Call rapcha
C-- change standard parameters of HERACLES	
	Call hercha
C-- change standard parameters of JETSET
	Call pytcha
	IF(IFPS.EQ.10) then
C Initialize ARIADNE
         CALL ARINIT("RAPGAP")
C-- change standard parameters of ARIADNE
	   Call aricha
      endif
C--- CALCULATE X SECTION
      CALL GETENV("HEPMCOUT", hepmcout_env)
      if(hepmcout_env.ne.' ') then
        hepmcout = trim(hepmcout_env)//char(0)
        write(6,*) ' hepmcout from environment variable = ',trim(hepmcout)
        else
        hepmcout=trim(hepmcout)//char(0)
        write(6,*) ' hepmcout from steering file = ',trim(hepmcout)
      endif
      CALL PTIME(' rapgap  ',1,0)
      CALL RAPGAP
C--- print x section
      CALL RAEND(1)
C--- event generation
      DO 10 I=1,Nevent
         CALL PTIME(' event  ',1,0)
c	   call timel(timeleft)
c	   if(timeleft.le.10.) then
c	      write(6,*) ' *** time limit reached. time left = ',timeleft
c            goto 123
c	   endif
         CALL EVENT
C--- user analysis routine
c         CALL ANALYS
C--- user analysis routine (hztool)
cc         CALL hzraana
         iev = I
         isub = ipro
c hepmc xsec in pb        
         xsec = avgi *1000.
         xsece = sd * 1000.          

         x1=XEL
         x2=XPR
         q2pdfeval=getq2q();
         xf1mom=XEL*PLEPIN
         xf2mom=XPR*PPIN
         pdf1=MSTP(52)
         pdf2=MSTP(51)
C-- This is for Rivet with HepMC2 interface
         if ( hm2OUT .OR. rvtHM .EQ. 2 .AND.rvtOUT) then          
          HM2STATUS=HepMC2_convert_event(OID2)
          HM2STATUS=HepMC2_new_weight(OID2,'Default'//char(0))
          HM2STATUS=HepMC2_set_weight_by_index(OID2,1.0D0,0)
          HM2STATUS=HepMC2_set_cross_section(OID2,
     &  xsec, xsece,iev,iev)
          HM2STATUS=HepMC2_set_pdf_info(OID2,
     &  KE, KP,x1,x2,q2pdfeval,xf1mom,xf2mom ,pdf1,pdf2)
          HM2STATUS=HepMC2_set_attribute_int(OID2,
     & -1,'mpi'//char(0))
          HM2STATUS=HepMC2_set_attribute_int(OID2,isub,
     &    'set_signal_process_id'//char(0))
          HM2STATUS=HepMC2_set_attribute_int(OID2,ISEED,
     &  'random_states1'//char(0))
          HM2STATUS=HepMC2_set_attribute_double(OID2,-1.0D0,
     &   'alphaEM'//char(0))
          HM2STATUS=HepMC2_set_attribute_double(OID2,-1.0D0,
     &   'alphaQCD'//char(0))
          HM2STATUS=HepMC2_set_attribute_double(OID2,q2pdfeval,
     &  'event_scale'//char(0))
          if (HM2OUT) HM2STATUS=HepMC2_write_event(OID2)
         end if
         if (rvtHM .EQ. 2 .AND.rvtOUT) then
         if (I.EQ.1) then 
         RIVETSTATUS=rivet_init_first_event(OID2)
         end if
         RIVETSTATUS=rivet_run(OID2)
         end if
         
C-- This is for Rivet with HepMC3 interface
         if (hm3OUT .OR. rvtHM.EQ.3 .AND.rvtOUT) then
          call pyhepc(1);
          NEVHEPL=IEV
          NHEPL=NHEP
           DO 500 J=1,NHEP
          ISTHEPL(J)=ISTHEP(J)
          IDHEPL(J)=IDHEP(J)
          JMOHEPL(1,J)=JMOHEP(1,J)
          JMOHEPL(2,J)=JMOHEP(2,J)
          JDAHEPL(1,J)=JDAHEP(1,J)
          JDAHEPL(2,J)=JDAHEP(2,J)
          PHEPL(1,J)=PHEP(1,J)
          PHEPL(2,J)=PHEP(2,J)
          PHEPL(3,J)=PHEP(3,J)
          PHEPL(4,J)=PHEP(4,J)
          PHEPL(5,J)=PHEP(5,J)
          VHEPL(1,J)=VHEP(1,J)
          VHEPL(2,J)=VHEP(2,J)
          VHEPL(3,J)=VHEP(3,J)
          VHEPL(4,J)=VHEP(4,J)
  500     CONTINUE  
          HM3STATUS=HepMC3_convert_event(OID3)
          HM3STATUS=HepMC3_set_weight_by_index(OID3,1.0D0,0)
          HM3STATUS=HepMC3_set_cross_section(OID3,
     &  xsec, xsece,iev,iev)
          HM3STATUS=HepMC3_set_pdf_info(OID3,
     &  KE, KP,x1,x2,q2pdfeval,xf1mom,xf2mom ,pdf1,pdf2)
          HM3STATUS=HepMC3_set_attribute_int(OID3,
     & -1,'mpi'//char(0))
          HM3STATUS=HepMC3_set_attribute_int(OID3,isub,
     &    'set_signal_process_id'//char(0))
          HM3STATUS=HepMC3_set_attribute_int(OID3,ISEED,
     &  'random_states1'//char(0))
          HM3STATUS=HepMC3_set_attribute_double(OID3,-1.0D0,
     &   'alphaEM'//char(0))
          HM3STATUS=HepMC3_set_attribute_double(OID3,-1.0D0,
     &   'alphaQCD'//char(0))
          HM3STATUS=HepMC3_set_attribute_double(OID3,q2pdfeval,
     &  'event_scale'//char(0))
          HM3STATUS=HepMC3_set_attribute_int(OID3,ICHNN,
     &  'HSCHNN_ICHNN'//char(0))
          if (hm3OUT) HM3STATUS=HepMC3_write_event(OID3)
C-- This is for Rivet
         if (rvtHM .EQ. 3 .AND.rvtOUT) then
         if (I.EQ.1) then 
         RIVETSTATUS=rivet_init_first_event(OID3)
         end if
         RIVETSTATUS=rivet_run(OID3)
         end if
         HM3STATUS=HepMC3_clear_event(OID3)  
         endif
         CALL PTIME(' event  ',2,0)
C---
   10 CONTINUE
123   Continue
      if (hm3OUT) HM3STATUS=HepMC3_delete_writer(OID3)
      if (hm2OUT) HM2STATUS=HepMC2_delete_writer(OID2)
      if (rvtOUT) then
      RIVETSTATUS=rivet_done(trim(rvtfilnam)//char(0))
      endif   
C---PRINT NR OF GENERATED EVENTS
      CALL RAEND(20)	
C--- user terminate routine (hztool)
cc	CALL hzraend
	
      CALL PTIME(' rapgap  ',2,0)
      CALL PTIME('        ',2,99)
      STOP
      END
      subroutine dummysub(a)
      return
      end
