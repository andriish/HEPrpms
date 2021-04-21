      PROGRAM CASMAIN
      Implicit None
c	Integer N1,N2
	Integer K1,K2,LUX
#include "casluco.inc"
#include "casparas.inc"
#include "caspara5.inc"
#include "casvalues.inc"
#include "caseffic.inc"
#include "caslujets.inc"
#include "casludat1.inc"
#include "casludat2.inc"
#include "casherup.inc"
#include "caslhe.inc"
#include "casevent.inc"
      double precision neg_weight,neg_event
      common/lheweight/neg_weight,neg_event
      

      integer iev,isub
      double precision xsec,xsece

      Integer j
      real pta0, eta0, dna0, ptam, ptrms, etam, etrms, dnam, dnrms
      real ptrm0, etrm0, dnrm0
      real av, etas, pts, cost, eta, pt 
      real sec0, secrm0, sec, secrms
      real val, errval 
      
#include "casiglu.inc"

      Integer ISEED,I
		
      Integer IVEC(25)
      Integer Irr	
	real timeleft
	Integer Minuts
	External Minuts
      External Pydata

      character*255 caseed
      character*255 test
      
      Double Precision weighthepmc,sumOfWeights
      
      character *512 hepmcout_env
      Integer II,i_scale
      
      Real start,finish

      Integer iflag, idum
      Double Precision dum
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
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
      
      sumOfWeights = 0d0
      call cpu_time(start)

c  read default paramters and input steering file	
      call input_steer
            
c
c Start integration of x-section
c

      CALL CASCADE
      
c      call PYLIST(12)
      CALL GETENV("HEPMCOUT", hepmcout_env)
      if(hepmcout_env.ne.' ') then
        hm2filnam = trim(hepmcout_env)//char(0)
        write(6,*) ' hepmcout from environment variable = ',trim(hm2filnam)
        else
        write(6,*) ' hepmcout from steering file = ',trim(hm2filnam)
      endif
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
      write(6,*)'rvtana(i)= ',i,'->',trim(rvtana(i)),'<-'
      if (len(trim(rvtana(i))) .EQ. 0 ) continue
      RIVETSTATUS=rivet_add_analysis(trim(rvtana(i))//char(0))
      end do
      if (len(rvtrun).GT. 0) then
      RIVETSTATUS=rivet_init(trim(rvtrun)//char(0))
      end if
      endif      
      
      
      CALL GETENV("CASEED", caseed)
c note CASEED can have only 10 digits
c i.e   export CASEED='1234567890
      if(CASEED.eq.' ') then
         write(6,*) ' CASEED set = 12345 '
         iseed = 12345
      else
         read(caseed,'(I30)') iseed  
         write(6,*) ' CASEED from environment variable ', iseed
      endif
      LUX = 4
      K1=0
      K2=0
      CALL RLUXGO(LUX,ISEED,K1,K2)


c
c Print out result of integration of x-section
c
 
      CALL CAEND(1)

c      write(6,*) ' book histo '
      call book_histo
c
c Start event loop
      Do I=1,max(4,Nevent)

c generate an event
         CALL EVENT
         If(Iflag_event.eq.1) goto 123
         
         IF(MSTU(24).NE.0.or.LST(21).NE.0) goto 111

        iev = I
        isub = ipro
c xsec in CASCADE in nb, hepmc need pb
        xsec = avgi *1000.
        xsece = sd *1000.
        weighthepmc = 1d0
        if(ilha.ne.0) then
          weighthepmc = normhepmc*weight
          do i_scale=1,nw_scale
            w_scale(i_scale) = w_scale(i_scale)*normhepmc
c            write(6,*) ' weights ',i_scale,w_scale(i_scale), weighthepmc
          enddo
          do i_scale=1,nw_pdf
            w_pdf(i_scale) = w_pdf(i_scale)*normhepmc
          enddo
          if(weight.le.0) then
c             write(6,*) ' casmain weight < 0 ', weight,weighthepmc 
             neg_weight= neg_weight + weighthepmc
             neg_event= neg_event + 1
          endif
c         write(6,*) ' casmain ',  normhepmc,weight
          
          sumOfWeights = sumOfWeights + weighthepmc
        endif
        
C-- This is for Rivet with HepMC2 interface
         if ( hm2OUT .OR. rvtHM .EQ. 2 .AND. rvtOUT) then 
          HM2STATUS=HepMC2_convert_event(OID2)
          HM2STATUS=HepMC2_new_weight(OID2,'Default'//char(0))
          HM2STATUS=HepMC2_set_weight_by_index(OID2,weighthepmc,0)
          HM2STATUS=HepMC2_set_cross_section(OID2,
     &  xsec, xsece,iev,iev)
C -- FIXME/CHECK the meaning of the variables
          HM2STATUS=HepMC2_set_pdf_info(OID2,
     &  idP1,idP2,xidP1,xidP2,scale_process,xidP1,xidP2,pdf1,pdf2)
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
          HM2STATUS=HepMC2_set_attribute_double(OID2,scale_process,
     &  'event_scale'//char(0))
         if (HM2OUT) HM2STATUS=HepMC2_write_event(OID2)     
         if (rvtHM .EQ. 2 .AND.rvtOUT) then
         if (I.EQ.1) then 
         RIVETSTATUS=rivet_init_first_event(OID2)
         end if
         RIVETSTATUS=rivet_run(OID2)
         end if
         HM2STATUS=HepMC2_clear_event(OID2)  
         end if         

C-- This is for Rivet with HepMC3 interface
         if (hm3OUT .OR. rvtHM.EQ.3 .AND.rvtOUT) then
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
          HM3STATUS=HepMC3_set_weight_by_index(OID3,weighthepmc,0)
          HM3STATUS=HepMC3_set_cross_section(OID3,
     &  xsec, xsece,iev,iev)
C -- FIXME/CHECK the meaning of the variables
          HM3STATUS=HepMC3_set_pdf_info(OID3,
     &  idP1,idP2,xidP1,xidP2,scale_process,xidP1,xidP2,pdf1,pdf2)
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
          HM3STATUS=HepMC3_set_attribute_double(OID3,scale_process,
     &  'event_scale'//char(0))
C -- FIXME and put some physical scale!
          HM3STATUS=HepMC3_set_attribute_double(OID3,0.4242D0,
     &  'tmd_scale'//char(0))
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

        if(N.le.0) write(6,*) "N=0  !!!!!"
111     Continue        
      Enddo
123   Continue
c
c Print out of generated events summary
c 
      write(6,*) ' casmain ', Nevent
      if(ilha.ne.0) then
         avgi = sumOfWeights/float(Nevent)*dabs(avgi)
         sd = sumOfWeights/float(Nevent)*dabs(sd)
      endif
c xsec in CASCADE in nv, hepmc need pb
      xsec = avgi * 1000.
      xsece = sd *1000.
      WRITE(6,10000) xsec, xsece
CAV      call convhepmc(iev, isub, weighthepmc, xsec, xsece, idP1,idP2,xidP1,xidP2,scale_process,pdf1,pdf2 )
   
      if (hm3OUT) HM3STATUS=HepMC3_delete_writer(OID3)
      if (hm2OUT) HM2STATUS=HepMC2_delete_writer(OID2)
      if (rvtOUT) then
      RIVETSTATUS=rivet_done(trim(rvtfilnam)//char(0))
      endif
      CALL CAEND(20)
c
10000 FORMAT('  final x-sections: sigma  = ',G10.3,' pb   +/- ',G10.3)
      sec = AVGI
      secrms = SD
      	
      if(ilha.ne.0) then          
         write(6,*) ' sum of weights ',sumOfWeights   
      endif   
      iflag = 3

      call print_histo
      
      call cpu_time(finish)
      write(6,*) ' CASCADE running time = ',finish-start,' seconds.  Time/event =',(finish-start)/Nevent
      STOP
      END
