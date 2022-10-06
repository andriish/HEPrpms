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
#include "casfNLO.inc"
#include "casevent.inc"
      double precision neg_weight,neg_event
      common/lheweight/neg_weight,neg_event
      

      integer iev,isub,nev
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
      
      character *4096 hepmcout_env
      Integer II,i_scale
      
      Real start,finish

      Integer iflag, idum
      Double Precision dum

      Integer IMultiJetMerge,MinJets_CAS,MaxJets_CAS,Nqmax_CAS,iMLM,iLHE_tmd
      Common/Merge/IMultiJetMerge,MinJets_CAS,MaxJets_CAS,Nqmax_CAS,iMLM,iLHE_tmd
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      character *512 temp_wname
      Integer jpdf, jscale, wid
#include "HepMCSteering.inc"
#include "Pythia6ToHepMC.inc"
      INTEGER OID, HMSTATUS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      
      sumOfWeights = 0d0
      nev = 0
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
        hmfilnam = trim(hepmcout_env)//char(0)
        write(6,*) ' hepmcout from environment variable = ',trim(hmfilnam), hmOUT
        hmOUT = .TRUE.
        else
        write(6,*) ' hepmcout from steering file = ',trim(hmfilnam), hmOUT
      endif
C-- Init HepMC

      if (hmOUT.and.p6frag.eq.1) then
        if (hmfilfor.EQ.0) hmfilfor=2
        OID=HepMC_new_writer(0,hmfilfor,trim(hmfilnam)//char(0))
        NEVHEP=-123456
        HMSTATUS=HepMC_set_hepevt_address(NEVHEPL)
        write(6,*) ' hepmcout ',trim(hmfilnam),hmfilfor,OID
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
        Nev = Nev + 1 
c generate an event
        CALL EVENT
        If(Iflag_event.eq.1) goto 123
         
        IF(MSTU(24).NE.0.or.LST(21).NE.0) goto 111

        if(ilha.eq.0) iev = I
        isub = ipro
c xsec in CASCADE in nb, hepmc need pb
        xsec = avgi *1000.
        xsece = sd *1000.
        weighthepmc = 1d0
        if(ilha.ne.0) then
          if(In_eventgroup.eq.1.and.ifixed_NLO.eq.1) then 
            Iev = Iev + 1
            elseif(ifixed_NLO.eq.0) then
            Iev = Iev + 1
          endif
c          write(6,*) ' new event ',Iev,I, In_eventgroup
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
        

        if(hmOUT.and.p6frag.eq.0) then 
c            write(6,*) 'after event generated ' 

c            do  jscale = 1, nw_scale
c              wid = id_scale(jscale)
c              write(6,*) ' before ', wid,w_scale(jscale), jscale
c            enddo
            call convhepmcP8(iev, isub, weighthepmc, xsec, xsece, idP1,idP2,xidP1,xidP2,scale_process,pdf1,pdf2 )
          elseif (hmOUT.and.p6frag.eq.1) then 
C-- This is for HepMC interface
         
          NEVHEP=IEV
          call pyhepc(1)
          HMSTATUS=HepMC_version()
          if (HMSTATUS.EQ.3) then
           NEVHEPL=IEV
           NHEPL=NHEP
           DO J=1,NHEP
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
           END DO
          END IF
          HMSTATUS=HepMC_convert_event(OID)
          HMSTATUS=HepMC_set_event_number(OID,IEV)
          HMSTATUS=HepMC_set_weight_by_name(OID,dble(weighthepmc),'Default'//char(0))
          if (nw_scale .GT. 0) THEN 
C         Convert weight names in MadGraph5 convention to the convention outlined
C         in https://arxiv.org/pdf/1405.1067.pdf, page  162ff.
            do  jscale = 1, nw_scale
              wid = id_scale(jscale)
              if (wid.EQ.1001) HMSTATUS=HepMC_set_weight_by_name(OID,dble(w_scale(jscale)),'MUR1.0_MUF1.0'//char(0))
              if (wid.EQ.1002) HMSTATUS=HepMC_set_weight_by_name(OID,dble(w_scale(jscale)),'MUR1.0_MUF2.0'//char(0))
              if (wid.EQ.1003) HMSTATUS=HepMC_set_weight_by_name(OID,dble(w_scale(jscale)),'MUR1.0_MUF0.5'//char(0))
              if (wid.EQ.1004) HMSTATUS=HepMC_set_weight_by_name(OID,dble(w_scale(jscale)),'MUR2.0_MUF1.0'//char(0))
              if (wid.EQ.1005) HMSTATUS=HepMC_set_weight_by_name(OID,dble(w_scale(jscale)),'MUR2.0_MUF2.0'//char(0))
              if (wid.EQ.1006) HMSTATUS=HepMC_set_weight_by_name(OID,dble(w_scale(jscale)),'MUR2.0_MUF0.5'//char(0))
              if (wid.EQ.1007) HMSTATUS=HepMC_set_weight_by_name(OID,dble(w_scale(jscale)),'MUR0.5_MUF1.0'//char(0))
              if (wid.EQ.1008) HMSTATUS=HepMC_set_weight_by_name(OID,dble(w_scale(jscale)),'MUR0.5_MUF2.0'//char(0))
              if (wid.EQ.1009) HMSTATUS=HepMC_set_weight_by_name(OID,dble(w_scale(jscale)),'MUR0.5_MUF0.5'//char(0))
            end do
          end if
          if (nw_pdf .GT. 0) THEN 
C         Convert PDF weight names to the convention outlined
C         in https://arxiv.org/pdf/1405.1067.pdf, page  162ff.
C         but indicating that it is the TMDlib numbering 
            do jpdf = 1, nw_pdf
             wid = id_pdf(jpdf)
             write(temp_wname,*) wid,'/0' 
             HMSTATUS=HepMC_set_weight_by_name(OID, dble(w_pdf(jpdf)),'TMD'//temp_wname//char(0))
            end do
          end if 
          if(ilha.ne.0) then
           if (ifixed_NLO.eq.1) then
             HMSTATUS=HepMC_set_cross_section(OID,
     & sumOfWeights*dabs(avgi)* 1000.0D0, sumOfWeights*dabs(sd)* 1000.0D0, iev,iev)
           else 
             if (iMLM.eq.0) then
             HMSTATUS=HepMC_set_cross_section(OID,
     & sumOfWeights/dble(Iev)*dabs(avgi)*1000.0D0, sumOfWeights/dble(Iev)*dabs(sd)* 1000.0D0, iev,iev)
             else
             HMSTATUS=HepMC_set_cross_section(OID,
     & sumOfWeights/dble(Nev)*dabs(avgi)*1000.0D0, sumOfWeights/dble(Nev)*dabs(sd)* 1000.0D0, iev,iev)
             endif
           endif 
          else
             HMSTATUS=HepMC_set_cross_section(OID,
     & dabs(avgi)*1000.0D0, dabs(sd)* 1000.0D0, iev,iev)         
          endif
          HMSTATUS=HepMC_set_pdf_info(OID,idP1,idP2,dble(xidP1),dble(xidP2),dble(scale_process),dble(xidP1),dble(xidP2),pdf1,pdf2)
          HMSTATUS=HepMC_set_attribute_int(OID,-1,'mpi'//char(0))
          HMSTATUS=HepMC_set_attribute_int(OID,isub,'set_signal_process_id'//char(0))
          HMSTATUS=HepMC_set_attribute_int(OID,ISEED,'random_states1'//char(0))
          HMSTATUS=HepMC_set_attribute_double(OID,-1.0D0,'alphaQED'//char(0))
          HMSTATUS=HepMC_set_attribute_double(OID,-1.0D0,'alphaQCD'//char(0))
          HMSTATUS=HepMC_set_attribute_double(OID,dble(scale_process),'event_scale'//char(0))
C Set here some physicl TM scale. Is meaningfull only in HepMC3
          HMSTATUS=HepMC_set_attribute_double(OID,-1.0D0,'tmd_scale'//char(0))
          HMSTATUS=HepMC_write_event(OID)
          HMSTATUS=HepMC_clear_event(OID)  
         endif
C-- End HepMC  interface

        if(N.le.0) write(6,*) "N=0  !!!!!"
111     Continue        
      Enddo
123   Continue
c
c Print out of generated events summary
c 
      write(6,*) ' casmain ', Iev,Nev,dabs(avgi)
      if(ilha.ne.0) then
         if (ifixed_NLO.eq.1) then
           avgi = sumOfWeights*dabs(avgi)
           sd = sumOfWeights*dabs(sd)
           else 
           if (iMLM.eq.0) then
             avgi = sumOfWeights/float(Iev)*dabs(avgi)
             sd = sumOfWeights/float(Iev)*dabs(sd)
           else
             avgi = sumOfWeights/float(Nev)*dabs(avgi)
             sd = sumOfWeights/float(Nev)*dabs(sd)
           endif
         endif 
      endif
c xsec in CASCADE in nb, hepmc need pb
      xsec = avgi * 1000.
      xsece = sd *1000.
      WRITE(6,10000) xsec, xsece
      if(hmOUT.and.p6frag.eq.0) then 
         call convhepmcP8(iev, isub, weighthepmc, xsec, xsece, idP1,idP2,xidP1,xidP2,scale_process,pdf1,pdf2 )
CAV   call convhepmc(iev, isub, weighthepmc, xsec, xsece, idP1,idP2,xidP1,xidP2,scale_process,pdf1,pdf2 )
         elseif (hmOUT.and.p6frag.eq.1) then
         HMSTATUS=HepMC_delete_writer(OID)
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
