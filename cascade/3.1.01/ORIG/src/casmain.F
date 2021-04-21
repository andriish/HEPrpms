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
        hepmcout = trim(hepmcout_env)//char(0)
        write(6,*) ' hepmcout from environment variable = ',trim(hepmcout)
        else
        hepmcout=trim(hepmcout)//char(0)
        write(6,*) ' hepmcout from steering file = ',trim(hepmcout)
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
        
        call convhepmc(iev, isub, weighthepmc, xsec, xsece, idP1,idP2,xidP1,xidP2,scale_process,pdf1,pdf2 )
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
      call convhepmc(iev, isub, weighthepmc, xsec, xsece, idP1,idP2,xidP1,xidP2,scale_process,pdf1,pdf2 )
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
