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

      character*512 hepmcout,lhefile
      common/CAHEPMCOUT/hepmcout,lhefile
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

      Integer I,ISEED
      character*255 raseed
      character*255 test
      
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
      WRITE (6,*) ' test raseed:',Iseed
      LUX = 4
      K1=0
      K2=0
      CALL RLUXGO(LUX,ISEED,K1,K2)
C---initialise RAPGAP parameters
      CALL GRAINI
C-- read in parameters from file 
      Call Steer
C-- change standard parameters of RAPGAP 	
	Call rapcha
C-- change standard parameters of HERACLES	
	Call hercha
C-- change standard parameters of JETSET
	Call pytcha
	IF(IFPS.EQ.10) then
C Initialize ARIADNE
         CALL ARINIT('RAPGAP')
C-- change standard parameters of ARIADNE
	   Call aricha
      endif
C--- CALCULATE X SECTION
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
          
         call convhepmc(iev, isub, xsec, xsece,
     &  KE, KP,x1,x2,q2pdfeval,xf1mom,xf2mom ,pdf1,pdf2  )
         CALL PTIME(' event  ',2,0)
C---
   10 CONTINUE
123   Continue
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
