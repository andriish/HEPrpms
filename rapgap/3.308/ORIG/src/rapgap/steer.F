	subroutine steer
	
	Implicit None
	
      character *132 TXT
	character *6 char
	character *10 char2
      character *132 c1
	Integer i1,i2,i3
	Integer inte,i,j
	real r3
	
	
	Integer nloop,nmax
	Parameter (nmax=1000)
      character *4   para
	Real Rval 
	Integer Ival,Ld,Le
      character*132 Cval 
	Common/steering/Nloop,Ld(nmax),Le(nmax),Ival(nmax),
     &	para(nmax),Rval(nmax),Cval(nmax)
	Integer Nevent
	Common/steer1/Nevent
	character *132 filnam
	Integer lunhbk
	Common/steer2/lunhbk,filnam
      LOGICAL         HBKOUT
      COMMON /QHBKLO/ HBKOUT
	
	
	nloop = 0
   20 Read(5,101,END=100) TXT
c      write(6,*) ' reading txt: ',txt
      If(TXT(1:1).EQ.'*') then
*          WRITE(6,*)  TXT
         GOTO 20
      Endif
c check for special cards	
      If(TXT(1:6).EQ.'NEVENT') then
         read(txt,*) char,i1
         NEVENT = i1
	   write(6,*) ' Nevents = ',i1
         GOTO 20
      Endif
      If(TXT(1:6).EQ.'HBKOUT') then
         read(txt,*) char,lunhbk,filnam
	   write(6,*) char,lunhbk,filnam
	   HBKOUT=.TRUE.
         GOTO 20
      Endif
      
	If(TXT(1:4).EQ.'END$') goto 90
	inte = 0 
      do j=2,10
	i=j-1
	if(TXT(i:i).EQ."'".and.
     &   (TXT(J:J).EQ.'H'.OR.TXT(J:J).EQ.'I'.OR.TXT(J:J).EQ.'J'.
     &   OR.TXT(J:J).EQ.'K'.OR.TXT(J:J).EQ.'L'.OR.TXT(J:J).EQ.'M'.
     &   OR.TXT(J:J).EQ.'N')) then
         inte = 1
	   elseif(TXT(i:i).EQ."'".and.TXT(J:J).EQ.'U') then
         inte=2
	endif
      if(TXT(i:i).EQ."'".and.TXT(J:J+2).EQ.'PDF') then
         inte=2
	endif
      
	if(TXT(i:i).EQ."'".and.
     &   (TXT(J:J+1).EQ.'KT'.OR.TXT(J:J+1).EQ.'kt')) Then
         inte = 0
      endif
	if(TXT(i:i).EQ."'".and.
     &   (TXT(J:J+2).EQ.'LQ2'.OR.TXT(J:J+2).EQ.'lq2')) Then
         inte = 0
      endif
	enddo
	nloop = nloop + 1
	Ival(nloop) = -9999
	Rval(nloop) = -9999.
	Cval(nloop) = ' '
	if(inte.eq.1) then
c      write(6,*) ' txt ',txt
      read(txt,*) char,i1,i2,i3,char2
c	write(6,*) ' output Integer :',char,i1,i2,i3 
	Ival(nloop) = i3
	elseif(inte.eq.2) then
      read(txt,*) char,c1
c	write(6,*) ' output Charcter :',char,c1,nloop
	Cval(nloop) = c1
      else
      read(txt,*) char,i1,i2,r3,char2
c	write(6,*) ' output real    :',char,i1,i2,r3
	Rval(nloop) = r3
	endif
	Ld(nloop) = i1
	Le(nloop) = i2
	para(nloop) = char
	
c	write(6,*) ' output : ',nloop,para(nloop),Ival(nloop),rval(nloop),cval(nloop)
	
      goto 20
	
   90 Continue
  100 Return
	
  101 Format(A132)
      End
