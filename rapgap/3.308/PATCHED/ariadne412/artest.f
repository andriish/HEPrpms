C***********************************************************************
C $Id: artest.f,v 3.8 2001/11/23 12:02:25 leif Exp $

      SUBROUTINE ARTEST(IPRINT)

C...ARiadne subroutine TEST

C...Performs various tests on Ariadne

      INCLUDE 'arimpl.f'
      INCLUDE 'ardat1.f'
      INCLUDE 'ardat3.f'
      INCLUDE 'arint1.f'
      INCLUDE 'pyjets.f'
      INCLUDE 'pydat1.f'
      INCLUDE 'ardble.f'

      MSTA(9)=1
      MSTA(6)=-1
      MSTA(20)=1

      MSTJ(21)=0

      CALL ARINIT('ARIADNE')

      DO 110 I=1,10000

        PARA(1)=0.1+0.5*REAL(PYR(IDUM))
        PARA(2)=0.05+0.25*REAL(PYR(IDUM))
        PARA(3)=PARA(1)+0.1+REAL(PYR(IDUM))
        PARA(5)=0.1+REAL(PYR(IDUM))
        PARA(10)=0.5+REAL(PYR(IDUM))
        PARA(11)=0.5+REAL(PYR(IDUM))
        PARA(12)=5.0+10.0*REAL(PYR(IDUM))
        PARA(25)=2.0*REAL(PYR(IDUM))

        MSTA(11)=INT(5.0D0*PYR(IDUM))
        MSTA(12)=INT(2.0D0*PYR(IDUM))
        MSTA(16)=INT(3.0D0*PYR(IDUM))
        MSTA(17)=INT(4.0D0*PYR(IDUM))
        MSTA(18)=INT(4.0D0*PYR(IDUM))
        MSTA(19)=INT(2.0D0*PYR(IDUM))
        MSTA(25)=INT(3.0D0*PYR(IDUM))
        MSTA(31)=INT(2.0D0*PYR(IDUM))

        W=10.0D0*EXP(PYR(IDUM)*LOG(1000.0D0))
 100    SM1=PYR(IDUM)*20.0D0
        SM2=PYR(IDUM)*20.0D0
        D1=0.5D0*(W**2+SM1**2-SM2**2)/W
        D2=W-D1
        IF (D1.LT.SM1) GOTO 100
        IF (D2.LT.SM2) GOTO 100
        NE1=INT(PYR(IDUM)*4.0D0)
        NE2=INT(PYR(IDUM)*4.0D0)
        N=2
        P(1,1)=ARDB2X(0.0D0)
        P(1,2)=ARDB2X(0.0D0)
        P(1,3)=-ARDB2X(SQRT(D1**2-SM1**2))
        P(1,4)=ARDB2X(D1)
        P(1,5)=ARDB2X(SM1)
        K(1,1)=2
        K(1,2)=1
        K(1,3)=999
        K(1,4)=NE1
        K(1,5)=0
        P(2,1)=ARDB2X(0.0D0)
        P(2,2)=ARDB2X(0.0D0)
        P(2,3)=ARDB2X(SQRT(D2**2-SM2**2))
        P(2,4)=ARDB2X(D2)
        P(2,5)=ARDB2X(SM2)
        K(2,1)=1
        K(2,2)=-1
        K(2,3)=999
        K(2,4)=NE2
        K(2,5)=0

        CALL AREXEC
        IF (PYR(IDUM).GT.0.99D0) CALL PYEXEC

        IF (IPRINT.GT.0.AND.MOD(I,100).EQ.0) CALL PYLIST(2)

 110  CONTINUE

      NERRA=0
      DO 200 I=1,40
        NERRA=NERRA+IWRN(I)
 200  CONTINUE

      NWRNA=IWRN(13)+IWRN(10)
      NERRA=NERRA-NWRNA
      IF (NERRA.EQ.0) THEN
        WRITE(MSTA(7),1000)
      ELSE
        WRITE(MSTA(7),1010) NERRA
      ENDIF

      IF (NWRNA.GT.0) WRITE(MSTA(7),1020) NWRNA

      NWRNJ=MSTU(27)
      NERRJ=MSTU(23)

      IF (NWRNJ+NERRJ.NE.0) WRITE(MSTA(7),1030) NWRNJ,NERRJ

 1000 FORMAT('No errors experienced by Ariadne.')
 1010 FORMAT(I5,' errors occurred in Ariadne.')
 1020 FORMAT(I5,' Non-serious warnings issued by Ariadne')
 1030 FORMAT(I5,' warnings and',I5,' errors occured in JETSET when ',
     $     'attempting to fragment',/
     $     ,' parton state produced by Ariadne.')

      RETURN

C**** END OF ARTEST ****************************************************
      END
