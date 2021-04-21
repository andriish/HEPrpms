C***********************************************************************
C $Id: areeme.f,v 3.18 2001/11/23 12:02:24 leif Exp $

      SUBROUTINE AREEME

C...ARiadne function E+E- annihilation Matrix Element

C...Generate an event according to alpha_S^2 Matrix elements

      INCLUDE 'arimpl.f'
      INCLUDE 'arstrs.f'
      INCLUDE 'aronia.f'
      INCLUDE 'ardat1.f'
      INCLUDE 'arhide.f'
      INCLUDE 'pydat1.f'
      INCLUDE 'pyjets.f'

      save CT,XQQGG,XQQQQ

      INCLUDE 'ardble.f'


C...Reset some stuff
      MHAR(133)=0

      ECM=SQRT(MAX((P(N-1,4)+P(N,4))**2-(P(N-1,3)+P(N,3))**2-
     $             (P(N-1,2)+P(N,2))**2-(P(N-1,1)+P(N,1))**2,0.0D0))
      KFL=ABS(K(N,2))
      K(N-1,1)=K(N-1,1)+10
      K(N,1)=K(N,1)+10
      IFIRST=N+1

      PH161=DBLE(PHAR(161))
      IF ( PHAR(161).LE.0.0 ) THEN
      PHAR(161)=MAX(sngl(ARX2DB(SQRT(PARJ(125)))*ECM),ARX2DB(PARJ(126)))
      ENDIF
      CUT=(DBLE(PHAR(161))/ECM)**2

      PH160=DBLE(PHAR(160))
      IF ( PHAR(160).LE.0.0 ) THEN
        IF ( MHAR(160).EQ.1.AND.PHAR(161).GT.0.0D0 ) THEN
          PHAR(160)=REAL(ARALPS(DBLE(PHAR(161))**2,ECM**2))
        ELSE
          PHAR(160)=REAL(ARALPS(DBLE(PARA(3))**2,ECM**2))
        ENDIF
      ENDIF
      ALSPI=DBLE(PHAR(160))/ARX2DB(PARU(1))

C...N-jet probabilities

      SIG2=1.0D0
      IF ( MHAR(160).EQ.1 ) THEN
        SIG3=2.0D0*ALSPI*LOG(CUT)**2/3.0D0
      ELSE IF( MHAR(168).EQ.4 ) THEN
        SIG3=4.0D0*ALSPI*LOG(CUT)**2/3.0D0
      ELSE
C        SIG3=4.0D0*ALSPI*LOG(CUT)**2/3.0D0
        SIG3=MAX((2D0*ALSPI/3D0)*((3D0-6D0*CUT+2D0*LOG(CUT))*
     &       LOG(CUT/(1D0-2D0*CUT))+(2.5D0+1.5D0*CUT-6.571D0)*
     &       (1D0-3D0*CUT)+5.833D0*(1D0-3D0*CUT)**2-3.894D0*
     &       (1D0-3D0*CUT)**3+1.342D0*(1D0-3D0*CUT)**4),0.0D0)
      ENDIF
      CT=LOG(1D0/CUT-5D0)
      IF(CUT.LE.0.018D0) THEN
        XQQGG=6.349D0-4.330D0*CT+0.8304D0*CT**2
        XQQQQ=1.25D0*(-0.1080D0+0.01486D0*CT+0.009364D0*CT**2)
      ELSE
        XQQGG=-0.09773D0+0.2959D0*CT-0.2764D0*CT**2+0.08832D0*CT**3
        XQQQQ=1.25D0*(0.003661D0-0.004888D0*CT-0.001081D0*CT**2+
     &       0.002093D0*CT**3)
      ENDIF
      XQQQQ=DBLE(MSTA(15))*XQQQQ/5.0D0
      SIG4G=(ALSPI**2*CT**2)*MAX(XQQGG,0.0D0)
      SIG4Q=(ALSPI**2*CT**2)*MAX(XQQQQ,0.0D0)
C      CF=4.0D0/3.0D0
C      CN=3.0D0
C      TR=0.5D0
C      WTMXGG=0.7D0/CUT**2
C      XQQGG=WTMXGG*CT*(1.0D0-6.0D0*CUT)**2
C      WTMXQQ=0.1125D0*CF*TR/CUT**2
C      XQQQQ=DBLE(MSTA(15))*0.25D0*WTMXQQ*(1.0D0-6.0D0*CUT)**3
C      SIG4G=MAX(XQQGG,0.0D0)*ALSPI**2
C      SIG4Q=MAX(XQQQQ,0.0D0)*ALSPI**2

      IF ( MHAR(161).LT.2 ) THEN
        SIG4G=0.0D0
        SIG4Q=0.0D0
      ENDIF

 200  R=(SIG2+SIG3+SIG4G+SIG4Q)*PYR(0)

      NJET=2
      IF (R.LT.SIG2) THEN
        CALL PY2ENT(IFIRST,KFL,-KFL,ECM)
        MHAR(162)=MHAR(161)
      ELSEIF ( R.LT.SIG2+SIG3 ) THEN
        IF ( MHAR(160).EQ.1.OR.MHAR(168).EQ.4) THEN
          CALL ARX3JT(NJET,CUT,KFL,ECM,X1,X3)
        ELSE
          MSTJ(101)=1
          CALL PYX3JT(NJET,CUT,KFL,ECM,X1,X3)
C          CALL ARXJT3(NJET,CUT,KFL,ECM,X1,X3)
          MSTJ(101)=5
        ENDIF
        IF (NJET.NE.3) GOTO 200
        CALL PY3ENT(IFIRST,KFL,21,-KFL,ECM,X1,X3)
        MHAR(162)=MHAR(161)-1
      ELSE
        KFLN=0
        PARJ(155)=SIG4Q/(SIG4G+SIG4Q)
        CALL ARX4JTOLD(NJET,CUT,KFL,ECM,KFLN,X1,X2,X4,X12,X14)
        IF (NJET.NE.4) GOTO 200
        IF (KFLN.EQ.21) THEN
          CALL AR4ENT(IFIRST,KFL,KFLN,KFLN,-KFL,ECM,X1,X2,X4,X12,X14)
        ELSE
          CALL AR4ENT(IFIRST,KFL,-KFLN,KFLN,-KFL,ECM,X1,X2,X4,X12,X14)
        ENDIF
        IF ( N.NE.IFIRST+3 ) GOTO 200
        MHAR(162)=MHAR(161)-2
      ENDIF
      IF(MSTU(24).NE.0) GOTO 200

C...Angular orientation according to matrix element.
      IF(MSTJ(106).EQ.1) THEN
        CALL PYXDIF(IFIRST-1,NJET,KFL,ECM,CHI,THE,PHI)
        CALL PYROBO(IFIRST,N,0D0,CHI,0D0,0D0,0D0)
        CALL PYROBO(IFIRST,N,THE,PHI,0D0,0D0,0D0)
      ENDIF

      IF ( MHAR(170).GE.0 ) CALL ARMEPS(IFIRST,N)

      IF ( MHAR(163).NE.0.AND.MHAR(170).GE.0 ) GOTO 200

      MHAR(170+NJET)=MHAR(170+NJET)+1
      IF ( NJET.EQ.4.AND.KFLN.NE.21 ) MHAR(175)=MHAR(175)+1
      PHAR(172)=SIG2
      PHAR(173)=SIG3
      PHAR(174)=SIG4G+SIG4Q
      PHAR(175)=SIG4Q

      IF (.NOT.QDUMP) CALL ARDUMP
      PHAR(161)=SNGL(PH161)
      PHAR(160)=SNGL(PH160)

      RETURN

C**** END OF AREEME ****************************************************
      END
