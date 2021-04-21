      SUBROUTINE UGOPEN(OPTN,IDNT)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                       OPEN A GRAPHIC DEVICE                       *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO OPEN A GRAPHIC DEVICE AND MAKE    *
C *  IT READY FOR USE.  A GRAPHIC DEVICE MUST BE OPENED BEFORE ANY    *
C *  USE CAN BE MADE OF IT.  OPENING A GRAPHIC DEVICE MAKES IT THE    *
C *  ACTIVE DEVICE.                                                   *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGOPEN(OPTN,IDNT)                                         *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    OPTN  THE OPTIONS LIST.                                        *
C *    IDNT  THE IDENTIFICATION OF THE GRAPHIC DEVICE.                *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      CHARACTER*(*) OPTN
      INTEGER       IDNT

#if ( defined(__LINUX_AOUT) != 1 )
      EXTERNAL UGNUCL , UGSIMP , UGDUPL
#endif

C  NUMBER OF SUPPORTED GRAPHIC DEVICES.
      INTEGER       NSGD
      PARAMETER     (NSGD=45)
C
      INCLUDE       'UGSYSTEM:UGMCACBK.FOR'
C
      INCLUDE       'UGSYSTEM:UGDDACBK.FOR'
C
      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'
C
      INTEGER*4     EXST(1),EXIX
      EQUIVALENCE   (EXIX,EXST(1))
C
      INTEGER       INDX
      CHARACTER*8   NAMS(NSGD)
      INTEGER       DDIN(2),DDEX(1)
C
      REAL          FLT1,FLT2
      INTEGER       INT1,INT2
      INTEGER       DESC_NUM
      PARAMETER    (DESC_NUM = 45)
      INTEGER*4     INST(4,DESC_NUM)
      CHARACTER*16  IFLAG(DESC_NUM)
C
      DATA   (INST(I,1),I=1,4)  / 1,7,1,1  /, IFLAG(1)  / 'SDDXCOW'/
      DATA   (INST(I,2),I=1,4)  / 1,7,1,2  /, IFLAG(2)  / 'SEQGIGI'/
      DATA   (INST(I,3),I=1,4)  / 1,7,1,3  /, IFLAG(3)  / 'SDDGIGI'/
      DATA   (INST(I,4),I=1,4)  / 1,7,1,4  /, IFLAG(4)  / 'DECGIGI'/
      DATA   (INST(I,5),I=1,4)  / 1,7,1,5  /, IFLAG(5)  / 'SDDVST2'/
      DATA   (INST(I,6),I=1,4)  / 1,7,1,6  /, IFLAG(6)  / 'DECVST2'/
      DATA   (INST(I,7),I=1,4)  / 1,7,1,7  /, IFLAG(7)  / 'SDDGRIN'/
      DATA   (INST(I,8),I=1,4)  / 1,7,1,8  /, IFLAG(8)  / 'IMGN300'/
      DATA   (INST(I,9),I=1,4)  / 1,7,1,9  /, IFLAG(9)  / 'IMGNIBM'/
      DATA   (INST(I,10),I=1,4) / 1,7,1,10 /, IFLAG(10) / 'SDDMETH'/
      DATA   (INST(I,11),I=1,4) / 1,7,1,11 /, IFLAG(11) / 'POSTSCR'/
      DATA   (INST(I,12),I=1,4) / 1,7,1,12 /, IFLAG(12) / 'PRNTRNX'/
      DATA   (INST(I,13),I=1,4) / 1,7,1,13 /, IFLAG(13) / 'SEQSEKO'/
      DATA   (INST(I,14),I=1,4) / 1,7,1,14 /, IFLAG(14) / 'SDDSKRS'/
      DATA   (INST(I,15),I=1,4) / 1,7,1,15 /, IFLAG(15) / 'SEIKORS'/
      DATA   (INST(I,16),I=1,4) / 1,7,1,16 /, IFLAG(16) / 'SDDSKRF'/
      DATA   (INST(I,17),I=1,4) / 1,7,1,17 /, IFLAG(17) / 'SEIKORF'/
      DATA   (INST(I,18),I=1,4) / 1,7,1,18 /, IFLAG(18) / 'SDDSXSS'/
      DATA   (INST(I,19),I=1,4) / 1,7,1,19 /, IFLAG(19) / 'TALARIS'/
      DATA   (INST(I,20),I=1,4) / 1,7,1,20 /, IFLAG(20) / 'SEQ4010'/
      DATA   (INST(I,21),I=1,4) / 1,7,1,21 /, IFLAG(21) / 'SDD4010'/
      DATA   (INST(I,22),I=1,4) / 1,7,1,22 /, IFLAG(22) / 'TEK4010'/
      DATA   (INST(I,23),I=1,4) / 1,7,1,23 /, IFLAG(23) / 'SEQTKEM'/
      DATA   (INST(I,24),I=1,4) / 1,7,1,24 /, IFLAG(24) / 'SDDTKEM'/
      DATA   (INST(I,25),I=1,4) / 1,7,1,25 /, IFLAG(25) / 'TEKEMUL'/
      DATA   (INST(I,26),I=1,4) / 1,7,1,26 /, IFLAG(26) / 'SEQ4027'/
      DATA   (INST(I,27),I=1,4) / 1,7,1,27 /, IFLAG(27) / 'SDD4027'/
      DATA   (INST(I,28),I=1,4) / 1,7,1,28 /, IFLAG(28) / 'TEK4027'/
      DATA   (INST(I,29),I=1,4) / 1,7,1,29 /, IFLAG(29) / 'SEQ4105'/
      DATA   (INST(I,30),I=1,4) / 1,7,1,30 /, IFLAG(30) / 'SDD4105'/
      DATA   (INST(I,31),I=1,4) / 1,7,1,31 /, IFLAG(31) / 'TEK4105'/
      DATA   (INST(I,32),I=1,4) / 1,7,1,32 /, IFLAG(32) / 'SDD4207'/
      DATA   (INST(I,33),I=1,4) / 1,7,1,33 /, IFLAG(33) / 'TEK4207'/
      DATA   (INST(I,34),I=1,4) / 1,7,1,34 /, IFLAG(34) / 'TEK4510'/
      DATA   (INST(I,35),I=1,4) / 1,7,1,35 /, IFLAG(35) / 'VEP12FF'/
      DATA   (INST(I,36),I=1,4) / 1,7,1,36 /, IFLAG(36) / 'SDDXWDO'/
      DATA   (INST(I,37),I=1,4) / 1,7,1,37 /, IFLAG(37) / 'XWINDOW'/
      DATA   (INST(I,38),I=1,4) / 1,7,1,38 /, IFLAG(38) / 'PDEVUGS'/
      DATA   (INST(I,39),I=1,4) / 1,7,1,39 /, IFLAG(39) / 'PDEVLIN'/
      DATA   (INST(I,40),I=1,4) / 1,7,1,40 /, IFLAG(40) / 'PDEVSVR'/
      DATA   (INST(I,41),I=1,4) / 1,7,1,41 /, IFLAG(41) / 'PDEVMVR'/
      DATA   (INST(I,42),I=1,4) / 1,7,1,42 /, IFLAG(42) / 'GENWKST'/
      DATA   (INST(I,43),I=1,4) / 1,7,1,43 /, IFLAG(43) / 'TESTDEV'/
      DATA   (INST(I,44),I=1,4) / 1,7,1,44 /, IFLAG(44) / 'QMS1200'/
      DATA   (INST(I,45),I=1,4) / 1,4,1,45 /, IFLAG(45) / 'EPSF   '/
C
      DATA          NAMS/'UGCW01  ',
     X                   'UGGS01  ',
     X                   'UGGD01  ',
     X                   'UGGI01  ',
     X                   'UGVS01  ',
     X                   'UGVI01  ',
     X                   'UGGR01  ',
     X                   'UGIN01  ',
     X                   'UGIX01  ',
     X                   'UGMT01  ',
     X                   'UGPR01  ',
     X                   'UGPX01  ',
     X                   'UGSA01  ',
     X                   'UGSB01  ',
     X                   'UGSC01  ',
     X                   'UGSD01  ',
     X                   'UGSE01  ',
     X                   'UGSX01  ',
     X                   'UGTA01  ',
     X                   'UGTS01  ',
     X                   'UGTD01  ',
     X                   'UGTX01  ',
     X                   'UGXA01  ',
     X                   'UGXB01  ',
     X                   'UGXC01  ',
     X                   'UGUS01  ',
     X                   'UGUD01  ',
     X                   'UGUX01  ',
     X                   'UGWA01  ',
     X                   'UGWB01  ',
     X                   'UGWC01  ',
     X                   'UGWD01  ',
     X                   'UGWE01  ',
     X                   'UGWZ01  ',
     X                   'UGVF01  ',
     X                   'UGXS01  ',
     X                   'UGXI01  ',
     X                   'UGPU01  ',
     X                   'UGPL01  ',
     X                   'UGPS01  ',
     X                   'UGPM01  ',
     X                   'UGPI01  ',
     X                   'UGZZ01  ',
     X                   'UGQM01  ',
     X                   'UGEP01  '/
C
C  SCAN THE OPTIONS LIST.
      EXIX=0
      CALL UGOPTION(OPTN,DESC_NUM,INST,IFLAG,EXST,EXSTR)
C
C  CHECK FOR INPUT ERRORS.
      IF (EXIX.EQ.0) GO TO 501
      IF (IDNT.EQ.0) GO TO 502
      INDX=0
      DO 101 INT1=MCAZ1,1,-1
        IF (MCAOI(INT1).EQ.IDNT) GO TO 503
        IF (MCAOI(INT1).EQ.0) INDX=INT1
  101 CONTINUE
      IF (INDX.EQ.0) GO TO 504
C
C  IF A DEVICE IS ACTIVE, MAKE IT INACTIVE.
      IF (DDAAI.NE.0) THEN
        IF (DDAPX.EQ.0) CALL UGZ003(0,DDALX,DDAPX)
        CALL UGB001(%VAL(DDAPX),1,%VAL(DDACX),1,DDALX)
        IF (DDAPA.EQ.0) THEN
          CALL UGZ003(0,DDALG,DDAPA)
          MCAOP(DDAAI)=DDAPA
        END IF
        CALL UGB001(%VAL(DDAPA),1,DDARY(1),1,DDALG)
        DDAAI=0
      END IF
C
C  ACTIVATE THE DEVICE DEPENDENT CODE.
      CALL UGZ002(0,NAMS(EXIX),DDAAC)
      IF (DDAAC.EQ.0) THEN
          WRITE(6,99)
  99      FORMAT(' DDAAC FROM UGZ002 = 0')
          GO TO 505
      END IF
C
C  PRE-INITIALIZE THE DEVICE DEPENDENT AREA.
      DDAPA=0
      DDAPX=0
      DDAAN=NAMS(EXIX)
      DDAIL=1
      DDADM=1
      DDADF=2
      DO 201 INT1=1,DDAZ1
        DDAIC(INT1)=0
        DDABC(INT1)=0
  201 CONTINUE
      DDAKX=0
      DDAKY=0
      DO 202 INT1=1,DDAZ2
        DDAKS(INT1:INT1)=' '
  202 CONTINUE
      DDAKN=32
      DDAKF=0
      DO 203 INT1=1,DDAZ3
        DDABF(INT1)=0
  203 CONTINUE
      DDASL=(2**30)/128
      DDAST=25
      DDASN=32
      DDABE(1,1)=0
      DDABE(2,1)=0
      DDABE(1,2)=0
      DDABE(2,2)=0
      DDADS(1,1)=0.0
      DDADS(2,1)=0.0
      DDADS(1,2)=1.0
      DDADS(2,2)=1.0
C
C  ENTER DEVICE-DEPENDENT CODE.
      DDIN(1)=1
      CALL UGZ006(DDAAC,0,0,DDIN,OPTN,DDEX)
      IF (DDEX(1).LT.0) THEN
         WRITE(6,98)
  98     FORMAT(' DDEX FROM UGZ006 LT 0 ')
         print *, DDEX(1)
         GO TO 505
      END IF
      IF (DDEX(1).NE.0) GO TO 506
C
C  FINISH INITIALIZATION OF THE DEVICE DEPENDENT AREA.
      DDAAI=INDX
      FLT1=DDABX*REAL(DDABD(1,2)-DDABD(1,1))
      FLT2=DDABY*REAL(DDABD(2,2)-DDABD(2,1))
      IF (FLT2.LE.FLT1) THEN
        INT1=NINT((0.5*(FLT1-FLT2)/DDABX))
        DDADD(1,1)=DDABD(1,1)+INT1
        DDADD(2,1)=DDABD(2,1)
        DDADD(1,2)=DDABD(1,2)-INT1
        DDADD(2,2)=DDABD(2,2)
      ELSE
        INT1=NINT((0.5*(FLT2-FLT1)/DDABY))
        DDADD(1,1)=DDABD(1,1)
        DDADD(2,1)=DDABD(2,1)+INT1
        DDADD(1,2)=DDABD(1,2)
        DDADD(2,2)=DDABD(2,2)-INT1
      END IF
      DDADX=DDABX*REAL(DDADD(1,2)-DDADD(1,1))
      DDADY=DDADX
      DDADA=1.0
      DO 302 INT1=1,2
        DO 301 INT2=1,2
          DDAWA(INT1,INT2)=DDADS(INT1,INT2)
          DDAWS(INT1,INT2)=DDADS(INT1,INT2)
          DDAWD(INT1,INT2)=DDADD(INT1,INT2)
          DDA3V(INT1,INT2)=DDADS(INT1,INT2)
  301   CONTINUE
  302 CONTINUE
      DDAWX=DDADX
      DDAWY=DDADY
      CALL UGB005
      DDA3W(1,1)=-4.5
      DDA3W(2,1)=-4.5
      DDA3W(3,1)=-4.5
      DDA3W(1,2)=5.5
      DDA3W(2,2)=5.5
      DDA3W(3,2)=5.5
      CALL UGB010
      CALL UGB011(INT1)
      DDAFW=0
C
C  SAVE DEVICE POINTERS IN THE MAIN COMMUNICATION AREA.
      MCAOI(INDX)=IDNT
      MCAOP(INDX)=0
C
C  CLEAR THE NEWLY ACTIVE DEVICE.
      DDIN(1)=3
      DDIN(2)=0
      CALL UGZ006(DDAAC,0,0,DDIN,'        ',DDEX)
C
C  SEND THE PROJECTION PARAMETERS TO A THREE-DIMENSIONAL DEVICE.
      IF (DDADF.EQ.3) THEN
        DDIN(1)=15
        DDIN(2)=1
        CALL UGZ006(DDAAC,0,0,DDIN,'        ',DDEX)
      END IF
C
C  CHECK FOR MULTIPLE FULLY INTERACTIVE GRAPHIC DEVICES.
      IF (DDAIL.EQ.3) THEN
        MCAIC=MCAIC+1
        IF (MCAIC.GT.1) GO TO 507
      END IF
C
C  RESET ERROR INDICATORS AND RETURN TO CALLER.
      UGELV=0
      UGENM='        '
      UGEIX=0
  401 RETURN
C
C  REPORT ERRORS TO THE UNIFIED GRAPHICS SYSTEM ERROR PROCESSOR.
  501 CALL UGRERR(3,'UGOPEN  ', 1)
      GO TO 401
  502 CALL UGRERR(3,'UGOPEN  ', 2)
      GO TO 401
  503 CALL UGRERR(3,'UGOPEN  ', 3)
      GO TO 401
  504 CALL UGRERR(3,'UGOPEN  ', 4)
      GO TO 401
  505 CALL UGRERR(3,'UGOPEN  ', 5)
      GO TO 401
  506 CALL UGRERR(3,'UGOPEN  ', 6)
      GO TO 401
  507 CALL UGRERR(2,'UGOPEN  ', 7)
      GO TO 401
C
      END
