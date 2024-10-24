      SUBROUTINE UGXC01(DDIN,DDST,DDEX)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                    DEVICE-DEPENDENT MODULE FOR                    *
C *                   TEKTRONIX 4010/4014 EMULATORS                   *
C *                   IN THE FULLY INTERACTIVE MODE                   *
C *                                                                   *
C *  THIS SUBROUTINE IS A DEVICE-DEPENDENT MODULE FOR A FULLY         *
C *  INTERACTIVE DISPLAY GRAPHIC DEVICE.                              *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGXC01(DDIN,DDST,DDEX)                                    *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    DDIN  AN INTEGER INPUT ARRAY.                                  *
C *    DDST  A CHARACTER STRING FOR INPUT AND OUTPUT.                 *
C *    DDEX  AN INTEGER OUTPUT ARRAY.                                 *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      INTEGER       DDIN(*)
      CHARACTER*(*) DDST
      INTEGER       DDEX(*)
C
      INCLUDE       'UGSYSTEM:UGDDACBK.FOR/LIST'
C
      INCLUDE       'UGSYSTEM:UGDDXTXC.FOR/LIST'
C
      INTEGER*4     INST(149)
      INTEGER*4     EXST(161)
      INTEGER*4     EXXN,EXXX,EXYN,EXYX
      REAL*4        EXXZ,EXYZ
      INTEGER*4     EXLR,EXNO
      INTEGER*4     EXGS
      INTEGER*4     EXC1,EXC2,EXC3,EXC4,EXNC
      CHARACTER*64  EXCH
      INTEGER*4     EXNL
      CHARACTER*64  EXBP,EXEP
      CHARACTER*64  EXBR,EXER
      CHARACTER*64  EXCL
      CHARACTER*4   EXIL
      INTEGER*4     EXNI
      CHARACTER*64  EXBE
      CHARACTER*64  EXBX,EXEX
      EQUIVALENCE   (EXXN,EXST(  1)),     (EXXX,EXST(  2)),
     X              (EXYN,EXST(  3)),     (EXYX,EXST(  4)),
     X              (EXXZ,EXST(  5)),     (EXYZ,EXST(  6)),
     X              (EXLR,EXST(  7)),     (EXNO,EXST(  8)),
     X              (EXGS,EXST(  9)),
     X              (EXC1,EXST( 10)),     (EXC2,EXST( 11)),
     X              (EXC3,EXST( 12)),     (EXC4,EXST( 13)),
     X              (EXNC,EXST( 14)),
     X              (EXCH,EXST( 15)),
     X              (EXNL,EXST( 31)),
     X              (EXBP,EXST( 32)),     (EXEP,EXST( 48)),
     X              (EXBR,EXST( 64)),     (EXER,EXST( 80)),
     X              (EXCL,EXST( 96)),     (EXIL,EXST(112)),
     X              (EXNI,EXST(113)),
     X              (EXBE,EXST(114)),
     X              (EXBX,EXST(130)),     (EXEX,EXST(146))
C
      INTEGER       CSIZ
      CHARACTER*1   LNFG(4),CHFG(4)
      INTEGER       CFLG
      CHARACTER*1   ESCP,EVCM,EALM
C
      INTEGER       INT1
C
      DATA          INST/26,2,4,  1, 0,'XMIN',
     X                      2,4,  2, 0,'XMAX',
     X                      2,4,  3, 0,'YMIN',
     X                      2,4,  4, 0,'YMAX',
     X                      3,4,  5, 0,'XSIZ',
     X                      3,4,  6, 0,'YSIZ',
     X                      1,5,  7, 1,'LORE','S   ',
     X                      1,5,  8, 1,'NOOP','T   ',
     X                      1,5,  9, 1,'GENL','S   ',
     X                      2,5, 10, 0,'CSIZ','1   ',
     X                      2,5, 11, 0,'CSIZ','2   ',
     X                      2,5, 12, 0,'CSIZ','3   ',
     X                      2,5, 13, 0,'CSIZ','4   ',
     X                      2,5, 14, 0,'NCSI','Z   ',
     X                      4,7, 15,64,'CHAN','NEL ',
     X                      1,5, 31, 1,'NOLO','C   ',
     X                      4,6, 32,64,'BEGP','GM  ',
     X                      4,6, 48,64,'ENDP','GM  ',
     X                      4,6, 64,64,'BEGR','EC  ',
     X                      4,6, 80,64,'ENDR','EC  ',
     X                      4,5, 96,64,'CLEA','R   ',
     X                      4,4,112, 4,'IDLE',
     X                      2,5,113, 0,'NIDL','E   ',
     X                      4,4,114,64,'BELL',
     X                      4,6,130,64,'BEGL','OC  ',
     X                      4,6,146,64,'ENDL','OC  '/
      DATA          LNFG/'`','d','a','b'/
      DATA          CHFG/'8','9',':',';'/
      DATA          ESCP/Z1B/
      DATA          EVCM/Z1D/
      DATA          EALM/Z1F/
C
C  CHECK OPERATION FLAG AND BRANCH TO THE CORRECT SECTION.
      INT1=DDIN(1)
      IF ((INT1.LT.1).OR.(INT1.GT.14)) GO TO 902
      GO TO (101,151,201,251,301,351,401,451,501,551,
     X       601,651,701,751),INT1
C
C  OPERATION 1: OPEN THE GRAPHIC DEVICE.
  101 EXXN=0
      EXXX=DDXZ2
      EXYN=0
      EXYX=DDXZ3
      EXXZ=24.0
      EXYZ=18.0
      EXLR=0
      EXNO=0
      EXGS=0
      EXC1=56
      EXC2=51
      EXC3=34
      EXC4=31
      EXNC=4
      EXCH='TT'
      EXNL=0
      EXBP='  '
      EXEP='  '
      EXBR='  '
      EXER='  '
      EXCL='1D1B0C1F'
      EXIL='1F'
      EXNI=0
      EXBE='1D070707070707070707070707070707071F'
      EXBX='1D1B1A'
      EXEX='1F'
      CALL UGOPTN(DDST,INST,EXST)
      DDALX=DDXZZ
      CALL UGZ005(DDXRY,DDACX)
      DDAAT='TEKEMUL '
      DDAIL=3
      DDAIC(1)=1
      IF (EXNL.EQ.0) DDAIC(5)=1
      DDABD(1,1)=MAX(EXXN,0)
      DDABD(1,2)=MIN(EXXX,DDXZ2)
      DDABD(2,1)=MAX(EXYN,0)
      DDABD(2,2)=MIN(EXYX,DDXZ3)
      IF (DDABD(1,1).GE.DDABD(1,2)) GO TO 902
      IF (DDABD(2,1).GE.DDABD(2,2)) GO TO 902
      DDABX=EXXZ/REAL(DDXZ2)
      DDABY=EXYZ/REAL(DDXZ3)
      DDXID='DDA/XC00'
      DDXLR=EXLR
      DDXNO=EXNO
      DDXGS=EXGS
      DDXCZ(1)=EXC1
      DDXCZ(2)=EXC2
      DDXCZ(3)=EXC3
      DDXCZ(4)=EXC4
      DDXNC=MAX(0,MIN(4,EXNC))
      CALL UGXC02(EXBP,DDXBP,DDXN1)
      CALL UGXC02(EXEP,DDXEP,DDXN2)
      CALL UGXC02(EXBR,DDXBR,DDXN3)
      CALL UGXC02(EXER,DDXER,DDXN4)
      CALL UGXC02(EXCL,DDXCL,DDXN5)
      CALL UGXC02(EXIL(1:2),DDXIL,INT1)
      IF (INT1.EQ.0) DDXIL=CHAR(0)
      DDXNI=MIN(1024,EXNI)
      CALL UGXC02(EXBE,DDXBE,DDXN6)
      CALL UGXC02(EXBX,DDXBX,DDXN7)
      CALL UGXC02(EXEX,DDXEX,DDXN8)
      DDXBM=DDXZ1-1-DDXN4
      IF ((EXNL.EQ.0).AND.(DDXN7.LE.0)) GO TO 902
C    OPEN A PATH TO THE DEVICE.
      CALL UGXC07(EXCH,DDXIO,INT1)
      IF (INT1.NE.0) GO TO 902
C    INITIALIZE THE DEVICE.
      IF (DDXN1.GT.0) THEN
        DDXBN=0
        CALL UGXC05(0)
        CALL UGXC03(1,DDXBP(1:DDXN1))
        CALL UGXC05(0)
      END IF
      GO TO 901
C
C  OPERATION 2: CLOSE THE GRAPHIC DEVICE.
  151 IF (DDXN2.GT.0) THEN
        DDXBN=0
        CALL UGXC05(0)
        CALL UGXC03(1,DDXEP(1:DDXN2))
        CALL UGXC05(0)
      END IF
      CALL UGXC08(DDXIO)
      GO TO 901
C
C  OPERATION 3: CLEAR THE SCREEN ON THE GRAPHIC DEVICE.
  201 IF (DDIN(2).NE.0) GO TO 902
C    CLEAR THE SCREEN.
      DDXBN=0
      CALL UGXC05(0)
      IF (DDXN5.GT.0) CALL UGXC03(1,DDXCL(1:DDXN5))
      DO 202 INT1=1,DDXNI
        CALL UGXC03(1,DDXIL)
  202 CONTINUE
      CALL UGXC05(0)
      DDXLT=-9999
      DDXTZ=-9999
      GO TO 901
C
C  OPERATION 4: MANIPULATE THE SCREEN ON THE GRAPHIC DEVICE.
  251 GO TO 901
C
C  OPERATION 5: BEGIN A NEW GRAPHIC SEGMENT.
  301 DDXBN=0
      CALL UGXC05(0)
      CALL UGXC03(1,EVCM)
      DDXFF=.TRUE.
      DDEX(2)=0
      GO TO 901
C
C  OPERATION 6: TERMINATE A GRAPHIC SEGMENT.
  351 IF ((DDXBM-DDXBN).LT.8) THEN
        CALL UGXC05(1)
        CALL UGXC03(1,EVCM)
      END IF
      IF (DDXNC.GT.1) THEN
        IF (DDXTZ.NE.1) THEN
          CALL UGXC03(1,ESCP)
          CALL UGXC03(0,CHFG(1))
          DDXTZ=1
        END IF
      END IF
      CALL UGXC03(1,EVCM)
      CALL UGXC04(0,DDXZ3-DDXCZ(1))
      CALL UGXC05(1)
      GO TO 901
C
C  OPERATION 7: MANIPULATE A GRAPHIC SEGMENT.
  401 GO TO 902
C
C  OPERATION 8: INQUIRE ABOUT A GRAPHIC PRIMITIVE.
  451 INT1=DDIN(2)
      IF ((INT1.LT.1).OR.(INT1.GT.5)) GO TO 902
      GO TO (456,461,466,471,476),INT1
C    INQUIRE ABOUT POINTS.
  456 IF (DDIN(3).LT.3) THEN
        DDXPI=0
      ELSE
        DDXPI=1
      END IF
      IF (DDXGS.EQ.0) THEN
        IF (DDXLT.NE.1) THEN
          IF ((DDXBM-DDXBN).LT.2) THEN
            CALL UGXC05(1)
            CALL UGXC03(1,EVCM)
          END IF
          CALL UGXC03(1,ESCP)
          CALL UGXC03(0,LNFG(1))
          DDXLT=1
        END IF
      END IF
      GO TO 901
C    INQUIRE ABOUT LINES.
  461 IF (DDXGS.EQ.0) THEN
        IF (DDXLT.NE.DDIN(7)) THEN
          IF ((DDXBM-DDXBN).LT.2) THEN
            CALL UGXC05(1)
            CALL UGXC03(1,EVCM)
          END IF
          CALL UGXC03(1,ESCP)
          CALL UGXC03(0,LNFG(DDIN(7)))
          DDXLT=DDIN(7)
        END IF
      ELSE
        IF (DDIN(7).NE.1) THEN
          IF (DDIN(3).LT.3) THEN
            DDXPI=0
          ELSE
            DDXPI=1
          END IF
          GO TO 902
        END IF
      END IF
      GO TO 901
C    INQUIRE ABOUT TEXT.
  466 IF ((DDIN(8).GE.5).AND.(DDIN(8).LE.355)) GO TO 902
      IF (DDXNC.LE.0) GO TO 902
      IF (DDIN(9).NE.2) THEN
        IF (DDIN(7).GT.(3*DDXCZ(1)/2)) GO TO 902
        IF (DDIN(7).LT.(DDXCZ(DDXNC)/2)) GO TO 902
      END IF
      CSIZ=1
      DO 467 INT1=2,DDXNC
        IF (DDIN(7).LT.((DDXCZ(INT1-1)+DDXCZ(INT1))/2)) CSIZ=INT1
  467 CONTINUE
      IF (DDXNC.GT.1) THEN
        IF (DDXTZ.NE.CSIZ) THEN
          IF ((DDXBM-DDXBN).LT.2) THEN
            CALL UGXC05(1)
            CALL UGXC03(1,EVCM)
          END IF
          CALL UGXC03(1,ESCP)
          CALL UGXC03(0,CHFG(CSIZ))
          DDXTZ=CSIZ
        END IF
      END IF
      DDEX(2)=-5*DDXCZ(CSIZ)/14
      DDEX(3)=-DDXCZ(CSIZ)/2
      DDEX(4)=DDXCZ(CSIZ)
      DDEX(5)=0
      GO TO 901
C    INQUIRE ABOUT POLYGON-FILL.
  471 GO TO 902
C    INQUIRE ABOUT DEVICE-DEPENDENT DATA.
  476 DDXLT=-9999
      DDXTZ=-9999
      GO TO 901
C
C  OPERATION 9: DISPLAY A GRAPHIC PRIMITIVE.
  501 INT1=DDIN(2)
      IF ((INT1.LT.1).OR.(INT1.GT.5)) GO TO 902
      GO TO (506,511,516,521,526),INT1
C    DISPLAY POINTS.
  506 IF ((DDXBM-DDXBN).LT.21) CALL UGXC05(1)
      CALL UGXC03(1,EVCM)
      CALL UGXC04(DDIN(3),DDIN(4))
      IF (DDXPI.EQ.0) THEN
        CALL UGXC04(DDIN(3),DDIN(4))
      ELSE
        CALL UGXC04(    DDIN(3),
     X              MIN(DDIN(4)+4,DDABD(2,2)))
        CALL UGXC04(MIN(DDIN(3)+4,DDABD(1,2)),
     X              MIN(DDIN(4)+4,DDABD(2,2)))
        CALL UGXC04(MIN(DDIN(3)+4,DDABD(1,2)),
     X                  DDIN(4))
      END IF
      GO TO 901
C    DISPLAY LINES.
  511 IF (DDIN(5).EQ.0) THEN
        IF ((DDXBM-DDXBN).LT.11) CALL UGXC05(1)
        CALL UGXC03(1,EVCM)
      ELSE
        IF ((DDXBM-DDXBN).LT.5) THEN
          CALL UGXC05(1)
          CALL UGXC03(1,EVCM)
          IF (DDXLR.EQ.0) THEN
            CALL UGXC03(1,DDXOP)
          ELSE
            CALL UGXC03(1,DDXOP(1:1))
            CALL UGXC03(1,DDXOP(3:5))
          END IF
        END IF
      ENDIF
      CALL UGXC04(DDIN(3),DDIN(4))
      GO TO 901
C    DISPLAY TEXT.
  516 CALL UGXC06(2,DDST)
      IF ((DDXBM-DDXBN).LT.(LEN(DDST)+7)) CALL UGXC05(1)
      CALL UGXC03(1,EVCM)
      DDXFF=.TRUE.
      CALL UGXC04(DDIN(3),DDIN(4))
      CALL UGXC03(1,EALM)
      CALL UGXC03(0,DDST)
      DDXFF=.TRUE.
      GO TO 901
C    DISPLAY POLYGON-FILL.
  521 GO TO 902
C    DISPLAY DEVICE-DEPENDENT DATA.
  526 IF (LEN(DDST).LT.9) GO TO 901
      IF (DDST(1:8).NE.'TEKEMUL:') GO TO 901
      IF ((DDXBM-DDXBN).LT.(LEN(DDST)-8+6)) THEN
        CALL UGXC05(1)
        IF ((DDXBM-DDXBN).LT.(LEN(DDST)-8+6)) GO TO 901
      END IF
      CALL UGXC03(1,EVCM)
      DDXFF=.TRUE.
      CALL UGXC04(DDIN(3),DDIN(4))
      CALL UGXC03(1,DDST(9:))
      DDXFF=.TRUE.
      GO TO 901
C
C  OPERATION 10: PROCESS MISCELLANEOUS CONTROL FUNCTIONS.
  551 IF (DDIN(2).NE.1) GO TO 902
      IF (DDXN6.GT.0) THEN
        DDXBN=0
        CALL UGXC05(0)
        CALL UGXC03(1,DDXBE(1:DDXN6))
        CALL UGXC05(0)
      END IF
      GO TO 901
C
C  OPERATION 11: MODIFY THE STATUS OF A CONTROL.
  601 IF (DDIN(2).EQ.1) THEN
        DDAKX=MIN(DDXZ2,MAX(0,DDAKX))
        DDAKY=MIN(DDXZ3,MAX(0,DDAKY))
        CALL UGXC06(2,DDAKS(1:DDAKN))
      END IF
      GO TO 901
C
C  OPERATION 12: ENABLE OR DISABLE A CONTROL.
  651 GO TO 901
C
C  OPERATION 13: OBTAIN AN EVENT FROM THE GRAPHIC DEVICE.
C    INITIALIZE FOR THE KEYBOARD.
  701 DDXBN=0
      CALL UGXC05(0)
      IF (DDXNC.GT.1) THEN
        CALL UGXC03(1,EVCM)
        CALL UGXC03(1,ESCP)
        CALL UGXC03(0,CHFG(1))
      END IF
      CALL UGXC03(1,EVCM)
      CALL UGXC04(MAX(0,DDAKX-5*DDXCZ(1)/14),
     X            MAX(0,DDAKY-DDXCZ(1)/2))
      CALL UGXC05(1)
C    READ AND PROCESS THE RESPONSE FROM THE TERMINAL.
      CALL UGXC10(DDXIO,DDIN(2),DDXBF,DDXBN)
      IF (DDXBN.EQ.-1) GO TO 901
      IF (DDABC(1).EQ.0) GO TO 701
      INT1=MIN(DDAZ2,DDXBN)
      DDEX(1)=1
      DDEX(2)=INT1
      IF (INT1.GT.0) THEN
        DDST(1:INT1)=DDXBF(1:INT1)
        IF (DDAKF.EQ.0) CALL UGXC06(3,DDST(1:INT1))
      END IF
      GO TO 903
C
C  OPERATION 14: SAMPLE AN INTERACTIVE CONTROL.
  751 IF (DDIN(2).NE.5) GO TO 901
C    INITIALIZE FOR THE LOCATOR.
  752 DDXBN=0
      IF (DDXN3.GT.0) CALL UGXC03(1,DDXBR(1:DDXN3))
      CALL UGXC03(1,DDXBX(1:DDXN7))
      CALL UGXC06(0,DDXBF(1:DDXBN))
      CALL UGXC09(DDXIO,DDXBF,DDXBN)
C    READ AND PROCESS THE RESPONSE FROM THE TERMINAL.
      CALL UGXC10(DDXIO,-1,DDXBF,DDXBN)
      IF (DDXBN.GE.5) THEN
        DDEX(1)=5
        DDEX(2)=128*MOD(ICHAR(DDXBF(DDXBN-3:DDXBN-3)),32)+
     X            4*MOD(ICHAR(DDXBF(DDXBN-2:DDXBN-2)),32)
        DDEX(3)=128*MOD(ICHAR(DDXBF(DDXBN-1:DDXBN-1)),32)+
     X            4*MOD(ICHAR(DDXBF(DDXBN  :DDXBN  )),32)
        CFLG=1
      ELSE
        CFLG=0
      END IF
C    TERMINATE THE OPERATION.
      IF ((DDXN4+DDXN8).GT.0) THEN
        DDXBN=0
        IF (DDXN8.GT.0) CALL UGXC03(1,DDXEX(1:DDXN8))
        IF (DDXN4.GT.0) CALL UGXC03(1,DDXER(1:DDXN4))
        CALL UGXC06(0,DDXBF(1:DDXBN))
        CALL UGXC09(DDXIO,DDXBF,DDXBN)
      END IF
      IF (CFLG.EQ.0) GO TO 752
      GO TO 903
C
C  SET ERROR INDICATOR AND RETURN TO CALLER.
  901 DDEX(1)=0
      GO TO 903
  902 DDEX(1)=1
  903 RETURN
C
      END
      SUBROUTINE UGXC02(HEXS,BITS,NBIT)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                                                                   *
C *  THIS SUBROUTINE IS USED TO CONVERT A CHARACTER STRING            *
C *  CONTAINING HEXADECIMAL CHARACTERS INTO THE EQUIVALENT BIT        *
C *  STRING.  ANY ERROR CAUSES A ZERO STRING LENGTH TO BE RETURNED.   *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGXC02(HEXS,BITS,NBIT)                                    *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    HEXS  THE GIVEN STRING CONTAINING THE HEXADECIMAL CHARACTERS.  *
C *    BITS  THE COMPUTED BIT STRING.                                 *
C *    NBIT  THE NUMBER OF CHARACTERS IN THE BIT STRING.              *
C *                                                                   *
C *********************************************************************
C
      CHARACTER*(*) HEXS,BITS
      INTEGER       NBIT
C
      CHARACTER*1   DIGT(0:15)
      INTEGER       VAL1,VAL2
C
      INTEGER       INT1,INT2
C
      DATA          DIGT/'0','1','2','3','4','5','6','7',
     X                   '8','9','A','B','C','D','E','F'/
C
C  CONVERT THE HEXADECIMAL STRING.
      NBIT=0
      DO 105 INT1=1,(LEN(HEXS)-1),2
        IF (HEXS(INT1:INT1).EQ.' ') GO TO 201
        DO 101 INT2=0,15
          IF (HEXS(INT1:INT1).EQ.DIGT(INT2)) THEN
            VAL1=INT2
            GO TO 102
          END IF
  101   CONTINUE
        GO TO 202
  102   DO 103 INT2=0,15
          IF (HEXS(INT1+1:INT1+1).EQ.DIGT(INT2)) THEN
            VAL2=INT2
            GO TO 104
          END IF
  103   CONTINUE
        GO TO 202
  104   NBIT=NBIT+1
        BITS(NBIT:NBIT)=CHAR(16*VAL1+VAL2)
  105 CONTINUE
C
C  RETURN TO CALLER.
  201 RETURN
  202 NBIT=0
      GO TO 201
C
      END
      SUBROUTINE UGXC03(FLAG,STRG)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                                                                   *
C *  THIS SUBROUTINE IS USED TO PACK A CHARACTER STRING INTO THE      *
C *  OUTPUT BUFFER.  THE STRING MAY BE SUPPLIED IN THE FORMAT OF THE  *
C *  COMPUTER OR THE TERMINAL.  THE CALLER MUST ASSURE THAT THERE IS  *
C *  ENOUGH ROOM IN THE OUTPUT BUFFER TO HOLD THE NEW DATA.           *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGXC03(FLAG,STRG)                                         *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    FLAG  THE FLAG WHICH SPECIFIES WHETHER THE STRING IS IN THE    *
C *          FORMAT OF THE COMPUTER OR TERMINAL (0 MEANS COMPUTER     *
C *          AND 1 MEANS TERMINAL).                                   *
C *    STRG  THE CHARACTER STRING.                                    *
C *                                                                   *
C *********************************************************************
C
      INTEGER       FLAG
      CHARACTER*(*) STRG
C
      INCLUDE       'UGSYSTEM:UGDDXTXC.FOR/NOLIST'
C
      INTEGER       NCHR
C
C  PACK THE CHARACTERS INTO THE OUTPUT BUFFER AND MAKE SURE THE
C  CHARACTERS ARE IN THE FORMAT OF THE TERMINAL.
      NCHR=LEN(STRG)
      DDXBF(DDXBN+1:DDXBN+NCHR)=STRG(1:NCHR)
      IF (FLAG.NE.1) CALL UGXC06(1,DDXBF(DDXBN+1:DDXBN+NCHR))
      DDXBN=DDXBN+NCHR
C
C  RETURN TO CALLER.
      RETURN
C
      END
      SUBROUTINE UGXC04(XCRD,YCRD)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                                                                   *
C *  THIS SUBROUTINE IS USED TO GENERATE X-Y POSITIONING ORDERS.      *
C *  THE ORDERS ARE ADDED TO THE OUTPUT BUFFER.  THE CALLER MUST      *
C *  ASSURE THAT THERE IS ENOUGH ROOM IN THE OUTPUT BUFFER TO HOLD    *
C *  THE NEW DATA.                                                    *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGXC04(XCRD,YCRD)                                         *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    XCRD  THE X COORDINATE OF THE NEW POSITION.                    *
C *    YCRD  THE Y COORDINATE OF THE NEW POSITION.                    *
C *                                                                   *
C *********************************************************************
C
      INTEGER       XCRD,YCRD
C
      INCLUDE       'UGSYSTEM:UGDDXTXC.FOR/NOLIST'
C
      CHARACTER*5   ORDR
C
C  COMPUTE THE NECESSARY CHARACTERS.
      ORDR(1:1)=CHAR((YCRD/128)+32)
      IF (DDXLR.EQ.0) ORDR(2:2)=CHAR(4*MOD(YCRD,4)+MOD(XCRD,4)+96)
      ORDR(3:3)=CHAR(MOD(YCRD/4,32)+96)
      ORDR(4:4)=CHAR((XCRD/128)+32)
      ORDR(5:5)=CHAR(MOD(XCRD/4,32)+64)
C
C  PACK THE NECESSARY CHARACTERS INTO THE OUTPUT BUFFER.
      IF (DDXFF.OR.(DDXNO.NE.0)) THEN
        IF (DDXLR.EQ.0) THEN
          DDXBF(DDXBN+1:DDXBN+5)=ORDR
          DDXBN=DDXBN+5
        ELSE
          DDXBF(DDXBN+1:DDXBN+1)=ORDR(1:1)
          DDXBF(DDXBN+2:DDXBN+4)=ORDR(3:5)
          DDXBN=DDXBN+4
        END IF
      ELSE
        IF (ORDR(1:1).NE.DDXOP(1:1)) THEN
          DDXBF(DDXBN+1:DDXBN+1)=ORDR(1:1)
          DDXBN=DDXBN+1
        END IF
        IF (DDXLR.EQ.0) THEN
          IF (ORDR(2:2).NE.DDXOP(2:2)) THEN
            DDXBF(DDXBN+1:DDXBN+1)=ORDR(2:2)
            DDXBN=DDXBN+1
          END IF
          IF (ORDR(2:4).NE.DDXOP(2:4)) THEN
            DDXBF(DDXBN+1:DDXBN+1)=ORDR(3:3)
            DDXBN=DDXBN+1
          END IF
        ELSE
          IF (ORDR(3:4).NE.DDXOP(3:4)) THEN
            DDXBF(DDXBN+1:DDXBN+1)=ORDR(3:3)
            DDXBN=DDXBN+1
          END IF
        END IF
        IF (ORDR(4:4).NE.DDXOP(4:4)) THEN
          DDXBF(DDXBN+1:DDXBN+1)=ORDR(4:4)
          DDXBN=DDXBN+1
        END IF
        DDXBF(DDXBN+1:DDXBN+1)=ORDR(5:5)
        DDXBN=DDXBN+1
      END IF
C
C  SAVE THE CURRENT POSITION AND RESET FIRST POINT FLAG.
      DDXOP=ORDR
      DDXFF=.FALSE.
C
C  RETURN TO CALLER.
      RETURN
C
      END
      SUBROUTINE UGXC05(FLAG)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                                                                   *
C *  THIS SUBROUTINE IS USED TO COMPLETE THE CURRENT RECORD, WRITE    *
C *  IT TO THE DISPLAY DEVICE, AND RE-INITIALIZE THE BUFFER.  THE     *
C *  CALLER MUST ASSURE THAT THERE IS ENOUGH ROOM IN THE OUTPUT       *
C *  BUFFER TO COMPLETE THE RECORD.                                   *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGXC05(FLAG)                                              *
C *                                                                   *
C *  THE PARAMETER IN THE CALLING SEQUENCE IS:                        *
C *    FLAG  EXIT PLOTTING MODE FLAG (0 MEANS DO NOT ADD ORDER TO     *
C *          EXIT PLOTTING MODE, AND 1 MEANS ADD THE ORDER).          *
C *                                                                   *
C *********************************************************************
C
      INTEGER       FLAG
C
      INCLUDE       'UGSYSTEM:UGDDXTXC.FOR/NOLIST'
C
      CHARACTER*1   EALM
C
      DATA          EALM/Z1F/
C
C  VERIFY THAT THE BUFFER CONTAINS DATA TO BE WRITTEN OUT.
      IF (DDXBN.LE.0) GO TO 101
C
C  COMPLETE THE RECORD BY ADDING THE OPTIONAL EXIT PLOTTING MODE
C  ORDER AND THE END OF RECORD ORDERS.
      IF (FLAG.NE.0) THEN
        CALL UGXC03(1,EALM)
      END IF
      IF (DDXN4.GT.0) CALL UGXC03(1,DDXER(1:DDXN4))
      CALL UGXC06(0,DDXBF(1:DDXBN))
C
C  WRITE THE RECORD TO THE DEVICE.
      CALL UGXC09(DDXIO,DDXBF,DDXBN)
C
C  RE-INITIALIZE THE BUFFER.
  101 DDXBN=0
      IF (DDXN3.GT.0) CALL UGXC03(1,DDXBR(1:DDXN3))
      DDXFF=.TRUE.
C
C  RETURN TO CALLER.
      RETURN
C
      END
      SUBROUTINE UGXC06(FLAG,STRG)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                                                                   *
C *  THIS SUBROUTINE IS USED TO TRANSLATE CHARACTER STRINGS FROM THE  *
C *  TERMINAL CHARACTER SET TO THE COMPUTER CHARACTER SET, FROM THE   *
C *  COMPUTER CHARACTER SET TO THE TERMINAL CHARACTER SET, FROM THE   *
C *  COMPUTER CHARACTER SET TO DISPLAYABLE CHARACTERS, OR TO UPPER    *
C *  CASE IN THE COMPUTER CHARACTER SET.                              *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGXC06(FLAG,STRG)                                         *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    FLAG  TRANSLATE FLAG (0 MEANS TERMINAL TO COMPUTER, 1 MEANS    *
C *          COMPUTER TO TERMINAL, 2 MEANS COMPUTER TO DISPLAYABLE    *
C *          CHARACTERS, AND 3 MEANS UPPER CASE).                     *
C *    STRG  THE CHARACTER STRING TO BE TRANSLATED.                   *
C *                                                                   *
C *********************************************************************
C
      INTEGER       FLAG
      CHARACTER*(*) STRG
C
      INTEGER       INT1,INT2
      CHARACTER*1   CHR1
C
C  TRANSLATE THE CHARACTER STRING AS NEEDED.
      IF (FLAG.EQ.2) THEN
        DO 101 INT1=1,LEN(STRG)
          CHR1=STRG(INT1:INT1)
          IF ((CHR1.LT.' ').OR.(CHR1.GT.'~')) STRG(INT1:INT1)='@'
  101   CONTINUE
      ELSE IF (FLAG.EQ.3) THEN
        DO 102 INT1=1,LEN(STRG)
          INT2=ICHAR(STRG(INT1:INT1))
          IF ((INT2.GE.ICHAR('a')).AND.(INT2.LE.ICHAR('z')))
     X      STRG(INT1:INT1)=CHAR(INT2-ICHAR('a')+ICHAR('A'))
  102   CONTINUE
      END IF
C
C  RETURN TO CALLER.
      RETURN
C
      END
      SUBROUTINE UGXC07(NAME,PIOD,FLAG)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                                                                   *
C *  THESE SUBROUTINES ARE THE ACTUAL INPUT/OUTPUT MODULES.  UGXC07   *
C *  IS THE OPEN MODULE, UGXC08 IS THE CLOSE MODULE, UGXC09 IS THE    *
C *  WRITE MODULE, AND UGXC10 IS THE READ MODULE.                     *
C *                                                                   *
C *  THE CALLING SEQUENCES ARE:                                       *
C *    CALL UGXC07(NAME,PIOD,FLAG)                                    *
C *    CALL UGXC08(PIOD)                                              *
C *    CALL UGXC09(PIOD,STRG,WCNT)                                    *
C *    CALL UGXC10(PIOD,TIME,STRG,RCNT)                               *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCES ARE:                     *
C *    NAME  THE NAME OF THE INPUT/OUTPUT CHANNEL.  THE CHARACTER     *
C *          STRING SHOULD BE 64 CHARACTERS LONG.                     *
C *    PIOD  THE CHANNEL IDENTIFICATION.  THIS ITEM IS GENERATED BY   *
C *          THE OPEN FUNCTION AND MUST BE SUPPLIED TO THE OTHER      *
C *          FUNCTIONS.                                               *
C *    FLAG  A SUCCESS FLAG (0 MEANS SUCCESS, 1 MEANS ERROR).         *
C *    STRG  THE INPUT/OUTPUT CHARACTER STRING.                       *
C *    WCNT  THE WRITE COUNT FOR THE CHARACTERS IN STRG.              *
C *    TIME  THE DELAY TIME IN HUNDREDTHS OF A SECOND.  A VALUE OF    *
C *          -1 INDICATES NO TIME-OUT IS REQUIRED.                    *
C *    RCNT  THE READ COUNT FOR THE CHARACTERS IN STRG.  A -1 VALUE   *
C *          INDICATES A TIME-OUT HAS OCCURRED.                       *
C *                                                                   *
C *********************************************************************
C
      CHARACTER*64  NAME
      INTEGER       PIOD,FLAG
      CHARACTER*(*) STRG
      INTEGER       TIME,WCNT,RCNT
C
      INCLUDE       'UGSYSTEM:UGDDXTKC.FOR/NOLIST'
C
      INCLUDE       'UGSYSTEM:UGIOPARM.FOR/LIST'
C
      EXTERNAL      UGXC11,UGXC12
      INTEGER       SYS$ASSIGN,SYS$DASSGN,
     X              SYS$BINTIM,SYS$SETIMR,SYS$CANTIM,
     X              SYS$QIO,SYS$QIOW,SYS$CANCEL
C
      CHARACTER*13  CTIM
      INTEGER*4     XTIM(2)
      INTEGER*2     IOSB(4)
C
      INTEGER       INT1,INT2
C
      DATA          CTIM/'0 00:XX:XX.XX'/
C
C  PERFORM THE OPEN FUNCTION.
      DO 101 INT1=1,64
        IF (NAME(INT1:INT1).EQ.' ') THEN
          INT2=INT1-1
          GO TO 102
        END IF
  101 CONTINUE
      INT2=64
  102 INT1=SYS$ASSIGN(NAME(1:INT2),PIOD,,)
      IF (.NOT.INT1) THEN
        FLAG=1
      ELSE
        FLAG=0
      END IF
      GO TO 301
C
C  PERFORM THE CLOSE FUNCTION.
      ENTRY UGXC08(PIOD)
      INT1=SYS$DASSGN(%VAL(PIOD))
      IF (.NOT.INT1) CALL UGZ001
      GO TO 301
C
C  PERFORM THE WRITE FUNCTION.
      ENTRY UGXC09(PIOD,STRG,WCNT)
      INT1=SYS$QIOW(,%VAL(PIOD),
     X  %VAL(IO$_WRITEVBLK+IO$M_NOFORMAT),,,,
     X  %REF(STRG),%VAL(WCNT),,%VAL('00000000'X),,)
      IF (.NOT.INT1) CALL UGZ001
      GO TO 301
C
C  PERFORM THE READ FUNCTION.
      ENTRY UGXC10(PIOD,TIME,STRG,RCNT)
      IF (TIME.GE.0) THEN
        INT1=MOD(TIME,6000)
        CALL UGCNVF(REAL(INT1)/100.0,2,CTIM(9:13),INT2)
        IF (CTIM(9:9).EQ.' ') CTIM(9:9)='0'
        INT1=(TIME-INT1)/6000
        CALL UGCNVF(REAL(INT1),0,CTIM(6:7),INT2)
        IF (CTIM(6:6).EQ.' ') CTIM(6:6)='0'
        INT1=SYS$BINTIM(CTIM,XTIM)
        IF (.NOT.INT1) CALL UGZ001
        DDXHT=0
        INT1=SYS$SETIMR(,XTIM,UGXC11,%VAL(99))
        IF (.NOT.INT1) CALL UGZ001
      END IF
      DDXHQ=0
      INT1=SYS$QIO(,%VAL(PIOD),
     X  %VAL(IO$_READVBLK),IOSB,UGXC12,,
     X  %REF(STRG),%VAL(LEN(STRG)),,,,)
      IF (.NOT.INT1) CALL UGZ001
  201 CALL SYS$HIBER()
      IF (DDXHQ.EQ.1) THEN
        RCNT=IOSB(2)
        IF (TIME.GE.0) THEN
          DDXHT=-1
          INT1=SYS$CANTIM(%VAL(99),)
          IF (.NOT.INT1) CALL UGZ001
        END IF
        GO TO 301
      ELSE IF (DDXHT.EQ.1) THEN
        DDXHQ=-1
        INT1=SYS$CANCEL(%VAL(PIOD))
        IF (.NOT.INT1) CALL UGZ001
        RCNT=-1
        GO TO 301
      END IF
      GO TO 201
C
C  RETURN TO CALLER.
  301 RETURN
C
      END
      SUBROUTINE UGXC11
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                                                                   *
C *  THIS SUBROUTINE IS ENTERED WHENEVER THE TIME INTERVAL            *
C *  ESTABLISHED IN UGXC10 EXPIRES.                                   *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGXC11                                                    *
C *                                                                   *
C *  THERE ARE NO PARAMETERS IN THE CALLING SEQUENCE.                 *
C *                                                                   *
C *********************************************************************
C
      INCLUDE       'UGSYSTEM:UGDDXTKC.FOR/NOLIST'
C
      IF (DDXHT.EQ.0) THEN
        DDXHT=1
        CALL SYS$WAKE(,)
      END IF
C
C  RETURN TO CALLER.
      RETURN
C
      END
      SUBROUTINE UGXC12
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                                                                   *
C *  THIS SUBROUTINE IS ENTERED WHENEVER KEYBOARD INPUT IS READY TO   *
C *  BE PROCESSED IN SUBROUTINE UGXC10.                               *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGXC12                                                    *
C *                                                                   *
C *  THERE ARE NO PARAMETERS IN THE CALLING SEQUENCE.                 *
C *                                                                   *
C *********************************************************************
C
      INCLUDE       'UGSYSTEM:UGDDXTKC.FOR/NOLIST'
C
      IF (DDXHQ.EQ.0) THEN
        DDXHQ=1
        CALL SYS$WAKE(,)
      END IF
C
C  RETURN TO CALLER.
      RETURN
C
      END
