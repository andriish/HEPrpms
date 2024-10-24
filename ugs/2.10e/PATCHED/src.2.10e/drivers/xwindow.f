      SUBROUTINE UGXI01(DDIN,DDST,DDEX)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                  DEVICE-DEPENDENT MODULE FOR THE                  *
C *          DEC-WINDOWS SYSTEM IN THE FULLY INTERACTIVE MODE         *
C *                                                                   *
C *  THIS SUBROUTINE IS A DEVICE-DEPENDENT MODULE FOR A FULLY         *
C *  INTERACTIVE DISPLAY GRAPHIC DEVICE.                              *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGXI01(DDIN,DDST,DDEX)                                    *
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

* 19940509	KREYMER
*     Porting to IRIX 5.5, using X/Motif calls in place of DECwindows

* 19940113    KREYMER
*     Changed DDIN(6/7/8 TO 7/8/9, for use with UGS 2.00

      INTEGER       DDIN(*)
      CHARACTER*(*) DDST
      INTEGER       DDEX(*)
C
      INCLUDE       'UGSYSTEM:UGDDACBK.FOR'
      INCLUDE       'UGSYSTEM:UGDDXXWI.FOR'

      EXTERNAL      UGXI05,UGXI06
C
*      CHARACTER*40  FNM1(DDXZ2)
*      CHARACTER*40  FNM2(DDXZ2)
*      INTEGER       FLN1(DDXZ2),FLN2(DDXZ2)
      INTEGER       CSIZ
*      CHARACTER*13  CTIM
*      INTEGER*4     XTIM(2)

      INTEGER       INT1,INT2

      INTEGER*4     EXST(39)
      INTEGER*4     EXXO,EXYO,EXXZ,EXYZ,EXSY,EXEX,EXCT
      EQUIVALENCE   (EXXO,EXST(1)),
     X              (EXYO,EXST(2)),
     X              (EXXZ,EXST(3)),
     X              (EXYZ,EXST(4)),
     X              (EXSY,EXST(5)),
     X              (EXEX,EXST(6)),
     X              (EXCT,EXST(7))

      CHARACTER*64  EXSTR(2)
      CHARACTER*64  EXTL,EXCH
      EQUIVALENCE   (EXTL,EXSTR(1)),
     X              (EXCH,EXSTR(2))

      INTEGER       DESC_NUM
      PARAMETER    (DESC_NUM = 9)
      INTEGER*4     INST(4,DESC_NUM)
      CHARACTER*16  IFLAG(DESC_NUM)

      LOGICAL       OPENED

*  Count draw operations for diagnostics
      INTEGER  NOP9

      DATA (INST(I,1),I=1,4) / 2,4, 1, 0 /, IFLAG(1) /'XORG'   /
      DATA (INST(I,2),I=1,4) / 2,4, 2, 0 /, IFLAG(2) /'YORG'   /
      DATA (INST(I,3),I=1,4) / 2,4, 3, 0 /, IFLAG(3) /'XSIZ'   /
      DATA (INST(I,4),I=1,4) / 2,4, 4, 0 /, IFLAG(4) /'YSIZ'   /
      DATA (INST(I,5),I=1,4) / 1,4, 5, 1 /, IFLAG(5) /'SYNC'   /
      DATA (INST(I,6),I=1,4) / 2,6, 6, 0 /, IFLAG(6) /'EXPOSE' /
      DATA (INST(I,7),I=1,4) / 1,5, 7, 1 /, IFLAG(7) /'GENCH'  /

      DATA (INST(I,8),I=1,4) / 4,5, 1, 64 /, IFLAG(8) /'TITLE'  /
      DATA (INST(I,9),I=1,4) / 4,7, 2, 64 /, IFLAG(9) /'CHANNEL'/

*      DATA          FNM1/'-DEC-TERMINAL-MEDIUM-R-NORMAL-',
*     X                   '-BITSTREAM-TERMINAL-MEDIUM-R-NORMAL-',
*     X                   '-DEC-TERMINAL-MEDIUM-R-NORMAL-',
*     X                   '-BITSTREAM-TERMINAL-MEDIUM-R-NORMAL-'/
*      DATA          FNM2/'-14-140-75-75-C-8-ISO8859-1',
*     X                   '-18-180-75-75-C-11-ISO8859-1',
*     X                   '-28-280-75-75-C-16-ISO8859-1',
*     X                   '-36-360-75-75-C-22-ISO8859-1'/
*      DATA          FLN1/30,36,30,36/
*      DATA          FLN2/27,28,28,28/
*      DATA          CTIM/'0 00:XX:XX.XX'/

      DATA     EXXZ,EXYZ/1000,750/

      DATA     NOP9 / 0 /

      DATA     OPENED /.FALSE./

************************************************************
*  CHECK OPERATION FLAG AND BRANCH TO THE CORRECT SECTION  *
************************************************************
      INT1=DDIN(1)

* * * * *
*      IF (INT1.EQ.9) THEN
*         NOP9 = NOP9 + 1
*      ELSE
*         WRITE (*,*) ' *x* OPERATION ',INT1,NOP9
*         NOP9 = 0
*      ENDIF
* * * * *

      IF ((INT1.LT.1).OR.(INT1.GT.14)) GO TO 902
      GO TO (101,151,201,251,301,351,401,451,501,551,
     X       601,651,701,751),INT1

******************************************
*  OPERATION 1: OPEN THE GRAPHIC DEVICE  *
******************************************
 101  EXXO =   -1
      EXYO =   -1
C     EXXZ = 1000
C     EXYZ =  750
      EXSY =    0
      EXEX =    2
      EXCT =    0
      EXTL = 'Unified Graphics System'
      EXCH = ' '
      CALL UGOPTION(DDST,DESC_NUM,INST,IFLAG,EXST,EXSTR)
      DDALX=DDXZZ
      CALL UGZ005(DDXRY,DDACX)
      DDAAT='XWINDOW '
      DDAIL=3  ! interactive device
      DDADM    =  2
      DDAIC(1) =  1
      DDAIC(3) = 36
      DDAIC(5) =  1

      IF (.NOT.OPENED) THEN
      CALL FXOPEN ( EXXZ , EXYZ )
      IF ( EXXZ .EQ. 0 ) THEN
           WRITE (*,*) ' X-server window cannot be opened '
           GOTO 902
      ENDIF
      OPENED=.TRUE.
      ENDIF

      DDABD(1,1) =    0   ! Min X pixel
      DDABD(1,2) = EXXZ   ! Max X pixel
      DDABD(2,1) =    0   ! Min Y pixel
      DDABD(2,2) = EXYZ   ! Max Y pixel

      DDXID='DDA/XI00'  !

      DDABX = 2.54 / 75.   ! cm/pixel
      DDABY = 2.54 / 75.   ! cm/pixel

*      IF (EXSY.NE.0) THEN
*        INT1=X$SYNCHRONIZE(DDXDS,1,INT2)
*      ENDIF
*      DDXEX=MIN(3,MAX(0,EXEX))
*      DDXCT=EXCT
*      IF (EXXO.EQ.-1) THEN
*        EXXO=(X$WIDTH_OF_SCREEN(DDXSC)-EXXZ)/2
*      ENDIF
*      IF (EXYO.EQ.-1) THEN
*        EXYO=(X$HEIGHT_OF_SCREEN(DDXSC)-EXYZ)/2
*      ENDIF
*      CALL X$DEFAULT_VISUAL_OF_SCREEN(DDXSC,XVIS)
*      INT1=X$M_CW_BACK_PIXEL
*      XSWA.X$L_SWDA_BACKGROUND_PIXEL=X$BLACK_PIXEL_OF_SCREEN(DDXSC)
*      DDXWD=X$CREATE_WINDOW(DDXDS,
*     X                      X$ROOT_WINDOW_OF_SCREEN(DDXSC),
*     X                      EXXO,EXYO,EXXZ,EXYZ,
*     X                      0,DDXDP,X$C_INPUT_OUTPUT,XVIS,INT1,XSWA)
*      INT1=X$M_GC_BACKGROUND
*      XGCV.X$L_GCVL_BACKGROUND=X$BLACK_PIXEL_OF_SCREEN(DDXSC)
*      DDXGC=X$CREATE_GC(DDXDS,DDXWD,INT1,XGCV)
*      IF ((XVIS.X$L_VISU_CLASS.EQ.X$C_PSEUDO_COLOR).OR.
*     X    (XVIS.X$L_VISU_CLASS.EQ.X$C_DIRECT_COLOR)) THEN
        DDXTY=1
*      ELSE
*        DDXTY=0
*      ENDIF
      DDXXZ=EXXZ
      DDXYZ=EXYZ
*      IF (DDXEX.GT.1) THEN
*        DDXPM=X$CREATE_PIXMAP(DDXDS,DDXWD,EXXZ,EXYZ,DDXDP)
*        CALL X$SELECT_ASYNC_INPUT(DDXDS,DDXWD,X$M_KEY_PRESS.OR.
*     X                                        X$M_BUTTON_PRESS.OR.
*     X                                        X$M_EXPOSURE,
*     X                                        UGXI05,0)
*        CALL X$SELECT_INPUT(DDXDS,DDXWD,X$M_KEY_PRESS.OR.
*     X                                  X$M_BUTTON_PRESS.OR.
*     X                                  X$M_EXPOSURE)
*      ELSE
*        CALL X$SELECT_ASYNC_INPUT(DDXDS,DDXWD,X$M_KEY_PRESS.OR.
*     X                                        X$M_BUTTON_PRESS,
*     X                                        UGXI05,0)
*      CALL X$SELECT_INPUT(DDXDS,DDXWD,X$M_KEY_PRESS.OR.
*     X                                  X$M_BUTTON_PRESS)
*       ENDIF
*      DO 104 INT1=64,1,-1
*        IF (EXTL(INT1:INT1).NE.' ') THEN
*          CALL X$STORE_NAME(DDXDS,DDXWD,EXTL(1:INT1))
*          GO TO 105
*        ENDIF
*  104 CONTINUE
*  105 IF (DDXCT.EQ.0) THEN
*        DO 106 INT1=1,DDXZ2
*          INT2=X$LOAD_QUERY_FONT(DDXDS,
*     X                           FNM1(INT1)(1:FLN1(INT1))//
*     X                           FNM2(INT1)(1:FLN2(INT1)),XFST)
*          DDXFI(INT1)=XFST.X$L_FSTR_FID
*          DDXFW(INT1)=XFST.X$R_FSTR_MAX_BOUNDS.X$W_CHAR_RBEARING-
*     X                XFST.X$R_FSTR_MIN_BOUNDS.X$W_CHAR_LBEARING
*          DDXFH(INT1)=XFST.X$R_FSTR_MAX_BOUNDS.X$W_CHAR_ASCENT
*  106   CONTINUE
*      ELSE
*        INT2=X$LOAD_QUERY_FONT(DDXDS,
*     X                         FNM1(DDXZ3)(1:FLN1(DDXZ3))//
*     X                         FNM2(DDXZ3)(1:FLN2(DDXZ3)),XFST)
*        DDXFI(DDXZ3)=XFST.X$L_FSTR_FID
*        DDXFW(DDXZ3)=XFST.X$R_FSTR_MAX_BOUNDS.X$W_CHAR_RBEARING-
*     X               XFST.X$R_FSTR_MIN_BOUNDS.X$W_CHAR_LBEARING
*        DDXFH(DDXZ3)=XFST.X$R_FSTR_MAX_BOUNDS.X$W_CHAR_ASCENT
*      ENDIF

*      CALL X$MAP_WINDOW(DDXDS,DDXWD)

      DDXDM=0
      DDXPN=0
      DDXPF=0
      DDXHF=0
      GO TO 901

*******************************************
*  OPERATION 2: CLOSE THE GRAPHIC DEVICE  *
*******************************************
  151 CONTINUE
C     CALL FXCLOSE
*=    write (*,*) ' closed device '
      GO TO 901
C
************************************************************
*  OPERATION 3: CLEAR THE SCREEN ON THE GRAPHIC DEVICE     *
************************************************************
  201 CALL UGXI02(3,8,0)
*      IF (DDIN(2).EQ.0) THEN
*=    write (*,*) ' cleared screen '

c     if (intrac(0).eq.0) then  ! stdin is redirected
c     CALL FXPAUSE
c     endif
      CALL FXCLEAR

      GO TO 901
C
************************************************************
* OPERATION 4: MANIPULATE THE SCREEN ON THE GRAPHIC DEVICE *
************************************************************
  251 GO TO 901
C
************************************************************
*  OPERATION 5: BEGIN A NEW GRAPHIC SEGMENT                *
************************************************************
  301 DDXDM=DDIN(4)
      DDXPN=0
      DDXPF=0
      DDEX(2)=0
*=    write (*,*) ' new segment '
      CALL FXRESET
      GO TO 901
C
************************************************************
*  OPERATION 6: TERMINATE A GRAPHIC SEGMENT                *
************************************************************
  351 CALL UGXI03

*=    write (*,*) ' end segment '
      DDXDM=0
      CALL FXDRAW
      GO TO 901
C
************************************************************
*  OPERATION 7: MANIPULATE A GRAPHIC SEGMENT               *
************************************************************
  401 CONTINUE
*=      write (*,*) ' manipulate segment '
      GO TO 902
C
************************************************************
*  OPERATION 8: INQUIRE ABOUT A GRAPHIC PRIMITIVE          *
************************************************************
  451 CALL UGXI03
      DDXPN=0
      INT1=DDIN(2)
      IF ((INT1.LT.1).OR.(INT1.GT.4)) GO TO 902
      GO TO (461,471,481,491),INT1
C    INQUIRE ABOUT POINTS.
  461 CALL UGXI02(0,DDIN(4),0)
      DDXPF=0
      GO TO 901
C    INQUIRE ABOUT LINES.
  471 CALL UGXI02(1,DDIN(4),DDIN(7))
      DDXPF=1
      GO TO 901
C    INQUIRE ABOUT TEXT.
  481 IF (DDXCT.NE.0) GO TO 902
C     IF ((DDIN(8).GE.5).AND.(DDIN(8).LE.355)) GO TO 902
C     IF (DDIN(9).NE.2) THEN
C       IF (DDIN(7).LT.(DDXFW(1)/2)) GO TO 902
C       IF (DDIN(7).GT.(3*DDXFW(DDXZ2)/2)) GO TO 902
C     ENDIF
C     CSIZ=1
C     DO 467 INT1=2,DDXZ2
C       IF (DDIN(7).GT.((DDXFW(INT1-1)+DDXFW(INT1))/2)) CSIZ=INT1
C 467 CONTINUE
      CALL UGXI02(2,DDIN(4),0)
C     CALL X$SET_FONT(DDXDS,DDXGC,DDXFI(CSIZ))
C     DDEX(2)=-DDXFW(CSIZ)/2
C     DDEX(3)=-DDXFH(CSIZ)/2
C     DDEX(4)=DDXFW(CSIZ)
      CALL FXSETAN(DDIN(8))
      CALL FXSETFN(DDIN(7),DDEX(3),DDEX(4))
      IF (DDEX(3).EQ.0.OR.DDEX(4).EQ.0) GOTO 902
      DDEX(2)=-NINT(DDEX(4)/2.*COS(DDIN(8)/57.2957795))
      DDEX(3)=-NINT(DDEX(4)/2.*SIN(DDIN(8)/57.2957795))
      DDEX(5)=0
      GO TO 901
C    INQUIRE ABOUT POLYGON FILL.
  491 CALL UGXI02(3,DDIN(4),0)
      GO TO 901
C
************************************************************
*  OPERATION 9: DISPLAY A GRAPHIC PRIMITIVE                *
************************************************************
  501 INT1=DDIN(2)

      IF ((INT1.LT.1).OR.(INT1.GT.4)) GO TO 902
      GO TO (511,521,531,541),INT1

C    DISPLAY POINTS.
  511 IF (DDXPN.EQ.DDXZ1) CALL UGXI03
      DDXPN=DDXPN+1
*      XPNT(DDXPN).X$W_GPNT_X = DDIN(3)
*      XPNT(DDXPN).X$W_GPNT_Y = DDXYZ-DDIN(4)
      XPNT(DDXPN) =       DDIN(3)
      YPNT(DDXPN) = DDXYZ-DDIN(4)
      GO TO 901

C    DISPLAY LINES.
  521 IF (DDXPN.EQ.DDXZ1) CALL UGXI03
      IF (DDIN(5).EQ.0) THEN
        CALL UGXI03
        DDXPN=0
      ENDIF
      DDXPN=DDXPN+1
*      XPNT(DDXPN).X$W_GPNT_X = DDIN(3)
*      XPNT(DDXPN).X$W_GPNT_Y = DDXYZ-DDIN(4)
      XPNT(DDXPN) =         DDIN(3)
      YPNT(DDXPN) = DDXYZ - DDIN(4)
      GO TO 901

C    DISPLAY TEXT.
C 531 CALL UGXI04(0,DDST)
  531 CALL FXTEXT(DDIN(3),DDXYZ-DDIN(4),DDST)
*      IF (DDXEX.LT.3) THEN
*        CALL X$DRAW_STRING(DDXDS,DDXWD,DDXGC,
*     X                     DDIN(3),DDXYZ-DDIN(4),DDST)
*      ENDIF
*      IF (DDXEX.GT.1) THEN
*        CALL X$DRAW_STRING(DDXDS,DDXPM,DDXGC,
*     X                     DDIN(3),DDXYZ-DDIN(4),DDST)
*      ENDIF
      GO TO 901

C    DISPLAY POLYGON FILL.
  541 DDXPN=0
      DO 542 INT1=1,DDIN(3)
        DDXPN=DDXPN+1
*        XPNT(DDXPN).X$W_GPNT_X = DDIN(2*INT1+2)
*        XPNT(DDXPN).X$W_GPNT_Y = DDXYZ-DDIN(2*INT1+3)
        XPNT(DDXPN) = DDIN(2*INT1+2)
        YPNT(DDXPN) = DDXYZ-DDIN(2*INT1+3)
  542 CONTINUE
*      IF (DDXEX.LT.3) THEN
*        CALL X$FILL_POLYGON(DDXDS,DDXWD,DDXGC,XPNT,DDXPN,
*     X                      X$C_POLYCOMPLEX,
*     X                      X$C_COORD_MODE_ORIGIN)
*      ENDIF
*      IF (DDXEX.GT.1) THEN
*        CALL X$FILL_POLYGON(DDXDS,DDXPM,DDXGC,XPNT,DDXPN,
*     X                      X$C_POLYCOMPLEX,
*     X                      X$C_COORD_MODE_ORIGIN)
*      ENDIF
      CALL FXPOLY(DDXPN,XPNT,YPNT)
      DDXPN=0
      GO TO 901
C
************************************************************
*  OPERATION 10: PROCESS MISCELLANEOUS CONTROL FUNCTIONS   *
************************************************************
  551 IF (DDIN(2).NE.1) GO TO 902
*=      write (*,*) ' control functions '

*      CALL X$BELL(DDXDS,100)
*      CALL X$FLUSH(DDXDS)
      CALL FXBELL
      GO TO 901
C
************************************************************
*  OPERATION 11: MODIFY THE STATUS OF A CONTROL            *
************************************************************
  601 GO TO 902
C
************************************************************
*  OPERATION 12: ENABLE OR DISABLE A CONTROL               *
************************************************************
  651 GO TO 901
C
************************************************************
*  OPERATION 13: OBTAIN AN EVENT FROM THE GRAPHIC DEVICE   *
************************************************************
  701 DDXKX=DDAKX-(DDXFW(DDXZ3)/2)
      IF (DDXKX.LT.0) DDXKX=0
      INT1=95*DDXXZ/100
      IF (DDXKX.GT.INT1) DDXKX=INT1
      DDXKY=DDAKY-(DDXFH(DDXZ3)/2)
      IF (DDXKY.LT.0) DDXKY=0
      INT1=95*DDXYZ/100
      IF (DDXKY.GT.INT1) DDXKY=INT1
*      CALL X$SET_FONT(DDXDS,DDXGC,DDXFI(DDXZ3))
  702 DDXKN=0
      DDXKO=0
      CALL UGXI02(2,1,0)
*      CALL X$DRAW_STRING(DDXDS,DDXWD,DDXGC,
*     X                   DDXKX,DDXYZ-DDXKY,'_')
*      IF (DDXEX.GT.1) THEN
*        CALL X$DRAW_STRING(DDXDS,DDXPM,DDXGC,
*     X                     DDXKX,DDXYZ-DDXKY,'_')
*      ENDIF
*      CALL X$FLUSH(DDXDS)
*      DDXHF=-1
*      IF (DDIN(2).GT.0) THEN
*        INT1=MOD(DDIN(2),6000)
*        CALL UGCNVF(REAL(INT1)/100.0,2,CTIM(9:13),INT2)
*        IF (CTIM(9:9).EQ.' ') CTIM(9:9)='0'
*        INT1=(DDIN(2)-INT1)/6000
*        CALL UGCNVF(REAL(INT1),0,CTIM(6:7),INT2)
*        IF (CTIM(6:6).EQ.' ') CTIM(6:6)='0'
*        INT1=SYS$BINTIM(CTIM,XTIM)
*        IF (.NOT.INT1) CALL UGZ001
*        INT1=SYS$SETIMR(,XTIM,UGXI06,%VAL(99))
*        IF (.NOT.INT1) CALL UGZ001
*      ENDIF
*      CALL SYS$HIBER()
*      IF ((DDIN(2).GT.0).AND.(DDXHF.NE.1)) THEN
*        INT1=SYS$CANTIM(%VAL(99),)
*        IF (.NOT.INT1) CALL UGZ001
*      ENDIF
*      CALL UGXI02(2,8,0)
*      CALL X$DRAW_STRING(DDXDS,DDXWD,DDXGC,
*     X                   DDXKX,DDXYZ-DDXKY,DDXKS(1:DDXKN)//'_')
*      IF (DDXEX.GT.1) THEN
*        CALL X$DRAW_STRING(DDXDS,DDXPM,DDXGC,
*     X                     DDXKX,DDXYZ-DDXKY,DDXKS(1:DDXKN)//'_')
*      ENDIF
*      CALL X$FLUSH(DDXDS)
*      IF (DDXHF.EQ.1) THEN
C    PROCESS A TIME-OUT.
*        DDXHF=0
*        GO TO 901
*      ELSE IF (DDXHF.EQ.2) THEN
*C    PROCESS A KEYBOARD EVENT.
*        DDXHF=0
*        IF (DDABC(1).EQ.0) GO TO 702
*        DDEX(1)=1
*        DDEX(2)=DDXKN
*        DDST(1:DDXKN)=DDXKS(1:DDXKN)
*        IF (DDAKF.EQ.0) CALL UGXI04(1,DDST(1:DDXKN))
*        GO TO 903
*      ELSE IF (DDXHF.EQ.3) THEN
*C    PROCESS A BUTTON EVENT.
*        DDXHF=0
*        IF (DDABC(3).EQ.0) GO TO 702
*        DDEX(1)=3
*        DDEX(2)=DDXKV
*        RETURN
*      ENDIF
*      DDXHF=0
      GO TO 702
C
************************************************************
*  OPERATION 14: SAMPLE AN INTERACTIVE CONTROL             *
************************************************************
  751 IF (DDIN(2).EQ.5) THEN
*        INT1=X$CREATE_FONT_CURSOR(DDXDS,X$C_X_CURSOR)
*        CALL X$DEFINE_CURSOR(DDXDS,DDXWD,INT1)
*        CALL X$FLUSH(DDXDS)
*  752   DDXHF=-2
*        CALL SYS$HIBER()
*        IF (DDXHF.NE.4) GO TO 752
*        DDXHF=0
*        CALL X$UNDEFINE_CURSOR(DDXDS,DDXWD)
*        CALL X$FLUSH(DDXDS)
         CALL FXCURSOR(DDEX(2),DDEX(3),DDEX(4))
         DDEX(1)=5
*        DDEX(2)=DDXPX
*        DDEX(3)=DDXPY
         GOTO 901
      ENDIF
      GO TO 902

C  Reset/Set error indicator and return to caller

  901 DDEX(1) = 0
      RETURN

  902 DDEX(1) = 1
      RETURN
C
      END
      SUBROUTINE UGXI02(TYPE,COLR,LTYP)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                                                                   *
C *  THIS SUBROUTINE IS USED TO SET UP THE GRAPHIC CONTEXT FOR A      *
C *  GRAPHIC PRIMITIVE.                                               *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGXI02(TYPE,COLR,LTYP)                                    *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    TYPE  THE TYPE OF THE PRIMITIVE.                               *
C *            0 MEANS POINTS,                                        *
C *            1 MEANS LINES,                                         *
C *            2 MEANS TEXT, AND                                      *
C *            3 MEANS POLYGON FILL.                                  *
C *    COLR  THE COLOR OF THE PRIMITIVE.                              *
C *    LTYP  THE LINE TYPE FOR A LINE PRIMITIVE.                      *
C *                                                                   *
C *********************************************************************
C
      INTEGER       TYPE,COLR,LTYP
C
      INCLUDE       'UGSYSTEM:UGDDXXWI.FOR'
C
      INTEGER       ACLR,ALTP
      BYTE          DASH(2),DOTS(2),DDSH(4)
C
      DATA          DASH/13,13/
      DATA          DOTS/1,9/
      DATA          DDSH/9,9,1,9/

C  SET THE COLOR IN THE GRAPHIC CONTEXT.
      IF (DDXDM.NE.0) THEN
        ACLR=8
      ELSE
        ACLR=COLR
      ENDIF
      IF (DDXTY.EQ.0) THEN
        IF (ACLR.NE.8) ACLR=1
      ENDIF

      CALL FXCOLOR ( ACLR )

C  SET THE LINE STRUCTURE IN THE GRAPHIC CONTEXT.
      IF (TYPE.NE.1) THEN
        ALTP=1
      ELSE
        ALTP=LTYP
      ENDIF
      CALL FXSETSTYLE ( 0 , ALTP )  ! Width and style
C
C  RETURN TO CALLER.
      RETURN
C
      END
      SUBROUTINE UGXI03
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                                                                   *
C *  THIS SUBROUTINE IS USED TO WRITE OUT THE ACCUMULATED CONTENTS    *
C *  OF THE POINT/LINE END POINT ARRAY.                               *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGXI03                                                    *
C *                                                                   *
C *  THERE ARE NO PARAMETERS IN THE CALLING SEQUENCE.                 *
C *                                                                   *
C *********************************************************************
C
      INCLUDE       'UGSYSTEM:UGDDXXWI.FOR'
      INTEGER I
C
C
C  WRITE OUT THE ACCUMULATED POINTS AND RE-INITIALIZE THE POINT
C  ARRAY.
      IF (DDXPF.EQ.0) THEN
C    WRITE OUT POINT DATA.
        IF (DDXPN.GT.0) THEN

*          IF (DDXEX.LT.3) THEN
*            CALL X$DRAW_POINTS(DDXDS,DDXWD,DDXGC,XPNT,DDXPN,
*     X                         X$C_COORD_MODE_ORIGIN)
*          ENDIF

*          IF (DDXEX.GT.1) THEN
*            CALL X$DRAW_POINTS(DDXDS,DDXPM,DDXGC,XPNT,DDXPN,
*     X                         X$C_COORD_MODE_ORIGIN)
*          ENDIF

        DO  I = 1 , DDXPN
           CALL FXPOINT ( XPNT(I),YPNT(I))
        ENDDO

        ENDIF
        DDXPN=0
      ELSE

C    Write out line data
*        IF (DDXPN.GT.1) THEN
*          IF (DDXEX.LT.3) THEN
*            CALL X$DRAW_LINES(DDXDS,DDXWD,DDXGC,XPNT,DDXPN,
*     X                        X$C_COORD_MODE_ORIGIN)
*          ENDIF
*          IF (DDXEX.GT.1) THEN
*            CALL X$DRAW_LINES(DDXDS,DDXPM,DDXGC,XPNT,DDXPN,
*     X                        X$C_COORD_MODE_ORIGIN)
*          ENDIF
*        ENDIF
        DO  I = 1 , DDXPN-1
           CALL FXLINE ( XPNT(I),YPNT(I),XPNT(I+1),YPNT(I+1))
        ENDDO

        XPNT(1) = XPNT(DDXZ1)
        YPNT(1) = YPNT(DDXZ1)
        DDXPN=1
      ENDIF
C
C  RETURN TO CALLING SUBROUTINE.
      RETURN
C
      END
      SUBROUTINE UGXI04(FLAG,STRG)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                                                                   *
C *  THIS SUBROUTINE IS USED TO TRANSLATE CHARACTER STRINGS FROM THE  *
C *  COMPUTER CHARACTER SET TO THE DISPLAY CHARACTER SET, OR TO       *
C *  UPPER CASE IN THE COMPUTER CHARACTER SET.                        *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGXI04(FLAG,STRG)                                         *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    FLAG  TRANSLATE FLAG (0 MEANS TERMINAL TO COMPUTER, 1 MEANS    *
C *          UPPER CASE).                                             *
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
      IF (FLAG.EQ.0) THEN
        DO 101 INT1=1,LEN(STRG)
          CHR1=STRG(INT1:INT1)
          IF ((CHR1.LT.' ').OR.(CHR1.GT.'~')) STRG(INT1:INT1)='@'
  101   CONTINUE
      ELSE IF (FLAG.EQ.1) THEN
        DO 102 INT1=1,LEN(STRG)
          INT2=ICHAR(STRG(INT1:INT1))
          IF ((INT2.GE.ICHAR('a')).AND.(INT2.LE.ICHAR('z')))
     X      STRG(INT1:INT1)=CHAR(INT2-ICHAR('a')+ICHAR('A'))
  102   CONTINUE
      ENDIF
C
C  RETURN TO CALLER.
      RETURN
C
      END
      SUBROUTINE UGXI05(ARGU)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                                                                   *
C *  THIS SUBROUTINE IS AN ASYNCHRONOUS SYSTEM TRAP (AST) THAT IS     *
C *  ENTERED WHENEVER A "KEY PRESS", "BUTTON PRESS" OR "EXPOSE"       *
C *  EVENT OCCURS.                                                    *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGXI05(ARGU)                                              *
C *                                                                   *
C *  THE PARAMETER IN THE CALLING SEQUENCE IS:                        *
C *    ARGU  THE ARGUMENT SUPPLIED TO THE SUBROUTINE.                 *
C *                                                                   *
C *********************************************************************
C
      INTEGER*4     ARGU
C
      INCLUDE       'UGSYSTEM:UGDDACBK.FOR'
      INCLUDE       'UGSYSTEM:UGDDXXWI.FOR'
C
*      RECORD        /X$EVENT/              XEVT
C
      INTEGER*4     CHIN
      CHARACTER*4   CHST
      INTEGER       BTVL
C
      INTEGER       INT1
C
C  PROCESS ANY WAITING EVENTS.
*  101 IF (X$PENDING(DDXDS).GT.0) THEN
C    OBTAIN THE NEXT AVAILABLE EVENT.
*        CALL X$NEXT_EVENT(DDXDS,XEVT)
*        IF (XEVT.EVNT_TYPE.EQ.X$C_KEY_PRESS) THEN
C    PROCESS A KEY PRESS EVENT.
*          IF (DDXHF.EQ.-1) THEN
*            INT1=X$LOOKUP_STRING(XEVT,CHST,4,CHIN,)
*            IF (CHIN.EQ.'0000FF0D'X) THEN
C      PROCESS A CARRIAGE RETURN.
*              DDXHF=2
*              CALL SYS$WAKE(,)
*            ELSE IF ((CHIN.LT.'0000007F'X).AND.
*     X               (CHIN.GT.'0000001F'X).AND.
*     X               (CHST(1:1).EQ.CHAR(CHIN))) THEN
C      PROCESS A PRINTABLE CHARACTER.
*              IF (DDXKN.GE.DDAKN) THEN
*                CALL X$BELL(DDXDS,100)
*              ELSE
*                CALL UGXI02(2,8,0)
*                CALL X$DRAW_STRING(DDXDS,DDXWD,DDXGC,
*     X                             DDXKX+DDXFW(DDXZ3)*DDXKN,
*     X                             DDXYZ-DDXKY,'_')
*                IF (DDXEX.GT.1) THEN
*                  CALL X$DRAW_STRING(DDXDS,DDXPM,DDXGC,
*     X                               DDXKX+DDXFW(DDXZ3)*DDXKN,
*     X                               DDXYZ-DDXKY,'_')
*                ENDIF
*                DDXKN=DDXKN+1
*                DDXKS(DDXKN:DDXKN)=CHST(1:1)
*                CALL UGXI02(2,1,0)
*                CALL X$DRAW_STRING(DDXDS,DDXWD,DDXGC,
*     X                             DDXKX+DDXFW(DDXZ3)*(DDXKN-1),
*     X                             DDXYZ-DDXKY,
*     X                             DDXKS(DDXKN:DDXKN)//'_')
*                IF (DDXEX.GT.1) THEN
*                  CALL X$DRAW_STRING(DDXDS,DDXPM,DDXGC,
*     X                               DDXKX+DDXFW(DDXZ3)*(DDXKN-1),
*     X                               DDXYZ-DDXKY,
*     X                               DDXKS(DDXKN:DDXKN)//'_')
*                ENDIF
*              ENDIF
*              CALL X$FLUSH(DDXDS)
*            ELSE IF (((CHST(1:1).EQ.CHAR('08'X)).AND.
*     X                (CHIN.EQ.'00000068'X)).OR.
*     X               ((CHST(1:1).EQ.CHAR('7F'X)).AND.
*     X                (CHIN.EQ.'0000FFFF'X))) THEN
C      PROCESS A BACKSPACE OR DELETE.
*              IF (DDXKN.EQ.0) THEN
*                CALL X$BELL(DDXDS,100)
*              ELSE
*                CALL UGXI02(2,8,0)
*                CALL X$DRAW_STRING(DDXDS,DDXWD,DDXGC,
*     X                             DDXKX+DDXFW(DDXZ3)*(DDXKN-1),
*     X                             DDXYZ-DDXKY,
*     X                             DDXKS(DDXKN:DDXKN)//'_')
*                IF (DDXEX.GT.1) THEN
*                  CALL X$DRAW_STRING(DDXDS,DDXPM,DDXGC,
*     X                               DDXKX+DDXFW(DDXZ3)*(DDXKN-1),
*     X                               DDXYZ-DDXKY,
*     X                               DDXKS(DDXKN:DDXKN)//'_')
*                ENDIF
*                DDXKN=DDXKN-1
*                CALL UGXI02(2,1,0)
*                CALL X$DRAW_STRING(DDXDS,DDXWD,DDXGC,
*     X                             DDXKX+DDXFW(DDXZ3)*DDXKN,
*     X                             DDXYZ-DDXKY,'_')
*                IF (DDXEX.GT.1) THEN
*                  CALL X$DRAW_STRING(DDXDS,DDXPM,DDXGC,
*     X                               DDXKX+DDXFW(DDXZ3)*DDXKN,
*     X                               DDXYZ-DDXKY,'_')
*                ENDIF
*              ENDIF
*              CALL X$FLUSH(DDXDS)
*            ELSE IF (CHIN.EQ.'0000FFB0'X) THEN
C      PROCESS A FUNCTION BUTTON.
*              DDXKO=12
*              GO TO 201
*            ELSE IF (CHIN.EQ.'0000FFAC'X) THEN
*              DDXKO=24
*              GO TO 201
*            ELSE IF (CHIN.EQ.'0000FF91'X) THEN
*              BTVL=1
*              GO TO 102
*            ELSE IF (CHIN.EQ.'0000FF92'X) THEN
*              BTVL=2
*              GO TO 102
*            ELSE IF (CHIN.EQ.'0000FF93'X) THEN
*              BTVL=3
*              GO TO 102
*            ELSE IF (CHIN.EQ.'0000FFB7'X) THEN
*              BTVL=4
*              GO TO 102
*            ELSE IF (CHIN.EQ.'0000FFB8'X) THEN
*              BTVL=5
*              GO TO 102
*            ELSE IF (CHIN.EQ.'0000FFB9'X) THEN
*              BTVL=6
*              GO TO 102
*            ELSE IF (CHIN.EQ.'0000FFB4'X) THEN
*              BTVL=7
*              GO TO 102
*            ELSE IF (CHIN.EQ.'0000FFB5'X) THEN
*              BTVL=8
*              GO TO 102
*            ELSE IF (CHIN.EQ.'0000FFB6'X) THEN
*              BTVL=9
*              GO TO 102
*            ELSE IF (CHIN.EQ.'0000FFB1'X) THEN
*              BTVL=10
*              GO TO 102
*            ELSE IF (CHIN.EQ.'0000FFB2'X) THEN
*              BTVL=11
*              GO TO 102
*            ELSE IF (CHIN.EQ.'0000FFB3'X) THEN
*              BTVL=12
*  102         DDXKV=DDXKO+BTVL
*              DDXHF=3
*              CALL SYS$WAKE(,)
*            ENDIF
*          ENDIF
*        ELSE IF (XEVT.EVNT_TYPE.EQ.X$C_BUTTON_PRESS) THEN
C    PROCESS A BUTTON PRESS EVENT.
*          IF (DDXHF.EQ.-2) THEN
*            DDXPX=XEVT.EVNT_BUTTON.X$L_BTEV_X
*            DDXPY=DDXYZ-XEVT.EVNT_BUTTON.X$L_BTEV_Y
*            DDXHF=4
*            CALL SYS$WAKE(,)
*          ENDIF
*        ELSE IF (XEVT.EVNT_TYPE.EQ.X$C_EXPOSE) THEN
C    PROCESS AN EXPOSURE EVENT.
*          CALL X$COPY_AREA(DDXDS,DDXPM,DDXWD,DDXGC,
*     X                     0,0,DDXXZ,DDXYZ,0,0)
*          CALL X$FLUSH(DDXDS)
*        ELSE
C    SKIP THE EVENT AND TRY AGAIN.
*          GO TO 101
*        ENDIF
*      ENDIF
      DDXKO=0
C
C  RETURN TO CALLING SUBROUTINE.
  201 RETURN
C
      END
      SUBROUTINE UGXI06
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                                                                   *
C *  THIS SUBROUTINE IS AN ASYNCHRONOUS SYSTEM TRAP (AST) FOR THE     *
C *  TIMER.  WHEN ENABLED, IT IS ENTERED WHENEVER THE TIME INTERVAL   *
C *  ESTABLISHED IN UGXI01 EXPIRES.                                   *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGXI06                                                    *
C *                                                                   *
C *  THERE ARE NO PARAMETERS IN THE CALLING SEQUENCE.                 *
C *                                                                   *
C *********************************************************************
C
      INCLUDE       'UGSYSTEM:UGDDXXWI.FOR'
C
C  PROCESS A TIME-OUT.
      IF (DDXHF.EQ.-1) THEN
        DDXHF=1
*        CALL SYS$WAKE(,)
      ENDIF
C
C  RETURN TO CALLER.
      RETURN
C
      END
