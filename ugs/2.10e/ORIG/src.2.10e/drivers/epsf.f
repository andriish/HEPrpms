      SUBROUTINE UGEP01(DDIN,DDST,DDEX)

* 19940519    KREYMER
*     Read/write formatted
*     expect status 0
*     SCRATCH file unnamed
*     Fixed unit numbers 71, 72

* 19940420    KREYMER
*     Changed to use UGOPTION

* 19940113    KREYMER
*     Changed DDIN(6/7/8 TO 7/8/9, for use with UGS 2.00

* 19930624    KREYMER
*     Added scratch file name SYS$SCRATCH:POSTSCR.TMP
*        to avoid writing scratch file on current default disk.

* 19930413    KREYMER
*     Changed UGPR10/11/12 Open/Close/Write routines from .MAR to .FOR
*     Reread file (STATUS=SCRATCH) at close to allow BoundingBox insertion
*     Generated final file with good BoundingBox near beginning

* 19930401    KREYMER
*     Moved BoundingBox comment to end
*     Added BoundingBox (atend)

* 19921210    KREYMER
*     Changed BoundingBox comment basis from EX* to DDA* to handle rotations

C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                    DEVICE-DEPENDENT MODULE FOR                    *
C *                      THE POSTSCRIPT LANGUAGE                      *
C *                                                                   *
C *  THIS SUBROUTINE IS A DEVICE-DEPENDENT MODULE FOR A NON-          *
C *  INTERACTIVE GRAPHIC DISPLAY DEVICE.                              *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGEP01(DDIN,DDST,DDEX)                                    *
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
*
*     UGS needs to know cm/unit
*         Units are set to be 300/inch, per typical laser printer
*         Postscript units are 72/inch
*     Therefore,
*     cm/unit = cm/inch * inch/unit = 2.54/300. = 

      COMMON / EPSFBOX / XMIN,XMAX,YMIN,YMAX
      INTEGER            XMIN,XMAX,YMIN,YMAX

      INTEGER       DDIN(*)
      CHARACTER*(*) DDST
      INTEGER       DDEX(*)
C
      INCLUDE       'UGSYSTEM:UGDDACBK.FOR'
      INCLUDE       'UGSYSTEM:UGDDXEPS.FOR'
C
      INTEGER*4     EXST(10)
      INTEGER*4     EXXN,EXXX,EXYN,EXYX
      REAL*4        EXZX,EXZY
      INTEGER*4     EXRA,EXFT,EXGL,EXAS
      CHARACTER*256 EXSTR(16)
      CHARACTER*256 EXNM
      CHARACTER*256 EXBP,EXEP
      CHARACTER*256 EXBX,EXEX
      CHARACTER*256 EXBR,EXER
      CHARACTER*256 EXFN
      CHARACTER*256 EXCW,EXCR,EXCG,EXCB,EXCY,EXCM,EXCC,EXCK
      EQUIVALENCE   (EXXN,EXST( 1)),     (EXXX,EXST( 2)),
     X              (EXYN,EXST( 3)),     (EXYX,EXST( 4)),
     X              (EXZX,EXST( 5)),     (EXZY,EXST( 6)),
     X              (EXRA,EXST( 7)),     (EXFT,EXST( 8)),
     X              (EXGL,EXST( 9)),     (EXAS,EXST(10))
      EQUIVALENCE   (EXNM,EXSTR( 1)),
     X              (EXBP,EXSTR( 2)),    (EXEP,EXSTR( 3)),
     X              (EXBX,EXSTR( 4)),    (EXEX,EXSTR( 5)),
     X              (EXBR,EXSTR( 6)),    (EXER,EXSTR( 7))
      EQUIVALENCE   (EXFN,EXSTR( 8)),
     X              (EXCW,EXSTR( 9)),    (EXCR,EXSTR(10)),
     X              (EXCG,EXSTR(11)),    (EXCB,EXSTR(12)),
     X              (EXCY,EXSTR(13)),    (EXCM,EXSTR(14)),
     X              (EXCC,EXSTR(15)),    (EXCK,EXSTR(16))
C
      REAL          FLT1,FLT2,FLT3
      INTEGER       INT1,INT2
C
      INTEGER       DESC_NUM
      PARAMETER    (DESC_NUM = 26)
      INTEGER*4     INST(4,DESC_NUM)
      CHARACTER*16  IFLAG(DESC_NUM)

      DATA (INST(I, 1),I=1,4)  / 2,4,  1, 0 /, IFLAG( 1) /'XMIN'   /
      DATA (INST(I, 2),I=1,4)  / 2,4,  2, 0 /, IFLAG( 2) /'XMAX'   /
      DATA (INST(I, 3),I=1,4)  / 2,4,  3, 0 /, IFLAG( 3) /'YMIN'   /
      DATA (INST(I, 4),I=1,4)  / 2,4,  4, 0 /, IFLAG( 4) /'YMAX'   /
      DATA (INST(I, 5),I=1,4)  / 3,5,  5, 0 /, IFLAG( 5) /'RUCMX'  /
      DATA (INST(I, 6),I=1,4)  / 3,5,  6, 0 /, IFLAG( 6) /'RUCMY'  /
      DATA (INST(I, 7),I=1,4)  / 1,7,  7, 1 /, IFLAG( 7) /'ROTAXIS'/
      DATA (INST(I, 8),I=1,4)  / 1,7,  8, 1 /, IFLAG( 8) /'SLDFILL'/
      DATA (INST(I, 9),I=1,4)  / 1,5,  9, 1 /, IFLAG( 9) /'GENIL'  /
      DATA (INST(I,10),I=1,4)  / 1,5, 10, 1 /, IFLAG(10) /'ASCII'  /

      DATA (INST(I,11),I=1,4)  / 4,6,  1, 64 /, IFLAG(11) /'DDNAME' /
      DATA (INST(I,12),I=1,4)  / 4,6,  2, 64 /, IFLAG(12) /'BEGFIL' /
      DATA (INST(I,13),I=1,4)  / 4,6,  3, 64 /, IFLAG(13) /'ENDFIL' /
      DATA (INST(I,14),I=1,4)  / 4,6,  4, 64 /, IFLAG(14) /'BEGPIC' /
      DATA (INST(I,15),I=1,4)  / 4,6,  5, 64 /, IFLAG(15) /'ENDPIC' /
      DATA (INST(I,16),I=1,4)  / 4,6,  6, 64 /, IFLAG(16) /'BEGREC' /
      DATA (INST(I,17),I=1,4)  / 4,6,  7, 64 /, IFLAG(17) /'ENDREC' /

      DATA (INST(I,18),I=1,4)  / 4,4,  8, 64 /, IFLAG(18) /'FONT'   /
      DATA (INST(I,19),I=1,4)  / 4,5,  9, 64 /, IFLAG(19) /'WHITE'  /
      DATA (INST(I,20),I=1,4)  / 4,3, 10, 64 /, IFLAG(20) /'RED'    /
      DATA (INST(I,21),I=1,4)  / 4,5, 11, 64 /, IFLAG(21) /'GREEN'  /
      DATA (INST(I,22),I=1,4)  / 4,4, 12, 64 /, IFLAG(22) /'BLUE'   /
      DATA (INST(I,23),I=1,4)  / 4,6, 13, 64 /, IFLAG(23) /'YELLOW' /
      DATA (INST(I,24),I=1,4)  / 4,7, 14, 64 /, IFLAG(24) /'MAGENTA'/
      DATA (INST(I,25),I=1,4)  / 4,4, 15, 64 /, IFLAG(25) /'CYAN'   /
      DATA (INST(I,26),I=1,4)  / 4,5, 16, 64 /, IFLAG(26) /'BLACK'  /

* * *  debug
*      integer nopt9
*      data    nopt9 / 0 /
*      save    nopt9
* 
*      if (ddin(1).ne.9) then
*         write (*,*) ' EPSF op/nopt9 ',DDIN(1),nopt9
*         nopt9 = 0
*      else
*         nopt9 = nopt9 + 1
*      endif
* * *  debug

C  CHECK OPERATION FLAG AND BRANCH TO THE CORRECT SECTION.

      INT1=DDIN(1)
      GOTO (101,151,201,251,301,351,401,451,501,551),INT1
      GOTO 902
C
C  OPERATION 1: OPEN THE GRAPHIC DEVICE.
C@101 EXXN=150
C@@@  EXXX=3150
C@@@  EXYN=150
C@@@  EXYX=2400
c 101 EXXN=150
c     EXXX=1880
c     EXYN=150
c     EXYX=2400
c     EXZX=300./2.54   ! UGunits/cm at 300/in
c     EXZY=300./2.54   ! UGunits/cm at 300/in
  101 EXXN=300
      EXXX=3760
      EXYN=300
      EXYX=4800
      EXZX=600./2.54   ! UGunits/cm at 600/in
      EXZY=600./2.54   ! UGunits/cm at 600/in
      EXRA=0
c     EXFT=0
      EXFT=1           ! color device ?
      EXGL=0
      EXAS=0
c     EXNM='UGDEVICE.DAT'
      EXNM='ugdevice.eps'
      EXBP='  '
      EXEP='  '
      EXBX='  '
      EXEX='  '
      EXBR='  '
      EXER='  '
      EXFN='Helvetica'
      EXCW='000000'   ! White
      EXCR='FF0000'   ! Red
      EXCG='00FF00'   ! Green
      EXCB='0000FF'   ! Blue
      EXCY='FFFF00'   ! Yellow
      EXCM='FF00FF'   ! Magenta
      EXCC='00FFFF'   ! Cyan
      EXCK='FFFFFF'   ! Black
      J=0
      DO I=1,LEN(DDST)
         IF (DDST(I:I).EQ.',') J=0
         IF (DDST(I:I).EQ.'=') J=1
         IF (J.EQ.0.AND.DDST(I:I).GE.'a'.AND.DDST(I:I).LE.'z')
     &        DDST(I:I)=CHAR(ICHAR(DDST(I:I))-32)
      ENDDO
      CALL UGOPTION(DDST,DESC_NUM,INST,IFLAG,EXST,EXSTR)
      CALL UGEP13(EXFN,*902)
      CALL UGEP14(EXCW,*902)
      DDALX=DDXZZ
      CALL UGZ005(DDXRY,DDACX)
      DDAAT='EPSF    '
C@@@  IF (EXRA.EQ.0) THEN
      IF (EXRA.NE.0) THEN
        DDABD(1,1)=EXXN
        DDABD(1,2)=EXXX
        DDABD(2,1)=EXYN
        DDABD(2,2)=EXYX
        DDABX=1.0/EXZX
        DDABY=1.0/EXZY
      ELSE
        DDABD(1,1)=EXYN
        DDABD(2,1)=EXXN
        DDABD(1,2)=EXYX
        DDABD(2,2)=EXXX
        DDABX=1.0/EXZY
        DDABY=1.0/EXZX
      END IF
      IF (DDABD(1,1).GE.DDABD(1,2)) GO TO 902
      IF (DDABD(2,1).GE.DDABD(2,2)) GO TO 902
      DDXID='DDA/EP00'
      DDXRA=EXRA
      DDXFT=EXFT
      DDXGL=EXGL
      DDXAS=EXAS
      CALL UGEP02(EXBP,DDXBP,DDXN1)
      CALL UGEP02(EXEP,DDXEP,DDXN2)
      CALL UGEP02(EXBX,DDXBX,DDXN3)
      CALL UGEP02(EXEX,DDXEX,DDXN4)
      CALL UGEP02(EXBR,DDXBR,DDXN5)
      CALL UGEP02(EXER,DDXER,DDXN6)
      DDXPC=0
      DDXPV=0
      DDXDC=0
C    OPEN A PATH TO THE DEVICE.
      CALL UGEP10(EXNM,DDXIO,INT1,INT2)
      IF (INT2.NE.0) GO TO 902
      DDXBM=INT1-DDXN6
      IF ((DDXBM-DDXN5).LT.64) GO TO 902
      IF ((DDXBM-DDXN5).LT.DDXN1) GO TO 902
      IF ((DDXBM-DDXN5).LT.DDXN2) GO TO 902
      IF ((DDXBM-DDXN5).LT.DDXN3) GO TO 902
      IF ((DDXBM-DDXN5).LT.DDXN4) GO TO 902
C    INITIALIZE THE OUTPUT PROCESS.
      DDXBN=0
      CALL UGEP08
C    GENERATE INITIAL RECORD IF NECESSARY.
      IF (DDXN1.GT.0) THEN
        CALL UGEP07(DDXBP(1:DDXN1))
        CALL UGEP08
      END IF
C    GENERATE INITIALIZATION RECORDS.
      CALL UGEP07('%!PS-Adobe-1.0')
      CALL UGEP08
      CALL UGEP07('%%Creator: Unified Graphics System')
      CALL UGEP08
      CALL UGEP07('%%Pages: (atend)')
      CALL UGEP08
C     CALL UGEP07('%%DocumentFonts: Courier')
      CALL UGEP07('%%DocumentFonts: ')
      CALL UGEP07(EXFN(:INDEX(EXFN,' ')-1))
      CALL UGEP08
      CALL UGEP07('%%BoundingBox: (atend)')
      CALL UGEP08
      XMIN = 999999
      YMIN = 999999
      XMAX = 0
      YMAX = 0
*      CALL UGEP07('%%BoundingBox: ')
*      CALL UGEP06((72./2.54)*DDABD(2,1)*DDABY,0)
*      CALL UGEP06((72./2.54)*DDABD(1,1)*DDABX,0)
*      CALL UGEP06((72./2.54)*DDABD(2,2)*DDABY,0)
*      CALL UGEP06((72./2.54)*DDABD(1,2)*DDABX,0)
      DDXBN=DDXBN-1
      CALL UGEP08
      CALL UGEP07('%%EndComments')
      CALL UGEP08
      CALL UGEP07('/Sw{setlinewidth}def')
      CALL UGEP08
      CALL UGEP07('/Sg{setgray}def')
      CALL UGEP08
      CALL UGEP07('/Sd{setdash}def')
      CALL UGEP08
      CALL UGEP07('/P {newpath')
      CALL UGEP08
      CALL UGEP07('    moveto')
      CALL UGEP08
      CALL UGEP07('0 0 rlineto')
      CALL UGEP08
      CALL UGEP07('    stroke}def')
      CALL UGEP08
      CALL UGEP07('/M {moveto}def')
      CALL UGEP08
      CALL UGEP07('/D {lineto}def')
      CALL UGEP08
      CALL UGEP07('/N {newpath}def')
      CALL UGEP08
      CALL UGEP07('/S {stroke}def')
      CALL UGEP08
C     CALL UGEP07('/T {/Courier findfont')
      CALL UGEP07('/T {/')
      CALL UGEP07(EXFN(:INDEX(EXFN,' ')-1))
      CALL UGEP07(' findfont')
      CALL UGEP08
      CALL UGEP07('    exch')
      CALL UGEP08
      CALL UGEP07('    scalefont')
      CALL UGEP08
      CALL UGEP07('    setfont}def')
      CALL UGEP08
      CALL UGEP07('/R {rotate}def')
      CALL UGEP08
      CALL UGEP07('/W {show}def')
      CALL UGEP08
      CALL UGEP07('/F {fill}def')
      CALL UGEP08
      CALL UGEP07('/X {gsave}def')
      CALL UGEP08
      CALL UGEP07('/Y {grestore}def')
      CALL UGEP08
      CALL UGEP07('%%EndProlog')
      CALL UGEP08
      GO TO 901
C
C  OPERATION 2: CLOSE THE GRAPHIC DEVICE.
  151 IF (DDXPV.NE.0) THEN
        CALL UGEP07('showpage')
        CALL UGEP08
C    GENERATE FINAL PICTURE RECORD IF NECESSARY.
        IF (DDXN4.GT.0) THEN
          CALL UGEP07(DDXEX(1:DDXN4))
          CALL UGEP08
        END IF
      END IF
C    GENERATE FILE TRAILER.
      CALL UGEP07('%%Trailer')
      CALL UGEP08
      CALL UGEP07('%%Pages: ')
      CALL UGEP05(DDXPC)
      DDXBN=DDXBN-1
      CALL UGEP08

      CALL UGEP07('%%BoundingBox: ')
c     CALL UGPR06( XMIN * 72./300. - 2 , 0 )
c     CALL UGPR06( YMIN * 72./300. - 2 , 0 )
c     CALL UGPR06( XMAX * 72./300. + 1 , 0 )
c     CALL UGPR06( YMAX * 72./300. + 1 , 0 )
      CALL UGPR06( XMIN * (72./2.54)*DDABX - 4 , 0 )
      CALL UGPR06( YMIN * (72./2.54)*DDABY - 4 , 0 )
      CALL UGPR06( XMAX * (72./2.54)*DDABX + 3 , 0 )
      CALL UGPR06( YMAX * (72./2.54)*DDABY + 3 , 0 )
      CALL UGEP08

C    GENERATE FINAL RECORD IF NECESSARY.
      IF (DDXN2.GT.0) THEN
        CALL UGEP07(DDXEP(1:DDXN2))
        CALL UGEP08
      END IF
C    TERMINATE THE DATA SET.
      CALL UGEP11(EXNM,DDXIO,DDABX,DDABY)
      GO TO 901
C
C  OPERATION 3: CLEAR THE SCREEN ON THE GRAPHIC DEVICE.
  201 IF (DDIN(2).NE.0) GO TO 902
      DDXDC=1
      DDXCI=-9999
      DDXCC=-9999
      DDXCL=-9999
      DDXCT=-9999
      DDXLA=0
      GO TO 901
C
C  OPERATION 4: MANIPULATE THE SCREEN ON THE GRAPHIC DEVICE.
  251 GO TO 901
C
C  OPERATION 5: BEGIN A NEW GRAPHIC SEGMENT.
  301 IF (DDXDC.NE.0) THEN
C    PROCESS DEFERRED CLEAR AND START A NEW PICTURE.
        IF (DDXPV.NE.0) THEN
          CALL UGEP07('showpage')
          CALL UGEP08
C    GENERATE FINAL PICTURE RECORD IF NECESSARY.
          IF (DDXN4.GT.0) THEN
            CALL UGEP07(DDXEX(1:DDXN4))
            CALL UGEP08
          END IF
        END IF
C    GENERATE INITIAL PICTURE RECORD IF NECESSARY.
        IF (DDXN3.GT.0) THEN
          CALL UGEP07(DDXBX(1:DDXN3))
          CALL UGEP08
        END IF
C    START THE PICTURE ITSELF.
        DDXPC=DDXPC+1
        CALL UGEP07('%%Page: ')
        CALL UGEP05(DDXPC)
        CALL UGEP05(DDXPC)
        DDXBN=DDXBN-1
        CALL UGEP08
        CALL UGEP06((72.0/2.54)*DDABX,5)
        CALL UGEP06((72.0/2.54)*DDABY,5)
        CALL UGEP07('scale')
        CALL UGEP08
        CALL UGEP07('1 setlinecap')
        CALL UGEP08
        CALL UGEP07('1 setlinejoin')
        CALL UGEP08
        DDXDC=0
        DDXPV=1
      END IF
      DDEX(2)=0
      GO TO 901
C
C  OPERATION 6: TERMINATE A GRAPHIC SEGMENT.
  351 IF (DDXLA.NE.0) THEN
        CALL UGEP07('S')
        CALL UGEP08
        DDXLA=0
      END IF
      GO TO 901
C
C  OPERATION 7: MANIPULATE A GRAPHIC SEGMENT.
  401 GO TO 902
C
C  OPERATION 8: INQUIRE ABOUT A GRAPHIC PRIMITIVE.
  451 IF (DDXLA.NE.0) THEN
        CALL UGEP07('S')
        CALL UGEP08
        DDXLA=0
      END IF
      INT1=DDIN(2)
* * *  debug
*      write (*,*) ' into EPSF inquiry ',INT1
* * *  debug
      IF ((INT1.LT.1).OR.(INT1.GT.5)) GO TO 902
      GO TO (456,461,466,471,476),INT1
C    INQUIRE ABOUT POINTS.
  456 CALL UGEP03(1,DDIN(3),DDIN(4),1)
      GO TO 901
C    INQUIRE ABOUT LINES.
  461 CALL UGEP03(2,DDIN(3),DDIN(4),DDIN(7))
      GO TO 901
C    INQUIRE ABOUT TEXT.
  466 CALL UGEP03(3,DDIN(3),DDIN(4),1)
      IF (DDXCT.NE.DDIN(7)) THEN
        CALL UGEP05(NINT(1.667*REAL(DDIN(7))))
        CALL UGEP07('T')
        CALL UGEP08
        DDXCT=DDIN(7)
      END IF
C@@@  IF (DDXRA.EQ.0) THEN
      IF (DDXRA.NE.0) THEN
        DDXSA=DDIN(8)-90
      ELSE
        DDXSA=DDIN(8)
      END IF
      FLT1=REAL(DDXCT)
      FLT2=COS(REAL(DDIN(8))/57.2957795)
      FLT3=SIN(REAL(DDIN(8))/57.2957795)
      DDEX(2)=-NINT(FLT1*(0.500*FLT2-0.467*FLT3))
      DDEX(3)=-NINT(FLT1*(0.500*FLT3+0.467*FLT2))
      DDEX(4)=NINT(FLT1*FLT2)
      DDEX(5)=NINT(FLT1*FLT3)
      GO TO 901
C    INQUIRE ABOUT POLYGON-FILL.
  471 DDXSI=DDIN(3)
      DDXSC=DDIN(4)
      GO TO 901
C    INQUIRE ABOUT DEVICE-DEPENDENT DATA.
  476 CALL UGEP03(2,DDIN(3),DDIN(4),1)
      GO TO 901
C
C  OPERATION 9: DISPLAY A GRAPHIC PRIMITIVE.
  501 INT1=DDIN(2)
      IF ((INT1.LT.1).OR.(INT1.GT.5)) GO TO 902
      GO TO (506,511,516,521,526),INT1
C    DISPLAY POINTS.
  506 CALL UGEP04(DDIN(3),DDIN(4))
      CALL UGEP07('P')
      CALL UGEP08
      GO TO 901
C    DISPLAY LINES.
  511 IF (DDIN(5).EQ.0) THEN
        IF (DDXLA.NE.0) THEN
          CALL UGEP07('S')
          CALL UGEP08
          DDXLA=0
        END IF
        CALL UGEP07('N')
        CALL UGEP08
        CALL UGEP04(DDIN(3),DDIN(4))
        CALL UGEP07('M')
        CALL UGEP08
      ELSE
        CALL UGEP04(DDIN(3),DDIN(4))
        CALL UGEP07('D')
        CALL UGEP08
      END IF
      DDXLA=1
      GO TO 901
C    DISPLAY TEXT.
  516 CALL UGEP09(1,DDST)
      CALL UGEP04(DDIN(3),DDIN(4))
      CALL UGEP07('M')
      CALL UGEP08
      IF (DDXSA.NE.0) THEN
        CALL UGEP05(DDXSA)
        CALL UGEP07('R')
        CALL UGEP08
      END IF
      CALL UGEP07('(')
      DO 517 INT1=1,LEN(DDST)
        IF ((DDXBM-DDXBN).LT.5) THEN
          CALL UGEP07('\')
          CALL UGEP08
        END IF
        IF ((DDST(INT1:INT1).EQ.'(').OR.
     X      (DDST(INT1:INT1).EQ.')').OR.
     X      (DDST(INT1:INT1).EQ.'\')) CALL UGEP07('\')
        CALL UGEP07(DDST(INT1:INT1))
  517 CONTINUE
      CALL UGEP07(') W')
      CALL UGEP08
      IF (DDXSA.NE.0) THEN
        CALL UGEP04(DDIN(3),DDIN(4))
        CALL UGEP07('M')
        CALL UGEP08
        CALL UGEP05(-DDXSA)
        CALL UGEP07('R')
        CALL UGEP08
      END IF
      GO TO 901
C    DISPLAY POLYGON-FILL.
  521 IF (DDXFT.EQ.0) THEN
        CALL UGEP03(4,DDXSI,8,1)
      ELSE
        CALL UGEP03(5,DDXSI,DDXSC,1)
      END IF
      CALL UGEP07('N')
      CALL UGEP08
      DO 522 INT1=1,DDIN(3)
        CALL UGEP04(DDIN(2*INT1+2),DDIN(2*INT1+3))
        IF (INT1.EQ.1) THEN
          CALL UGEP07('M')
        ELSE
          CALL UGEP07('D')
        END IF
        CALL UGEP08
  522 CONTINUE
      IF (DDXFT.EQ.0) THEN
        CALL UGEP07('X')
        CALL UGEP08
        CALL UGEP07('F')
        CALL UGEP08
        CALL UGEP07('Y')
        CALL UGEP08
        CALL UGEP03(5,DDXSI,DDXSC,1)
        CALL UGEP07('S')
        CALL UGEP08
      ELSE
        CALL UGEP07('F')
        CALL UGEP08
      END IF
      GO TO 901
C    DISPLAY DEVICE-DEPENDENT DATA.
  526 IF (LEN(DDST).LT.9) GO TO 901
      IF (DDST(1:8).NE.'EPSF:   ') GO TO 901
      IF ((DDXBM-DDXBN).LT.LEN(DDST)) GO TO 901
      CALL UGEP04(DDIN(3),DDIN(4))
      CALL UGEP07('M')
      CALL UGEP08
      CALL UGEP07(DDST(9:))
      CALL UGEP08
      GO TO 901
C
C  OPERATION 10: PROCESS MISCELLANEOUS CONTROL FUNCTIONS.
  551 GO TO 902
C
C  SET ERROR INDICATOR AND RETURN TO CALLER.
  901 DDEX(1)=0
      GO TO 903
  902 DDEX(1)=1
  903 CONTINUE
      RETURN
C
      END
      SUBROUTINE UGEP02(HEXS,BITS,NBIT)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                                                                   *
C *  THIS SUBROUTINE IS USED TO CONVERT A CHARACTER STRING            *
C *  CONTAINING HEXADECIMAL CHARACTERS INTO THE EQUIVALENT BIT        *
C *  STRING.  ANY ERROR CAUSES A ZERO STRING LENGTH TO BE RETURNED.   *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGEP02(HEXS,BITS,NBIT)                                    *
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
      SUBROUTINE UGEP03(TYPE,INTN,COLR,LSTR)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                                                                   *
C *  THIS SUBROUTINE IS USED TO PROCESS THE INTENSITY, COLOR, AND     *
C *  LINE STRUCTURE INFORMATION.                                      *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGEP03(TYPE,INTN,COLR,LSTR)                               *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    TYPE  THE TYPE OF THE GRAPHIC PRIMITIVE BEING PROCESSED (1     *
C *          MEANS POINTS, 2 MEANS LINES, 3 MEANS TEXT, AND 4 AND 5   *
C *          MEAN FILL AREA).                                         *
C *    INTN  THE INTENSITY LEVEL.                                     *
C *    COLR  1/8 = WHITE/RED/GREEN/BLUE/YELLOW/MAGENTA/CYAN/BLACK     *
C *          NOTE: for postscript visibility interchange black/white  *
C *    LSTR  THE LINE STRUCTURE.                                      *
C *                                                                   *
C *********************************************************************
C
      INTEGER       TYPE,INTN,COLR,LSTR
C
      INCLUDE       'UGSYSTEM:UGDDACBK.FOR'
C
      INCLUDE       'UGSYSTEM:UGDDXEPS.FOR'
C
      INTEGER       AINT
C
      PARAMETER     (UNITW=300./2.54)
C
C  SET UP THE ACTUAL INTENSITY AND COLOR INDICES.
      IF (DDXGL.EQ.0) THEN
        AINT=3 ! 1
      ELSE
        AINT=INTN
      END IF
      IF (DDXFT.EQ.0) THEN
        IF (TYPE.EQ.4) THEN
            COLRR = 1.
            COLRG = 1.
            COLRB = 1.
        ELSE
            COLRR = 0.
            COLRG = 0.
            COLRB = 0.
        END IF
      ELSE
      END IF
C
C  SET THE INTENSITY LEVEL IF NECESSARY.
      IF (DDXCI.NE.AINT) THEN
        CALL UGEP06(MAX(REAL(AINT)/DDABX/UNITW,1.0),0)
        CALL UGEP07('Sw')
        CALL UGEP08
        DDXCI=AINT
      END IF
C
C  SET THE COLOR IF NECESSARY.
      IF (COLR.NE.DDXCC) THEN
         DDXCC = COLR
         CALL UGEP15(COLR,COLRR,COLRG,COLRB)
         CALL UGEP06(COLRR,2)
         CALL UGEP06(COLRG,2)
         CALL UGEP06(COLRB,2)
         CALL UGEP07('setrgbcolor')
         CALL UGEP08
      END IF
C
C  SET THE LINE STRUCTURE IF NECESSARY.
      IF (DDXCL.NE.LSTR) THEN
        CALL UGEP07('[')
        IF (LSTR.EQ.2) THEN
c         CALL UGEP06(1.0/(3.0*DDABX),0)
          CALL UGEP06(1.0/(6.0*DDABX),0)
          DDXBN=DDXBN-1
        ELSE IF (LSTR.EQ.3) THEN
          CALL UGEP06(REAL(AINT)/DDABX/UNITW,0)
c         CALL UGEP06(1.0/(4.0*DDABX),0)
          CALL UGEP06(1.0/(8.0*DDABX),0)
          DDXBN=DDXBN-1
        ELSE IF (LSTR.EQ.4) THEN
c         CALL UGEP06(1.0/(4.0*DDABX),0)
c         CALL UGEP06(1.0/(4.0*DDABX),0)
          CALL UGEP06(1.0/(8.0*DDABX),0)
          CALL UGEP06(1.0/(8.0*DDABX),0)
          CALL UGEP06(REAL(AINT)/DDABX/UNITW,0)
c         CALL UGEP06(1.0/(4.0*DDABX),0)
          CALL UGEP06(1.0/(8.0*DDABX),0)
          DDXBN=DDXBN-1
        END IF
        CALL UGEP07('] 0 Sd')
        CALL UGEP08
        DDXCL=LSTR
      END IF
C
C  RETURN TO CALLER.
      RETURN
C
      END
      SUBROUTINE UGEP04(XCRD,YCRD)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                                                                   *
C *  THIS SUBROUTINE IS USED TO ADD AN X AND Y COORDINATE TO THE      *
C *  OUTPUT BUFFER.                                                   *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGEP04(XCRD,YCRD)                                         *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    XCRD  THE X COORDINATE.                                        *
C *    YCRD  THE Y COORDINATE.                                        *
C *                                                                   *
C *********************************************************************
C
      INTEGER       XCRD,YCRD
C
      INCLUDE       'UGSYSTEM:UGDDACBK.FOR'
C
      INCLUDE       'UGSYSTEM:UGDDXEPS.FOR'
C
      COMMON / EPSFBOX / XMIN,XMAX,YMIN,YMAX
      INTEGER            XMIN,XMAX,YMIN,YMAX

C  ADD THE COORDINATES TO THE OUTPUT BUFFER.
C@@@  IF (DDXRA.EQ.0) THEN
      IF (DDXRA.NE.0) THEN
        CALL UGEP05(YCRD)
        CALL UGEP05(DDABD(1,2)+DDABD(1,1)-XCRD)
        XMIN = MIN ( XMIN , YCRD )
        XMAX = MAX ( XMAX , YCRD )
        YMIN = MIN ( YMIN , DDABD(1,2)+DDABD(1,1)-XCRD )
        YMAX = MAX ( YMAX , DDABD(1,2)+DDABD(1,1)-XCRD )
      ELSE
        CALL UGEP05(XCRD)
        CALL UGEP05(YCRD)
        XMIN = MIN ( XMIN , XCRD )
        XMAX = MAX ( XMAX , XCRD )
        YMIN = MIN ( YMIN , YCRD )
        YMAX = MAX ( YMAX , YCRD )
      END IF
C
C  RETURN TO CALLER.
      RETURN
C
      END
      SUBROUTINE UGEP05(VALU)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                                                                   *
C *  THIS SUBROUTINE IS USED TO ADD AN INTEGER VALUE TO THE OUTPUT    *
C *  BUFFER.  THE VALUE IS ALWAYS TERMINATED WITH A BLANK.  THE       *
C *  CALLER MUST ASSURE THAT THERE IS ENOUGH ROOM IN THE OUTPUT       *
C *  BUFFER TO HOLD THE NEW DATA.                                     *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGEP05(VALU)                                              *
C *                                                                   *
C *  THE PARAMETER IN THE CALLING SEQUENCE IS:                        *
C *    VALU  THE INTEGER VALUE TO BE ADDED TO THE OUTPUT BUFFER.      *
C *                                                                   *
C *********************************************************************
C
      INTEGER       VALU
C
      CHARACTER*13  STRG
      INTEGER       NBLK
C
C  CREATE THE STRING AND PACK IT INTO THE OUTPUT BUFFER.
      CALL UGCNVF(REAL(VALU),0,STRG(1:12),NBLK)
      STRG(13:13)=' '
      CALL UGEP07(STRG(13-NBLK:13))
C
C  RETURN TO CALLER.
      RETURN
C
      END
      SUBROUTINE UGEP06(VALU,NDEC)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                                                                   *
C *  THIS SUBROUTINE IS USED TO ADD A REAL VALUE TO THE OUTPUT        *
C *  BUFFER.  THE VALUE IS ALWAYS TERMINATED WITH A BLANK.  THE       *
C *  CALLER MUST ASSURE THAT THERE IS ENOUGH ROOM IN THE OUTPUT       *
C *  BUFFER TO HOLD THE NEW DATA.                                     *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGEP06(VALU,NDEC)                                         *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    VALU  THE REAL VALUE TO BE ADDED TO THE OUTPUT BUFFER.         *
C *    NDEC  THE NUMBER OF DECIMAL PLACES IN THE OUTPUT STRING.       *
C *                                                                   *
C *********************************************************************
C
      REAL          VALU
      INTEGER       NDEC
C
      CHARACTER*13  STRG
      INTEGER       NBLK
C
C  CREATE THE STRING AND PACK IT INTO THE OUTPUT BUFFER.
      CALL UGCNVF(VALU,NDEC,STRG(1:12),NBLK)
      STRG(13:13)=' '
      CALL UGEP07(STRG(13-NBLK:13))
C
C  RETURN TO CALLER.
      RETURN
C
      END
      SUBROUTINE UGEP07(STRG)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                                                                   *
C *  THIS SUBROUTINE IS USED TO PACK A CHARACTER STRING INTO THE      *
C *  OUTPUT BUFFER.  THE CALLER MUST ASSURE THAT THERE IS ENOUGH      *
C *  ROOM IN THE OUTPUT BUFFER TO HOLD THE NEW DATA.                  *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGEP07(STRG)                                              *
C *                                                                   *
C *  THE PARAMETER IN THE CALLING SEQUENCE IS:                        *
C *    STRG  THE CHARACTER STRING.                                    *
C *                                                                   *
C *********************************************************************
C
      CHARACTER*(*) STRG
C
      INCLUDE       'UGSYSTEM:UGDDXEPS.FOR'
C
      INTEGER       NCHR
C
C  PACK THE CHARACTERS INTO THE OUTPUT BUFFER.
      NCHR=LEN(STRG)
      DDXBF(DDXBN+1:DDXBN+NCHR)=STRG(1:NCHR)
      DDXBN=DDXBN+NCHR
C
C  RETURN TO CALLER.
      RETURN
C
      END
      SUBROUTINE UGEP08
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                                                                   *
C *  THIS SUBROUTINE IS USED TO COMPLETE THE CURRENT RECORD, WRITE    *
C *  IT TO THE OUTPUT FILE, AND RE-INITIALIZE THE BUFFER.  THE        *
C *  CALLER MUST ASSURE THAT THERE IS ENOUGH ROOM IN THE OUTPUT       *
C *  BUFFER TO COMPLETE THE RECORD.                                   *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGEP08                                                    *
C *                                                                   *
C *  THERE ARE NO PARAMETERS IN THE CALLING SEQUENCE.                 *
C *                                                                   *
C *********************************************************************
C
      INCLUDE       'UGSYSTEM:UGDDXEPS.FOR'
C
C  VERIFY THAT THE BUFFER CONTAINS DATA TO BE WRITTEN OUT.
      IF (DDXBN.LE.0) GO TO 201
C
C  COMPLETE THE RECORD.
C    ADD THE END OF RECORD STRING IF NECESSARY.
      IF (DDXN6.GT.0) CALL UGEP07(DDXER(1:DDXN6))
C    TRANSLATE THE RECORD IF NECESSARY.
      IF (DDXAS.NE.0) CALL UGEP09(0,DDXBF(1:DDXBN))
C
C  WRITE THE RECORD TO THE DEVICE.
      CALL UGEP12(DDXIO,DDXBF(1:DDXBN))
C
C  RE-INITIALIZE THE BUFFER.
  201 DDXBN=0
C    ADD THE BEGINNING OF RECORD STRING IF NECESSARY.
      IF (DDXN5.GT.0) CALL UGEP07(DDXBR(1:DDXN5))
C
C  RETURN TO CALLER.
      RETURN
C
      END
      SUBROUTINE UGEP09(FLAG,STRG)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                                                                   *
C *  THIS SUBROUTINE IS USED TO TRANSLATE CHARACTER STRINGS FROM THE  *
C *  COMPUTER CHARACTER SET TO THE DEVICE CHARACTER SET, OR FROM THE  *
C *  COMPUTER CHARACTER SET TO DISPLAYABLE CHARACTERS.                *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGEP09(FLAG,STRG)                                         *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    FLAG  TRANSLATE FLAG (0 MEANS COMPUTER TO DEVICE, AND 1        *
C *          COMPUTER TO DISPLAYABLE CHARACTERS).                     *
C *    STRG  THE CHARACTER STRING TO BE TRANSLATED.                   *
C *                                                                   *
C *********************************************************************
C
      INTEGER       FLAG
      CHARACTER*(*) STRG
C
      INTEGER       INT1
      CHARACTER*1   CHR1
C
C  TRANSLATE THE CHARACTER STRING AS NEEDED.
      IF (FLAG.EQ.1) THEN
        DO 101 INT1=1,LEN(STRG)
          CHR1=STRG(INT1:INT1)
          IF ((CHR1.LT.' ').OR.(CHR1.GT.'~')) STRG(INT1:INT1)='@'
  101   CONTINUE
      END IF
C
C  RETURN TO CALLER.
      RETURN
C
      END

      SUBROUTINE UGEP10 ( NAME , IUNI , LREL , IFLG )
*******************  THE UNIFIED GRAPHICS SYSTEM  *******************
*                    DEVICE-DEPENDENT MODULE FOR                    *
*                      THE POSTSCRIPT LANGUAGE                      *
*                                                                   *
*  THESE SUBROUTINES ARE THE ACTUAL OUTPUT ROUTINES.  UGEP10 IS     *
*  THE OPEN MODULE, UGEP11 IS THE CLOSE MODULE, AND UGEP12 IS THE   *
*  WRITE MODULE.                                                    *
*                                                                   *
*  THE CALLING SEQUENCES ARE:                                       *
*    CALL UGEP10(NAME,IUNI,LREL,IFLG)                               *
*    CALL UGEP11(NAME,IUNI)                                         *
*    CALL UGEP12(IUNI,STRG)                                         *
*                                                                   *
*  THE PARAMETERS IN THE CALLING SEQUENCES ARE:                     *
*    NAME  THE NAME OF THE OUTPUT DATA SET.  THE CHARACTER STRING   *
*          SHOULD BE 64 CHARACTERS LONG.                            *
*    IUNI  A POINTER TO THE I/O CONTROL BLOCKS.  THIS ITEM IS       *
*          GENERATED BY THE OPEN FUNCTION AND MUST BE SUPPLIED IN   *
*          THE OTHER FUNCTIONS.                                     *
*    LREL  LOGICAL RECORD LENGTH OF THE OUTPUT BUFFER.              *
*    IFLG  A SUCCESS FLAG (0 MEANS SUCCESS, 1 MEANS ERROR).         *
*    STRG  THE CHARACTER STRING THAT IS TO BE WRITTEN TO THE        *
*          OUTPUT DATA SET.                                         *
*                                                                   *
*                          ROBERT C. BEACH                          *
*                    COMPUTATION RESEARCH GROUP                     *
*                STANFORD LINEAR ACCELERATOR CENTER                 *
*                                                                   *
*********************************************************************

      CHARACTER*(*) NAME

C         Get a free Fortran logical unit number
*      ISTAT = LIB$GET_LUN ( IUNI )
       IUNI = 71

C         Open the file, with Carriage Return carriage control,
C         status SCRATCH because we will regenerate with correct BoundingBox
      OPEN (
     +    IUNI ,
     +    STATUS          = 'SCRATCH' ,
     +    FORM            = 'FORMATTED' ,
     +    IOSTAT          =  IFLG
     +    )
* * *    +    FILE            = 'scratch'//NAME,     !! For IRIX 4 only * * *


      IF ( IFLG.NE.0 ) IFLG = 1

      LREL = 132

      RETURN
      END

      SUBROUTINE UGEP11 ( NAME , IUNI , DDABX , DDABY )

      CHARACTER*(*)  NAME

      CHARACTER*132 LINE

      COMMON / EPSFBOX / XMIN,XMAX,YMIN,YMAX
      INTEGER            XMIN,XMAX,YMIN,YMAX

      LOGICAL SEARCH

      REWIND (IUNI)

C         Get a free Fortran logical unit number
C      ISTAT = LIB$GET_LUN ( JUNI )
      JUNI = 72

      OPEN (
     +    JUNI ,
     +    FILE            =  NAME ,
     +    STATUS          = 'UNKNOWN' ,
     +    FORM            = 'FORMATTED' ,
     +    IOSTAT          =  IFLG
     +    )

      SEARCH = .TRUE.

      DO 1 I = 1 , 10 000 000

          LINE = ' '
          READ (IUNI, '(A)', END=10, ERR=2, IOSTAT=IOST ) LINE
    2     CONTINUE
          IF ( IOST.NE.0 ) GO TO 10
          IF ( SEARCH ) THEN
             IF ( LINE .EQ. '%%BoundingBox: (atend)' ) THEN
                 WRITE (LINE, '( ''%%BoundingBox:'',4I5)' )
c    +           XMIN*72/300-1,YMIN*72/300-1,XMAX*72/300+1,YMAX*72/300+1
     +               NINT( XMIN * 72./2.54*DDABX ) - 4 ,
     +               NINT( YMIN * 72./2.54*DDABY ) - 4 ,
     +               NINT( XMAX * 72./2.54*DDABX ) + 3 ,
     +               NINT( YMAX * 72./2.54*DDABY ) + 3
                SEARCH = .FALSE.
             ENDIF
          ENDIF

          LENG = INDEX ( LINE , '          ' ) - 1
          LENG = MAX ( LENG , 1 )

          WRITE ( JUNI,'(A)' ) LINE(:LENG)

    1 CONTINUE

      WRITE (*,*) ' * * ERROR IN UGEP11, EPSF FILE CLOSE * * '
      WRITE (*,*) ' More than 10 million lines of data in PS output '

   10 CONTINUE

      CLOSE ( IUNI )
      CLOSE ( JUNI )

      RETURN
      END

      SUBROUTINE UGEP12 ( IUNI , STRG )

      CHARACTER*(*) STRG

      WRITE ( IUNI ,'(A)') STRG

      RETURN
      END

      SUBROUTINE UGEP13 ( FONT , * )
      CHARACTER FONT*(*)
      J=0
      DO I=1,INDEX(FONT,' ')-1
         IF (J.EQ.0
     *        .AND.ICHAR(FONT(I:I)).GE.ICHAR('a')
     *        .AND.ICHAR(FONT(I:I)).LE.ICHAR('z'))
     *        FONT(I:I)=CHAR(ICHAR(FONT(I:I))-32)
         IF (J.NE.0
     *        .AND.ICHAR(FONT(I:I)).GE.ICHAR('A')
     *        .AND.ICHAR(FONT(I:I)).LE.ICHAR('Z'))
     *        FONT(I:I)=CHAR(ICHAR(FONT(I:I))+32)
         J=1
         IF (FONT(I:I).EQ.'-') J=0
      ENDDO
      J=INDEX(FONT,'Boldoblique')
      if (J.NE.0) FONT(J:)='BoldOblique'
      IF  (FONT.NE.'Times-Roman'
     *.AND.FONT.NE.'Times-Italic'
     *.AND.FONT.NE.'Times-Bold'
     *.AND.FONT.NE.'Times-BoldItalic'
     *.AND.FONT.NE.'Helvetica'
     *.AND.FONT.NE.'Helvetica-Oblique'
     *.AND.FONT.NE.'Helvetica-Bold'
     *.AND.FONT.NE.'Helvetica-BoldOblique'
     *.AND.FONT.NE.'Courier'
     *.AND.FONT.NE.'Courier-Oblique'
     *.AND.FONT.NE.'Courier-Bold'
     *.AND.FONT.NE.'Courier-BoldOblique'
     *.AND.FONT.NE.'Symbol') RETURN 1
      RETURN
      END

      SUBROUTINE UGEP14( CTBL , * )
      CHARACTER CTBL(*)*(*)
      INTEGER   ICOL(3)
      REAL      COL(3,8)
      SAVE      COL
      DO I=1,8
         IF      (INDEX(CTBL(I),' ').EQ.13) THEN
            READ(CTBL(I),'(3Z4.4)',ERR=90000) (ICOL(J),J=1,3)
            DO J=1,3
               COL(J,I)=FLOAT(ICOL(J))/65535.
            ENDDO
         ELSE IF (INDEX(CTBL(I),' ').EQ. 7) THEN
            READ(CTBL(I),'(3Z2.2)',ERR=90000) (ICOL(J),J=1,3)
            DO J=1,3
               COL(J,I)=FLOAT(ICOL(J))/255.
            ENDDO
         ELSE
            GOTO 90000
         ENDIF
      ENDDO
      RETURN
      ENTRY UGEP15( NCOL , COLR , COLG , COLB )
      COLR=COL(1,NCOL)
      COLG=COL(2,NCOL)
      COLB=COL(3,NCOL)
      RETURN
90000 RETURN 1
      END
