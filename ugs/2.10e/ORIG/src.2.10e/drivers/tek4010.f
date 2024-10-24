      SUBROUTINE UGTX01(DDIN,DDST,DDEX)

*******************  THE UNIFIED GRAPHICS SYSTEM  *******************
*                  DEVICE-DEPENDENT MODULE FOR THE                  *
*              TEKTRONIX 4010 SERIES GRAPHIC TERMINALS              *
*                   IN A FULLY INTERACTIVE MODE                     *
*                                                                   *
*  This subroutine is a device-dependent module for a fully         *
*  interactive graphic display device.                              *
*                                                                   *
*  The calling sequence is:                                         *
*    CALL UGTX01(DDIN,DDST,DDEX)                                    *
*                                                                   *
*  The parameters in the calling sequence are:                      *
*    DDIN  An integer input array.                                  *
*    DDST  A character string for input and output.                 *
*    DDEX  An integer output array.                                 *
*                                                                   *
*                          ROBERT C. BEACH                          *
*                    COMPUTATION RESEARCH GROUP                     *
*                STANFORD LINEAR ACCELERATOR CENTER                 *
*                                                                   *
*********************************************************************

* 19940802	KREYMER@FNAL.GOV
*
* Conversion to Unix
*     removed /LIST, /NOLIST from INCLUDES
*     dummied UGTX10  read routine for now
*     UGTX02 - changed data statements from hex(Z) to decimal
*     opened /dev/tt, formatted (Unformatted fails on irix 5.1.1.1)
*         will use (A,$) format, supported on irix/ultrix/aix/sunos
*     corrected string passed to UGOPTION from OPTN to DDST

      INTEGER       DDIN(*)
      CHARACTER*(*) DDST
      INTEGER       DDEX(*)

      INCLUDE       'UGSYSTEM:UGDDACBK.FOR'
      INCLUDE       'UGSYSTEM:UGDDXTIN.FOR'

      INTEGER*4     EXST(16)
      CHARACTER*64  EXCH
      EQUIVALENCE   (EXCH,EXST(1))
C
      CHARACTER*3   CLER
      CHARACTER*1   CLRX(3)
      EQUIVALENCE   (CLER,CLRX(1))
      CHARACTER*2   CRSH
      CHARACTER*1   CRSX(2)
      EQUIVALENCE   (CRSH,CRSX(1))
      CHARACTER*6   CTEK
      CHARACTER*2   CVT1
      CHARACTER*1   BELL
      CHARACTER*1   TERM
C
      INTEGER       INT1
C
      INTEGER       DESC_NUM
      PARAMETER    (DESC_NUM = 1)
      INTEGER*4     INST(4,DESC_NUM)
      CHARACTER*16  IFLAG(DESC_NUM)

      DATA (INST(I, 1),I=1,4) / 4, 7, 1, 64 /, IFLAG( 1) /'CHANNEL' /


C  Check operation flag and branch to the correct section.

      INT1 = DDIN(1)
      IF ((INT1.LT.1).OR.(INT1.GT.14)) GO TO 902
      GO TO (101,151,201,251,301,351,401,451,501,551,
     +       601,651,701,751),INT1

C  OPERATION 1: Open the graphic device.

  101 EXCH='/dev/tty'
      CALL UGOPTION ( DDST, DESC_NUM, INST, IFLAG, EXST )
      DDALX = DDXZZ
      CALL UGZ005(DDXRY,DDACX)
      DDAAT='TEK4010 '
      DDAIL=3
      DDAIC(1)=1
      DDAIC(5)=1
      DDABD(1,1)=0
      DDABD(1,2)=1023
      DDABD(2,1)=0
      DDABD(2,2)=779
      DDABX=0.0195
      DDABY=0.0195
      DDXID='DDA/TX00'
      DDXBN=0

      CALL UGTX07(EXCH,DDXIO,INT1)
      IF (INT1.NE.0) GO TO 902

      CLRX(1) = CHAR(29)
      CLRX(2) = CHAR(27)
      CLRX(3) = CHAR(12)

      CRSX(1) = CHAR(27)
      CRSX(2) = CHAR(26)

      CTEK    = CHAR(27)//'[?38h'

      CVT1    = CHAR(27)//CHAR(3)

      BELL     =  CHAR( 7)

      TERM     =  CHAR(31)

*     Switch to TEK4010 mode (for Xterm)
      DDXBN=LEN(CTEK)
      DDXBF(:DDXBN)=CTEK
      CALL UGTX04(0)

      GO TO 901

C  OPERATION 2: Close the graphic device.

  151 CONTINUE
*     Switch to VT100 mode (for Xterm)
      DDXBN=LEN(CVT1)
      DDXBF(:DDXBN)=CVT1
      CALL UGTX04(0)
      CALL UGTX08(DDXIO)
      GO TO 901

C  OPERATION 3: Clear the screen on the graphic device.

  201 IF (DDIN(2).NE.0) GO TO 902
      DDXBF(1:3)=CLER
      DDXBN=3
      CALL UGTX04(0)
*      CALL UGTX06(80)  * removed pause *
      GO TO 901

C  OPERATION 4: Manipulate the screen on the graphic device.

  251 GO TO 901

C  OPERATION 5: Begin a new graphic segment.

  301 DDXBN=0
      DDEX(2)=0
      GO TO 901

C  OPERATION 6: Terminate a graphic segment.

  351 CALL UGTX04(1)
      GO TO 901

C  OPERATION 7: Manipulate a graphic segment.

  401 GO TO 902

C  OPERATION 8: Inquire about a graphic primitive.

  451 IF (DDIN(3).LT.3) THEN
        DDXIL = 0
      ELSE
        DDXIL = 1
      END IF
      INT1 = DDIN(2)
      IF ((INT1.LT.1).OR.(INT1.GT.3)) GO TO 902
      GO TO (461,471,481),INT1
  461 GO TO 901
  471 IF (DDIN(7).NE.1) GO TO 902
      GO TO 901
  481 IF ((DDIN(8).GE.5).AND.(DDIN(8).LE.355)) GO TO 902
      IF (DDIN(9).NE.2) THEN
        IF (DDIN(7).LT.11) GO TO 902
        IF (DDIN(7).GT.17) GO TO 902
      END IF
      DDEX(2) =  -5
      DDEX(3) =  -7
      DDEX(4) =  14
      DDEX(5) =   0
      GO TO 901

C  OPERATION 9: Display a graphic primitive.

  501 INT1=DDIN(2)
      IF ((INT1.LT.1).OR.(INT1.GT.3)) GO TO 902
      GO TO (511,521,531),INT1

  511 CALL UGTX02(DDIN(3),DDIN(4),0)
      IF (DDXIL.EQ.0) THEN
        CALL UGTX02(DDIN(3),DDIN(4),1)
      ELSE
        CALL UGTX02(DDIN(3),DDIN(4)+1,1)
        IF (DDIN(3).EQ.1023) GO TO 901
        CALL UGTX02(DDIN(3)+1,DDIN(4)+1,1)
        CALL UGTX02(DDIN(3)+1,DDIN(4),1)
      END IF
      GO TO 901

  521 CALL UGTX02(DDIN(3),DDIN(4),DDIN(5))
      GO TO 901

  531 CALL UGTX03(DDIN(3),DDIN(4),DDST)
      GO TO 901

C  OPERATION 10: Process miscellaneous control functions.

  551 IF (DDIN(2).NE.1) GO TO 902
      DO 552 INT1 = 1,16
        DDXBF(INT1:INT1) = BELL
  552 CONTINUE
      DDXBN = 16
      CALL UGTX04(0)
*      CALL UGTX06(20)
      GO TO 901

C  OPERATION 11: Modify the status of a control.

  601 IF (DDIN(2).NE.1) GO TO 902
      DDAKX = MIN ( DDABD(1,2), MAX(0,DDAKX-5) )
      DDAKY = MIN ( DDABD(2,2), MAX(0,DDAKY-7) )
      GO TO 901

C  OPERATION 12: Enable or disable a control.

  651 IF ((DDIN(3).NE.1).AND.(DDIN(3).NE.5)) GO TO 902
      GO TO 901

C  OPERATION 13: Obtain an event from the graphic device.

  701 DDXBN=0
      CALL UGTX02(DDAKX,DDAKY,0)
      DDXBF(DDXBN+1:DDXBN+1) = TERM
      DDXBN                  = DDXBN+1
      CALL UGTX05 ( 0, DDXBF(1:DDXBN) )
      CALL UGTX10 ( DDXIO, DDIN(2), DDXBF, DDXBN, INT1 )
      IF ( INT1.EQ.-1 ) GO TO 901
      IF ( DDABC(1).EQ.0 ) GO TO 701
      INT1    = MIN(DDAZ2,INT1)
      DDEX(1) = 1
      DDEX(2) = INT1
      IF (INT1.GT.0) THEN
        DDST(1:INT1) = DDXBF(1:INT1)
        IF (DDAKF.EQ.0) CALL UGTX05(2,DDST(1:INT1))
      ENDIF
      GO TO 903

C  OPERATION 14: Sample an interactive control.

  751 IF (DDIN(2).NE.5) GO TO 902
  752 DDXBF(1:2)=CRSH
      CALL UGTX05(0,DDXBF(1:2))
      CALL UGTX10(DDXIO,-1,DDXBF,2,INT1)
      IF (INT1.NE.5) GO TO 752
      DDEX(1)=5
      CALL UGTX05(1,DDXBF(1:5))
      DDEX(2)=32*MOD(ICHAR(DDXBF(2:2)),32)+MOD(ICHAR(DDXBF(3:3)),32)
      DDEX(3)=32*MOD(ICHAR(DDXBF(4:4)),32)+MOD(ICHAR(DDXBF(5:5)),32)
      GO TO 903

C  Set error indicator and return to caller.

  901 DDEX(1)=0
      GO TO 903

  902 DDEX(1)=1
  903 RETURN

      END

      SUBROUTINE UGTX02(XCRD,YCRD,BBIT)

*******************  THE UNIFIED GRAPHICS SYSTEM  *******************
*                                                                   *
*  This subroutine is used to generate orders to move the beam to   *
*  a new position.                                                  *
*                                                                   *
*  The calling sequence is:                                         *
*                                                                   *
*    CALL UGTX02(XCRD,YCRD,BBIT)                                    *
*                                                                   *
*  The parameters in the calling sequence are:                      *
*                                                                   *
*    XCRD  x coordinate of the new position.                        *
*    YCRD  y coordinate of the new position.                        *
*    BBIT  blanking bit.                                            *
*                                                                   *
*********************************************************************

      INTEGER       XCRD,YCRD,BBIT

      INCLUDE       'UGSYSTEM:UGDDXTIN.FOR'

      INTEGER       MYHI,MYLO,MXHI,MXLO
      INTEGER       CREQ

      DATA          MYHI / 32 / ! /Z00000020/
      DATA          MYLO / 96 / ! /Z00000060/
      DATA          MXHI / 32 / ! /Z00000020/
      DATA          MXLO / 64 / ! /Z00000040/

C  Compute requirements and write out current record if necessary.

      IF (BBIT.EQ.0) THEN
          CREQ = 10
      ELSE
          CREQ =  5
      END IF

      IF ( CREQ .GT. (DDXZ1-DDXBN) ) THEN
          CALL UGTX04(1)
          IF (BBIT.NE.0) THEN
              DDXBF(DDXBN+1:DDXBN+1) = CHAR(29)
              DDXBF(DDXBN+2:DDXBN+5) = DDXOP
              DDXBN=DDXBN+5
          END IF
      END IF

C  Create the new point and add it to the current record.

      DDXOP(1:1) = CHAR( MOD(YCRD/32,32)+MYHI )
      DDXOP(2:2) = CHAR( MOD(YCRD   ,32)+MYLO )
      DDXOP(3:3) = CHAR( MOD(XCRD/32,32)+MXHI )
      DDXOP(4:4) = CHAR( MOD(XCRD   ,32)+MXLO )
      IF (BBIT.EQ.0) THEN
        DDXBF(DDXBN+1:DDXBN+1) = CHAR(29)
        DDXBN                  = DDXBN+1
      END IF
      DDXBF(DDXBN+1:DDXBN+4) = DDXOP
      DDXBN = DDXBN+4

      RETURN
      END

      SUBROUTINE UGTX03(XCRD,YCRD,TEXT)

*******************  THE UNIFIED GRAPHICS SYSTEM  *******************
*                                                                   *
*  This subroutine is used to generate orders to display            *
*  characters at a specific position.                               *
*                                                                   *
*  The calling sequence is:                                         *
*                                                                   *
*    CALL UGTX03 ( XCRD , YCRD , TEXT )                             *
*                                                                   *
*  The parameters in the calling sequence are:                      *
*                                                                   *
*    XCRD  x coordinate of the first character.                     *
*    YCRD  y coordinate of the first character.                     *
*    TEXT  character string.                                        *
*                                                                   *
*********************************************************************

      INTEGER       XCRD,YCRD
      CHARACTER*(*) TEXT

      INCLUDE       'UGSYSTEM:UGDDXTIN.FOR'

      INTEGER       NTXT
      CHARACTER*80  ITXT
      INTEGER       CRDX
      INTEGER       INSI,INSJ,INSN

C  Translate the characters and initialize the program.

      NTXT         = LEN(TEXT)
      ITXT(1:NTXT) = TEXT
      CALL UGTX05 ( 1, ITXT(1:NTXT) )
      CRDX         = XCRD
      INSI         = 1
      INSN         = NTXT

C  Write out the current record if necessary.

      IF (11.GT.(DDXZ1-DDXBN)) CALL UGTX04(1)

C  Insert the text into the record.

  101 CALL UGTX02(CRDX,YCRD,0)
      DDXBF(DDXBN+1:DDXBN+1)      = CHAR(31)
      INSJ                        = MIN(DDXZ1-DDXBN-2,INSN)
      DDXBF(DDXBN+2:DDXBN+INSJ+1) = ITXT(INSI:INSI+INSJ-1)
      DDXBN                       = DDXBN+INSJ+1
      IF (INSJ.NE.INSN) THEN
          CRDX = CRDX + 14*INSJ
          INSI = INSI+INSJ
          INSN = INSN-INSJ
          CALL UGTX04 (1)
          GO TO 101
      END IF

      RETURN
      END

      SUBROUTINE UGTX04(FLAG)

*******************  THE UNIFIED GRAPHICS SYSTEM  *******************
*                                                                   *
*  This subroutine is used to complete the current record and       *
*  write it out.  a terminal byte may optionally be added to the    *
*  record.                                                          *
*                                                                   *
*  The calling sequence is:                                         *
*    CALL UGTX04(FLAG)                                              *
*                                                                   *
*  The parameter in the calling sequence is:                        *
*    FLAG  terminal byte flag                                       *
*        0 - do not add terminal byte                               *
*        1 - add terminal byte                                      *
*                                                                   *
*********************************************************************

      INTEGER       FLAG

      INCLUDE       'UGSYSTEM:UGDDXTIN.FOR'

C  Process the record and write it out.

      IF (FLAG.NE.0) THEN
          DDXBF(DDXBN+1:DDXBN+1) = CHAR(29)
          DDXBN                  = DDXBN + 1
      END IF

      CALL UGTX05 ( 0, DDXBF(1:DDXBN) )
      CALL UGTX09 (DDXIO, DDXBF, DDXBN )
      DDXBN = 0

      RETURN
      END

      SUBROUTINE UGTX05(FLAG,STRG)
 
*******************  THE UNIFIED GRAPHICS SYSTEM  *******************
*                                                                   *
*  This subroutine is used to translate character strings from the  *
*  terminal character set to the computer character set, from the   *
*  computer character set to the terminal character set, or to      *
*  force upper case letters in the computer character set.          *
*                                                                   *
*  The calling sequence is:                                         *
*    CALL UGTX05(FLAG,STRG)                                         *
*                                                                   *
*  The parameters in the calling sequence are:                      *
*    FLAG  Translate flag                                           *
*        0 - terminal to computer                                   *
*        1 - computer to terminal                                   *
*        2 - force upper case                                       *
*    STRG  The character string to be translated.                   *
*                                                                   *
*********************************************************************

      INTEGER       FLAG
      CHARACTER*(*) STRG

      INTEGER       INT1,INT2
      CHARACTER*1   CHR1

C  Translate the character string as needed.

      IF (FLAG.EQ.1) THEN
          DO 101 INT1=1,LEN(STRG)
              CHR1=STRG(INT1:INT1)
              IF ((CHR1.LT.' ').OR.(CHR1.GT.'~')) STRG(INT1:INT1)='@'
  101     CONTINUE

      ELSE IF (FLAG.EQ.2) THEN
          DO 102 INT1=1,LEN(STRG)
              INT2=ICHAR(STRG(INT1:INT1))
              IF ((INT2.GE.ICHAR('a')).AND.(INT2.LE.ICHAR('z')))
     +          STRG(INT1:INT1)=CHAR(INT2-ICHAR('a')+ICHAR('A'))
  102     CONTINUE
      END IF

      RETURN
      END

      SUBROUTINE UGTX06(TIME)

*******************  THE UNIFIED GRAPHICS SYSTEM  *******************
*                                                                   *
*  This subroutine is used to put the program into the wait state   *
*  for a specified length of time.                                  *
*                                                                   *
*  The calling sequence is:                                         *
*    CALL UGTX06(TIME)                                              *
*                                                                   *
*  The parameter in the calling sequence is:                        *
*    TIME  delay time in hundredths of a second                     *
*                                                                   *
*********************************************************************

      INTEGER       TIME

      RETURN
      END

      SUBROUTINE UGTX07(NAME,PIOD,FLAG)

*******************  THE UNIFIED GRAPHICS SYSTEM  *******************
*                                                                   *
*  THESE SUBROUTINES ARE THE ACTUAL INPUT/OUTPUT MODULES.           *
*                                                                   *
*  THE CALLING SEQUENCES ARE:                                       *
*    open    CALL UGTX07(NAME,PIOD,FLAG)                            *
*    close   CALL UGTX08(PIOD)                                      *
*    write   CALL UGTX09(PIOD,STRG,WCNT)                            *
*    read    CALL UGTX10(PIOD,TIME,STRG,WCNT,RCNT)                  *
*                                                                   *
*  THE PARAMETERS IN THE CALLING SEQUENCES ARE:                     *
*                                                                   *
*    NAME  The name of the input/output device.   The character     *
*          string should be 64 characters long.                     *
*    PIOD  The channel identification.  This item is generated by   *
*          the open function and must be supplied to the other      *
*          functions.                                               *
*    FLAG  A success flag (0 means success, 1 means error).         *
*    STRG  The input/output character string.                       *
*    WCNT  The write count for the characters in strg.              *
*    TIME  The delay time in hundredths of a second.  a value of    *
*          -1 indicates no time-out is required.                    *
*    RCNT  The read count for the characters in strg.  a -1 value   *
*          indicates a time-out has occurred.                       *
*                                                                   *
*********************************************************************

      CHARACTER*64  NAME
      INTEGER       PIOD,FLAG
      CHARACTER*(*) STRG
      INTEGER       TIME,WCNT,RCNT

      INCLUDE       'UGSYSTEM:UGDDXTIN.FOR'
C     INCLUDE       'UGSYSTEM:UGIOPARM.FOR'

C  Perform the OPEN function.

      PIOD = 77

      OPEN (
     +    UNIT = PIOD
     +  , FILE   = NAME
     +  , FORM   = 'FORMATTED'
     +  , STATUS = 'UNKNOWN'
     +  , IOSTAT = FLAG
     +   )
      IF (FLAG.NE.0) FLAG = 1
      RETURN

C  Perform the CLOSE function.

      ENTRY UGTX08 ( PIOD )
      CLOSE ( PIOD )
      RETURN

C  Perform the WRITE function.

      ENTRY UGTX09 ( PIOD , STRG, WCNT )

**      WRITE ( PIOD , '(A,$)' ) STRG(:WCNT)
      WRITE ( PIOD , '(A)' ) STRG(:WCNT)

      RETURN

C  Perform the READ function.

      ENTRY UGTX10(PIOD,TIME,STRG,WCNT,RCNT)

      RETURN
      END
