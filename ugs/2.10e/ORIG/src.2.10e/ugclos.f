      SUBROUTINE UGCLOS(OPTN)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                      CLOSE A GRAPHIC DEVICE                       *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO TERMINATE THE USE OF A GRAPHIC    *
C *  DEVICE.  NO MORE USE CAN BE MADE OF THE GRAPHIC DEVICE UNTIL IT  *
C *  IS RE-OPENED.  EITHER THE ACTIVE DEVICE OR ALL DEVICES MAY BE    *
C *  CLOSED.                                                          *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGCLOS(OPTN)                                              *
C *                                                                   *
C *  THE PARAMETER IN THE CALLING SEQUENCE IS:                        *
C *    OPTN  THE OPTIONS LIST.                                        *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      CHARACTER*(*) OPTN
C
      INCLUDE       'UGSYSTEM:UGMCACBK.FOR'
C
      INCLUDE       'UGSYSTEM:UGDDACBK.FOR'
C
      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'
C
      INTEGER*4     EXST(2),EXAF,EXNC
      EQUIVALENCE   (EXAF,EXST(1)),       (EXNC,EXST(2))
C
      INTEGER       DDIN(3),DDEX(1)
C
      INTEGER       INT1
C
      INTEGER       DESC_NUM
      PARAMETER    (DESC_NUM = 2)
      INTEGER*4     INST(4,DESC_NUM)
      CHARACTER*16  IFLAG(DESC_NUM)
C
      DATA     (INST(I,1),I=1,4) / 1,3,1,1 /, IFLAG(1) /'ALL'/
      DATA     (INST(I,2),I=1,4) / 1,7,2,1 /, IFLAG(2) /'NOCLEAR'/
C
C  INITIALIZE AND SCAN THE OPTIONS LIST.
      EXAF=0
      EXNC=0
      CALL UGOPTION(OPTN,DESC_NUM,INST,IFLAG,EXST,EXSTR)
C
C  CHECK "ALL" AND ACTIVE DEVICE COMBINATIONS.
      IF (EXAF.EQ.0) THEN
        IF (DDAAI.EQ.0) GO TO 401
      ELSE
        IF (DDAAI.EQ.0) GO TO 201
      END IF
C
C  CLEAR THE DEVICE, DISABLE ANY ACTIVE CONTROL UNITS, AND CLOSE THE
C  DEVICE.
  101 IF ((EXNC.EQ.0).OR.(DDAIL.EQ.1)) THEN
        DDIN(1)=3
        DDIN(2)=0
        CALL UGZ006(DDAAC,0,0,DDIN,'        ',DDEX)
      END IF
      DO 102 INT1=1,DDAZ1
        IF (DDABC(INT1).NE.0) THEN
          DDIN(1)=12
          DDIN(2)=0
          DDIN(3)=INT1
          CALL UGZ006(DDAAC,0,0,DDIN,' ',DDEX)
        END IF
  102 CONTINUE
      DDIN(1)=2
      DDIN(2)=EXNC
      CALL UGZ006(DDAAC,0,0,DDIN,OPTN,DDEX)
      IF (DDAIL.EQ.3) MCAIC=MCAIC-1
      IF (DDAPX.NE.0) CALL UGZ003(1,DDALX,DDAPX)
      IF (DDAPA.NE.0) CALL UGZ003(1,DDALG,DDAPA)
      CALL UGZ002(1,DDAAN,DDAAC)
      MCAOI(DDAAI)=0
      DDAAI=0
C
C  CHECK "ALL" AND OPEN DEVICE COMBINATIONS.
  201 IF (EXAF.EQ.0) THEN
        DO 202 INT1=1,MCAZ1
          IF (MCAOI(INT1).NE.0) GO TO 301
  202   CONTINUE
      ELSE
        DO 203 INT1=1,MCAZ1
          IF (MCAOI(INT1).NE.0) THEN
            CALL UGB001(DDARY,1,%VAL(MCAOP(INT1)),1,DDALG)
            CALL UGB001(%VAL(DDACX),1,%VAL(DDAPX),1,DDALX)
            GO TO 101
          END IF
  203   CONTINUE
      END IF
C
C  ALL DEVICES ARE CLOSED: DELETE THE CHARACTER STROKE GENERATOR.
      MCACN = ' '
C
C  RESET ERROR INDICATORS AND RETURN TO CALLER.
  301 UGELV=0
      UGENM='        '
      UGEIX=0
  302 RETURN
C
C  REPORT ERRORS TO THE UNIFIED GRAPHICS SYSTEM ERROR PROCESSOR.
  401 CALL UGRERR(3,'UGCLOS  ',12)
      GO TO 302
C
      END
