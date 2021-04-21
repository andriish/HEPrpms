      SUBROUTINE UGSLCT(OPTN,IDNT)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *               MAKE AN OPEN DEVICE THE ACTIVE DEVICE               *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO SELECT AN OPEN GRAPHIC DEVICE     *
C *  AND MAKE IT THE ACTIVE DEVICE.                                   *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGSLCT(OPTN,IDNT)                                         *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    OPTN  THE OPTIONS LIST.                                        *
C *    IDNT  THE IDENTIFICATION OF THE GRAPHIC DEVICE THAT IS TO      *
C *          BECOME THE ACTIVE GRAPHIC DEVICE.                        *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************

      CHARACTER*(*) OPTN
      INTEGER       IDNT

      INCLUDE       'UGSYSTEM:UGMCACBK.FOR'
      INCLUDE       'UGSYSTEM:UGDDACBK.FOR'
      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'

      INTEGER       INDX

      INTEGER       INT1

C  INITIALIZE AND CHECK THE GIVEN IDENTIFICATION.
      IF (IDNT.EQ.0) GO TO 401
      IF (DDAAI.NE.0) THEN
        IF (IDNT.EQ.MCAOI(DDAAI)) GO TO 301
      END IF
      DO 101 INT1=1,MCAZ1
        IF (IDNT.EQ.MCAOI(INT1)) THEN
          INDX=INT1
          GO TO 201
        END IF
  101 CONTINUE
      GO TO 401
C
C  IF A DEVICE IS ACTIVE, MAKE IT INACTIVE.
  201 IF (DDAAI.NE.0) THEN
        IF (DDAPX.EQ.0) CALL UGZ003(0,DDALX,DDAPX)
        CALL UGB001(%VAL(DDAPX),1,%VAL(DDACX),1,DDALX)
        IF (DDAPA.EQ.0) THEN
          CALL UGZ003(0,DDALG,DDAPA)
          MCAOP(DDAAI)=DDAPA
        END IF
        CALL UGB001(%VAL(DDAPA),1,DDARY,1,DDALG)
        DDAAI=0
      END IF
C
C  MAKE THE SELECTED DEVICE ACTIVE.
      CALL UGB001(DDARY,1,%VAL(MCAOP(INDX)),1,DDALG)
      CALL UGB001(%VAL(DDACX),1,%VAL(DDAPX),1,DDALX)
C
C  RESET ERROR INDICATORS AND RETURN TO CALLER.
  301 UGELV=0
      UGENM='        '
      UGEIX=0
  302 RETURN
C
C  REPORT ERRORS TO THE UNIFIED GRAPHICS SYSTEM ERROR PROCESSOR.
  401 CALL UGRERR(3,'UGSLCT  ', 1)
      GO TO 302
C
      END
