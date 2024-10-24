      PROGRAM TESTAR

      CALL ARTEST(0)

      END

      SUBROUTINE DUMMY9
C...This is a dummy routine to ensure the relevant block data routines
C...are linked from the JETSET/PYTHIA library. This routine should never
C...be called.
      EXTERNAL PYDATA
      EXTERNAL LUDATA
C      CALL LUDATA
C      CALL PYDATA

      RETURN
      END
