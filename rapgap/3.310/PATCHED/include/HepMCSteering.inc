C...HepMC
      character *4096 hmfilnam
      Integer hmfilfor
        Common/steerhm/hmfilnam,hmfilfor
      LOGICAL         hmOUT
      COMMON /QhmLO/ hmOUT
C-----------------------
C...Rivet
      character *132 rvtfilnam
      character *132 rvtrun
      character *132 rvtana
      Integer Nrvtana
      Common/steerrvt/Nrvtana,rvtfilnam,rvtrun,rvtana(50)
      LOGICAL         rvtOUT
      COMMON /QrvtLO/ rvtOUT
