C-----------------------
C...HepMC2
      character *132 hm2filnam
        Common/steerhm2/hm2filnam
      LOGICAL         hm2OUT
      COMMON /Qhm2LO/ hm2OUT
C-----------------------
C...HepMC3
      character *132 hm3filnam
      Integer hm3filfor
        Common/steerhm3/hm3filnam,hm3filfor
      LOGICAL         hm3OUT
      COMMON /Qhm3LO/ hm3OUT
C-----------------------
C...Rivet
      character *132 rvtfilnam
      character *132 rvtrun
      character *132 rvtana
      Integer Nrvtana
      Common/steerrvt/Nrvtana,rvtfilnam,rvtrun,rvtana(50)
      LOGICAL         rvtOUT
      COMMON /QrvtLO/ rvtOUT
