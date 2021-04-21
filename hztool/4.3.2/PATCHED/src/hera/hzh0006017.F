      subroutine HZH0006017(IFLAG)
*
**********************************************************************
* Purpose: histograms for inclusive Photoproduction of Neutral Pions 
*          in the Photon Hemisphere at HERA, DESY-00-085, hep-ex/0006017 
*          (H1 paper)
* 
*          Figures 2, 3 and 6 are implemented.
*          An example PAW macro k_hz00085 is available.
*
* Authors: Daniel Beneckenstein (Heidelberg "Miniresearch" student)
*          Victor Lendermann (supervisor)      Created at: 31.03.2005
*
* Arguments: iflag=1000*[IPROC]+1 initialisation
*            iflag=1000*[IPROC]+2 filling
*            iflag=1000*[IPROC]+3 termination
*
* IPROC = 1 direct photoproduction
* IPROC = 2 resolved photoproduction
* IPROC = 0 both together
*
* Photoproduction Event selection:
*	 Q2 < 0.01 GeV^2
*	 0.35 < y_Bjorken < 0.65
*
* pi0 selection: -3.5 < y < 1.0 (rapidity)
*                0.2 < pt < 2.0 GeV/c
*
* Beams: 27.5 GeV positrons, 820 GeV protons  [1996/97 HERA running]
*
* Output Histograms:
*
*--------------------------------------------------------------------
*                             Data Plots
*  -101 ...  -311 Data Plots with tot. errors,
*   ID-10    fine binning to plot shifted bin centers where necessary
* -1101 ... -1311 Data Plots with stat. errors
*   ID-10    fine binning to plot shifted bin centers where necessary
*
* Either ID-10 should be plotted, or, if horizontal bars are desired,
* ID should be plotted and overlayed by ID-10. For IDs -10x it is not
* necessary, since geometrical bin centers are used.
* See the example PAW macro k_hz00085.
*--------------------------------------------------------------------
* Histo-ID   Description
*--------------------------------------------------------------------
* -101       d^2s/dpt^2dy [mub/(GeV/c)^2]  0.2 < pt < 0.6 Gev/c
* -102       d^2s/dpt^2dy [mub/(GeV/c)^2]  0.6 < pt < 0.8 Gev/c
* -103       d^2s/dpt^2dy [mub/(GeV/c)^2]  0.8 < pt < 1.0 Gev/c
* -104       d^2s/dpt^2dy [mub/(GeV/c)^2]  1.0 < pt < 2.0 Gev/c
* -201,-211  d^2s/dpt^2dy [mub/(GeV/c)^2]  -3.5 < y < -2.8
* -202,-212  d^2s/dpt^2dy [mub/(GeV/c)^2]  -2.8 < y < -2.4
* -203,-213  d^2s/dpt^2dy [mub/(GeV/c)^2]  -2.4 < y < -2.0
* -204,-214  d^2s/dpt^2dy [mub/(GeV/c)^2]  -2.0 < y < -1.5
* -205,-215  d^2s/dpt^2dy [mub/(GeV/c)^2]  -0.5 < y <  1.0 
* -301,-311  ds/dxf [mub]
*--------------------------------------------------------------------
*                         Monte Carlo Histograms
* ID+10 have a factor fc (def=10) finer bins to produce "smooth" curves
*--------------------------------------------------------------------
* 101,111    d^2s/dpt^2dy [mub/(GeV/c)^2]  0.2 < pt < 0.6 Gev/c
* 102,112    d^2s/dpt^2dy [mub/(GeV/c)^2]  0.6 < pt < 0.8 Gev/c
* 103,113    d^2s/dpt^2dy [mub/(GeV/c)^2]  0.8 < pt < 1.0 Gev/c
* 104,114    d^2s/dpt^2dy [mub/(GeV/c)^2]  1.0 < pt < 2.0 Gev/c
* 201,211    d^2s/dpt^2dy [mub/(GeV/c)^2]  -3.5 < y < -2.8
* 202,212    d^2s/dpt^2dy [mub/(GeV/c)^2]  -2.8 < y < -2.4
* 203,213    d^2s/dpt^2dy [mub/(GeV/c)^2]  -2.4 < y < -2.0
* 204,214    d^2s/dpt^2dy [mub/(GeV/c)^2]  -2.0 < y < -1.5
* 205,215    d^2s/dpt^2dy [mub/(GeV/c)^2]  -0.5 < y <  1.0 
* 301,311    ds/dxf [mub]
*--------------------------------------------------------------------
*********************************************************************
      IMPLICIT NONE
*
* Function declarations for Hztool functions
#include "hepevtp.inc"
#include "heracmn.inc"
#include "hzfunc.inc"
#include "hzhbook.inc"
*
      Integer IFLAG, IPROC
      Character*8 xxxx
      Integer NENTRY
      Data xxxx /'H0006017'/, NENTRY /0/
      Save xxxx
*
* define bins for histograms
*
* x-section as a function of rapidity y with different values of pt
      Integer ybn
      Parameter (ybn=5)
      Real ybins(ybn+2), ybinsw(ybn)
      Save ybinsw
      Integer ybh(ybn)
      Data ybins /-3.5, -2.8, -2.4, -2.0, -1.5, -0.5, 1.0/
      Data ybh /1,2,3,4,6/
*
* x-section as a function of pt with different values of rapidity y
      Integer ptbn, ptbnf
      Parameter (ptbn=4, ptbnf=180)
      Real ptbins(ptbn+1), ptbinsw(ptbn)
      Save ptbinsw
      Data ptbins /0.2, 0.6, 0.8, 1.0, 2.0/
*
* x-section as a function of Feynman x
      Integer xbn, xbnf
      Parameter (xbn=6, xbnf=500)
      Real xbins(xbn+1), xbinsw(xbn)
      Save xbinsw
      Data xbins /0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.6/
*
* photon flux factor
*
      Real pf
      Parameter (pf=0.00968)
*
* bins of above defined histograms are a factor fc finer
*
      Integer fc
      Parameter (fc=10)
*
* define variables
*
      real Nevt, Npass, lum, lummb, wfactor
      save Nevt, Npass
      double precision q2, yb, pl, pt, sep, spp, plmax, xf, rap
      double precision pbeam(4), pgam(4), ph(5), ppi0(4), pnew(4)
      integer i, ini, inj, ipt, irap, ipbeam, ilbeam, ierr
*
* define cuts for Q2, y_Bjorken and rapidity
*
      double precision q2hi
      parameter (q2hi=0.01D0)
*
      double precision ylow,yhigh
      parameter (ylow=0.35)
      parameter (yhigh=0.65)
*
      double precision raplow,raphigh
      parameter (raplow=-3.5)
      parameter (raphigh=-1.5)
*
* for cs as function of pt and y (fig2,3)
*
      real cs1(ptbn,ybn),cs1ste(ptbn,ybn)
      real cs1sye(ptbn,ybn),cs1te(ptbn,ybn),yav(ybn),ptav(ptbn)
      real arh1(ybn+1)
      real arh2(ptbn),arh2e(ptbn),arh2s(ptbn)
* 
* for cs as function of xf (fig6)
*
      real xfav(xbn),cs2(xbn),cs2ste(xbn),cs2sye(xbn),cs2te(xbn)
      double precision  zero3(xbn),arh1e(ybn+1),arh1s(ybn+1)
      double precision  zero2(ptbn)
*
* data for figures 2 and 3 [d^2s/dpt^2dy]
*
      data yav /-3.15,-2.6,-2.2,-1.75,0.25/
      data ptav /0.36,0.69,0.89,1.31/
*
      data cs1 /47.,5.,0.,0.,94.,12.4,3.6,0.23,158.,27.1,8.,0.68,
     & 176.,28.3,12.9,1.34,0.,0.,16.9,1.97/
      data cs1ste /3.,0.4,0.,0.,6.,0.8,0.1,0.01,9.,1.6,0.1,0.02,
     & 9.,2.2,0.4,0.04,0.,0.,1.2,0.15/
      data cs1sye /10.,1.1,0.,0.,15.,2.1,0.8,0.08,25.,4.4,2.0,0.17,
     & 25.,4.3,3.4,0.32,0.,0.,3.3,0.32/
*
* data for figure 6 [ds/dxf]
*
      data xfav /0.125,0.175,0.225,0.275,0.348,0.493/
*
      data cs2 /477.,279.,209.,131.,65.,19./
      data cs2ste /33.,13.,13.,7.,2.,1./
      data cs2sye /58.,41.,32.,18.,12.,5./
*
*********************************************************************
*
*                      Initialization
*
*********************************************************************
*

*     Determine the process type.
      iproc=MOD(iflag,10000)/1000
      if ((iproc.lt.0).or.(iproc.gt.2)) then
         write(hzunit,*) 'HZ',xxxx,': illegal IPROC:',iproc
         return
      endif

      NENTRY=NENTRY+1
*
      IF (MOD(iflag,10).eq.1) THEN
*
         Nevt  = 0.
         Npass = 0.
*
* Initialisation: The following MUST always be done
* (i) make subdirectory in PAWC
* - use the name as the xxxxx in HZxxxxx subroutine
* (i) make subdirectory in o/p file
*
         call hcdir('//PAWC',' ')
         if (iproc.le.1) then
            call hmdir(xxxx,'S')
         else
            call hcdir(xxxx,' ')  
         endif
            
         call hcdir('//HISTO',' ')
         if (iproc.le.1) then 
            call hmdir(xxxx,'S')
         else
            call hcdir(xxxx,' ')  
         endif
*
* book histograms
*
c        ybinswf  = (ybins(ybh(ybn)+1) - ybins(1)) / (fc * (ybn+1))
c        ptbinswf = (ptbins(ptbn+1) - ptbins(1)) / (fc * ptbn)
c        xbinswf  = (xbins(xbn+1) - xbins(1)) / (fc * xbn)
*
         CALL HBOOKB(1000*iproc+101,'Fig2a',ybn+1,ybins,0.)
         CALL HBOOKB(1000*iproc+102,'Fig2b',ybn+1,ybins,0.)
         CALL HBOOKB(1000*iproc+103,'Fig2c',ybn+1,ybins,0.)
         CALL HBOOKB(1000*iproc+104,'Fig2d',ybn+1,ybins,0.)
         CALL HBOOKB(1000*iproc+201,'Fig3a',ptbn,ptbins,0.)
         CALL HBOOKB(1000*iproc+202,'Fig3b',ptbn,ptbins,0.)
         CALL HBOOKB(1000*iproc+203,'Fig3c',ptbn,ptbins,0.)
         CALL HBOOKB(1000*iproc+204,'Fig3d',ptbn,ptbins,0.)
         CALL HBOOKB(1000*iproc+205,'Fig3e',ptbn,ptbins,0.)
         CALL HBOOKB(1000*iproc+301,'Fig6',xbn,xbins,0.)
*
         CALL HBOOK1(1000*iproc+111,'Fig2a_f',(ybn+1)*fc,ybins(1)
     $        ,ybins(ybn+2),0.)
         CALL HBOOK1(1000*iproc+112,'Fig2b_f',(ybn+1)*fc,ybins(1)
     $        ,ybins(ybn+2),0.)
         CALL HBOOK1(1000*iproc+113,'Fig2c_f',(ybn+1)*fc,ybins(1)
     $        ,ybins(ybn+2),0.)
         CALL HBOOK1(1000*iproc+114,'Fig2d_f',(ybn+1)*fc,ybins(1)
     $        ,ybins(ybn+2),0.)
         CALL HBOOK1(1000*iproc+211,'Fig3a_f',ptbn*fc,ptbins(1)
     $        ,ptbins(ptbn+1),0.)
         CALL HBOOK1(1000*iproc+212,'Fig3b_f',ptbn*fc,ptbins(1)
     $        ,ptbins(ptbn+1),0.)
         CALL HBOOK1(1000*iproc+213,'Fig3c_f',ptbn*fc,ptbins(1)
     $        ,ptbins(ptbn+1),0.)
         CALL HBOOK1(1000*iproc+214,'Fig3d_f',ptbn*fc,ptbins(1)
     $        ,ptbins(ptbn+1),0.)
         CALL HBOOK1(1000*iproc+215,'Fig3e_f',ptbn*fc,ptbins(1)
     $        ,ptbins(ptbn+1),0.)
         CALL HBOOK1(1000*iproc+311,'Fig6_f',xbn*fc,xbins(1),xbins(xbn+1
     $        ),0.)
*
         if (iproc.le.1) then

            do ini=1,ybn
               ybinsw(ini)  = ybins(ybh(ini)+1) - ybins(ybh(ini))
            enddo
            do ini=1,ptbn
               ptbinsw(ini) = ptbins(ini+1) - ptbins(ini)
            enddo
            do ini=1,xbn
               xbinsw(ini)  = xbins(ini+1) - xbins(ini)
            enddo
            do ini=1,ptbn
               do inj=1,ybn
                  cs1te(ini,inj) = sqrt(cs1ste(ini,inj)**2+
     &                 cs1sye(ini,inj)**2)
               enddo
            enddo
            do ini=1,xbn
               cs2te(ini) = sqrt(cs2ste(ini)**2 + cs2sye(ini)**2)
            enddo

            CALL HCOPY(1000*iproc+101,-101,' ')
            CALL HCOPY(1000*iproc+102,-102,' ')
            CALL HCOPY(1000*iproc+103,-103,' ')
            CALL HCOPY(1000*iproc+104,-104,' ')
            CALL HCOPY(1000*iproc+201,-201,' ')
            CALL HCOPY(1000*iproc+202,-202,' ')
            CALL HCOPY(1000*iproc+203,-203,' ')
            CALL HCOPY(1000*iproc+204,-204,' ')
            CALL HCOPY(1000*iproc+205,-205,' ')
            CALL HCOPY(1000*iproc+301,-301,' ')
*     
            CALL HCOPY(1000*iproc+101,-1101,' ')
            CALL HCOPY(1000*iproc+102,-1102,' ')
            CALL HCOPY(1000*iproc+103,-1103,' ')
            CALL HCOPY(1000*iproc+104,-1104,' ')
c        CALL HCOPY(201,-1201,' ')
c        CALL HCOPY(202,-1202,' ')
c        CALL HCOPY(203,-1203,' ')
c        CALL HCOPY(204,-1204,' ')
c        CALL HCOPY(205,-1205,' ')
c        CALL HCOPY(301,-1301,' ')
*
            CALL HBOOK1(-211,'Fig3a_f',ptbnf,ptbins(1),ptbins(ptbn+1),0.
     $           )
            CALL HBOOK1(-212,'Fig3b_f',ptbnf,ptbins(1),ptbins(ptbn+1),0.
     $           )
            CALL HBOOK1(-213,'Fig3c_f',ptbnf,ptbins(1),ptbins(ptbn+1),0.
     $           )
            CALL HBOOK1(-214,'Fig3d_f',ptbnf,ptbins(1),ptbins(ptbn+1),0.
     $           )
            CALL HBOOK1(-215,'Fig3e_f',ptbnf,ptbins(1),ptbins(ptbn+1),0.
     $           )
            CALL HBOOK1(-311,'Fig6_f',xbnf,xbins(1),xbins(xbn+1),0.)
            CALL HCOPY(-211,-1211,' ')
            CALL HCOPY(-212,-1212,' ')
            CALL HCOPY(-213,-1213,' ')
            CALL HCOPY(-214,-1214,' ')
            CALL HCOPY(-215,-1215,' ')
            CALL HCOPY(-311,-1311,' ')
*
            do inj=1,ptbn
               do ini=1,ybn
                  arh1(ybh(ini))  = cs1(inj,ini)
                  arh1e(ybh(ini)) = cs1te(inj,ini)
                  arh1s(ybh(ini)) = cs1ste(inj,ini)
               enddo
               CALL HPAK ( -100-inj,arh1)
               CALL HPAKe( -100-inj,arh1e)
               CALL HPAK (-1100-inj,arh1)
               CALL HPAKe(-1100-inj,arh1s)
            enddo
*
            call VZERO(zero2,ptbn)
            do ini=1,ybn
               do inj=1,ptbn
                  arh2(inj)  = cs1(inj,ini)
                  arh2e(inj) = cs1te(inj,ini)
                  arh2s(inj) = cs1ste(inj,ini)
               enddo
               CALL HPAK ( -200-ini,arh2)
c           CALL HPAKe( -200-ini,arh2e)
               CALL HPAKe( -200-ini,zero2)
c           CALL HPAK (-1200-ini,arh2)
c           CALL HPAKe(-1200-ini,arh2s)
c           CALL HPAKe(-1200-ini,zero2)
            enddo
*
            CALL HBARX( -211)
            CALL HBARX( -212)
            CALL HBARX( -213)
            CALL HBARX( -214)
            CALL HBARX( -215)
            CALL HBARX(-1211)
            CALL HBARX(-1212)
            CALL HBARX(-1213)
            CALL HBARX(-1214)
            CALL HBARX(-1215)
            do ini=1,ptbn
               CALL HF1E( -211,ptav(ini),cs1(ini,1),cs1te(ini,1))
               CALL HF1E( -212,ptav(ini),cs1(ini,2),cs1te(ini,2))
               CALL HF1E( -213,ptav(ini),cs1(ini,3),cs1te(ini,3))
               CALL HF1E( -214,ptav(ini),cs1(ini,4),cs1te(ini,4))
               CALL HF1E( -215,ptav(ini),cs1(ini,5),cs1te(ini,5))
               CALL HF1E(-1211,ptav(ini),cs1(ini,1),cs1ste(ini,1))
               CALL HF1E(-1212,ptav(ini),cs1(ini,2),cs1ste(ini,2))
               CALL HF1E(-1213,ptav(ini),cs1(ini,3),cs1ste(ini,3))
               CALL HF1E(-1214,ptav(ini),cs1(ini,4),cs1ste(ini,4))
               CALL HF1E(-1215,ptav(ini),cs1(ini,5),cs1ste(ini,5))
            enddo
*
            call VZERO(zero3,xbn)
            CALL HPAK ( -301,cs2)
c        CALL HPAKe( -301,cs2te)
            CALL HPAKe( -301,zero3)
c        CALL HPAK (-1301,cs2)
c        CALL HPAKe(-1301,cs2ste)
c        CALL HPAKe(-1301,zero3)
*
            CALL HBARX( -311)
            CALL HBARX(-1311)
            do ini = 1, xbn
               CALL HF1E( -311,xfav(ini),cs2(ini),cs2te(ini))
               CALL HF1E(-1311,xfav(ini),cs2(ini),cs2ste(ini))
            enddo
*
         endif

         CALL HBARX(1000*iproc+101)
         CALL HBARX(1000*iproc+102)
         CALL HBARX(1000*iproc+103)
         CALL HBARX(1000*iproc+104)
         CALL HBARX(1000*iproc+201)
         CALL HBARX(1000*iproc+202)
         CALL HBARX(1000*iproc+203)
         CALL HBARX(1000*iproc+204)
         CALL HBARX(1000*iproc+205)
         CALL HBARX(1000*iproc+301)
*
         CALL HBARX(1000*iproc+111)
         CALL HBARX(1000*iproc+112)
         CALL HBARX(1000*iproc+113)
         CALL HBARX(1000*iproc+114)
         CALL HBARX(1000*iproc+211)
         CALL HBARX(1000*iproc+212)
         CALL HBARX(1000*iproc+213)
         CALL HBARX(1000*iproc+214)
         CALL HBARX(1000*iproc+215)
         CALL HBARX(1000*iproc+311)
*
*********************************************************************
*
*                      Event Processing
*
*********************************************************************
*
      Else if (MOD(iflag,10).eq.2) then
*
* Filling: The following MUST always be done
* (i) move to the correct sub-directory in PAWC
*
         call hcdir('//PAWC/'//xxxx,' ')
*
* count events
*
         Nevt = Nevt + wtx
*
* get kinematics
*
         q2 = HZDISKIN(1)
         yb = HZDISKIN(3)
*
* get electron - cross check
*
c         iel=HZIDELEC(idum)
c         if (iel.eq.-1) then
c            write(*,*) 'HZ',xxxx,': electron not found!'
c            return
c         endif
c         yb=1D0-(PHEP(4,iel)/PHEP(4,2))
*
* select events
*
         if (yb.lt.ylow)  return
         if (yb.gt.yhigh) return
         if (q2.gt.q2hi)  return
c        if (PHEP(4,iel).lt.scele) return
*
* count number of events passing cuts
*
         Npass = Npass + wtx
*
* initialize variables for gamma-proton CM frame
*
         ierr = HZIBEAM(ipbeam,ilbeam)
         if ((ierr.eq.1).or.(ierr.eq.-1)) then
            do ini = 1, 4
               pbeam(ini) = PHEP(ini,ipbeam)
            enddo
         else
            write(hzunit,*) 'HZ'//xxxx,': proton beam not found !!!'
            return
         endif
*
         ierr = HZIPGAM(ph)
         if (ierr.eq.-1) then
            write(hzunit,*) 'HZ'//xxxx,': boson vector not found !!!'
            call VZERO(ph,5)
            call VZERO(pgam,4)
         else
            do ini = 1,4
               pgam(ini) = ph(ini)
            enddo
         endif
         call HZHCMINI(pbeam,pgam,ierr)
*
         sep = 4D0*PHEP(4,1)*PHEP(4,2)
         spp = yb*sep
         plmax = dsqrt(spp) / 2D0
*
* main loop - find pi0s, calculate pt, rap, xf and fill histrograms
*
         do i=1,nhep
            if (iabs(IDHEP(i)).eq.111) then
               pt = dsqrt(PHEP(1,i)**2 + PHEP(2,i)**2)
               rap = 0.5D0 *
     &              dlog((PHEP(4,i)+PHEP(3,i))/(PHEP(4,i)-PHEP(3,i)))
*
               ipt = 0
               do ini=1,ptbn
                  if ( (pt.lt.ptbins(ini+1)).and.
     &                 (pt.ge.ptbins(ini)) ) then
                     ipt = ini
                     goto 100
                  endif
               enddo
 100           continue
*
               irap = 0
               do ini=1,ybn
                  if ( (rap.lt.ybins(ybh(ini)+1)).and.
     &                 (rap.ge.ybins(ybh(ini))) ) then
                     irap = ini
                     goto 200
                  endif
               enddo
 200           continue
*
               if (ipt.ne.0) then
                  wfactor = real(wtx / (2.*pt*ptbinsw(ipt)))
                  call hfill(1000*iproc+100+ipt,real(rap),0.,wfactor)
                  call hfill(1000*iproc+110+ipt,real(rap),0.,wfactor)
               endif
*
               if (irap.ne.0) then
                  wfactor = real(wtx / (2.*pt*ybinsw(irap)))
                  call hfill(1000*iproc+200+irap,real(pt),0.,wfactor)
                  call hfill(1000*iproc+210+irap,real(pt),0.,wfactor)
               endif
*
               if ((rap.lt.raphigh).and.(rap.ge.raplow)) then
                  do ini=1,4
                     ppi0(ini) = PHEP(ini,i)
                  enddo
                  call HZHCM(ppi0,pnew,ierr) ! boost into gamma-p frame
                  xf = dabs(pnew(3)/plmax)
                  wfactor = real(wtx)
                  call hfill(1000*iproc+311,real(xf),0.,wfactor)
                  call hfill(1000*iproc+301,real(xf),0.,wfactor)
               endif
            endif
         enddo
*
*********************************************************************
*
*                      Termination
*
*********************************************************************
*
      Else if (MOD(iflag,10).eq.3) then
*
* Termination: The following MUST always be done
* (i) Move to the correct PAW subdirectory
*
         if (iproc.eq.0.or.iproc.eq.2) then 
            write (*,*) 'HZ'//xxxx/ /': finishing off'
         endif

         call hcdir('//PAWC/'//xxxx,' ')
*
         lum = 999999999.
         if (xsec.gt.0.) then
            lum = Nevt/real(xsec)
            lummb = lum * 1.e3
c            write(hzunit,*) 'HZ',xxxx,' Number of events processed: '
c     $           ,Nevt
c            write(hzunit,*) 'HZ',xxxx,' Number of events after cuts: '
c     $           ,Npass
c            write(hzunit,*) 'HZ',xxxx,' Total cross section (nb): ',xsec
c            write(hzunit,*) 'HZ',xxxx,' Equivalent luminosity (mub-1): '
c     $           ,lummb
         else
            write(hzunit,*) 'HZ',xxxx,': xsec = 0 !!!'
         endif
*
* normalize the histograms on lumi and apply the photon flux factor
*
         do ini=1,ptbn
            call hzhinrm(1000*iproc+100+ini,0,pf*lummb,1)
            call hzhinrm(1000*iproc+110+ini,0,pf*lummb,1)
         enddo
         do ini=1,ybn
            call hzhinrm(1000*iproc+200+ini,0,pf*lummb,1)
            call hzhinrm(1000*iproc+210+ini,0,pf*lummb,1)
         enddo
         call hzhinrm(1000*iproc+301,0,pf*lummb,1)
         call hzhinrm(1000*iproc+311,0,pf*lummb,1)

c     If this is the last of a multi-process run, must add histograms.
         if (iproc.eq.2) then

            do ini=1,ptbn
               call hopera(1100+ini,'+e',2100+ini,100+ini,1.,1.)
               call hopera(1110+ini,'+e',2110+ini,110+ini,1.,1.)
            enddo
            do ini=1,ybn
               call hopera(1200+ini,'+e',2200+ini,200+ini,1.,1.)
               call hopera(1210+ini,'+e',2210+ini,210+ini,1.,1.)
            enddo
            call hopera(1301,'+e',2301,301,1.,1.)
            call hopera(1311,'+e',2311,311,1.,1.)

         endif

      Endif

      RETURN
      END
*
