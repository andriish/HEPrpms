!******************************************************************************!
! Copyright (C) 2014-2024 OpenLoops Collaboration. For authors see authors.txt !
!                                                                              !
! This file is part of OpenLoops.                                              !
!                                                                              !
! OpenLoops is free software: you can redistribute it and/or modify            !
! it under the terms of the GNU General Public License as published by         !
! the Free Software Foundation, either version 3 of the License, or            !
! (at your option) any later version.                                          !
!                                                                              !
! OpenLoops is distributed in the hope that it will be useful,                 !
! but WITHOUT ANY WARRANTY; without even the implied warranty of               !
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                !
! GNU General Public License for more details.                                 !
!                                                                              !
! You should have received a copy of the GNU General Public License            !
! along with OpenLoops.  If not, see <http://www.gnu.org/licenses/>.           !
!******************************************************************************!


module ofred_basis_construction_/**/REALKIND

  use KIND_TYPES, only: REALKIND

  ! basis_selection (_4/_5) (criterion for the choice of the basis):
  !  - 0: the basis is built out of the first two momenta p1,p2
  !  - 1: gamma                    is maximized
  !  - 2: Delta (Gram-Determinant) is maximized
  integer, save :: basis_selection_4 = 2
  integer, save :: basis_selection_5 = 2

  ! norm_mode_gd2 (normalization in the choice of the basis):
  !  - 1: gamma or Delta are normalized to 1
  !  - 2: gamma or Delta are normalized to X, where X is the largest entry of the Gram Matrix.
  !       gamma/X, Delta/X^2
  integer, save :: norm_mode_gd2 = 1

  ! norm_mode_gd3 (criterion for for the choice of basis for 5pt reduction):
  !  - 0: Delta3 (Rank3 Gram-Determinant) is maximized
  !  - 1: ...
  !  - 2: ..., inlcuding ...
  integer, save :: norm_mode_gd3 = 2

  ! Include \gamma/p_3 l_{3/4} terms in error estimator for open loops
  logical, save :: gdm2_err_estim_OL = .true.
  logical, save :: gdm2_err_estim_CL = .false.

  logical, save :: l12_correction_stop_on_failure = .false.

contains

subroutine LC_Contr2Cov(p1ctrv,p1cv)
  use KIND_TYPES, only: REALKIND
  implicit none
  complex(REALKIND), intent(in)  :: p1ctrv(1:4)
  complex(REALKIND), intent(out) :: p1cv(1:4)

  p1cv(1) =   p1ctrv(2)*0.5_/**/REALKIND
  p1cv(2) =   p1ctrv(1)*0.5_/**/REALKIND
  p1cv(3) = - p1ctrv(4)*0.5_/**/REALKIND
  p1cv(4) = - p1ctrv(3)*0.5_/**/REALKIND

end subroutine LC_Contr2Cov

!*************************************************************
subroutine correct_li(li)
!*************************************************************
! One of the components of the lightlike momenta l_i is fixed
! such that the l_i^2 is imposed
!*************************************************************
  use KIND_TYPES, only: REALKIND
  implicit none
  complex(REALKIND), intent(inout)  :: li(1:4)

  if(ABS(li(1)*li(2)) > ABS(li(3)*li(4))) then
    if(ABS(li(1))>ABS(li(2))) then
      li(2) = li(3)*li(4)/li(1)
    else
      li(1) = li(3)*li(4)/li(2)
    end if
  else
    if(ABS(li(3))>ABS(li(4))) then
      li(4) = li(1)*li(2)/li(3)
    else
      li(3) = li(1)*li(2)/li(4)
    end if
  end if

end subroutine correct_li


!************************************************************************
subroutine construct_l1l2_1(mom1,mom2,alpha,gamma,l1,l2,r1,r2)
!-----------------------------------------------------------------------
! Costruction of light-like momenta l_1, l_2. Mode-1
!-----------------------------------------------------------------------
! Details on the l_1, l_2 in arXiv:1710.11452 sect. 5.1 formulas (113-118)
!************************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_kinematics_/**/REALKIND, only: cont_LC_cntrv
  use ol_parameters_decl_/**/REALKIND, only: CI,cone
  use ol_momenta_decl_/**/REALKIND, only: L
  implicit none
  integer, intent(in) :: mom1, mom2
  complex(REALKIND), intent(out) :: gamma, alpha(2)
  complex(REALKIND) :: sqrt_delta, one_sqrt_delta
  complex(REALKIND), intent(out) :: l1(4), l2(4), r1(4), r2(4)
  real(REALKIND) :: p1p2, p1xp2, x1, x2, delta
  real(REALKIND) :: signp1p2, signp1xp2

  p1p2 = REAL(cont_LC_cntrv(L(1:4,mom1),L(1:4,mom2)))
  x1 = REAL(L(5,mom1) + L(6,mom1))/p1p2
  x2 = REAL(L(5,mom2) + L(6,mom2))/p1p2

  ! Construct l1/l2/delta/... for special cases
  ! Case p_{1/2,T} = 0
  if (L(3,mom1) .eq. 0 .and. L(3,mom2) .eq. 0) then
    p1xp2 = REAL(L(1,mom1)*L(2,mom2) - L(2,mom1)*L(1,mom2))
    signp1p2 = sign(1._/**/REALKIND,p1p2)
    signp1xp2 = sign(1._/**/REALKIND,p1xp2)

    if (signp1p2*signp1xp2 .eq. 1) then
      alpha(1) = L(2,mom1)/L(2,mom2)
      alpha(2) = L(1,mom2)/L(1,mom1)
      l1 = L(1:4,mom1) - alpha(1)*L(1:4,mom2)
      l2 = L(1:4,mom2) - alpha(2)*L(1:4,mom1)
      l1(2) = 0._/**/REALKIND                      ! This is important in order to have exact zeros for the cases in the redset construction
      l2(1) = 0._/**/REALKIND
    else
      alpha(1) = L(1,mom1)/L(1,mom2)
      alpha(2) = L(2,mom2)/L(2,mom1)
      l1 = L(1:4,mom1) - alpha(1)*L(1:4,mom2)
      l2 = L(1:4,mom2) - alpha(2)*L(1:4,mom1)
      l1(1) = 0._/**/REALKIND
      l2(2) = 0._/**/REALKIND
    end if

    delta = p1xp2**2/p1p2**2/4
    if (delta > 0) then
      sqrt_delta = sqrt(delta)
    else
      sqrt_delta = - CI*sqrt(-delta)
    end if

    one_sqrt_delta = cone + sqrt_delta

  ! Case (p_1-p_2)^2 = 0
  else if ((L(5,mom2-mom1) + L(6,mom2-mom1)) .eq. 0) then

    delta = cone - x1*x2

    p1xp2 = REAL(cont_LC_cntrv(L(1:4,mom1),L(1:4,mom2-mom1)))
    signp1p2 = sign(1._/**/REALKIND,p1p2)
    signp1xp2 = sign(1._/**/REALKIND,p1xp2)

    if (signp1p2*signp1xp2 .eq. 1) then
      alpha(1) = x1/x2
      alpha(2) = 1
      one_sqrt_delta = x2
      l1 = L(1:4,mom1) - alpha(1)*L(1:4,mom2)
      l2 = L(1:4,mom2-mom1)
    else
      alpha(1) = 1
      alpha(2) = x2/x1
      one_sqrt_delta = x1
      l1 = -L(1:4,mom2-mom1)
      l2 = L(1:4,mom2) - alpha(2)*L(1:4,mom1)
    end if

  ! General case
  else

    delta = cone - x1*x2

    if (delta > 0) then
      sqrt_delta = sqrt(delta)
    else
      sqrt_delta = - CI*sqrt(-delta)
    end if
    one_sqrt_delta = cone + sqrt_delta

    alpha(1) = x1/one_sqrt_delta
    alpha(2) = x2/one_sqrt_delta

    l1 = L(1:4,mom1) - alpha(1)*L(1:4,mom2)
    l2 = L(1:4,mom2) - alpha(2)*L(1:4,mom1)

  end if

  r1 = (L(1:4,mom1) - x1*L(1:4,mom2))/one_sqrt_delta
  r2 = (L(1:4,mom2) - x2*L(1:4,mom1))/one_sqrt_delta

  gamma = 4*p1p2*delta/one_sqrt_delta

end subroutine construct_l1l2_1


!****************************************************************
subroutine construct_l1l2_2(p1,p2,alpha,gamma,l1,l2,r1,r2)
!----------------------------------------------------------------
! Costruction of light-like momenta l_1, l_2. Mode-2
!****************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_kinematics_/**/REALKIND, only: cont_LC_cntrv
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none
  complex(REALKIND), intent(in)  :: p1(1:5), p2(1:5)
  complex(REALKIND), intent(out) :: gamma, alpha(2)
  complex(REALKIND), intent(out) :: l1(1:4), l2(1:4), r1(1:4), r2(1:4)

  real(REALKIND) :: p1p1, p2p2, p1p2, GramDelta, sqrtDelta
  complex(REALKIND) :: l1l1, l2l2

  p1p1 = REAL(p1(5))
  p2p2 = REAL(p2(5))
  p1p2 = REAL(cont_LC_cntrv(p1(1:4),p2(1:4)))

  GramDelta = p1p2**2 - p1p1*p2p2
  sqrtDelta = SQRT(GramDelta)

  alpha(1) = p1p1/(p1p2 + SIGN(sqrtDelta,p1p2))
  alpha(2) = p2p2/(p1p2 + SIGN(sqrtDelta,p1p2))

  l1 = p1(1:4) - alpha(1)*p2(1:4)
  l2 = p2(1:4) - alpha(2)*p1(1:4)

  l1l1 = l1(1)*l1(2) - l1(3)*l1(4)
  l2l2 = l2(1)*l2(2) - l2(3)*l2(4)

  r1 = l1 - alpha(1)*l2
  r2 = l2 - alpha(2)*l1

  gamma = 2*cont_LC_cntrv(l1(1:4),l2(1:4))

end subroutine construct_l1l2_2


!************************************************************************
subroutine construct_l3l4_1(l1,l2,l3,l4)
!------------------------------------------------------------------------
! Costruction of light-like momenta l_3 and l_4. Mode-1
!------------------------------------------------------------------------
! Details on the l_3, l_4 in arXiv:1710.11452 sect. 5.1 formulas (113-118)
!************************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_parameters_decl_/**/REALKIND, only: CI,cone,zero
  implicit none

  complex(REALKIND), intent(in)  :: l1(1:4), l2(1:4)
  complex(REALKIND), intent(out) :: l3(1:4), l4(1:4)
  complex(REALKIND) :: b1, b2, b12, bi12, bi21, c1, c2
  complex(REALKIND) :: OnePlusI, OneMinusI, two

  OnePlusI  = cone + CI
  OneMinusI = cone - CI
  two  = 2*cone

  if (l1(2)==zero .AND. l2(2)==zero) then
    l3 = zero
    l3(1) = two*sqrt((l1(1)+l1(2))*(l2(1)+l2(2)))
    l4 = l3
  else if (l1(1)==zero .AND. l2(1)==zero) then
    l3 = zero
    l3(2) = two*sqrt((l1(1)+l1(2))*(l2(1)+l2(2)))
    l4 = l3
  else if (l1(2)==zero) then
    b1 = sqrt(l2(2))
    b2 = sqrt(l1(1)*two)
    c1 = b2/b1
    c2 = b1*b2
    l3(1) = -(OneMinusI)*l2(3)*c1
    l3(2) = zero
    l3(3) = zero
    l3(4) = -(OneMinusI)*c2
    l4(1) = -(OnePlusI)*l2(4)*c1
    l4(2) = zero
    l4(3) = -(OnePlusI)*c2
    l4(4) = zero
  else if (l1(1)==zero) then
    b1 = sqrt(l2(1))
    b2 = sqrt(l1(2)*two)
    c1 = b2/b1
    c2 = b1*b2
    l3(2) = -(OneMinusI)*l2(3)*c1
    l3(1) = zero
    l3(3) = zero
    l3(4) = -(OneMinusI)*c2
    l4(2) = -(OnePlusI)*l2(4)*c1
    l4(1) = zero
    l4(3) = -(OnePlusI)*c2
    l4(4) = zero
  else if (l2(2)==zero) then
    b1 = sqrt(l1(2))
    b2 = sqrt(l2(1)*two)
    c1 = b2/b1
    c2 = b1*b2
    l3(1) = -(OnePlusI)*l1(4)*c1
    l3(2) = zero
    l3(3) = -(OnePlusI)*c2
    l3(4) = zero
    l4(1) = -(OneMinusI)*l1(3)*c1
    l4(2) = zero
    l4(3) = zero
    l4(4) = -(OneMinusI)*c2
  else if (l2(1)==zero) then
    b1 = sqrt(l1(1))
    b2 = sqrt(l2(2)*two)
    c1 = b2/b1
    c2 = b1*b2
    l3(2) = -(OnePlusI)*l1(4)*c1
    l3(1) = zero
    l3(3) = -(OnePlusI)*c2
    l3(4) = zero
    l4(2) = -(OneMinusI)*l1(3)*c1
    l4(1) = zero
    l4(3) = zero
    l4(4) = -(OneMinusI)*c2
  else
    if (min(abs(l1(2)),abs(l2(2))) .gt. min(abs(l1(1)),abs(l2(1)))) then
      b1 = sqrt(l1(2))
      b2 = sqrt(l2(2))
      b12 = b1*b2
      bi12 = b1/b2
      bi21 = b2/b1
      l3(1) = two*l1(4)*l2(3)/b12
      l3(2) = two*b12
      l3(3) = two*bi12*l2(3)
      l3(4) = two*bi21*l1(4)
      l4(1) = two*l1(3)*l2(4)/b12
      l4(2) = l3(2)
      l4(3) = two*bi21*l1(3)
      l4(4) = two*bi12*l2(4)
    else
      b1 = sqrt(l1(1))
      b2 = sqrt(l2(1))
      b12 = b1*b2
      bi12 = b1/b2
      bi21 = b2/b1
      l3(2) = two*l1(4)*l2(3)/b12
      l3(1) = two*b12
      l3(3) = two*bi12*l2(3)
      l3(4) = two*bi21*l1(4)
      l4(2) = two*l1(3)*l2(4)/b12
      l4(1) = l3(1)
      l4(3) = two*bi21*l1(3)
      l4(4) = two*bi12*l2(4)
    end if
  end if

end subroutine construct_l3l4_1


!*************************************************************
subroutine construct_l3l4_3(l1,l2,l3,l4)
  use KIND_TYPES, only: REALKIND
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none

  complex(REALKIND), intent(in)  :: l1(1:4), l2(1:4)
  complex(REALKIND), intent(out) :: l3(1:4), l4(1:4)
  real(REALKIND) :: COST1, COST2, SINT1, SINT2, COST1HALF, COST2HALF, SINT1HALF, SINT2HALF
  real(REALKIND) :: L_T1sq, L_T2sq, L_T1, L_T2, COST1p1, COST2p1, MODL1, MODL2, MODL1L2
  complex(REALKIND) :: COSP1pSINP1, COSP1mSINP1, COSP2pSINP2, COSP2mSINP2
  complex(REALKIND) :: b1, b2, cp1, cm1, cp2, cm2

  L_T1sq = real(l1(3)*l1(4))
  L_T2sq = real(l2(3)*l2(4))
  L_T1 = sqrt(L_T1sq)
  L_T2 = sqrt(L_T2sq)

  MODL1 = real((l1(1)+l1(2))*0.5_/**/REALKIND)
  MODL2 = real((l2(1)+l2(2))*0.5_/**/REALKIND)

  COST1p1 = real(l1(2)/MODL1)
  COST2p1 = real(l2(2)/MODL2)

  if(L_T1sq==0.0) then
    if (COST1p1>1.0) then
      b1 = sqrt(COST1p1*0.5_/**/REALKIND)
      cp1 = 0._/**/REALKIND
      cm1 = 0._/**/REALKIND
    else
      b1  = 0._/**/REALKIND
      cp1 = (1._/**/REALKIND/sqrt(2._/**/REALKIND))*(1._/**/REALKIND + CI)
      cm1 = (1._/**/REALKIND/sqrt(2._/**/REALKIND))*(1._/**/REALKIND - CI)
    end if
  else
    if (COST1p1>1.) then
      SINT1 = L_T1/MODL1
      COST1HALF = sqrt(COST1p1*0.5_/**/REALKIND)
      SINT1HALF = (SINT1/COST1HALF)*0.5_/**/REALKIND
      COSP1pSINP1 = l1(3)/L_T1
      COSP1mSINP1 = l1(4)/L_T1
      b1  = COST1HALF
      cp1 = SINT1HALF*COSP1pSINP1
      cm1 = SINT1HALF*COSP1mSINP1
    else
      SINT1 = L_T1/MODL1
      SINT1HALF = sqrt((2._/**/REALKIND - COST1p1)*0.5_/**/REALKIND)
      COST1HALF = (SINT1/SINT1HALF)*0.5_/**/REALKIND
      COSP1pSINP1 = l1(3)/L_T1
      COSP1mSINP1 = l1(4)/L_T1
      b1  = COST1HALF
      cp1 = SINT1HALF*COSP1pSINP1
      cm1 = SINT1HALF*COSP1mSINP1
    end if
  end if

  if(L_T2sq==0.0) then
    if (COST2p1>1.0) then
      b2 = sqrt(COST2p1*0.5_/**/REALKIND)
      cp2 = 0._/**/REALKIND
      cm2 = 0._/**/REALKIND
    else
      b2  = 0._/**/REALKIND
      cp2 = (1._/**/REALKIND/sqrt(2._/**/REALKIND))*(1._/**/REALKIND + CI)
      cm2 = (1._/**/REALKIND/sqrt(2._/**/REALKIND))*(1._/**/REALKIND - CI)
    end if
  else
    if (COST2p1>1.) then
      SINT2 = L_T2/MODL2
      COST2HALF = sqrt(COST2p1*0.5_/**/REALKIND)
      SINT2HALF = (SINT2/COST2HALF)*0.5_/**/REALKIND
      COSP2pSINP2 = l2(3)/L_T2
      COSP2mSINP2 = l2(4)/L_T2
      b2  = COST2HALF
      cp2 = SINT2HALF*COSP2pSINP2
      cm2 = SINT2HALF*COSP2mSINP2
    else
      SINT2 = L_T2/MODL2
      SINT2HALF = sqrt((2._/**/REALKIND - COST2p1)*0.5_/**/REALKIND)
      COST2HALF = (SINT2/SINT2HALF)*0.5_/**/REALKIND
      COSP2pSINP2 = l2(3)/L_T2
      COSP2mSINP2 = l2(4)/L_T2
      b2  = COST2HALF
      cp2 = SINT2HALF*COSP2pSINP2
      cm2 = SINT2HALF*COSP2mSINP2
    end if
  end if

  MODL1L2 = sqrt(MODL1*MODL2)*4._/**/REALKIND

  l3(1) = cm1*cp2*MODL1L2
  l3(2) = b1*b2*MODL1L2
  l3(3) = b1*cp2*MODL1L2
  l3(4) = b2*cm1*MODL1L2

  l4(1) = cp1*cm2*MODL1L2
  l4(2) = b1*b2*MODL1L2
  l4(3) = b2*cp1*MODL1L2
  l4(4) = b1*cm2*MODL1L2

end subroutine construct_l3l4_3

!*************************************************************
subroutine construct_l3l4_2(l1,l2,l3,l4)
  use KIND_TYPES, only: REALKIND
  use ol_parameters_decl_/**/REALKIND, only: CI
  implicit none

  complex(REALKIND), intent(in)  :: l1(0:3), l2(0:3)
  complex(REALKIND), intent(out) :: l3(1:4), l4(1:4)
  real(REALKIND) :: COST1, COST2, SINT1, SINT2, COST1HALF, COST2HALF, SINT1HALF, SINT2HALF
  real(REALKIND) :: L_T1sq, L_T2sq, L_T1, L_T2, COST1p1, COST2p1, MODL1, MODL2, MODL1L2
  complex(REALKIND) :: COSP1pSINP1, COSP1mSINP1, COSP2pSINP2, COSP2mSINP2
  complex(REALKIND) :: b1, b2, cp1, cm1, cp2, cm2

  L_T1sq = real(l1(2)*l1(3))
  L_T2sq = real(l2(2)*l2(3))
  L_T1 = sqrt(L_T1sq)
  L_T2 = sqrt(L_T2sq)

  MODL1 = real((l1(0)+l1(1))*0.5_/**/REALKIND)
  MODL2 = real((l2(0)+l2(1))*0.5_/**/REALKIND)

  COST1p1 = real(l1(1)/MODL1)
  COST2p1 = real(l2(1)/MODL2)

  if(COST1p1==0.0) then
    b1  = 0._/**/REALKIND
    cp1 = (1._/**/REALKIND/sqrt(2._/**/REALKIND))*(1._/**/REALKIND + CI)
    cm1 = (1._/**/REALKIND/sqrt(2._/**/REALKIND))*(1._/**/REALKIND - CI)
  else if (COST1p1>1.) then
    SINT1 = L_T1/MODL1
    COST1HALF = sqrt(COST1p1*0.5_/**/REALKIND)
    SINT1HALF = (SINT1/COST1HALF)*0.5_/**/REALKIND
  if (COST1p1==2.0) then
    COSP1pSINP1 = 0._/**/REALKIND
    COSP1mSINP1 = 0._/**/REALKIND
  else
    COSP1pSINP1 = l1(2)/L_T1
    COSP1mSINP1 = l1(3)/L_T1
  end if
    b1  = COST1HALF
    cp1 = SINT1HALF*COSP1pSINP1
    cm1 = SINT1HALF*COSP1mSINP1
  else
    SINT1 = L_T1/MODL1
    SINT1HALF = sqrt((2._/**/REALKIND - COST1p1)*0.5_/**/REALKIND)
    COST1HALF = (SINT1/SINT1HALF)*0.5_/**/REALKIND
    COSP1pSINP1 = l1(2)/L_T1
    COSP1mSINP1 = l1(3)/L_T1
    b1  = COST1HALF
    cp1 = SINT1HALF*COSP1pSINP1
    cm1 = SINT1HALF*COSP1mSINP1
  end if

  if(COST2p1==0._/**/REALKIND) then
    b2  = 0._/**/REALKIND
    cp2 = (1._/**/REALKIND/sqrt(2._/**/REALKIND))*(1._/**/REALKIND + CI)
    cm2 = (1._/**/REALKIND/sqrt(2._/**/REALKIND))*(1._/**/REALKIND - CI)
  else if (COST2p1>1.) then
    SINT2 = L_T2/MODL2
    COST2HALF = sqrt(COST2p1*0.5_/**/REALKIND)
    SINT2HALF = (SINT2/COST2HALF)*0.5_/**/REALKIND
  if (COST2p1==2.0) then
    COSP2pSINP2 = 0._/**/REALKIND
    COSP2mSINP2 = 0._/**/REALKIND
  else
    COSP2pSINP2 = l2(2)/L_T2
    COSP2mSINP2 = l2(3)/L_T2
  end if
    b2  = COST2HALF
    cp2 = SINT2HALF*COSP2pSINP2
    cm2 = SINT2HALF*COSP2mSINP2
  else
    SINT2 = L_T2/MODL2
    SINT2HALF = sqrt((2._/**/REALKIND - COST2p1)*0.5_/**/REALKIND)
    COST2HALF = (SINT2/SINT2HALF)*0.5_/**/REALKIND
    COSP2pSINP2 = l2(2)/L_T2
    COSP2mSINP2 = l2(3)/L_T2
    b2  = COST2HALF
    cp2 = SINT2HALF*COSP2pSINP2
    cm2 = SINT2HALF*COSP2mSINP2
  end if

  MODL1L2 = sqrt(MODL1*MODL2)*4._/**/REALKIND

  l3(1) = cm1*cp2*MODL1L2
  l3(2) = b1*b2*MODL1L2
  l3(3) = b1*cp2*MODL1L2
  l3(4) = b2*cm1*MODL1L2

  l4(1) = cp1*cm2*MODL1L2
  l4(2) = b1*b2*MODL1L2
  l4(3) = b2*cp1*MODL1L2
  l4(4) = b1*cm2*MODL1L2

end subroutine construct_l3l4_2

! *********************************************************************************
subroutine construct_Vjj(vj,Vjj)
! **********************************************************************************
! Given a four-vector v^\mu it builds the symmetric tensor V^\{\mu\nu} = v^\mu v^\nu
! **********************************************************************************
  use KIND_TYPES, only: REALKIND
  implicit none

  complex(REALKIND), intent(in)  :: vj(1:4)
  complex(REALKIND), intent(out) :: Vjj(6:15)

  Vjj(6)  = vj(1)*vj(1)
  Vjj(7)  = vj(1)*vj(2)
  Vjj(8)  = vj(1)*vj(3)
  Vjj(9)  = vj(1)*vj(4)
  Vjj(10) = vj(2)*vj(2)
  Vjj(11) = vj(2)*vj(3)
  Vjj(12) = vj(2)*vj(4)
  Vjj(13) = vj(3)*vj(3)
  Vjj(14) = vj(3)*vj(4)
  Vjj(15) = vj(4)*vj(4)

end subroutine construct_Vjj

! ******************************************************************************
subroutine construct_Vij(vi,vj,Vij)
! **********************************************************************************
! Given a four-vectors vi^\mu vj^\mu it builds the symmetric tensor
! V^\{\mu\nu} = vi^\mu vj^\nu + vj^\mu vi^\nu
! **********************************************************************************
  use KIND_TYPES, only: REALKIND
  implicit none

  complex(REALKIND), intent(in)  :: vi(1:4), vj(1:4)
  complex(REALKIND), intent(out) :: Vij(6:15)

  Vij(6)  = (vi(1)*vj(1) + vi(1)*vj(1))
  Vij(7)  = (vi(1)*vj(2) + vi(2)*vj(1))
  Vij(8)  = (vi(1)*vj(3) + vi(3)*vj(1))
  Vij(9)  = (vi(1)*vj(4) + vi(4)*vj(1))
  Vij(10) = (vi(2)*vj(2) + vi(2)*vj(2))
  Vij(11) = (vi(2)*vj(3) + vi(3)*vj(2))
  Vij(12) = (vi(2)*vj(4) + vi(4)*vj(2))
  Vij(13) = (vi(3)*vj(3) + vi(3)*vj(3))
  Vij(14) = (vi(3)*vj(4) + vi(4)*vj(3))
  Vij(15) = (vi(4)*vj(4) + vi(4)*vj(4))

end subroutine construct_Vij

! ******************************************************************************
subroutine construct_RedBasis(mom1,mom2,RedBasis)
! ******************************************************************************
! OpenLoops On-the-fly Reduction. Basis construction
! ------------------------------------------------------------------------------
! Details on the reduction basis in arXiv:1710.11452 sect. 5.1
! ------------------------------------------------------------------------------
! p1, p2 = input momenta
! p1p1, p2p2, p1p2 = invariants p1.p1, p2.p2 and p1.p2
! sqrtDelta = square root of the Gram determinant built out of momenta p1,p2
! l1, l2 = massless 4-momenta, linear combination of p1 and p2
! l3, l4 = massless 4-momenta orthogonal to l1 and l2
! ******************************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_momenta_decl_/**/REALKIND, only: L
  use ol_data_types_/**/REALKIND, only: basis
  implicit none
  integer, intent(in) :: mom1, mom2
  type(basis), intent(out) :: RedBasis
  complex(REALKIND) :: p1(5), p2(5), gamma, alphas(2)
  complex(REALKIND) :: l1(1:4), l2(1:4), l3(1:4), l4(1:4), li(4,4), r1(1:4), r2(1:4)
  complex(REALKIND) :: r1_cv(1:4), r2_cv(1:4), l3_cv(1:4), l4_cv(1:4)
  complex(REALKIND) :: L33(6:15), L44(6:15), L34(6:15)
  complex(REALKIND) :: Tb(1:4,1:4), r1Tb(6:15,1:4), r2Tb(6:15,1:4)

  p1(1:4) = L(1:4,mom1)
  p1(5)   = L(5,mom1) + L(6,mom1)
  p2(1:4) = L(1:4,mom2)
  p2(5)   = L(5,mom2) + L(6,mom2)

  call construct_l1l2_1(mom1,mom2,alphas,gamma,l1,l2,r1,r2)
  call construct_l3l4_1(l1,l2,l3,l4)

  li(:,1) = l1(:)
  li(:,2) = l2(:)
  li(:,3) = l3(:)
  li(:,4) = l4(:)

  call construct_Vjj(l3,L33)
  call construct_Vjj(l4,L44)
  call construct_Vij(l3,l4,L34)

  !! Tb ==> L^\mu_{34,\lambda}
  Tb(1,1) = - L34(7)
  Tb(1,2) = - L34(6)
  Tb(1,3) =   L34(9)
  Tb(1,4) =   L34(8)
  Tb(2,1) = - L34(10)
  Tb(2,2) =   Tb(1,1)
  Tb(2,3) =   L34(12)
  Tb(2,4) =   L34(11)
  Tb(3,1) = - Tb(2,4)
  Tb(3,2) = - Tb(1,4)
  Tb(3,3) =   L34(14)
  Tb(3,4) =   L34(13)
  Tb(4,1) = - Tb(2,3)
  Tb(4,2) = - Tb(1,3)
  Tb(4,3) =   L34(15)
  Tb(4,4) =   Tb(3,3)

  Tb = Tb/2

  !! Definitions of r1Tb, r2Tv in arXiv:1710.11452 eq. (133)
  !! r1Tb ==> r_1^\mu L^\nu_{34,\lambda} + r_1^\nu L^\mu_{34,\lambda}
  !! r2Tb ==> r_2^\mu L^\nu_{34,\lambda} + r_2^\nu L^\mu_{34,\lambda}

  r1Tb(6,:)  = (r1(1)*Tb(1,:) + r1(1)*Tb(1,:))
  r1Tb(7,:)  = (r1(1)*Tb(2,:) + r1(2)*Tb(1,:))
  r1Tb(8,:)  = (r1(1)*Tb(3,:) + r1(3)*Tb(1,:))
  r1Tb(9,:)  = (r1(1)*Tb(4,:) + r1(4)*Tb(1,:))
  r1Tb(10,:) = (r1(2)*Tb(2,:) + r1(2)*Tb(2,:))
  r1Tb(11,:) = (r1(2)*Tb(3,:) + r1(3)*Tb(2,:))
  r1Tb(12,:) = (r1(2)*Tb(4,:) + r1(4)*Tb(2,:))
  r1Tb(13,:) = (r1(3)*Tb(3,:) + r1(3)*Tb(3,:))
  r1Tb(14,:) = (r1(3)*Tb(4,:) + r1(4)*Tb(3,:))
  r1Tb(15,:) = (r1(4)*Tb(4,:) + r1(4)*Tb(4,:))

  r2Tb(6,:)  = (r2(1)*Tb(1,:) + r2(1)*Tb(1,:))
  r2Tb(7,:)  = (r2(1)*Tb(2,:) + r2(2)*Tb(1,:))
  r2Tb(8,:)  = (r2(1)*Tb(3,:) + r2(3)*Tb(1,:))
  r2Tb(9,:)  = (r2(1)*Tb(4,:) + r2(4)*Tb(1,:))
  r2Tb(10,:) = (r2(2)*Tb(2,:) + r2(2)*Tb(2,:))
  r2Tb(11,:) = (r2(2)*Tb(3,:) + r2(3)*Tb(2,:))
  r2Tb(12,:) = (r2(2)*Tb(4,:) + r2(4)*Tb(2,:))
  r2Tb(13,:) = (r2(3)*Tb(3,:) + r2(3)*Tb(3,:))
  r2Tb(14,:) = (r2(3)*Tb(4,:) + r2(4)*Tb(3,:))
  r2Tb(15,:) = (r2(4)*Tb(4,:) + r2(4)*Tb(4,:))

  call LC_Contr2Cov(r1,r1_cv)
  call LC_Contr2Cov(r2,r2_cv)
  call LC_Contr2Cov(l3,l3_cv)
  call LC_Contr2Cov(l4,l4_cv)

  RedBasis = basis(r1_cv,r2_cv,l3_cv,l4_cv,L33,L44,L34,&
   r1Tb,r2Tb,gamma,alphas,mom1,mom2,li)

end subroutine construct_RedBasis

!*******************************************************************
subroutine normalize_gamma(gamma,mom1,mom2,mom3,gamma_norm)
!******************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_kinematics_/**/REALKIND, only: cont_LC_cntrv, get_LC_5
  implicit none

  integer, intent(in) :: mom1, mom2, mom3
  complex(REALKIND), intent(in) :: gamma
  real(REALKIND), intent(out) :: gamma_norm
  complex(REALKIND) :: p1(5), p2(5), p3(5)

  complex(REALKIND) :: p1p1, p2p2, p3p3, p1p2, p1p3, p2p3
  real(REALKIND) :: maxinv

  p1 = get_LC_5(mom1)
  p2 = get_LC_5(mom2)
  p3 = get_LC_5(mom3)

  p1p1 = p1(5)
  p2p2 = p2(5)
  p3p3 = p3(5)
  p1p2 = cont_LC_cntrv(p1(1:4),p2(1:4))
  p1p3 = cont_LC_cntrv(p1(1:4),p3(1:4))
  p2p3 = cont_LC_cntrv(p2(1:4),p3(1:4))

  maxinv = MAX(ABS(p1p1),ABS(p2p2),ABS(p3p3),ABS(p1p2),ABS(p1p3),ABS(p2p3))

  gamma_norm = abs(gamma/maxinv)

  gamma_norm = min(1._/**/REALKIND, abs(gamma_norm))

end subroutine normalize_gamma

! ******************************************************************************
subroutine construct_p3scalars(mom3,RedBasis,p3scalars,gd2,gd3)
! OpenLoops Reduction Step. Calculation of p3 dependent scalars
! ------------------------------------------------------------------------------
! Details on the reduction basis in arXiv:1710.11452 sect. 5.1
! ------------------------------------------------------------------------------
! p3 = input momentum
! RedBasis = input reduction basis
! ******************************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_data_types_/**/REALKIND, only: basis
  use ol_kinematics_/**/REALKIND, only: get_LC_5
  implicit none
  integer,     intent(in) :: mom3
  type(basis), intent(in) :: RedBasis
  complex(REALKIND), intent(out) :: p3scalars(0:4)
  real(REALKIND),    intent(out) :: gd2,gd3

  complex(REALKIND) :: p3(1:5), r1(1:4), r2(1:4), l3(1:4), l4(1:4)
  complex(REALKIND) :: p3r1, p3r2, p3l3, p3l4
  complex(REALKIND) :: two = 2._/**/REALKIND

  p3 = get_LC_5(mom3)

  p3r1 = SUM(p3(1:4)*RedBasis%vect1)
  p3r2 = SUM(p3(1:4)*RedBasis%vect2)
  p3l3 = SUM(p3(1:4)*RedBasis%vect3)
  p3l4 = SUM(p3(1:4)*RedBasis%vect4)

  p3scalars(0) = (1._/**/REALKIND/p3l3)/two
  p3scalars(1) = (p3r1/p3l3)*two
  p3scalars(2) = (p3r2/p3l3)*two
  p3scalars(3) = p3l4/p3l3                  !! --> alpha formula (133)
  p3scalars(4) = p3(5)                      !! --> p3.p3

  call normalize_gamma(RedBasis%gamma,RedBasis%mom1, &
                       RedBasis%mom2,mom3,gd2)

  call GramDeterminant3_2(RedBasis%mom1,RedBasis%mom2,mom3,gd3)

end subroutine construct_p3scalars


!******************************************************************************
subroutine construct_redset4(mom1,mom2,mom3,RedBasis12,RedBasis13,RedBasis23,RedSet)
! OpenLoops Reduction Step. Choice of the basis and assembling of the reduction set
!------------------------------------------------------------------------------
! p_i = input momenta i = 1,2,3
! RedBasis_ij = input reduction bases built out of momenta p_i,p_j
! RedSet = output data type containing the chosen reduction basis and the
!          corresponding set of p_3 scalars.
!******************************************************************************
  use KIND_TYPES, only: REALKIND,QREALKIND
  use ol_data_types_/**/REALKIND, only: basis, redset4
  use ol_parameters_decl_/**/REALKIND, only: hp_switch,hp_err_thres,hp_redset_gd3_thres
#ifdef PRECISION_dp
  use ol_data_types_/**/QREALKIND, only: basis_qp=>basis, redset4_qp=>redset4
  use ofred_basis_construction_/**/QREALKIND, only: &
      construct_RedBasis_qp=>construct_RedBasis, &
      construct_p3scalars_qp=>construct_p3scalars
  use ol_external_decl_/**/DREALKIND, only: init_qp
  use ol_kinematics_/**/DREALKIND, only: init_qp_kinematics
  use ol_parameters_decl_/**/REALKIND, only: hp_qp_kinematics_init_mode
#endif
  implicit none
  integer, intent(in)  :: mom1, mom2, mom3
  type(basis), intent(in) :: RedBasis12, RedBasis13, RedBasis23
  type(redset4), intent(out) :: RedSet
  complex(REALKIND) :: p1(1:5), p2(1:5), p3(1:5), scalars(0:4)
  real(REALKIND) :: gd2,gd3
  logical :: b1, b2, b3
  integer :: perm(3)
#ifdef PRECISION_dp
  type(redset4_qp)   :: RedSet_qp
  type(basis_qp)     :: Redbasis_qp
  real(QREALKIND)    :: gd2_qp,gd3_qp
  complex(QREALKIND) :: scalars_qp(0:4)
#endif

  !! Selection criterion
  if(basis_selection_4 == 0) then
    b1 = .true.
    b2 = .false.
    b3 = .false.
  else if(basis_selection_4 == 1) then
    call basis_choice_1(mom1,mom2,mom3,RedBasis12,RedBasis13,RedBasis23,b1,b2,b3)
  else if(basis_selection_4 == 2) then
    call basis_choice_2(mom1,mom2,mom3,RedBasis12,RedBasis13,RedBasis23,b1,b2,b3)
  end if

  !! Choice of the basis
  if (b3) then
    call construct_p3scalars(mom1,RedBasis23,scalars,gd2,gd3)
    perm = [2,3,1]
    RedSet%redbasis=RedBasis23
    RedSet%p3scalars=scalars
    RedSet%perm=perm
    RedSet%mom3=mom1
    RedSet%gd2=gd2
    RedSet%gd3=gd3
#ifdef PRECISION_dp
    RedSet%qp_computed=.false.
#endif
  else if (b2) then
    call construct_p3scalars(mom2,RedBasis13,scalars,gd2,gd3)
    perm = [1,3,2]
    RedSet%redbasis=RedBasis13
    RedSet%p3scalars=scalars
    RedSet%perm=perm
    RedSet%mom3=mom2
    RedSet%gd2=gd2
    RedSet%gd3=gd3
#ifdef PRECISION_dp
    RedSet%qp_computed=.false.
#endif
  else if (b1) then
    call construct_p3scalars(mom3,RedBasis12,scalars,gd2,gd3)
    perm = [1,2,3]
    RedSet%redbasis=RedBasis12
    RedSet%p3scalars=scalars
    RedSet%perm=perm
    RedSet%mom3=mom3
    RedSet%gd2=gd2
    RedSet%gd3=gd3
#ifdef PRECISION_dp
    RedSet%qp_computed=.false.
#endif
  end if

#ifdef PRECISION_dp
  if (hp_switch .eq. 1 .and. sqrt(abs(gd3)) .lt. hp_redset_gd3_thres) then
    if (hp_qp_kinematics_init_mode .gt. 0 .and. .not. init_qp) call init_qp_kinematics
    call construct_RedBasis_qp(RedSet%redbasis%mom1,RedSet%redbasis%mom2,Redbasis_qp)
    call construct_p3scalars_qp(RedSet%mom3,Redbasis_qp,scalars_qp,gd2_qp,gd3_qp)
    RedSet_qp = redset4_qp(redbasis=Redbasis_qp, &
                           p3scalars=scalars_qp, &
                           perm=RedSet%perm,     &
                           mom3=RedSet%mom3,     &
                           gd2=gd2_qp,gd3=gd3_qp)
    RedSet%qp_computed=.true.
    RedSet%rsqp=RedSet_qp
  end if
#endif

end subroutine construct_redset4

subroutine reconstruct_redset4(RedSet,RedSet_rec)
! OpenLoops Reduction Step. Recomputes p3 dependent scalars given RedBasis
! ------------------------------------------------------------------------------
! RedBasis = input reduction basis
! RedBasis_rec = reconstructed reduction basis
! ******************************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_data_types_/**/REALKIND, only: redset4
  implicit none
  type(redset4), intent(in)  :: RedSet
  type(redset4), intent(out) :: RedSet_rec

  complex(REALKIND) :: scalars(0:4)
  real(REALKIND)    :: gd2,gd3
  integer           :: perm(3)

  call construct_p3scalars(RedSet%mom3,RedSet%redbasis,scalars,gd2,gd3)
  RedSet_rec%redbasis=RedSet%redbasis
  RedSet_rec%p3scalars=scalars
  RedSet_rec%perm=RedSet%perm
  RedSet_rec%mom3=RedSet%mom3
  RedSet_rec%gd2=gd2
  RedSet_rec%gd3=gd3
#ifdef PRECISION_dp
  RedSet_rec%qp_computed=.false.
#endif

end subroutine reconstruct_redset4

#ifdef PRECISION_dp
subroutine upgrade_redset4(RedSet,RedSet_rec)
! ------------------------------------------------------------------------------
! RedBasis = input reduction basis
! RedBasis_rec = upgraded reduction basis
! ******************************************************************************
  use KIND_TYPES, only: DREALKIND, QREALKIND
  use ol_data_types_/**/DREALKIND, only: redset4_dp=>redset4
  use ol_data_types_/**/QREALKIND, only: basis_qp=>basis, redset4_qp=>redset4
  use ofred_basis_construction_/**/QREALKIND, only: construct_RedBasis_qp => construct_RedBasis, &
                                                    construct_p3scalars_qp=>construct_p3scalars
  implicit none
  type(redset4_dp), intent(in)  :: RedSet
  type(redset4_qp), intent(out) :: RedSet_rec
  type(basis_qp)  :: Redbasis_rec

  complex(QREALKIND) :: scalars(0:4)
  real(QREALKIND)    :: gd2,gd3
  integer            :: perm(3)

  call construct_RedBasis_qp(RedSet%redbasis%mom1, &
                             RedSet%redbasis%mom2, &
                             Redbasis_rec)

  call construct_p3scalars_qp(RedSet%mom3,Redbasis_rec,scalars,gd2,gd3)
  RedSet_rec = redset4_qp(redbasis=Redbasis_rec,p3scalars=scalars, &
                          perm=RedSet%perm,mom3=RedSet%mom3,gd2=gd2,gd3=gd3)

end subroutine upgrade_redset4

subroutine upgrade_redset5(RedSet,RedSet_rec)
! ------------------------------------------------------------------------------
! RedBasis = input reduction basis
! RedBasis_rec = upgraded reduction basis
! ******************************************************************************
  use KIND_TYPES, only: DREALKIND, QREALKIND
  use ol_data_types_/**/DREALKIND, only: redset5_dp=>redset5
  use ol_data_types_/**/QREALKIND, only: basis_qp=>basis, redset5_qp=>redset5
  use ofred_basis_construction_/**/QREALKIND, only: construct_RedBasis_qp => construct_RedBasis, &
                                                    construct_p3scalars_qp=>construct_p3scalars
  implicit none
  type(redset5_dp), intent(in)  :: RedSet
  type(redset5_qp), intent(out) :: RedSet_rec
  type(basis_qp)  :: Redbasis_rec

  complex(QREALKIND) :: scalars(0:4)
  real(QREALKIND)    :: gd2,gd3
  integer            :: perm(3)

  call construct_RedBasis_qp(RedSet%redbasis%mom1, &
                             RedSet%redbasis%mom2, &
                             Redbasis_rec)
  call construct_p3scalars_qp(RedSet%mom3,Redbasis_rec,scalars,gd2,gd3)
  RedSet_rec = redset5_qp(redbasis=Redbasis_rec,p3scalars=scalars, &
                          perm=RedSet%perm,mom3=RedSet%mom3,mom4=RedSet%mom4, &
                          gd2=gd2,gd3=gd3)

end subroutine upgrade_redset5
#endif

subroutine reconstruct_redset5(RedSet,RedSet_rec)
! OpenLoops Reduction Step. Calculation of p3 dependent scalars
! ------------------------------------------------------------------------------
! RedBasis = input reduction basis
! RedBasis_rec = reconstructed reduction basis
! ******************************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_data_types_/**/REALKIND, only: redset5
  implicit none
  type(redset5), intent(in)  :: RedSet
  type(redset5), intent(out) :: RedSet_rec

  complex(REALKIND) :: scalars(0:4)
  real(REALKIND)    :: gd2,gd3
  integer           :: perm(3)

  call construct_p3scalars(RedSet%mom3,RedSet%redbasis,scalars,gd2,gd3)
  RedSet_rec = redset5(redbasis=RedSet%redbasis,p3scalars=scalars, &
                       perm=RedSet%perm,mom3=RedSet%mom3,mom4=RedSet%mom4, &
                       gd2=gd2,gd3=gd3)

end subroutine reconstruct_redset5


!*******************************************************************
subroutine basis_choice_1(mom1,mom2,mom3,bas1,bas2,bas3,v1,v2,v3)
!-------------------------------------------------------------------
! In this choice of basis, ABS(gamma) is maximed.
! ABS(gamma) can be normalized either to 1 or to the largest entry
! of the Gram Matrix G_ij = p_i p_j
!*******************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_data_types_/**/REALKIND, only: basis, redset4
  use ol_kinematics_/**/REALKIND, only: cont_LC_cntrv, get_LC_5
  implicit none
  integer,     intent(in)  :: mom1, mom2, mom3
  type(basis), intent(in)  :: bas1, bas2, bas3
  logical,     intent(out) :: v1, v2, v3

  complex(REALKIND) :: p1(1:5), p2(1:5), p3(1:5)
  real(REALKIND)    :: gamma12(3), inv1, inv2, inv3, p1p2, p1p3, p2p3

  p1 = get_LC_5(mom1)
  p2 = get_LC_5(mom2)
  p3 = get_LC_5(mom3)

  v1 = .false.
  v2 = .false.
  v3 = .false.

  gamma12(1) = ABS(bas1%gamma)
  gamma12(2) = ABS(bas2%gamma)
  gamma12(3) = ABS(bas3%gamma)

  if(norm_mode_gd2 == 2) then
    p1p2 = REAL(cont_LC_cntrv(p1(1:4),p2(1:4)))
    p1p3 = REAL(cont_LC_cntrv(p1(1:4),p3(1:4)))
    p2p3 = REAL(cont_LC_cntrv(p2(1:4),p3(1:4)))

    inv1 = MAX(ABS(p1p2),ABS(p1(5)),ABS(p2(5)))
    inv2 = MAX(ABS(p1p3),ABS(p1(5)),ABS(p3(5)))
    inv3 = MAX(ABS(p2p3),ABS(p2(5)),ABS(p3(5)))

    gamma12(1) = gamma12(1)/inv1
    gamma12(2) = gamma12(2)/inv2
    gamma12(3) = gamma12(3)/inv3
  end if

  if (gamma12(2) .ge. gamma12(1)) then
    if (gamma12(3) .ge. gamma12(2)) then
      v3 = .true.
    else
      v2 = .true.
    end if
  else
     if (gamma12(3) .ge. gamma12(1)) then
      v3 = .true.
    else
      v1 = .true.
    end if
  end if

end subroutine basis_choice_1


!*******************************************************************
subroutine basis_choice_2(mom1,mom2,mom3,bas1,bas2,bas3,v1,v2,v3)
!-------------------------------------------------------------------
! In this choice of basis, ABS(Delta) is maximed.
! ABS(Delta) can be normalized either to 1 or to the largest entry
! of the Gram Matrix squared (G_ij)^2 = (p_i p_j)^2
!*******************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_data_types_/**/REALKIND, only: basis, redset4
  use ol_kinematics_/**/REALKIND, only: cont_LC_cntrv, get_LC_5
  implicit none
  integer,     intent(in)  :: mom1, mom2, mom3
  type(basis), intent(in) :: bas1, bas2, bas3
  logical, intent(out) :: v1, v2, v3

  complex(REALKIND) :: p1(1:5), p2(1:5), p3(1:5)
  real(REALKIND)    :: delta12(3), inv1, inv2, inv3, p1p2, p1p3, p2p3

  p1 = get_LC_5(mom1)
  p2 = get_LC_5(mom2)
  p3 = get_LC_5(mom3)

  v1 = .false.
  v2 = .false.
  v3 = .false.

  p1p2 = REAL(cont_LC_cntrv(p1(1:4),p2(1:4)))
  p1p3 = REAL(cont_LC_cntrv(p1(1:4),p3(1:4)))
  p2p3 = REAL(cont_LC_cntrv(p2(1:4),p3(1:4)))

  delta12(1) = ABS((p1p2)**2 - REAL(p1(5)*p2(5)))
  delta12(2) = ABS((p1p3)**2 - REAL(p1(5)*p3(5)))
  delta12(3) = ABS((p2p3)**2 - REAL(p2(5)*p3(5)))

  inv1 = MAX(ABS(p1p2),ABS(p1(5)),ABS(p2(5)))
  inv2 = MAX(ABS(p1p3),ABS(p1(5)),ABS(p3(5)))
  inv3 = MAX(ABS(p2p3),ABS(p2(5)),ABS(p3(5)))

  if(norm_mode_gd2 == 2) then
    delta12(1) = delta12(1)/inv1**2
    delta12(2) = delta12(2)/inv2**2
    delta12(3) = delta12(3)/inv3**2
  end if

  if (delta12(2) .ge. delta12(1)) then
    if (delta12(3) .ge. delta12(2)) then
      v3 = .true.
    else
      v2 = .true.
    end if
  else
     if (delta12(3) .ge. delta12(1)) then
      v3 = .true.
    else
      v1 = .true.
    end if
  end if

end subroutine basis_choice_2

!*******************************************************************
subroutine GDrank2_choice_1(mom1,mom2,mom3,mom4,bas12,bas13,bas14,bas23, &
                            bas24,bas34,v12,v13,v14,v23,v24,v34)
!-------------------------------------------------------------------
! In this choice of basis, ABS(gamma) is maximed.
! ABS(gamma) can be normalized either to 1 or to the largest entry
! of the Gram Matrix G_ij = p_i p_j
!*******************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_data_types_/**/REALKIND, only: basis, redset5
  use ol_kinematics_/**/REALKIND, only: cont_LC_cntrv, get_LC_5
  implicit none
  integer,     intent(in)  :: mom1,mom2,mom3,mom4
  type(basis), intent(in)  :: bas12,bas13,bas14,bas23,bas24,bas34
  logical,     intent(out) :: v12, v13, v14, v23, v24, v34

  complex(REALKIND) :: p1(1:5), p2(1:5), p3(1:5), p4(1:5)
  real(REALKIND) :: gamma12(6), inv12, inv13, inv14, inv23, inv24, inv34, &
                    p1p2, p1p3, p1p4, p2p3, p2p4, p3p4, max_value
  integer :: idx, i

  p1 = get_LC_5(mom1)
  p2 = get_LC_5(mom2)
  p3 = get_LC_5(mom3)
  p4 = get_LC_5(mom4)

  v12 = .false.
  v13 = .false.
  v14 = .false.
  v23 = .false.
  v24 = .false.
  v34 = .false.

  gamma12(1) = ABS(bas12%gamma)
  gamma12(2) = ABS(bas13%gamma)
  gamma12(3) = ABS(bas14%gamma)
  gamma12(4) = ABS(bas23%gamma)
  gamma12(5) = ABS(bas24%gamma)
  gamma12(6) = ABS(bas34%gamma)

  if(norm_mode_gd2 == 2) then
    p1p2 = REAL(cont_LC_cntrv(p1(1:4),p2(1:4)))
    p1p3 = REAL(cont_LC_cntrv(p1(1:4),p3(1:4)))
    p1p4 = REAL(cont_LC_cntrv(p1(1:4),p4(1:4)))
    p2p3 = REAL(cont_LC_cntrv(p2(1:4),p3(1:4)))
    p2p4 = REAL(cont_LC_cntrv(p2(1:4),p4(1:4)))
    p3p4 = REAL(cont_LC_cntrv(p3(1:4),p4(1:4)))

    inv12 = MAX(ABS(p1p2),ABS(p1(5)),ABS(p2(5)))
    inv13 = MAX(ABS(p1p3),ABS(p1(5)),ABS(p3(5)))
    inv14 = MAX(ABS(p2p3),ABS(p1(5)),ABS(p4(5)))
    inv23 = MAX(ABS(p1p2),ABS(p2(5)),ABS(p3(5)))
    inv24 = MAX(ABS(p1p3),ABS(p2(5)),ABS(p4(5)))
    inv34 = MAX(ABS(p2p3),ABS(p3(5)),ABS(p4(5)))

    gamma12(1) = gamma12(1)/inv12
    gamma12(2) = gamma12(2)/inv13
    gamma12(3) = gamma12(3)/inv14
    gamma12(4) = gamma12(4)/inv23
    gamma12(5) = gamma12(5)/inv24
    gamma12(6) = gamma12(6)/inv34
  end if

  max_value = 0._/**/REALKIND

  do i = 1,6
    if(gamma12(i) > max_value) then
      max_value = gamma12(i)
      idx = i
    end if
  end do

  if (idx.eq.1) then
    v12 =.true.
  else if (idx.eq.2) then
    v13 = .true.
  else if (idx.eq.3) then
    v14 = .true.
  else if (idx.eq.4) then
    v23 = .true.
  else if (idx.eq.5) then
    v24 = .true.
  else if (idx.eq.6) then
    v34 = .true.
  end if


end subroutine GDrank2_choice_1


!*******************************************************************
subroutine GDrank2_choice_2(mom1,mom2,mom3,mom4,bas12,bas13,bas14,bas23, &
                            bas24,bas34,v12,v13,v14,v23,v24,v34)
!-------------------------------------------------------------------
! In this choice of basis, ABS(Delta) is maximed.
! ABS(Delta) can be normalized either to 1 or to the largest entry
! of the Gram Matrix squared (G_ij)^2 = (p_i p_j)^2
!*******************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_data_types_/**/REALKIND, only: basis, redset5
  use ol_kinematics_/**/REALKIND, only: cont_LC_cntrv, get_LC_5
  implicit none

  integer, intent(in) :: mom1,mom2,mom3,mom4
  type(basis), intent(in) :: bas12,bas13,bas14,bas23,bas24,bas34
  logical, intent(out) :: v12, v13, v14, v23, v24, v34

  complex(REALKIND) :: p1(1:5), p2(1:5), p3(1:5), p4(1:5)
  real(REALKIND) :: delta12(6), inv12, inv13, inv14, inv23, inv24, inv34, &
                    p1p2, p1p3, p1p4, p2p3, p2p4, p3p4, max_value
  integer :: idx, i

  p1 = get_LC_5(mom1)
  p2 = get_LC_5(mom2)
  p3 = get_LC_5(mom3)
  p4 = get_LC_5(mom4)

  v12 = .false.
  v13 = .false.
  v14 = .false.
  v23 = .false.
  v24 = .false.
  v34 = .false.

  p1p2 = REAL(cont_LC_cntrv(p1(1:4),p2(1:4)))
  p1p3 = REAL(cont_LC_cntrv(p1(1:4),p3(1:4)))
  p1p4 = REAL(cont_LC_cntrv(p1(1:4),p4(1:4)))
  p2p3 = REAL(cont_LC_cntrv(p2(1:4),p3(1:4)))
  p2p4 = REAL(cont_LC_cntrv(p2(1:4),p4(1:4)))
  p3p4 = REAL(cont_LC_cntrv(p3(1:4),p4(1:4)))

  delta12(1) = ABS((p1p2)**2 - REAL(p1(5)*p2(5)))
  delta12(2) = ABS((p1p3)**2 - REAL(p1(5)*p3(5)))
  delta12(3) = ABS((p1p4)**2 - REAL(p1(5)*p4(5)))
  delta12(4) = ABS((p2p3)**2 - REAL(p2(5)*p3(5)))
  delta12(5) = ABS((p2p4)**2 - REAL(p2(5)*p4(5)))
  delta12(6) = ABS((p3p4)**2 - REAL(p3(5)*p4(5)))

  inv12 = MAX(ABS(p1p2),ABS(p1(5)),ABS(p2(5)))
  inv13 = MAX(ABS(p1p3),ABS(p1(5)),ABS(p3(5)))
  inv14 = MAX(ABS(p2p3),ABS(p1(5)),ABS(p4(5)))
  inv23 = MAX(ABS(p1p2),ABS(p2(5)),ABS(p3(5)))
  inv24 = MAX(ABS(p1p3),ABS(p2(5)),ABS(p4(5)))
  inv34 = MAX(ABS(p2p3),ABS(p3(5)),ABS(p4(5)))

  if(norm_mode_gd2 == 2) then
    delta12(1) = delta12(1)/inv12**2
    delta12(2) = delta12(2)/inv13**2
    delta12(3) = delta12(3)/inv14**2
    delta12(4) = delta12(4)/inv23**2
    delta12(5) = delta12(5)/inv24**2
    delta12(6) = delta12(6)/inv34**2
  end if

  max_value = 0._/**/REALKIND

  do i = 1,6
    if(delta12(i) > max_value) then
      max_value = delta12(i)
      idx = i
    end if
  end do

  if (idx.eq.1) then
    v12 =.true.
  else if (idx.eq.2) then
    v13 = .true.
  else if (idx.eq.3) then
    v14 = .true.
  else if (idx.eq.4) then
    v23 = .true.
  else if (idx.eq.5) then
    v24 = .true.
  else if (idx.eq.6) then
    v34 = .true.
  end if

end subroutine GDrank2_choice_2


!*******************************************************************
subroutine GramDeterminant3(p1,p2,p3,delta)
!******************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_kinematics_/**/REALKIND, only: cont_LC_cntrv
  implicit none

  complex(REALKIND), intent(in) :: p1(5), p2(5), p3(5)
  complex(REALKIND), intent(out) :: delta

  complex(REALKIND) :: p1p1, p2p2, p3p3, p1p2, p1p3, p2p3
  real(REALKIND) :: maxinv

  p1p1 = p1(5)
  p2p2 = p2(5)
  p3p3 = p3(5)
  p1p2 = cont_LC_cntrv(p1(1:4),p2(1:4))
  p1p3 = cont_LC_cntrv(p1(1:4),p3(1:4))
  p2p3 = cont_LC_cntrv(p2(1:4),p3(1:4))

  maxinv = MAX(ABS(p1p1),ABS(p2p2),ABS(p3p3),ABS(p1p2),ABS(p1p3),ABS(p2p3))

  delta = p1p1*p2p2*p3p3 + 2*p1p2*p2p3*p1p3 -&
          p1p3*p2p2*p1p3 - p1p1*p2p3*p2p3 - p3p3*p1p2*p1p2

  delta = delta/(maxinv**3)

end subroutine GramDeterminant3

!*******************************************************************
subroutine GramDeterminant3_2(mom1,mom2,mom3,delta)
!******************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_kinematics_/**/REALKIND, only: cont_LC_cntrv, get_LC_5
  implicit none

  integer,        intent(in)  :: mom1,mom2,mom3
  real(REALKIND), intent(out) :: delta

  complex(REALKIND) :: p1(5),p2(5),p3(5)
  real(REALKIND)    :: p1p1,p2p2,p3p3,p1p2,p1p3,p2p3
  real(REALKIND)    :: maxinv

  p1 = get_LC_5(mom1)
  p2 = get_LC_5(mom2)
  p3 = get_LC_5(mom3)

  p1p1 = real(p1(5))
  p2p2 = real(p2(5))
  p3p3 = real(p3(5))
  p1p2 = real(cont_LC_cntrv(p1(1:4),p2(1:4)))
  p1p3 = real(cont_LC_cntrv(p1(1:4),p3(1:4)))
  p2p3 = real(cont_LC_cntrv(p2(1:4),p3(1:4)))

  maxinv = MAX(ABS(p1p1),ABS(p2p2),ABS(p3p3),ABS(p1p2),ABS(p1p3),ABS(p2p3))

  delta = p1p1*p2p2*p3p3 + 2*p1p2*p2p3*p1p3 - &
          p1p3*p2p2*p1p3 - p1p1*p2p3*p2p3 - p3p3*p1p2*p1p2

  delta = delta/(maxinv**3)

end subroutine GramDeterminant3_2

! ******************************************************************************
subroutine construct_basis_redset5(mom1,mom2,mom3,RedBasis12,RedBasis13, &
                                   RedBasis23,RedSet,perm_in,mom4)
! OpenLoops Reduction Step. Choice of the basis and assembling of the reduction set
! ------------------------------------------------------------------------------
! p_i = input momenta i = 1,2,3
! RedBasis_ij = input reduction bases built out of momenta p_i,p_j
! RedSet = output data type containing the chosen reduction basis and the
!          corresponding set of p_3 scalars.
! ******************************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_data_types_/**/REALKIND, only: basis, redset5
  implicit none
  integer,        intent(in)  :: mom1,mom2,mom3,perm_in(4), mom4
  type(basis),    intent(in)  :: RedBasis12, RedBasis13, RedBasis23
  type(redset5),  intent(out) :: RedSet

  complex(REALKIND) :: scalars(0:4)
  real(REALKIND)    :: gd2,gd3

  logical :: b1, b2, b3
  integer :: perm(4)

  !! Selection criterion
  if(basis_selection_5 == 0) then
    b1 = .true.
    b2 = .false.
    b3 = .false.
  else if(basis_selection_5 == 1) then
    call basis_choice_1(mom1,mom2,mom3,RedBasis12,RedBasis13,RedBasis23,b1,b2,b3)
  else if(basis_selection_5 == 2) then
    call basis_choice_2(mom1,mom2,mom3,RedBasis12,RedBasis13,RedBasis23,b1,b2,b3)
  end if

  !! Choice of the basis
  if (b3) then
    call construct_p3scalars(mom1,RedBasis23,scalars,gd2,gd3)
    perm = [perm_in(2),perm_in(3),perm_in(1),perm_in(4)]
    RedSet = redset5(RedBasis23,scalars,perm,mom1,mom4,gd2,gd3)
  else if (b2) then
    call construct_p3scalars(mom2,RedBasis13,scalars,gd2,gd3)
    perm = [perm_in(1),perm_in(3),perm_in(2),perm_in(4)]
    RedSet = redset5(RedBasis13,scalars,perm,mom2,mom4,gd2,gd3)
  else if (b1) then
    call construct_p3scalars(mom3,RedBasis12,scalars,gd2,gd3)
    perm = [perm_in(1),perm_in(2),perm_in(3),perm_in(4)]
    RedSet = redset5(RedBasis12,scalars,perm,mom3,mom4,gd2,gd3)
  end if

end subroutine construct_basis_redset5

! ******************************************************************************
subroutine construct_redset5(mom1,mom2,mom3,mom4,RedBasis12,&
  RedBasis13,RedBasis14,RedBasis23,RedBasis24,RedBasis34,RedSet)
! OpenLoops Reduction Step. Choice of the basis and assembling of the reduction set
! ------------------------------------------------------------------------------
! p_i = input momenta i = 1,2,3,4
! RedBasis_ij = input reduction bases built out of momenta p_i,p_j
! RedSet = output data type containing the chosen reduction basis and the
!          corresponding set of p_3 scalars.
! ******************************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_data_types_/**/REALKIND, only: basis, redset5
  implicit none
  integer,       intent(in)  :: mom1,mom2,mom3,mom4
  type(basis),   intent(in)  :: RedBasis12,RedBasis13,RedBasis14, &
                                RedBasis23,RedBasis24,RedBasis34
  type(redset5), intent(out) :: RedSet

  complex(REALKIND) :: scalars1(0:4),scalars2(0:4)
  real(REALKIND)    :: alfa_max1,alfa_max2,gd21,gd22,gd31,gd32
  logical :: b12,b13,b14,b23,b24,b34
  integer :: perm(4)

  !! Selection criterion
  if(basis_selection_5 == 0) then
    b12 = .true.
    b13 = .false.
    b14 = .false.
    b23 = .false.
    b24 = .false.
    b34 = .false.

  else if(basis_selection_5 == 1) then
    call GDrank2_choice_1(mom1,mom2,mom3,mom4,RedBasis12,RedBasis13,RedBasis14,&
      RedBasis23,RedBasis24,RedBasis34,b12,b13,b14,b23,b24,b34)
  else if(basis_selection_5 == 2) then
    call GDrank2_choice_2(mom1,mom2,mom3,mom4,RedBasis12,RedBasis13,RedBasis14,&
      RedBasis23,RedBasis24,RedBasis34,b12,b13,b14,b23,b24,b34)
  end if

  if (b12) then
    if (choose_basis(mom3,mom4,RedBasis12)) then
      perm = [1,2,3,4]
      RedSet = redset5(RedBasis12,scalars1,perm,mom3,mom4,gd21,gd31)
    else
      perm = [1,2,4,3]
      RedSet = redset5(RedBasis12,scalars2,perm,mom4,mom3,gd22,gd32)
    end if
  else if (b13) then
    if (choose_basis(mom2,mom4,RedBasis13)) then
      perm = [1,3,2,4]
      RedSet = redset5(RedBasis13,scalars1,perm,mom2,mom4,gd21,gd31)
    else
      perm = [1,3,4,2]
      RedSet = redset5(RedBasis13,scalars2,perm,mom4,mom2,gd22,gd32)
    end if
  else if (b14) then
    if (choose_basis(mom2,mom3,RedBasis14)) then
      perm = [1,4,2,3]
      RedSet = redset5(RedBasis14,scalars1,perm,mom2,mom3,gd21,gd31)
    else
      perm = [1,4,3,2]
      RedSet = redset5(RedBasis14,scalars2,perm,mom3,mom2,gd22,gd32)
    end if
  else if (b23) then
    if (choose_basis(mom1,mom4,RedBasis23)) then
      perm = [2,3,1,4]
      RedSet = redset5(RedBasis23,scalars1,perm,mom1,mom4,gd21,gd31)
    else
      perm = [2,3,4,1]
      RedSet = redset5(RedBasis23,scalars2,perm,mom4,mom1,gd22,gd32)
    end if
  else if (b24) then
    if (choose_basis(mom1,mom3,RedBasis24)) then
      perm = [2,4,1,3]
      RedSet = redset5(RedBasis24,scalars1,perm,mom1,mom3,gd21,gd31)
    else
      perm = [2,4,3,1]
      RedSet = redset5(RedBasis24,scalars2,perm,mom3,mom1,gd22,gd32)
    end if
  else if (b34) then
    if (choose_basis(mom1,mom2,RedBasis34)) then
      perm = [3,4,1,2]
      RedSet = redset5(RedBasis34,scalars1,perm,mom1,mom2,gd21,gd31)
    else
      perm = [3,4,2,1]
      RedSet = redset5(RedBasis34,scalars2,perm,mom2,mom1,gd22,gd32)
    end if
  end if

  contains

  function choose_basis(m1,m2,RB)
    type(basis),   intent(in) :: RB
    integer,       intent(in) :: m1,m2
    complex(REALKIND) :: B31,B32
    logical :: choose_basis

    call construct_p3scalars(m1,RB,scalars1,gd21,gd31)
    call construct_p3scalars(m2,RB,scalars2,gd22,gd32)
    if (norm_mode_gd3 .eq. 0) then
      choose_basis = abs(gd31) .gt. abs(gd32)
    else
      if (norm_mode_gd3 .eq. 2) then
        B31 = 2*(RB%gamma)*scalars1(0)
        B32 = 2*(RB%gamma)*scalars2(0)
      else
        B31 = 0
        B32 = 0
      end if
      alfa_max1 = max(abs(scalars1(1)),abs(scalars1(2)),abs(B31))
      alfa_max2 = max(abs(scalars2(1)),abs(scalars2(2)),abs(B32))
      choose_basis = alfa_max1 .lt. alfa_max2
    end if

  end function choose_basis

end subroutine construct_redset5

end module ofred_basis_construction_/**/REALKIND


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                                               !!
!! ----------------------------------  ON-THE-FLY REDUCTION  ----------------------------------- !!
!!                                                                                               !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module ofred_reduction_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  use ol_debug, only: ol_error
  use ol_parameters_decl_/**/REALKIND, only: hp_switch,hp_alloc_mode
  use ofred_basis_construction_/**/REALKIND, only: gdm2_err_estim_OL, &
                                                   gdm2_err_estim_CL

  implicit none

  interface otf_4pt_reduction
    module procedure otf_4pt_red
  end interface

  interface Hotf_4pt_reduction
    module procedure Hotf_4pt_red, Hotf_4pt_red_R1
  end interface

  interface Hotf_4pt_reduction_last
    module procedure Hotf_4pt_red_last, Hotf_4pt_red_last_R1
  end interface

  interface otf_5pt_reduction
    module procedure otf_5pt_red
  end interface

  interface Hotf_5pt_reduction
    module procedure Hotf_5pt_red, Hotf_5pt_red_R1
  end interface

  interface Hotf_5pt_reduction_last
    module procedure Hotf_5pt_red_last, Hotf_5pt_red_last_R1
  end interface

  interface valid_4pt
    module procedure valid_4pt_hol, valid_4pt_hcl
  end interface

  interface valid_5pt
    module procedure valid_5pt_hol, valid_5pt_hcl
  end interface

  interface err_estim_4pt
    module procedure err_estim_4pt_hol, err_estim_4pt_hcl
  end interface

  interface err_estim_5pt
    module procedure err_estim_5pt_hol, err_estim_5pt_hcl
  end interface

  interface trigger_upgrade
    module procedure trigger_upgrade_hcl, trigger_upgrade_hol
  end interface

#ifdef PRECISION_dp
  interface downgrade_4pt
    module procedure downgrade_4pt_hol,downgrade_4pt_hcl
  end interface

  interface downgrade_5pt
    module procedure downgrade_5pt_hol,downgrade_5pt_hcl
  end interface
#endif

contains

! ******************************************************************************
subroutine LC_Cov2Contr(v_cov,v_ctrv)
! ------------------------------------------------------------------------------
! Transformation from covariant to contravariant in light cone representation
! ******************************************************************************
  use KIND_TYPES, only: REALKIND
  implicit none
  complex(REALKIND), intent(in) :: v_cov(4)
  complex(REALKIND), intent(out) :: v_ctrv(4)

  v_ctrv(1) =   2*v_cov(2)
  v_ctrv(2) =   2*v_cov(1)
  v_ctrv(3) = - 2*v_cov(4)
  v_ctrv(4) = - 2*v_cov(3)

end subroutine LC_Cov2Contr

! ******************************************************************************
subroutine tadpole_assignment(msq,mass,B0coeff,GTad)
! ------------------------------------------------------------------------------
! It assigns the coefficient to a tadpole after the reduction of a tensor bubble
! which follows from the reduction of a tensor triangle.
! The assignment is done according to the masses of the parent triangle.
! ------------------------------------------------------------------------------
! msq     = masses squared in the parent triangle
! mass    = mass in the tadpole
! B0coeff = coefficients of the tadpole from the reduction of the tensor bubble
! GTad    = output tadpole coefficient
! ******************************************************************************
  use KIND_TYPES, only: REALKIND
  implicit none
  complex(REALKIND), intent(in) :: msq(0:2), mass, B0coeff(1:4,0:2)
  complex(REALKIND), intent(out) :: GTad(1)

  !! (m,m,m)
  if(mass == msq(0) .AND. mass == msq(1) .AND. mass == msq(2)) then
    GTad(1) = msq(0)*(B0coeff(2,0) + B0coeff(2,1) + B0coeff(2,2))
  !! (m,m,m2)
  else if(mass == msq(0) .AND. mass == msq(1)) then
    GTad(1) = msq(0)*(B0coeff(2,1) + B0coeff(2,2) + B0coeff(3,2) + B0coeff(2,0))
  !! (m,m1,m)
  else if(mass == msq(0) .AND. mass == msq(2)) then
    GTad(1) = msq(0)*(B0coeff(2,1) + B0coeff(3,1) + B0coeff(2,2) + B0coeff(3,0))
  !! (m0,m,m)
  else if(mass == msq(1) .AND. mass == msq(2)) then
    GTad(1) = msq(1)*(B0coeff(2,0) + B0coeff(3,0) + B0coeff(3,1) + B0coeff(3,2))
  !! (m,m1,m2)
  else if(mass == msq(0)) then
    GTad(1) = msq(0)*(B0coeff(2,1) + B0coeff(2,2))
  !! (m0,m,m2)
  else if(mass == msq(1)) then
    GTad(1) = msq(1)*(B0coeff(2,0) + B0coeff(3,2))
  !! (m0,m1,m)
  else if(mass == msq(2)) then
    GTad(1) = msq(2)*(B0coeff(3,0) + B0coeff(3,1))
  end if

end subroutine tadpole_assignment


! ******************************************************************************
subroutine r1r1_combo(tens1,R2_out)
! ******************************************************************************
! Product of two rank-1 objects to produce a fully symmetrized rank-2 one, i.e.
! v^\mu * \v^\nu ==> V^{\mu\nu}
! ------------------------------------------------------------------------------
! tens1  = tens(2:5,1:4) --> v^\mu and v^\nu
! R2_out = rank-2 output
! ******************************************************************************
  use KIND_TYPES, only: REALKIND
  implicit none
  complex(REALKIND), intent(in) :: tens1(2:5,1:4)
  complex(REALKIND), intent(out) :: R2_out(6:15)

  R2_out(6)  = tens1(2,1)
  R2_out(7)  = tens1(2,2) + tens1(3,1)
  R2_out(8)  = tens1(2,3) + tens1(4,1)
  R2_out(9)  = tens1(2,4) + tens1(5,1)

  R2_out(10) = tens1(3,2)
  R2_out(11) = tens1(3,3) + tens1(4,2)
  R2_out(12) = tens1(3,4) + tens1(5,2)

  R2_out(13) = tens1(4,3)
  R2_out(14) = tens1(4,4) + tens1(5,3)

  R2_out(15) = tens1(5,4)

end subroutine r1r1_combo


! =============================================================================
!                     Reduction of 2-point integrals
! -----------------------------------------------------------------------------
! OTF Reduction subroutines for closed 2-point functions
! =============================================================================

! ******************************************************************************
subroutine twopoint_reduction(Gin,p,msq,B0coeff)
! ******************************************************************************
! OpenLoops Reduction Step. Reduction of two point integrals
! ------------------------------------------------------------------------------
! Gin     = input closed open-loops coefficient
! msq     = array of squared masses in the loop
! p(1:4)  = momentum flowing in the loop
! p(5)    = p^2
! B0coeff = coefficients of the reduced integrals:
!           1) coefficient of the scalar bubble
!           2) coefficient of the tadpole with mass m0
!           3) coefficient of the tadpole with mass m1
!           4) Rational contributions
! ******************************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_debug, only: ol_error
  use ol_loop_parameters_decl_/**/DREALKIND, only: ti_os_thresh, mureg
  implicit none
  complex(REALKIND), intent(in)  :: Gin(:)
  complex(REALKIND), intent(in)  :: p(1:5), msq(0:1)
  complex(REALKIND), intent(out) :: B0coeff(1:4)
  complex(REALKIND) :: tempcoeff(1:4)
  real(REALKIND) :: thres_exp
  complex(REALKIND) :: pp(6:15), PPmu, Gmunu, PPmunu, PPmunu_psq, PPmunu_m0, PP_m0m1
  real(REALKIND) :: zero = 0._/**/REALKIND, one = 1._/**/REALKIND, two = 2._/**/REALKIND
  complex(REALKIND) :: psq_m0, xi, f0, f1, f2, f3, f4, f5, z0
  integer :: i, j, n
  logical :: onshell_zero

  B0coeff = zero
  tempcoeff = zero

  !Threshold for the expansion in p^2
  thres_exp = 1.E-6

  !Check if the external momentum is light-like
  onshell_zero = abs(p(5))/mureg**2 < ti_os_thresh

  !------------------------------------------------------------------------*
  !---------------------------- Scalar bubble -----------------------------*
  !------------------------------------------------------------------------*
  if (size(Gin) == 1) then
    B0coeff(1) = Gin(1)
    return
  end if

  !------------------------------------------------------------------------*
  !--------------------- Reduction of the rank-2 part ---------------------*
  !------------------------------------------------------------------------*
  if (size(Gin) > 5) then
    n = 6
    do i = 1, 4
      do j = i, 4
        pp(n) = p(i)*p(j)
        n = n + 1
      end do
    end do

    Gmunu = (Gin(7)-Gin(14))/6
    PPmunu = SUM(pp(6:15)*Gin(6:15))

    !Massless bubble first
    if(msq(0) == msq(1) .AND. msq(0) == zero) then
      !scaleless bubble
      if (onshell_zero) then
        tempcoeff(1) = PPmunu/3
        tempcoeff(4) = zero
        !tempcoeff(4) = PPmunu/18
      !massless bubble with p^2 /= 0
      else
        tempcoeff(1) = PPmunu/3 - p(5)*Gmunu
        tempcoeff(4) = -(12*p(5)*Gmunu-PPmunu)/18
      end if

    !Bubble with same masses, both different from zero
    else if (msq(0) == msq(1)) then
      f2 = p(5)/msq(0)

      !Exact formula
      if(ABS(f2) > thres_exp) then
        PPmunu_psq = PPmunu/(3*p(5))
        tempcoeff(1) = msq(0)*((4*one-f2)*Gmunu + (f2-one)*PPmunu_psq )
        tempcoeff(2) =(2*Gmunu + PPmunu_psq)
        tempcoeff(4) = -msq(0)*(two-f2/3)*(PPmunu_psq-4*Gmunu)/2

      !Expansion in p-squared
      else
        PPmunu_m0 = PPmunu/msq(0)
        tempcoeff(2) = ((6*one-f2)*Gmunu + PPmunu_m0/3)
        tempcoeff(4) = msq(0)*(-PPmunu_m0/3 +&
        (Gmunu + PPmunu_m0/20)*f2 +((PPmunu_m0/21 - Gmunu)/10)*f2**2 )

      end if

    !Bubble with different masses
    else
      xi = p(5)/(msq(0)-msq(1))
      f0 = msq(0)/(msq(0)-msq(1))
      f1 = msq(1)/(msq(0)-msq(1))

      !Exact formula
      if(ABS(xi) > thres_exp) then

        f4 = (msq(0)-msq(1)-p(5))/p(5)
        f3 = (msq(0)**2 + (msq(1)-p(5))**2)/p(5)
        f5 = msq(1)/p(5)
        PPmunu_psq = PPmunu/(3*p(5))

        tempcoeff(1) = (2*msq(0)*(one+f5)-f3)*Gmunu + &
        (f3 + msq(0)*(one-2*f5) )*PPmunu_psq
        tempcoeff(2) = (two + f4)*(Gmunu-PPmunu_psq)
        tempcoeff(3) = (-f4*Gmunu + (3*one + f4)*PPmunu_psq)
        tempcoeff(4) = -(p(5)/3 - msq(0) - msq(1))*(4*Gmunu-PPmunu_psq)/2

      !Expansion in p-squared
      else
        PP_m0m1 = PPmunu/(msq(0)-msq(1))

        tempcoeff(2) =f0*((3*Gmunu+(f0/3)*PP_m0m1)+((3*f1-f0)*Gmunu+f0*f1*PP_m0m1)*xi + &
        f1*(3*f1*Gmunu + f0*(f0+2*f1)*PP_m0m1)*xi**2)

        tempcoeff(3) = ((f0*f1-f0**2-(f1**2)/3)*PP_m0m1 - 3*f1*Gmunu) + &
        ((f1-3*f0)*f1*Gmunu -(f0**3)*PP_m0m1)*xi + &
        (-(3*f1*f0**2)*Gmunu - (f0**3)*(f0+2*f1)*PP_m0m1)*xi**2

        tempcoeff(4) = (msq(0)-msq(1))*((1.5*(f0+f1)*Gmunu + &
        ((5*f0*(f0+f1)-4*f1**2)/18)*PP_m0m1) + &
        ((f0**2 + 10*f0*f1+f1**2)*(Gmunu/6) + &
        (3*f0**3 + 13*f1*f0**2-5*f0*f1**2 + f1**3)*(PP_m0m1/12))*xi +&
        ((f0+f1)*(8*f0*f1-f1**2-f0**2)*(Gmunu/4) + (6*f0**4 +&
        131*f1*f0**3+51*(f0**2)*(f1**2)-9*f0*f1**3+f1**4)*(PP_m0m1/60))*xi**2)

      end if
    end if

  end if

  !------------------------------------------------------------------------*
  !--------------------- Reduction of the rank-1 part ---------------------*
  !------------------------------------------------------------------------*

  PPmu = SUM(Gin(2:5)*p(1:4))/2

  !Massless bubble
  if(msq(0) == msq(1) .AND. msq(0) == 0._/**/REALKIND) then
    B0coeff(1) = Gin(1) - PPmu

  !Bubble with same masses, both different from zero
  else if(msq(0) == msq(1)) then
    B0coeff(1) = Gin(1) - PPmu

  !Bubble with different masses
  else
    xi = p(5)/(msq(0)-msq(1))
    !Exact reduction formula
    if(ABS(xi) > thres_exp) then
      B0coeff(1) = Gin(1) - ((msq(0)-msq(1))/p(5) + one)*PPmu
      B0coeff(2) = PPmu/p(5)
      B0coeff(3) = -B0coeff(2)
    !Expansion
    else
      f0 = msq(0)/(msq(0)-msq(1))
      f1 = msq(1)/(msq(0)-msq(1))
      !p^2 = 0 case
      if(abs(xi) == 0._/**/REALKIND) then
        B0coeff(1) = Gin(1)
        B0coeff(2) = (-f0**2)*PPmu/msq(0)
        B0coeff(3) = f1*(2*f0-f1)*PPmu/msq(1)
        B0coeff(4) = -(f0+f1)*PPmu/2
      ! Expansion for p^2!=0 up to order xi^2
      else
        B0coeff(1) = Gin(1)
        B0coeff(2) = f0*(-f0-2*f0*f1*xi -((2*f0+3*f1)*f0*f1)*xi**2)*(PPmu/msq(0))
        B0coeff(3) = f1*((2*f0-f1) +2*xi*f0**2 + (xi**2)*(2*f0+3*f1)*f0**2)*(PPmu/msq(1))
        B0coeff(4) = (-(f0 +f1)/2 + xi*(one/3 - f0*(f0+f1)) + (-3*f0**3 - 47*f1*f0**2 - &
        11*f0*f1**2 + f1**3)*xi**2/12)*PPmu
      end if
    end if

  end if

  B0coeff = B0coeff + tempcoeff

end subroutine twopoint_reduction

! =============================================================================
!                     Reduction of 3-point integrand/ls
! -----------------------------------------------------------------------------
! OTF Reduction subroutines for open and closed 3-point integrands/integrals
! =============================================================================

! ******************************************************************************
subroutine threepoint_reduction(G_ab,RedBasis,m0sq,fk0,gammas,&
l1,l2,w1,w2,redcoeff)
! ******************************************************************************
! on-the-fly OpenLoops reduction. 4-point functions or higher
! ------------------------------------------------------------------------------
! G_ab      = input open-loop, a(lpha) b(eta) components. Only rank-2 in input
! RedBasis  = input reduction basis
! m0sq      = m0^2
! fk0       = (m_k)^2 - (m_0)^2 - (p_k)^2
! gammas(2) = gamma, gamma^2
! l1, l2    = covariant basis vector
! w1, w2    = l1, l2 contravariant basis vector
! redcoeff  = coefficients of the reduction. 5x4 matrix: l1=1...5 x 4 subtopol.
! ******************************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_data_types_/**/REALKIND, only: basis
  implicit none
  complex(REALKIND), intent(in)  :: G_ab(6:15)
  type(basis), intent(in) :: RedBasis
  complex(REALKIND), intent(in) :: m0sq, fk0(2), gammas(2)
  complex(REALKIND), intent(in) :: l1(4), l2(4), w1(4), w2(4)
  complex(REALKIND), intent(out) :: redcoeff(1:5,1:4)

  complex(REALKIND) :: GV34, Gv1Tc(1:4), Gv2Tc(1:4), Gv1Tb(1:4), Gv2Tb(1:4)
  complex(REALKIND) :: Atens(1:2), Btens(1:4,1:4)
  integer :: j

  GV34 = SUM(G_ab*RedBasis%tens3)

  do j = 1, 4
    Gv1Tb(j) = SUM(G_ab*RedBasis%tens4(:,j))
    Gv2Tb(j) = SUM(G_ab*RedBasis%tens5(:,j))
  end do

  Atens(2) = -GV34/(4*gammas(1))
  Atens(1) = m0sq*Atens(2)

  Gv1Tc(1) = 2*w1(1)*G_ab(6) +   w1(2)*G_ab(7)  +   w1(3)*G_ab(8)  +   w1(4)*G_ab(9)
  Gv1Tc(2) =   w1(1)*G_ab(7) + 2*w1(2)*G_ab(10) +   w1(3)*G_ab(11) +   w1(4)*G_ab(12)
  Gv1Tc(3) =   w1(1)*G_ab(8) +   w1(2)*G_ab(11) + 2*w1(3)*G_ab(13) +   w1(4)*G_ab(14)
  Gv1Tc(4) =   w1(1)*G_ab(9) +   w1(2)*G_ab(12) +   w1(3)*G_ab(14) + 2*w1(4)*G_ab(15)

  Gv2Tc(1) = 2*w2(1)*G_ab(6) +   w2(2)*G_ab(7)  +   w2(3)*G_ab(8)  +   w2(4)*G_ab(9)
  Gv2Tc(2) =   w2(1)*G_ab(7) + 2*w2(2)*G_ab(10) +   w2(3)*G_ab(11) +   w2(4)*G_ab(12)
  Gv2Tc(3) =   w2(1)*G_ab(8) +   w2(2)*G_ab(11) + 2*w2(3)*G_ab(13) +   w2(4)*G_ab(14)
  Gv2Tc(4) =   w2(1)*G_ab(9) +   w2(2)*G_ab(12) +   w2(3)*G_ab(14) + 2*w2(4)*G_ab(15)

  do j = 1, 4
    Btens(4,j) = Gv1Tc(j)/gammas(1) + (GV34*l1(j) + Gv1Tb(j))/(2*gammas(2))
    Btens(3,j) = Gv2Tc(j)/gammas(1) + (GV34*l2(j) + Gv2Tb(j))/(2*gammas(2))
    Btens(2,j) = - Btens(3,j) - Btens(4,j)
    Btens(1,j) = fk0(1)*Btens(3,j) + fk0(2)*Btens(4,j)
  end do

  redcoeff(1,3:4) = 0._/**/REALKIND

  ! unpinched OL coefficient
  redcoeff(1,1)   = Atens(1)
  redcoeff(2:5,1) = Btens(1,1:4)

  ! D0-pinch OL coefficient
  redcoeff(1,2)   = Atens(2)
  redcoeff(2:5,2) = Btens(2,1:4)

  ! D1-pinch OL coefficient
  redcoeff(2:5,3) = Btens(3,1:4)

  ! D2-pinch OL coefficient
  redcoeff(2:5,4) = Btens(4,1:4)

end subroutine threepoint_reduction

! ******************************************************************************
subroutine otf_3pt_reduction_last(Gin_A,RedBasis,msq,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_R1,&
A0msq,A0_0,A0_1,A0_2)
! ******************************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_data_types_/**/REALKIND, only: basis
  implicit none
  complex(REALKIND), intent(in)  :: Gin_A(:)
  type(basis), intent(in) :: RedBasis
  complex(REALKIND), intent(in) :: msq(0:2)
  complex(REALKIND), intent(out):: Gout_A(1), Gout_A0(1), Gout_A1(1), Gout_A2(1), Gout_R1
  complex(REALKIND), optional, intent(in) :: A0msq(:)
  complex(REALKIND), optional, intent(out) :: A0_0(1), A0_1(1), A0_2(1)
  complex(REALKIND) :: zero = 0._/**/REALKIND

  Gout_A  = zero
  Gout_A0 = zero
  Gout_A1 = zero
  Gout_A2 = zero
  Gout_R1 = zero

  if(present(A0_2)) A0_2 = zero
  if(present(A0_1)) A0_1 = zero
  if(present(A0_0)) A0_0 = zero

  !! Rank-3 reduction
  if(size(Gin_A)==35) then

    if(present(A0_2)) then
      call otf_3pt_rank3_red_last(Gin_A,RedBasis,msq,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_R1,&
      A0msq, A0_0, A0_1, A0_2)

    else if (present(A0_1)) then
      call otf_3pt_rank3_red_last(Gin_A,RedBasis,msq,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_R1,&
      A0msq, A0_0, A0_1)

    else if(present(A0_0)) then
      call otf_3pt_rank3_red_last(Gin_A,RedBasis,msq,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_R1,&
      A0msq, A0_0)

    else
      call otf_3pt_rank3_red_last(Gin_A,RedBasis,msq,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_R1)

    end if

  end if

  !! Rank-2 reduction
  if(size(Gin_A)==15) then

    if(present(A0_2)) then
      call otf_3pt_rank2_red_last(Gin_A,RedBasis,msq,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_R1,&
      A0msq, A0_0, A0_1, A0_2)

    else if (present(A0_1)) then
      call otf_3pt_rank2_red_last(Gin_A,RedBasis,msq,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_R1,&
      A0msq, A0_0, A0_1)

    else if(present(A0_0)) then
      call otf_3pt_rank2_red_last(Gin_A,RedBasis,msq,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_R1,&
      A0msq, A0_0)

    else
      call otf_3pt_rank2_red_last(Gin_A,RedBasis,msq,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_R1)

    end if

  !! Rank-1 reduction
  else if(size(Gin_A)==5) then
    call otf_3pt_rank1_red_last(Gin_A,RedBasis,msq,Gout_A,Gout_A0,Gout_A1,Gout_A2)
  end if

end subroutine otf_3pt_reduction_last

! ******************************************************************************
subroutine otf_3pt_rank1_red_last(Gin_A,RedBasis,msq,Gout_A,Gout_A0,Gout_A1,Gout_A2)
! ******************************************************************************
! on-the-fly OpenLoops reduction of closed rank-1 3-pt integrals.
! The output is the rank-0 subtopologies
! ------------------------------------------------------------------------------
! Gin_A     = input closed-loop. Only rank-1 in input
! RedBasis  = input reduction basis
! msq       = input squared masses m0^2, m1^2, m2^2
! Gout_Ai   = output rank-1 subtopologies where the Di propagator has been pinched
! ******************************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_data_types_/**/REALKIND, only: basis
  use ol_kinematics_/**/REALKIND, only: get_LC_mass2
  complex(REALKIND), intent(in)  :: Gin_A(5)
  type(basis),       intent(in)  :: RedBasis
  complex(REALKIND), intent(in)  :: msq(0:2)
  complex(REALKIND), intent(out) :: Gout_A(1), Gout_A0(1), Gout_A1(1), Gout_A2(1)

  complex(REALKIND) :: fk0(2), v1(4), v2(4), Gv1, Gv2

  call LC_Cov2Contr(RedBasis%vect1,v1)
  call LC_Cov2Contr(RedBasis%vect2,v2)

  fk0(1) = msq(1) - msq(0) - get_LC_mass2(RedBasis%mom1)
  fk0(2) = msq(2) - msq(0) - get_LC_mass2(RedBasis%mom2)

  Gv1 = 2*SUM(Gin_A(2:5)*v1)
  Gv2 = 2*SUM(Gin_A(2:5)*v2)

  Gout_A1 = Gv2/RedBasis%gamma
  Gout_A2 = Gv1/RedBasis%gamma
  Gout_A0 = - Gout_A2 - Gout_A1
  Gout_A  = fk0(1)*Gout_A1 + fk0(2)*Gout_A2 + Gin_A(1)

end subroutine otf_3pt_rank1_red_last


! ******************************************************************************
subroutine otf_3pt_rank2_red_last(Gin_A,RedBasis,msq,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_R1,&
A0msq, A0_0, A0_1, A0_2)
! ******************************************************************************
! on-the-fly OpenLoops reduction of closed rank-2 3-pt integrals.
! The output are all the scalar subtopologies
! ------------------------------------------------------------------------------
! Gin_A     = input closed-loop. Only rank-2 in input
! RedBasis  = input reduction basis
! msq       = input squared masses m0^2, m1^2, m2^2
! Gout_Ai   = scala bubble subtopologies where the Di propagator has been pinched
! Gout_R1   = rational part
! A0msq     = array of tadpole masses
! A0_i      = tadpole coefficients
! ******************************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_data_types_/**/REALKIND, only: basis
  use ol_loop_handling_/**/REALKIND, only: G_TensorShift_otf
  use ol_kinematics_/**/REALKIND, only: get_LC_4, get_LC_5, get_LC_mass2
  use ol_debug, only: ol_error, ol_msg
  complex(REALKIND), intent(in)  :: Gin_A(15)
  type(basis), intent(in) :: RedBasis
  complex(REALKIND), intent(in) :: msq(0:2)
  complex(REALKIND), intent(out) :: Gout_A(1), Gout_A0(1), Gout_A1(1), Gout_A2(1)
  complex(REALKIND), intent(out) :: Gout_R1
  complex(REALKIND), optional, intent(in) :: A0msq(:)
  complex(REALKIND), optional, intent(out) :: A0_0(1), A0_1(1), A0_2(1)

  complex(REALKIND) :: fk0(2), gammas(2), l1(4), l2(4), v1(4), v2(4), RedCoeff(5,4)
  complex(REALKIND) :: Gtmp_A(5), Gtmp_A0(5), Gtmp_A1(5), Gtmp_A2(5), Gtmp_A3(5)
  complex(REALKIND) :: B0_0(1), B0_1(1), B0_2(1), k2(5), B0coeff(1:4,0:2)
  complex(REALKIND) :: zero = 0._/**/REALKIND

  !! ------------ Reduction of the rank-2 triangle

  !! gamma and gamma^2
  gammas(1) = RedBasis%gamma
  gammas(2) = gammas(1)**2

  !! l1_{\mu}, l2_{\mu}
  l1 = RedBasis%vect1
  l2 = RedBasis%vect2

  !! l1^{\mu}, l2^{\mu}
  call LC_Cov2Contr(l1,v1)
  call LC_Cov2Contr(l2,v2)

  !! f_{k0} = {m_k}^2 - {m_0}^2 - {p_k}^2
  fk0(1) = msq(1) - msq(0) - get_LC_mass2(RedBasis%mom1)
  fk0(2) = msq(2) - msq(0) - get_LC_mass2(RedBasis%mom2)

  call threepoint_reduction(Gin_A(6:15),RedBasis,msq(0),fk0,gammas,&
  l1,l2,v1,v2,RedCoeff)

  !! unpinched rank-1 triangle
  Gtmp_A(1:5)  = RedCoeff(1:5,1) + Gin_A(1:5)

  ! rank-1 bubbles. Output of rank-2 triangle reduction
  Gtmp_A0 = RedCoeff(:,2)
  Gtmp_A1 = RedCoeff(:,3)
  Gtmp_A2 = RedCoeff(:,4)

  !! Rational contribution from rank-2 triangle reduction
  Gout_R1 = Gtmp_A0(1)/2

  !! Reduction of the rank-1 triangle
  call otf_3pt_rank1_red_last(Gtmp_A,RedBasis,msq,Gout_A,B0_0,B0_1,B0_2)

  !! The scalar part of the bubbles is merged
  Gtmp_A0(1) = Gtmp_A0(1) + B0_0(1)
  Gtmp_A1(1) = Gtmp_A1(1) + B0_1(1)
  Gtmp_A2(1) = Gtmp_A2(1) + B0_2(1)

  !! Reduction of the rank-1 bubbles
  k2 = get_LC_5(RedBasis%mom2-RedBasis%mom1)
  call G_TensorShift_otf(Gtmp_A0,-get_LC_4(RedBasis%mom1))
  call twopoint_reduction(Gtmp_A0,k2,(/msq(1),msq(2)/),B0coeff(:,0))
  call twopoint_reduction(Gtmp_A1,get_LC_5(RedBasis%mom2),(/msq(0),msq(2)/),B0coeff(:,1))
  call twopoint_reduction(Gtmp_A2,get_LC_5(RedBasis%mom1),(/msq(0),msq(1)/),B0coeff(:,2))

  Gout_A0(1) =  B0coeff(1,0)
  Gout_A1(1) =  B0coeff(1,1)
  Gout_A2(1) =  B0coeff(1,2)

  !! Rational contribution from rank-1 bubbles reduction
  Gout_R1 = Gout_R1 + B0coeff(4,0) + B0coeff(4,1) + B0coeff(4,2)

  !! Tadpoles
  if(present(A0msq)) then
    if (size(A0msq) == 3) then
      if(A0msq(1) == 0._/**/REALKIND) then
        A0_0 = zero
      else
        A0_0 = A0msq(1)*(B0coeff(2,1) + B0coeff(2,2))
      end if
      if(A0msq(2) == 0._/**/REALKIND) then
        A0_1 = zero
      else
        A0_1 = A0msq(2)*(B0coeff(2,0) + B0coeff(3,2))
      end if
      if(A0msq(3) == 0._/**/REALKIND) then
        A0_2 = zero
      else
        A0_2 = A0msq(3)*(B0coeff(3,0) + B0coeff(3,1))
      end if
    else if (size(A0msq) == 2) then
      if(A0msq(1) == 0._/**/REALKIND .AND. A0msq(2) == 0._/**/REALKIND) then
        A0_0 = zero
        A0_1 = zero
      else if(A0msq(1) == zero) then
        A0_0 = zero
        call tadpole_assignment(msq,A0msq(2),B0coeff,A0_1)
      else if(A0msq(2) == 0._/**/REALKIND) then
        A0_1 = zero
        call tadpole_assignment(msq,A0msq(1),B0coeff,A0_0)
      else
        call tadpole_assignment(msq,A0msq(1),B0coeff,A0_0)
        call tadpole_assignment(msq,A0msq(2),B0coeff,A0_1)
      end if
    else if (size(A0msq) == 1) then
      if (A0msq(1) == 0._/**/REALKIND ) then
        A0_0 = zero
      else
        call tadpole_assignment(msq,A0msq(1),B0coeff,A0_0)
      end if
    end if
  end if

end subroutine otf_3pt_rank2_red_last


! ******************************************************************************
subroutine otf_3pt_rank3_red_last(Gin_A,RedBasis,msq,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_R1,&
A0msq, A0_0, A0_1, A0_2)
! ******************************************************************************
! on-the-fly OpenLoops reduction of closed rank-3 3-pt integrals.
! The output are all the scalar subtopologies
! ------------------------------------------------------------------------------
! Gin_A     = input closed-loop. Only rank-2 in input
! RedBasis  = input reduction basis
! msq       = input squared masses m0^2, m1^2, m2^2
! Gout_Ai   = scalar bubbles subtopologies where the Di propagator has been pinched
! Gout_R1   = rational part
! A0msq     = array of tadpole masses
! A0_i      = tadpole coefficients
! ******************************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_data_types_/**/REALKIND, only: basis
  use ol_kinematics_/**/REALKIND, only: get_LC_4, get_LC_5, get_LC_mass2
  use ol_loop_handling_/**/REALKIND, only: G_TensorShift_otf
  use ol_debug, only: ol_error, ol_msg
  complex(REALKIND), intent(in)  :: Gin_A(35)
  type(basis), intent(in) :: RedBasis
  complex(REALKIND), intent(in) :: msq(0:2)
  complex(REALKIND), intent(out) :: Gout_A(1), Gout_A0(1), Gout_A1(1), Gout_A2(1)
  complex(REALKIND), intent(out) :: Gout_R1
  complex(REALKIND), optional, intent(in) :: A0msq(:)
  complex(REALKIND), optional, intent(out) :: A0_0(1), A0_1(1), A0_2(1)

  !rank2 tensors factored out from the input rank-3 ones
  complex(REALKIND) :: G_r2(6:15,4), RedCoeff1(5,4), RedCoeff2(5,4), RedCoeff3(5,4), RedCoeff4(5,4)
  !rank2 subtopologies resulting from rank-3 3-p reduction
  complex(REALKIND), dimension(15) :: Sub_A, Sub_A0, Sub_A1, Sub_A2
  complex(REALKIND) :: tad_0(1), tad_1(1), tad_2(1)
  !various rational contributions
  complex(REALKIND) :: R1_term(5)
  complex(REALKIND) :: fk0(2), gammas(2), l1(4), l2(4), v1(4), v2(4), p12(4)
  complex(REALKIND) :: Gtmp_A(5), Gtmp_A0(5), Gtmp_A1(5), Gtmp_A2(5), Gtmp_A3(5)
  complex(REALKIND) :: B0_0(1), B0_1(1), B0_2(1), k2(5), B0coeff(1:4,0:2)
  integer :: i

  !! ------------ Reduction of the rank-3 triangle ------------

  p12 = get_LC_4(RedBasis%mom1) + get_LC_4(RedBasis%mom2)

  !! gamma and gamma^2
  gammas(1) = RedBasis%gamma
  gammas(2) = gammas(1)**2

  !! l1_{\mu}, l2_{\mu}
  l1 = RedBasis%vect1
  l2 = RedBasis%vect2

  !! l1^{\mu}, l2^{\mu}
  call LC_Cov2Contr(l1,v1)
  call LC_Cov2Contr(l2,v2)

  !! f_{k0} = {m_k}^2 - {m_0}^2 - {p_k}^2
  fk0(1) = msq(1) - msq(0) - get_LC_mass2(RedBasis%mom1)
  fk0(2) = msq(2) - msq(0) - get_LC_mass2(RedBasis%mom2)

  !! rank-2 component of the rank-3 numerator, i.e. q^\mu q^\nu q^rho = q^\mu (q^\nu q^rho)
  G_r2 = 0._/**/REALKIND
  G_r2(6:15,1)  = Gin_A(16:25)
  G_r2(10:15,2) = Gin_A(26:31)
  G_r2(13:15,3) = Gin_A(32:34)
  G_r2(15,4)    = Gin_A(35)

  call threepoint_reduction(G_r2(6:15,1),RedBasis,msq(0),fk0,gammas,&
  l1,l2,v1,v2,RedCoeff1(:,:))

  call threepoint_reduction(G_r2(6:15,2),RedBasis,msq(0),fk0,gammas,&
  l1,l2,v1,v2,RedCoeff2(:,:))

  call threepoint_reduction(G_r2(6:15,3),RedBasis,msq(0),fk0,gammas,&
  l1,l2,v1,v2,RedCoeff3(:,:))

  call threepoint_reduction(G_r2(6:15,4),RedBasis,msq(0),fk0,gammas,&
  l1,l2,v1,v2,RedCoeff4(:,:))

  !! Recombination of the rank-1 factorized part and the rank-1 part resulting from the reduction.
  !! q^\mu (q^\nu q^rho) --> q^\mu (A^{\nu\rho} + B^{\nu\rho}_\lambda q^\lambda)

  !! Child rank-2 triangle
  Sub_A(1) = Gin_A(1)
  Sub_A(2) = Gin_A(2) + RedCoeff1(1,1)
  Sub_A(3) = Gin_A(3) + RedCoeff2(1,1)
  Sub_A(4) = Gin_A(4) + RedCoeff3(1,1)
  Sub_A(5) = Gin_A(5) + RedCoeff4(1,1)
  call r1r1_combo((/RedCoeff1(2:5,1),RedCoeff2(2:5,1),RedCoeff3(2:5,1),RedCoeff4(2:5,1)/),Sub_A(6:15))
  Sub_A(6:15) = Sub_A(6:15) + Gin_A(6:15)

  !! rank-2 D0-pinched bubble
  Sub_A0(1) = 0._/**/REALKIND
  Sub_A0(2) = RedCoeff1(1,2)
  Sub_A0(3) = RedCoeff2(1,2)
  Sub_A0(4) = RedCoeff3(1,2)
  Sub_A0(5) = RedCoeff4(1,2)
  call r1r1_combo((/RedCoeff1(2:5,2),RedCoeff2(2:5,2),RedCoeff3(2:5,2),RedCoeff4(2:5,2)/),Sub_A0(6:15))

  !! rank-2 D1-pinched bubble
  Sub_A1(1) = 0._/**/REALKIND
  Sub_A1(2) = RedCoeff1(1,3)
  Sub_A1(3) = RedCoeff2(1,3)
  Sub_A1(4) = RedCoeff3(1,3)
  Sub_A1(5) = RedCoeff4(1,3)
  call r1r1_combo((/RedCoeff1(2:5,3),RedCoeff2(2:5,3),RedCoeff3(2:5,3),RedCoeff4(2:5,3)/),Sub_A1(6:15))

  !! rank-2 D2-pinched bubble
  Sub_A2(1) = 0._/**/REALKIND
  Sub_A2(2) = RedCoeff1(1,4)
  Sub_A2(3) = RedCoeff2(1,4)
  Sub_A2(4) = RedCoeff3(1,4)
  Sub_A2(5) = RedCoeff4(1,4)
  call r1r1_combo((/RedCoeff1(2:5,4),RedCoeff2(2:5,4),RedCoeff3(2:5,4),RedCoeff4(2:5,4)/),Sub_A2(6:15))

  !! R1-part
  R1_term(2:5) = - Sub_A0(2:5)

  !! ------------ Reduction of the rank-2 triangle ------------
  if (present(A0_2)) then
    call otf_3pt_rank2_red_last(Sub_A,RedBasis,msq,Gout_A,Gout_A0,Gout_A1,Gout_A2,R1_term(1),&
      A0msq, tad_0, tad_1, tad_2)

  else if (present(A0_1)) then
    call otf_3pt_rank2_red_last(Sub_A,RedBasis,msq,Gout_A,Gout_A0,Gout_A1,Gout_A2,R1_term(1),&
      A0msq, tad_0, tad_1)

  else if (present(A0_0)) then
    call otf_3pt_rank2_red_last(Sub_A,RedBasis,msq,Gout_A,Gout_A0,Gout_A1,Gout_A2,R1_term(1),&
      A0msq, tad_0)

  else
    call otf_3pt_rank2_red_last(Sub_A,RedBasis,msq,Gout_A,Gout_A0,Gout_A1,Gout_A2,R1_term(1))

  end if

  !! ------------ Reduction of the rank-2 bubbles ------------
  k2 = get_LC_5(RedBasis%mom2-RedBasis%mom1)
  call G_TensorShift_otf(Sub_A0,-get_LC_4(RedBasis%mom1))
  call twopoint_reduction(Sub_A0,k2,(/msq(1),msq(2)/),B0coeff(:,0))
  call twopoint_reduction(Sub_A1,get_LC_5(RedBasis%mom2),(/msq(0),msq(2)/),B0coeff(:,1))
  call twopoint_reduction(Sub_A2,get_LC_5(RedBasis%mom1),(/msq(0),msq(1)/),B0coeff(:,2))

  !! Scalar Bubbles
  Gout_A0 = B0coeff(1,0) + Gout_A0(1)
  Gout_A1 = B0coeff(1,1) + Gout_A1(1)
  Gout_A2 = B0coeff(1,2) + Gout_A2(1)

  !! Rational Part
  Gout_R1 = R1_term(1) + SUM(p12(1:4)*R1_term(2:5))/6 + B0coeff(4,0) + B0coeff(4,1) + B0coeff(4,2)

  !! Tadpoles
  if(present(A0msq)) then
    if (size(A0msq) == 3) then
        call tadpole_assignment(msq,A0msq(1),B0coeff,A0_0)
        call tadpole_assignment(msq,A0msq(2),B0coeff,A0_1)
        call tadpole_assignment(msq,A0msq(3),B0coeff,A0_2)
    else if (size(A0msq) == 2) then
      if(A0msq(1) == 0._/**/REALKIND .AND. A0msq(2) == 0._/**/REALKIND) then
        A0_0 = 0._/**/REALKIND
        A0_1 = 0._/**/REALKIND
      else if(A0msq(1) == 0._/**/REALKIND) then
        A0_0 = 0._/**/REALKIND
        call tadpole_assignment(msq,A0msq(2),B0coeff,A0_1)
      else if(A0msq(2) == 0._/**/REALKIND) then
        A0_1 = 0._/**/REALKIND
        call tadpole_assignment(msq,A0msq(1),B0coeff,A0_0)
      else
        call tadpole_assignment(msq,A0msq(1),B0coeff,A0_0)
        call tadpole_assignment(msq,A0msq(2),B0coeff,A0_1)
      end if
    else if (size(A0msq) == 1) then
      if (A0msq(1) == 0._/**/REALKIND ) then
        A0_0 = 0._/**/REALKIND
      else
        call tadpole_assignment(msq,A0msq(1),B0coeff,A0_0)
      end if
    end if
  end if

  if (present(A0_2)) then
    A0_0 = A0_0 + tad_0
    A0_1 = A0_1 + tad_1
    A0_2 = A0_2 + tad_2
  else if (present(A0_1)) then
    A0_0 = A0_0 + tad_0
    A0_1 = A0_1 + tad_1
  else if (present(A0_0)) then
    A0_0 = A0_0 + tad_0
  end if

end subroutine otf_3pt_rank3_red_last


! =============================================================================
!                     Reduction of 4-point integrand/ls
! -----------------------------------------------------------------------------
! OTF Reduction subroutines for open and closed 4-point integrands/integrals
! =============================================================================

! ******************************************************************************
subroutine fourpoint_reduction(G_ab,RedBasis,p3scalars,m0sq,fk0,gammas,&
l1,l2,w1,w2,redcoeff)
! ******************************************************************************
! on-the-fly OpenLoops reduction. 4-point functions or higher
! ------------------------------------------------------------------------------
! G_ab      = input open-loop, a(lpha) b(eta) components. Only rank-2 in input
! RedBasis  = input reduction basis
! p3scalars = (p3.l3), (p3.l4), (p3.r1), (p3.r2)
! m0sq      = m0^2
! fk0       = (m_k)^2 - (m_0)^2 - (p_k)^2
! gammas(2) = gamma, gamma^2
! l1, l2    = covariant basis vector
! w1, w2    = l1, l2 contravariant basis vector
! redcoeff  = coefficients of the reduction. 5x4 matrix: l1=1...5 x 5 subtopol.
! ******************************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_data_types_/**/REALKIND, only: basis
  implicit none
  complex(REALKIND), intent(in)  :: G_ab(6:15)
  type(basis), intent(in) :: RedBasis
  complex(REALKIND), intent(in) :: m0sq, p3scalars(0:4), fk0(3), gammas(2)
  complex(REALKIND), intent(in) :: l1(4), l2(4), w1(4), w2(4)
  complex(REALKIND), dimension(5,5), intent(out) :: redcoeff

  complex(REALKIND) :: p3l3, sc1, sc2, alpha, GL33, GL44, GL34, GTa
  complex(REALKIND) :: Gv1Tb(1:4), Gv2Tb(1:4), Gv1Tc(1:4), Gv2Tc(1:4), A_munu(2), Btens(4,5)
  integer :: k, j

  !! The following corresponds to (p3.l3)^(-1)
  p3l3 = p3scalars(0)

  sc1   = p3scalars(1)
  sc2   = p3scalars(2)
  alpha = p3scalars(3)

  !! G_{\mu\nu} L_ij^{\mu\nu}
  GL33 = SUM(G_ab*RedBasis%tens1)
  GL44 = SUM(G_ab*RedBasis%tens2)
  GL34 = SUM(G_ab*RedBasis%tens3)

  GTa = alpha*GL33 + GL44/alpha - GL34

  do k = 1, 4
    Gv1Tb(k) = SUM(G_ab*RedBasis%tens4(:,k))
    Gv2Tb(k) = SUM(G_ab*RedBasis%tens5(:,k))
  end do

  ! r_1^\mu \delta^\nu_\lambda + r_1^\nu \delta^\mu_\lambda
  Gv1Tc(1) = 2*w1(1)*G_ab(6) +   w1(2)*G_ab(7)  +   w1(3)*G_ab(8)  +   w1(4)*G_ab(9)
  Gv1Tc(2) =   w1(1)*G_ab(7) + 2*w1(2)*G_ab(10) +   w1(3)*G_ab(11) +   w1(4)*G_ab(12)
  Gv1Tc(3) =   w1(1)*G_ab(8) +   w1(2)*G_ab(11) + 2*w1(3)*G_ab(13) +   w1(4)*G_ab(14)
  Gv1Tc(4) =   w1(1)*G_ab(9) +   w1(2)*G_ab(12) +   w1(3)*G_ab(14) + 2*w1(4)*G_ab(15)

  ! r_2^\mu \delta^\nu_\lambda + r_2^\nu \delta^\mu_\lambda (1 <--> 2)
  Gv2Tc(1) = 2*w2(1)*G_ab(6) +   w2(2)*G_ab(7)  +   w2(3)*G_ab(8)  +   w2(4)*G_ab(9)
  Gv2Tc(2) =   w2(1)*G_ab(7) + 2*w2(2)*G_ab(10) +   w2(3)*G_ab(11) +   w2(4)*G_ab(12)
  Gv2Tc(3) =   w2(1)*G_ab(8) +   w2(2)*G_ab(11) + 2*w2(3)*G_ab(13) +   w2(4)*G_ab(14)
  Gv2Tc(4) =   w2(1)*G_ab(9) +   w2(2)*G_ab(12) +   w2(3)*G_ab(14) + 2*w2(4)*G_ab(15)

  A_munu(2) = GTa/(4._/**/REALKIND*gammas(1)) ! D0-pinch contribution
  A_munu(1) = m0sq*A_munu(2)                  ! unpinched contribution

  Btens(:,5) = (GL44/alpha)*RedBasis%vect3(:) + GL33*RedBasis%vect4(:)                    ! D3-pinch
  Btens(:,4) = (sc1*Btens(:,5) - GTa*l1(:) + Gv1Tb(:))/(2*gammas(2)) + Gv1Tc(:)/gammas(1) ! D2-pinch
  Btens(:,3) = (sc2*Btens(:,5) - GTa*l2(:) + Gv2Tb(:))/(2*gammas(2)) + Gv2Tc(:)/gammas(1) ! D1-pinch
  Btens(:,5) = (-Btens(:,5)*p3l3)/(2*gammas(1))                                           ! D3-pinch
  Btens(:,2) = - Btens(:,3) - Btens(:,4) - Btens(:,5)                                     ! D0-pinch
  Btens(:,1) = fk0(1)*Btens(:,3) + fk0(2)*Btens(:,4) + fk0(3)*Btens(:,5)                  ! unpinched

  redcoeff(1,3:5) = 0._/**/REALKIND

  ! unpinched OL coefficient
  redcoeff(1,1)   = A_munu(1)
  redcoeff(2:5,1) = Btens(1:4,1)

  ! D0-pinch OL coefficient
  redcoeff(1,2)   = A_munu(2)
  redcoeff(2:5,2) = Btens(1:4,2)

  ! D1-pinch OL coefficient
  redcoeff(2:5,3) = Btens(1:4,3)

  ! D2-pinch OL coefficient
  redcoeff(2:5,4) = Btens(1:4,4)

  ! D3-pinch OL coefficient
  redcoeff(2:5,5) = Btens(1:4,5)

end subroutine fourpoint_reduction


!****************************************************************************************
subroutine fourpoint_reduction_OL(G_a,RedBasis,p3scalars,m0sq,fk0,gammas,&
l1,l2,w1,w2,redcoeff)
!****************************************************************************************
! on-the-fly OpenLoops reduction. 4-point functions or higher. The loop is open
! ---------------------------------------------------------------------------------------
! G_a       = input open-loop, a(lpha). Only rank-2 in input
! RedBasis  = input reduction basis
! p3scalars = (p3.l3), (p3.l4), (p3.r1), (p3.r2)
! m0sq      = m0^2
! fk0       = (m_k)^2 - (m_0)^2 - (p_k)^2
! gammas(2) = gamma, gamma^2
! l1, l2    = covariant basis vector
! w1, w2    = l1, l2 contravariant basis vector
! redcoeff  = coefficients of the reduction. 5x4 matrix: l1=1...5 x 5 subtopol.
! ***************************************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_data_types_/**/REALKIND, only: basis
  implicit none
  complex(REALKIND), intent(in)  :: G_a(4,15)
  type(basis), intent(in) :: RedBasis
  complex(REALKIND), intent(in) :: m0sq, p3scalars(0:4), fk0(3), gammas(2)
  complex(REALKIND), intent(in) :: l1(4), l2(4), w1(4), w2(4)
  complex(REALKIND), intent(out) :: redcoeff(4,5,5)

  complex(REALKIND) :: p3l3, sc1, sc2, alpha, GL33(4), GL44(4), GL34(4), GTa(4)
  complex(REALKIND) :: Gv1Tb(4,4), Gv2Tb(4,4), Gv1Tc(4,4), Gv2Tc(4,4), A_munu(4,2), Btens(4,4,5)
  integer :: k, j

  !! The following corresponds to (p3.l3)^(-1)
  p3l3 = p3scalars(0)
  sc1   = p3scalars(1)
  sc2   = p3scalars(2)
  alpha = p3scalars(3)

  !! G_{\mu\nu} L_ij^{\mu\nu}
  do k = 1,4
    GL33(k) = SUM(G_a(k,6:15)*RedBasis%tens1)
    GL44(k) = SUM(G_a(k,6:15)*RedBasis%tens2)
    GL34(k) = SUM(G_a(k,6:15)*RedBasis%tens3)
  end do

  GTa = alpha*GL33 + GL44/alpha - GL34

  do k = 1, 4
    do j = 1, 4
      Gv1Tb(j,k) = SUM(G_a(j,6:15)*RedBasis%tens4(:,k))
      Gv2Tb(j,k) = SUM(G_a(j,6:15)*RedBasis%tens5(:,k))
    end do
  end do

  ! r_1^\mu \delta^\nu_\lambda + r_1^\nu \delta^\mu_\lambda
  Gv1Tc(:,1) = 2*w1(1)*G_a(:,6) +   w1(2)*G_a(:,7)  +   w1(3)*G_a(:,8)  +   w1(4)*G_a(:,9)
  Gv1Tc(:,2) =   w1(1)*G_a(:,7) + 2*w1(2)*G_a(:,10) +   w1(3)*G_a(:,11) +   w1(4)*G_a(:,12)
  Gv1Tc(:,3) =   w1(1)*G_a(:,8) +   w1(2)*G_a(:,11) + 2*w1(3)*G_a(:,13) +   w1(4)*G_a(:,14)
  Gv1Tc(:,4) =   w1(1)*G_a(:,9) +   w1(2)*G_a(:,12) +   w1(3)*G_a(:,14) + 2*w1(4)*G_a(:,15)

  ! r_2^\mu \delta^\nu_\lambda + r_2^\nu \delta^\mu_\lambda (1 <--> 2)
  Gv2Tc(:,1) = 2*w2(1)*G_a(:,6) +   w2(2)*G_a(:,7)  +   w2(3)*G_a(:,8)  +   w2(4)*G_a(:,9)
  Gv2Tc(:,2) =   w2(1)*G_a(:,7) + 2*w2(2)*G_a(:,10) +   w2(3)*G_a(:,11) +   w2(4)*G_a(:,12)
  Gv2Tc(:,3) =   w2(1)*G_a(:,8) +   w2(2)*G_a(:,11) + 2*w2(3)*G_a(:,13) +   w2(4)*G_a(:,14)
  Gv2Tc(:,4) =   w2(1)*G_a(:,9) +   w2(2)*G_a(:,12) +   w2(3)*G_a(:,14) + 2*w2(4)*G_a(:,15)

  A_munu(:,2) = GTa(:)/(4*gammas(1))              ! D0-pinch contribution
  A_munu(:,1) = m0sq*A_munu(:,2)                  ! unpinched contribution

  do k = 1, 4
    Btens(:,k,5) = (GL44(:)/alpha)*RedBasis%vect3(k) + GL33(:)*RedBasis%vect4(k)                       ! D3-pinch
    Btens(:,k,4) = (sc1*Btens(:,k,5) - GTa(:)*l1(k) + Gv1Tb(:,k))/(2*gammas(2)) + Gv1Tc(:,k)/gammas(1) ! D2-pinch
    Btens(:,k,3) = (sc2*Btens(:,k,5) - GTa(:)*l2(k) + Gv2Tb(:,k))/(2*gammas(2)) + Gv2Tc(:,k)/gammas(1) ! D1-pinch
    Btens(:,k,5) = (-Btens(:,k,5)*p3l3)/(2*gammas(1))                                                  ! D3-pinch
    Btens(:,k,2) = - Btens(:,k,3) - Btens(:,k,4) - Btens(:,k,5)                                        ! D0-pinch
    Btens(:,k,1) = fk0(1)*Btens(:,k,3) + fk0(2)*Btens(:,k,4) + fk0(3)*Btens(:,k,5)                     ! unpinched
  end do

  redcoeff(:,1,3:5) = 0._/**/REALKIND

  ! unpinched OL coefficient
  redcoeff(:,1,1)   = A_munu(:,1)
  redcoeff(:,2:5,1) = Btens(:,1:4,1)

  ! D0-pinch OL coefficient
  redcoeff(:,1,2)   = A_munu(:,2)
  redcoeff(:,2:5,2) = Btens(:,1:4,2)

  ! D1-pinch OL coefficient
  redcoeff(:,2:5,3) = Btens(:,1:4,3)

  ! D2-pinch OL coefficient
  redcoeff(:,2:5,4) = Btens(:,1:4,4)

  ! D3-pinch OL coefficient
  redcoeff(:,2:5,5) = Btens(:,1:4,5)

end subroutine fourpoint_reduction_OL


! **********************************************************************************
subroutine otf_4pt_red(Gin_A,RedSet_4,msq_in,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3,Gout_R1)
! ----------------------------------------------------------------------------------
! OpenLoops on-the-fly reduction step. Reduction of an open rank-2 4-pt segment
! ----------------------------------------------------------------------------------
! Gin_A    = input open-loop
! RedSet_4 = input Reduction Set containig the basis and p3scalars
! msq_in   = array of the squared masses m0^2, m1^2, m2^2, m3^2
! Gout_A   = unpinched reduced open-loop
! Gout_Ai  = output reduced open-loops. Subtopology with Di propagator pinched
! Gout_R1  = R1-rational contribution. Present for UV divergent integrals
! **********************************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_data_types_/**/REALKIND, only: basis, redset4
  use ol_kinematics_/**/REALKIND, only: get_LC_mass2
  implicit none
  complex(REALKIND), dimension(4,15,4), intent(in) :: Gin_A
  type(redset4), intent(in) :: RedSet_4
  complex(REALKIND), intent(in) :: msq_in(0:3)
  complex(REALKIND), dimension(4,5,4), intent(out) :: Gout_A, Gout_A0, Gout_A1, Gout_A2, Gout_A3
  complex(REALKIND), optional, intent(out) :: Gout_R1(4,1,4)

  complex(REALKIND), dimension(4,5,4) :: Gtmp_A1, Gtmp_A2, Gtmp_A3
  type(basis) :: Red_Basis
  complex(REALKIND) :: gammas(2), p3_scalars(0:4), zero = 0._/**/REALKIND
  integer :: a, b, l
  logical :: perm(3), zerocheck
  complex(REALKIND) :: fk0(3), msq(0:3), RedCoeff(4,5,5), l1(4), l2(4), v1(4), v2(4)

  Red_Basis = RedSet_4%redbasis
  p3_scalars = RedSet_4%p3scalars

  perm(1) = (RedSet_4%perm(1)+RedSet_4%perm(2)) == 3 !! 1,2,3 permutation
  perm(2) = (RedSet_4%perm(1)+RedSet_4%perm(2)) == 4 !! 1,3,2 permutation
  perm(3) = (RedSet_4%perm(1)+RedSet_4%perm(2)) == 5 !! 2,3,1 permutation

  !! Assignment of internal masses according to the permutation in the basis
  msq(0) = msq_in(0)
  if(perm(1)) then
    msq = msq_in
  else if (perm(2)) then
    msq(1) = msq_in(1)
    msq(2) = msq_in(3)
    msq(3) = msq_in(2)
  else if(perm(3)) then
    msq(1) = msq_in(2)
    msq(2) = msq_in(3)
    msq(3) = msq_in(1)
  end if

  !! f_{k0} = {m_k}^2 - {m_0}^2 - {p_k}^2
  fk0(1) = msq(1) - msq(0) - get_LC_mass2(Red_Basis%mom1)
  fk0(2) = msq(2) - msq(0) - get_LC_mass2(Red_Basis%mom2)
  fk0(3) = msq(3) - msq(0) - p3_scalars(4)

  !! gamma and gamma^2
  gammas(1) = Red_Basis%gamma
  gammas(2) = gammas(1)**2

  !! l1_{\mu}, l2_{\mu}
  l1 = Red_Basis%vect1
  l2 = Red_Basis%vect2

  !! l1^{\mu}, l2^{\mu}
  call LC_Cov2Contr(l1,v1)
  call LC_Cov2Contr(l2,v2)

  do a = 1, 4

    call fourpoint_reduction_OL(Gin_A(:,:,a),Red_Basis,p3_scalars,msq(0),fk0,gammas,&
    l1,l2,v1,v2,RedCoeff)

    Gout_A(:,1:5,a) = RedCoeff(:,1:5,1) + Gin_A(:,1:5,a)

    Gout_A0(:,:,a) = RedCoeff(:,:,2)
    Gtmp_A1(:,:,a) = RedCoeff(:,:,3)
    Gtmp_A2(:,:,a) = RedCoeff(:,:,4)
    Gtmp_A3(:,:,a) = RedCoeff(:,:,5)

  end do

  if(perm(1)) then
    Gout_A1 = Gtmp_A1
    Gout_A2 = Gtmp_A2
    Gout_A3 = Gtmp_A3
  else if (perm(2)) then
    Gout_A1 = Gtmp_A1
    Gout_A2 = Gtmp_A3
    Gout_A3 = Gtmp_A2
  else
    Gout_A1 = Gtmp_A3
    Gout_A2 = Gtmp_A1
    Gout_A3 = Gtmp_A2
  end if

  !! Assignment of the rational R1 contribution
  if(present(Gout_R1)) Gout_R1(:,1,:) = - Gout_A0(:,1,:)

end subroutine otf_4pt_red

! ******************************************************************************
subroutine otf_4pt_reduction_last(Gin_A,RedSet_4,msq_in,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3,&
                                  Gout_R1)
! ******************************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_data_types_/**/REALKIND, only: redset4
  implicit none
  complex(REALKIND), intent(in)  :: Gin_A(:)
  type(redset4), intent(in) :: RedSet_4
  complex(REALKIND), intent(in) :: msq_in(0:3)
  complex(REALKIND), intent(out) :: Gout_A(:), Gout_A0(:), Gout_A1(:), Gout_A2(:), Gout_A3(:)
  complex(REALKIND), optional, intent(out) :: Gout_R1

  !! Rank-2 reduction
  if(size(Gin_A)==15) then
    if(present(Gout_R1)) then
      call otf_4pt_rank2_red_last(Gin_A,RedSet_4,msq_in,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3,Gout_R1)
    else
      call otf_4pt_rank2_red_last(Gin_A,RedSet_4,msq_in,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3)
    end if

  !! Rank-1 reduction
  else if(size(Gin_A)==5) then
    call otf_4pt_rank1_red_last(Gin_A,RedSet_4,msq_in,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3)

  end if

end subroutine otf_4pt_reduction_last

! ******************************************************************************
subroutine otf_4pt_rank2_red_last(Gin_A,RedSet_4,msq_in,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3,Gout_R1)
! ----------------------------------------------------------------------------------
! OpenLoops on-the-fly reduction step. Reduction of a closed rank-2 4-pt segment.
! No R1-rational terms contributing to a rank-2 box
! ----------------------------------------------------------------------------------
! Gin_A    = input open-loop
! RedSet_4 = input Reduction Set containig the basis and p3scalars
! msq_in   = array of the squared masses m0^2, m1^2, m2^2, m3^2
! Gout_A   = unpinched reduced open-loop
! Gout_Ai  = output reduced open-loops. Subtopology with Di propagator pinched
! **********************************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_data_types_/**/REALKIND, only: basis, redset4
  use ol_kinematics_/**/REALKIND, only: get_LC_mass2
  implicit none
  complex(REALKIND), intent(in)  :: Gin_A(15)
  type(redset4), intent(in) :: RedSet_4
  complex(REALKIND), intent(in) :: msq_in(0:3)
  complex(REALKIND), intent(out), dimension(5) :: Gout_A, Gout_A0, Gout_A1, Gout_A2, Gout_A3
  complex(REALKIND), optional, intent(out) :: Gout_R1

  complex(REALKIND) :: RedCoeff(5,5), fk0(3), p3_scalars(0:4), msq(0:3), gammas(2)
  complex(REALKIND) :: l1(4), l2(4), v1(4), v2(4)
  complex(REALKIND), dimension(5) :: Gtmp_A1, Gtmp_A2, Gtmp_A3
  type(basis) :: Red_Basis
  logical :: perm(3)

  Red_Basis = RedSet_4%redbasis
  p3_scalars = RedSet_4%p3scalars

  perm(1) = (RedSet_4%perm(1)+RedSet_4%perm(2)) == 3 !! 1,2,3 permutation
  perm(2) = (RedSet_4%perm(1)+RedSet_4%perm(2)) == 4 !! 1,3,2 permutation
  perm(3) = (RedSet_4%perm(1)+RedSet_4%perm(2)) == 5 !! 2,3,1 permutation

  msq(0) = msq_in(0)
  if(perm(1)) then
    msq = msq_in
  else if (perm(2)) then
    msq(1) = msq_in(1)
    msq(2) = msq_in(3)
    msq(3) = msq_in(2)
  else if(perm(3)) then
    msq(1) = msq_in(2)
    msq(2) = msq_in(3)
    msq(3) = msq_in(1)
  end if

  !! f_{k0} = {m_k}^2 - {m_0}^2 - {p_k}^2
  fk0(1) = msq(1) - msq(0) - get_LC_mass2(Red_Basis%mom1)
  fk0(2) = msq(2) - msq(0) - get_LC_mass2(Red_Basis%mom2)
  fk0(3) = msq(3) - msq(0) - p3_scalars(4)

  !! gamma and gamma^2
  gammas(1) = Red_Basis%gamma
  gammas(2) = gammas(1)**2

  !! l1_{\mu}, l2_{\mu}
  l1 = Red_Basis%vect1
  l2 = Red_Basis%vect2

  !! l1^{\mu}, l2^{\mu}
  call LC_Cov2Contr(l1,v1)
  call LC_Cov2Contr(l2,v2)

  call fourpoint_reduction(Gin_A(6:15),Red_Basis,p3_scalars,msq(0),fk0,gammas,&
  l1,l2,v1,v2,RedCoeff)

  Gout_A(:) = RedCoeff(:,1) + Gin_A(1:5)
  Gout_A0   = RedCoeff(:,2)
  Gtmp_A1   = RedCoeff(:,3)
  Gtmp_A2   = RedCoeff(:,4)
  Gtmp_A3   = RedCoeff(:,5)

  if(present(Gout_R1)) Gout_R1 = - Gout_A0(1)

  if(perm(1)) then
    Gout_A1 = Gtmp_A1
    Gout_A2 = Gtmp_A2
    Gout_A3 = Gtmp_A3
  else if (perm(2)) then
    Gout_A1 = Gtmp_A1
    Gout_A2 = Gtmp_A3
    Gout_A3 = Gtmp_A2
  else if (perm(3)) then
    Gout_A1 = Gtmp_A3
    Gout_A2 = Gtmp_A1
    Gout_A3 = Gtmp_A2
  end if

end subroutine otf_4pt_rank2_red_last


! ******************************************************************************
subroutine otf_4pt_rank1_red_last(Gin_A,RedSet_4,msq_in,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3)
! ----------------------------------------------------------------------------------
! OpenLoops on-the-fly reduction step. Reduction of a closed rank-1 4-pt segment.
! No R1-rational terms contributing to a rank-1 box
! ----------------------------------------------------------------------------------
! Gin_A    = input open-loop
! RedSet_4 = input Reduction Set containig the basis and p3scalars
! msq_in   = array of the squared masses m0^2, m1^2, m2^2, m3^2
! Gout_A   = unpinched reduced open-loop
! Gout_Ai  = output reduced open-loops. Subtopology with Di propagator pinched
! **********************************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_data_types_/**/REALKIND, only: basis, redset4
  use ol_kinematics_/**/REALKIND, only: get_LC_mass2,cont_LC_cntrv
  implicit none
  complex(REALKIND), intent(in) :: Gin_A(5)
  type(redset4),     intent(in) :: RedSet_4
  complex(REALKIND), intent(in) :: msq_in(0:3)
  complex(REALKIND), dimension(1), intent(out) :: Gout_A, Gout_A0, Gout_A1, Gout_A2, Gout_A3
  complex(REALKIND) :: alpha0,alpha1,alpha2,alpha3,gamma,p3_scalars(0:4)
  complex(REALKIND) :: l1(4),l2(4),l3(4),l4(4),fk0(3),msq(0:3)
  complex(REALKIND) :: Gv1,Gv2,Gl3,Gl4,Gtmp_A1(1),Gtmp_A2(1),Gtmp_A3(1)
  type(basis) :: Red_Basis
  logical :: perm(3)

  Red_Basis = RedSet_4%redbasis

  perm(1) = (RedSet_4%perm(1)+RedSet_4%perm(2)) == 3 !! 1,2,3 permutation
  perm(2) = (RedSet_4%perm(1)+RedSet_4%perm(2)) == 4 !! 1,3,2 permutation
  perm(3) = (RedSet_4%perm(1)+RedSet_4%perm(2)) == 5 !! 2,3,1 permutation

  gamma = Red_Basis%gamma

  alpha0 = RedSet_4%p3scalars(0)
  alpha1 = RedSet_4%p3scalars(1)
  alpha2 = RedSet_4%p3scalars(2)
  alpha3 = RedSet_4%p3scalars(3)

  msq(0) = msq_in(0)
  if(perm(1)) then
    msq = msq_in
  else if (perm(2)) then
    msq(1) = msq_in(1)
    msq(2) = msq_in(3)
    msq(3) = msq_in(2)
  else if(perm(3)) then
    msq(1) = msq_in(2)
    msq(2) = msq_in(3)
    msq(3) = msq_in(1)
  end if

  !! f_{k0} = {m_k}^2 - {m_0}^2 - {p_k}^2
  fk0(1) = msq(1) - msq(0) - get_LC_mass2(Red_Basis%mom1)
  fk0(2) = msq(2) - msq(0) - get_LC_mass2(Red_Basis%mom2)
  fk0(3) = msq(3) - msq(0) - RedSet_4%p3scalars(4)

  call LC_Cov2Contr(Red_Basis%vect1,l1)
  call LC_Cov2Contr(Red_Basis%vect2,l2)
  call LC_Cov2Contr(Red_Basis%vect3,l3)
  call LC_Cov2Contr(Red_Basis%vect4,l4)

  Gv1 = 2*SUM(Gin_A(2:5)*l1)
  Gv2 = 2*SUM(Gin_A(2:5)*l2)
  Gl3 =   SUM(Gin_A(2:5)*l3)/2
  Gl4 =   SUM(Gin_A(2:5)*l4)/(2*alpha3)

  Gtmp_A3 = alpha0*(Gl3 + Gl4)
  Gtmp_A2 = (Gv1 - alpha1*(Gl3 + Gl4))/gamma
  Gtmp_A1 = (Gv2 - alpha2*(Gl3 + Gl4))/gamma
  Gout_A0 = - Gtmp_A1 - Gtmp_A2 - Gtmp_A3
  Gout_A  = Gtmp_A1*fk0(1) + Gtmp_A2*fk0(2) + Gtmp_A3*fk0(3) + Gin_A(1)

  if(perm(1)) then
    Gout_A1 = Gtmp_A1
    Gout_A2 = Gtmp_A2
    Gout_A3 = Gtmp_A3
  else if (perm(2)) then
    Gout_A1 = Gtmp_A1
    Gout_A2 = Gtmp_A3
    Gout_A3 = Gtmp_A2
  else if(perm(3)) then
    Gout_A1 = Gtmp_A3
    Gout_A2 = Gtmp_A1
    Gout_A3 = Gtmp_A2
  end if

end subroutine otf_4pt_rank1_red_last


! ******************************************************************************
subroutine otf_5pt_red(Gin_A,RedSet_5,msq_in,Gout_A,Gout_A0,Gout_A1,Gout_A2, &
                       Gout_A3,Gout_A4,Gout_R1)
! ------------------------------------------------------------------------------
! OpenLoops on-the-fly reduction step. Reduction of an open rank-2 5-pt segment
! ------------------------------------------------------------------------------
! Gin_A    = input open-loop
! RedSet_5 = input Reduction Set containig the basis and p3scalars
! msq_in   = array of the squared masses m0^2, m1^2, m2^2, m3^2, m4^2
! Gout_A   = unpinched reduced open-loop
! Gout_Ai  = output reduced open-loops. Subtopology with Di propagator pinched
! Gout_R1  = R1-rational contribution. Present for UV divergent integrals
! ******************************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_data_types_/**/REALKIND, only: basis, redset5
  use ol_kinematics_/**/REALKIND, only: get_LC_mass2
  implicit none
  complex(REALKIND), dimension(4,15,4), intent(in)  :: Gin_A
  type(redset5),                        intent(in)  :: RedSet_5
  complex(REALKIND),                    intent(in)  :: msq_in(0:4)
  complex(REALKIND), dimension(4,5,4),  intent(out) :: Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3,Gout_A4
  complex(REALKIND), optional,          intent(out) :: Gout_R1(4,1,4)

  complex(REALKIND), dimension(4,5,4) :: Gtmp_A1,Gtmp_A2,Gtmp_A3,Gtmp_A4
  complex(REALKIND) :: gammas(2),p3_scalars(0:4)
  type(basis) :: Red_Basis
  integer :: a,b,l,perm(4)
  logical :: zerocheck
  complex(REALKIND) :: fk0(3),msq(0:4),RedCoeff(4,5,5),l1(4),l2(4),v1(4),v2(4)

  Red_Basis = RedSet_5%redbasis
  p3_scalars = RedSet_5%p3scalars
  perm = RedSet_5%perm

  msq(0) = msq_in(0)
  if (perm(1) == 1) then
    if (perm(2) == 2) then
      if (perm(3) == 3) then
        ! 1,2,3,4 permutation
        msq = msq_in
      else
        ! 1,2,4,3 permutation
        msq(1) = msq_in(1)
        msq(2) = msq_in(2)
        msq(3) = msq_in(4)
        msq(4) = msq_in(3)
      end if
    else if (perm(2) == 3) then
      if (perm(3) == 2) then
        ! 1,3,2,4 permutation
        msq(1) = msq_in(1)
        msq(2) = msq_in(3)
        msq(3) = msq_in(2)
        msq(4) = msq_in(4)
      else
        ! 1,3,4,2 permutation
        msq(1) = msq_in(1)
        msq(2) = msq_in(3)
        msq(3) = msq_in(4)
        msq(4) = msq_in(2)
      end if
    else if (perm(2) == 4) then
      if (perm(3) == 2) then
        ! 1,4,2,3 permutation
        msq(1) = msq_in(1)
        msq(2) = msq_in(4)
        msq(3) = msq_in(2)
        msq(4) = msq_in(3)
      else
        ! 1,4,3,2 permutation
        msq(1) = msq_in(1)
        msq(2) = msq_in(4)
        msq(3) = msq_in(3)
        msq(4) = msq_in(2)
      end if
    end if

  else if (perm(1) == 2) then
    if (perm(2) == 3) then
      if (perm(3) == 1) then
        ! 2,3,1,4 permutation
        msq(1) = msq_in(2)
        msq(2) = msq_in(3)
        msq(3) = msq_in(1)
        msq(4) = msq_in(4)
      else
        ! 2,3,4,1 permutation
        msq(1) = msq_in(2)
        msq(2) = msq_in(3)
        msq(3) = msq_in(4)
        msq(4) = msq_in(1)
      end if
    else if(perm(2) == 4) then
      if (perm(3) == 1) then
        ! 2,4,1,3 permutation
        msq(1) = msq_in(2)
        msq(2) = msq_in(4)
        msq(3) = msq_in(1)
        msq(4) = msq_in(3)
      else
        ! 2,4,3,1 permutation
        msq(1) = msq_in(2)
        msq(2) = msq_in(4)
        msq(3) = msq_in(3)
        msq(4) = msq_in(1)
      end if
    end if

  else if (perm(1) == 3) then
    if (perm(2) == 4) then
      if (perm(3) == 1) then
        ! 3,4,1,2 permutation
        msq(1) = msq_in(3)
        msq(2) = msq_in(4)
        msq(3) = msq_in(1)
        msq(4) = msq_in(2)
      else
        !3,4,2,1 permutation
        msq(1) = msq_in(3)
        msq(2) = msq_in(4)
        msq(3) = msq_in(2)
        msq(4) = msq_in(1)
      end if
    end if

  end if

  !! f_{k0} = {m_k}^2 - {m_0}^2 - {p_k}^2
  fk0(1) = msq(1) - msq(0) - get_LC_mass2(Red_Basis%mom1)
  fk0(2) = msq(2) - msq(0) - get_LC_mass2(Red_Basis%mom2)
  fk0(3) = msq(3) - msq(0) - p3_scalars(4)

  !! gamma and gamma^2
  gammas(1) = Red_Basis%gamma
  gammas(2) = gammas(1)**2

  !! l1_{\mu}, l2_{\mu}
  l1 = Red_Basis%vect1
  l2 = Red_Basis%vect2

  !! l1^{\mu}, l2^{\mu}
  call LC_Cov2Contr(l1,v1)
  call LC_Cov2Contr(l2,v2)

  do a = 1, 4

    call fourpoint_reduction_OL(Gin_A(:,:,a),Red_Basis,p3_scalars,msq(0),fk0,gammas,&
    l1,l2,v1,v2,RedCoeff)

    Gout_A(:,1:5,a) = RedCoeff(:,1:5,1) + Gin_A(:,1:5,a)

    Gout_A0(:,:,a) = RedCoeff(:,:,2)
    Gtmp_A1(:,:,a) = RedCoeff(:,:,3)
    Gtmp_A2(:,:,a) = RedCoeff(:,:,4)
    Gtmp_A3(:,:,a) = RedCoeff(:,:,5)
    Gtmp_A4(:,:,a) = 0._/**/REALKIND

  end do

  if (perm(1) == 1) then
    if (perm(2) == 2) then
      if (perm(3) == 3) then
        ! 1,2,3,4 permutation
        Gout_A1 = Gtmp_A1
        Gout_A2 = Gtmp_A2
        Gout_A3 = Gtmp_A3
        Gout_A4 = Gtmp_A4
      else
        ! 1,2,4,3 permutation
        Gout_A1 = Gtmp_A1
        Gout_A2 = Gtmp_A2
        Gout_A3 = Gtmp_A4
        Gout_A4 = Gtmp_A3
      end if
    else if (perm(2) == 3) then
      if (perm(3) == 2) then
        ! 1,3,2,4 permutation
        Gout_A1 = Gtmp_A1
        Gout_A2 = Gtmp_A3
        Gout_A3 = Gtmp_A2
        Gout_A4 = Gtmp_A4
      else
        ! 1,3,4,2 permutation
        Gout_A1 = Gtmp_A1
        Gout_A2 = Gtmp_A4
        Gout_A3 = Gtmp_A2
        Gout_A4 = Gtmp_A3
      end if
    else if (perm(2) == 4) then
      if (perm(3) == 2) then
        ! 1,4,2,3 permutation
        Gout_A1 = Gtmp_A1
        Gout_A2 = Gtmp_A3
        Gout_A3 = Gtmp_A4
        Gout_A4 = Gtmp_A2
      else
        ! 1,4,3,2 permutation
        Gout_A1 = Gtmp_A1
        Gout_A2 = Gtmp_A4
        Gout_A3 = Gtmp_A3
        Gout_A4 = Gtmp_A2
      end if
    end if

  else if (perm(1) == 2) then
    if (perm(2) == 3) then
      if (perm(3) == 1) then
        ! 2,3,1,4 permutation
        Gout_A1 = Gtmp_A3
        Gout_A2 = Gtmp_A1
        Gout_A3 = Gtmp_A2
        Gout_A4 = Gtmp_A4
      else
        ! 2,3,4,1 permutation
        Gout_A1 = Gtmp_A4
        Gout_A2 = Gtmp_A1
        Gout_A3 = Gtmp_A2
        Gout_A4 = Gtmp_A3
      end if
    else if(perm(2) == 4) then
      if (perm(3) == 1) then
        ! 2,4,1,3 permutation
        Gout_A1 = Gtmp_A3
        Gout_A2 = Gtmp_A1
        Gout_A3 = Gtmp_A4
        Gout_A4 = Gtmp_A2
      else
        ! 2,4,3,1 permutation
        Gout_A1 = Gtmp_A4
        Gout_A2 = Gtmp_A1
        Gout_A3 = Gtmp_A3
        Gout_A4 = Gtmp_A2
      end if
    end if

  else if (perm(1) == 3) then
    if (perm(2) == 4) then
      if (perm(3) == 1) then
        ! 3,4,1,2 permutation
        Gout_A1 = Gtmp_A3
        Gout_A2 = Gtmp_A4
        Gout_A3 = Gtmp_A1
        Gout_A4 = Gtmp_A2
      else
        !3,4,2,1 permutation
        Gout_A1 = Gtmp_A4
        Gout_A2 = Gtmp_A3
        Gout_A3 = Gtmp_A1
        Gout_A4 = Gtmp_A2
      end if
    end if

  end if

  !! Assignment of the rational R1 contribution
  if(present(Gout_R1)) Gout_R1(:,1,:) = - Gout_A0(:,1,:)

end subroutine otf_5pt_red

! ******************************************************************************
subroutine otf_5pt_reduction_last(Gin_A,RedSet_5,msq_in,Gout_A,Gout_A0, &
                                  Gout_A1,Gout_A2,Gout_A3,Gout_A4,Gout_R1)
! ******************************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_data_types_/**/REALKIND, only: redset5
  implicit none
  complex(REALKIND), intent(in)  :: Gin_A(:)
  type(redset5), intent(in) :: RedSet_5
  complex(REALKIND), intent(in) :: msq_in(0:4)
  complex(REALKIND), intent(out) :: Gout_A(:), Gout_A0(:), Gout_A1(:), Gout_A2(:), Gout_A3(:), Gout_A4(:)
  complex(REALKIND), optional, intent(out) :: Gout_R1

  !! Rank-2 reduction
  if(size(Gin_A)==15) then
    if(present(Gout_R1)) then
      call otf_5pt_rank2_red_last(Gin_A,RedSet_5,msq_in,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3,Gout_A4,Gout_R1)
    else
      call otf_5pt_rank2_red_last(Gin_A,RedSet_5,msq_in,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3,Gout_A4)
    end if

  else
    call ol_error("Reduction of a rank 1 higher point (N>=5) function should in loopreduction module")
  end if

end subroutine otf_5pt_reduction_last

! ******************************************************************************
subroutine otf_5pt_rank2_red_last(Gin_A,RedSet_5,msq_in,Gout_A,Gout_A0, &
                                  Gout_A1,Gout_A2,Gout_A3,Gout_A4,Gout_R1)
! ----------------------------------------------------------------------------------
! OpenLoops on-the-fly reduction step. Reduction of a closed rank-2 5-pt segment.
! No R1-rational terms contributing to a rank-2 box
! ----------------------------------------------------------------------------------
! Gin_A    = input open-loop
! RedSet_5 = input Reduction Set containig the basis and p3scalars
! msq_in   = array of the squared masses m0^2, m1^2, m2^2, m3^2
! Gout_A   = unpinched reduced open-loop
! Gout_Ai  = output reduced open-loops. Subtopology with Di propagator pinched
! **********************************************************************************
  use KIND_TYPES, only: REALKIND
  use ol_data_types_/**/REALKIND, only: basis, redset5
  use ol_kinematics_/**/REALKIND, only: get_LC_mass2
  implicit none
  complex(REALKIND), intent(in)  :: Gin_A(15)
  type(redset5), intent(in) :: RedSet_5
  complex(REALKIND), intent(in) :: msq_in(0:4)
  complex(REALKIND), intent(out), dimension(5) :: Gout_A, Gout_A0, Gout_A1, Gout_A2, Gout_A3, Gout_A4
  complex(REALKIND), optional, intent(out) :: Gout_R1

  complex(REALKIND) :: RedCoeff(5,5), fk0(3), p3_scalars(0:4), msq(0:4), gammas(2)
  complex(REALKIND) :: l1(4), l2(4), v1(4), v2(4)
  complex(REALKIND), dimension(5) :: Gtmp_A1, Gtmp_A2, Gtmp_A3, Gtmp_A4
  type(basis) :: Red_Basis
  integer :: perm(4)

  Red_Basis = RedSet_5%redbasis
  p3_scalars = RedSet_5%p3scalars
  perm = RedSet_5%perm

  msq(0) = msq_in(0)
  if (perm(1) == 1) then
    if (perm(2) == 2) then
      if (perm(3) == 3) then
        ! 1,2,3,4 permutation
        msq = msq_in
      else
        ! 1,2,4,3 permutation
        msq(1) = msq_in(1)
        msq(2) = msq_in(2)
        msq(3) = msq_in(4)
        msq(4) = msq_in(3)
      end if
    else if (perm(2) == 3) then
      if (perm(3) == 2) then
        ! 1,3,2,4 permutation
        msq(1) = msq_in(1)
        msq(2) = msq_in(3)
        msq(3) = msq_in(2)
        msq(4) = msq_in(4)
      else
        ! 1,3,4,2 permutation
        msq(1) = msq_in(1)
        msq(2) = msq_in(3)
        msq(3) = msq_in(4)
        msq(4) = msq_in(2)
      end if
    else if (perm(2) == 4) then
      if (perm(3) == 2) then
        ! 1,4,2,3 permutation
        msq(1) = msq_in(1)
        msq(2) = msq_in(4)
        msq(3) = msq_in(2)
        msq(4) = msq_in(3)
      else
        ! 1,4,3,2 permutation
        msq(1) = msq_in(1)
        msq(2) = msq_in(4)
        msq(3) = msq_in(3)
        msq(4) = msq_in(2)
      end if
    end if

  else if (perm(1) == 2) then
    if (perm(2) == 3) then
      if (perm(3) == 1) then
        ! 2,3,1,4 permutation
        msq(1) = msq_in(2)
        msq(2) = msq_in(3)
        msq(3) = msq_in(1)
        msq(4) = msq_in(4)
      else
        ! 2,3,4,1 permutation
        msq(1) = msq_in(2)
        msq(2) = msq_in(3)
        msq(3) = msq_in(4)
        msq(4) = msq_in(1)
      end if
    else if(perm(2) == 4) then
      if (perm(3) == 1) then
        ! 2,4,1,3 permutation
        msq(1) = msq_in(2)
        msq(2) = msq_in(4)
        msq(3) = msq_in(1)
        msq(4) = msq_in(3)
      else
        ! 2,4,3,1 permutation
        msq(1) = msq_in(2)
        msq(2) = msq_in(4)
        msq(3) = msq_in(3)
        msq(4) = msq_in(1)
      end if
    end if

  else if (perm(1) == 3) then
    if (perm(2) == 4) then
      if (perm(3) == 1) then
        ! 3,4,1,2 permutation
        msq(1) = msq_in(3)
        msq(2) = msq_in(4)
        msq(3) = msq_in(1)
        msq(4) = msq_in(2)
      else
        !3,4,2,1 permutation
        msq(1) = msq_in(3)
        msq(2) = msq_in(4)
        msq(3) = msq_in(2)
        msq(4) = msq_in(1)
      end if
    end if

  end if

  !! f_{k0} = {m_k}^2 - {m_0}^2 - {p_k}^2
  fk0(1) = msq(1) - msq(0) - get_LC_mass2(Red_Basis%mom1)
  fk0(2) = msq(2) - msq(0) - get_LC_mass2(Red_Basis%mom2)
  fk0(3) = msq(3) - msq(0) - p3_scalars(4)

  !! gamma and gamma^2
  gammas(1) = Red_Basis%gamma
  gammas(2) = gammas(1)**2

  !! l1_{\mu}, l2_{\mu}
  l1 = Red_Basis%vect1
  l2 = Red_Basis%vect2

  !! l1^{\mu}, l2^{\mu}
  call LC_Cov2Contr(l1,v1)
  call LC_Cov2Contr(l2,v2)

  call fourpoint_reduction(Gin_A(6:15),Red_Basis,p3_scalars,msq(0),fk0,gammas,&
        l1,l2,v1,v2,RedCoeff)

  Gout_A(:) = RedCoeff(:,1) + Gin_A(1:5)
  Gout_A0   = RedCoeff(:,2)
  Gtmp_A1   = RedCoeff(:,3)
  Gtmp_A2   = RedCoeff(:,4)
  Gtmp_A3   = RedCoeff(:,5)
  Gtmp_A4   = 0._/**/REALKIND

  if(present(Gout_R1)) Gout_R1 = - Gout_A0(1)

  if (perm(1) == 1) then
    if (perm(2) == 2) then
      if (perm(3) == 3) then
        ! 1,2,3,4 permutation
        Gout_A1 = Gtmp_A1
        Gout_A2 = Gtmp_A2
        Gout_A3 = Gtmp_A3
        Gout_A4 = Gtmp_A4
      else
        ! 1,2,4,3 permutation
        Gout_A1 = Gtmp_A1
        Gout_A2 = Gtmp_A2
        Gout_A3 = Gtmp_A4
        Gout_A4 = Gtmp_A3
      end if
    else if (perm(2) == 3) then
      if (perm(3) == 2) then
        ! 1,3,2,4 permutation
        Gout_A1 = Gtmp_A1
        Gout_A2 = Gtmp_A3
        Gout_A3 = Gtmp_A2
        Gout_A4 = Gtmp_A4
      else
        ! 1,3,4,2 permutation
        Gout_A1 = Gtmp_A1
        Gout_A2 = Gtmp_A4
        Gout_A3 = Gtmp_A2
        Gout_A4 = Gtmp_A3
      end if
    else if (perm(2) == 4) then
      if (perm(3) == 2) then
        ! 1,4,2,3 permutation
        Gout_A1 = Gtmp_A1
        Gout_A2 = Gtmp_A3
        Gout_A3 = Gtmp_A4
        Gout_A4 = Gtmp_A2
      else
        ! 1,4,3,2 permutation
        Gout_A1 = Gtmp_A1
        Gout_A2 = Gtmp_A4
        Gout_A3 = Gtmp_A3
        Gout_A4 = Gtmp_A2
      end if
    end if

  else if (perm(1) == 2) then
    if (perm(2) == 3) then
      if (perm(3) == 1) then
        ! 2,3,1,4 permutation
        Gout_A1 = Gtmp_A3
        Gout_A2 = Gtmp_A1
        Gout_A3 = Gtmp_A2
        Gout_A4 = Gtmp_A4
      else
        ! 2,3,4,1 permutation
        Gout_A1 = Gtmp_A4
        Gout_A2 = Gtmp_A1
        Gout_A3 = Gtmp_A2
        Gout_A4 = Gtmp_A3
      end if
    else if(perm(2) == 4) then
      if (perm(3) == 1) then
        ! 2,4,1,3 permutation
        Gout_A1 = Gtmp_A3
        Gout_A2 = Gtmp_A1
        Gout_A3 = Gtmp_A4
        Gout_A4 = Gtmp_A2
      else
        ! 2,4,3,1 permutation
        Gout_A1 = Gtmp_A4
        Gout_A2 = Gtmp_A1
        Gout_A3 = Gtmp_A3
        Gout_A4 = Gtmp_A2
      end if
    end if

  else if (perm(1) == 3) then
    if (perm(2) == 4) then
      if (perm(3) == 1) then
        ! 3,4,1,2 permutation
        Gout_A1 = Gtmp_A3
        Gout_A2 = Gtmp_A4
        Gout_A3 = Gtmp_A1
        Gout_A4 = Gtmp_A2
      else
        !3,4,2,1 permutation
        Gout_A1 = Gtmp_A4
        Gout_A2 = Gtmp_A3
        Gout_A3 = Gtmp_A1
        Gout_A4 = Gtmp_A2
      end if
    end if

  end if

end subroutine otf_5pt_rank2_red_last

! ******************************************************************************
function valid_4pt_hol(Gin_A, Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3,Gout_R1)
! ------------------------------------------------------------------------------
! Intitialises OL/CL and returns whether further processing is required
! ******************************************************************************
  use ol_data_types_/**/REALKIND, only: hol
  use ol_loop_handling_/**/REALKIND, only: hybrid_zero_mode
  type(hol), intent(in)    :: Gin_A
  type(hol), intent(inout) :: Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3
  type(hol), optional, intent(inout) :: Gout_R1
  logical :: valid_4pt_hol

  Gout_A%hf(:) = Gin_A%hf(:)
  Gout_A0%hf(:) = Gin_A%hf(:)
  Gout_A1%hf(:) = Gin_A%hf(:)
  Gout_A2%hf(:) = Gin_A%hf(:)
  Gout_A3%hf(:) = Gin_A%hf(:)
  if(present(Gout_R1)) then
      Gout_R1%hf(:) = Gin_A%hf(:)
  end if

  Gout_A%mode = Gin_A%mode
  Gout_A0%mode = Gin_A%mode
  Gout_A1%mode = Gin_A%mode
  Gout_A2%mode = Gin_A%mode
  Gout_A3%mode = Gin_A%mode
  Gout_A%error = 0
  Gout_A0%error = 0
  Gout_A1%error = 0
  Gout_A2%error = 0
  Gout_A3%error = 0

  Gout_A%ndrs = Gin_A%ndrs
  Gout_A0%ndrs = 0
  Gout_A1%ndrs = 0
  Gout_A2%ndrs = 0
  Gout_A3%ndrs = 0
  Gout_A%nred = Gin_A%nred
  Gout_A0%nred = 0
  Gout_A1%nred = 0
  Gout_A2%nred = 0
  Gout_A3%nred = 0
#ifdef PRECISION_dp
  Gout_A%ndrs_qp = Gin_A%ndrs_qp
  Gout_A0%ndrs_qp = 0
  Gout_A1%ndrs_qp = 0
  Gout_A2%ndrs_qp = 0
  Gout_A3%ndrs_qp = 0
  Gout_A%nred_qp = Gin_A%nred_qp
  Gout_A0%nred_qp = 0
  Gout_A1%nred_qp = 0
  Gout_A2%nred_qp = 0
  Gout_A3%nred_qp = 0
#endif

  if(present(Gout_R1)) then
    Gout_R1%mode = Gin_A%mode
    Gout_R1%error = 0
    Gout_R1%ndrs = 0
    Gout_R1%nred = 0
#ifdef PRECISION_dp
    Gout_R1%ndrs_qp = 0
    Gout_R1%nred_qp = 0
#endif
  end if

  if (Gin_A%mode .eq. hybrid_zero_mode) then
    valid_4pt_hol = .false.
    Gout_A%j = 0
    Gout_A0%j = 0
    Gout_A1%j = 0
    Gout_A2%j = 0
    Gout_A3%j = 0

    if(present(Gout_R1)) then
      Gout_R1%j = 0
    end if
#ifdef PRECISION_dp
    if (hp_switch .eq. 1) then
      if (hp_alloc_mode .eq. 0) then
        Gout_A%j_qp = 0
        Gout_A0%j_qp = 0
        Gout_A1%j_qp = 0
        Gout_A2%j_qp = 0
        Gout_A3%j_qp = 0
        if(present(Gout_R1)) then
          Gout_R1%j_qp = 0
        end if
      end if
    end if
#endif
  else
    valid_4pt_hol = .true.
  end if

end function valid_4pt_hol

function valid_4pt_hcl(Gin_A, Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3,Gout_R1)
  use ol_data_types_/**/REALKIND, only: hcl
  use ol_loop_handling_/**/REALKIND, only: hybrid_zero_mode
  type(hcl),           intent(in)    :: Gin_A
  type(hcl),           intent(inout) :: Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3
  type(hcl), optional, intent(inout) :: Gout_R1
  logical :: valid_4pt_hcl

  Gout_A%mode = Gin_A%mode
  Gout_A0%mode = Gin_A%mode
  Gout_A1%mode = Gin_A%mode
  Gout_A2%mode = Gin_A%mode
  Gout_A3%mode = Gin_A%mode

  Gout_A%error = 0
  Gout_A0%error = 0
  Gout_A1%error = 0
  Gout_A2%error = 0
  Gout_A3%error = 0

  Gout_A%ndrs = Gin_A%ndrs
  Gout_A0%ndrs = 0
  Gout_A1%ndrs = 0
  Gout_A2%ndrs = 0
  Gout_A3%ndrs = 0
  Gout_A%nred = Gin_A%nred
  Gout_A0%nred = 0
  Gout_A1%nred = 0
  Gout_A2%nred = 0
  Gout_A3%nred = 0
#ifdef PRECISION_dp
  Gout_A%ndrs_qp = Gin_A%ndrs_qp
  Gout_A0%ndrs_qp = 0
  Gout_A1%ndrs_qp = 0
  Gout_A2%ndrs_qp = 0
  Gout_A3%ndrs_qp = 0
  Gout_A%nred_qp = Gin_A%nred_qp
  Gout_A0%nred_qp = 0
  Gout_A1%nred_qp = 0
  Gout_A2%nred_qp = 0
  Gout_A3%nred_qp = 0
#endif

  if(present(Gout_R1)) then
    Gout_R1%mode = Gin_A%mode
    Gout_R1%error = 0
    Gout_R1%ndrs = 0
    Gout_R1%nred = 0
#ifdef PRECISION_dp
    Gout_R1%ndrs_qp = 0
    Gout_R1%nred_qp = 0
#endif
  end if

  if (Gin_A%mode .eq. hybrid_zero_mode) then
    valid_4pt_hcl = .false.
    Gout_A%cmp = 0
    Gout_A0%cmp = 0
    Gout_A1%cmp = 0
    Gout_A2%cmp = 0
    Gout_A3%cmp = 0

    if(present(Gout_R1)) then
      Gout_R1%cmp = 0
    end if

#ifdef PRECISION_dp
    if (hp_switch .eq. 1) then
      if (hp_alloc_mode .eq. 0) then
        Gout_A%cmp_qp = 0
        Gout_A0%cmp_qp = 0
        Gout_A1%cmp_qp = 0
        Gout_A2%cmp_qp = 0
        Gout_A3%cmp_qp = 0
        if(present(Gout_R1)) then
          Gout_R1%cmp_qp = 0
        end if
      end if
    end if
#endif
  else
    valid_4pt_hcl = .true.
  end if

end function valid_4pt_hcl

function valid_5pt_hol(Gin_A, Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3,Gout_A4,Gout_R1) result(valid_hol)
  use ol_data_types_/**/REALKIND, only: hol
  use ol_loop_handling_/**/REALKIND, only: hybrid_zero_mode
  type(hol), intent(in)    :: Gin_A
  type(hol), intent(inout) :: Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3,Gout_A4
  type(hol), optional, intent(inout) :: Gout_R1
  logical :: valid_hol

  Gout_A%hf(:) = Gin_A%hf(:)
  Gout_A0%hf(:) = Gin_A%hf(:)
  Gout_A1%hf(:) = Gin_A%hf(:)
  Gout_A2%hf(:) = Gin_A%hf(:)
  Gout_A3%hf(:) = Gin_A%hf(:)
  Gout_A4%hf(:) = Gin_A%hf(:)
  if(present(Gout_R1)) then
      Gout_R1%hf(:) = Gin_A%hf(:)
  end if

  Gout_A%mode = Gin_A%mode
  Gout_A0%mode = Gin_A%mode
  Gout_A1%mode = Gin_A%mode
  Gout_A2%mode = Gin_A%mode
  Gout_A3%mode = Gin_A%mode
  Gout_A4%mode = Gin_A%mode
  Gout_A%error = 0
  Gout_A0%error = 0
  Gout_A1%error = 0
  Gout_A2%error = 0
  Gout_A3%error = 0
  Gout_A4%error = 0
  Gout_A%ndrs = Gin_A%ndrs
  Gout_A0%ndrs = 0
  Gout_A1%ndrs = 0
  Gout_A2%ndrs = 0
  Gout_A3%ndrs = 0
  Gout_A4%ndrs = 0
  Gout_A%nred = Gin_A%nred
  Gout_A0%nred = 0
  Gout_A1%nred = 0
  Gout_A2%nred = 0
  Gout_A3%nred = 0
  Gout_A4%nred = 0
#ifdef PRECISION_dp
  Gout_A%ndrs_qp = Gin_A%ndrs_qp
  Gout_A0%ndrs_qp = 0
  Gout_A1%ndrs_qp = 0
  Gout_A2%ndrs_qp = 0
  Gout_A3%ndrs_qp = 0
  Gout_A4%ndrs_qp = 0
  Gout_A%nred_qp = Gin_A%nred_qp
  Gout_A0%nred_qp = 0
  Gout_A1%nred_qp = 0
  Gout_A2%nred_qp = 0
  Gout_A3%nred_qp = 0
  Gout_A4%nred_qp = 0
#endif

  if(present(Gout_R1)) then
    Gout_R1%mode = Gin_A%mode
    Gout_R1%error = 0
    Gout_R1%ndrs = 0
    Gout_R1%nred = 0
#ifdef PRECISION_dp
    Gout_R1%ndrs_qp = 0
    Gout_R1%nred_qp = 0
#endif
  end if

  if (Gin_A%mode .eq. hybrid_zero_mode) then
    valid_hol = .false.
    Gout_A%j = 0
    Gout_A0%j = 0
    Gout_A1%j = 0
    Gout_A2%j = 0
    Gout_A3%j = 0
    Gout_A4%j = 0

    if(present(Gout_R1)) then
      Gout_R1%j = 0
    end if

#ifdef PRECISION_dp
    if (hp_switch .eq. 1) then
      if (hp_alloc_mode .eq. 0) then
        Gout_A%j_qp = 0
        Gout_A0%j_qp = 0
        Gout_A1%j_qp = 0
        Gout_A2%j_qp = 0
        Gout_A3%j_qp = 0
        Gout_A4%j_qp = 0
        if(present(Gout_R1)) then
          Gout_R1%j_qp = 0
        end if
      end if
    end if
#endif
  else
    valid_hol = .true.
  end if

end function valid_5pt_hol

function valid_5pt_hcl(Gin_A, Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3,Gout_A4,Gout_R1) result(valid_hol)
  use ol_data_types_/**/REALKIND, only: hcl
  use ol_loop_handling_/**/REALKIND, only: hybrid_zero_mode
  type(hcl), intent(in)    :: Gin_A
  type(hcl), intent(inout) :: Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3,Gout_A4
  type(hcl), optional, intent(inout) :: Gout_R1
  logical :: valid_hol

  Gout_A%mode = Gin_A%mode
  Gout_A0%mode = Gin_A%mode
  Gout_A1%mode = Gin_A%mode
  Gout_A2%mode = Gin_A%mode
  Gout_A3%mode = Gin_A%mode
  Gout_A4%mode = Gin_A%mode
  Gout_A%error = 0
  Gout_A0%error = 0
  Gout_A1%error = 0
  Gout_A2%error = 0
  Gout_A3%error = 0
  Gout_A4%error = 0
  Gout_A%ndrs = Gin_A%ndrs
  Gout_A0%ndrs = 0
  Gout_A1%ndrs = 0
  Gout_A2%ndrs = 0
  Gout_A3%ndrs = 0
  Gout_A4%ndrs = 0
  Gout_A%nred = Gin_A%nred
  Gout_A0%nred = 0
  Gout_A1%nred = 0
  Gout_A2%nred = 0
  Gout_A3%nred = 0
  Gout_A4%nred = 0
#ifdef PRECISION_dp
  Gout_A%ndrs_qp = Gin_A%ndrs_qp
  Gout_A0%ndrs_qp = 0
  Gout_A1%ndrs_qp = 0
  Gout_A2%ndrs_qp = 0
  Gout_A3%ndrs_qp = 0
  Gout_A4%ndrs_qp = 0
  Gout_A%nred_qp = Gin_A%nred_qp
  Gout_A0%nred_qp = 0
  Gout_A1%nred_qp = 0
  Gout_A2%nred_qp = 0
  Gout_A3%nred_qp = 0
  Gout_A4%nred_qp = 0
#endif

  if (present(Gout_R1)) then
    Gout_R1%mode = Gin_A%mode
    Gout_R1%error = 0
    Gout_R1%ndrs = 0
    Gout_R1%nred = 0
#ifdef PRECISION_dp
    Gout_R1%ndrs_qp = 0
    Gout_R1%nred_qp = 0
#endif
  end if

  if (Gin_A%mode .eq. hybrid_zero_mode) then
    valid_hol = .false.
    Gout_A%cmp = 0
    Gout_A0%cmp = 0
    Gout_A1%cmp = 0
    Gout_A2%cmp = 0
    Gout_A3%cmp = 0
    Gout_A4%cmp = 0

    if(present(Gout_R1)) then
      Gout_R1%cmp = 0
    end if

#ifdef PRECISION_dp
    if (hp_switch .eq. 1) then
      if (hp_alloc_mode .eq. 0) then
        Gout_A%cmp_qp = 0
        Gout_A0%cmp_qp = 0
        Gout_A1%cmp_qp = 0
        Gout_A2%cmp_qp = 0
        Gout_A3%cmp_qp = 0
        Gout_A4%cmp_qp = 0
        if(present(Gout_R1)) then
          Gout_R1%cmp_qp = 0
        end if
      end if
    end if
#endif
  else
    valid_hol = .true.
  end if

end function valid_5pt_hcl

subroutine trigger_upgrade_hol(alfa_max,gamman,Gin)
  use ol_parameters_decl_/**/REALKIND, only: hp_step_thres,hp_err_thres
  use ol_data_types_/**/REALKIND, only: hol
#ifdef PRECISION_dp
  use ol_loop_handling_/**/REALKIND, only: upgrade_qp
#endif
  real(REALKIND), intent(in)    :: alfa_max,gamman
  type(hol),      intent(inout) :: Gin
  integer :: i

  if (alfa_max > hp_step_thres .and. Gin%error + alfa_max > hp_err_thres) then
#ifdef PRECISION_dp
    if (hp_switch .eq. 1) then
      call upgrade_qp(Gin)
    end if
#endif
  else if (-log10(gamman) > hp_step_thres .and. Gin%error - log10(gamman) > hp_err_thres) then
#ifdef PRECISION_dp
    if (hp_switch .eq. 1) then
      call upgrade_qp(Gin)
    end if
#endif
  end if

end subroutine trigger_upgrade_hol

subroutine trigger_upgrade_hcl(alfa_max,gamman,Gin)
  use ol_parameters_decl_/**/REALKIND, only: hp_step_thres,hp_err_thres
  use ol_data_types_/**/REALKIND, only: hcl
#ifdef PRECISION_dp
  use ol_loop_handling_/**/REALKIND, only: upgrade_qp
#endif
  real(REALKIND), intent(in)    :: alfa_max,gamman
  type(hcl),      intent(inout) :: Gin

  if (alfa_max > hp_step_thres .and. Gin%error + alfa_max > hp_err_thres) then
#ifdef PRECISION_dp
    if (hp_switch .eq. 1) then
      call upgrade_qp(Gin)
    end if
#endif
  else if (-log10(gamman) > hp_step_thres .and. Gin%error -log10(gamman) > hp_err_thres) then
#ifdef PRECISION_dp
    if (hp_switch .eq. 1) then
      call upgrade_qp(Gin)
    end if
#endif
  end if

end subroutine trigger_upgrade_hcl

! -----------------------------------------------------------------------------
! Check error and qp_trigger settings
!  -----------------------------------------------------------------------------

subroutine err_estim_4pt_hol(RedSet,Gin_A,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3,Gout_R1)
  use ol_data_types_/**/REALKIND, only: hol,REALKIND,QREALKIND,redset4
  use ol_parameters_decl_/**/DREALKIND, only: hp_max_err
  use ol_parameters_decl_/**/DREALKIND, only: hp_gamma_trig, &
                                              hp_alloc_mode, &
                                              hybrid_dp_mode
#ifdef PRECISION_dp
  use ol_loop_handling_/**/REALKIND, only: hol_alloc_hybrid
#endif
  implicit none
  type(redset4),       intent(in)    :: RedSet
  type(hol),           intent(inout) :: Gin_A
  type(hol),           intent(inout) :: Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3
  type(hol), optional, intent(inout) :: Gout_R1
  real(REALKIND)    :: alfa_max,alfa(3), gamma_inv
  complex(REALKIND) :: B3coeff
  integer           :: mode,perm(3)

  perm =RedSet%perm
  B3coeff = (RedSet%redbasis%gamma)*(RedSet%p3scalars(0))*2._/**/REALKIND
  alfa = [abs(RedSet%p3scalars(1)), abs(RedSet%p3scalars(2)), abs(B3coeff)]
  alfa = log10(alfa)

  if (hp_gamma_trig) then
    gamma_inv=log10( 1._/**/REALKIND / RedSet%gd2**2)
  else
    gamma_inv=0
  end if

  if (gdm2_err_estim_OL .eqv. .true.) then
    alfa_max = max(alfa(1),alfa(2))
  else
    alfa_max = max(alfa(1),alfa(2),alfa(3))
  end if

  Gout_A%error = Gin_A%error + max(alfa(1),alfa(2),alfa(3),gamma_inv)
  Gout_A0%error = Gin_A%error + max(alfa(1),alfa(2),alfa(3),gamma_inv)
  if (perm(1)+perm(2) == 3) then
    ! perm = 1,2,3
    Gout_A1%error = Gin_A%error + max(alfa(1),gamma_inv)
    Gout_A2%error = Gin_A%error + max(alfa(2),gamma_inv)
    Gout_A3%error = Gin_A%error + max(alfa(3),gamma_inv/2)
  else if (perm(1)+perm(2) == 4) then
    ! perm = 1,3,2
    Gout_A1%error = Gin_A%error + max(alfa(1),gamma_inv)
    Gout_A3%error = Gin_A%error + max(alfa(2),gamma_inv)
    Gout_A2%error = Gin_A%error + max(alfa(3),gamma_inv/2)
  else if (perm(1)+perm(2) == 5) then
    ! perm = 2,3,1
    Gout_A2%error = Gin_A%error + max(alfa(1),gamma_inv)
    Gout_A3%error = Gin_A%error + max(alfa(2),gamma_inv)
    Gout_A1%error = Gin_A%error + max(alfa(3),gamma_inv/2)
  end if

  call trigger_upgrade(alfa_max,RedSet%gd2,Gin_A)

  mode = Gin_A%mode
  Gout_A%mode = mode
  Gout_A0%mode = mode
  Gout_A1%mode = mode
  Gout_A2%mode = mode
  Gout_A3%mode = mode

  if (iand(mode, hybrid_dp_mode) .ne. 0) then
    Gout_A%nred = Gout_A%nred + 1
    Gout_A0%nred = 1
    Gout_A1%nred = 1
    Gout_A2%nred = 1
    Gout_A3%nred = 1
  end if

  if(present(Gout_R1)) then
    Gout_R1%error = Gout_A%error
    Gout_R1%mode = mode
  end if

#ifdef PRECISION_dp
  if (mode .gt. hybrid_dp_mode) then
    Gout_A%nred_qp = Gout_A%nred_qp + 1
    Gout_A0%nred_qp = 1
    Gout_A1%nred_qp = 1
    Gout_A2%nred_qp = 1
    Gout_A3%nred_qp = 1
  end if
  if (hp_alloc_mode .gt. 1 .and. mode .gt. hybrid_dp_mode) then
    call hol_alloc_hybrid(Gout_A)
    call hol_alloc_hybrid(Gout_A0)
    call hol_alloc_hybrid(Gout_A1)
    call hol_alloc_hybrid(Gout_A2)
    call hol_alloc_hybrid(Gout_A3)
    if(present(Gout_R1)) call hol_alloc_hybrid(Gout_R1)
  end if
#endif

  if (Gout_A%error > hp_max_err) hp_max_err = Gout_A%error

end subroutine err_estim_4pt_hol

subroutine err_estim_4pt_hcl(RedSet,Gin_A,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3,Gout_R1)
  use ol_data_types_/**/REALKIND, only: hcl,REALKIND,QREALKIND,redset4
  use ol_parameters_decl_/**/REALKIND, only: hp_max_err
  use ol_parameters_decl_/**/DREALKIND, only: hp_gamma_trig, &
                                              hp_alloc_mode, &
                                              hybrid_dp_mode
#ifdef PRECISION_dp
  use ol_loop_handling_/**/REALKIND, only: hcl_alloc_hybrid
#endif
  implicit none
  type(redset4),       intent(in)    :: RedSet
  type(hcl),           intent(inout) :: Gin_A,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3
  type(hcl), optional, intent(inout) :: Gout_R1
  real(REALKIND)    :: alfa_max,alfa(3), gamma_inv
  complex(REALKIND) :: B3coeff
  integer           :: mode,perm(3)

  perm =RedSet%perm
  B3coeff = (RedSet%redbasis%gamma)*(RedSet%p3scalars(0))*2._/**/REALKIND
  alfa = [abs(RedSet%p3scalars(1)), abs(RedSet%p3scalars(2)), abs(B3coeff)]
  alfa = log10(alfa)

  if (hp_gamma_trig) then
    gamma_inv=log10( 1._/**/REALKIND / RedSet%gd2**2)
  else
    gamma_inv=0
  end if

  if (gdm2_err_estim_CL .eqv. .true.) then
    alfa_max = max(alfa(1),alfa(2))
  else
    alfa_max = max(alfa(1),alfa(2),alfa(3))
  end if

  Gout_A%error = Gin_A%error + max(alfa_max,gamma_inv)
  Gout_A0%error = Gin_A%error + max(alfa_max,gamma_inv)
  if (perm(1)+perm(2) == 3) then
    ! perm = 1,2,3
    Gout_A1%error = Gin_A%error + max(alfa(1),gamma_inv)
    Gout_A2%error = Gin_A%error + max(alfa(2),gamma_inv)
    Gout_A3%error = Gin_A%error + max(alfa(3),gamma_inv/2)
  else if (perm(1)+perm(2) == 4) then
    ! perm = 1,3,2
    Gout_A1%error = Gin_A%error + max(alfa(1),gamma_inv)
    Gout_A3%error = Gin_A%error + max(alfa(2),gamma_inv)
    Gout_A2%error = Gin_A%error + max(alfa(3),gamma_inv/2)
  else if (perm(1)+perm(2) == 5) then
    ! perm = 2,3,1
    Gout_A2%error = Gin_A%error + max(alfa(1),gamma_inv)
    Gout_A3%error = Gin_A%error + max(alfa(2),gamma_inv)
    Gout_A1%error = Gin_A%error + max(alfa(3),gamma_inv/2)
  end if

  call trigger_upgrade(alfa_max,RedSet%gd2,Gin_A)

  mode = Gin_A%mode
  Gout_A%mode = mode
  Gout_A0%mode = mode
  Gout_A1%mode = mode
  Gout_A2%mode = mode
  Gout_A3%mode = mode

  if (iand(mode, hybrid_dp_mode) .ne. 0) then
    Gout_A%nred = Gout_A%nred + 1
    Gout_A0%nred = 1
    Gout_A1%nred = 1
    Gout_A2%nred = 1
    Gout_A3%nred = 1
  end if

  if(present(Gout_R1)) then
    Gout_R1%error = Gout_A%error
    Gout_R1%mode = mode
  end if

#ifdef PRECISION_dp
  if (mode .gt. hybrid_dp_mode) then
    Gout_A%nred_qp = Gout_A%nred_qp + 1
    Gout_A0%nred_qp = 1
    Gout_A1%nred_qp = 1
    Gout_A2%nred_qp = 1
    Gout_A3%nred_qp = 1
  end if
  if (hp_alloc_mode .gt. 1 .and. mode .gt. hybrid_dp_mode) then
    call hcl_alloc_hybrid(Gout_A)
    call hcl_alloc_hybrid(Gout_A0)
    call hcl_alloc_hybrid(Gout_A1)
    call hcl_alloc_hybrid(Gout_A2)
    call hcl_alloc_hybrid(Gout_A3)
    if(present(Gout_R1)) call hcl_alloc_hybrid(Gout_R1)
  end if
#endif

  if (Gout_A%error > hp_max_err) hp_max_err = Gout_A%error

end subroutine err_estim_4pt_hcl

subroutine err_estim_5pt_hol(RedSet,Gin_A,Gout_A,Gout_A0,Gout_A1,Gout_A2, &
                             Gout_A3,Gout_A4,Gout_R1)
  use ol_data_types_/**/REALKIND, only: hol,REALKIND,QREALKIND,redset5
  use ol_parameters_decl_/**/REALKIND, only: hp_max_err
  use ol_parameters_decl_/**/DREALKIND, only: hp_gamma_trig, &
                                              hp_alloc_mode, &
                                              hybrid_dp_mode
#ifdef PRECISION_dp
  use ol_loop_handling_/**/REALKIND, only: hol_alloc_hybrid
#endif
  implicit none
  type(redset5),       intent(in)    :: RedSet
  type(hol),           intent(inout) :: Gin_A,Gout_A,Gout_A0,Gout_A1,Gout_A2, &
                                        Gout_A3,Gout_A4
  type(hol), optional, intent(inout) :: Gout_R1
  real(REALKIND)    :: alfa_max,alfa(3),gamma_inv
  complex(REALKIND) :: B3coeff
  integer           :: mode,perm(4)

  perm = RedSet%perm
  B3coeff = (RedSet%redbasis%gamma)*(RedSet%p3scalars(0))*2
  alfa = [abs(RedSet%p3scalars(1)), abs(RedSet%p3scalars(2)), abs(B3coeff)]
  alfa = log10(alfa)

  if (hp_gamma_trig) then
    gamma_inv=log10( 1._/**/REALKIND / RedSet%gd2**2)
  else
    gamma_inv=0
  end if

  if (gdm2_err_estim_OL .eqv. .true.) then
    alfa_max = max(alfa(1),alfa(2))
  else
    alfa_max = max(alfa(1),alfa(2),alfa(3))
  end if

  Gout_A%error = Gin_A%error + max(alfa(1),alfa(2),alfa(3),gamma_inv)
  Gout_A0%error = Gin_A%error + max(alfa(1),alfa(2),alfa(3),gamma_inv)
  if (perm(1) == 1) then
    if (perm(2) == 2) then
      if (perm(3) == 3) then
        ! 1,2,3,4 permutation
        Gout_A1%error = Gin_A%error + max(alfa(1),gamma_inv)
        Gout_A2%error = Gin_A%error + max(alfa(2),gamma_inv)
        Gout_A3%error = Gin_A%error + max(alfa(3),gamma_inv/2)
      else
        ! 1,2,4,3 permutation
        Gout_A1%error = Gin_A%error + max(alfa(1),gamma_inv)
        Gout_A2%error = Gin_A%error + max(alfa(2),gamma_inv)
        Gout_A4%error = Gin_A%error + max(alfa(3),gamma_inv/2)
      end if
    else if (perm(2) == 3) then
      if (perm(3) == 2) then
        ! 1,3,2,4 permutation
        Gout_A1%error = Gin_A%error + max(alfa(1),gamma_inv)
        Gout_A3%error = Gin_A%error + max(alfa(2),gamma_inv)
        Gout_A2%error = Gin_A%error + max(alfa(3),gamma_inv/2)
      else
        ! 1,3,4,2 permutation
        Gout_A1%error = Gin_A%error + max(alfa(1),gamma_inv)
        Gout_A3%error = Gin_A%error + max(alfa(2),gamma_inv)
        Gout_A4%error = Gin_A%error + max(alfa(3),gamma_inv/2)
      end if
    else if (perm(2) == 4) then
      if (perm(3) == 2) then
        ! 1,4,2,3 permutation
        Gout_A1%error = Gin_A%error + max(alfa(1),gamma_inv)
        Gout_A4%error = Gin_A%error + max(alfa(2),gamma_inv)
        Gout_A2%error = Gin_A%error + max(alfa(3),gamma_inv/2)
        Gout_A3%error = 0
      else
        ! 1,4,3,2 permutation
        Gout_A1%error = Gin_A%error + max(alfa(1),gamma_inv)
        Gout_A4%error = Gin_A%error + max(alfa(2),gamma_inv)
        Gout_A3%error = Gin_A%error + max(alfa(3),gamma_inv/2)
    end if

    else if (perm(1) == 2) then
      if (perm(2) == 3) then
        if (perm(3) == 1) then
          ! 2,3,1,4 permutation
          Gout_A2%error = Gin_A%error + max(alfa(1),gamma_inv)
          Gout_A3%error = Gin_A%error + max(alfa(2),gamma_inv)
          Gout_A1%error = Gin_A%error + max(alfa(3),gamma_inv/2)
        else
          ! 2,3,4,1 permutation
          Gout_A2%error = Gin_A%error + max(alfa(1),gamma_inv)
          Gout_A3%error = Gin_A%error + max(alfa(2),gamma_inv)
          Gout_A4%error = Gin_A%error + max(alfa(3),gamma_inv/2)
        end if
      else if(perm(2) == 4) then
        if (perm(3) == 1) then
          ! 2,4,1,3 permutation
          Gout_A2%error = Gin_A%error + max(alfa(1),gamma_inv)
          Gout_A4%error = Gin_A%error + max(alfa(2),gamma_inv)
          Gout_A1%error = Gin_A%error + max(alfa(3),gamma_inv/2)
        else
          ! 2,4,3,1 permutation
          Gout_A2%error = Gin_A%error + max(alfa(1),gamma_inv)
          Gout_A4%error = Gin_A%error + max(alfa(2),gamma_inv)
          Gout_A3%error = Gin_A%error + max(alfa(3),gamma_inv/2)
        end if
      end if
    end if
  end if

  call trigger_upgrade(alfa_max,RedSet%gd2,Gin_A)

  mode = Gin_A%mode
  Gout_A%mode = mode
  Gout_A0%mode = mode
  Gout_A1%mode = mode
  Gout_A2%mode = mode
  Gout_A3%mode = mode
  Gout_A4%mode = mode

  if (iand(mode, hybrid_dp_mode) .ne. 0) then
    Gout_A%nred = Gout_A%nred + 1
    Gout_A0%nred = 1
    Gout_A1%nred = 1
    Gout_A2%nred = 1
    Gout_A3%nred = 1
    Gout_A4%nred = 1
  end if

  if(present(Gout_R1)) then
    Gout_R1%error = Gout_A%error
    Gout_R1%mode = mode
  end if

#ifdef PRECISION_dp
  if (mode .gt. hybrid_dp_mode) then
    Gout_A%nred_qp = Gout_A%nred_qp + 1
    Gout_A0%nred_qp = 1
    Gout_A1%nred_qp = 1
    Gout_A2%nred_qp = 1
    Gout_A3%nred_qp = 1
    Gout_A4%nred_qp = 1
  end if
  if (hp_alloc_mode .gt. 1 .and. mode .gt. hybrid_dp_mode) then
    call hol_alloc_hybrid(Gout_A)
    call hol_alloc_hybrid(Gout_A0)
    call hol_alloc_hybrid(Gout_A1)
    call hol_alloc_hybrid(Gout_A2)
    call hol_alloc_hybrid(Gout_A3)
    call hol_alloc_hybrid(Gout_A4)
    if(present(Gout_R1)) call hol_alloc_hybrid(Gout_R1)
  end if
#endif

  if (Gout_A%error > hp_max_err) hp_max_err = Gout_A%error

end subroutine err_estim_5pt_hol

subroutine err_estim_5pt_hcl(RedSet,Gin_A,Gout_A,Gout_A0,Gout_A1,Gout_A2, &
                             Gout_A3,Gout_A4,Gout_R1)
  use ol_data_types_/**/REALKIND, only: hcl,REALKIND,QREALKIND,redset5
  use ol_parameters_decl_/**/REALKIND, only: hp_max_err
  use ol_parameters_decl_/**/DREALKIND, only: hp_gamma_trig, &
                                              hp_alloc_mode, &
                                              hybrid_dp_mode
#ifdef PRECISION_dp
  use ol_loop_handling_/**/REALKIND, only: hcl_alloc_hybrid
#endif
  implicit none
  type(redset5),       intent(in)    :: RedSet
  type(hcl),           intent(inout) :: Gin_A,Gout_A,Gout_A0,Gout_A1,Gout_A2, &
                                        Gout_A3,Gout_A4
  type(hcl), optional, intent(inout) :: Gout_R1
  real(REALKIND)    :: alfa_max,alfa(3),gamma_inv
  complex(REALKIND) :: B3coeff
  integer           :: mode,perm(4)

  perm = RedSet%perm

  if (hp_gamma_trig) then
    gamma_inv=log10( 1._/**/REALKIND / RedSet%gd2**2)
  else
    gamma_inv=0
  end if

  B3coeff = (RedSet%redbasis%gamma)*(RedSet%p3scalars(0))*2
  alfa = [abs(RedSet%p3scalars(1)), abs(RedSet%p3scalars(2)), abs(B3coeff)]
  alfa = log10(alfa)

  if (gdm2_err_estim_CL .eqv. .true.) then
    alfa_max = max(alfa(1),alfa(2))
  else
    alfa_max = max(alfa(1),alfa(2),alfa(3))
  end if

  Gout_A%error = Gin_A%error + max(alfa_max,gamma_inv)
  Gout_A0%error = Gin_A%error + max(alfa_max,gamma_inv)
  if (perm(1) == 1) then
    if (perm(2) == 2) then
      if (perm(3) == 3) then
        ! 1,2,3,4 permutation
        Gout_A1%error = Gin_A%error + max(alfa(1),gamma_inv)
        Gout_A2%error = Gin_A%error + max(alfa(2),gamma_inv)
        Gout_A3%error = Gin_A%error + max(alfa(3),gamma_inv/2)
      else
        ! 1,2,4,3 permutation
        Gout_A1%error = Gin_A%error + max(alfa(1),gamma_inv)
        Gout_A2%error = Gin_A%error + max(alfa(2),gamma_inv)
        Gout_A4%error = Gin_A%error + max(alfa(3),gamma_inv/2)
      end if
    else if (perm(2) == 3) then
      if (perm(3) == 2) then
        ! 1,3,2,4 permutation
        Gout_A1%error = Gin_A%error + max(alfa(1),gamma_inv)
        Gout_A3%error = Gin_A%error + max(alfa(2),gamma_inv)
        Gout_A2%error = Gin_A%error + max(alfa(3),gamma_inv/2)
      else
        ! 1,3,4,2 permutation
        Gout_A1%error = Gin_A%error + max(alfa(1),gamma_inv)
        Gout_A3%error = Gin_A%error + max(alfa(2),gamma_inv)
        Gout_A4%error = Gin_A%error + max(alfa(3),gamma_inv/2)
      end if
    else if (perm(2) == 4) then
      if (perm(3) == 2) then
        ! 1,4,2,3 permutation
        Gout_A1%error = Gin_A%error + max(alfa(1),gamma_inv)
        Gout_A4%error = Gin_A%error + max(alfa(2),gamma_inv)
        Gout_A2%error = Gin_A%error + max(alfa(3),gamma_inv/2)
      else
        ! 1,4,3,2 permutation
        Gout_A1%error = Gin_A%error + max(alfa(1),gamma_inv)
        Gout_A4%error = Gin_A%error + max(alfa(2),gamma_inv)
        Gout_A3%error = Gin_A%error + max(alfa(3),gamma_inv/2)
    end if

    else if (perm(1) == 2) then
      if (perm(2) == 3) then
        if (perm(3) == 1) then
          ! 2,3,1,4 permutation
          Gout_A2%error = Gin_A%error + max(alfa(1),gamma_inv)
          Gout_A3%error = Gin_A%error + max(alfa(2),gamma_inv)
          Gout_A1%error = Gin_A%error + max(alfa(3),gamma_inv/2)
        else
          ! 2,3,4,1 permutation
          Gout_A2%error = Gin_A%error + max(alfa(1),gamma_inv)
          Gout_A3%error = Gin_A%error + max(alfa(2),gamma_inv)
          Gout_A4%error = Gin_A%error + max(alfa(3),gamma_inv/2)
        end if
      else if(perm(2) == 4) then
        if (perm(3) == 1) then
          ! 2,4,1,3 permutation
          Gout_A2%error = Gin_A%error + max(alfa(1),gamma_inv)
          Gout_A4%error = Gin_A%error + max(alfa(2),gamma_inv)
          Gout_A1%error = Gin_A%error + max(alfa(3),gamma_inv/2)
        else
          ! 2,4,3,1 permutation
          Gout_A2%error = Gin_A%error + max(alfa(1),gamma_inv)
          Gout_A4%error = Gin_A%error + max(alfa(2),gamma_inv)
          Gout_A3%error = Gin_A%error + max(alfa(3),gamma_inv/2)
        end if
      end if
    end if
  end if

  call trigger_upgrade(alfa_max,RedSet%gd2,Gin_A)

  mode = Gin_A%mode
  Gout_A%mode = mode
  Gout_A0%mode = mode
  Gout_A1%mode = mode
  Gout_A2%mode = mode
  Gout_A3%mode = mode
  Gout_A4%mode = mode

  if (iand(mode, hybrid_dp_mode) .ne. 0) then
    Gout_A%nred = Gout_A%nred + 1
    Gout_A0%nred = 1
    Gout_A1%nred = 1
    Gout_A2%nred = 1
    Gout_A3%nred = 1
    Gout_A4%nred = 1
  end if

  if(present(Gout_R1)) then
    Gout_R1%error = Gout_A%error
    Gout_R1%mode = mode
  end if

#ifdef PRECISION_dp
  if (mode .gt. hybrid_dp_mode) then
    Gout_A%nred_qp = Gout_A%nred_qp + 1
    Gout_A0%nred_qp = 1
    Gout_A1%nred_qp = 1
    Gout_A2%nred_qp = 1
    Gout_A3%nred_qp = 1
    Gout_A4%nred_qp = 1
  end if
  if (hp_alloc_mode .gt. 1 .and. mode .gt. hybrid_dp_mode) then
    call hcl_alloc_hybrid(Gout_A)
    call hcl_alloc_hybrid(Gout_A0)
    call hcl_alloc_hybrid(Gout_A1)
    call hcl_alloc_hybrid(Gout_A2)
    call hcl_alloc_hybrid(Gout_A3)
    call hcl_alloc_hybrid(Gout_A4)
    if(present(Gout_R1)) call hcl_alloc_hybrid(Gout_R1)
  end if
#endif

  if (Gout_A%error > hp_max_err) hp_max_err = Gout_A%error

end subroutine err_estim_5pt_hcl


! -----------------------------------------------------------------------------
! Downgrade to dp for unnecessary reduction coefficients from B1, B2, B3
!  -----------------------------------------------------------------------------

#ifdef PRECISION_dp
subroutine downgrade_4pt_hol(RedSet,mode_in,Gout_A1,Gout_A2,Gout_A3)
  use ol_data_types_/**/REALKIND, only: hol,REALKIND,redset4
  use ol_parameters_decl_/**/REALKIND, only: hp_step_thres,hybrid_dp_mode
  implicit none
  type(redset4), intent(in)     :: RedSet
  integer,       intent(in)    :: mode_in
  type(hol),     intent(inout) :: Gout_A1,Gout_A2,Gout_A3
  real(REALKIND) :: alfa(3)
  integer        :: perm(3)

    if ((-log10(RedSet%gd2) > hp_step_thres)) return

    alfa = [abs(RedSet%p3scalars(1)), abs(RedSet%p3scalars(2)), &
            abs((RedSet%redbasis%gamma)*(RedSet%p3scalars(0))*2)]
    alfa = log10(alfa)

    perm = RedSet%perm
    if (mode_in == hybrid_dp_mode) then
    if (perm(1)+perm(2) == 3) then
      ! perm = 1,2,3
      call downgrade(Gout_A1,Gout_A2,Gout_A3)
    else if (perm(1)+perm(2) == 4) then
      ! perm = 1,3,2
      call downgrade(Gout_A1,Gout_A3,Gout_A2)
    else if (perm(1)+perm(2) == 5) then
      ! perm = 2,3,1
      call downgrade(Gout_A2,Gout_A3,Gout_A1)
    end if
  end if

  contains

  subroutine downgrade(Gout_P1,Gout_P2,Gout_P3)
    use ol_parameters_decl_/**/REALKIND, only: hp_step_thres,hp_err_thres
    use ol_loop_handling_/**/REALKIND, only: downgrade_dp
    type(hol), intent(inout) :: Gout_P1,Gout_P2,Gout_P3
    if (alfa(1) < hp_step_thres .and. Gout_P1%error <  hp_err_thres) then
      !Gout_P1%ndrs_qp = Gout_P1%ndrs_qp - 1
      call downgrade_dp(Gout_P1)
    end if
    if (alfa(2) < hp_step_thres .and. Gout_P2%error <  hp_err_thres) then
      !Gout_P2%ndrs_qp = Gout_P2%ndrs_qp - 1
      call downgrade_dp(Gout_P2)
    end if
    if (alfa(3) < hp_step_thres .and. Gout_P3%error <  hp_err_thres) then
      !Gout_P3%ndrs_qp = Gout_P3%ndrs_qp - 1
      call downgrade_dp(Gout_P3)
    end if
  end subroutine downgrade

end subroutine downgrade_4pt_hol

subroutine downgrade_4pt_hcl(RedSet,mode_in,Gout_A1,Gout_A2,Gout_A3)
  use ol_data_types_/**/REALKIND, only: hcl,REALKIND,redset4
  use ol_parameters_decl_/**/REALKIND, only: hp_step_thres,hybrid_dp_mode
  implicit none
  type(redset4), intent(in)    :: RedSet
  integer,       intent(in)    :: mode_in
  type(hcl),     intent(inout) :: Gout_A1,Gout_A2,Gout_A3
  real(REALKIND) :: alfa(3)
  integer        :: perm(3)

    if ((-log10(RedSet%gd2) > hp_step_thres)) return

    alfa = [abs(RedSet%p3scalars(1)), abs(RedSet%p3scalars(2)), &
            abs((RedSet%redbasis%gamma)*(RedSet%p3scalars(0))*2)]
    alfa = log10(alfa)

    perm = RedSet%perm
    if (mode_in == hybrid_dp_mode) then
    if (perm(1)+perm(2) == 3) then
      ! perm = 1,2,3
      call downgrade(Gout_A1,Gout_A2,Gout_A3)
    else if (perm(1)+perm(2) == 4) then
      ! perm = 1,3,2
      call downgrade(Gout_A1,Gout_A3,Gout_A2)
    else if (perm(1)+perm(2) == 5) then
      ! perm = 2,3,1
      call downgrade(Gout_A2,Gout_A3,Gout_A1)
    end if
  end if

  contains

  subroutine downgrade(Gout_P1,Gout_P2,Gout_P3)
    use ol_parameters_decl_/**/REALKIND, only: hp_step_thres,hp_err_thres
    use ol_loop_handling_/**/REALKIND, only: downgrade_dp
    type(hcl), intent(inout) :: Gout_P1,Gout_P2,Gout_P3
    if (alfa(1) < hp_step_thres .and. Gout_P1%error <  hp_err_thres) then
      !Gout_P1%ndrs_qp = Gout_P1%ndrs_qp - 1
      call downgrade_dp(Gout_P1)
    end if
    if (alfa(2) < hp_step_thres .and. Gout_P2%error <  hp_err_thres) then
      !Gout_P2%ndrs_qp = Gout_P2%ndrs_qp - 1
      call downgrade_dp(Gout_P2)
    end if
    if (alfa(3) < hp_step_thres .and. Gout_P3%error <  hp_err_thres) then
      !Gout_P3%ndrs_qp = Gout_P3%ndrs_qp - 1
      call downgrade_dp(Gout_P3)
    end if
  end subroutine downgrade

end subroutine downgrade_4pt_hcl

subroutine downgrade_5pt_hol(RedSet,mode_in,Gout_A1,Gout_A2,Gout_A3,Gout_A4)
  use ol_data_types_/**/REALKIND, only: hol,REALKIND,redset5
  use ol_parameters_decl_/**/REALKIND, only: hp_step_thres,hybrid_dp_mode
  implicit none
  type(redset5), intent(in)    :: RedSet
  integer,       intent(in)    :: mode_in
  type(hol),     intent(inout) :: Gout_A1,Gout_A2,Gout_A3,Gout_A4
  real(REALKIND) :: alfa(3)
  integer        :: perm(4)

  if ((-log10(RedSet%gd2) > hp_step_thres)) return

  alfa = [abs(RedSet%p3scalars(1)), abs(RedSet%p3scalars(2)), &
          abs((RedSet%redbasis%gamma)*(RedSet%p3scalars(0))*2)]
  alfa = log10(alfa)

  perm = RedSet%perm

  if (mode_in == hybrid_dp_mode) then
    if (perm(1) == 1) then
      if (perm(2) == 2) then
        if (perm(3) == 3) then
          ! 1,2,3,4 permutation
          call downgrade(Gout_A1,Gout_A2,Gout_A3)
        else
          ! 1,2,4,3 permutation
          call downgrade(Gout_A1,Gout_A2,Gout_A4)
        end if
      else if (perm(2) == 3) then
        if (perm(3) == 2) then
          ! 1,3,2,4 permutation
          call downgrade(Gout_A1,Gout_A3,Gout_A2)
        else
          ! 1,3,4,2 permutation
          call downgrade(Gout_A1,Gout_A3,Gout_A4)
        end if
      else if (perm(2) == 4) then
        if (perm(3) == 2) then
          ! 1,4,2,3 permutation
          call downgrade(Gout_A1,Gout_A4,Gout_A2)
        else
          ! 1,4,3,2 permutation
          call downgrade(Gout_A1,Gout_A4,Gout_A3)
        end if
      end if

    else if (perm(1) == 2) then
      if (perm(2) == 3) then
        if (perm(3) == 1) then
          ! 2,3,1,4 permutation
          call downgrade(Gout_A2,Gout_A3,Gout_A1)
        else
          ! 2,3,4,1 permutation
          call downgrade(Gout_A2,Gout_A3,Gout_A4)
        end if
      else if(perm(2) == 4) then
        if (perm(3) == 1) then
          ! 2,4,1,3 permutation
          call downgrade(Gout_A2,Gout_A4,Gout_A1)
        else
          ! 2,4,3,1 permutation
          call downgrade(Gout_A2,Gout_A4,Gout_A3)
        end if
      end if
    end if
  end if

  contains

  subroutine downgrade(Gout_P1,Gout_P2,Gout_P3)
    use ol_parameters_decl_/**/REALKIND, only: hp_step_thres,hp_err_thres
    use ol_loop_handling_/**/REALKIND, only: downgrade_dp
    type(hol), intent(inout) :: Gout_P1,Gout_P2,Gout_P3
    if (alfa(1) < hp_step_thres .and. Gout_P1%error <  hp_err_thres) then
      !Gout_P1%ndrs_qp = Gout_P1%ndrs_qp - 1
      call downgrade_dp(Gout_P1)
    end if
    if (alfa(2) < hp_step_thres .and. Gout_P2%error <  hp_err_thres) then
      !Gout_P2%ndrs_qp = Gout_P2%ndrs_qp - 1
      call downgrade_dp(Gout_P2)
    end if
    if (alfa(3) < hp_step_thres .and. Gout_P3%error <  hp_err_thres) then
      !Gout_P3%ndrs_qp = Gout_P3%ndrs_qp - 1
      call downgrade_dp(Gout_P3)
    end if
  end subroutine downgrade

end subroutine downgrade_5pt_hol

subroutine downgrade_5pt_hcl(RedSet,mode_in,Gout_A1,Gout_A2,Gout_A3,Gout_A4)
  use ol_data_types_/**/REALKIND, only: hcl,REALKIND,redset5
  use ol_parameters_decl_/**/REALKIND, only: hp_step_thres,hybrid_dp_mode
  implicit none
  type(redset5), intent(in)    :: RedSet
  integer,       intent(in)    :: mode_in
  type(hcl),     intent(inout) :: Gout_A1,Gout_A2,Gout_A3,Gout_A4
  real(REALKIND) :: alfa(3)
  integer        :: perm(4)

  if ((-log10(RedSet%gd2) > hp_step_thres)) return

  alfa = [abs(RedSet%p3scalars(1)), abs(RedSet%p3scalars(2)), &
          abs((RedSet%redbasis%gamma)*(RedSet%p3scalars(0))*2)]
  alfa = log10(alfa)

  perm = RedSet%perm

  if (mode_in == hybrid_dp_mode) then
    if (perm(1) == 1) then
      if (perm(2) == 2) then
        if (perm(3) == 3) then
          ! 1,2,3,4 permutation
          call downgrade(Gout_A1,Gout_A2,Gout_A3)
        else
          ! 1,2,4,3 permutation
          call downgrade(Gout_A1,Gout_A2,Gout_A4)
        end if
      else if (perm(2) == 3) then
        if (perm(3) == 2) then
          ! 1,3,2,4 permutation
          call downgrade(Gout_A1,Gout_A3,Gout_A2)
        else
          ! 1,3,4,2 permutation
          call downgrade(Gout_A1,Gout_A3,Gout_A4)
        end if
      else if (perm(2) == 4) then
        if (perm(3) == 2) then
          ! 1,4,2,3 permutation
          call downgrade(Gout_A1,Gout_A4,Gout_A2)
        else
          ! 1,4,3,2 permutation
          call downgrade(Gout_A1,Gout_A4,Gout_A3)
        end if
      end if

    else if (perm(1) == 2) then
      if (perm(2) == 3) then
        if (perm(3) == 1) then
          ! 2,3,1,4 permutation
          call downgrade(Gout_A2,Gout_A3,Gout_A1)
        else
          ! 2,3,4,1 permutation
          call downgrade(Gout_A2,Gout_A3,Gout_A4)
        end if
      else if(perm(2) == 4) then
        if (perm(3) == 1) then
          ! 2,4,1,3 permutation
          call downgrade(Gout_A2,Gout_A4,Gout_A1)
        else
          ! 2,4,3,1 permutation
          call downgrade(Gout_A2,Gout_A4,Gout_A3)
        end if
      end if
    end if
  end if

  contains

  subroutine downgrade(Gout_P1,Gout_P2,Gout_P3)
    use ol_parameters_decl_/**/REALKIND, only: hp_step_thres,hp_err_thres
    use ol_loop_handling_/**/REALKIND, only: downgrade_dp
    type(hcl), intent(inout) :: Gout_P1,Gout_P2,Gout_P3
    if (alfa(1) < hp_step_thres .and. Gout_P1%error <  hp_err_thres) then
      !Gout_P1%ndrs_qp = Gout_P1%ndrs_qp - 1
      call downgrade_dp(Gout_P1)
    end if
    if (alfa(2) < hp_step_thres .and. Gout_P2%error <  hp_err_thres) then
      !Gout_P2%ndrs_qp = Gout_P2%ndrs_qp - 1
      call downgrade_dp(Gout_P2)
    end if
    if (alfa(3) < hp_step_thres .and. Gout_P3%error <  hp_err_thres) then
      !Gout_P3%ndrs_qp = Gout_P3%ndrs_qp - 1
      call downgrade_dp(Gout_P3)
    end if
  end subroutine downgrade

end subroutine downgrade_5pt_hcl

#endif

! =============================================================================
!                      HELICITY BOOKKEEPING INTERFACE
! -----------------------------------------------------------------------------
! Reduction steps for all the non-vanishing helicitiy configurations
! =============================================================================

! ******************************************************************************
subroutine Hotf_4pt_red(Gin_A,RedSet_4,msq,Gout_A,Gout_A0,Gout_A1,Gout_A2, &
                        Gout_A3,nhel)
! ------------------------------------------------------------------------------
! On-the-fly reduction for 4-pt segments. No R1 rational contributions
! ******************************************************************************
  use KIND_TYPES, only: REALKIND,QREALKIND
  use ol_data_types_/**/REALKIND, only: redset4, hol
  use ofred_basis_construction_/**/REALKIND, only: reconstruct_redset4
  use ol_kinematics_/**/REALKIND, only: get_mass2
#ifdef PRECISION_dp
  use ol_data_types_/**/QREALKIND, only: redset4_qp=>redset4
  use ofred_basis_construction_/**/REALKIND, only: upgrade_redset4
  use ofred_reduction_/**/QREALKIND, only: otf_4pt_red_qp=>otf_4pt_red
  use ol_loop_handling_/**/REALKIND, only: req_qp_cmp,hol_dealloc_hybrid
  use ol_kinematics_/**/QREALKIND, only: get_mass2_qp=>get_mass2
#endif
  use ol_parameters_decl_/**/DREALKIND, only: hp_switch,hybrid_qp_mode, &
                                              hp_alloc_mode
  implicit none
  type(redset4), intent(in)    :: RedSet_4
  integer,       intent(in)    :: msq(0:3)
  integer,       intent(in)    :: nhel
  type(hol),     intent(inout) :: Gin_A,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3
  integer           :: h, mode_in, phys_hel
#ifdef PRECISION_dp
  type(redset4_qp) :: RedSet_4_qp
#endif

  mode_in = Gin_A%mode
  phys_hel = size(Gin_A%hf)

  if (.not. valid_4pt(Gin_A, Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3)) return
  call err_estim_4pt(RedSet_4,Gin_A,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3)


#ifdef PRECISION_dp
  if (Gin_A%mode .ne. hybrid_qp_mode) then
#endif
    do h = 1, phys_hel
      call otf_4pt_red(Gin_A%j(:,:,:,h),   &
                       RedSet_4,           &
                       get_mass2(msq),     &
                       Gout_A%j(:,:,:,h),  &
                       Gout_A0%j(:,:,:,h), &
                       Gout_A1%j(:,:,:,h), &
                       Gout_A2%j(:,:,:,h), &
                       Gout_A3%j(:,:,:,h))
    end do
#ifdef PRECISION_dp
  else
    ! TODO:  <14-11-18, J.-N. Lang> !
    ! do we need to initialise?
    Gout_A%j = 0
    Gout_A0%j = 0
    Gout_A1%j = 0
    Gout_A2%j = 0
    Gout_A3%j = 0
  end if
#endif


#ifdef PRECISION_dp
  if (req_qp_cmp(Gout_A)) then

    call upgrade_redset4(RedSet_4, RedSet_4_qp)
    do h = 1, phys_hel
      call otf_4pt_red_qp(Gin_A%j_qp(:,:,:,h),   &
                          RedSet_4_qp,           &
                          get_mass2_qp(msq),     &
                          Gout_A%j_qp(:,:,:,h),  &
                          Gout_A0%j_qp(:,:,:,h), &
                          Gout_A1%j_qp(:,:,:,h), &
                          Gout_A2%j_qp(:,:,:,h), &
                          Gout_A3%j_qp(:,:,:,h))
    end do

    call downgrade_4pt(RedSet_4,mode_in,Gout_A1,Gout_A2,Gout_A3)
    call hol_dealloc_hybrid(Gin_A)

  else if (hp_switch .eq. 1) then
    if (hp_alloc_mode .eq. 0) then
      Gout_A%j_qp = 0
      Gout_A0%j_qp = 0
      Gout_A1%j_qp = 0
      Gout_A2%j_qp = 0
      Gout_A3%j_qp = 0
    end if
  end if

#endif


end subroutine Hotf_4pt_red


! ******************************************************************************
subroutine Hotf_4pt_red_R1(Gin_A,RedSet_4,msq,Gout_A,Gout_A0,Gout_A1,Gout_A2, &
                           Gout_A3,Gout_R1,nhel)
! ------------------------------------------------------------------------------
! On-the-fly reduction for 4-pt segments with R1-rational contribution
! ******************************************************************************
  use KIND_TYPES, only: REALKIND,QREALKIND
  use ol_data_types_/**/REALKIND, only: redset4,hol
  use ol_kinematics_/**/REALKIND, only: get_mass2
#ifdef PRECISION_dp
  use ol_data_types_/**/QREALKIND, only: redset4_qp=>redset4
  use ofred_basis_construction_/**/REALKIND, only: upgrade_redset4
  use ofred_reduction_/**/QREALKIND, only: otf_4pt_red_qp=>otf_4pt_red
  use ol_loop_handling_/**/REALKIND, only: req_qp_cmp,hol_dealloc_hybrid
  use ol_kinematics_/**/QREALKIND, only: get_mass2_qp=>get_mass2
#endif
  use ol_parameters_decl_/**/DREALKIND, only: hp_switch,hybrid_qp_mode, &
                                              hp_alloc_mode
  implicit none
  type(redset4), intent(in)    :: RedSet_4
  integer,       intent(in)    :: msq(0:3)
  type(hol),     intent(inout) :: Gin_A,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3, &
                                  Gout_R1
  integer,       intent(in)    :: nhel
  integer :: h, mode_in, phys_hel
#ifdef PRECISION_dp
  type(redset4_qp) :: RedSet_4_qp
#endif

  mode_in = Gin_A%mode
  phys_hel = size(Gin_A%hf)

  if (.not. valid_4pt(Gin_A, Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3,Gout_R1)) return
  call err_estim_4pt(RedSet_4,Gin_A,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3,Gout_R1)


#ifdef PRECISION_dp
  if (Gin_A%mode.ne.hybrid_qp_mode) then
#endif
    do h = 1, phys_hel
      call otf_4pt_red(Gin_A%j(:,:,:,h),   &
                       RedSet_4,           &
                       get_mass2(msq),     &
                       Gout_A%j(:,:,:,h),  &
                       Gout_A0%j(:,:,:,h), &
                       Gout_A1%j(:,:,:,h), &
                       Gout_A2%j(:,:,:,h), &
                       Gout_A3%j(:,:,:,h), &
                       Gout_R1%j(:,:,:,h))
    end do
#ifdef PRECISION_dp
  else
    Gout_A%j = 0
    Gout_A0%j = 0
    Gout_A1%j = 0
    Gout_A2%j = 0
    Gout_A3%j = 0
    Gout_R1%j = 0
  end if
#endif


#ifdef PRECISION_dp
  if (req_qp_cmp(Gout_A)) then
    call upgrade_redset4(RedSet_4, RedSet_4_qp)
    do h = 1, phys_hel
     call otf_4pt_red_qp(Gin_A%j_qp(:,:,:,h),   &
                         RedSet_4_qp,           &
                         get_mass2_qp(msq),     &
                         Gout_A%j_qp(:,:,:,h),  &
                         Gout_A0%j_qp(:,:,:,h), &
                         Gout_A1%j_qp(:,:,:,h), &
                         Gout_A2%j_qp(:,:,:,h), &
                         Gout_A3%j_qp(:,:,:,h), &
                         Gout_R1%j_qp(:,:,:,h))
    end do

    call downgrade_4pt(RedSet_4,mode_in,Gout_A1,Gout_A2,Gout_A3)
    call hol_dealloc_hybrid(Gin_A)

  else if (hp_switch .eq. 1) then
    if (hp_alloc_mode .eq. 0) then
      Gout_A%j_qp = 0
      Gout_A0%j_qp = 0
      Gout_A1%j_qp = 0
      Gout_A2%j_qp = 0
      Gout_A3%j_qp = 0
      Gout_R1%j_qp = 0
    end if
  end if

#endif


end subroutine Hotf_4pt_red_R1


! ******************************************************************************
subroutine Hotf_5pt_red(Gin_A,RedSet_5,msq,Gout_A,Gout_A0,Gout_A1,Gout_A2, &
                        Gout_A3,Gout_A4,nhel)
! ------------------------------------------------------------------------------
! On-the-fly reduction for 5-pt segments. No R1 rational contributions
! ******************************************************************************
  use KIND_TYPES, only: REALKIND,QREALKIND
  use ol_data_types_/**/REALKIND, only: redset5,hol
  use ol_parameters_decl_/**/REALKIND, only: rzero
  use ol_kinematics_/**/REALKIND, only: get_mass2
#ifdef PRECISION_dp
  use ol_data_types_/**/QREALKIND, only: redset5_qp=>redset5
  use ofred_basis_construction_/**/REALKIND, only: upgrade_redset5
  use ofred_reduction_/**/QREALKIND, only: otf_5pt_red_qp=>otf_5pt_red
  use ol_loop_handling_/**/REALKIND, only: req_qp_cmp,hol_dealloc_hybrid
  use ol_kinematics_/**/QREALKIND, only: get_mass2_qp=>get_mass2
#endif
  use ol_loop_handling_/**/REALKIND, only: hybrid_zero_mode
  use ol_parameters_decl_/**/DREALKIND, only: hp_switch,hybrid_qp_mode, &
                                              hp_alloc_mode
  implicit none
  type(redset5), intent(in)    :: RedSet_5
  integer,       intent(in)    :: msq(0:4)
  type(hol),     intent(inout) :: Gin_A,Gout_A,Gout_A0,Gout_A1,Gout_A2, &
                                  Gout_A3,Gout_A4
  integer,       intent(in)    :: nhel
  integer :: h, mode_in, phys_hel
#ifdef PRECISION_dp
  type(redset5_qp) :: RedSet_5_qp
#endif

  mode_in = Gin_A%mode
  phys_hel = size(Gin_A%hf)

  if (.not. valid_5pt(Gin_A, Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3,Gout_A4)) return
  call err_estim_5pt(RedSet_5,Gin_A,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3,Gout_A4)

#ifdef PRECISION_dp
  if (Gin_A%mode .ne. hybrid_qp_mode) then
#endif
    do h = 1, phys_hel
        call otf_5pt_red(Gin_A%j(:,:,:,h),   &
                         RedSet_5,           &
                         get_mass2(msq),      &
                         Gout_A%j(:,:,:,h),  &
                         Gout_A0%j(:,:,:,h), &
                         Gout_A1%j(:,:,:,h), &
                         Gout_A2%j(:,:,:,h), &
                         Gout_A3%j(:,:,:,h), &
                         Gout_A4%j(:,:,:,h))
    end do
#ifdef PRECISION_dp
  else
    Gout_A%j = 0
    Gout_A0%j = 0
    Gout_A1%j = 0
    Gout_A2%j = 0
    Gout_A3%j = 0
    Gout_A4%j = 0
  end if
#endif


  if ( RedSet_5%perm(4).eq.1 ) then
    Gout_A1%mode = hybrid_zero_mode
    Gout_A1%error = rzero
    Gout_A1%ndrs = 0
    Gout_A1%nred = 0
#ifdef PRECISION_dp
    Gout_A1%ndrs_qp = 0
    Gout_A1%nred_qp = 0
#endif
  else if ( RedSet_5%perm(4).eq.2 ) then
    Gout_A2%mode = hybrid_zero_mode
    Gout_A2%error = rzero
    Gout_A2%ndrs = 0
    Gout_A2%nred = 0
#ifdef PRECISION_dp
    Gout_A2%ndrs_qp = 0
    Gout_A2%nred_qp = 0
#endif
  else if ( RedSet_5%perm(4).eq.3 ) then
    Gout_A3%mode = hybrid_zero_mode
    Gout_A3%error = rzero
    Gout_A3%ndrs = 0
    Gout_A3%nred = 0
#ifdef PRECISION_dp
    Gout_A3%ndrs_qp = 0
    Gout_A3%nred_qp = 0
#endif
  else if ( RedSet_5%perm(4).eq.4 ) then
    Gout_A4%mode = hybrid_zero_mode
    Gout_A4%error = rzero
    Gout_A4%ndrs = 0
    Gout_A4%nred = 0
#ifdef PRECISION_dp
    Gout_A4%ndrs_qp = 0
    Gout_A4%nred_qp = 0
#endif
  end if


#ifdef PRECISION_dp
  if (req_qp_cmp(Gout_A)) then
    call upgrade_redset5(RedSet_5,RedSet_5_qp)
    do h = 1, phys_hel
      call otf_5pt_red_qp(Gin_A%j_qp(:,:,:,h),   &
                          RedSet_5_qp,           &
                          get_mass2_qp(msq),      &
                          Gout_A%j_qp(:,:,:,h),  &
                          Gout_A0%j_qp(:,:,:,h), &
                          Gout_A1%j_qp(:,:,:,h), &
                          Gout_A2%j_qp(:,:,:,h), &
                          Gout_A3%j_qp(:,:,:,h), &
                          Gout_A4%j_qp(:,:,:,h))
    end do

    call downgrade_5pt(RedSet_5,mode_in,Gout_A1,Gout_A2,Gout_A3,Gout_A4)
    call hol_dealloc_hybrid(Gin_A)

  else if (hp_switch .eq. 1) then
    if (hp_alloc_mode .eq. 0) then
      Gout_A%j_qp = 0
      Gout_A0%j_qp = 0
      Gout_A1%j_qp = 0
      Gout_A2%j_qp = 0
      Gout_A3%j_qp = 0
      Gout_A4%j_qp = 0
    end if
  end if
#endif

end subroutine Hotf_5pt_red


! ******************************************************************************
subroutine Hotf_5pt_red_R1(Gin_A,RedSet_5,msq,Gout_A,Gout_A0,Gout_A1, &
                           Gout_A2,Gout_A3,Gout_A4,Gout_R1,nhel)
! ------------------------------------------------------------------------------
! On-the-fly reduction for 5-pt segments with R1-rational contribution
! ******************************************************************************
  use KIND_TYPES, only: REALKIND,QREALKIND
  use ol_data_types_/**/REALKIND, only: redset5,hol
  use ol_parameters_decl_/**/REALKIND, only: rzero
  use ol_kinematics_/**/REALKIND, only: get_mass2
#ifdef PRECISION_dp
  use ol_data_types_/**/QREALKIND, only: redset5_qp=>redset5
  use ofred_basis_construction_/**/REALKIND, only: upgrade_redset5
  use ofred_reduction_/**/QREALKIND, only: otf_5pt_red_qp=>otf_5pt_red
  use ol_loop_handling_/**/REALKIND, only: req_qp_cmp,hol_dealloc_hybrid
  use ol_kinematics_/**/QREALKIND, only: get_mass2_qp=>get_mass2
#endif
  use ol_loop_handling_/**/REALKIND, only: hybrid_zero_mode
  use ol_parameters_decl_/**/DREALKIND, only: hp_switch,hybrid_qp_mode, &
                                              hp_alloc_mode
  implicit none
  type(redset5), intent(in)    :: RedSet_5
  integer,       intent(in)    :: msq(0:4)
  type(hol),     intent(inout) :: Gin_A,Gout_A,Gout_A0,Gout_A1,Gout_A2, &
                                  Gout_A3,Gout_A4,Gout_R1
  integer,       intent(in)    :: nhel
  integer :: h, mode_in, phys_hel
#ifdef PRECISION_dp
  type(redset5_qp) :: RedSet_5_qp
#endif

  mode_in = Gin_A%mode
  phys_hel = size(Gin_A%hf)

  if (.not. valid_5pt(Gin_A,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3,Gout_A4,Gout_R1)) return
  call err_estim_5pt(RedSet_5,Gin_A,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3,Gout_A4,Gout_R1)

#ifdef PRECISION_dp
  if (Gin_A%mode.ne.hybrid_qp_mode) then
#endif
    do h = 1, phys_hel
        call otf_5pt_red(Gin_A%j(:,:,:,h),   &
                         RedSet_5,           &
                         get_mass2(msq),      &
                         Gout_A%j(:,:,:,h),  &
                         Gout_A0%j(:,:,:,h), &
                         Gout_A1%j(:,:,:,h), &
                         Gout_A2%j(:,:,:,h), &
                         Gout_A3%j(:,:,:,h), &
                         Gout_A4%j(:,:,:,h), &
                         Gout_R1%j(:,:,:,h))
    end do
#ifdef PRECISION_dp
  else
    Gout_A%j = 0
    Gout_A0%j = 0
    Gout_A1%j = 0
    Gout_A2%j = 0
    Gout_A3%j = 0
    Gout_A4%j = 0
    Gout_R1%j = 0
  end if
#endif


  if (RedSet_5%perm(4) .eq. 1 ) then
    Gout_A1%mode = hybrid_zero_mode
    Gout_A1%error = rzero
    Gout_A1%ndrs = 0
    Gout_A1%nred = 0
#ifdef PRECISION_dp
    Gout_A1%ndrs_qp = 0
    Gout_A1%nred_qp = 0
#endif
  else if (RedSet_5%perm(4) .eq. 2) then
    Gout_A2%mode = hybrid_zero_mode
    Gout_A2%error = rzero
    Gout_A2%ndrs = 0
    Gout_A2%nred = 0
#ifdef PRECISION_dp
    Gout_A2%ndrs_qp = 0
    Gout_A2%nred_qp = 0
#endif
  else if (RedSet_5%perm(4) .eq. 3) then
    Gout_A3%mode = hybrid_zero_mode
    Gout_A3%error = rzero
    Gout_A3%ndrs = 0
    Gout_A3%nred = 0
#ifdef PRECISION_dp
    Gout_A3%ndrs_qp = 0
    Gout_A3%nred_qp = 0
#endif
  else if (RedSet_5%perm(4) .eq. 4) then
    Gout_A4%mode = hybrid_zero_mode
    Gout_A4%error = rzero
    Gout_A4%ndrs = 0
    Gout_A4%nred = 0
#ifdef PRECISION_dp
    Gout_A4%ndrs_qp = 0
    Gout_A4%nred_qp = 0
#endif
  end if

#ifdef PRECISION_dp
  if (req_qp_cmp(Gout_A)) then
    call upgrade_redset5(RedSet_5, RedSet_5_qp)
    do h = 1, phys_hel
      call otf_5pt_red_qp(Gin_A%j_qp(:,:,:,h),   &
                          RedSet_5_qp,           &
                          get_mass2_qp(msq),      &
                          Gout_A%j_qp(:,:,:,h),  &
                          Gout_A0%j_qp(:,:,:,h), &
                          Gout_A1%j_qp(:,:,:,h), &
                          Gout_A2%j_qp(:,:,:,h), &
                          Gout_A3%j_qp(:,:,:,h), &
                          Gout_A4%j_qp(:,:,:,h), &
                          Gout_R1%j_qp(:,:,:,h))
    end do

    call downgrade_5pt(RedSet_5,mode_in,Gout_A1,Gout_A2,Gout_A3,Gout_A4)
    call hol_dealloc_hybrid(Gin_A)

  else if (hp_switch .eq. 1) then
    if (hp_alloc_mode .eq. 0) then
      Gout_A%j_qp = 0
      Gout_A0%j_qp = 0
      Gout_A1%j_qp = 0
      Gout_A2%j_qp = 0
      Gout_A3%j_qp = 0
      Gout_A4%j_qp = 0
      Gout_R1%j_qp = 0
    end if
  end if
#endif

end subroutine Hotf_5pt_red_R1


! =============================================================================
!                      Closed-loop reduction INTERFACE
! -----------------------------------------------------------------------------
! Reduction steps for closed-loop derived data type
! =============================================================================


! ******************************************************************************
subroutine Hotf_4pt_red_last(Gin_A,RedSet_4,msq,Gout_A,Gout_A0,Gout_A1, &
                             Gout_A2,Gout_A3)
! ******************************************************************************
  use KIND_TYPES, only: REALKIND,QREALKIND
  use ol_data_types_/**/REALKIND, only: redset4, hcl
  use ol_kinematics_/**/REALKIND, only: get_mass2
#ifdef PRECISION_dp
  use ol_data_types_/**/QREALKIND, only: redset4_qp=>redset4
  use ofred_basis_construction_/**/REALKIND, only: upgrade_redset4
  use ofred_reduction_/**/QREALKIND, only: otf_4pt_reduction_last_qp=>otf_4pt_reduction_last
  use ol_loop_handling_/**/REALKIND, only: req_qp_cmp,hcl_dealloc_hybrid
  use ol_kinematics_/**/QREALKIND, only: get_mass2_qp=>get_mass2
#endif
  use ol_parameters_decl_/**/DREALKIND, only: hp_switch,hybrid_qp_mode, &
                                              hp_alloc_mode
  implicit none
  type(redset4),   intent(in)    :: RedSet_4
  integer,         intent(in)    :: msq(0:3)
  type(hcl),       intent(inout) :: Gin_A,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3
  integer :: mode_in
#ifdef PRECISION_dp
  type(redset4_qp) :: RedSet_4_qp
#endif

  mode_in = Gin_A%mode

  if (.not. valid_4pt(Gin_A, Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3)) return
  call err_estim_4pt(RedSet_4,Gin_A,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3)

#ifdef PRECISION_dp
  if (Gin_A%mode .ne. hybrid_qp_mode) then
#endif
    call otf_4pt_reduction_last(Gin_A%cmp,     &
                                RedSet_4,      &
                                get_mass2(msq),&
                                Gout_A%cmp,    &
                                Gout_A0%cmp,   &
                                Gout_A1%cmp,   &
                                Gout_A2%cmp,   &
                                Gout_A3%cmp)
#ifdef PRECISION_dp
  else
    Gout_A%cmp = 0
    Gout_A0%cmp = 0
    Gout_A1%cmp = 0
    Gout_A2%cmp = 0
    Gout_A3%cmp = 0
  end if
#endif


#ifdef PRECISION_dp
  if (req_qp_cmp(Gout_A)) then
    call upgrade_redset4(RedSet_4,RedSet_4_qp)
    call otf_4pt_reduction_last_qp(Gin_A%cmp_qp,      &
                                   RedSet_4_qp,       &
                                   get_mass2_qp(msq), &
                                   Gout_A%cmp_qp(:),  &
                                   Gout_A0%cmp_qp(:), &
                                   Gout_A1%cmp_qp(:), &
                                   Gout_A2%cmp_qp(:), &
                                   Gout_A3%cmp_qp(:))
    call downgrade_4pt(RedSet_4,mode_in,Gout_A1,Gout_A2,Gout_A3)
    call hcl_dealloc_hybrid(Gin_A)
  else if (hp_switch .eq. 1) then
    if (hp_alloc_mode .eq. 0) then
      Gout_A%cmp_qp = 0
      Gout_A0%cmp_qp = 0
      Gout_A1%cmp_qp = 0
      Gout_A2%cmp_qp = 0
      Gout_A3%cmp_qp = 0
    end if
  end if
#endif

end subroutine Hotf_4pt_red_last

! ******************************************************************************
subroutine Hotf_4pt_red_last_R1(Gin_A,RedSet_4,msq,Gout_A,Gout_A0,Gout_A1, &
                                Gout_A2,Gout_A3,Gout_R1)
! ******************************************************************************
  use KIND_TYPES, only: REALKIND, QREALKIND
  use ol_data_types_/**/REALKIND, only: redset4, hcl
  use ol_kinematics_/**/REALKIND, only: get_mass2
#ifdef PRECISION_dp
  use ol_data_types_/**/QREALKIND, only: redset4_qp=>redset4
  use ofred_basis_construction_/**/REALKIND, only: upgrade_redset4
  use ofred_reduction_/**/QREALKIND, only:  &
    otf_4pt_reduction_last_qp=>otf_4pt_reduction_last
  use ol_loop_handling_/**/REALKIND, only: req_qp_cmp,hcl_dealloc_hybrid
  use ol_kinematics_/**/QREALKIND, only: get_mass2_qp=>get_mass2
#endif
  use ol_parameters_decl_/**/DREALKIND, only: hp_switch,hybrid_qp_mode, &
                                              hp_alloc_mode
  implicit none
  type(redset4), intent(in)    :: RedSet_4
  integer,       intent(in)    :: msq(0:3)
  type(hcl),     intent(inout) :: Gin_A,Gout_A,Gout_A0,Gout_A1,Gout_A2, &
                                  Gout_A3,Gout_R1
  integer :: mode_in
#ifdef PRECISION_dp
  type(redset4_qp) :: RedSet_4_qp
#endif

  mode_in = Gin_A%mode

  if (.not. valid_4pt(Gin_A,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3,Gout_R1)) return
  call err_estim_4pt(RedSet_4,Gin_A,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3,Gout_R1)

#ifdef PRECISION_dp
  if (Gin_A%mode .ne. hybrid_qp_mode) then
#endif
    call otf_4pt_reduction_last(Gin_A%cmp,     &
                                RedSet_4,      &
                                get_mass2(msq), &
                                Gout_A%cmp,    &
                                Gout_A0%cmp,   &
                                Gout_A1%cmp,   &
                                Gout_A2%cmp,   &
                                Gout_A3%cmp,   &
                                Gout_R1%cmp(1))
#ifdef PRECISION_dp
  else
    Gout_A%cmp = 0
    Gout_A0%cmp = 0
    Gout_A1%cmp = 0
    Gout_A2%cmp = 0
    Gout_A3%cmp = 0
    Gout_R1%cmp = 0
  end if
#endif


#ifdef PRECISION_dp
  if (req_qp_cmp(Gout_A)) then
    call upgrade_redset4(RedSet_4,RedSet_4_qp)
    call otf_4pt_reduction_last_qp(Gin_A%cmp_qp,      &
                                   RedSet_4_qp,       &
                                   get_mass2_qp(msq), &
                                   Gout_A%cmp_qp,     &
                                   Gout_A0%cmp_qp,    &
                                   Gout_A1%cmp_qp,    &
                                   Gout_A2%cmp_qp,    &
                                   Gout_A3%cmp_qp,    &
                                   Gout_R1%cmp_qp(1))
    call downgrade_4pt(RedSet_4,mode_in,Gout_A1,Gout_A2,Gout_A3)
    call hcl_dealloc_hybrid(Gin_A)
  else if (hp_switch .eq. 1) then
    if (hp_alloc_mode .eq. 0) then
      Gout_A%cmp_qp = 0
      Gout_A0%cmp_qp = 0
      Gout_A1%cmp_qp = 0
      Gout_A2%cmp_qp = 0
      Gout_A3%cmp_qp = 0
      Gout_R1%cmp_qp = 0
    end if
  end if
#endif

end subroutine Hotf_4pt_red_last_R1


! ******************************************************************************
subroutine Hotf_5pt_red_last(Gin_A,RedSet_5,msq,Gout_A,Gout_A0,Gout_A1, &
                             Gout_A2,Gout_A3,Gout_A4)
! ******************************************************************************
  use KIND_TYPES, only: REALKIND,QREALKIND
  use ol_data_types_/**/REALKIND, only: redset5, hcl
  use ol_parameters_decl_/**/REALKIND, only: rzero
  use ol_kinematics_/**/REALKIND, only: get_mass2
#ifdef PRECISION_dp
  use ol_data_types_/**/QREALKIND, only: redset5_qp=>redset5
  use ofred_basis_construction_/**/REALKIND, only: upgrade_redset5
  use ofred_reduction_/**/QREALKIND, only: &
    otf_5pt_reduction_last_qp=>otf_5pt_reduction_last
  use ol_loop_handling_/**/REALKIND, only: req_qp_cmp,hcl_dealloc_hybrid
  use ol_kinematics_/**/QREALKIND, only: get_mass2_qp=>get_mass2
#endif
  use ol_loop_handling_/**/REALKIND, only: hybrid_zero_mode
  use ol_parameters_decl_/**/DREALKIND, only: hp_switch,hybrid_qp_mode, &
                                              hp_alloc_mode
  implicit none
  type(redset5), intent(in)    :: RedSet_5
  integer,       intent(in)    :: msq(0:4)
  type(hcl),     intent(inout) :: Gin_A,Gout_A,Gout_A0,Gout_A1,Gout_A2, &
                                  Gout_A3,Gout_A4
  integer :: mode_in
#ifdef PRECISION_dp
  type(redset5_qp) :: RedSet_5_qp
#endif

  mode_in = Gin_A%mode

  if (.not. valid_5pt(Gin_A, Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3,Gout_A4)) return
  call err_estim_5pt(RedSet_5,Gin_A,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3,Gout_A4)

#ifdef PRECISION_dp
  if (Gin_A%mode.ne.hybrid_qp_mode) then
#endif
    call otf_5pt_reduction_last(Gin_A%cmp,     &
                                RedSet_5,      &
                                get_mass2(msq), &
                                Gout_A%cmp,    &
                                Gout_A0%cmp,   &
                                Gout_A1%cmp,   &
                                Gout_A2%cmp,   &
                                Gout_A3%cmp,   &
                                Gout_A4%cmp)
#ifdef PRECISION_dp
  else
    Gout_A%cmp = 0
    Gout_A0%cmp = 0
    Gout_A1%cmp = 0
    Gout_A2%cmp = 0
    Gout_A3%cmp = 0
    Gout_A4%cmp = 0
  end if
#endif


  if (RedSet_5%perm(4) .eq. 1) then
    Gout_A1%mode = hybrid_zero_mode
    Gout_A1%error = rzero
    Gout_A1%ndrs = 0
    Gout_A1%nred = 0
#ifdef PRECISION_dp
    Gout_A1%ndrs_qp = 0
    Gout_A1%nred_qp = 0
#endif
  else if (RedSet_5%perm(4) .eq. 2) then
    Gout_A2%mode = hybrid_zero_mode
    Gout_A2%error = rzero
    Gout_A2%ndrs = 0
    Gout_A2%nred = 0
#ifdef PRECISION_dp
    Gout_A2%ndrs_qp = 0
    Gout_A2%nred_qp = 0
#endif
  else if (RedSet_5%perm(4) .eq. 3) then
    Gout_A3%mode = hybrid_zero_mode
    Gout_A3%error = rzero
    Gout_A3%ndrs = 0
    Gout_A3%nred = 0
#ifdef PRECISION_dp
    Gout_A3%ndrs_qp = 0
    Gout_A3%nred_qp = 0
#endif
  else if (RedSet_5%perm(4) .eq. 4) then
    Gout_A4%mode = hybrid_zero_mode
    Gout_A4%error = rzero
    Gout_A4%ndrs = 0
    Gout_A4%nred = 0
#ifdef PRECISION_dp
    Gout_A4%ndrs_qp = 0
    Gout_A4%nred_qp = 0
#endif
  end if

#ifdef PRECISION_dp
  if (req_qp_cmp(Gout_A)) then
    call upgrade_redset5(RedSet_5,RedSet_5_qp)
    call otf_5pt_reduction_last_qp(       &
                        Gin_A%cmp_qp,     &
                        RedSet_5_qp,      &
                        get_mass2_qp(msq),&
                        Gout_A%cmp_qp,    &
                        Gout_A0%cmp_qp,   &
                        Gout_A1%cmp_qp,   &
                        Gout_A2%cmp_qp,   &
                        Gout_A3%cmp_qp,   &
                        Gout_A4%cmp_qp)
    call downgrade_5pt(RedSet_5,mode_in,Gout_A1,Gout_A2,Gout_A3,Gout_A4)
    call hcl_dealloc_hybrid(Gin_A)
  else if (hp_switch .eq. 1) then
    if (hp_alloc_mode .eq. 0) then
      Gout_A%cmp_qp = 0
      Gout_A0%cmp_qp = 0
      Gout_A1%cmp_qp = 0
      Gout_A2%cmp_qp = 0
      Gout_A3%cmp_qp = 0
      Gout_A4%cmp_qp = 0
    end if
  end if
#endif

end subroutine Hotf_5pt_red_last

! ******************************************************************************
subroutine Hotf_5pt_red_last_R1(Gin_A,RedSet_5,msq,Gout_A,Gout_A0,Gout_A1, &
                                Gout_A2,Gout_A3,Gout_A4,Gout_R1)
! ******************************************************************************
  use KIND_TYPES, only: REALKIND,QREALKIND
  use ol_data_types_/**/REALKIND, only: redset5,hcl
  use ol_parameters_decl_/**/REALKIND, only: rzero
  use ol_kinematics_/**/REALKIND, only: get_mass2
#ifdef PRECISION_dp
  use ol_data_types_/**/QREALKIND, only: redset5_qp=>redset5
  use ofred_basis_construction_/**/REALKIND, only: upgrade_redset5
  use ofred_reduction_/**/QREALKIND, only: &
    otf_5pt_reduction_last_qp=>otf_5pt_reduction_last
  use ol_loop_handling_/**/REALKIND, only: req_qp_cmp,hcl_dealloc_hybrid
  use ol_kinematics_/**/QREALKIND, only: get_mass2_qp=>get_mass2
#endif
  use ol_loop_handling_/**/REALKIND, only: hybrid_zero_mode
  use ol_parameters_decl_/**/DREALKIND, only: hp_switch,hybrid_qp_mode, &
                                              hp_alloc_mode
  implicit none
  type(redset5), intent(in)    :: RedSet_5
  integer,       intent(in)    :: msq(0:4)
  type(hcl),     intent(inout) :: Gin_A,Gout_A,Gout_A0,Gout_A1,Gout_A2, &
                                  Gout_A3,Gout_A4,Gout_R1
  integer :: mode_in
#ifdef PRECISION_dp
  type(redset5_qp) :: RedSet_5_qp
#endif

  mode_in = Gin_A%mode

  if (.not. valid_5pt(Gin_A, Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3,Gout_A4,Gout_R1)) return
  call err_estim_5pt(RedSet_5,Gin_A,Gout_A,Gout_A0,Gout_A1,Gout_A2,Gout_A3,Gout_A4,Gout_R1)

#ifdef PRECISION_dp
  if (Gin_A%mode .ne. hybrid_qp_mode) then
#endif
    call otf_5pt_reduction_last(Gin_A%cmp,     &
                                RedSet_5,      &
                                get_mass2(msq),&
                                Gout_A%cmp,    &
                                Gout_A0%cmp,   &
                                Gout_A1%cmp,   &
                                Gout_A2%cmp,   &
                                Gout_A3%cmp,   &
                                Gout_A4%cmp,   &
                                Gout_R1%cmp(1))
#ifdef PRECISION_dp
  else
    Gout_A%cmp = 0
    Gout_A0%cmp = 0
    Gout_A1%cmp = 0
    Gout_A2%cmp = 0
    Gout_A3%cmp = 0
    Gout_A4%cmp = 0
    Gout_R1%cmp = 0
  end if
#endif


  if (RedSet_5%perm(4) .eq. 1) then
    Gout_A1%mode = hybrid_zero_mode
    Gout_A1%error = rzero
    Gout_A1%ndrs = 0
    Gout_A1%nred = 0
#ifdef PRECISION_dp
    Gout_A1%ndrs_qp = 0
    Gout_A1%nred_qp = 0
#endif
  else if (RedSet_5%perm(4) .eq. 2) then
    Gout_A2%mode = hybrid_zero_mode
    Gout_A2%error = rzero
    Gout_A2%ndrs = 0
    Gout_A2%nred = 0
#ifdef PRECISION_dp
    Gout_A2%ndrs_qp = 0
    Gout_A2%nred_qp = 0
#endif
  else if (RedSet_5%perm(4).eq. 3) then
    Gout_A3%mode = hybrid_zero_mode
    Gout_A3%error = rzero
    Gout_A3%ndrs = 0
    Gout_A3%nred = 0
#ifdef PRECISION_dp
    Gout_A3%ndrs_qp = 0
    Gout_A3%nred_qp = 0
#endif
  else if (RedSet_5%perm(4) .eq. 4) then
    Gout_A4%mode = hybrid_zero_mode
    Gout_A4%error = rzero
    Gout_A4%ndrs = 0
    Gout_A4%nred = 0
#ifdef PRECISION_dp
    Gout_A4%ndrs_qp = 0
    Gout_A4%nred_qp = 0
#endif
  end if

#ifdef PRECISION_dp
  if (req_qp_cmp(Gout_A)) then
    call upgrade_redset5(RedSet_5,RedSet_5_qp)
    call otf_5pt_reduction_last_qp(Gin_A%cmp_qp,     &
                                   RedSet_5_qp,      &
                                   get_mass2_qp(msq),&
                                   Gout_A%cmp_qp,    &
                                   Gout_A0%cmp_qp,   &
                                   Gout_A1%cmp_qp,   &
                                   Gout_A2%cmp_qp,   &
                                   Gout_A3%cmp_qp,   &
                                   Gout_A4%cmp_qp,   &
                                   Gout_R1%cmp_qp(1))
    call downgrade_5pt(RedSet_5,mode_in,Gout_A1,Gout_A2,Gout_A3,Gout_A4)
    call hcl_dealloc_hybrid(Gin_A)
  else if (hp_switch .eq. 1) then
    if (hp_alloc_mode .eq. 0) then
      Gout_A%cmp_qp = 0
      Gout_A0%cmp_qp = 0
      Gout_A1%cmp_qp = 0
      Gout_A2%cmp_qp = 0
      Gout_A3%cmp_qp = 0
      Gout_A4%cmp_qp = 0
      Gout_R1%cmp_qp = 0
    end if
  end if
#endif

end subroutine Hotf_5pt_red_last_R1

end module ofred_reduction_/**/REALKIND
