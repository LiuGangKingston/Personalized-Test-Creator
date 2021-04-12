!
!
!   This is the only routine for users to update in the
!           Personalized Test Creator
!           Version 1.0
!           free for registered non-commercial use.
!   Any problems/suggestions/comments, please send to gang.liu@queensu.ca
!   Please be advised that neither the author nor HPCVL accept any
!   responsibility for any consequences arising out of the usage of this
!   software, especially for damage.
!   Copyright (c) 2007-2008 by High Performance Computing Virtual Laboratory
!
!
SUBROUTINE SOLVE_THE_QUESTION(PRINT_OUT_NUMBER, AN_ACCESSORY_INTEGER)
  USE BASE_DATA
  USE RAND_INTERFACE
  USE CONSTANT_INTERFACE
  IMPLICIT NONE
  INTEGER  ::  PRINT_OUT_NUMBER, AN_ACCESSORY_INTEGER
  INTEGER  ::  I, J, K, L, M, N

  QUESTION_IDENTIFIER=QUESTION_POSITION(0,QUESTION_NUMBER)
  SELECT CASE (QUESTION_IDENTIFIER)

  CASE(:0)
      PRINT*,' Sorry for illegal Question ID: ', QUESTION_IDENTIFIER, QUESTION_NUMBER
      STOP

  CASE(101)
      CALCULATED_I(1)=GENERATED_I(1) + GENERATED_I(2)
      CALCULATED_TYPE(1)=I_TYPE
      CALCULATED_NUMBER=1

  CASE(102)
      CALCULATED_I(1)=GENERATED_I(1) + GENERATED_I(2)
      CALCULATED_TYPE(1)=I_TYPE
      CALCULATED_NUMBER=1

  CASE(103)
      CALCULATED_I(1)=GENERATED_I(1) - GENERATED_I(2)
      CALCULATED_TYPE(1)=I_TYPE
      CALCULATED_NUMBER=1


  CASE(104)
      CALCULATED_I(1)=GENERATED_I(1) - GENERATED_I(2)
      CALCULATED_TYPE(1)=I_TYPE
      CALCULATED_NUMBER=1

  CASE(105)
      CALCULATED_I(1)=GENERATED_I(1) * GENERATED_I(2)
      CALCULATED_TYPE(1)=I_TYPE
      CALCULATED_NUMBER=1


  CASE(106)
      CALCULATED_I(1)=GENERATED_I(1) * GENERATED_I(2)
      CALCULATED_TYPE(1)=I_TYPE
      CALCULATED_NUMBER=1


  CASE(107)
      CALCULATED_R(1)=GENERATED_I(1) * 1.0d0 / GENERATED_I(2)
      CALCULATED_TYPE(1)=R_TYPE
      CALCULATED_NUMBER=1


  CASE(108)
      CALCULATED_R(1)=GENERATED_I(1) * 1.0d0 / GENERATED_I(2)
      CALCULATED_TYPE(1)=R_TYPE
      CALCULATED_NUMBER=1



  CASE(201)


  CASE(202)


  CASE DEFAULT
      ! CALCULATED_NUMBER must be set as the maximum calculated number.
      ! The date type for the i-th out-put/calculate can be one of
      ! INTEGER, PARAMETER :: I_TYPE=1
      ! INTEGER, PARAMETER :: R_TYPE=2
      ! INTEGER, PARAMETER :: C_TYPE=3
      ! INTEGER, PARAMETER :: S_TYPE=4
      ! INTEGER, PARAMETER :: L_TYPE=5
      ! INTEGER, PARAMETER :: V_TYPE=6
      ! INTEGER, PARAMETER :: I_MATRIX_TYPE=101
      ! INTEGER, PARAMETER :: R_MATRIX_TYPE=102
      ! INTEGER, PARAMETER :: C_MATRIX_TYPE=103
      ! INTEGER, PARAMETER :: S_MATRIX_TYPE=104
      ! and must be assigned to CALCULATED_TYPE(I).
      ! The value must be placed to the corresponding CALCULATED_* arrays.

      ! The following functions can be used to get some built-in constant values:
      ! print*, get_constant_value_of("Acceleration due to earth's gravity")
      ! print*, get_constant_value_of("EARTH-ACCELERATION")
      ! print*, get_constant_value_of("Avogadro's number")
      ! print*, get_constant_value_of("BOLTZMANN-CONSTANT")
      ! print*, get_constant_value_of("Coulomb's constant")
      ! print*, get_constant_value_of("ELECTRON-CHARGE")
      ! print*, get_constant_value_of("Pi")
      ! print*, get_constant_value_of("Speed of light in vacuum")
      ! print*, get_constant_value_of("UNIVERSAL-GAS-CONSTANT")

      PRINT*,' Sorry for not defined Question ID: ', QUESTION_POSITION(:,QUESTION_NUMBER), QUESTION_NUMBER
      STOP

  END SELECT

  RETURN
END SUBROUTINE SOLVE_THE_QUESTION
!
!
!   This is the only routine for users to update in the
!           Personalized Test Creator
!           Version 1.0
!           free for registered non-commercial use.
!   Any problems/suggestions/comments, please send to gang.liu@queensu.ca
!   Please be advised that neither the author nor HPCVL accept any
!   responsibility for any consequences arising out of the usage of this
!   software, especially for damage.
!   Copyright (c) 2007-2008 by High Performance Computing Virtual Laboratory
!
!
