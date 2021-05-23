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
      STOP                           ! Total number of out-puts

  CASE(60)
      ! The first out-put, an integer matrix.
      ! It will be the product of two integer matrixes: input 1 and input 2.
      CALCULATED_NUMBER=1
      CALCULATED_TYPE(1)=I_MATRIX_TYPE                   ! The integer matrix type
      CALCULATED_M(1)%SIZES(1)=GENERATED_M(1)%SIZES(1)   ! The sizes of the out-put matrix
      CALCULATED_M(1)%SIZES(2)=GENERATED_M(2)%SIZES(2)   ! The sizes of the out-put matrix
      ! The bracket index of the out-put matrix
      CALCULATED_M(1)%BRACE_INDEX=GENERATED_M(1)%BRACE_INDEX
      IF(GENERATED_M(1)%SIZES(2).NE.GENERATED_M(2)%SIZES(1)) THEN
         PRINT*, 'MATRIX SIZE PROBLEM FOR INPUT 1 AND 2: ',GENERATED_M(1)%SIZES,GENERATED_M(2)%SIZES
         STOP
      END IF
      ! Allocate full memory for the out-put matrix to its integer pointer component:
      ! (will be deallocated by other routines (automatically) later)
      ALLOCATE(CALCULATED_M(1)%INTEGER_M(CALCULATED_M(1)%SIZES(1),CALCULATED_M(1)%SIZES(2)))
      ! Calculate each element of the out-put matrix
      CALCULATED_M(1)%INTEGER_M=0
      DO I=1, CALCULATED_M(1)%SIZES(1)
         DO J=1, CALCULATED_M(1)%SIZES(2)
            DO K=1, GENERATED_M(1)%SIZES(2)
               CALCULATED_M(1)%INTEGER_M(I,J)=CALCULATED_M(1)%INTEGER_M(I,J)+ &
                 GENERATED_M(1)%INTEGER_M(I,K)*GENERATED_M(2)%INTEGER_M(K,J)
            END DO
         END DO
      END DO

      ! The second out-put, a string matrix.
      ! It will be the product of two string matrixes: input 3 and input 4.
      CALCULATED_NUMBER=2
      CALCULATED_TYPE(2)=S_MATRIX_TYPE                 ! The string matrix type
      CALCULATED_M(2)%SIZES(1)=GENERATED_M(3)%SIZES(1) ! The sizes of the out-put matrix
      CALCULATED_M(2)%SIZES(2)=GENERATED_M(4)%SIZES(2) ! The sizes of the out-put matrix
      ! The bracket index of the out-put matrix
      CALCULATED_M(2)%BRACE_INDEX=MATRIX_BRACE_A
      ! This means that bracket will be generated as:
      !                 \left( \begin{array} ... \end{array} \right)

      IF(GENERATED_M(3)%SIZES(2).NE.GENERATED_M(4)%SIZES(1)) THEN
         PRINT*, 'MATRIX SIZE PROBLEM FOR INPUT 3 AND 4: ',GENERATED_M(3)%SIZES,GENERATED_M(4)%SIZES
         STOP
      END IF

      ! Allocate full memory for the out-put matrix to its string pointer component:
      ! (will be deallocated by other routines (automatically) later)
      ALLOCATE(CALCULATED_M(2)%STRING_M(CALCULATED_M(2)%SIZES(1),CALCULATED_M(2)%SIZES(2)))

      ! Initialize each element of the out-put matrix to empty
      CALCULATED_M(2)%STRING_M=' '
      ! Do the string matrix multiplication for each element of the out-put matrix
      DO I=1, CALCULATED_M(2)%SIZES(1)
         DO J=1, CALCULATED_M(2)%SIZES(2)
            L=0
            M=0
            DO K=1, GENERATED_M(3)%SIZES(2)
                  M=LEN_TRIM(GENERATED_M(3)%STRING_M(I,K))+LEN_TRIM(GENERATED_M(4)%STRING_M(K,J))+10
                  ! Check whether space is enough, since each element of the out-put matrix
                  ! is a string of MAX_NUMBER_OF_CHARACTERS_SSS characters.
                  IF(L+M.GT.MAX_NUMBER_OF_CHARACTERS_SSS) THEN
                     PRINT*, 'TOO LONG STRING FOR OUTPUT 2: ', L, MAX_NUMBER_OF_CHARACTERS_SSS
                     STOP
                  END IF
                  CALCULATED_M(2)%STRING_M(I,J)(L+1:L+M)= &
                    TRIM(GENERATED_M(3)%STRING_M(I,K))//' \times '//TRIM(GENERATED_M(4)%STRING_M(K,J))//'  '
                  L=L+M
               IF(K.LT.GENERATED_M(3)%SIZES(2)) THEN
                  M=3
                  IF(L+M.GT.MAX_NUMBER_OF_CHARACTERS_SSS) THEN
                     PRINT*, 'TOO LONG STRING FOR OUTPUT 2: ', L, MAX_NUMBER_OF_CHARACTERS_SSS
                     STOP
                  END IF
                  CALCULATED_M(2)%STRING_M(I,J)(L+1:L+M)=' + '
                  L=L+M
               END IF
            END DO
         END DO
      END DO

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
