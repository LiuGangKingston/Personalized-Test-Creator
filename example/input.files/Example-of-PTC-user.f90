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

  CASE(1)
      IF(GENERATED_R(2).NE.0.0D0) &                      ! If zero, no need to do next.
      CALCULATED_V(:,1)=GENERATED_V(:,1)/GENERATED_R(2)  ! Acceleration=Force/Mass
      CALCULATED_TYPE(1)=V_TYPE                          ! Vector data type of the out-puts
      CALCULATED_NUMBER=1                                ! Total number of out-puts

  CASE(2)
      DO I=1, 3
         CALCULATED_TYPE(I)=R_TYPE                       ! Real data type of the out-put
         IF(GENERATED_R(4).NE.0.0D0) &                   ! If zero, no need to do next.
         CALCULATED_R(I)=GENERATED_R(I)/GENERATED_R(4)   ! Acceleration=Force/Mass
                                                         ! for the three components separately
         CALCULATED_R(I+3)=CALCULATED_R(I)*(3600.0D0)**2/1000.0D0
                                                         ! Unit converted for acceleration
         CALCULATED_TYPE(I+3)=R_TYPE                     ! Real data type of the out-put
      END DO
      CALCULATED_NUMBER=6                                ! Total number of out-puts

  CASE(3)
      DO I=1, 20
         CALCULATED_NUMBER=I                             ! Total number of out-puts
         CALCULATED_TYPE(CALCULATED_NUMBER)=I_TYPE       ! Integer data type of the out-put
         CALCULATED_I(CALCULATED_NUMBER)=20+I               ! A false integer
      END DO
      CALCULATED_I(1)=10                                 ! Number of provinces Canada has.
      CALCULATED_I(2)=3                                  ! Number of territories Canada has.

  CASE(4)
      IF(GENERATED_I(2).NE.0.0D0) &                      ! If zero, no need to do next.
      CALCULATED_I(1)=GENERATED_I(1)/GENERATED_I(2)      ! Just a simple integer division.
      CALCULATED_TYPE(1)=I_TYPE                          ! Integer data type of the out-put
      CALCULATED_NUMBER=1                                ! Total number of out-puts

  CASE(5)
      ! In this question, we will have three string out-puts, numbered as 1:3 in the following.
      OUTPUT_SSS=' '               ! Not necessary, initialize all out-put strings as a habit
      OUTPUT_SSS(3)='$T$'          ! Code all possible strings in sequence here
      OUTPUT_SSS(4)='$F$'          ! Code all possible strings in sequence here
                                   !   then the element numbers 3 and 4 are used as index later
      CALCULATED_POSITION(1,1:3)=3 ! For each string out-put, code the beginning string index
      CALCULATED_POSITION(2,1:3)=4 ! For each string out-put, code the ending string index
                                   ! Specifically, CALCULATED_POSITION(1,n) and
                                   !   CALCULATED_POSITION(2,n) are the index range for the
                                   !   out-put number n.
      CALCULATED_TYPE(1:3)=S_TYPE  ! String data type
      CALCULATED_I(1:3)=4          ! For each string out-put,
                                   !   initialize the string index to be used,
                                   !   then correct it based on generated data from inputs
                                   !   in the following.
      IF(MOD(GENERATED_I(1),2).EQ.0) THEN
          IF(GENERATED_I(2).EQ.GENERATED_POSITION(1,2)) CALCULATED_I(1)=3
      ELSE
          IF(GENERATED_I(2).EQ.GENERATED_POSITION(2,2)) CALCULATED_I(1)=3
      END IF
      IF(GENERATED_I(3)-GENERATED_POSITION(1,3).LE.1) THEN
          IF(GENERATED_I(4).EQ.GENERATED_POSITION(1,4)) CALCULATED_I(2)=3
      ELSE
          IF(GENERATED_I(4).EQ.GENERATED_POSITION(2,4)) CALCULATED_I(2)=3
      END IF
      IF((GENERATED_I(5)-GENERATED_POSITION(1,5)).EQ.(GENERATED_I(6)-GENERATED_POSITION(1,6))) THEN
          CALCULATED_I(3)=3
      END IF

      CALCULATED_NUMBER=3

  CASE(21)
      ! Not necessary, initialize all out-put as a habit
      CALCULATED_TYPE=0
      CALCULATED_I=0
      CALCULATED_R=0.0D0
      CALCULATED_C=CMPLX(0.0D0,0.0D0)

      DO I=1, 3
         CALCULATED_TYPE(I)=R_TYPE                       ! Real data type of the out-put
         IF(GENERATED_R(4).NE.0.0D0) &                   ! If zero, no need to do next.
         CALCULATED_R(I)=GENERATED_R(I)/GENERATED_R(4)   ! Acceleration=Force/Mass
                                                         ! for the three components separately
         CALCULATED_R(I+3)=CALCULATED_R(I)*(3600.0D0)**2/1000.0D0
                                                         ! Unit converted for acceleration
         CALCULATED_TYPE(I+3)=R_TYPE                     ! Real data type of the out-put
      END DO
      CALCULATED_NUMBER=6                                ! Total number of out-puts

  CASE(22)
      ! Not necessary, initialize all out-put as a habit
      CALCULATED_TYPE=0
      CALCULATED_I=0
      CALCULATED_R=0.0D0
      CALCULATED_C=CMPLX(0.0D0,0.0D0)

      DO I=1, 3
         CALCULATED_TYPE(I)=R_TYPE                       ! Real data type of the out-put
         IF(GENERATED_R(4).NE.0.0D0) &                   ! If zero, no need to do next.
         CALCULATED_R(I)=GENERATED_R(I)/GENERATED_R(4)   ! Acceleration=Force/Mass
                                                         ! for the three components separately
         CALCULATED_R(I+3)=CALCULATED_R(I)*(3600.0D0)**2/1000.0D0
                                                         ! Unit converted for acceleration
         CALCULATED_TYPE(I+3)=R_TYPE                     ! Real data type of the out-put
      END DO
      CALCULATED_NUMBER=6                                ! Total number of out-puts


  CASE(23)
      ! Not necessary, initialize all out-put as a habit
      CALCULATED_TYPE=0
      CALCULATED_I=0
      CALCULATED_R=0.0D0
      CALCULATED_C=CMPLX(0.0D0,0.0D0)

      DO I=1, 3
         CALCULATED_TYPE(I)=R_TYPE                       ! Real data type of the out-put
         IF(GENERATED_R(4).NE.0.0D0) &                   ! If zero, no need to do next.
         CALCULATED_R(I)=GENERATED_R(I)/GENERATED_R(4)   ! Acceleration=Force/Mass
                                                         ! for the three components separately
         CALCULATED_R(I+3)=CALCULATED_R(I)*(3600.0D0)**2/1000.0D0
                                                         ! Unit converted for acceleration
         CALCULATED_TYPE(I+3)=R_TYPE                     ! Real data type of the out-put
      END DO
      CALCULATED_NUMBER=6                                ! Total number of out-puts


  CASE(24)
      DO I=1, 8
         CALCULATED_TYPE(I)=R_TYPE    ! Real data type of the out-put
         CALCULATED_R(I)=GENERATED_R(18)*GENERATED_R(1)*GENERATED_R(I*2)/GENERATED_R(I*2+1)**2
                               ! Force calculated with Newton's Law of Universal Gravitation
      END DO
      CALCULATED_NUMBER=8             ! Total number of out-puts

  CASE(25)

  CASE(26)
      CALCULATED_R(1)=1.0D0 - GENERATED_R(2)            ! Possibility
      CALCULATED_R(2)=1.0D0 - GENERATED_R(4)            ! Possibility
      CALCULATED_R(3)=CALCULATED_R(1) * CALCULATED_R(2) ! Possibility
      CALCULATED_TYPE(1:3)=R_TYPE                       ! Real data type of the out-put
      CALCULATED_NUMBER=3                               ! Total number of out-puts

  CASE(27)
      ! All kinds of possibilities.

      CALCULATED_R(1)=1.0D0 - GENERATED_R(2)
      CALCULATED_R(2)=1.0D0 - GENERATED_R(4)

      IF(GENERATED_L(1)) THEN
         CALCULATED_R(3)=        GENERATED_R(2)
         CALCULATED_R(4)=1.0D0 - GENERATED_R(2)
      ELSE
         CALCULATED_R(3)=1.0D0 - GENERATED_R(2)
         CALCULATED_R(4)=        GENERATED_R(2)
      END IF

      IF(GENERATED_L(3)) THEN
         CALCULATED_R(5)=        GENERATED_R(4)
         CALCULATED_R(6)=1.0D0 - GENERATED_R(4)
      ELSE
         CALCULATED_R(5)=1.0D0 - GENERATED_R(4)
         CALCULATED_R(6)=        GENERATED_R(4)
      END IF

      CALCULATED_R(7)=CALCULATED_R(3)  *  CALCULATED_R(5)
      CALCULATED_R(8)=CALCULATED_R(3)  *  CALCULATED_R(6)
      CALCULATED_R(9)=CALCULATED_R(4)  *  CALCULATED_R(5)
      CALCULATED_R(10)=CALCULATED_R(4) *  CALCULATED_R(6)

      CALCULATED_R(11)=SUM(CALCULATED_R(7:10))

      CALCULATED_TYPE(1:11)=R_TYPE  ! Real data type of the out-put
      CALCULATED_NUMBER=11          ! Total number of out-puts


  CASE(28)
      OUTPUT_SSS=' '
      ! The following strings can not be inputted from the question, so defined here.
      ! They should be corresponding to the operations of input 3 respectively.
      OUTPUT_SSS(7)='ADDITION'         ! A predefined string in sequence
      OUTPUT_SSS(8)='SUBTRACTION'      ! A predefined string in sequence
      OUTPUT_SSS(9)='MULTIPLICATION'   ! A predefined string in sequence
      OUTPUT_SSS(10)='DIVISION'        ! A predefined string in sequence
      CALCULATED_POSITION(1,1)=7       ! Set lower limit of index range for these strings
      CALCULATED_POSITION(2,1)=10      ! Set upper limit of index range for these strings
      CALCULATED_I(1)=GENERATED_I(3)-GENERATED_POSITION(1,3)+ &
              CALCULATED_POSITION(1,1) ! Match the chosen operation with the above string
      CALCULATED_TYPE(1)=S_TYPE        ! Set the out-put data type (string)
      CALCULATED_TYPE(2)=R_TYPE        ! Set the out-put data type (real)
      ! Then do the math based on the operation chosen,
      !      and place the result as the last (second) out-put:
      IF(GENERATED_I(3).EQ.1) CALCULATED_R(2)=1.0D0*(GENERATED_I(1)+GENERATED_I(2))
      IF(GENERATED_I(3).EQ.2) CALCULATED_R(2)=1.0D0*(GENERATED_I(1)-GENERATED_I(2))
      IF(GENERATED_I(3).EQ.3) CALCULATED_R(2)=1.0D0*(GENERATED_I(1)*GENERATED_I(2))
      IF(GENERATED_I(3).EQ.4) CALCULATED_R(2)=(1.0D0*GENERATED_I(1))/GENERATED_I(2)
      CALCULATED_NUMBER=2   ! Total number of out-puts

  CASE(50)
      IF(GENERATED_R(2).NE.0.0D0) &                      ! If zero, no need to do next.
      CALCULATED_V(:,1)=GENERATED_V(:,1)/GENERATED_R(2)  ! Acceleration=Force/Mass
      CALCULATED_TYPE(1)=V_TYPE                          ! Vector data type of the out-puts
      CALCULATED_NUMBER=1                                ! Total number of out-puts

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

  CASE(70)
      ! Assuming GENERATED_I(1:2) are the two integer roots x1 and x2 of equation: A*x^2+B*x+C=0,
      ! the task is to construct  and to verify the equation:
      CALCULATED_TYPE(:)=I_TYPE    ! Most are integers
      OUTPUT_SSS=' '               ! Not necessary, initialize all out-put strings as a habit
      OUTPUT_SSS(1)='+'            ! Addition sign needed somewhere in the equation
      OUTPUT_SSS(2)=' '            ! Subtraction sign is never needed, as outputted with integer
                                   ! automatically

      CALCULATED_I(1)=GENERATED_I(3)
                                   ! A in equation of A*x^2+B*x+C=0

      CALCULATED_I(3)=-GENERATED_I(3)*(GENERATED_I(1)+GENERATED_I(2))
                                   ! B in equation of A*x^2+B*x+C=0
      CALCULATED_POSITION(1,2)=1   ! For each string out-put, code the beginning string index
      CALCULATED_POSITION(2,2)=2   ! For each string out-put, code the ending string index
                                   ! Specifically, CALCULATED_POSITION(1,n) and
                                   !   CALCULATED_POSITION(2,n) are the index range for the
                                   !   out-put number n
      CALCULATED_TYPE(2)=S_TYPE    ! String data type
      CALCULATED_I(2)=1            ! Set the sign for B
      IF(CALCULATED_I(3).LT.0) CALCULATED_I(2)=2

      CALCULATED_I(5)= GENERATED_I(3)*(GENERATED_I(1)*GENERATED_I(2))
                                   ! C in equation of A*x^2+B*x+C=0
      CALCULATED_POSITION(1,4)=1   ! For each string out-put, code the beginning string index
      CALCULATED_POSITION(2,4)=2   ! For each string out-put, code the ending string index
                                   ! Specifically, CALCULATED_POSITION(1,n) and
                                   !   CALCULATED_POSITION(2,n) are the index range for the
                                   !   out-put number n
      CALCULATED_TYPE(4)=S_TYPE    ! String data type
      CALCULATED_I(4)=1            ! Set the sign for C
      IF(CALCULATED_I(5).LT.0) CALCULATED_I(4)=2

      CALCULATED_I(6)= CALCULATED_I(1)*(GENERATED_I(1)*GENERATED_I(1))
                                   ! A*(x1)^2 for verifying purpose
      CALCULATED_I(7)= CALCULATED_I(3)*GENERATED_I(1)
                                   ! B*x1 for verifying purpose
      CALCULATED_I(8)= CALCULATED_I(6)+CALCULATED_I(7)
                                   ! A*(x1)^2 +B*x1 for verifying purpose
      CALCULATED_I(9)= CALCULATED_I(8)+CALCULATED_I(5)
                                   ! A*(x1)^2 +B*x1 +C for verifying purpose

      CALCULATED_I(10)= CALCULATED_I(1)*(GENERATED_I(2)*GENERATED_I(2))
                                   ! A*(x2)^2 for verifying purpose
      CALCULATED_I(11)= CALCULATED_I(3)*GENERATED_I(2)
                                   ! B*x2 for verifying purpose
      CALCULATED_I(12)= CALCULATED_I(10)+CALCULATED_I(11)
                                   ! A*(x2)^2 +B*x2 for verifying purpose
      CALCULATED_I(13)= CALCULATED_I(12)+CALCULATED_I(5)
                                   ! A*(x2)^2 +B*x2 +C for verifying purpose

      CALCULATED_NUMBER=13

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
