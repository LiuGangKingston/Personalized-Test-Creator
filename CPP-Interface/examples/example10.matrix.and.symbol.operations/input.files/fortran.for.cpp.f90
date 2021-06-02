MODULE FORTRAN_FOR_C
    USE BASE_DATA
    use iso_c_binding
    USE CONSTANT_INTERFACE
    IMPLICIT NONE
    INTEGER, PARAMETER :: LENGTHOFTYPENAME = 16
    CHARACTER (LEN = LENGTHOFTYPENAME), PARAMETER ::  TYPE_NAMES(10)=         &
                                    (/ ' INTEGER        ', ' REAL           ', &
                                       ' COMPLEX        ', ' STRING         ', &
                                       ' LOGICAL        ', ' VECTOR         ', &
                                       ' INTEGER MATRIX ', ' REAL MATRIX    ', &
                                       ' COMPLEX MATRIX ', ' STRING MATRIX  '    /)
    INTEGER :: OUTPUT_SSS_USED = 0

    INTERFACE
      ! SUBROUTINE C_SOLVE_THE_QUESTION_(QUESTION_IDENTIFIER) BIND ( C )
      !    USE ISO_C_BINDING
      !    INTEGER ( C_INT ) :: QUESTION_IDENTIFIER
      !    !REAL ( C_DOUBLE ) :: W2(*)
      ! END SUBROUTINE C_SOLVE_THE_QUESTION_
    END INTERFACE

CONTAINS


    FUNCTION PICKTYPENAME(I)
       IMPLICIT NONE
       INTEGER  :: I, K
       CHARACTER (LEN=LENGTHOFTYPENAME) :: PICKTYPENAME
       K = I
       IF(I.GT.100) K = I - 100 + 6
       PICKTYPENAME = TYPE_NAMES(K)
       RETURN
    END FUNCTION PICKTYPENAME


    SUBROUTINE CHECK_IN_NUMBER(IN_NUMBER, ROUTINENAME)
        IMPLICIT NONE
        INTEGER           :: IN_NUMBER
        CHARACTER (LEN=*) :: ROUTINENAME
        IF((IN_NUMBER.LE.0) .OR. (IN_NUMBER.GT.INPUT_NUMBER)) THEN
            print*, 'This is in the FORTRAN-called-by-C routine '
            print*, TRIM(ROUTINENAME)
            print*, 'for QUESTION_IDENTIFIER ',QUESTION_IDENTIFIER
            print*, 'for EXAMINEE_NUMBER ', EXAMINEE_NUMBER, ' .'
            print*, 'The value of the argument "in_number" is ', IN_NUMBER
            print*, "which must be in the range from 1 to ", INPUT_NUMBER
            print*, "then this run stopped."
            STOP
        END IF
        RETURN
    END SUBROUTINE CHECK_IN_NUMBER


    SUBROUTINE CHECK_MATRIX_BRACKET(IN_NUMBER, ROUTINENAME)
        IMPLICIT NONE
        INTEGER           :: IN_NUMBER
        CHARACTER (LEN=*) :: ROUTINENAME
        CALL CHECK_IN_NUMBER(IN_NUMBER, ROUTINENAME)
        IF((GENERATED_M(IN_NUMBER)%BRACE_INDEX.LT.0) .OR. &
          &(GENERATED_M(IN_NUMBER)%BRACE_INDEX.GT.NUMBER_OF_MATRIX_BRACES)) THEN
            print*, 'This is in the FORTRAN-called-by-C routine '
            print*, TRIM(ROUTINENAME)
            print*, 'for QUESTION_IDENTIFIER ',QUESTION_IDENTIFIER
            print*, 'for EXAMINEE_NUMBER ', EXAMINEE_NUMBER
            print*, 'for the IN_NUMBER   ', IN_NUMBER
            print*, 'Since the BRACE_INDEX is ', GENERATED_M(IN_NUMBER)%BRACE_INDEX
            print*, "which must be in the range from 0 to ", NUMBER_OF_MATRIX_BRACES
            print*, "then this run stopped."
            STOP
        END IF
        RETURN
    END SUBROUTINE CHECK_MATRIX_BRACKET


    SUBROUTINE CHECK_DATA_TYPE(IN_NUMBER, ASSUMED_TYPE, ROUTINENAME)
        IMPLICIT NONE
        INTEGER           :: IN_NUMBER
        INTEGER           :: ASSUMED_TYPE, ACTUAL_TYPE
        CHARACTER (LEN=*) :: ROUTINENAME
        CALL CHECK_IN_NUMBER(IN_NUMBER, ROUTINENAME)
        ACTUAL_TYPE  = INPUT_TYPE(IN_NUMBER)
        IF(ASSUMED_TYPE.NE.ACTUAL_TYPE) THEN
            print*, 'This is in the FORTRAN-called-by-C routine '
            print*, TRIM(ROUTINENAME)
            print*, 'for QUESTION_IDENTIFIER ',QUESTION_IDENTIFIER
            print*, 'for EXAMINEE_NUMBER ', EXAMINEE_NUMBER, ' .'
            print*, 'The generated data type of the input number ', IN_NUMBER, ' is '
            print*, PICKTYPENAME(ACTUAL_TYPE)
            print*, 'Since it is not the assumed data type '//PICKTYPENAME(ASSUMED_TYPE)//' ,'
            print*, 'this run stopped.'
            STOP
        END IF
        RETURN
    END SUBROUTINE CHECK_DATA_TYPE


    SUBROUTINE CHECK_STRING_SIZE(IN_NUMBER, THE_SEQUENCE, ROUTINENAME)
        IMPLICIT NONE
        INTEGER           :: IN_NUMBER
        INTEGER           :: THE_SEQUENCE
        CHARACTER (LEN=*) :: ROUTINENAME
        !THE_SEQUENCE = GENERATED_I(IN_NUMBER)
        IF((THE_SEQUENCE.LE.0) .OR. (THE_SEQUENCE.LT.GENERATED_POSITION(1,IN_NUMBER)) &
           &                   .OR. (THE_SEQUENCE.GT.GENERATED_POSITION(2,IN_NUMBER))) THEN
            print*, 'This is in the FORTRAN-called-by-C routine '
            print*, TRIM(ROUTINENAME)
            print*, 'for QUESTION_IDENTIFIER ',QUESTION_IDENTIFIER
            print*, 'for EXAMINEE_NUMBER ', EXAMINEE_NUMBER, ' .'
            print*, 'with the input number ', IN_NUMBER
            print*, 'Since the sequence: ',THE_SEQUENCE, ' is not between: ',GENERATED_POSITION(1:2,IN_NUMBER)
            print*, 'a bug is there, then stopped. Please send email to: '
            print*, 'gl.cell@outlook.com '
            STOP
        END IF
        IF(LEN_TRIM(INPUT_SSS(THE_SEQUENCE)).GE.MAX_NUMBER_OF_CHARACTERS_SSS) THEN
            print*, 'This is in the FORTRAN-called-by-C routine '
            print*, TRIM(ROUTINENAME)
            print*, 'for QUESTION_IDENTIFIER ',QUESTION_IDENTIFIER
            print*, 'for EXAMINEE_NUMBER ', EXAMINEE_NUMBER, ' .'
            print*, 'The generated string of the input number ', IN_NUMBER
            print*, 'is full in the string capacity: ', MAX_NUMBER_OF_CHARACTERS_SSS, ' ,'
            print*, 'then not possible to append "NULL" for C/C++ convention.'
            print*, 'Then stopped. '
            print*, 'The macro SINGLE_STRING_SIZE in the C file and '
            print*, 'the constant MAX_NUMBER_OF_CHARACTERS_SSS in PTC-*.f90 '
            print*, 'are recommended to update. '
            STOP
        END IF
        RETURN
    END SUBROUTINE CHECK_STRING_SIZE


    SUBROUTINE CHECK_ELEMENT_STRING_SIZE(IN_NUMBER, ROW, COL, ROUTINENAME)
        IMPLICIT NONE
        INTEGER           :: IN_NUMBER
        INTEGER           :: ROW, COL
        CHARACTER (LEN=*) :: ROUTINENAME
        IF(LEN_TRIM(GENERATED_M(IN_NUMBER)%STRING_M(ROW,COL)).GE.MAX_NUMBER_OF_CHARACTERS_SSS) THEN
            print*, 'This is in the FORTRAN-called-by-C routine '
            print*, TRIM(ROUTINENAME)
            print*, 'for QUESTION_IDENTIFIER ',QUESTION_IDENTIFIER
            print*, 'for EXAMINEE_NUMBER ', EXAMINEE_NUMBER, ' .'
            print*, 'The generated elemental string of the input number, row, and col: ', IN_NUMBER, ROW-1, COL-1
            print*, 'is full in the string capacity: ', MAX_NUMBER_OF_CHARACTERS_SSS, ' ,'
            print*, 'then not possible to append "NULL" for C/C++ convention.'
            print*, 'Then stopped. '
            print*, 'The macro SINGLE_STRING_SIZE in the C file and '
            print*, 'the constant MAX_NUMBER_OF_CHARACTERS_SSS in PTC-*.f90 '
            print*, 'are recommended to update. '
            STOP
        END IF
        RETURN
    END SUBROUTINE CHECK_ELEMENT_STRING_SIZE


    SUBROUTINE CHECK_ARRAY_EXISTING_AND_SIZE(IN_NUMBER, ASSUMED_TYPE, ROW_NUMBER, COL_NUMBER, ROUTINENAME)
        IMPLICIT NONE
        INTEGER           :: IN_NUMBER, ASSUMED_TYPE, ROW_NUMBER, COL_NUMBER, SZ(2)
        CHARACTER (LEN=*) :: ROUTINENAME
        LOGICAL           :: A_ALLOCATED
        CALL CHECK_DATA_TYPE(IN_NUMBER, ASSUMED_TYPE, ROUTINENAME)

        SZ = 0
        A_ALLOCATED = .FALSE.
        SELECT CASE (ASSUMED_TYPE)

        CASE(I_MATRIX_TYPE)
            IF(ASSOCIATED(GENERATED_M(IN_NUMBER)%INTEGER_M)) THEN
               SZ = SHAPE(GENERATED_M(IN_NUMBER)%INTEGER_M)
               A_ALLOCATED = .TRUE.
            END IF

        CASE(R_MATRIX_TYPE)
            IF(ASSOCIATED(GENERATED_M(IN_NUMBER)%REAL_M)) THEN
               SZ = SHAPE(GENERATED_M(IN_NUMBER)%REAL_M)
               A_ALLOCATED = .TRUE.
            END IF

        CASE(C_MATRIX_TYPE)
            IF(ASSOCIATED(GENERATED_M(IN_NUMBER)%COMPLEX_M)) THEN
               SZ = SHAPE(GENERATED_M(IN_NUMBER)%COMPLEX_M)
               A_ALLOCATED = .TRUE.
            END IF

        CASE(S_MATRIX_TYPE)
            IF(ASSOCIATED(GENERATED_M(IN_NUMBER)%STRING_M)) THEN
               SZ = SHAPE(GENERATED_M(IN_NUMBER)%STRING_M)
               A_ALLOCATED = .TRUE.
            END IF

        CASE DEFAULT
            print*, 'This is in the FORTRAN-called-by-C routine '
            print*, TRIM(ROUTINENAME)
            print*, 'for QUESTION_IDENTIFIER ',QUESTION_IDENTIFIER
            print*, 'for EXAMINEE_NUMBER ', EXAMINEE_NUMBER, ' .'
            print*, 'with the input number ', IN_NUMBER
            print*, 'Since the ASSUMED_TYPE: ',ASSUMED_TYPE, ' is not legal, '
            print*, 'a bug is there, then stopped. Please send email to: '
            print*, 'gl.cell@outlook.com '
            STOP

        END SELECT

        IF(.NOT. A_ALLOCATED) THEN
            print*, 'This is in the FORTRAN-called-by-C routine '
            print*, TRIM(ROUTINENAME)
            print*, 'for QUESTION_IDENTIFIER ',QUESTION_IDENTIFIER
            print*, 'for EXAMINEE_NUMBER ', EXAMINEE_NUMBER, ' '
            print*, 'for the input number ', IN_NUMBER
            print*, 'Since the allocatable matrix of type '//PICKTYPENAME(ASSUMED_TYPE)//' is not allocated,'
            print*, 'this run stopped.'
            STOP
        END IF

        IF( (SZ(1).LE.0) .OR. (SZ(1).NE.GENERATED_M(IN_NUMBER)%SIZES(1)) .OR. &
           &(SZ(2).LE.0) .OR. (SZ(2).NE.GENERATED_M(IN_NUMBER)%SIZES(2)))      THEN
            print*, 'This is in the FORTRAN-called-by-C routine '
            print*, TRIM(ROUTINENAME)
            print*, 'for QUESTION_IDENTIFIER ',QUESTION_IDENTIFIER
            print*, 'for EXAMINEE_NUMBER ', EXAMINEE_NUMBER, ' '
            print*, 'for the input number ', IN_NUMBER
            print*, 'Since the sizes of the allocatable matrix of type '//PICKTYPENAME(ASSUMED_TYPE)
            print*, sz, GENERATED_M(IN_NUMBER)%SIZES
            print*, 'are not correct, this run stopped.'
            print*, 'If this is believed a bug, please send email to: '
            print*, 'gl.cell@outlook.com '
            STOP
        END IF

        IF( (ROW_NUMBER.LE.0) .OR. (ROW_NUMBER .GT. SZ(1)) )      THEN
            print*, 'This is in the FORTRAN-called-by-C routine '
            print*, TRIM(ROUTINENAME)
            print*, 'for QUESTION_IDENTIFIER ',QUESTION_IDENTIFIER
            print*, 'for EXAMINEE_NUMBER ', EXAMINEE_NUMBER, ' '
            print*, 'for the input number ', IN_NUMBER
            print*, 'Since the ROW_NUMBER of the array is ', ROW_NUMBER-1
            print*, 'which should be from 0 to ', SZ(1)-1
            print*, 'this run stopped.'
            STOP
        END IF

        IF( (COL_NUMBER.LE.0) .OR. (COL_NUMBER .GT. SZ(2)) )      THEN
            print*, 'This is in the FORTRAN-called-by-C routine '
            print*, TRIM(ROUTINENAME)
            print*, 'for QUESTION_IDENTIFIER ',QUESTION_IDENTIFIER
            print*, 'for EXAMINEE_NUMBER ', EXAMINEE_NUMBER, ' '
            print*, 'for the input number ', IN_NUMBER
            print*, 'Since the COL_NUMBER of the array is ', COL_NUMBER-1
            print*, 'which should be from 0 to ', SZ(2)-1
            print*, 'this run stopped.'
            STOP
        END IF

        RETURN
    END SUBROUTINE CHECK_ARRAY_EXISTING_AND_SIZE


!   Calculated_Number
!   Calculated_Number
!   Calculated_Number
!   Calculated_Number
!   Calculated_Number
!   Calculated_Number


    SUBROUTINE CHECK_CAL_NUMBER(CAL_NUMBER, ROUTINENAME)
        IMPLICIT NONE
        INTEGER           :: CAL_NUMBER
        CHARACTER (LEN=*) :: ROUTINENAME
        IF((CAL_NUMBER.LE.0) .OR. (CAL_NUMBER.GT.MAX_NUMBER_OF_CALCULATES_PQ)) THEN
            print*, 'This is in the FORTRAN-called-by-C routine '
            print*, TRIM(ROUTINENAME)
            print*, 'for QUESTION_IDENTIFIER ',QUESTION_IDENTIFIER
            print*, 'for EXAMINEE_NUMBER ', EXAMINEE_NUMBER, ' .'
            print*, 'The value of the argument "CAL_NUMBER" is ', CAL_NUMBER
            print*, "which must be in the range from 1 to ", MAX_NUMBER_OF_CALCULATES_PQ
            print*, "then this run stopped."
            STOP
        END IF
        RETURN
    END SUBROUTINE CHECK_CAL_NUMBER


    SUBROUTINE CHECK_CAL_MATRIX_BRACKET(CAL_NUMBER, B_INDEX, ROUTINENAME)
        IMPLICIT NONE
        INTEGER           :: CAL_NUMBER
        INTEGER           :: B_INDEX
        CHARACTER (LEN=*) :: ROUTINENAME
        CALL CHECK_CAL_NUMBER(CAL_NUMBER, ROUTINENAME)
        IF((B_INDEX.LT.0) .OR. (B_INDEX.GT.NUMBER_OF_MATRIX_BRACES)) THEN
            print*, 'This is in the FORTRAN-called-by-C routine '
            print*, TRIM(ROUTINENAME)
            print*, 'for QUESTION_IDENTIFIER ',QUESTION_IDENTIFIER
            print*, 'for EXAMINEE_NUMBER ', EXAMINEE_NUMBER
            print*, 'for the CAL_NUMBER   ', CAL_NUMBER
            print*, 'Since the intended BRACE_INDEX is ', B_INDEX
            print*, "which must be in the range from 0 to ", NUMBER_OF_MATRIX_BRACES
            print*, "then this run stopped."
            STOP
        END IF
        RETURN
    END SUBROUTINE CHECK_CAL_MATRIX_BRACKET


    SUBROUTINE CHECK_CAL_DATA_TYPE(CAL_NUMBER, ASSUMED_TYPE, ROUTINENAME)
        IMPLICIT NONE
        INTEGER           :: CAL_NUMBER
        INTEGER           :: ASSUMED_TYPE, ACTUAL_TYPE
        CHARACTER (LEN=*) :: ROUTINENAME
        CALL CHECK_CAL_NUMBER(CAL_NUMBER, ROUTINENAME)
        ACTUAL_TYPE  = CALCULATED_TYPE(CAL_NUMBER)
        IF(ACTUAL_TYPE.EQ.UNDEFINED_TYPE) THEN
            CALCULATED_TYPE(CAL_NUMBER) = ASSUMED_TYPE
        ELSE IF(ACTUAL_TYPE.NE.ASSUMED_TYPE) THEN
            print*, 'This is in the FORTRAN-called-by-C routine '
            print*, TRIM(ROUTINENAME)
            print*, 'for QUESTION_IDENTIFIER ',QUESTION_IDENTIFIER
            print*, 'for EXAMINEE_NUMBER ', EXAMINEE_NUMBER, ' .'
            print*, 'Since the CAL_NUMBER ', CAL_NUMBER, ' is already used as data type '
            print*, PICKTYPENAME(ACTUAL_TYPE)
            print*, 'this run stopped.'
            STOP
        END IF
        RETURN
    END SUBROUTINE CHECK_CAL_DATA_TYPE


    SUBROUTINE CHECK_CAL_DATA_TYPE_AND_SIZE(CAL_NUMBER, ASSUMED_TYPE, SZ, ROUTINENAME)
        IMPLICIT NONE
        INTEGER           :: CAL_NUMBER
        INTEGER           :: ASSUMED_TYPE, ACTUAL_TYPE, SZ(2)
        CHARACTER (LEN=*) :: ROUTINENAME
        CALL CHECK_CAL_DATA_TYPE(CAL_NUMBER, ASSUMED_TYPE, ROUTINENAME)
        IF( (SZ(1).LE.0) .OR. (SZ(2).LE.0) ) THEN
            print*, 'This is in the FORTRAN-called-by-C routine '
            print*, TRIM(ROUTINENAME)
            print*, 'for QUESTION_IDENTIFIER ',QUESTION_IDENTIFIER
            print*, 'for EXAMINEE_NUMBER ', EXAMINEE_NUMBER, ' '
            print*, 'for the CAL_NUMBER ', CAL_NUMBER
            print*, 'since the sizes used to allocate matrix of type '//PICKTYPENAME(ASSUMED_TYPE)
            print*, SZ
            print*, 'are not reasonable, this run stopped.'
            STOP
        END IF
        RETURN
    END SUBROUTINE CHECK_CAL_DATA_TYPE_AND_SIZE


    SUBROUTINE CHECK_CAL_DATA_TYPE2(CAL_NUMBER, ASSUMED_TYPE, ROUTINENAME)
        IMPLICIT NONE
        INTEGER           :: CAL_NUMBER
        INTEGER           :: ASSUMED_TYPE, ACTUAL_TYPE
        CHARACTER (LEN=*) :: ROUTINENAME
        CALL CHECK_CAL_NUMBER(CAL_NUMBER, ROUTINENAME)
        ACTUAL_TYPE  = CALCULATED_TYPE(CAL_NUMBER)
        IF(ACTUAL_TYPE.NE.ASSUMED_TYPE) THEN
            print*, 'This is in the FORTRAN-called-by-C routine '
            print*, TRIM(ROUTINENAME)
            print*, 'for QUESTION_IDENTIFIER ',QUESTION_IDENTIFIER
            print*, 'for EXAMINEE_NUMBER ', EXAMINEE_NUMBER, ' .'
            print*, 'The intended data type of the CAL_NUMBER ', CAL_NUMBER, ' is '
            print*, PICKTYPENAME(ASSUMED_TYPE)
            print*, 'Since the data type '//PICKTYPENAME(ACTUAL_TYPE)//' is already used,'
            print*, 'this run stopped.'
            STOP
        END IF
        RETURN
    END SUBROUTINE CHECK_CAL_DATA_TYPE2


    SUBROUTINE CHECK_CAL_ARRAY_EXIST_AND_SIZE(CAL_NUMBER, ASSUMED_TYPE, ROW_NUMBER, COL_NUMBER, ROUTINENAME)
        IMPLICIT NONE
        INTEGER           :: CAL_NUMBER, ASSUMED_TYPE, ROW_NUMBER, COL_NUMBER, SZ(2)
        CHARACTER (LEN=*) :: ROUTINENAME
        LOGICAL           :: A_ALLOCATED
        CALL CHECK_CAL_DATA_TYPE2(CAL_NUMBER, ASSUMED_TYPE, ROUTINENAME)

        SZ = 0
        A_ALLOCATED = .FALSE.
        SELECT CASE (ASSUMED_TYPE)

        CASE(I_MATRIX_TYPE)
            IF(ASSOCIATED(CALCULATED_M(CAL_NUMBER)%INTEGER_M)) THEN
               SZ = SHAPE(CALCULATED_M(CAL_NUMBER)%INTEGER_M)
               A_ALLOCATED = .TRUE.
            END IF

        CASE(R_MATRIX_TYPE)
            IF(ASSOCIATED(CALCULATED_M(CAL_NUMBER)%REAL_M)) THEN
               SZ = SHAPE(CALCULATED_M(CAL_NUMBER)%REAL_M)
               A_ALLOCATED = .TRUE.
            END IF

        CASE(C_MATRIX_TYPE)
            IF(ASSOCIATED(CALCULATED_M(CAL_NUMBER)%COMPLEX_M)) THEN
               SZ = SHAPE(CALCULATED_M(CAL_NUMBER)%COMPLEX_M)
               A_ALLOCATED = .TRUE.
            END IF

        CASE(S_MATRIX_TYPE)
            IF(ASSOCIATED(CALCULATED_M(CAL_NUMBER)%STRING_M)) THEN
               SZ = SHAPE(CALCULATED_M(CAL_NUMBER)%STRING_M)
               A_ALLOCATED = .TRUE.
            END IF

        CASE DEFAULT
            print*, 'This is in the FORTRAN-called-by-C routine '
            print*, TRIM(ROUTINENAME)
            print*, 'for QUESTION_IDENTIFIER ',QUESTION_IDENTIFIER
            print*, 'for EXAMINEE_NUMBER ', EXAMINEE_NUMBER, ' .'
            print*, 'for the CAL_NUMBER ', CAL_NUMBER
            print*, 'Since the ASSUMED_TYPE: ',ASSUMED_TYPE, ' is not legal, '
            print*, 'a bug is there, then stopped. Please send email to: '
            print*, 'gl.cell@outlook.com '
            STOP

        END SELECT

        IF(.NOT. A_ALLOCATED) THEN
            print*, 'This is in the FORTRAN-called-by-C routine '
            print*, TRIM(ROUTINENAME)
            print*, 'for QUESTION_IDENTIFIER ',QUESTION_IDENTIFIER
            print*, 'for EXAMINEE_NUMBER ', EXAMINEE_NUMBER, ' '
            print*, 'for the CAL_NUMBER ', CAL_NUMBER
            print*, 'Since the allocatable matrix of type '//PICKTYPENAME(ASSUMED_TYPE)//' is not allocated,'
            print*, 'this run stopped.'
            STOP
        END IF

        IF( (SZ(1).LE.0) .OR. (SZ(1).NE.CALCULATED_M(CAL_NUMBER)%SIZES(1)) .OR. &
           &(SZ(2).LE.0) .OR. (SZ(2).NE.CALCULATED_M(CAL_NUMBER)%SIZES(2)))      THEN
            print*, 'This is in the FORTRAN-called-by-C routine '
            print*, TRIM(ROUTINENAME)
            print*, 'for QUESTION_IDENTIFIER ',QUESTION_IDENTIFIER
            print*, 'for EXAMINEE_NUMBER ', EXAMINEE_NUMBER, ' '
            print*, 'for the CAL_NUMBER ', CAL_NUMBER
            print*, 'Since the sizes of the allocatable matrix of type '//PICKTYPENAME(ASSUMED_TYPE)
            print*, sz, CALCULATED_M(CAL_NUMBER)%SIZES
            print*, 'are not correct, this run stopped.'
            print*, 'If this is believed a bug, please send email to: '
            print*, 'gl.cell@outlook.com '
            STOP
        END IF

        IF( (ROW_NUMBER.LE.0) .OR. (ROW_NUMBER .GT. SZ(1)) )      THEN
            print*, 'This is in the FORTRAN-called-by-C routine '
            print*, TRIM(ROUTINENAME)
            print*, 'for QUESTION_IDENTIFIER ',QUESTION_IDENTIFIER
            print*, 'for EXAMINEE_NUMBER ', EXAMINEE_NUMBER, ' '
            print*, 'for the CAL_NUMBER ', CAL_NUMBER
            print*, 'Since the ROW_NUMBER of the array is ', ROW_NUMBER-1
            print*, 'which should be from 0 to ', SZ(1)-1
            print*, 'this run stopped.'
            STOP
        END IF

        IF( (COL_NUMBER.LE.0) .OR. (COL_NUMBER .GT. SZ(2)) )      THEN
            print*, 'This is in the FORTRAN-called-by-C routine '
            print*, TRIM(ROUTINENAME)
            print*, 'for QUESTION_IDENTIFIER ',QUESTION_IDENTIFIER
            print*, 'for EXAMINEE_NUMBER ', EXAMINEE_NUMBER, ' '
            print*, 'for the CAL_NUMBER ', CAL_NUMBER
            print*, 'Since the COL_NUMBER of the array is ', COL_NUMBER-1
            print*, 'which should be from 0 to ', SZ(2)-1
            print*, 'this run stopped.'
            STOP
        END IF

        RETURN
    END SUBROUTINE CHECK_CAL_ARRAY_EXIST_AND_SIZE


END MODULE FORTRAN_FOR_C




subroutine check_single_string_size(c_single_string_size)
    use BASE_DATA
    implicit none
    integer  :: c_single_string_size
    if(c_single_string_size .ne. MAX_NUMBER_OF_CHARACTERS_SSS) then
        print*, "The value of the macro SINGLE_STRING_SIZE in the C file is ", c_single_string_size
        print*, "while the constant MAX_NUMBER_OF_CHARACTERS_SSS in PTC-*.f90 is ", MAX_NUMBER_OF_CHARACTERS_SSS
        print*, "then this run stopped. "
        print*, "Please change the source code, so that they are the same."
        stop
    end if
    return
end subroutine check_single_string_size




subroutine get_generated_i(in_number, the_result)
    use FORTRAN_FOR_C
    implicit none
    integer        :: in_number
    integer        :: the_result
    call CHECK_DATA_TYPE(in_number, I_TYPE, "get_generated_i(in_number, the_result)")
    the_result = GENERATED_I(in_number)
    return
end subroutine get_generated_i




subroutine get_generated_r(in_number, the_result)
    use FORTRAN_FOR_C
    implicit none
    integer          :: in_number
    double precision :: the_result
    call CHECK_DATA_TYPE(in_number, R_TYPE, "get_generated_r(in_number, the_result)")
    the_result = GENERATED_R(in_number)
    return
end subroutine get_generated_r




subroutine get_generated_c(in_number, the_result)
    use FORTRAN_FOR_C
    implicit none
    integer          :: in_number
    double precision :: the_result(2)
    call CHECK_DATA_TYPE(in_number, C_TYPE, "get_generated_c(in_number, the_result)")
    the_result(1) =  real(GENERATED_C(in_number))
    the_result(2) = aimag(GENERATED_C(in_number))
    return
end subroutine get_generated_c




subroutine get_generated_s(in_number, the_sequence, the_string)
    use FORTRAN_FOR_C
    implicit none
    integer          :: in_number
    integer          :: the_sequence
    character(len=MAX_NUMBER_OF_CHARACTERS_SSS):: the_string
    call CHECK_DATA_TYPE(in_number, S_TYPE, "get_generated_s(in_number, the_sequence, the_result)")
    the_sequence = GENERATED_I(in_number)
    CALL CHECK_STRING_SIZE(in_number, the_sequence, "get_generated_s(in_number, the_sequence, the_string)")
    the_string   = ' '
    the_string   = trim(INPUT_SSS(the_sequence))//C_NULL_CHAR
    the_sequence = GENERATED_I(in_number) - GENERATED_POSITION(1,in_number) + 1
    return
end subroutine get_generated_s




subroutine get_generated_l(in_number, the_sequence, the_result, the_string)
    use FORTRAN_FOR_C
    implicit none
    integer          :: in_number
    integer          :: the_sequence
    integer          :: the_result
    character(len=MAX_NUMBER_OF_CHARACTERS_SSS):: the_string
    call CHECK_DATA_TYPE(in_number, L_TYPE, "get_generated_l(in_number, the_sequence, the_result, the_string)")
    the_sequence = GENERATED_I(in_number)
    CALL CHECK_STRING_SIZE(in_number, the_sequence, "get_generated_l(in_number, the_sequence, the_result, the_string)")
    the_string   = ' '
    the_string   = trim(INPUT_SSS(the_sequence))//C_NULL_CHAR
    the_sequence = GENERATED_I(in_number) - GENERATED_POSITION(1,in_number) + 1
    the_result   = 0
    if(GENERATED_L(in_number)) the_result = 1
    return
end subroutine get_generated_l




subroutine get_generated_v(in_number, the_result)
    use FORTRAN_FOR_C
    implicit none
    integer          :: in_number
    double precision :: the_result(3)
    call CHECK_DATA_TYPE(in_number, V_TYPE, "get_generated_v(in_number, the_result)")
    the_result(1) = GENERATED_V(1,in_number)
    the_result(2) = GENERATED_V(2,in_number)
    the_result(3) = GENERATED_V(3,in_number)
    return
end subroutine get_generated_v




subroutine get_generated_accuracy(in_number, the_result)
    use FORTRAN_FOR_C
    implicit none
    integer          :: in_number
    double precision :: the_result(3)
    call CHECK_IN_NUMBER(in_number, "get_generated_accuracy(in_number, the_result)")
    the_result(1) = INPUT_ACCURACY(1,in_number)
    the_result(2) = INPUT_ACCURACY(2,in_number)
    the_result(3) = INPUT_ACCURACY(3,in_number)
    return
end subroutine get_generated_accuracy




subroutine get_generated_mtx_bracket(in_number, the_result)
    use FORTRAN_FOR_C
    implicit none
    integer        :: in_number
    integer        :: the_result
    call CHECK_MATRIX_BRACKET(in_number, "get_generated_mtx_bracket(in_number, the_result)")
    the_result = GENERATED_M(in_number)%BRACE_INDEX
    return
end subroutine get_generated_mtx_bracket




subroutine get_generated_mtx_sizes(in_number, the_result)
    use FORTRAN_FOR_C
    implicit none
    integer          :: in_number
    integer          :: the_result(2)
    call CHECK_IN_NUMBER(in_number, "get_generated_mtx_sizes(in_number, the_result)")
    if( (INPUT_TYPE(in_number).lt.I_MATRIX_TYPE) .OR. (INPUT_TYPE(in_number).gt.S_MATRIX_TYPE) ) then
            print*, 'This is in the FORTRAN-called-by-C routine '
            print*, "get_generated_mtx_sizes(in_number, the_result)"
            print*, 'for QUESTION_IDENTIFIER ',QUESTION_IDENTIFIER
            print*, 'for EXAMINEE_NUMBER ', EXAMINEE_NUMBER, ' '
            print*, 'for the input number ', IN_NUMBER
            print*, 'the generated data is not a matrix, '
            print*, 'then this run stopped.'
            stop
    end if
    the_result =  GENERATED_M(in_number)%SIZES
    return
end subroutine get_generated_mtx_sizes




subroutine get_generated_mtx_element_i(in_number, row_number, col_number, the_result)
    use FORTRAN_FOR_C
    implicit none
    integer        :: in_number
    integer        :: row_number
    integer        :: col_number
    integer        :: the_result
    call CHECK_ARRAY_EXISTING_AND_SIZE(in_number, I_MATRIX_TYPE, row_number, col_number, &
                        &"get_generated_mtx_element_i(in_number, row_number, col_number, the_result)")
    the_result = GENERATED_M(in_number)%INTEGER_M(row_number,col_number)
    return
end subroutine get_generated_mtx_element_i




subroutine get_generated_mtx_element_r(in_number, row_number, col_number, the_result)
    use FORTRAN_FOR_C
    implicit none
    integer          :: in_number
    integer          :: row_number
    integer          :: col_number
    double precision :: the_result
    call CHECK_ARRAY_EXISTING_AND_SIZE(in_number, R_MATRIX_TYPE, row_number, col_number, &
                        &"get_generated_mtx_element_r(in_number, row_number, col_number, the_result)")
    the_result = GENERATED_M(in_number)%REAL_M(row_number,col_number)
    return
end subroutine get_generated_mtx_element_r




subroutine get_generated_mtx_element_c(in_number, row_number, col_number, the_result)
    use FORTRAN_FOR_C
    implicit none
    integer          :: in_number
    integer          :: row_number
    integer          :: col_number
    double precision :: the_result(2)
    call CHECK_ARRAY_EXISTING_AND_SIZE(in_number, C_MATRIX_TYPE, row_number, col_number, &
                        &"get_generated_mtx_element_c(in_number, row_number, col_number, the_result)")
    the_result(1) =  real(GENERATED_M(in_number)%COMPLEX_M(row_number,col_number))
    the_result(2) = aimag(GENERATED_M(in_number)%COMPLEX_M(row_number,col_number))
    return
end subroutine get_generated_mtx_element_c




subroutine get_generated_mtx_element_s(in_number, row_number, col_number, the_string)
    use FORTRAN_FOR_C
    implicit none
    integer          :: in_number
    integer          :: row_number
    integer          :: col_number
    character(len=MAX_NUMBER_OF_CHARACTERS_SSS):: the_string
    call CHECK_ARRAY_EXISTING_AND_SIZE(in_number, S_MATRIX_TYPE, row_number, col_number, &
                        &"get_generated_mtx_element_s(in_number, row_number, col_number, the_string)")
    call CHECK_ELEMENT_STRING_SIZE(in_number, row_number, col_number, &
                        &"get_generated_mtx_element_s(in_number, row_number, col_number, the_string)")
    the_string = ' '
    the_string = trim(GENERATED_M(in_number)%STRING_M(row_number,col_number))//C_NULL_CHAR
    return
end subroutine get_generated_mtx_element_s




subroutine get_total_n_of_input_strings(in_number, the_total)
    use FORTRAN_FOR_C
    implicit none
    integer          :: in_number
    integer          :: the_total, atype
    call CHECK_IN_NUMBER(in_number, "get_total_n_of_input_strings(in_number, the_total)")
    atype = INPUT_TYPE(in_number)
    IF(((atype.NE.L_TYPE) .and. (atype.NE.S_TYPE) .and. (atype.NE.S_MATRIX_TYPE)) .or. &
      &(GENERATED_POSITION(2,in_number) .le. 0) )                                        THEN
            print*, 'This is in the FORTRAN-called-by-C routine '
            print*, "get_total_n_of_input_strings(in_number, the_total)"
            print*, 'for QUESTION_IDENTIFIER ',QUESTION_IDENTIFIER
            print*, 'for EXAMINEE_NUMBER ', EXAMINEE_NUMBER
            print*, 'for the input number ', IN_NUMBER, ' , '
            print*, 'the data type is ', PICKTYPENAME(atype)
            print*, 'Since there were no string(s) inputted previously, '
            print*, 'this run stopped.'
            STOP
    END IF
    the_total = GENERATED_POSITION(2,in_number)-GENERATED_POSITION(1,in_number)+1
    return
end subroutine get_total_n_of_input_strings




subroutine get_input_string(in_number, sequential_number, the_string)
    use FORTRAN_FOR_C
    implicit none
    integer          :: in_number
    integer          :: sequential_number, atype
    character(len=MAX_NUMBER_OF_CHARACTERS_SSS):: the_string
    call CHECK_IN_NUMBER(in_number, "get_input_string(in_number, sequential_number, the_string)")
    atype = INPUT_TYPE(in_number)
    IF((atype.NE.L_TYPE) .and. (atype.NE.S_TYPE) .and. (atype.NE.S_MATRIX_TYPE)) THEN
            print*, 'This is in the FORTRAN-called-by-C routine '
            print*, "get_input_string(in_number, sequential_number, the_string)"
            print*, 'for QUESTION_IDENTIFIER ',QUESTION_IDENTIFIER
            print*, 'for EXAMINEE_NUMBER ', EXAMINEE_NUMBER
            print*, 'for the input number ', IN_NUMBER, ' , '
            print*, 'the data type is ', PICKTYPENAME(atype)
            print*, 'Since there were no string(s) inputted previously, '
            print*, 'this run stopped.'
            STOP
    END IF
    IF((sequential_number .le. 0) .or. &
      &(sequential_number .gt. (GENERATED_POSITION(2,in_number)-GENERATED_POSITION(1,in_number)+1))) THEN
            print*, 'This is in the FORTRAN-called-by-C routine '
            print*, "get_input_string(in_number, sequential_number, the_string)"
            print*, 'for QUESTION_IDENTIFIER ',QUESTION_IDENTIFIER
            print*, 'for EXAMINEE_NUMBER ', EXAMINEE_NUMBER
            print*, 'for the input number ', IN_NUMBER, ' , '
            print*, 'the sequential_number is ', sequential_number
            print*, 'which should be in between 1 and ', &
                    & GENERATED_POSITION(2,in_number)-GENERATED_POSITION(1,in_number)+1
            print*, 'then this run stopped.'
            STOP
    END IF
    sequential_number = sequential_number + GENERATED_POSITION(1,in_number) - 1
    call CHECK_STRING_SIZE(in_number, sequential_number, &
                           &"get_input_string(in_number, sequential_number, the_string)")
    the_string = ' '
    the_string = trim(INPUT_SSS(sequential_number))//C_NULL_CHAR
    return
end subroutine get_input_string




subroutine get_input_iii(in_number, the_result)
    use FORTRAN_FOR_C
    implicit none
    integer :: in_number
    integer :: the_result(3)
    call CHECK_IN_NUMBER(in_number, "get_input_iii(in_number, the_result)")
    the_result(1:3) = INPUT_III(1:3,in_number)
    return
end subroutine get_input_iii




subroutine get_input_rrr(in_number, the_result)
    use FORTRAN_FOR_C
    implicit none
    integer          :: in_number
    double precision :: the_result(3)
    call CHECK_IN_NUMBER(in_number, "get_input_rrr(in_number, the_result)")
    the_result(1:3) = INPUT_RRR(1:3,in_number)
    return
end subroutine get_input_rrr




subroutine get_input_ccc(in_number, the_result)
    use FORTRAN_FOR_C
    implicit none
    integer          :: in_number
    double precision :: the_result(6)
    call CHECK_IN_NUMBER(in_number, "get_input_ccc(in_number, the_result)")
    the_result(1) =  real(INPUT_CCC(1,in_number))
    the_result(2) =  real(INPUT_CCC(2,in_number))
    the_result(3) =  real(INPUT_CCC(3,in_number))
    the_result(4) = aimag(INPUT_CCC(1,in_number))
    the_result(5) = aimag(INPUT_CCC(2,in_number))
    the_result(6) = aimag(INPUT_CCC(3,in_number))
    return
end subroutine get_input_ccc




subroutine get_input_vvv(in_number, the_result)
    use FORTRAN_FOR_C
    implicit none
    integer          :: in_number
    double precision :: the_result(9)
    call CHECK_IN_NUMBER(in_number, "get_input_vvv(in_number, the_result)")
    the_result(1:9) = INPUT_VVV(1:9,in_number)
    return
end subroutine get_input_vvv




subroutine set_calculated_i(cal_number, the_result)
    use FORTRAN_FOR_C
    implicit none
    integer        :: cal_number
    integer        :: the_result
    call CHECK_CAL_DATA_TYPE(cal_number, I_TYPE, "set_calculated_i(cal_number, the_result)")
    CALCULATED_I(cal_number) = the_result
    if(CALCULATED_NUMBER .LT. cal_number) CALCULATED_NUMBER = cal_number
    return
end subroutine set_calculated_i




subroutine set_calculated_r(cal_number, the_result)
    use FORTRAN_FOR_C
    implicit none
    integer        :: cal_number
    double precision :: the_result
    call CHECK_CAL_DATA_TYPE(cal_number, R_TYPE, "set_calculated_r(cal_number, the_result)")
    CALCULATED_R(cal_number) = the_result
    if(CALCULATED_NUMBER .LT. cal_number) CALCULATED_NUMBER = cal_number
    return
end subroutine set_calculated_r




subroutine set_calculated_c(cal_number, the_result)
    use FORTRAN_FOR_C
    implicit none
    integer        :: cal_number
    double precision :: the_result(2)
    call CHECK_CAL_DATA_TYPE(cal_number, C_TYPE, "set_calculated_c(cal_number, the_result)")
    CALCULATED_C(cal_number) = CMPLX(the_result(1), the_result(2))
    if(CALCULATED_NUMBER .LT. cal_number) CALCULATED_NUMBER = cal_number
    return
end subroutine set_calculated_c




subroutine set_calculated_v(cal_number, the_result)
    use FORTRAN_FOR_C
    implicit none
    integer        :: cal_number
    double precision :: the_result(3)
    call CHECK_CAL_DATA_TYPE(cal_number, V_TYPE, "set_calculated_v(cal_number, the_result)")
    CALCULATED_V(1, cal_number) = the_result(1)
    CALCULATED_V(2, cal_number) = the_result(2)
    CALCULATED_V(3, cal_number) = the_result(3)
    if(CALCULATED_NUMBER .LT. cal_number) CALCULATED_NUMBER = cal_number
    return
end subroutine set_calculated_v




subroutine set_calculated_accuracy(cal_number, the_result)
    use FORTRAN_FOR_C
    implicit none
    integer        :: cal_number
    double precision :: the_result(3)
    call CHECK_CAL_NUMBER(cal_number, "set_calculated_accuracy(cal_number, the_result)")
    CALCULATED_ACCURACY(1, cal_number) = the_result(1)
    CALCULATED_ACCURACY(2, cal_number) = the_result(2)
    CALCULATED_ACCURACY(3, cal_number) = the_result(3)
    return
end subroutine set_calculated_accuracy




subroutine set_calculated_s(cal_number, the_string)
    use FORTRAN_FOR_C
    implicit none
    integer        :: cal_number, new_string_p, k
    character(len=MAX_NUMBER_OF_CHARACTERS_SSS):: the_string
    call CHECK_CAL_DATA_TYPE(cal_number, S_TYPE, "set_calculated_s(cal_number, the_string)")
    k = index(the_string, C_NULL_CHAR)
    if (k.le.0) then
            print*, 'This is in the FORTRAN-called-by-C routine '
            print*, "set_calculated_s(cal_number, the_string)"
            print*, 'for QUESTION_IDENTIFIER ',QUESTION_IDENTIFIER
            print*, 'for EXAMINEE_NUMBER ', EXAMINEE_NUMBER
            print*, 'for CAL_NUMBER ', CAL_NUMBER
            print*, 'Since the new string to be saved is not ended with "NULL",'
            print*, 'this run stopped. '
            STOP
    end if
    OUTPUT_SSS_USED = OUTPUT_SSS_USED + 1
    if (OUTPUT_SSS_USED.gt.MAX_NUMBER_OF_SSS_INPUT_TERMS) then
            print*, 'This is in the FORTRAN-called-by-C routine '
            print*, "set_calculated_s(cal_number, the_string)"
            print*, 'for QUESTION_IDENTIFIER ',QUESTION_IDENTIFIER
            print*, 'for EXAMINEE_NUMBER ', EXAMINEE_NUMBER
            print*, 'for CAL_NUMBER ', CAL_NUMBER
            print*, 'Since no more room to save the new string ,'
            print*, 'this run stopped. '
            print*, 'It is recommended to increase the constant '
            print*, 'MAX_NUMBER_OF_SSS_INPUT_TERMS in file PTC-*.f90 .'
            STOP
    end if
    new_string_p = OUTPUT_SSS_USED
    OUTPUT_SSS(new_string_p) = ' '
    OUTPUT_SSS(new_string_p)(1:k-1) = the_string(1:k-1)
    CALCULATED_I(cal_number) = new_string_p
    CALCULATED_POSITION(1, cal_number) = new_string_p
    CALCULATED_POSITION(2, cal_number) = new_string_p
    if(CALCULATED_NUMBER .LT. cal_number) CALCULATED_NUMBER = cal_number
    return
end subroutine set_calculated_s




subroutine set_calculated_mtx_bracket(cal_number, the_result)
    use FORTRAN_FOR_C
    implicit none
    integer        :: cal_number
    integer        :: the_result
    call CHECK_CAL_MATRIX_BRACKET(cal_number, the_result, "set_calculated_mtx_bracket(in_number, the_result)")
    CALCULATED_M(cal_number)%BRACE_INDEX = the_result
    return
end subroutine set_calculated_mtx_bracket




subroutine allocate_mtx_i(cal_number, sz)
    use FORTRAN_FOR_C
    implicit none
    integer        :: cal_number
    integer        :: sz(2)
    call CHECK_CAL_DATA_TYPE_AND_SIZE(cal_number, I_MATRIX_TYPE, sz, "allocate_mtx_i(cal_number, sz)")
    CALCULATED_M(cal_number)%SIZES = sz
    IF(ASSOCIATED(CALCULATED_M(cal_number)%INTEGER_M)) THEN
       DEALLOCATE(CALCULATED_M(cal_number)%INTEGER_M)
    END IF
    ALLOCATE(CALCULATED_M(cal_number)%INTEGER_M(sz(1),sz(2)))
    CALCULATED_M(cal_number)%INTEGER_M = 0
    if(CALCULATED_NUMBER .LT. cal_number) CALCULATED_NUMBER = cal_number
    return
end subroutine allocate_mtx_i




subroutine allocate_mtx_r(cal_number, sz)
    use FORTRAN_FOR_C
    implicit none
    integer        :: cal_number
    integer        :: sz(2)
    call CHECK_CAL_DATA_TYPE_AND_SIZE(cal_number, R_MATRIX_TYPE, sz, "allocate_mtx_r(cal_number, sz)")
    CALCULATED_M(cal_number)%SIZES = sz
    IF(ASSOCIATED(CALCULATED_M(cal_number)%REAL_M)) THEN
       DEALLOCATE(CALCULATED_M(cal_number)%REAL_M)
    END IF
    ALLOCATE(CALCULATED_M(cal_number)%REAL_M(sz(1),sz(2)))
    CALCULATED_M(cal_number)%REAL_M = 0.0d0
    if(CALCULATED_NUMBER .LT. cal_number) CALCULATED_NUMBER = cal_number
    return
end subroutine allocate_mtx_r




subroutine allocate_mtx_c(cal_number, sz)
    use FORTRAN_FOR_C
    implicit none
    integer        :: cal_number
    integer        :: sz(2)
    call CHECK_CAL_DATA_TYPE_AND_SIZE(cal_number, C_MATRIX_TYPE, sz, "allocate_mtx_c(cal_number, sz)")
    CALCULATED_M(cal_number)%SIZES = sz
    IF(ASSOCIATED(CALCULATED_M(cal_number)%COMPLEX_M)) THEN
       DEALLOCATE(CALCULATED_M(cal_number)%COMPLEX_M)
    END IF
    ALLOCATE(CALCULATED_M(cal_number)%COMPLEX_M(sz(1),sz(2)))
    CALCULATED_M(cal_number)%COMPLEX_M = cmplx(0.0d0, 0.0d0)
    if(CALCULATED_NUMBER .LT. cal_number) CALCULATED_NUMBER = cal_number
    return
end subroutine allocate_mtx_c




subroutine allocate_mtx_s(cal_number, sz)
    use FORTRAN_FOR_C
    implicit none
    integer        :: cal_number
    integer        :: sz(2)
    call CHECK_CAL_DATA_TYPE_AND_SIZE(cal_number, S_MATRIX_TYPE, sz, "allocate_mtx_s(cal_number, sz)")
    CALCULATED_M(cal_number)%SIZES = sz
    IF(ASSOCIATED(CALCULATED_M(cal_number)%STRING_M)) THEN
       DEALLOCATE(CALCULATED_M(cal_number)%STRING_M)
    END IF
    ALLOCATE(CALCULATED_M(cal_number)%STRING_M(sz(1),sz(2)))
    CALCULATED_M(cal_number)%STRING_M = ' '
    if(CALCULATED_NUMBER .LT. cal_number) CALCULATED_NUMBER = cal_number
    return
end subroutine allocate_mtx_s




subroutine set_calculated_mtx_element_i(cal_number, row_number, col_number, the_result)
    use FORTRAN_FOR_C
    implicit none
    integer        :: cal_number
    integer        :: row_number
    integer        :: col_number
    integer        :: the_result
    call CHECK_CAL_ARRAY_EXIST_AND_SIZE(cal_number, I_MATRIX_TYPE, row_number, col_number, &
                        &"set_calculated_mtx_element_i(cal_number, row_number, col_number, the_result)")
    CALCULATED_M(cal_number)%INTEGER_M(row_number,col_number) = the_result
    return
end subroutine set_calculated_mtx_element_i




subroutine set_calculated_mtx_element_r(cal_number, row_number, col_number, the_result)
    use FORTRAN_FOR_C
    implicit none
    integer        :: cal_number
    integer        :: row_number
    integer        :: col_number
    double precision :: the_result
    call CHECK_CAL_ARRAY_EXIST_AND_SIZE(cal_number, R_MATRIX_TYPE, row_number, col_number, &
                        &"set_calculated_mtx_element_r(cal_number, row_number, col_number, the_result)")
    CALCULATED_M(cal_number)%REAL_M(row_number,col_number) = the_result
    return
end subroutine set_calculated_mtx_element_r




subroutine set_calculated_mtx_element_c(cal_number, row_number, col_number, the_result)
    use FORTRAN_FOR_C
    implicit none
    integer        :: cal_number
    integer        :: row_number
    integer        :: col_number
    double precision :: the_result(2)
    call CHECK_CAL_ARRAY_EXIST_AND_SIZE(cal_number, C_MATRIX_TYPE, row_number, col_number, &
                        &"set_calculated_mtx_element_c(cal_number, row_number, col_number, the_result)")
    CALCULATED_M(cal_number)%COMPLEX_M(row_number,col_number) = cmplx(the_result(1), the_result(2))
    return
end subroutine set_calculated_mtx_element_c




subroutine set_calculated_mtx_element_s(cal_number, row_number, col_number, the_string)
    use FORTRAN_FOR_C
    implicit none
    integer        :: cal_number
    integer        :: row_number
    integer        :: col_number, k
    character(len=MAX_NUMBER_OF_CHARACTERS_SSS):: the_string
    call CHECK_CAL_ARRAY_EXIST_AND_SIZE(cal_number, S_MATRIX_TYPE, row_number, col_number, &
                        &"set_calculated_mtx_element_s(cal_number, row_number, col_number, the_result)")
    k = index(the_string, C_NULL_CHAR)
    if (k.le.0) then
            print*, 'This is in the FORTRAN-called-by-C routine '
            print*, "set_calculated_mtx_element_s(cal_number, row_number, col_number, the_string)"
            print*, 'for QUESTION_IDENTIFIER ',QUESTION_IDENTIFIER
            print*, 'for EXAMINEE_NUMBER ', EXAMINEE_NUMBER
            print*, 'for CAL_NUMBER ', cal_number
            print*, 'Since the new string to be saved is not ended with "NULL",'
            print*, 'this run stopped. '
            STOP
    end if
    CALCULATED_M(cal_number)%STRING_M(row_number,col_number) = ' '
    CALCULATED_M(cal_number)%STRING_M(row_number,col_number)(1:k-1) = the_string(1:k-1)
    return
end subroutine set_calculated_mtx_element_s




subroutine get_the_constant_value_of(the_string, the_value)
    use FORTRAN_FOR_C
    implicit none
    integer          :: k
    double precision :: the_value
    character(len=MAX_NUMBER_OF_CHARACTERS_SSS):: the_string
    k = index(the_string, C_NULL_CHAR)
    if (k.le.0) then
            print*, 'This is in the FORTRAN-called-by-C routine '
            print*, "get_the_constant_value_of(the_string, the_value)"
            print*, 'for QUESTION_IDENTIFIER ',QUESTION_IDENTIFIER
            print*, 'for EXAMINEE_NUMBER ', EXAMINEE_NUMBER
            print*, 'Since the string is not ended with "NULL": ', the_string
            print*, 'this run stopped. '
            STOP
    end if
    the_value = -1.0d99
    the_value = GET_CONSTANT_VALUE_OF(the_string(1:k-1))
    if (the_value .lt. -10.0d0) then
            print*, 'This is in the FORTRAN-called-by-C routine '
            print*, "get_the_constant_value_of(the_string, the_value)"
            print*, 'for QUESTION_IDENTIFIER ',QUESTION_IDENTIFIER
            print*, 'for EXAMINEE_NUMBER ', EXAMINEE_NUMBER
            print*, 'Since the returned value is not reasonable as ', the_value
            print*, 'this run stopped. '
            STOP
    end if
    return
end subroutine get_the_constant_value_of





SUBROUTINE SOLVE_THE_QUESTION(PRINT_OUT_NUMBER, AN_ACCESSORY_INTEGER)
  USE BASE_DATA
  USE RAND_INTERFACE
  USE CONSTANT_INTERFACE
  use FORTRAN_FOR_C
  IMPLICIT NONE
  INTEGER  ::  PRINT_OUT_NUMBER, AN_ACCESSORY_INTEGER
  QUESTION_IDENTIFIER=QUESTION_POSITION(0,QUESTION_NUMBER)
  call c_solve_the_question(QUESTION_IDENTIFIER)
  RETURN
END SUBROUTINE SOLVE_THE_QUESTION



