        !COMPILER-GENERATED INTERFACE MODULE: Fri Feb 23 15:32:23 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DATAOUTPUT__genmod
          INTERFACE 
            SUBROUTINE DATAOUTPUT(TIMESTEP,TESTNAME,IMAX,TIME,X,T)
              INTEGER(KIND=4), INTENT(IN) :: IMAX
              INTEGER(KIND=4), INTENT(IN) :: TIMESTEP
              CHARACTER(LEN=200), INTENT(IN) :: TESTNAME
              REAL(KIND=8), INTENT(IN) :: TIME
              REAL(KIND=8), INTENT(IN) :: X(IMAX+1)
              REAL(KIND=8), INTENT(IN) :: T(IMAX+1)
            END SUBROUTINE DATAOUTPUT
          END INTERFACE 
        END MODULE DATAOUTPUT__genmod
