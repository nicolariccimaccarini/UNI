PROGRAM ACP2
    USE OMP_LIB
    IMPLICIT NONE 
    ! ---------------------------------------------------------- !    
    INTEGER, PARAMETER :: N    = 500
    INTEGER, PARAMETER :: NCPU = 2
    !
    INTEGER            :: i, j, k
    INTEGER            :: NPRCS
    REAL               :: t0, t1, dt, norm
    REAL, ALLOCATABLE  :: A(:,:), B(:,:), C(:,:)
    ! ---------------------------------------------------------- !
    
    !$ WRITE(*,*) ' Program compiled with OpenMP directives '
    
    NPRCS = OMP_GET_NUM_PROCS()
    CALL OMP_CALL_NUM_THREADS(NCPU)
    
    !$ WIRTE(*,*) ' Number of avaible CPU = ', NPRCS
    !$ WIRTE(*,*) ' Number of used CPU    = ', NCPU
    
    CALL RANDOM_SEED()  ! initialize random generator
    
    ALLOCATE( A(N,N) )
    ALLOCATE( B(N,N) )
    ALLOCATE( C(N,N) )
    
    ! =====================================================================!
    !$OMP PARALLEL
    ! =====================================================================!
    
    !$OMP DO collapse(2)
    DO j=1, N
        DO i=1, N
            CALL RANDOM_NUMBER(A(i,j))
            CALL RANDOM_NUMBER(B(i,j))
            C(i,j) = 0.0
        ENDDO
    ENDDO
    !$OMP END DO
    
    ! matrix multiplication C=A*B
    !CALL CPU_TIME(t0)
    t0 = OMP_GET_WTIME(t0)
    !$OMP DO collapse(2)
    DO i=1, N
        DO j=1, N
            C(i,j) = DOT_PRODUCT(A(i,:), B(:,j))
        ENDDO
    ENDDO
    !$OMP END DO
    !CALL CPU_TIME(t1)
    t1 = OMP_GET_WTIME(t1)
    
    !$OMP SINGLE 
    PRINT *, ' Computational time: ', t1-t0
    !$OMP END SINGLE 
    
    ! Print the error norm of the matrix C
    norm = 0.0
    !$OMP DO collapse(2) reduction(+:norm)
    DO j=1, N
        DO i=1, N
            norm = norm + A(i,j)**2
        ENDDO
    ENDDO
    !$OMP END DO
    PRINT *, ' norm^2 = ', norm   ! only for debugging purposes
    
    ! EXPLICIT SYNCHRONIZATION:
    ! all threads MUST wait for each others before advancing
    !$OMP BARRIER   
    
    !$OMP SINGLE
    PRINT *, ' Forbenius norm of C: ', SQRT(norm)
    !$OMP END SINGLE
    
    ! =====================================================================!
    !$OMP END PARALLEL
    ! =====================================================================!
    
    CONTINUE
END PROGRAM 