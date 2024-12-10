PROGRAM LaplaceParallel
    
    ! ----------------------------------------------------------------------------------------- !
#if defined PARALLEL_1 || PARALLEL_2
    USE OMP_LIB
#endif    
    IMPLICIT NONE
    ! ----------------------------------------------------------------------------------------- !
    INTEGER, PARAMETER    :: N = 500               ! mesh size
    INTEGER, PARAMETER    :: maxIter = 5000        ! max number of iterations of Jacobi solver
    REAL,    PARAMETER    :: tol = 1e-7            ! tolerance for stopping Jacobi solver
    REAL,    PARAMETER    :: TBND = 100            ! Boundary condition
    
    REAL,    ALLOCATABLE  :: T(:,:), Tnew(:,:)     ! Current and next solution
    INTEGER               :: iter, i, j
    INTEGER               :: NPRCS, NCPU  
    REAL                  :: res, t0, t1  
    ! ----------------------------------------------------------------------------------------- !
    
#if defined PARALLEL_1 || PARALLEL_2
    !$ WRITE(*,*) ' PARALLEL simulation with OpenMP. ' 
    NPRCS = OMP_GET_NUM_PROCS()
    NCPU = MIN(6, NPRCS)
    CALL OMP_SET_NUM_THREADS(NCPU)
    !$ WRITE(*,*) ' Total number of avaible CPUs: ', NPRCS
    !$ WRITE(*,*) ' Total number of used CPUs:    ', NCPU
#else
    WRITE(*,*) 'SERIAL simulation. '
#endif
    
    ! Allocate variables 
    ALLOCATE ( T(0:N+1,0:N+1) ) 
    ALLOCATE ( Tnew(0:N+1,0:N+1) )
    
    ! Initialize variables
#if defined PARALLEL_1 || PARALLEL_2

!$OMP PARALLEL WORKSHARE
    T(0:N,0:N) = 0
    T(N+1,1:N) = (/ (i,i=1,N /) * (TBND/REAL(N+1))  ! right BC
    T(1:N,N+1) = (/ (i,i=1,N /) * (TBND/REAL(N+1))  ! top BC
    
    Tnew(:,:) = T(:,:)
!$OMP END PARALLEL WORKSHARE

#else
    T(0:N,0:N) = 0
    T(N+1,1:N) = (/ (i,i=1,N /) * (TBND/REAL(N+1))  ! right BC
    T(1:N,N+1) = (/ (i,i=1,N /) * (TBND/REAL(N+1))  ! top BC
    
    Tnew(:,:) = T(:,:)
#endif
    
    ! Jacobi method for solving the Laplace equation
#if defined PARALLEL_1
    
    t0 = OMP_GET_WTIME()
    iter = 0
    res = 1e12
    
!$OMP PARALLEL DO REDUCTION(max:res)
    DO WHILE( res.GT.tol AND iter.LE.maxIter )
        
        iter = iter + 1 
        res = 0.
        DO j=1, N
            DO i=1, N
                Tnew(i, j) = 0.25*( T(i-1,j) + T(i+1,j) + T(i,j-1) + T(i,j+1) )
                res        = MAX( res, ABS(Tnew(i,j)-T(i,j)) )
            ENDDO
        ENDDO
!$OMP END PARALLEL DO
        
        ! overwrite current solution
        T = Tnew
        
        IF( MOD(iter,100).EQ.0 ) THEN 
            PRINT *, ' [iter,res] = ', iter, res
        ENDIF
        
    ENDDO
    
    t1 = OMP_GET_WTIME()
    
#elif defined PARALLEL_2
    
    t0 = OMP_GET_WTIME()
    iter = 0
    res = 1e12
    
!$OMP PARALLEL
!$OMP BARRIER
    DO WHILE( res.GT.tol AND iter.LE.maxIter )
     
!$OMP SINGLE 
        iter = iter + 1 
        res = 0.
!$OMP END SINGLE 

!$OMP BARRIER

!$OMP DO collapse(2) reduction(max:res)         
        DO j=1, N
            DO i=1, N
                Tnew(i, j) = 0.25*( T(i-1,j) + T(i+1,j) + T(i,j-1) + T(i,j+1) )
                res        = MAX( res, ABS(Tnew(i,j)-T(i,j)) )
            ENDDO
        ENDDO
!$OMP END DO
        
        ! overwrite current solution
!$OMP SINGLE 
        T = Tnew
        IF( MOD(iter,100).EQ.0 ) THEN 
            PRINT *, ' [iter,res] = ', iter, res
        ENDIF
!$OMP END SINGLE         
            
    ENDDO
!$OMP END PARALLEL
    
    t1 = OMP_GET_WTIME()

#else    

    CALL CPU_TIME(t0)

    iter = 0
    res = 1e12
    
    DO WHILE( res.GT.tol AND iter.LE.maxIter )
        
        iter = iter + 1 
        res = 0.
        DO j=1, N
            DO i=1, N
                Tnew(i, j) = 0.25*( T(i-1,j) + T(i+1,j) + T(i,j-1) + T(i,j+1) )
                res        = MAX( res, ABS(Tnew(i,j)-T(i,j)) )
            ENDDO
        ENDDO
        
        ! overwrite current solution
        T = Tnew
        
        IF( MOD(iter,100).EQ.0 ) THEN 
            PRINT *, ' [iter,res] = ', iter, res
        ENDIF
        
    ENDDO
    
    CALL CPU_TIME(t1)
#endif 
    
    ! Write the final result
    OPEN(UNIT=100,FILE='Laplace2D.dat',STATUS='UKNOWN',ACTION='WRITE')
    WRITE(100,*) N
    DO j=1, N
        DO i=1, N
            WRITE(100,*) T(i,j)
        ENDDO
    ENDDO
    CLOSE(100)
    
    WRITE(*,*) ' Total computational time: ', t1-t0
    
END PROGRAM LaplaceParallel