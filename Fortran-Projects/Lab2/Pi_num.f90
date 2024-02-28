    PROGRAM Pi_num
    ! ---------------------------------------------------------- !
    USE OMP_LIB
    IMPLICIT NONE 
    ! ---------------------------------------------------------- !
    INTEGER, PARAMETER :: NCPU = 4
    INTEGER, PARAMETER :: N = 1e8
    
    INTEGER            :: i
    REAL               :: dx, sum, f, pi, x, t0, t1 
    ! ---------------------------------------------------------- !

    
    dx  = 1.0/REAL(N)
    sum = 0.0

#if defined PARALLEL_1 || PARALLEL_2 || PARALLEL_3 || PARALLEL_4   
    CALL OMP_SET_NUM_THREADS(NCPU)
    !$WRITE(*,*) ' Program run with OpenMP directives '
    !$WRITE(*,*) ' Number of used CPU = ', NCPU
#endif 

    ! Compute Pi numerically

#ifdef PARALLEL_1

    t0 = OMP_GET_WTIME(t0)
    !$OMP PARALLEL DO
    ! all variables are shared, thus the x and f are continuosly overwritten
    ! within the do loop, therefore the result is arbitrarily WRONG! 
    DO i=1, N
        x = dx * (REAL(i)-0.5)
        f = 4./(1+x**2)
        sum = sum + f
    ENDDO
    !$OMP END PARALLEL DO
    t1 = OMP_GET_WTIME(t1)
    pi = sum * dx
    
#elif PARALLEL_2

    t0 = OMP_GET_WTIME(t0)
    !$OMP PARALLEL DO private(x, f)
    ! sum must be updated sequentially to obtain the final GLOBAL sum
    DO i=1, N
        x = dx * (REAL(i)-0.5)
        f = 4./(1+x**2)
        sum = sum + f
    ENDDO
    !$OMP END PARALLEL DO
    t1 = OMP_GET_WTIME(t1)
    pi = sum * dx
    
#elif PARALLEL_3

    t0 = OMP_GET_WTIME(t0)
    !$OMP PARALLEL DO private(x, f)
    DO i=1, N
        x = dx * (REAL(i)-0.5)
        f = 4./(1+x**2)
        !$OMP CRITICAL
        sum = sum + f
        !$OMP END CRITICAL
    ENDDO
    !$OMP END PARALLEL DO
    t1 = OMP_GET_WTIME(t1)
    pi = sum * dx
    
#elif PARALLEL_4

    t0 = OMP_GET_WTIME(t0)
    !$OMP PARALLEL DO private(x, f) reductioon(+:sum)
    DO i=1, N
        x = dx * (REAL(i)-0.5)
        f = 4./(1+x**2)
        sum = sum + f
    ENDDO
    !$OMP END PARALLEL DO
    t1 = OMP_GET_WTIME(t1)
    pi = sum * dx


#else

    CALL CPU_TIME(t0)
    DO i=1, N
        x = dx * (REAL(i)-0.5)
        f = 4./(1+x**2)
        sum = sum + f
    ENDDO
    pi = sum * dx
    CALL CPU_TIME(t1)

#endif 

    PRINT *, ' Computed pi = ', pi
    PRINT *, ' Error = ', ABS(pi-ACOS(-1.0))
    PRINT *, ' Computational time: ' t1-t0
    
END PROGRAM Pi_num