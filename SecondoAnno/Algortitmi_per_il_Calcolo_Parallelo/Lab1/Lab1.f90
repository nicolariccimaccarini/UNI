PROGRAM Lab1
    
    ! ----------------------------------------------------------------- !
    USE vardef_mod
    USE StatisticOp_mod
    IMPLICIT NONE 
    INTEGER :: i, i_max, i_min
    REAL    :: x_av, sigma, x_max, x_min
    
    ! ----------------------------------------------------------------- !
    
    ! Initialize inoput data vector x
    CALL RANDOM_NUMBER(x)
    WRITE(*,*) ' Initial value of x: '
    DO i=1, N
        WRITE(*,'(f12.7)') x(i)
    ENDDO
    
    ! computing the average
    x_av = Average(x,N)
    WRITE(*,*) ' '
    WRITE(*,'a25,2x,f14.10') ' Average value = ', x_av
    
    ! Compute the standard deviation
    sigma = StandardDeviation(x,N)
    WRITE(*,*) ' '
    WRITE(*,'a25,2x,f14.10') ' Standard Deviation = ', sigma
    
    ! Find max and minimum values and location in array x
    CALL FindMaxMin(x,N,x_max,x_min,i_max,i_min)
    WRITE(*,*) ' '
    WRITE(*,*) ' Max value    = ', x_max
    WRITE(*,*) ' Min value    = ', x_min
    WRITE(*,*) ' Max location = ', i_max
    WRITE(*,*) ' Min location = ', i_min
    
    ! Order incresingly the entries of x
    CALL OrderPlus(x,N)
    WRITE(*,*) ' Ordered value of x: '
    DO i=1, N
        WRITE(*,'(f14.10)') x(i)
    ENDDO
    
END PROGRAM