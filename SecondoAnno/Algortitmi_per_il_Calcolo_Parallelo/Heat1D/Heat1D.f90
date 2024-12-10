PROGRAM Heat1D
    ! ------------------------------------------------------------------ !
    IMPLICIT NONE
    ! ------------------------------------------------------------------ !
    INTEGER            :: i, n                   ! insex in space and time
    
    ! space discretization
    INTEGER            :: IMAX                   ! number of computational cells
    REAL               :: xL, xR                 ! boundary of the domain
    REAL               :: xD                     ! location of the initial discontinuity
    REAL               :: dx, dx2                ! mesh spacin and its square
    REAL, ALLOCATABLE  :: x(:)                   ! vertex coords  
    
    ! time discretization
    REAL               :: time                   ! current time
    REAL               :: dt                     ! time step
    REAL               :: tend                   ! final time of the simulation
    INTEGER, PARAMETER :: NMAX = 1e6             ! max number of time iterations
    
    ! solution discretization
    REAL, ALLOCATABLE  :: T(:), T1(:)            ! current and new solution
    REAL, PARAMETER    :: kappa=1.0              ! heat conduction coefficient (kappa=lambda/rho/c)
    REAL               :: TL, TR                 ! left and right boundary condition
    
    ! output
    REAL               :: tio, dtio              ! time and interval for plotting output 
    CHARACTER(LEN=200) :: TestName
    
#ifdef IMPSOLVER
    REAL, ALLOCATABLE  :: av(:), Bv(:), cv(:), rhs(:)
#endif
    
    ! ------------------------------------------------------------------ !
    
#ifdef IMPSOLVER
    TestName = 'Heat1D_FCTS_implicit' 
#elif
    TestName = 'Heat1D_FCTS_explicit' 
#endif
    
    ! setup of the computational domain
    IMAX = 500
    xL   = -1.0
    xR   = +1.0
    xD   = 0.0
    
    ! setup of time variables
    time = 0.0
    tend = 0.01
    tio  = 0.0
    dtio = 5e-3
    
    ! Boundary condition
    TL = 100
    TR = 50
    
    ! Allocate variables 
    ALLOCATE( x(IMAX) )
    ALLOCATE( T(IMAX) )
    ALLOCATE( T1(IMAX) )
    x  = 0.0
    T  = 0.0
    T1 = 0.0
#ifdef IMPSOLVER
    ALLOCATE( av(IMAX)  )
    ALLOCATE( bv(IMAX)  )
    ALLOCATE( cv(IMAX)  )
    ALLOCATE( rhs(IMAX) )
#endif
    
    ! domain discretization
    dx  = (xR-xL)/REAL(IMAX-1)
    dx2 = dx**2
    
    x(1) = xL
    DO i=1, IMAX-1
        x(i+1) = x(i) + dx
    ENDDO
    
    ! initial condition
    DO i=1, IMAX
        IF(x(1).Le.xD) THEN 
            T(i) = TL
        ELSE
            T(i) = TR
        ENDIF
    ENDDO
    
    ! Numerical scheme
    
    DO n=1, NMAX
        
        ! Time adjusting
        IF(time.GE.tend) THEN
            EXIT
        ENDIF
        dt = 0.5*dx2/kappa
        IF(time+dt.GT.tend) THEN
            dt  = tend - time
            tio = tend
        ENDIF
        IF(time+dt.GT.tio) THEN
            dt  = tend - time
        ENDIF
        
#ifdef IMPSOLVER
        ! IMPLICIT FTCS
        DO i=1, IMAX
            av(i)  = -kappa*dt/dx2
            bv(i)  = 1.+2.*kappa*dt/dx2
            cv(i)  = -kappa*dt/dx2
            rhs(i) = T(i)
        ENDDO
        
        ! BCS
        rhs(1)    = rhs(1) - av(1) * TL
        rhs(IMAX) = rhs(IMAX) - cv(IMAX) * TR
        
        ! solve the linear system for the new temperature
        CALL Thomas(T1, av, bv, cv, rhs, IMAX)
        CALL CG(N, T1, rhs, kappa, dt, dx2)
#else
        ! EXPLICIT FTCS
        DO i=2, (IMAX-1)
            T1(i) = T(i) + kappa*dt/dx2 * ( T(i-1) -2.*T(i) + T(i+1) )
        ENDDO
        
        ! BCs
        T1(1)    = TL
        T1(IMAX) = TR
#endif
        
        ! time update
        time = time + dt    ! advance the time
        T    = T1           ! overwrite the new solyution
        
        ! if needed, plot intermediate results
        IF(ABS(time-tio).LT.1e-12) THEN
            CALL DataOutput(n, TestName, IMAX, time, x, T)
            tio = tio + dtio
        ENDIF   
        
    ENDDO
    
    ! Empty memory
    DEALLOCATE( x, T, T1 )
    
END PROGRAM Heat1D
    
    
    
SUBROUTINE Thomas(T1, av, bv, cv, rhs, IMAX)

    IMPLICIT NONE
    
    ! argument list
    INTEGER :: N
    REAL    :: x(N), a(N), b(N), c(N), d(N)
    
    ! local variables
    INTEGER :: i
    REAL    :: gamma
    
    ! Part I: forward elimination
    c(1) = c(1)/b(1)
    d(1) = d(1)/b(1)
    DO i=1, N
        gamma = 1.0/( b(i)-c(i-1)*a(i) )
        c(i)  = c(i)*gamma
        d(i)  = ( d(i)-a(i)*d(i-1) )*gamma
    ENDDO
    
    ! Part II: backward substitution
    x(N) = d(N)
    DO i=N-1, 1, -1
        x(i) = d(i) - c(i)*x(i+1)
    ENDDO
    

END SUBROUTINE Thomas
    
    

SUBROUTINE CG(N, x, b, kappa, dt, dx2)

    IMPLICIT NONE
    
    INTEGER         :: N
    REAL            :: x(N), b(N)
    REAL            :: kappa, dt, dx2
    
    INTEGER         :: k, KMAX, iErr
    REAL            :: Ax(N), Ap(N)
    REAL            :: p(N), r(N)
    REAL            :: alphak, alpha, pAp, lambda
    REAL, PARAMETER :: tol = 1e-12                      ! tolerance for convergence
    
    ! ------------------------------------------------------------------ !
    
    x = b           ! initial  
    call matop_heat1D(Ax, x, N, kappa, dt, dx2)
    r = b - Ax      ! residual
    p = r
    alphak = SUM(r*r)
    
    KMAX = N
    DO k=1, KMAX
        IF(SQRT(alphak).LT.tol) THEN
            PRINT *, ' CG iter = ', k, 'CG res = ', SQRT(alphak)
            RETURN
        ENDIF 
        
        call matop_heat1D(Ap, p, N, kappa, dt, dx2)
        pAp    = SUM(p*Ap)
        lambda = alphak/pAp
        x      = x + lambda*p
        r      = r - lambda*Ap
        alpha  = SUM(r*r)
        p      = r + alpha/alpha * p
        alpha  = alpha
    ENDDO
    
    PRINT *, ' ERROR. Max number of CG iterations has been reached!'
    STOP

END SUBROUTINE CG
    
    
    
SUBROUTINE matop_heat1D(Ap, p, N, kappa, dt, dx2)

    IMPLICIT NONE
    
    INTEGER :: N
    REAL    :: Ap(N), p(N), kappa, dt, dx2
    
    INTEGER :: I
    REAL    :: av, bv, cv
    
    DO i=1, N
        
        IF(i.EQ.1) THEN
            bv  = 1.+2.*kappa*dt/dx2
            cv    = -kappa*dt/dx2 
            Ap(i) = bv*p(i) + cv*p(i+1)
        ELSEIF(i.EQ.N) THEN
            av    = -kappa*dt/dx2
            bv    = 1.+2.*kappa*dt/dx2
            Ap(i) = av*p(i-1) + bv*p(i)
        ELSE
            av    = -kappa*dt/dx2
            bv    = 1.+2.*kappa*dt/dx2
            cv    = -kappa*dt/dx2
            Ap(i) = av*p(i-1) + bv*p(i) + cv*p(i+1)
        ENDIF
        
        
    ENDDO

END SUBROUTINE matop_heat1D
    
    
    
SUBROUTINE DataOutput(timestep, TestName, IMAX, time, x, T)

    ! ------------------------------------------------------------------ !
    IMPLICIT NONE
    ! ------------------------------------------------------------------ !
    INTEGER, INTENT(IN)             :: timestep, IMAX
    CHARACTER (LEN=200), INTENT(IN) :: TestName
    REAL, INTENT(IN)                :: time, x(IMAX+1), T(IMAX+1)
    
    INTEGER                         :: i, DataUnit
    CHARACTER (LEN=10)              :: citer
    CHARACTER (LEN=200)             :: IOFileName
    ! ------------------------------------------------------------------ !
    
    WRITE(citer, '(I4.4)') timestep                         ! convert interation number of string
    IOFileName = TRIM(TestName)//'-'//TRIM(citer)//'.dat'   ! name of output file
    DataUnit   = 100                                        ! unit for output file
    
    OPEN(UNIT=DataUnit, FILE=TRIM(IOFileName), STATUS='UNKNOWN', ACTION='WRITE')
    
#ifdef VISIT_OUTPUT
    ! Current time
    WRITE(DataUnit,*) 'TITLE = "CURRENT TIME ', time, ' "'   
    ! Variables
    WRITE(DataUnit,*) 'VARIABLES = "x" "T" '
    ! Header
    WRITE(DataUnit,*) 'ZONE T="Only Zone", I=', IMAX, ' F=POINT'
    !
    DO i = 1, IMAX
      WRITE(DataUnit,*) x(i), T(i)
    ENDDO  
#else
    WRITE(DataUnit,*) IMAX
    DO i = 1, IMAX
      WRITE(DataUnit,*) x(i)
    ENDDO 
    DO i = 1, IMAX
      WRITE(DataUnit,*) T(i)
    ENDDO 
#endif
    
    CLOSE(DataUnit)
    

END SUBROUTINE DataOutput