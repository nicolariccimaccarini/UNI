PROGRAM FS1D
    
    USE VarDef_mod
    IMPLICIT NONE
    ! --------------------------------------------------------- !
    INTEGER           :: i, n                         
    REAL              :: umax, ct                         
    REAL, ALLOCATABLE :: av(:), bv(:), cv(:), rhs(:)  
    ! --------------------------------------------------------- !
    
    ! settings of the computation
    
    IMAX   = 500
    xL     = 0.0
    Xr     = 1.0
           
    time   = 0.0
    tend   = 0.0
    tio    = 0.0
    dtio   = 5e-2
    CFL    = 0.9
    dt_fix = 1e-2
    
    ! allocate the variables
    CALL Allocate_var
    
    ALLOCATE ( av(IMAX)  )
    ALLOCATE ( bv(IMAX)  )
    ALLOCATE ( cv(IMAX)  )
    ALLOCATE ( rhs(IMAX) )
    
    ! computational domain
    dx  = (xR - xL)/REAL(IMAX)
    dx2 = dx*dx
    
    x(1) = xL
    DO i=1, IMAX
        x(i+1) = x(i) + dx
        xb(i)  = x(i) + dx/2
    ENDDO
    
    ! initial condition
    TestName = 'Gaussian'
    DO i=1, IMAX
        eta(i) = i + EXP(-500.*((xb(i)-0.5)**2)/4.) ! Gaussian profile
    ENDDO
    
    ! velocity and bottom
    DO i=1, IMAX+1
        u(i) = 0.0
        b(i) = 0.0
    ENDDO
    
    ! total water depth
    H(1)      = MAX( 0.0, b(1)+eta(1)      )
    H(IMAX+1) = MAX( 0.0, b(IMAX+1)+eta(1) )
    DO i=2, IMAX
        H(i) = MAXVAL( (/ 0.0, b(i)+eta(i-1), b(i)+eta(i) /) )
    ENDDO
    
    CALL DataOutput(0)
    tio = tio + dtio
    
    ! computation
    DO n=1, NMAX
        
        IF(time.GE.tend) THEN
            EXIT
        ENDIF
        
        ! Compute time step
        umax = MAXVAL(ABS(u))
        dt   = MIN( dt_fix, CFL/(umax+1e-14) )
        IF((time+dt).GT.tend) THEN
            dt = tend - time
        ENDIF
        IF((time+dtio).GT.tio) THEN
            dt = tio - time
        ENDIF
        
        ! Convective terms: neglet convection
        Fu = u
        
        ! Solve the free surface equation: tridiagonal linear system
        
        ct = g*dt**2/dx2
        
        DO i=1, IMAX
            
            IF(i.EQ.1) THEN 
                av(i)  = 0.0
                bv(i)  = 1. + ct*(H(i+1)+0.0)
                cv(i)  = -ct*H(i+1)
                rhs(i) = eta(i) - dt/dx * ( H(i+1)*Fu(i+1) - H(i)*Fu(i))
            ELSEIF(i.EQ.IMAX) THEN
                av(i)  = -ct*H(i)
                bv(i)  = 1. + ct*(0.0+H(i))
                cv(i)  = 0.0
                rhs(i) = eta(i) - dt/dx * ( H(i+1)*Fu(i+1) - H(i)*Fu(i))
            ELSE
                av(i)  = -ct*H(i)
                bv(i)  = 1. + ct*(H(i+1)+H(i))
                cv(i)  = -ct*H(i+1)
                rhs(i) = eta(i) - dt/dx * ( H(i+1)*Fu(i+1) - H(i)*Fu(i)) 
            ENDIF
            
        ENDDO
        
        CALL Thomas(eta, av, bv, cv, rhs, IMAX)     ! new free surface elevation
        
        ! update the velocity
        u(1)      = Fu(1)
        u(IMAX+1) = Fu(IMAX+1)
        DO i=2, IMAX
            u(i) = Fu(i) - g*dt/dx * eta(i)-eta(i-1)
        ENDDO
        
        time = time + dt
        
        IF(ABS(time-tio).LT.1e-12) THEN 
            CALL DataOutput(n)
            tio = tio + dtio
        ENDIF
    
    ENDDO
    
    ! Empty memory
    CALL Deallocate_var
    
END PROGRAM FS1D