MODULE VarDef_mod
    
    IMPLICIT NONE
    PUBLIC 
    
    ! ======================================= !
    !               GEOMETRY                  !
    ! ======================================= !
    
    INTEGER            :: IMAX               ! maximum number of cells
    REAL               :: xL, xR             ! domain borders
    REAL, ALLOCATABLE  :: x(:)               ! interface coords
    REAL, ALLOCATABLE  :: xb(:)              ! baricentary coords
    REAL               :: dx, dx2            ! mesh spacing and its square  
    
    ! ======================================= !
    !               DISCRETIZATION            !
    ! ======================================= !
    
    REAL               :: CFL
    REAL, PARAMETER    :: g=9.81             ! grtavity acceleration
                       
    REAL, ALLOCATABLE  :: u(:)               ! interface velocity
    REAL, ALLOCATABLE  :: Fu(:)              ! convection operator
    REAL, ALLOCATABLE  :: eta(:)             ! free surface elevation (cell centered)
    REAL, ALLOCATABLE  :: H(:)               ! water depht
    REAL, ALLOCATABLE  :: b(:)               ! bottom bethimetry
                       
    REAL               :: time               ! current time
    REAL               :: dt                 ! time step
    REAL               :: dt_fix             ! user-defined time step
    REAL               :: tend               ! final time of the simulation  
    INTEGER, PARAMETER :: NMAX=1e6
    
    
    ! ======================================= !
    !               INPUT/OUTPUT              !
    ! ======================================= !
    
    CHARACTER(LEN=200) :: Test name
    REAL               :: tio, dtio
    
    CONTAINS
    
SUBROUTINE Allocate_var

    ALLOCATE( x(IMAX+1), xb(IMAX) )
    x  = 0.0
    xb = 0.0
    
    ALLOCATE( u(IMAX+1), Fu(IMAX+1) )
    ALLOCATE( H(IMAX+1), b(IMAX+1)  )
    ALLOCATE( eta(IMAX)             )
    u   = 0.0
    Fu  = 0.0
    H   = 0.0
    b   = 0.0
    eta = 0.0

END SUBROUTINE Allocate_var


SUBROUTINE Dellocate_var

    DEALLOCATE( x, xb )
    
    DEALLOCATE( u, Fu )
    DEALLOCATE( H, b  )
    DEALLOCATE( eta   )

END SUBROUTINE Dellocate_var

    
END MODULE VarDef_mod