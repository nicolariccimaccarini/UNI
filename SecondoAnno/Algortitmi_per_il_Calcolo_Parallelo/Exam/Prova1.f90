#define PARALLEL

program heat_equation
    implicit none
#if defined PARALLEL
        include 'mpif.h'
#endif
    
    integer, parameter :: nx = 100 ! number of grid points in x-direction
    integer, parameter :: ny = 100 ! number of grid points in y-direction
    integer, parameter :: nt = 100 ! number of time steps
    real, parameter :: dx = 0.1 ! grid spacing in x-direction
    real, parameter :: dy = 0.1 ! grid spacing in y-direction
    real, parameter :: dt = 0.01 ! time step size
    real, parameter :: alpha = 0.1 ! thermal diffusivity
    
    real, dimension(0:nx, 0:ny) :: u ! temperature field
    real, dimension(0:nx, 0:ny) :: u_new ! updated temperature field

    real :: t0, t1 ! CPU time
    
    integer :: i, j, t

#if defined PARALLEL
    integer :: rank, size, ierr
    integer :: nx_local, ny_local
    integer :: i_start, i_end, j_start, j_end
    real, dimension(:,:), allocatable :: u_local, u_new_local

    call MPI_INIT(ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)

    ! Determine local grid size
    nx_local = nx / size
    ny_local = ny

    ! Determine local grid indices
    i_start = rank * nx_local + 1
    i_end = (rank + 1) * nx_local
    j_start = 1
    j_end = ny_local

    ! Allocate memory for local grids
    allocate(u_local(0:nx_local, 0:ny_local))
    allocate(u_new_local(0:nx_local, 0:ny_local))

    ! Initialize temperature field
    u_local = 0.0

    ! Set initial condition for local grid
    do i = i_start, i_end
        do j = j_start, j_end
            u_local(i, j) = sin(i*dx) * sin(j*dy)
        end do
    end do

    ! Time integration
    do t = 1, nt
        ! Update temperature field for local grid
        do i = i_start, i_end
            do j = j_start, j_end
                u_new_local(i, j) = u_local(i, j) + alpha * dt * (u_local(i+1, j) - 2.0*u_local(i, j) + u_local(i-1, j)) / dx**2 &
                                            + alpha * dt * (u_local(i, j+1) - 2.0*u_local(i, j) + u_local(i, j-1)) / dy**2
            end do
        end do

        ! Update temperature field for the next time step for local grid
        u_local = u_new_local

        ! Synchronize local grids
        call MPI_ALLGATHER(u_local, nx_local, ny_local, u, nx_local, ny_local, MPI_COMM_WORLD, ierr)
    end do

    call MPI_FINALIZE(ierr)


#else
    CALL CPU_TIME(t0)
    
    ! Initialize temperature field
    u = 0.0
    
    ! Set initial condition
    do i = 1, nx-1
        do j = 1, ny-1
            u(i, j) = sin(i*dx) * sin(j*dy)
        end do
    end do
    
    ! Time integration
    do t = 1, nt
        ! Update temperature field
        do i = 1, nx-1
            do j = 1, ny-1
                u_new(i, j) = u(i, j) + alpha * dt * (u(i+1, j) - 2.0*u(i, j) + u(i-1, j)) / dx**2 &
                                            + alpha * dt * (u(i, j+1) - 2.0*u(i, j) + u(i, j-1)) / dy**2
            end do
        end do
        
        ! Update temperature field for the next time step
        u = u_new
    end do

    CALL CPU_TIME(t1)
    
    ! Output the final temperature field
    do i = 1, nx-1
        do j = 1, ny-1
            print *, i*dx, j*dy, u(i, j)
        end do
    end do

    write(*, *) "CPU time = ", t1 - t0, " seconds"
#endif

end program heat_equation