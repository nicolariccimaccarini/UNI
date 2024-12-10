PROGRAM MPI_head
    ! ----------------------------------------------------------------------------- !
    IMPLICIT NONE
    INCLUDE 'mpif.h'
    ! ----------------------------------------------------------------------------- !
    
    INTEGER            :: iErr
    INTEGER            :: myrank
    INTEGER            :: nCPU
    
    INTEGER, PARAMETER :: test=2
    
    ! ----------------------------------------------------------------------------- !
    
    ! ============================================================================= !
    !                           MPI INITIALIZATION
    ! ============================================================================= !
    
    CALL MPI_INIT(iErr)
    CALL MPI_COMM_RANK(MPI_COMM_WORLD,myrank,iErr)
    CALL MPI_COMM_SIZE(MPI_COMM_WORLD,nCPU,iErr)
    
    
    WRITE(*,*) ' MPI test is running. '

    ! ============================================================================= !
    !                           MPI COMPUTATION
    ! ============================================================================= !
    
    SELECT CASE(test)
        CASE(0)
            IF(myrank.EQ.0) THEN 
                WRITE(*,*) ' Hello World test is choosen. '
            ENDIF
            CALL HelloWorld(myrank, nCPU)
            
        CASE(1)
            IF(myrank.EQ.0) THEN 
                WRITE(*,*) ' PingPong test is choosen. '
            ENDIF
            CALL PingPong(myrank, nCPU)
            
        CASE(2)
            IF(myrank.EQ.0) THEN 
                WRITE(*,*) ' Ring test is choosen. '
            ENDIF
            CALL Ring(myrank, nCPU)
            
        CASE(2)
            IF(myrank.EQ.0) THEN 
                WRITE(*,*) ' Deadlock test is choosen. '
            ENDIF
            CALL Deadlock(myrank, nCPU)
                
        CASE(2)
            IF(myrank.EQ.0) THEN 
                WRITE(*,*) ' Collective test is choosen. '
            ENDIF
            CALL Collective(myrank, nCPU)            
            
        CASE DEFAULT 
            IF(myrank.EQ.0) THEN 
                WRITE(*,*) ' Unknown test is choosen. '
                WRITE(*,*) ' Program will be terminated now (!). '
                STOP
            ENDIF 
    END SELECT
    
    ! ============================================================================= !
    !                           MPI FINALIZATION
    ! ============================================================================= !
    
    CALL MPI_FINALIZE()
    
    IF(myrank.EQ.0) THEN
        WRITE(*,*) ' MPI test has finished. '
    ENDIF
    
END PROGRAM MPI_head 