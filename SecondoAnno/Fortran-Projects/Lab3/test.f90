! ***************************************************** !  
!                        TEST 0                         !
! ***************************************************** !
SUBROUTINE HelloWorld(myrank, nCPU)
    ! ----------------------------------------------------------------------------- !
    IMPLICIT NONE
    INCLUDE 'mpif.h'
    ! ----------------------------------------------------------------------------- !
    
    INTEGER            :: iErr
    INTEGER            :: myrank
    INTEGER            :: nCPU
    
    INTEGER, PARAMETER :: test
    
    ! ----------------------------------------------------------------------------- !
    
    CALL MPI_BARRIER(MPI_COMM_WORLD,iErr)
    
    WRITE(*,*) ' I am processor ', myrank, ' of ', nCPU 
    
    CALL MPI_BARRIER(MPI_COMM_WORLD,iErr)
    
END SUBROUTINE HelloWorld
    
! ***************************************************** !  
!                        TEST 1                         !
! ***************************************************** !
SUBROUTINE PingPong(myrank, nCPU)
    ! ----------------------------------------------------------------------------- !
    IMPLICIT NONE
    INCLUDE 'mpif.h'
    ! ----------------------------------------------------------------------------- !
    
    INTEGER            :: myrank
    INTEGER            :: nCPU
    
    INTEGER            :: partner_rank, pingpong_count 
    INTEGER            :: MsgLenght, tag, status(MPI_STATUS_SIZE)  
    INTEGER            :: iErr, iter, MaxIter=20
    
    INTEGER, PARAMETER :: print_rank = 0
    
    ! ----------------------------------------------------------------------------- !
    
    CALL MPI_BARRIER(MPI_COMM_WORLD,iErr)
    
    IF(myrank.EQ.0) THEN 
        IF(nCPU.NE.2) THEN 
            WRITE(*,*) ' ERROR. PingPong test myst run only with 2 CPUs!'
            STOP
        ENDIF
    ENDIF 
    
    pingpongcount = 0
    partner_rank  = MOD(myrank+1,2)
    tag           = 10
    MsgLenght     = 1
    
    WIRTE(*,*) ' myrank is ', myrank, ' my partner_rank is ', partner_rank
    
    IF(myrank.EQ.print_rank) THEN
        t0 = MPI_WTIME()
    ENDIF
    
    DO iter=1, MaxIter
        
        ! iter=1 => 0 must send, 1 must recive
        ! iter=2 => 0 must recive, 1 must send 
        ! ...
        
        IF( myrank.EQ.MOD(pingpong_count,2) ) THEN
            pingpong_count = pingpong_count + 1
            CALL MPI_SEND(pingpong_count,MsgLenght,MPI_INTEGER,partner_rank,tag,MPI_COMM_WORLD,status,iErr)
            IF(myrank.EQ.print_rank) THEN
                WRITE(*,*) ' Rank ', myrank, ' sending pingpong_count = ', pingpong_count
            ENDIF
        ELSE
            CALL MPI_RECV(pingpong_count,MsgLenght,MPI_INTEGER,partner_rank,tag,MPI_COMM_WORLD,status,iErr)
            IF(myrank.EQ.print_rank) THEN
                WRITE(*,*) ' Rank ', myrank, ' reciving pingpong_count = ', pingpong_count
            ENDIF
        ENDIF
        
    ENDDO 
    
    IF(myrank.EQ.print_rank) THEN
        t1 = MPI_WTIME()
        WRITE(*,*) ' PingPong transfer time [micro s]= ', (t1-t0)/REAL(@*MaxIter) * 1e6
    ENDIF
    
    CALL MPI_BARRIER(MPI_COMM_WORLD,iErr)
    
END SUBROUTINE PingPong
 
    
! ***************************************************** !  
!                        TEST 2                         !
! ***************************************************** !
SUBROUTINE Ring(myrank, nCPU)
    ! ----------------------------------------------------------------------------- !
    IMPLICIT NONE
    INCLUDE 'mpif.h'
    ! ----------------------------------------------------------------------------- !
    
    INTEGER            :: myrank
    INTEGER            :: nCPU
    
    INTEGER            :: send_rank, recv_rank, token
    INTEGER            :: MsgLenght, tag, status(MPI_STATUS_SIZE)  
    INTEGER            :: iErr, iter, MaxIter=20
    
    INTEGER, PARAMETER :: print_rank = 0
    
    ! ----------------------------------------------------------------------------- !
    
    CALL MPI_BARRIER(MPI_COMM_WORLD,iErr)
    
    ! Set sending and reciving ranks 
    
    IF( myrank.EQ.0 ) THEN 
        recv_rank = nCPU - 1
        send_rank = myrank + 1
    ELSEIF( myrank.EQ.(nCPU-1) ) THEN
        recv_rank = myrank - 1
        send_rank = 0
    ELSE
        recv_rank = myrank - 1
        send_rank = myrank + 1
    ENDIF 
    
    MsgLenght = 1
    tag       = 15
    
    ! transfer the token among all processor
    IF(myrank.NE.0) THEN 
        CALL MPI_RECV(token,MsgLenght,MPI_INTEGER,recv_rank,tag,MPI_COMM_WORLD,status,iErr)
        WRITE(*,*) ' Rank ', myrank, ' recived token from ', recv_rank
    ELSE
        token = -1
    ENDIF
    
    ! send token
    CALL MPI_SEND(token,MsgLenght,MPI_INTEGER,send_rank,tag,MPI_COMM_WORLD,status,iErr)
    WRITE(*,*) ' Rank ', myrank, ' send token from ', send_rank
    
    ! close the ring
    IF(myrank.EQ.0) THEN 
        CALL MPI_RECV(token,MsgLenght,MPI_INTEGER,recv_rank,tag,MPI_COMM_WORLD,status,iErr)
        WRITE(*,*) ' Rank ', myrank, ' recived token from ', recv_rank
    ENDIF
    
END SUBROUTINE Ring   
    
    
! ***************************************************** !  
!                        TEST 3                         !
! ***************************************************** !
SUBROUTINE Deadlock(myrank, nCPU)
    ! ----------------------------------------------------------------------------- !
    IMPLICIT NONE
    INCLUDE 'mpif.h'
    ! ----------------------------------------------------------------------------- !
    
    INTEGER            :: myrank
    INTEGER            :: nCPU
    
    INTEGER            :: send_rank, recv_rank, token
    INTEGER            :: MsgLenght, tag, status(MPI_STATUS_SIZE)  
    INTEGER            :: iErr, iter, MaxIter=20
    
    INTEGER, PARAMETER :: do_deadlock = 0
    
    ! ----------------------------------------------------------------------------- !
    
    CALL MPI_BARRIER(MPI_COMM_WORLD,iErr)
    
    IF(myrank.EQ.0) THEN 
        IF(nCPU.NE.2) THEN 
            WRITE(*,*) ' ERROR. Deadlock test myst run only with 2 CPUs!'
            STOP
        ENDIF
    ENDIF 

    MsgLenght = 1
    tag       = 15
    token     = 1
    
    IF(do_deadlock==1) THEN
        IF(myrank.EQ.0) THEN 
            CALL MPI_RECV(token,MsgLenght,MPI_INTEGER,1,tag,MPI_COMM_WORLD,status,iErr)
            WRITE(*,*) ' Rank 0 recived token from 1'             
            CALL MPI_SEND(token,MsgLenght,MPI_INTEGER,1,tag,MPI_COMM_WORLD,status,iErr)
            WRITE(*,*) ' Rank 0 send token to 1' 
        ELSE
            CALL MPI_RECV(token,MsgLenght,MPI_INTEGER,0,tag,MPI_COMM_WORLD,status,iErr)
            WRITE(*,*) ' Rank 1 recived token from 0'
            CALL MPI_SEND(token,MsgLenght,MPI_INTEGER,0,tag,MPI_COMM_WORLD,status,iErr)
            WRITE(*,*) ' Rank 1 send token to 0' 
        ENDIF
    ELSE
        IF(myrank.EQ.0) THEN 
            CALL MPI_SEND(token,MsgLenght,MPI_INTEGER,1,tag,MPI_COMM_WORLD,status,iErr)
            WRITE(*,*) ' Rank 0 send token to 1' 
            CALL MPI_RECV(token,MsgLenght,MPI_INTEGER,1,tag,MPI_COMM_WORLD,status,iErr)
            WRITE(*,*) ' Rank 0 recived token from 1'             
        ELSE
            CALL MPI_RECV(token,MsgLenght,MPI_INTEGER,0,tag,MPI_COMM_WORLD,status,iErr)
            WRITE(*,*) ' Rank 1 recived token from 0'
            CALL MPI_SEND(token,MsgLenght,MPI_INTEGER,0,tag,MPI_COMM_WORLD,status,iErr)
            WRITE(*,*) ' Rank 1 send token to 0' 
        ENDIF
    ENDIF
    
END SUBROUTINE Deadlock
    
    
! ***************************************************** !  
!                        TEST 4                         !
! ***************************************************** !
SUBROUTINE Collective(myrank, nCPU)
    ! ----------------------------------------------------------------------------- !
    IMPLICIT NONE
    INCLUDE 'mpif.h'
    ! ----------------------------------------------------------------------------- !
    
    INTEGER            :: myrank
    INTEGER            :: nCPU
    !
    INTEGER            :: MsgLenght, tag, status(MPI_STATUS_SIZE), iErr 
    INTEGER            :: root, i 
    INTEGER, PARAMETER :: N = 4
    INTEGER            :: a(N), b(N) 
    ! ----------------------------------------------------------------------------- !
    
    CALL MPI_BARRIER(MPI_COMM_WORLD,iErr)
    
    ! We consider only an EVEN number of CPUs
    IF(MOD(nCPU,2).NE.0) THEN
        WRITE(*,*) ' ERROR. Collective test runs only with an EVEN number of CPUs!'
        STOP
    ENDIF   
    
    ! --------------------------------- !
    !         SCATTER - GAHTER          !
    ! --------------------------------- !
    
    root =0
    
    ! assemble the array to be distribuite
    IF(myrank.EQ.root) THEN 
        WRITE(*,*) ' Scatter-Gather'
        DO i=1, N
            a(i) = i
        ENDDO
        WRITE(*,*) a
    ENDIF
    
    ! number of elements that must be distribuited 
    MsgLenght = N/nCPU
    
    CALL MPI_SCATTER( a, MsgLenght, MPI_INTEGER, &    ! source information
                      b, MsgLenght, MPI_INTEGER, &    ! destination information
                      root, MPI_COMM_WORLD, iErr )    ! source CPU, communicator
                      
    b(1:MsgLenght) = b(1:MsgLenght) + 2*nCPU
    
    ! Gather the result in the array of rank=root
    CALL MPI_GATHER( b, MsgLenght, MPI_INTEGER, &     ! source information
                     a, MsgLenght, MPI_INTEGER, &     ! destination information
                     root, MPI_COMM_WORLD, iErr )     ! destination CPU, communicator
    
    IF(myrank.EQ.root) THEN
        WRITE(*,*) a
    ENDIF
    
    CALL MPI_BARRIER(MPI_COMM_WORLD,iErr)
    
    
    ! --------------------------------- !
    !             BROADCAST             !
    ! --------------------------------- !
    
    root =0
    
    ! assemble the array to be distribuite
    IF(myrank.EQ.root) THEN 
        WRITE(*,*) ' Broadcast'
        DO i=1, N
            a(i) = i
        ENDDO
        WRITE(*,*) a
    ENDIF
    
    ! number of elements that must be distribuited 
    MsgLenght = N/2
    
    CALL MPI_BROADCAST(a(1:MsgLenght), Msglenght, MPI_INTEGER, rott, MPI_COMM_WORLD, iErr)
    
    WRITE(*,*) ' Rank ', myrank, ' a2 = ', a(1:MsgLenght)
    
    CALL MPI_BARRIER(MPI_COMM_WORLD,iErr)
    
    
    ! --------------------------------- !
    !             ALLTOALL              !
    ! --------------------------------- !
    
    root =0
    
    ! assemble the array to be distribuite
    IF(myrank.EQ.root) THEN 
        WRITE(*,*) ' Alltoall'
    ENDIF
    DO i=1, N
        a(i) = N*myrank + i 
    ENDDO
    WRITE(*,*) ' Rank ', myrank, ' a = ', a 
    
    CALL MPI_BARRIER(MPI_COMM_WORLD,iErr)
    
    ! number of elements that must be distribuited 
    MsgLenght = N/nCPU
    
    CALL MPI_ALLTOALL( b, MsgLenght, MPI_INTEGER, &    ! source information
                       a, MsgLenght, MPI_INTEGER, &    ! destination information
                             MPI_COMM_WORLD, iErr )    ! communicator
    
    CALL MPI_BARRIER(MPI_COMM_WORLD,iErr)
    
    WRITE(*,*) ' Rank ', myrank, ' b = ', b
    
    
    ! --------------------------------- !
    !             REDUCTION             !
    ! --------------------------------- !
    
    root = 0
    
    IF(myrank.EQ.root) THEN 
        WRITE(*,*) ' Reduction'
    ENDIF
    
    DO i=1, N
        a(i) = N*myrank + i 
    ENDDO
    WRITE(*,*) ' Rank ', myrank, ' a = ', a 
    
    CALL MPI_BARRIER(MPI_COMM_WORLD,iErr)
    
    !CALL MPI_REDUCE(a, b, N, MPI_INTEGER, MPI_SUM, root, MPI_COMM_WORLD, iErr)
    CALL MPI_ALLREDUCE(a, b, N, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, iErr)
    
    IF(myrank.EQ.2) THEN 
        WRITE(*,*) ' the sum is ', b
    ENDIF   
                      
END SUBROUTINE Collective