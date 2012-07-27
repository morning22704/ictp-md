! message passing support
module messages

  use io
  implicit none

  private
  public :: nprocs, myrank, ioproc
  public :: init_messages, print_messages, end_messages

  integer :: nprocs ! number of message passing processes
  integer :: myrank ! my rank on communicator
  integer :: ioproc ! rank that does i/o
  integer :: comm   ! communicator

contains

  ! set up message passing environment
  subroutine init_messages
    implicit none

#if defined(_USE_MPI)
    include 'mpif.h'
    integer :: ierr
#endif

    ! set defaults for non-mpi
    nprocs = 1
    myrank = 0
    ioproc = 0
    comm   = 0

#if defined(_USE_MPI)
    comm=MPI_COMM_WORLD
    call mpi_init(ierr)
    call mpi_comm_size(comm,nprocs,ierr)
    call mpi_comm_rank(comm,myrank,ierr)
#endif
  end subroutine init_messages

  ! print message passing header
  subroutine print_messages
    implicit none

    if (myrank == ioproc) then
       write(stdout,*) 'Number of message passing processes :    ', nprocs
    end if
  end subroutine print_messages

  ! close message passing environment
  subroutine end_messages
    implicit none
    integer :: ierr
#if defined(_USE_MPI)
    call mpi_finalize(ierr)
#endif
  end subroutine end_messages
end module messages
