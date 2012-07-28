! message passing support
module messages

  use kinds
  implicit none

  private
  public :: nprocs, myrank, ioproc
  public :: init_messages, print_messages, end_messages
  public :: error, bcast

  integer :: nprocs ! number of message passing processes
  integer :: myrank ! my rank on communicator
  integer :: ioproc ! rank that does i/o
  integer :: comm   ! communicator

  interface bcast
     module procedure bcast_int
     module procedure bcast_dp
     module procedure bcast_sp
  end interface

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
    use io, only : stdout
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

  ! abort with error message
  subroutine error(msg)
    use io, only : stderr
    implicit none
    character(len=*),intent(in) :: msg
    integer :: ierr

    write(stderr,'(A)') trim(msg)
#if defined(_USE_MPI)
    call mpi_abort(comm,10,ierr)
#else
    stop 'fatal error'
#endif
  end subroutine error

  subroutine bcast_int(val)
    implicit none
    integer, intent(inout) :: val
#if defined(_USE_MPI)
    integer :: ierr
    include 'mpif.h'

    call mpi_bcast(val,1,MPI_INTEGER,ioproc,comm,ierr)
#endif
  end subroutine bcast_int

  subroutine bcast_dp(val)
    implicit none
    real(kind=dp), intent(inout) :: val
#if defined(_USE_MPI)
    integer :: ierr
    include 'mpif.h'

    call mpi_bcast(val,1,MPI_DOUBLE_PRECISION,ioproc,comm,ierr)
#endif
  end subroutine bcast_dp

  subroutine bcast_sp(val)
    implicit none
    real(kind=sp), intent(inout) :: val
#if defined(_USE_MPI)
    integer :: ierr
    include 'mpif.h'

    call mpi_bcast(val,1,MPI_REAL,ioproc,comm,ierr)
#endif
  end subroutine bcast_sp
end module messages
