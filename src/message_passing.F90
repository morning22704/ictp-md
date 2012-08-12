!> Wrapper module to provide message passing support.
!!
!! Rather than using calls directly to the message passing
!! library, we provide an abstract interface with adaptations
!! to specific data types of the application. This should also
!! make transparent to the rest of the application whether 
!! message passing support is compiled in or not.
module message_passing

  use kinds
  use memory, only: adjust_mem
  implicit none

  private
  integer :: comm   !< Communicator used for message passing
  integer :: nprocs !< Number of message passing processes on communicator
  integer :: myrank !< Rank of this message passing process on communicator
  integer :: ioproc !< Rank of the message process that does i/o

  public :: mp_init, mp_header, mp_finish
  public :: mp_error, mp_bcast
  public :: mp_ioproc

  interface mp_bcast
     module procedure bcast_int
     module procedure bcast_dp
     module procedure bcast_xyz_vec
     module procedure bcast_dp_vec
     module procedure bcast_int_vec
  end interface

contains

  !> Returns true if this is an io task
  function mp_ioproc()
    implicit none
    logical mp_ioproc

    mp_ioproc = (myrank == ioproc)
  end function mp_ioproc

  !> Initialize message passing environment
  subroutine mp_init
    implicit none

#if defined(_USE_MPI)
    include 'mpif.h'
    integer :: ierr
#endif

    ! set defaults for non-mpi runs
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
    call adjust_mem(4*sp)
  end subroutine mp_init

  !> Print message passing info banner
  subroutine mp_header(channel)
    use io, only : separator
    implicit none
    integer, intent(in) :: channel
    
    write(channel,*) 'Number of message passing processes :    ', nprocs
    write(channel,*) separator
  end subroutine mp_header

  !> Shut down message passing environment
  subroutine mp_finish
    implicit none
    integer :: ierr
#if defined(_USE_MPI)
    call mpi_finalize(ierr)
#endif
  end subroutine mp_finish

  !> Abort program with error message
  subroutine mp_error(msg,code)
    use io, only : stdout
    implicit none
    character(len=*),intent(in) :: msg
    integer, intent(in) :: code
    integer :: ierr

    write(stdout,'(A)') trim(msg)
    write(stdout,'(A,I3)') 'Error code: ',code
#if defined(_USE_MPI)
    call mpi_abort(comm,code,ierr)
#else
    stop 'Fatal error'
#endif
  end subroutine mp_error

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

  subroutine bcast_xyz_vec(val)
    implicit none
    type(xyz_vec), intent(inout) :: val
#if defined(_USE_MPI)
    integer :: ierr
    include 'mpif.h'

    call mpi_bcast(val%x,val%size,MPI_DOUBLE_PRECISION,&
         ioproc,comm,ierr)
    call mpi_bcast(val%y,val%size,MPI_DOUBLE_PRECISION,&
         ioproc,comm,ierr)
    call mpi_bcast(val%z,val%size,MPI_DOUBLE_PRECISION,&
         ioproc,comm,ierr)
#endif
  end subroutine bcast_xyz_vec

  subroutine bcast_dp_vec(val)
    implicit none
    type(dp_vec), intent(inout) :: val
#if defined(_USE_MPI)
    integer :: ierr
    include 'mpif.h'

    call mpi_bcast(val%v,val%size,MPI_DOUBLE_PRECISION,&
         ioproc,comm,ierr)
#endif
  end subroutine bcast_dp_vec

  subroutine bcast_int_vec(val)
    implicit none
    type(int_vec), intent(inout) :: val
#if defined(_USE_MPI)
    integer :: ierr
    include 'mpif.h'

    call mpi_bcast(val%v,val%size,MPI_DOUBLE_PRECISION,&
         ioproc,comm,ierr)
#endif
  end subroutine bcast_int_vec

end module message_passing
