! message passing support
module message_passing

  use kinds
  use memory, only: adjust_mem
  implicit none

  private
  type mp_info_type
     integer :: nprocs ! number of message passing processes
     integer :: myrank ! my rank on communicator
     integer :: ioproc ! rank that does i/o
     integer :: comm   ! communicator
  end type mp_info_type
  type (mp_info_type) :: mp_info

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

  ! returns true if this is an io task
  function mp_ioproc()
    implicit none
    logical mp_ioproc

    mp_ioproc = (mp_info%myrank == mp_info%ioproc)
  end function mp_ioproc

  ! set up message passing environment
  subroutine mp_init
    implicit none

#if defined(_USE_MPI)
    include 'mpif.h'
    integer :: ierr
#endif

    ! set defaults for non-mpi runs
    mp_info%nprocs = 1
    mp_info%myrank = 0
    mp_info%ioproc = 0
    mp_info%comm   = 0

#if defined(_USE_MPI)
    mp_info%comm=MPI_COMM_WORLD
    call mpi_init(ierr)
    call mpi_comm_size(mp_info%comm,mp_info%nprocs,ierr)
    call mpi_comm_rank(mp_info%comm,mp_info%myrank,ierr)
#endif
    call adjust_mem(4*sp)
  end subroutine mp_init

  ! print message passing header
  subroutine mp_header
    use io, only : stdout,separator
    implicit none

    if (mp_ioproc()) then
       write(stdout,*) 'Number of message passing processes :    ', &
            mp_info%nprocs
       write(stdout,*) separator
    end if
  end subroutine mp_header

  ! close message passing environment
  subroutine mp_finish
    implicit none
    integer :: ierr
#if defined(_USE_MPI)
    call mpi_finalize(ierr)
#endif
  end subroutine mp_finish

  ! abort with error message
  subroutine mp_error(msg)
    use io, only : stderr
    implicit none
    character(len=*),intent(in) :: msg
    integer :: ierr

    write(stderr,'(A)') trim(msg)
#if defined(_USE_MPI)
    call mpi_abort(mp_info%comm,10,ierr)
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

    call mpi_bcast(val,1,MPI_INTEGER,mp_info%ioproc,mp_info%comm,ierr)
#endif
  end subroutine bcast_int

  subroutine bcast_dp(val)
    implicit none
    real(kind=dp), intent(inout) :: val
#if defined(_USE_MPI)
    integer :: ierr
    include 'mpif.h'

    call mpi_bcast(val,1,MPI_DOUBLE_PRECISION,mp_info%ioproc,mp_info%comm,ierr)
#endif
  end subroutine bcast_dp

  subroutine bcast_xyz_vec(val)
    implicit none
    type(xyz_vec), intent(inout) :: val
#if defined(_USE_MPI)
    integer :: ierr
    include 'mpif.h'

    call mpi_bcast(val%x,val%size,MPI_DOUBLE_PRECISION,&
         mp_info%ioproc,mp_info%comm,ierr)
    call mpi_bcast(val%y,val%size,MPI_DOUBLE_PRECISION,&
         mp_info%ioproc,mp_info%comm,ierr)
    call mpi_bcast(val%z,val%size,MPI_DOUBLE_PRECISION,&
         mp_info%ioproc,mp_info%comm,ierr)
#endif
  end subroutine bcast_xyz_vec

  subroutine bcast_dp_vec(val)
    implicit none
    type(dp_vec), intent(inout) :: val
#if defined(_USE_MPI)
    integer :: ierr
    include 'mpif.h'

    call mpi_bcast(val%v,val%size,MPI_DOUBLE_PRECISION,&
         mp_info%ioproc,mp_info%comm,ierr)
#endif
  end subroutine bcast_dp_vec

  subroutine bcast_int_vec(val)
    implicit none
    type(int_vec), intent(inout) :: val
#if defined(_USE_MPI)
    integer :: ierr
    include 'mpif.h'

    call mpi_bcast(val%v,val%size,MPI_DOUBLE_PRECISION,&
         mp_info%ioproc,mp_info%comm,ierr)
#endif
  end subroutine bcast_int_vec

end module message_passing
