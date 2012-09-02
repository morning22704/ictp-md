!> Wrapper module to provide message passing support.
!!
!! Rather than using calls directly to the message passing
!! library, we provide an abstract interface with adaptations
!! to specific data types of the application. This should also
!! make transparent to the rest of the application whether
!! message passing support is compiled in or not.
module message_passing

  use kinds
  use memory, only: adjust_mem, alloc_vec, free_vec, alloc_mat, free_mat
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
     module procedure bcast_log
     module procedure bcast_dp_dim
     module procedure bcast_string
     module procedure bcast_xyz_vec
     module procedure bcast_dp_vec
     module procedure bcast_int_vec
     module procedure bcast_dp_mat
     module procedure bcast_int_mat
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
  subroutine mp_header
    use io, only : stdout,separator

    write(stdout,*) separator
#if defined(_USE_MPI)
    write(stdout,*) 'Number of message passing processes :    ', nprocs
#else
    write(stdout,*) 'Message passing not enabled in this build'
#endif
  end subroutine mp_header

  !> Shut down message passing environment
  subroutine mp_finish
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

    write(stdout,*) trim(msg)
    write(stdout,'(A,I3)') ' Error code: ',code
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

  subroutine bcast_log(val)
    implicit none
    logical, intent(inout) :: val
#if defined(_USE_MPI)
    integer :: ierr
    include 'mpif.h'

    call mpi_bcast(val,1,MPI_LOGICAL,ioproc,comm,ierr)
#endif
  end subroutine bcast_log

  subroutine bcast_string(val)
    implicit none
    character(len=*), intent(inout) :: val
#if defined(_USE_MPI)
    integer :: ierr
    include 'mpif.h'

    call mpi_bcast(val,len(val),MPI_CHARACTER,ioproc,comm,ierr)
#endif
  end subroutine bcast_string

  subroutine bcast_dp_dim(val,dim)
    implicit none
    real(kind=dp), dimension(*), intent(inout) :: val
    integer, intent(in) :: dim
#if defined(_USE_MPI)
    integer :: ierr
    include 'mpif.h'

    call mpi_bcast(val,dim,MPI_DOUBLE_PRECISION,ioproc,comm,ierr)
#endif
  end subroutine bcast_dp_dim

  subroutine bcast_xyz_vec(val)
    implicit none
    type(xyz_vec), intent(inout) :: val
#if defined(_USE_MPI)
    integer :: ierr, newsize
    include 'mpif.h'

    newsize = val%size
    call mpi_bcast(newsize,1,MPI_INTEGER,ioproc,comm,ierr)

    if (myrank /= ioproc) then
       call free_vec(val)
       call alloc_vec(val,newsize)
    end if
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
    integer :: ierr, newsize
    include 'mpif.h'

    newsize = val%size
    call mpi_bcast(newsize,1,MPI_INTEGER,ioproc,comm,ierr)

    if (myrank /= ioproc) then
       call free_vec(val)
       call alloc_vec(val,newsize)
    end if
    call mpi_bcast(val%v,val%size,MPI_DOUBLE_PRECISION,&
         ioproc,comm,ierr)
#endif
  end subroutine bcast_dp_vec

  subroutine bcast_int_vec(val)
    implicit none
    type(int_vec), intent(inout) :: val
#if defined(_USE_MPI)
    integer :: ierr, newsize
    include 'mpif.h'

    newsize = val%size
    call mpi_bcast(newsize,1,MPI_INTEGER,ioproc,comm,ierr)

    if (myrank /= ioproc) then
       call free_vec(val)
       call alloc_vec(val,newsize)
    end if
    call mpi_bcast(val%v,val%size,MPI_DOUBLE_PRECISION,&
         ioproc,comm,ierr)
#endif
  end subroutine bcast_int_vec

  subroutine bcast_dp_mat(val)
    implicit none
    type(dp_mat), intent(inout) :: val
#if defined(_USE_MPI)
    integer :: ierr, newsize(2)
    include 'mpif.h'

    newsize(1) = val%sizex
    newsize(2) = val%sizey
    call mpi_bcast(newsize,2,MPI_INTEGER,ioproc,comm,ierr)

    if (myrank /= ioproc) then
       call free_mat(val)
       call alloc_mat(val,newsize(1),newsize(2))
    end if
    call mpi_bcast(val%m,val%sizex*val%sizey,MPI_DOUBLE_PRECISION,&
         ioproc,comm,ierr)
#endif
  end subroutine bcast_dp_mat

  subroutine bcast_int_mat(val)
    implicit none
    type(int_mat), intent(inout) :: val
#if defined(_USE_MPI)
    integer :: ierr, newsize(2)
    include 'mpif.h'

    newsize(1) = val%sizex
    newsize(2) = val%sizey
    call mpi_bcast(newsize,2,MPI_INTEGER,ioproc,comm,ierr)

    if (myrank /= ioproc) then
       call free_mat(val)
       call alloc_mat(val,newsize(1),newsize(2))
    end if
    call mpi_bcast(val%m,val%sizex*val%sizey,MPI_INTEGER,&
         ioproc,comm,ierr)
#endif
  end subroutine bcast_int_mat


end module message_passing
