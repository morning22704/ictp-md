! multi-threading support
module threading

  use io, only : stdout
  use messages, only : myrank, ioproc
  implicit none

  private
  public :: nthreads, init_threads, print_threads

  integer :: nthreads ! number of active threads

contains

  ! determine the total number of threads available
  subroutine init_threads
    implicit none
!$  integer, external :: omp_get_max_threads

    nthreads = 1
!$  nthreads = omp_get_max_threads()
  end subroutine init_threads

  ! print header
  subroutine print_threads
    implicit none
    integer have_threads

    have_threads = 0
!$  have_threads = 1

    if (myrank == ioproc) then
       if (have_threads > 0) then
          write(stdout,*) 'OpenMP enabled build. Number of threads :',nthreads
       else
          write(stdout,*) 'OpenMP not enabled in build'
       end if
    end if
  end subroutine print_threads

end module threading
