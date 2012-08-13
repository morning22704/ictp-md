!> Module to provide an abstract interface to OpenMP library functions.
!!
!! The purpose of the threading module is to provide
!! a wrapper for OpenMP library and support functions
!! in a way that they can be called the same way, even
!! if compiled without OpenMP enabled or with a compiler
!! that does not support OpenMP.
!!
!! Furthermore useful utility functions are provided
!! that help with implementing thread support in a
!! more explicit way and not implicitly through directives.
module threading
  implicit none

  private
  public :: thr_init, thr_header, thr_get_num_threads

  integer :: nthreads !< Number of active threads

contains

  !> Initialize thread support and the threading module.
  !!
  !! Determine the total number of threads available and 
  !! store it in a module variable, which defaults to 1,
  !! if compiled OpenMP support is not available or included.
  subroutine thr_init
!$  integer, external :: omp_get_max_threads

    nthreads = 1
!$  nthreads = omp_get_max_threads()
  end subroutine thr_init

  !> Read-only access to nthreads variable
  !!
  !! @returns The number of active threads
  function thr_get_num_threads()
    integer :: thr_get_num_threads

    thr_get_num_threads = nthreads
  end function thr_get_num_threads

  !> Print informative header text
  !> @param channel I/O unit to write header text to
  subroutine thr_header
    use io, only : stdout,separator
    integer :: have_threads

    have_threads = 0
!$  have_threads = 1

    if (have_threads > 0) then
       write(stdout,*) 'OpenMP enabled build. Number of threads :',nthreads
    else
       write(stdout,*) 'OpenMP not enabled in this build'
    end if
    write (stdout,*) separator
  end subroutine thr_header

end module threading
