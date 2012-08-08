! wrapper module for multi-threading support via OpenMP
module threading
  implicit none

  private
  public :: thr_init, thr_header, thr_get_num_threads

  integer :: nthreads ! number of active threads

contains

  ! determine the total number of threads available
  ! falls back to 1, if compiled w/o OpenMP support
  subroutine thr_init
    implicit none
!$  integer, external :: omp_get_max_threads

    nthreads = 1
!$  nthreads = omp_get_max_threads()
  end subroutine thr_init

  ! function to for read-only access to nthreads
  function thr_get_num_threads()
    integer :: thr_get_num_threads

    thr_get_num_threads = nthreads
  end function thr_get_num_threads

  ! print header
  subroutine thr_header
    use io, only : stdout, separator
    use message_passing, only : mp_ioproc
    implicit none
    integer have_threads

    have_threads = 0
!$  have_threads = 1

    if (mp_ioproc()) then
       if (have_threads > 0) then
          write(stdout,*) 'OpenMP enabled build. Number of threads :',nthreads
       else
          write(stdout,*) 'OpenMP not enabled in build'
       end if
       write (stdout,*) separator
    end if
  end subroutine thr_header

end module threading
