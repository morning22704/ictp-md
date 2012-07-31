! multi-threading support
module threading
  implicit none

  private
  public :: thr_init, thr_header

  integer, public :: nthreads ! number of active threads

contains

  ! determine the total number of threads available
  subroutine thr_init
    implicit none
!$  integer, external :: omp_get_max_threads

    nthreads = 1
!$  nthreads = omp_get_max_threads()
  end subroutine thr_init

  ! print header
  subroutine thr_header
    use io, only : stdout, separator
    use message_passing, only : mp_info
    implicit none
    integer have_threads

    have_threads = 0
!$  have_threads = 1

    if (mp_info%myrank == mp_info%ioproc) then
       if (have_threads > 0) then
          write(stdout,*) 'OpenMP enabled build. Number of threads :',nthreads
       else
          write(stdout,*) 'OpenMP not enabled in build'
       end if
       write (stdout,*) separator
    end if
  end subroutine thr_header

end module threading
