
!> Perform basic simulation setup tasks
!!
!! This subroutine collects all basic setup tasks that need
!! to be performed before the input file is read. This includes
!! parallel setup, internal timing and memory profiling and
!! printing the generic program banner. 
subroutine basic_setup
  use io, only: stdout
  use header
  ! use timer
  use memory
  ! parallel programming modules
  use message_passing, only : mp_init, mp_header, mp_ioproc
  use threading, only : thr_init, thr_header
  implicit none

  ! initialize basic modules as needed
  !call timer_init
  call memory_init
  call mp_init
  call thr_init

  ! print banners 
  call version(stdout)
  call mp_header(stdout)
  call thr_header(stdout)
  if (mp_ioproc()) call memory_print(stdout)
end subroutine basic_setup

!> ICTP MD, a simple, modular, and parallel MD engine in Fortran 95
!!
!! The code in this project is primarily targeted to be used as a
!! teaching tool for classes and workshops on software development for
!! scientific high-performance computing applications. It is supposed
!! to demonstrate important concepts and allow practicing to work on a
!! collaborative project, without having to learn an overly complex and
!! elaborate infrastructure, as would be needed for absolute best
!! performance and scaling. Simplicity and maintainability are preferred.
!!
!! Ye, who enter here, beware of dragons.
program ictp_md
  use io, only : stdin
  use message_passing, only : mp_finish
  use input
  implicit none

  ! perform general setup tasks
  call basic_setup

  ! read system settings from input
  call input_read(stdin)

  ! finish off
  call mp_finish
end program ictp_md
