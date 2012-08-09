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

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! low level modules
  use kinds
  use io
  use header
  ! use timer
  use memory
  ! parallel programming modules
  use message_passing, only : mp_init, mp_header, mp_finish
  use threading, only : thr_init, thr_header
  ! simulation modules
  use input
  !
  implicit none
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! initialize basic modules as needed
  !call timer_init
  call memory_init
  call mp_init
  call thr_init

  ! print banners 
  call version(stdout)
  call mp_header(stdout)
  call thr_header(stdout)
  call memory_print(stdout)

  ! read system settings from input
  call input_read(stdin)

  ! open restart channel and verify validity and version
  open(unit=resout, file='new.restart', form='formatted',&
       access='sequential', action='write', status='unknown')
  close(resout)

  ! finish off
  call mp_finish
  
end program ictp_md
