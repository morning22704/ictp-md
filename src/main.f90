! ye who enter here beware of dragons
program asap_md

  ! low level modules
  use io
  use header
  use message_passing
  use threading, only : thr_init, thr_header

  ! simulation modules
  use sysinfo

  implicit none

  ! initialize modules as needed
  call mp_init
  call thr_init

  call init_sysinfo

  ! print banner 
  call version
  call mp_header
  call thr_header

  ! read run settings from input
  call read_sysinfo(stdin)

  ! echo input settings
  call print_sysinfo(stdout)

  ! finish off
  call mp_finish
  
end program asap_md
