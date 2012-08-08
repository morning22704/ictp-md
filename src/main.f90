! ye who enter here beware of dragons
program asap_md

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! low level modules
  use kinds
  use memory
  use io
  use header
  ! parallel programming modules
  use message_passing, only : mp_init, mp_header, mp_finish
  use threading, only : thr_init, thr_header
  ! simulation modules
  use read_input
  !
  implicit none
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! initialize basic modules as needed
  call memory_init
  call mp_init
  call thr_init
  call input_init

  ! print banner 
  call version
  call mp_header
  call thr_header

  call memory_print(stdout)

  ! read system settings from input
  call input_read(stdin)

  ! echo input settings
  call input_print(stdout)
  call memory_print(stdout)

  ! open restart channel and verify validity and version
  open(unit=resout, file='new.restart', form='formatted',&
       access='sequential', action='write', status='unknown')
  call input_print(resout)
  close(resout)

  ! finish off
  call mp_finish
  
end program asap_md
