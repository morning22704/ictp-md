! ye who enter here beware of dragons
program asap_md
  ! low level modules
  use io
  use messages
  use threading

  ! simulation modules
  use sysinfo

  implicit none

  ! initialize modules as needed
  call init_messages
  call init_threads

  call init_sysinfo

  ! print banner 
  call version
  call print_messages
  if (myrank == ioproc) write (stdout,*) separator
  call print_threads
  if (myrank == ioproc) write (stdout,*) separator

  ! read input
  call read_sysinfo(stdin)

  if (myrank == ioproc)  write(stdout,*) 'Input definitions:'
  call print_sysinfo(stdout)

  ! finish off
  call end_messages
  
end program asap_md
