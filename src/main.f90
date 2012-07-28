! who enters here, beware of the dragons
program asap_md
  use io
  use messages
  use threading

  implicit none

  call init_messages
  call init_threads

  call version
  call print_messages
  call print_threads
  write (stdout,*) separator

  call end_messages
  
end program asap_md
