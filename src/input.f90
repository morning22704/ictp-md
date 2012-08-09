!> Meta-module for reading input, topology, geometry and restart data
!!
!! This module actually does not do the real reading, but initializes
!! and calls the corresponding modules that handle the various logical
!! groups of data I/O. 
module input
  use io, only: stdout
  use memory, only: memory_print
  use control_io
  use sysinfo_io
  implicit none

  private
  logical, save :: need_init = .true. !< Flag whether to run initializers 
  public :: input_read

contains

  !> Initialize submodules
  subroutine input_init
    call control_init
    call sysinfo_init
    need_init = .false.
  end subroutine input_init

  !> Read input and restart information from all submodules
  !! @param channel I/O channel for input
  subroutine input_read(channel)
    integer, intent(in) :: channel

    if (need_init) call input_init
    call control_read(channel)
    call control_print(stdout)
    call memory_print(stdout)
    call sysinfo_read(channel)
    !call sysinfo_print(stdout)
    call memory_print(stdout)
  end subroutine input_read

end module input
