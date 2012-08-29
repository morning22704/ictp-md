!> Meta-module for reading input, topology, geometry and restart data
!!
!! This module actually does not do the real reading, but initializes
!! and calls the corresponding modules that handle the various logical
!! groups of data I/O. 
module input
  use control_io
  use sysinfo_io
  implicit none

  private
  logical, save :: need_init = .true. !< Flag whether to run initializers 
  public :: input_read, input_write

contains

  !> Initialize submodules
  subroutine input_init
    call control_init
    call sysinfo_init
    need_init = .false.
  end subroutine input_init

  !> Read input and restart information from all submodules
  subroutine input_read

    if (need_init) call input_init
    call control_read
    call sysinfo_read
  end subroutine input_read

  !> Write combined input information to a restart file
  subroutine input_write(level)
    use io, only : resout
    integer, intent(in) :: level
    call control_write(level)
    call sysinfo_write
    close(resout)
  end subroutine input_write

end module input
