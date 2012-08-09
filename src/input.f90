!> Meta-module for reading input, topology, geometry and restart data
!!
!! This module actually does not do the real reading, but initializes
!! and calls the corresponding modules that handle the various logical
!! groups of data I/O. 
module input
  use control_io
  use sysinfo_io
!  use output
  implicit none

  private
  public :: input_init, input_read, input_print, input_restart

contains

  !> Initialize submodules
  subroutine input_init
    call control_init
    call sysinfo_init
  end subroutine input_init

  !> Read input and restart information from all submodules
  !! @param channel I/O channel for input
  subroutine input_read(channel)
    integer, intent(in) :: channel

    call control_read(channel)
    call sysinfo_read(channel)
  end subroutine input_read

  !> Print useful information about the current run to screen or log
  subroutine input_print(channel)
    integer, intent(in) :: channel

    call control_print(channel)
    call sysinfo_print(channel)
  end subroutine input_print

  !> Write out a restart
  subroutine input_restart(channel)
    integer, intent(in) :: channel

!    call control_restart(channel)
!    call sysinfo_restart(channel)
  end subroutine input_restart
end module input
