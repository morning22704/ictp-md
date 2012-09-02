!> Meta-module for writing restart files
!!
!! This module actually does not do the real writing, but organizes
!! the restart handling and calls the corresponding modules that handle
!! the various logical groups of data I/O.
module restart
  use control_io
  use sysinfo_io
  use pair_io
  use neighbor_io
  implicit none

  private
  integer :: num_restarts, last_restart_no
  public :: restart_init, restart_write, set_num_restarts

contains

  !> Initialize module
  subroutine restart_init
    last_restart_no = 0
    num_restarts = 2
  end subroutine restart_init

  !> Define the maximum number of concurrent restart files
  subroutine set_num_restarts(num)
    use message_passing, only : mp_error
    integer, intent(in) :: num

    if (num > 9) &
         call mp_error('cannot have more than 9 concurrent restart files',num)
    num_restarts = num
  end subroutine set_num_restarts

  !> Write combined input information to a restart file
  !! @param level Restart request level
  subroutine restart_write(level)
    use io, only : resout
    integer, intent(in) :: level
    integer :: reslevel
 
   reslevel = level
    if (reslevel == 0 ) then
       last_restart_no = last_restart_no + 1
       if (last_restart_no > num_restarts) last_restart_no = 1
       reslevel = last_restart_no
    end if
    call control_write(reslevel)
    call sysinfo_write
    call pair_write
    call neighbor_write
    close(resout)
  end subroutine restart_write

end module restart
