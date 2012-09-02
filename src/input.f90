!> Meta-module for reading input, topology, geometry and restart data
!!
!! This module actually does not do the real reading, but initializes
!! and calls the corresponding modules that handle the various logical
!! groups of data I/O.
module input
  use control_io
  use sysinfo_io
  use pair_io
  use neighbor_io
  implicit none

  private
  public :: input_init, input_read

contains

  !> Initialize submodules
  subroutine input_init
    call control_init
    call sysinfo_init
    call pair_init
    call neighbor_init
  end subroutine input_init

  !> Read input and restart information from all submodules
  subroutine input_read
    call control_read
    call sysinfo_read
    call pair_read
    call neighbor_read
  end subroutine input_read

end module input
