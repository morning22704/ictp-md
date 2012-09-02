!> Module for neighbor/cell list generation

module neighbor_io
  use kinds
  use constants
  use message_passing, only : mp_error, mp_ioproc, mp_bcast
  implicit none
  private
  real(kind=dp) :: ratio, dx, dy, dz
  integer :: next_step, nx, ny, nz
  public neighbor_init, neighbor_read, neighbor_write

contains

  subroutine neighbor_init
    use memory, only : adjust_mem
    ratio = 2.0_dp
    dx = -d_one
    dy = -d_one
    dz = -d_one
    nx = 1
    ny = 1
    nz = 1
    next_step = -1
    call adjust_mem(4*dp+4*sp)
  end subroutine neighbor_init

  ! read neighbor list parameters
  subroutine neighbor_read
    use io
    use memory, only: alloc_vec, clear_vec, memory_print
    use control_io, only: is_restart
    integer :: ierr

    ! input is only read by io task
    if (mp_ioproc()) then
!       write(stdout,*) separator
!       write(stdout,*) 'Max. number of atom types: ', maxtypes
!       write(stdout,*) 'System info format   : ', trim(inpformat)
!       write(stdout,*) 'Topology read from   : ', trim(topfile)
!       write(stdout,*) 'Positions read from  : ', trim(posfile)
!       write(stdout,*) 'Velocities read from : ', trim(velfile)
!       write(stdout,*) separator
    end if
  end subroutine neighbor_read

  !> Write info for neighbor module
  subroutine neighbor_write
    use io, only: resout, stdout
    integer :: ierr

    if (mp_ioproc()) then
    end if
  end subroutine neighbor_write

end module neighbor_io
