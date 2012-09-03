!> Module for neighbor/cell list generation

module neighbor
  use kinds
  use constants
  use message_passing, only : mp_error, mp_ioproc, mp_bcast
  implicit none
  private
  real(kind=dp) :: ratio, dx, dy, dz
  integer :: next_step, nx, ny, nz, ncells
  public neighbor_init, neighbor_setup, neighbor_write

contains

  subroutine neighbor_init
    use memory, only : adjust_mem
    ratio = 2.0_dp
    dx = -d_one
    dy = -d_one
    dz = -d_one
    nx = -1
    ny = -1
    nz = -1
    ncells = -1
    next_step = -1
    call adjust_mem(5*dp+4*sp)
  end subroutine neighbor_init

  ! read neighbor list parameters
  subroutine neighbor_setup
    use io
    use memory,     only : alloc_vec, clear_vec, memory_print
    use cell,       only : get_hmat
    use pair_io,    only : get_max_cutoff
    use sysinfo_io, only : get_skin, get_nlevel
    integer :: ierr, nlevel
    real(kind=dp) :: cutoff, skin, hmat(6)

    call get_hmat(hmat)
    cutoff = get_max_cutoff() + get_skin()
    nlevel = get_nlevel()

    nx = int(dble(nlevel)*hmat(1)/cutoff)
    ny = int(dble(nlevel)*hmat(2)/cutoff)
    nz = int(dble(nlevel)*hmat(3)/cutoff)
    ncells = nx*ny*nz
    dx = d_one/dble(nx)
    dy = d_one/dble(ny)
    dz = d_one/dble(nz)


    print*,ncells,nx,ny,nz,dx,dy,dz
    stop
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
  end subroutine neighbor_setup

  !> Write info for neighbor module
  subroutine neighbor_write
    use io, only: resout, stdout
    integer :: ierr

    if (mp_ioproc()) then
    end if
  end subroutine neighbor_write

end module neighbor
