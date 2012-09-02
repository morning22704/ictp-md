!> Module for neighbor/cell list generation

module neighbor_io
  use kinds
  use constants
  use message_passing, only : mp_error, mp_ioproc, mp_bcast
  implicit none
  private
  real(kind=dp) :: skin, ratio, dx, dy, dz
  integer :: nsplit, nx, ny, nz
  type(int_vec) :: nlist
  logical :: newton
  public neighbor_init, neighbor_read, neighbor_write, is_newton

  namelist /neighbor/ skin, nsplit, newton

contains

  subroutine neighbor_init
    use memory, only : adjust_mem
    skin  = 2.0_dp
    ratio = 2.0_dp
    dx = -d_one
    dy = -d_one
    dz = -d_one
    nsplit = 1
    nx = 1
    ny = 1
    nz = 1
    nlist%size = -1
    newton = .true.
    call adjust_mem(5*dp+4*sp+(dp+sp)+sp)
  end subroutine neighbor_init

  ! read neighbor list parameters
  subroutine neighbor_read
    use io
    use memory, only: alloc_vec, clear_vec, memory_print
    use control_io, only: is_restart
    integer :: ierr

    ! input is only read by io task
    if (mp_ioproc()) then

       if (is_restart()) then
          write(stdout,*) 'Reading &neighbor namelist from restart'
          read(resin,nml=neighbor,iostat=ierr)
          if (ierr /= 0) call mp_error('Failure reading &neighbor namelist',ierr)
       end if
       write(stdout,*) 'Reading &neighbor namelist from input'
       read(stdin,nml=neighbor,iostat=ierr)
       if (ierr /= 0) &
            call mp_error('Failure reading &neighbor namelist',ierr)

       if (skin <= d_zero) then
          call mp_error('Parameter skin must be > 0.0',ierr)
       endif

       if (nsplit <= 0) then
          call mp_error('Parameter nsplit must be > 0',ierr)
       endif

!       write(stdout,*) separator
!       write(stdout,*) 'Max. number of atom types: ', maxtypes
!       write(stdout,*) 'System info format   : ', trim(inpformat)
!       write(stdout,*) 'Topology read from   : ', trim(topfile)
!       write(stdout,*) 'Positions read from  : ', trim(posfile)
!       write(stdout,*) 'Velocities read from : ', trim(velfile)
!       write(stdout,*) separator
    end if
  end subroutine neighbor_read

  function is_newton()
    logical is_newton
    is_newton = newton
  end function is_newton

  !> Write info for neighbor module
  subroutine neighbor_write
    use io, only: resout, stdout
    integer :: ierr

    if (mp_ioproc()) then
       write(stdout,*) 'Writing &neighbor namelist to restart'
       write(unit=resout, nml=neighbor, iostat=ierr)
       if (ierr /= 0) call mp_error('Failure writing &neighbor restart',ierr)
    end if
  end subroutine neighbor_write

end module neighbor_io
