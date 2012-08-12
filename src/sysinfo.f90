!
! FIXME: add documentation section here that documents the 
! entire &sysinfo section, but shows up in the user's guide 
! parts, not the developer's reference

!> Module to manage data of the current system
module sysinfo_io
  use kinds
  use constants
  use message_passing, only: mp_ioproc, mp_bcast, mp_error
  use threading, only: thr_get_num_threads
  use control_io, only: use_restart
  implicit none

  private
  integer :: natoms, ntypes, nbonds, nbtypes, nangles, natypes
  logical :: update_x_r, update_x_s
  type (xyz_vec) :: x_r, vel, for, x_s
  type (dp_vec)  :: chg
  type (int_vec) :: typ
  character(len=255) :: topofile, geomfile

  namelist /sysinfo/ natoms, ntypes, nbonds, nbtypes, nangles, natypes, &
          topofile, geomfile

  public :: sysinfo_init, sysinfo_read, sysinfo_print!, sysinfo_restart
!  public :: get_natoms, get_ntypes

contains

  ! set default values
  subroutine sysinfo_init
    use memory, only: adjust_mem

    natoms     = -1
    ntypes     = -1
    nbonds     = -1
    nbtypes    = -1
    nangles    = -1
    natypes    = -1
    call adjust_mem(6*sp)
    update_x_r = .false.
    update_x_s = .false.
    call adjust_mem(2*sp)
    x_r%size = -1
    vel%size = -1
    for%size = -1
    x_s%size = -1
    call adjust_mem(4*(3*dp+sp))
    chg%size   = -1
    typ%size   = -1
    call adjust_mem(2*(dp+sp))
    topofile = 'unknown'
    geomfile = 'unknown'
    call adjust_mem(2*(255+sp))
  end subroutine sysinfo_init
  
  ! read sysinfo parameters
  subroutine sysinfo_read(channel)
    use io, only: stdout
    use memory, only: alloc_vec
    integer, intent(in) :: channel
    integer :: nthr, ierr

    ! input is only read by io task
    if (mp_ioproc()) then

       write(stdout,*) 'Reading &sysinfo namelist from input'
       read(channel,nml=sysinfo,iostat=ierr)
       if (ierr /= 0) call mp_error('Failure reading &sysinfo namelist',ierr)

       ! some consistency checks
       if (natoms <= 0) then
          call mp_error('natoms must be > 0',natoms)
       endif
       if (ntypes <= 0) then
          call mp_error('ntypes must be > 0',ntypes)
       endif
    end if ! mp_ioproc()

    ! broadcast basic system info
    call mp_bcast(natoms)
    call mp_bcast(ntypes)

    ! allocate per atom storage
    nthr = thr_get_num_threads()
    call alloc_vec(x_r,natoms)
    call alloc_vec(x_s,natoms)
    call alloc_vec(vel,natoms)
    call alloc_vec(for,nthr*natoms)
    call alloc_vec(typ,natoms)
    call alloc_vec(chg,natoms)

!    call topology_read(channel,natoms,ntypes,typ,chg)


    chg%v(:) = d_zero
    vel%x(:) = d_zero
    vel%y(:) = d_zero
    vel%z(:) = d_zero
    ! fixme: read types/charge from topofile, coordinates from geomfile

    call mp_bcast(typ)
    call mp_bcast(x_r)
    call mp_bcast(chg)
    call mp_bcast(vel)
  end subroutine sysinfo_read

  subroutine sysinfo_print(channel)
    integer, intent(in) :: channel
    integer :: ierr

    if (mp_ioproc()) then
       write(unit=channel,nml=sysinfo,iostat=ierr)
       if (ierr /= 0) call mp_error('failed to write &sysinfo namelist',ierr)
    endif
    
  end subroutine sysinfo_print
end module sysinfo_io
