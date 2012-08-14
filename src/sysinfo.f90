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
  integer :: natoms, ntypes
  logical :: valid_x_r, valid_x_s
  logical :: have_pos, have_vel, have_chg, have_bonds
  type (xyz_vec)   :: x_r, x_s, vel, for
  type (dp_vec)    :: mss
  type (int_vec)   :: typ
  type (label_vec) :: lbl
  character(len=255) :: topofile, geomfile

  namelist /sysinfo/ topofile, geomfile

  public :: sysinfo_init, sysinfo_read
!  public :: get_natoms, get_ntypes

contains

  ! set default values
  subroutine sysinfo_init
    use memory, only: adjust_mem

    natoms     = -1
    ntypes     = -1
    call adjust_mem(2*sp)
    valid_x_r  = .false.
    valid_x_s  = .false.
    have_pos   = .false.
    have_vel   = .false.
    call adjust_mem(4*sp)
    x_r%size = -1
    vel%size = -1
    for%size = -1
    x_s%size = -1
    call adjust_mem(4*(3*dp+sp))
    mss%size = -1
    typ%size = -1
    lbl%size = -1
    call adjust_mem(3*(dp+sp))
    topofile = 'unknown'
    geomfile = 'unknown'
    call adjust_mem(2*(255+sp))
  end subroutine sysinfo_init
  
  ! read sysinfo parameters
  subroutine sysinfo_read(restart)
    use io, only: stdin, stdout, resin
    use memory, only: alloc_vec, clear_vec
    logical, intent(in) :: restart
    logical :: have_geom, have_
    integer :: nthr, ierr, extpos
    character(len=4) :: extension

    ! input is only read by io task
    if (mp_ioproc()) then

       if (restart) then
          write(stdout,*) 'Reading &sysinfo namelist from restart'
          read(resin,nml=sysinfo,iostat=ierr)
          if (ierr /= 0) call mp_error('Failure reading &sysinfo namelist',ierr)
       end if

       write(stdout,*) 'Reading &sysinfo namelist from input'
       read(stdin,nml=sysinfo,iostat=ierr)
       if (ierr /= 0) call mp_error('Failure reading &sysinfo namelist',ierr)

       ! first we need topology data. determined file type
       extpos = SCAN(topofile,'.',.true.)
       if (extpos > 0) then
          print*,'ext=',topofile(extpos:extpos+4)
          extension = topofile(extpos:extpos+4)
       else
          extension = 'none'
       end if

       ! xmol .xyz style file
       if ((extension == '.xyz') .or. (extension == '.XYZ')) then
!          call read_xyz_topology
          if (natoms <= 0) then
             call mp_error('natoms must be > 0 with topofile="internal"',natoms)
          end if
          if (ntypes <= 0) then
             call mp_error('ntypes must be > 0 with topofile="internal"',ntypes)
          end if
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

    
    call mp_bcast(typ)
    call mp_bcast(x_r)
    call mp_bcast(vel)
  end subroutine sysinfo_read

  subroutine internal_topology(channel)
    use io, only : stdout
    implicit none
    integer, intent(in) :: channel
    integer :: i, ierr
    character(len=255) :: line

    write(stdout,*) 'Reading &topology block'
    read(channel,fmt='(A)',iostat=ierr) line
    if (ierr /= 0) call mp_error('Failure reading &topology namelist',ierr)

    
  end subroutine internal_topology

end module sysinfo_io
