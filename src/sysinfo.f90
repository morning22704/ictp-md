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
  integer :: maxtypes
  real(kind=dp) :: cellparam(6) ! a, b, c, cosalpha, cosbeta, cosgamma
  character(len=8) :: inputfmt
  character(len=255) :: topofile, geomfile

  namelist /sysinfo/ maxtypes, cellparam, inputfmt, topofile, geomfile

  public :: sysinfo_init, sysinfo_read

contains

  ! set default values
  subroutine sysinfo_init
    use memory, only: adjust_mem

    maxtypes = 16
    call adjust_mem(sp)
    cellparam(:) = d_zero
    call adjust_mem(6*dp)
    inputfmt = 'unknown'
    call adjust_mem(8+sp)
    topofile = 'unknown'
    geomfile = 'unknown'
    call adjust_mem(2*(255+sp))
  end subroutine sysinfo_init
  
  ! read sysinfo parameters
  subroutine sysinfo_read
    use io
    use atom, only: atom_init
    use cell, only: cell_init
    use memory, only: alloc_vec, clear_vec
    integer :: nthr, ierr, nmlchannel
    character(len=3) :: extension

    ! input is only read by io task
    if (mp_ioproc()) then

       nmlchannel = stdin
       if (use_restart()) then
          nmlchannel = resin
          write(stdout,*) 'Reading &sysinfo namelist from restart'
          read(resin,nml=sysinfo,iostat=ierr)
          if (ierr /= 0) call mp_error('Failure reading &sysinfo namelist',ierr)
       end if
       write(stdout,*) 'Reading &sysinfo namelist from input'
       read(stdin,nml=sysinfo,iostat=ierr)
       if (ierr /= 0) call mp_error('Failure reading &sysinfo namelist',ierr)

       if (maxtypes < 1) then
          call mp_error('Parameter maxtypes must be > 0',ierr)
       endif

       write(stdout,*) separator
       write(stdout,*) 'Max. number of atom types: ', maxtypes
       write(stdout,*) 'System info format: ', trim(inputfmt)
       write(stdout,*) 'Topology read from: ', trim(topofile)
       write(stdout,*) 'Geometry read from: ', trim(geomfile)
       write(stdout,*) separator
       write(stdout,nml=sysinfo)
       write(stdout,*) separator

       ! initialize system storage
       call atom_init(maxtypes)
       call cell_init

       if (trim(topofile) /= 'internal') then
          open(unit=topin, file=trim(topofile), form='formatted', &
               status='old', iostat=ierr)
          if (ierr /= 0) call mp_error('Failure opening topology file',ierr)
          nmlchannel=topin
       end if
       if (trim(inputfmt) == 'lammps') then
       else if (trim(inputfmt) == 'xyz/xyz') then
          call xyz_topology(nmlchannel)
          if (nmlchannel == topin) close (unit=topin)
       else if (trim(inputfmt) == 'psf/xyz') then
       else
          call mp_error('Unknown or unsupported input data format',ierr)
       end if
    end if ! mp_ioproc()
  end subroutine sysinfo_read

  subroutine xyz_topology(channel)
    use atom, only: atom_resize, set_type, get_ntypes
    integer, intent(in) :: channel
    integer :: i,pos,ierr,natoms
    character(len=255) :: line
    character(len=16) :: name

    read(channel, fmt=*, iostat=ierr) natoms
    if (ierr /= 0) call mp_error('Failure to read "natoms" from xyz file',ierr)
    call atom_resize(natoms)
    
    read(channel, fmt='(A)', iostat=ierr) line
    do i=1, natoms
       read(channel, fmt='(A)', iostat=ierr) line
       line = adjustl(line)
       pos = index(line,' ')
       if (pos < 1) pos = 16
       name = line(1:pos)
       call set_type(i,name)
    end do

    print*,'ntypes is now:',get_ntypes()
  end subroutine xyz_topology
end module sysinfo_io
