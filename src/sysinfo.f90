!
! FIXME: add documentation section here that documents the 
! entire &sysinfo section, but shows up in the user's guide 
! parts, not the developer's reference

!> Module to manage data of the current system
module sysinfo_io
  use kinds
  use constants
  use atoms, only: deftypes
  use message_passing, only: mp_ioproc, mp_bcast, mp_error
  use threading, only: thr_get_num_threads
  use control_io, only: use_restart
  implicit none

  private
  integer :: maxtypes
  real(kind=dp) :: cellparam(6) ! a, b, c, cosalpha, cosbeta, cosgamma
  real(kind=dp), dimension(deftypes) :: defmass, defcharge
  character(len=8) :: inputfmt
  character(len=255) :: topofile, geomfile

  namelist /sysinfo/ maxtypes, cellparam, defmass, defcharge, &
       inputfmt, topofile, geomfile

  public :: sysinfo_init, sysinfo_read

contains

  ! set default values
  subroutine sysinfo_init
    use memory, only: adjust_mem

    maxtypes = deftypes
    call adjust_mem(sp)
    cellparam(:) = d_zero
    call adjust_mem(6*dp)
    defmass(:) = d_zero
    defcharge(:) = d_zero
    call adjust_mem(2*deftypes*dp)
    inputfmt = 'unknown'
    call adjust_mem(8+sp)
    topofile = 'unknown'
    geomfile = 'unknown'
    call adjust_mem(2*(255+sp))
  end subroutine sysinfo_init
  
  ! read sysinfo parameters
  subroutine sysinfo_read
    use io
    use atoms, only: atoms_init
    use cell, only: cell_init
    use memory, only: alloc_vec, clear_vec
    integer :: nthr, ierr, topchannel, geochannel
    character(len=3) :: extension

    ! input is only read by io task
    if (mp_ioproc()) then

       if (use_restart()) then
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
       call atoms_init(maxtypes)
       call cell_init

       if (trim(topofile) == 'internal') then
          if (use_restart()) then
             write(stdout,*) 'Using internal topology from restart'
             topchannel = resin
          else
             write(stdout,*) 'Using internal topology from input'
             topchannel = stdin
          end if
       else
          open(unit=topin, file=trim(topofile), form='formatted', &
               status='old', iostat=ierr)
          if (ierr /= 0) call mp_error('Failure opening topology file',ierr)
          write(stdout,*) 'Using topology from file', trim(topofile)
          topchannel = topin
       end if

       if (trim(geomfile) == 'internal') then
          if (use_restart()) then
             write(stdout,*) 'Using internal geometry from restart'
             geochannel = resin
          else
             write(stdout,*) 'Using internal geometry from input'
             geochannel = stdin
          end if
       else
          open(unit=geoin, file=trim(geomfile), form='formatted', &
               status='old', iostat=ierr)
          if (ierr /= 0) call mp_error('Failure opening geometry file',ierr)
          write(stdout,*) 'Using geometry from file', trim(geomfile)
          geochannel = geoin
       end if

       if (trim(inputfmt) == 'lammps') then
       else if (trim(inputfmt) == 'xyz') then
          call xyz_topology(topchannel)
          if (topchannel == topin) close (unit=topin)
          call xyz_geometry(geochannel)
          if (geochannel == geoin) close (unit=geoin)
       else if (trim(inputfmt) == 'psf/xyz') then
       else
          call mp_error('Unknown or unsupported input data format',ierr)
       end if
    end if ! mp_ioproc()
  end subroutine sysinfo_read

  subroutine xyz_topology(channel)
    use atoms, only: atoms_resize, set_type, set_charge, set_mass
    integer, intent(in) :: channel
    integer :: i, ierr, natoms, thistype
    character(len=255) :: line
    character(len=16) :: name

    read(channel, fmt=*, iostat=ierr) natoms
    if (ierr /= 0) call mp_error('Failure to read "natoms" from xyz file',ierr)
    call atoms_resize(natoms)
    
    read(channel, fmt='(A)', iostat=ierr) line
    do i=1, natoms
       read(channel, fmt='(A)', iostat=ierr) line
       line = adjustl(line)
       name = trim(line)
       thistype = set_type(i,name)
       if (thistype > deftypes) &
            call mp_error('Too many atom types. Increase deftypes.',thistype)
       call set_charge(i,defcharge(thistype))
       call set_mass(thistype,defmass(thistype))
    end do
  end subroutine xyz_topology

  subroutine xyz_geometry(channel)
    use atoms, only: atoms_resize, get_natoms
    integer, intent(in) :: channel
    integer :: i, ierr, natoms, pos
    character(len=255) :: line

    read(channel, fmt=*, iostat=ierr) natoms
    if (ierr /= 0) call mp_error('Failure to read "natoms" from xyz file',ierr)
    if (natoms /= get_natoms()) &
         call mp_error('Number of atoms inconsistent with topology',natoms)

    read(channel, fmt='(A)', iostat=ierr) line
    do i=1, natoms
       read(channel, fmt='(A)', iostat=ierr) line
       line = adjustl(line)
    end do
  end subroutine xyz_geometry
end module sysinfo_io
