!
! FIXME: add documentation section here that documents the 
! entire &sysinfo section, but shows up in the user's guide 
! parts, not the developer's reference

!> Module to manage data of the current system
module sysinfo_io
  use kinds
  use constants
  use atoms, only: ndeftypes
  use message_passing, only: mp_ioproc, mp_bcast, mp_error
  use threading, only: thr_get_num_threads
  use control_io, only: is_restart
  implicit none

  private
  integer :: maxtypes
  real(kind=dp) :: cellparam(6) ! a, b, c, cosalpha, cosbeta, cosgamma
  real(kind=dp), dimension(ndeftypes) :: defmass, defcharge
  character(len=16), dimension(ndeftypes) :: deftype
  character(len=8) :: inputfmt
  character(len=255) :: topofile, geomfile

  namelist /sysinfo/ maxtypes, cellparam, defmass, defcharge, deftype, &
       inputfmt, topofile, geomfile

  public :: sysinfo_init, sysinfo_read

contains

  ! set default values
  subroutine sysinfo_init
    use memory, only: adjust_mem

    maxtypes = ndeftypes
    call adjust_mem(sp)
    cellparam(:) = d_zero
    call adjust_mem(6*dp)
    defmass(:) = d_zero
    defcharge(:) = d_zero
    call adjust_mem(2*ndeftypes*dp)
    deftype(:) = 'unknown'
    call adjust_mem(ndeftypes*lblen)
    inputfmt = 'unknown'
    call adjust_mem(8+sp)
    topofile = 'unknown'
    geomfile = 'unknown'
    call adjust_mem(2*(255+sp))
  end subroutine sysinfo_init
  
  ! read sysinfo parameters
  subroutine sysinfo_read
    use io
    use atoms, only: atoms_init, atoms_replicate, types_init
    use cell, only: cell_init
    use memory, only: alloc_vec, clear_vec
    integer :: ierr, topchannel, geochannel

    ! input is only read by io task
    if (mp_ioproc()) then

       if (is_restart()) then
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
       call types_init(deftype,maxtypes)
       call cell_init

       if (trim(topofile) == 'internal') then
          if (is_restart()) then
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
          write(stdout,*) 'Using topology from file: ', trim(topofile)
          topchannel = topin
       end if

       if (trim(geomfile) == 'internal') then
          if (is_restart()) then
             write(stdout,*) 'Using internal geometry from restart'
             geochannel = resin
          else
             write(stdout,*) 'Using internal geometry from input'
             geochannel = stdin
          end if
       else if ((trim(inputfmt) == 'xyz/xyz') .and. &
            (trim(geomfile) == trim(topofile))) then
          call mp_error('Topology and geometry must be different files',0)
       else
          open(unit=geoin, file=trim(geomfile), form='formatted', &
               status='old', iostat=ierr)
          if (ierr /= 0) call mp_error('Failure opening geometry file',ierr)
          write(stdout,*) 'Using geometry from file', trim(geomfile)
          geochannel = geoin
       end if

       if (trim(inputfmt) == 'lammps') then
       else if (trim(inputfmt) == 'xyz') then
          call xyz_topogeom(topchannel)
          if (topchannel == topin) close (unit=topin)
       else if (trim(inputfmt) == 'xyz/xyz') then
          call xyz_topology(topchannel)
          if (topchannel == topin) close (unit=topin)
          call xyz_geometry(geochannel)
          if (geochannel == geoin) close (unit=geoin)
       else if (trim(inputfmt) == 'psf/xyz') then
       else
          call mp_error('Unknown or unsupported input data format',ierr)
       end if
    end if ! mp_ioproc()

    call atoms_replicate(maxtypes)
    ! call cell_replicate
  end subroutine sysinfo_read

  subroutine xyz_topogeom(channel)
    use atoms
    integer, intent(in) :: channel
    integer :: i, ierr, idx, natoms, thistype
    real(kind=dp), dimension(3) :: pos
    character(len=255) :: line
    character(len=16) :: name

    read(channel, fmt=*, iostat=ierr) natoms
    if (ierr /= 0) call mp_error('Failure to read "natoms" from xyz file',ierr)
    call atoms_resize(natoms)
    
    read(channel, fmt='(A)', iostat=ierr) line
    do i=1, natoms
       read(channel, fmt='(A)', iostat=ierr) line

       ! split off label from the rest
       line = adjustl(line)
       idx = index(line,' ')
       if ((idx > 16) .or. (idx < 1)) idx = 16
       name = line(1:idx)
       line = line(idx+1:)

       thistype = set_type(i,name)
       if (thistype > ndeftypes) &
            call mp_error('Too many atom types. Increase ndeftypes.',thistype)

       call set_idx(i,i)
       call set_charge(i,defcharge(thistype))
       call set_mass(thistype,defmass(thistype))
       read(line,fmt=*,iostat=ierr) pos(1),pos(2),pos(3)
       if (ierr /= 0) &
            call mp_error('Failure to read coordinates from xyz file',ierr)
       call set_pos(i,pos)
    end do
  end subroutine xyz_topogeom

  subroutine xyz_topology(channel)
    use atoms
    integer, intent(in) :: channel
    integer :: i, idx, ierr, natoms, thistype
    character(len=255) :: line
    character(len=16) :: name

    read(channel, fmt=*, iostat=ierr) natoms
    if (ierr /= 0) call mp_error('Failure to read "natoms" from xyz file',ierr)
    call atoms_resize(natoms)
    
    read(channel, fmt='(A)', iostat=ierr) line
    do i=1, natoms
       read(channel, fmt='(A)', iostat=ierr) line

       ! split off label from the rest
       line = adjustl(line)
       idx = index(line,' ')
       if ((idx > 16) .or. (idx < 1)) idx = 16
       name = line(1:idx)
       line = line(idx+1:)

       thistype = set_type(i,name)
       if (thistype > ndeftypes) &
            call mp_error('Too many atom types. Increase ndeftypes.',thistype)
       call set_idx(i,i)
       call set_charge(i,defcharge(thistype))
       call set_mass(thistype,defmass(thistype))
    end do
  end subroutine xyz_topology

  subroutine xyz_geometry(channel)
    use atoms, only: get_natoms, set_pos
    integer, intent(in) :: channel
    integer :: i, idx, ierr, natoms
    real(kind=dp), dimension(3) :: pos
    character(len=255) :: line

    read(channel, fmt=*, iostat=ierr) natoms
    if (ierr /= 0) call mp_error('Failure to read "natoms" from xyz file',ierr)
    if (natoms /= get_natoms()) &
         call mp_error('Number of atoms inconsistent with topology',natoms)

    read(channel, fmt='(A)', iostat=ierr) line
    do i=1, natoms
       read(channel, fmt='(A)', iostat=ierr) line
       line = adjustl(line)
       idx = index(line,' ');
       if (idx > 0) line = line(idx:)
       read(line,fmt=*,iostat=ierr) pos(1),pos(2),pos(3)
       if (ierr /= 0) &
            call mp_error('Failure to read coordinates from xyz file',ierr)
       call set_pos(i,pos)
    end do
  end subroutine xyz_geometry
end module sysinfo_io
