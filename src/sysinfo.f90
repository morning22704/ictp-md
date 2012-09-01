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
  character(len=lblen), dimension(ndeftypes) :: deftype
  character(len=lblen) :: inpformat
  character(len=lilen) :: topfile, posfile, velfile

  namelist /sysinfo/ maxtypes, cellparam, defmass, defcharge, deftype, &
       inpformat, topfile, posfile, velfile

  public :: sysinfo_init, sysinfo_read, sysinfo_write

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
    inpformat = 'unknown'
    call adjust_mem(lblen+sp)
    topfile = 'unknown'
    posfile = 'unknown'
    velfile = 'unknown'
    call adjust_mem(3*(lilen+sp))
  end subroutine sysinfo_init

  ! read sysinfo parameters
  subroutine sysinfo_read
    use io
    use atoms, only: atoms_init, atoms_replicate, types_init
    use cell, only: cell_init
    use memory, only: alloc_vec, clear_vec, memory_print
    integer :: ierr, topchannel, poschannel, velchannel

    ! input is only read by io task
    if (mp_ioproc()) then

       if (is_restart()) then
          write(stdout,*) 'Reading &sysinfo namelist from restart'
          read(resin,nml=sysinfo,iostat=ierr)
          if (ierr /= 0) call mp_error('Failure reading &sysinfo namelist',ierr)
       end if
       write(stdout,*) 'Reading &sysinfo namelist from input'
       read(stdin,nml=sysinfo,iostat=ierr)
       if (ierr /= 0) &
            call mp_error('Failure reading &sysinfo namelist',ierr)

       if (maxtypes < 1) then
          call mp_error('Parameter maxtypes must be > 0',ierr)
       endif

       write(stdout,*) separator
       write(stdout,*) 'Max. number of atom types: ', maxtypes
       write(stdout,*) 'System info format   : ', trim(inpformat)
       write(stdout,*) 'Topology read from   : ', trim(topfile)
       write(stdout,*) 'Positions read from  : ', trim(posfile)
       write(stdout,*) 'Velocities read from : ', trim(velfile)
       write(stdout,*) separator

       ! initialize system storage
       call atoms_init(maxtypes)
       call types_init(deftype,maxtypes)
       call cell_init

       if (trim(topfile) == 'internal') then
          if (is_restart()) then
             write(stdout,*) 'Using internal topology from restart'
             topchannel = resin
          else
             write(stdout,*) 'Using internal topology from input'
             topchannel = stdin
          end if
       else
          open(unit=topin, file=trim(topfile), form='formatted', &
               status='old', iostat=ierr)
          if (ierr /= 0) call mp_error('Failure opening topology file',ierr)
          write(stdout,*) 'Using topology from file: ', trim(topfile)
          topchannel = topin
       end if

       if (trim(posfile) == 'internal') then
          if (is_restart()) then
             write(stdout,*) 'Using internal geometry from restart'
             poschannel = resin
          else
             write(stdout,*) 'Using internal geometry from input'
             poschannel = stdin
          end if
       else if ((trim(inpformat) == 'xyz/xyz') .and. &
            (trim(posfile) == trim(topfile))) then
          call mp_error('Topology and geometry must be different files',0)
       else
          open(unit=geoin, file=trim(posfile), form='formatted', &
               status='old', iostat=ierr)
          if (ierr /= 0) call mp_error('Failure opening geometry file',ierr)
          write(stdout,*) 'Using geometry from file: ', trim(posfile)
          poschannel = geoin
       end if

       if (trim(inpformat) == 'lammps') then
          call mp_error('LAMMPS input format currently unsupported',ierr)
       else if (trim(inpformat) == 'xyz') then
          call xyz_topogeom(topchannel)
          if (topchannel == topin) close (unit=topin)
       else if (trim(inpformat) == 'xyz/xyz') then
          call xyz_topology(topchannel)
          if (topchannel == topin) close (unit=topin)
          call xyz_geometry(poschannel)
          if (poschannel == geoin) close (unit=geoin)
       else if (trim(inpformat) == 'psf/xyz') then
       else
          call mp_error('Unknown or unsupported input data format',ierr)
       end if
       call memory_print
    end if ! mp_ioproc()

    call atoms_replicate(maxtypes)
    ! call cell_replicate

  end subroutine sysinfo_read

  subroutine xyz_topogeom(channel)
    use atoms
    integer, intent(in) :: channel
    integer :: i, ierr, idx, natoms, thistype
    real(kind=dp), dimension(3) :: pos
    character(len=lilen) :: line
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
    character(len=lilen) :: line
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
    character(len=lilen) :: line

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

  ! write sysinfo parameters
  subroutine sysinfo_write
    use io
    use atoms, only: is_vel, xyz_write
!    use cell, only: cell_write
!    use memory, only: alloc_vec, clear_vec
    integer :: ierr
    character(len=lilen) :: tmp_topfile, tmp_posfile, tmp_velfile

    ! restart is only written by io task
    if (mp_ioproc()) then
       tmp_topfile=topfile
       tmp_posfile=posfile
       tmp_velfile=velfile
       topfile='internal'
       posfile='internal'
       if (is_vel()) then
          velfile='internal'
       else
          velfile='unknown'
       end if
       write(stdout,*) 'Writing &sysinfo namelist to restart'
       write(resout,nml=sysinfo,iostat=ierr)
       if (ierr /= 0) call mp_error('Failure writing &sysinfo namelist',ierr)

       if (trim(inpformat) == 'lammps') then
          call mp_error('LAMMPS input format currently unsupported',ierr)
       else if (trim(inpformat) == 'xyz') then
          call xyz_write(resout,'pos')
          if (is_vel()) call xyz_write(resout,'vel')
       else if (trim(inpformat) == 'xyz/xyz') then
          call xyz_write(resout,'pos')
          call xyz_write(resout,'pos')
          if (is_vel()) call xyz_write(resout,'vel')
       else if (trim(inpformat) == 'psf/xyz') then
       else
          call mp_error('Unknown or unsupported input data format',ierr)
       end if

       topfile=tmp_topfile
       posfile=tmp_posfile
       velfile=tmp_velfile
    end if
  end subroutine sysinfo_write

end module sysinfo_io
