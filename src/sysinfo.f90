!
! FIXME: add documentation section here that documents the
! entire &sysinfo section, but shows up in the user's guide
! parts, not the developer's reference

!> Module to manage data of the current system
module sysinfo_io
  use kinds
  use constants
  use atoms,           only: ndeftypes
  use message_passing, only: mp_ioproc, mp_bcast, mp_error
  use control_io,      only: is_restart
  implicit none

  private
  integer :: maxtypes                !< maximum number of atom types in run
  integer :: neigh_level             !< level of cell division
  integer :: neigh_delay             !< number of steps until neighbor update
  integer :: neigh_every             !< frequency of neighbor update checks
  logical :: neigh_check             !< check if neighbor updates are needed
  logical :: ortho_cell              !< Cell is orthogonal
  logical :: newton                  !< employ newton's 3rd law
  real(kind=dp) :: cellparam(6)         !< a, b, c, alpha, beta, gamma
  real(kind=dp) :: origin(3)            !< simulation cell origin
  real(kind=dp) :: neigh_skin           !< neighborlist skin distance
  real(kind=dp) :: neigh_ratio          !< max actual/average atoms/cell ratio
  real(kind=dp) :: defmass(ndeftypes)   !< default masses for types
  real(kind=dp) :: defcharge(ndeftypes) !< default charges for types
  character(len=lblen) :: deftype(ndeftypes) !< default atom labels
  character(len=lblen) :: inpformat  !< format of topology/geometry input
  character(len=lilen) :: topfile    !< name of topology file
  character(len=lilen) :: posfile    !< name of geometry file
  character(len=lilen) :: velfile    !< name of velocity file

  namelist /sysinfo/ maxtypes, neigh_level, neigh_delay, neigh_every, &
       neigh_check, ortho_cell, newton, &
       origin, cellparam, neigh_skin, neigh_ratio, defmass, defcharge, &
       deftype, inpformat, topfile, posfile, velfile

  public :: sysinfo_init, sysinfo_read, sysinfo_write
  public :: get_neigh_nlevel, get_neigh_ratio, get_neigh_skin, get_neigh_check
  public :: get_newton

contains

  !> Set defaults for the sysinfo module
  subroutine sysinfo_init
    use cell,   only: cell_init
    use memory, only: adjust_mem

    maxtypes = ndeftypes
    neigh_level = 1
    neigh_delay = 0
    neigh_every = 1
    call adjust_mem(4*sp)
    ortho_cell = .true.
    neigh_check = .true.
    newton = .true.
    call adjust_mem(3*sp)
    neigh_skin = d_one
    neigh_ratio = 2.5_dp
    origin(:) = d_zero
    cellparam(:) = d_zero
    cellparam(4:6) = 90.0_dp
    call adjust_mem((1+3+6)*dp)
    defmass(:) = d_zero
    defcharge(:) = d_zero
    call adjust_mem(2*ndeftypes*dp)
    deftype(:) = 'unknown'
    call adjust_mem(ndeftypes*lblen)
    inpformat  = 'unknown'
    call adjust_mem(1*(lblen+sp))
    topfile = 'unknown'
    posfile = 'unknown'
    velfile = 'unknown'
    call adjust_mem(3*(lilen+sp))
    call cell_init
  end subroutine sysinfo_init

  !> Read in input for the sysinfo module
  subroutine sysinfo_read
    use io
    use atoms, only: atoms_init, atoms_replicate, types_init, get_natoms
    use cell, only: set_cell, cell_replicate
    use memory, only: alloc_vec, clear_vec, memory_print
    integer :: ierr, topchannel, poschannel, velchannel

    ! use illegal unit numbers as default to trigger errors
    topchannel = -1
    poschannel = -1
    velchannel = -1

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
       write(stdout,*) 'Max. number of atom types : ', maxtypes
       if (newton) then
          write(stdout,*) 'Using Newton''s 3rd law    : Enabled'
       else
          write(stdout,*) 'Using Newton''s 3rd law    : Disabled'
       end if
       write(stdout,*) 'System info format        : ', trim(inpformat)
       write(stdout,*) 'Topology from             : ', trim(topfile)
       write(stdout,*) 'Positions from            : ', trim(posfile)
       write(stdout,*) 'Velocities from           : ', trim(velfile)
       write(stdout,*) separator

       ! initialize system storage
       call atoms_init(maxtypes)
       call types_init(deftype,maxtypes)
       call set_cell(origin,cellparam,ortho_cell)

       if (trim(topfile) == 'internal') then
          if (is_restart()) then
             write(stdout,*) 'Reading internal topology from restart'
             topchannel = resin
          else
             write(stdout,*) 'Reading internal topology from input'
             topchannel = stdin
          end if
       else
          open(unit=topin, file=trim(topfile), form='formatted', &
               status='old', iostat=ierr)
          if (ierr /= 0) call mp_error('Failure opening topology file',ierr)
          write(stdout,*) 'Reading topology file     : ', trim(topfile)
          topchannel = topin
       end if

       if (trim(posfile) == 'internal') then
          if (is_restart()) then
             write(stdout,*) 'Reading internal geometry from restart'
             poschannel = resin
          else
             write(stdout,*) 'Reading internal geometry from input'
             poschannel = stdin
          end if
       else if (trim(inpformat) == 'xyz') then
             write(stdout,*) 'Reading geometry together with topology'
       else if ((trim(inpformat) == 'xyz/xyz') .and. &
            (trim(posfile) == trim(topfile))) then
          call mp_error('Topology and geometry must be different files',0)
       else
          open(unit=geoin, file=trim(posfile), form='formatted', &
               status='old', iostat=ierr)
          if (ierr /= 0) call mp_error('Failure opening geometry file',ierr)
          write(stdout,*) 'Reading geometry file     : ', trim(topfile)
          poschannel = geoin
       end if

       if (trim(velfile) == 'internal') then
          if (is_restart()) then
             write(stdout,*) 'Reading internal velocities from restart'
             velchannel = resin
          else
             write(stdout,*) 'Reading internal velocities from input'
             velchannel = stdin
          end if
       else
          if (trim(velfile) /= 'unknown') then
             open(unit=velin, file=trim(velfile), form='formatted', &
                  status='old', iostat=ierr)
             if (ierr /= 0) &
                  call mp_error('Failure opening velocities file',ierr)
             write(stdout,*) 'Reading velocity file     : ', trim(velfile)
             velchannel = velin
          end if
       end if

       if (trim(inpformat) == 'lammps') then
          call mp_error('LAMMPS input format currently unsupported',ierr)
       else if (trim(inpformat) == 'xyz') then
          call xyz_topogeom(topchannel)
          if (topchannel == topin) close (unit=topin)
          if (trim(velfile) /= 'unknown') then
             call xyz_velocity(velchannel)
             if (velchannel == velin) close (unit=velin)
          end if
       else if (trim(inpformat) == 'xyz/xyz') then
          call xyz_topology(topchannel)
          if (topchannel == topin) close (unit=topin)
          call xyz_geometry(poschannel)
          if (poschannel == geoin) close (unit=geoin)
          if (trim(velfile) /= 'unknown') then
             call xyz_velocity(velchannel)
             if (velchannel == velin) close (unit=velin)
          end if
       else if (trim(inpformat) == 'psf/xyz') then
          call xyz_geometry(poschannel)
          if (poschannel == geoin) close (unit=geoin)
          if (trim(velfile) /= 'unknown') then
             call xyz_velocity(velchannel)
             if (velchannel == velin) close (unit=velin)
          end if
       else
          call mp_error('Unknown or unsupported input data format',ierr)
       end if

       write(stdout,*) 'Total number of atoms     : ', get_natoms()
       if (ortho_cell) then
          write(stdout,*) 'Cell geometry             : Orthogonal'
       else
          write(stdout,*) 'Cell geometry             : Triclinic'
       end if
       call memory_print
    end if ! mp_ioproc()

    call mp_bcast(neigh_level)
    call mp_bcast(neigh_delay)
    call mp_bcast(neigh_every)
    call mp_bcast(neigh_check)
    call mp_bcast(neigh_skin)
    call mp_bcast(newton)
    call cell_replicate
    call atoms_replicate

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
    call atoms_setup(natoms,newton)

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
    call atoms_setup(natoms,newton)

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

  subroutine xyz_velocity(channel)
    use atoms, only: get_natoms, set_vel
    integer, intent(in) :: channel
    integer :: i, idx, ierr, natoms
    real(kind=dp), dimension(3) :: vel
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
       read(line,fmt=*,iostat=ierr) vel(1),vel(2),vel(3)
       if (ierr /= 0) &
            call mp_error('Failure to read velocities from xyz file',ierr)
       call set_vel(i,vel)
    end do
  end subroutine xyz_velocity

  ! write sysinfo parameters
  subroutine sysinfo_write
    use io
    use atoms, only: is_vel, xyz_write
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

  function get_neigh_nlevel()
    integer :: get_neigh_nlevel
    get_neigh_nlevel = neigh_level
  end function get_neigh_nlevel

  function get_neigh_ratio()
    real(kind=dp) :: get_neigh_ratio
    get_neigh_ratio = neigh_ratio
  end function get_neigh_ratio

  function get_neigh_skin()
    real(kind=dp) :: get_neigh_skin
    get_neigh_skin = neigh_skin
  end function get_neigh_skin

  function get_neigh_check()
    logical :: get_neigh_check
    get_neigh_check = neigh_check
  end function get_neigh_check

  function get_newton()
    logical :: get_newton
    get_newton = newton
  end function get_newton

end module sysinfo_io
