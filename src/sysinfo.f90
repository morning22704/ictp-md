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
  use atoms
  use cell
  implicit none

  private
  integer :: natoms, ntypes
  real(kind=dp) :: xlo, xhi, ylo, yhi, zlo, zhi, txy, txz, tyz
  character(len=8) :: inputfmt
  character(len=255) :: topofile, geomfile

  namelist /sysinfo/ xlo, xhi, ylo, yhi, zlo, zhi, txy, txz, tyz, &
       inputfmt, topofile, geomfile

  public :: sysinfo_init, sysinfo_read
!  public :: get_natoms, get_ntypes

contains

  ! set default values
  subroutine sysinfo_init
    use memory, only: adjust_mem

    call atoms_init
    call cell_init

    natoms     = -1
    ntypes     = -1
    call adjust_mem(2*sp)
    xlo = d_zero
    xhi = d_zero
    ylo = d_zero
    yhi = d_zero
    zlo = d_zero
    zhi = d_zero
    txy = -d_one
    txz = -d_one
    tyz = -d_one
    call adjust_mem(9*dp)
    inputfmt = 'unknown'
    call adjust_mem(8+sp)
    topofile = 'unknown'
    geomfile = 'unknown'
    call adjust_mem(2*(255+sp))
  end subroutine sysinfo_init
  
  ! read sysinfo parameters
  subroutine sysinfo_read
    use io, only: stdin, stdout, resin
    use memory, only: alloc_vec, clear_vec
    integer :: nthr, ierr, extpos
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

       if (trim(inputfmt) == 'lammps') then
       else if (trim(inputfmt) == 'xyz/xyz') then
       else if (trim(inputfmt) == 'psf/xyz') then
       else
          call mp_error('Unknown or unsupported input data format',ierr)
       end if
    end if ! mp_ioproc()

    ! broadcast basic system info
    call mp_bcast(natoms)
    call mp_bcast(ntypes)

  end subroutine sysinfo_read

  subroutine read_lammps_topology(channel)
    use io, only : stdout
    implicit none
    integer, intent(in) :: channel
    integer :: i, ierr
    character(len=255) :: line

    if (use_restart()) then
       write(stdout,*) 'Reading &topology section in psf format'
       ierr=0
       do while (ierr == 0) 
          read(channel,fmt='(A)',iostat=ierr) line
          if (ierr /= 0) call mp_error('Failure finding &topology section',ierr)
          if ((index(line,'&topology') /= 0) &
               .or. (index(line,'&TOPOLOGY') /= 0)) ierr=1
       end do
    else
       write(stdout,*) 'Reading topology data from psf file'
    end if

    read(channel,fmt=*,err=100) natoms
    read(channel,fmt='(A)',iostat=ierr) line

    if (use_restart()) read(channel,fmt='(A)',iostat=ierr) line
    return
100 continue
    call mp_error('Premature end of psf data',100)
  end subroutine read_lammps_topology

end module sysinfo_io
