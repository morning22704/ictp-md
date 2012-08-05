! module to manage data of the current system
module sysinfo
  use io, only : resin
  use kinds
  use constants
  use memory, only: adjust_mem, alloc_vec
  use message_passing, only: mp_ioproc, mp_bcast, mp_error
  use threading, only: thr_get_num_threads
  implicit none

  private
  integer :: natoms, ntypes, nbonds, nbtypes, nangles, natypes
  logical :: restart, update_pos_r, update_pos_s
  type (xyz_vec) :: pos, vel, for, pos_s
  type (dp_vec)  :: chg
  type (int_vec) :: typ
  character(len=255) :: topofile, geomfile, restfile
  integer, parameter :: rest_major=1, rest_minor=0

  public :: sysinfo_init, sysinfo_read, sysinfo_comm, sysinfo_print
  !  public :: sysinfo_restart
  public :: use_restart
!  public :: get_natoms, get_ntypes

contains

  ! set default values
  subroutine sysinfo_init
    natoms     = -1
    ntypes     = -1
    nbonds     = -1
    nbtypes    = -1
    nangles    = -1
    natypes    = -1
    call adjust_mem(6*sp)
    restart      = .false.
    update_pos_r = .false.
    update_pos_s = .false.
    call adjust_mem(3*sp)
    pos%size   = -1
    vel%size   = -1
    for%size   = -1
    pos_s%size = -1
    call adjust_mem(4*(3*dp+sp))
    chg%size   = -1
    typ%size   = -1
    call adjust_mem(2*(dp+sp))
    topofile = '(undefined)'
    geomfile = '(undefined)'
    restfile = '(undefined)'
    call adjust_mem(3*(255+sp))
  end subroutine sysinfo_init
  
  ! read sysinfo parameters
  subroutine sysinfo_read(channel)
    integer, intent(in) :: channel
    integer :: major, minor, nthr, i
    character(len=7) :: label
    namelist /sysinfo/ natoms, ntypes, nbonds, nbtypes, nangles, natypes, &
          topofile, geomfile, restfile

    ! input is only read by io task
    if (mp_ioproc()) then

       read(channel,nml=sysinfo)

       if (restart) then
          ! open restart channel and verify validity and version
          open(unit=resin, file=TRIM(restfile), form='unformatted', &
               action='read', status='old')
          read(resin) label, major, minor
          read(resin) natoms, ntypes
       else
          ! some consistency checks
          if (natoms <= 0) then
             call mp_error('natoms must be > 0 w/o restart')
          endif
          if (ntypes <= 0) then
             call mp_error('ntypes must be > 0 w/o restart')
          endif
       end if
    end if

    ! broadcast basic system info
    call mp_bcast(natoms)
    call mp_bcast(ntypes)

    ! allocate per atom storage
    nthr = thr_get_num_threads()
    call alloc_vec(pos,natoms)
    call alloc_vec(typ,natoms)
    call alloc_vec(chg,natoms)
    call alloc_vec(vel,natoms)
    call alloc_vec(for,nthr*natoms)

    if (restart) then
       read(resin) (typ%v(i),i=1,natoms)
       read(resin) (pos%x(i),i=1,natoms)
       read(resin) (pos%y(i),i=1,natoms)
       read(resin) (pos%z(i),i=1,natoms)
       read(resin) (chg%v(i),i=1,natoms)
       read(resin) (vel%x(i),i=1,natoms)
       read(resin) (vel%y(i),i=1,natoms)
       read(resin) (vel%z(i),i=1,natoms)       
    else 
       ! initialize charge and velocities to zero
       chg%v(:) = d_zero
       vel%x(:) = d_zero
       vel%y(:) = d_zero
       vel%z(:) = d_zero
       ! fixme: read types/charge from topofile, coordinates from geomfile
    end if

    call mp_bcast(typ)
    call mp_bcast(pos)
    call mp_bcast(chg)
    call mp_bcast(vel)
  end subroutine sysinfo_read

  subroutine sysinfo_comm
  end subroutine sysinfo_comm

  ! determine, if input data is to be read from a restart
  function use_restart()
    logical use_restart
    use_restart = restart
  end function use_restart

  subroutine sysinfo_print(channel)
    integer, intent(in) :: channel

    namelist /sysinfo/ natoms, ntypes, nbonds, nbtypes, nangles, natypes, &
         restart, topofile, geomfile, restfile
    if (.NOT. mp_ioproc()) return
    write(channel,nml=sysinfo)
  end subroutine sysinfo_print
end module sysinfo
