! module to manage data of the current system
module sysinfo
  use kinds
  use memory, only: adjust_mem
  use message_passing, only: mp_ioproc, mp_bcast, mp_error
  implicit none

  private
  integer :: natoms, ntypes, nbonds, nbtypes, nangles, natypes
  type (xyz_vec) :: pos, vel, for, pos_s
  type (dp_vec)  :: chrg

  public :: sysinfo_init, sysinfo_read, sysinfo_comm, sysinfo_print
  !, sysinfo_restart
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
    pos%size   = -1
    vel%size   = -1
    for%size   = -1
    pos_s%size = -1
    chrg%size  = -1
    call adjust_mem(6*sp+4*(3*dp+sp)+(dp+sp))
  end subroutine sysinfo_init
  
  ! read sysinfo parameters
  subroutine sysinfo_read(channel)
    integer, intent(in) :: channel

    namelist /sysinfo/ natoms, ntypes, nbonds, nbtypes, nangles, natypes

    ! input is only read by io task
    if (.not. mp_ioproc()) return

    read(channel,nml=sysinfo)
  end subroutine sysinfo_read

  subroutine sysinfo_comm
    call mp_bcast(natoms)
    call mp_bcast(ntypes)
    call mp_bcast(nbonds)
    call mp_bcast(nbtypes)
    call mp_bcast(nangles)
    call mp_bcast(natypes)
  end subroutine sysinfo_comm

  subroutine sysinfo_print(channel)
    integer, intent(in) :: channel

    namelist /sysinfo/ natoms, ntypes, nbonds, nbtypes, nangles, natypes

    if (.NOT. mp_ioproc()) return
    write(channel,nml=sysinfo)
  end subroutine sysinfo_print
end module sysinfo
