! module for reading input 
module sysinfo
  use message_passing, only: mp_ioproc, mp_bcast, mp_error
  implicit none

  private
  public :: init_sysinfo, read_sysinfo, comm_sysinfo, print_sysinfo
  integer, public :: natoms, ntypes, nobjs, nafirst, nalast

contains

  ! set default values
  subroutine init_sysinfo
    natoms  = -1
    ntypes  =  1
    nobjs   =  1
    nafirst =  1
    nalast  = -1
  end subroutine init_sysinfo

  ! read and check sysinfo parameters
  subroutine read_sysinfo(channel)
    integer, intent(in) :: channel

    namelist /sysinfo/ natoms, ntypes, nobjs, nafirst, nalast

    ! input is only read by io task
    if (.NOT. mp_ioproc()) return

    read(channel,nml=sysinfo)
    if (natoms < 1) then
       call mp_error('invalid or unset natoms parameter in &sysinfo namelist')
    end if
    if (ntypes < 1) then
       call mp_error('invalid or ntypes parameter in &sysinfo namelist')
    end if
    if (nalast < 0) nalast = natoms
  end subroutine read_sysinfo

  subroutine comm_sysinfo
    call mp_bcast(natoms)
    call mp_bcast(ntypes)
    call mp_bcast(nobjs)
    call mp_bcast(nafirst)
    call mp_bcast(nalast)
  end subroutine comm_sysinfo

  subroutine print_sysinfo(channel)
    integer, intent(in) :: channel

    namelist /sysinfo/ natoms, ntypes, nobjs, nafirst, nalast

    if (.NOT. mp_ioproc()) return
    write(channel,nml=sysinfo)
  end subroutine print_sysinfo
end module sysinfo
