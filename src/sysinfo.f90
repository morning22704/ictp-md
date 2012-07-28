! module for global simulation system data information
module sysinfo
  use messages, only: myrank, ioproc, bcast, error
  implicit none

  private
  public :: init_sysinfo, read_sysinfo, comm_sysinfo, print_sysinfo
  integer, public :: natoms, ntypes, nobjs, nafirst, nalast

contains

  ! set default values
  subroutine init_sysinfo
    natoms  = -1
    ntypes  = 1
    nobjs   = 1
    nafirst = 1
    nalast  = -1
  end subroutine init_sysinfo

  ! read and check sysinfo parameters
  subroutine read_sysinfo(channel)
    integer, intent(in) :: channel

    namelist /sysinfo/ natoms, ntypes, nobjs, nafirst, nalast

    ! input is only read by io taks
    if (myrank /= ioproc) return

    read(channel,nml=sysinfo)
    if (natoms < 1) then
       call error('invalid or unset natoms parameter in &sysinfo namelist')
    end if
    if (ntypes < 1) then
       call error('invalid or ntypes parameter in &sysinfo namelist')
    end if
    if (nalast < 0) nalast = natoms
  end subroutine read_sysinfo

  subroutine comm_sysinfo
    call bcast(natoms)
    call bcast(ntypes)
    call bcast(nobjs)
    call bcast(nafirst)
    call bcast(nalast)
  end subroutine comm_sysinfo

  subroutine print_sysinfo(channel)
    integer, intent(in) :: channel

    namelist /sysinfo/ natoms, ntypes, nobjs, nafirst, nalast

    if (myrank /= ioproc) return
    write(channel,nml=sysinfo)
  end subroutine print_sysinfo
end module sysinfo
