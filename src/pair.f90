!> Meta-module for managing pairwise interactions
!!
!! This module is a wrapper around all non-bonded interactions.
!! It does some common tasks and then delegates the computation
!! to the individual pair modules.
module pair_io
  use kinds
  use constants
  use message_passing, only : mp_error, mp_ioproc, mp_bcast
  use pair_lj_cut
  implicit none

  private
  real(kind=dp) :: cutoff_pair !< Global cutoff for pairwise interactions
  real(kind=dp) :: cutoff_coul !< Global cutoff for coulomb interactions
  real(kind=dp) :: cutoff_max  !< Largest pairwise cutoff overall
  logical       :: shift_pot   !< Shift potential to be 0 at cutoff
  logical       :: do_coulomb  !< Calculate coulomb interactions
  character(len=lblen) :: pair_style !< String indicating the type of potential
  character(len=lblen) :: coul_style !< String indicating the type of potential

  namelist /pair/ cutoff_pair, cutoff_coul, shift_pot, do_coulomb, &
       pair_style, coul_style

  public :: pair_init, pair_read, pair_write
  public :: get_max_cutoff

contains

  !> Initialize module and submodules
  subroutine pair_init
    use io, only: stdout,separator
    use memory, only: adjust_mem

    cutoff_pair = -d_one
    cutoff_coul = -d_one
    cutoff_max  = -d_one
    call adjust_mem(3*dp)
    shift_pot   = .false.
    do_coulomb  = .false.
    call adjust_mem(2*sp)
    pair_style  = 'unknown'
    coul_style  = 'unknown'
    call adjust_mem(2*lblen)

    call pair_lj_cut_init
    write(stdout,*) separator
  end subroutine pair_init

  !> Read input
  subroutine pair_read
    use io
    use control_io, only : is_restart
    use atoms, only : is_chg, get_ntypes
    use memory, only : memory_print
    integer :: ierr, ntypes

    ! input is only read by io task
    if (mp_ioproc()) then

       ntypes = get_ntypes()
       if (is_chg()) then
          ! make default for coulomb calculation depend
          ! on whether we have charges in the system
          do_coulomb = .true.
       end if

       if (is_restart()) then
          write(stdout,*) 'Reading &pair namelist from restart'
          read(resin,nml=pair,iostat=ierr)
          if (ierr /= 0) call mp_error('Failure reading &pair namelist',ierr)
       end if

       write(stdout,*) 'Reading &pair namelist from input'
       read(stdin,nml=pair,iostat=ierr)
       if (ierr /= 0) call mp_error('Failure reading &pair namelist',ierr)

       if (cutoff_pair < d_zero) &
            call mp_error('Global pair cutoff must be set',1)
       if (trim(pair_style) == 'unknown') &
            call mp_error('Pair style must be set',1)

       cutoff_max = cutoff_pair
       ! coulomb specific checks
       if (do_coulomb) then
          if (cutoff_coul < d_zero) &
            call mp_error('Global coulomb cutoff must be set',1)
          if (trim(coul_style) == 'unknown') &
               call mp_error('Coulomb style must be set',1)
          if (cutoff_coul > cutoff_max) cutoff_max = cutoff_coul
       end if

       write(stdout,*) separator
       write(stdout,*) 'Pair style            : ', trim(pair_style)
       write(stdout,*) 'Global pair cutoff    : ', cutoff_pair
       if (do_coulomb) then
          write(stdout,*) 'Coulomb style         : ', trim(coul_style)
          write(stdout,*) 'Global coulomb cutoff : ', cutoff_coul
       end if
       if (shift_pot) then
          write(stdout,*) 'Pair potential shifted to zero at cutoff'
       else
          write(stdout,*) 'Unshifted pair potential'
       end if

       call memory_print

    end if

    call mp_bcast(cutoff_pair)
    call mp_bcast(cutoff_coul)
    call mp_bcast(cutoff_max)
    call mp_bcast(shift_pot)
    call mp_bcast(do_coulomb)
    call mp_bcast(pair_style)
    call mp_bcast(coul_style)

    if (trim(pair_style) == 'lj/cut') then
       call pair_lj_cut_read(ntypes,cutoff_pair,cutoff_max,shift_pot)
    else
       call mp_error('Unsupported pair style',1)
    end if

  end subroutine pair_read

  !> Write info for pair module
  subroutine pair_write
    use io, only: resout, stdout
    use atoms, only: get_ntypes
    integer :: ierr,ntypes

    ntypes = get_ntypes()
    if (mp_ioproc()) then
       write(stdout,*) 'Writing &pair namelist to restart'
       write(unit=resout, nml=pair, iostat=ierr)
       if (ierr /= 0) call mp_error('Failure writing &pair restart',ierr)

    if (trim(pair_style) == 'lj/cut') then
       call pair_lj_cut_write(resout,ntypes)
    else
       call mp_error('Unsupported pair style',1)
    end if

    endif
  end subroutine pair_write

  function get_max_cutoff()
    real(kind=dp) :: get_max_cutoff

    get_max_cutoff = cutoff_max
  end function get_max_cutoff

end module pair_io
