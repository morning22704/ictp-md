!> Meta-module for managing pairwise interactions
!!
!! This module is a wrapper around all non-bonded interactions.
!! It does some common tasks and then delegates the computation
!! to the individual pair modules.
module pair_io
  use kinds
  use constants
  use message_passing, only : mp_error, mp_ioproc, mp_bcast
  implicit none

  private
  logical, save :: need_init = .true. !< Flag whether to run initializers
  real(kind=dp) :: cutoff_pair !< Global cutoff for pairwise interactions
  real(kind=dp) :: cutoff_coul !< Global cutoff for coulomb interactions
  real(kind=dp) :: cutoff_max  !< Largest pairwise cutoff overall
  logical       :: shift_pot   !< Shift potential to be 0 at cutoff
  character(len=lblen) :: pair_style !< String indicating the type of potential
  
  public :: pair_read, pair_write
  namelist /pair/ cutoff_pair, cutoff_coul, shift_pot, pair_style

contains

  !> Initialize submodules
  subroutine pair_init

    cutoff_pair = -d_one
    cutoff_coul = -d_one
    cutoff_max  = -d_one
    shift_pot   = .false.
    pair_style  = 'unknown'
    need_init = .false.
  end subroutine pair_init

  !> Read input 
  subroutine pair_read
    use io
    use control_io, only : is_restart
    use atoms, only : is_chg
    use memory, only : memory_print
    integer :: ierr

    if (need_init) call pair_init

    ! input is only read by io task
    if (mp_ioproc()) then

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
       if (is_chg() .and. (cutoff_coul > cutoff_max)) cutoff_max = cutoff_coul

       write(stdout,*) separator
       write(stdout,*) 'Pair style: ', trim(pair_style)
       write(stdout,*) 'Global pair cutoff    : ', cutoff_pair
       if (is_chg()) &
            write(stdout,*) 'Global coulomb cutoff : ', cutoff_coul
       write(stdout,*) 'Maximal global cutoff : ', cutoff_max

       if (shift_pot) then
          write(stdout,*) 'Potential shifted to zero at cutoff'
       else
          write(stdout,*) 'Unshifted potential'
       end if

       write(stdout,*) separator
       call memory_print
    end if

    call mp_bcast(cutoff_pair)
    call mp_bcast(cutoff_coul)
    call mp_bcast(cutoff_max)

  end subroutine pair_read

  !> Write info for pair module
  subroutine pair_write
    use io, only: resout, stdout
    integer :: ierr

    if (mp_ioproc()) then
       write(stdout,*) 'Writing &pair namelist to restart'
       write(unit=resout, nml=pair, iostat=ierr)
       if (ierr /= 0) call mp_error('Failure writing &pair restart',ierr)
    endif
  end subroutine pair_write

end module pair_io
