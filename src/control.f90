!
! FIXME: add documentation section here that documents the
! entire &control section, but shows up in the user's guide
! parts, not the developer's reference

!> Module to control basic simulation parameters
module control_io
  use constants
  use kinds
  use io, only : stdout
  use message_passing, only: mp_ioproc, mp_bcast, mp_error
  implicit none

  private
  integer :: initial_step   !< Number of the first MD step of the trajectory
  integer :: last_step      !< Number of the last MD step of the trajectory
  integer :: current_step   !< Number of the current MD step
  integer :: run_step       !< Number of MD steps in this run
  integer :: end_step       !< Scheduled last step of this run
  integer :: seq_no         !< Sequence number of run in trajectory
  real(kind=dp) :: max_time !< Maximal wall time for this run
  logical :: restart        !< Flag to trigger reading in a restart
  logical :: debug          !< Flag to trigger debug output
  character(len=120) :: restfile !< Path to restart file
  character(len=120) :: prefix   !< Prefix

  namelist /control/ initial_step, current_step, last_step, run_step, &
       seq_no, max_time, restart, debug, restfile, prefix

  public :: control_init, control_read, control_write
  public :: is_restart, is_debug
!  public :: get_step

contains

  !> Set default values for control module and namelist variables
  subroutine control_init
    use memory, only: adjust_mem

    initial_step = -1
    current_step = -1
    last_step    = -1
    run_step     =  0
    end_step     = -1
    seq_no       = -1
    max_time     = -d_one
    restart      = .false.
    debug      = .true.
    restfile     = 'unknown'
    prefix       = 'mdrun'
    call adjust_mem(6*sp+dp+2*sp+2*(120+sp)) ! global memory use of module
  end subroutine control_init

  !> Read &control namelist
  !!
  !! The &control namelist is the first namelist to be read in
  !! and as such it differs from all other namelists as it is
  !! read in before any equivalent namelist section is read in
  !! from a restart file. For all other namelists, the order is
  !! different, i.e. the restart is read in first and then the
  !! settings in the input file can override the restart settings.
  subroutine control_read
    use io, only : stdin, resin
    use memory, only : memory_print
    integer :: ierr
    integer :: tmp_initial, tmp_current, tmp_last, tmp_run, tmp_seq
    logical :: tmp_debug
    real(kind=dp) :: tmp_max

    ! input is only read by io task
    if (mp_ioproc()) then

       write(stdout,*) 'Reading &control namelist from input'
       read(unit=stdin,nml=control,iostat=ierr)
       if (ierr /= 0) call mp_error('Failure reading &control namelist',ierr)

       ! for the control namelist, the restart overrides the input
       if (restart) then

          ! save data from input
          tmp_initial = initial_step
          tmp_current = current_step
          tmp_last    = last_step
          tmp_run     = run_step
          tmp_max     = max_time
          tmp_seq     = seq_no
          tmp_debug = debug

          if (trim(restfile) == 'unknown') then
             call mp_error('Set "restfile" to name of restart file',15)
          end if

          ! open restart channel and verify validity and version
          open(unit=resin, file=TRIM(restfile), form='formatted', &
               access='sequential', action='read', status='old', iostat=ierr)
          if (ierr /= 0) call mp_error('Failure opening restart file',ierr)

          write(stdout,*) 'Reading &control namelist from restart'
          read(unit=resin, nml=control, iostat=ierr)
          if (ierr /= 0) call mp_error('Failure reading &control restart',ierr)

          ! the sequence number needs to be stepped up
          seq_no = seq_no + 1
          ! override restart data from input as needed
          if (tmp_initial >= 0)  initial_step = tmp_initial
          if (tmp_current >= 0)  current_step = tmp_current
          if (tmp_last >= 0)     last_step    = tmp_last
          if (tmp_run > 0)       run_step     = tmp_run
          if (tmp_max >= d_zero) max_time     = tmp_max
          if (tmp_seq >= 0)      seq_no       = tmp_seq
          restart = .true.
          debug = tmp_debug
          if (current_step < initial_step) &
               call mp_error('current_step must be larger than initial_step',1)
       endif

       ! set defaults for unset parameters and sanitize choices
       if (initial_step < 0) initial_step = 0
       if (current_step < 0) current_step = 0
       if (run_step < 0) run_step = 0
       if (current_step < initial_step) current_step = initial_step
       end_step = current_step + run_step
       if (last_step > 0) then
          if (last_step < current_step) &
               call mp_error('last_step must be -1 or larger than current_step',1)
          if (last_step < end_step) end_step = last_step
       end if
       if (seq_no < 0) seq_no = 0
       call control_print
       call memory_print
    end if ! mp_ioproc()

    ! broadcast info that is needed on all processes
    call mp_bcast(current_step)
    call mp_bcast(end_step)
    call mp_bcast(debug)
  end subroutine control_read

  !> Flag if input data is to be read from a restart
  !! @returns True if we need to read a restart
  function is_restart()
    logical is_restart
    is_restart = restart
  end function is_restart

  !> Flag if debug output is requested
  !! @returns True if we need to be debug
  function is_debug()
    logical is_debug
    is_debug = debug
  end function is_debug

  !> Print run information from control module
  subroutine control_print
    use io, only : separator

    if (mp_ioproc()) then
       write(stdout,*) separator
       write(stdout,*) 'Trajectory name prefix: ', trim(prefix)
       if (debug) write(stdout,*) 'Debug output is requested '
       if (restart) write(stdout,*) 'Restart read from file: ', trim(restfile)
       write(stdout,*) 'Trajectory begins at step:   ', initial_step
       if (last_step > 0) &
            write(stdout,*) 'Trajectory finishes at step: ', last_step
       write(stdout,*) 'Sequence no. in trajectory:  ', seq_no
       write(stdout,*) 'Trajectory currently at step:',current_step
       write(stdout,*) 'Run scheduled to end at step:',end_step
       write(stdout,*) separator
    endif
  end subroutine control_print

  !> Write restart info for control module
  !! @param channel I/O channel of restart writing unit
  subroutine control_write(level)
    use io, only: resout
    integer, intent(in) :: level
    integer :: ierr
    logical :: tmp_restart
    character(len=255) :: filename

    tmp_restart = restart
    restart = .false.

    if (level > 0) then
       write(filename,fmt='(A,A,I1)') trim(prefix), '.restart.', level
    else
       write(filename,fmt='(A,A)') trim(prefix), '.restart'
    end if

    if (mp_ioproc()) then
       open(unit=resout, file=TRIM(filename), form='formatted', &
            access='sequential', action='write', status='unknown', iostat=ierr)
       if (ierr /= 0) call mp_error('Failure opening restart file',ierr)

       write(stdout,*) 'Writing &control namelist from restart'
       write(unit=resout, nml=control, iostat=ierr)
       if (ierr /= 0) call mp_error('Failure writing &control restart',ierr)
    endif
    restart = tmp_restart
  end subroutine control_write
end module control_io
