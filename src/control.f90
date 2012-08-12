!> Module to control basic simulation parameters
module control_io
  use io, only : resin, separator
  use constants
  use kinds
  use memory, only: adjust_mem
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
  character(len=120) :: restfile !< Path to restart file
  character(len=120) :: prefix   !< Prefix

  namelist /control/ initial_step, current_step, last_step, run_step, &
       seq_no, max_time, restart, restfile, prefix
  
  public :: control_init, control_read, control_print
  public :: use_restart
!  public :: get_step

contains

  !> Set default values for control module and namelist variables
  subroutine control_init
    initial_step = -1
    current_step = -1
    last_step    = -1
    run_step     =  0
    end_step     = -1
    seq_no       = -1
    max_time     = -d_one
    restart      = .false.
    restfile     = 'unknown'
    prefix       = 'mdrun'
    call adjust_mem(6*sp+dp+sp+2*(120+sp)) ! global memory use of module
  end subroutine control_init
  
  !> Read &control namelist
  !! @param channel I/O channel to read namelist from
  !!
  !! The &control namelist is the first namelist to be read in
  !! and as such it differs from all other namelists as it is
  !! read in before any equivalent namelist section is read in
  !! from a restart file. For all other namelists, the order is
  !! different, i.e. the restart is read in first and then the
  !! settings in the input file can override the restart settings.
  subroutine control_read(channel)
    integer, intent(in) :: channel
    integer :: ierr
    integer :: tmp_initial, tmp_current, tmp_last, tmp_run, tmp_seq
    real(kind=dp) :: tmp_max

    ! input is only read by io task
    if (mp_ioproc()) then

       read(unit=channel,nml=control,iostat=ierr)
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

          ! open restart channel and verify validity and version
          open(unit=resin, file=TRIM(restfile), form='formatted', &
               access='sequential', action='read', status='old', iostat=ierr)
          if (ierr /= 0) call mp_error('Failure opening restart file',ierr)

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
          if (current_step < initial_step) &
               call mp_error('current_step must be larger than initial_step',1)
       endif

       ! set defaults for unset parameters and sanitize choices
       if (initial_step < 0) initial_step = 0
       if (current_step < 0) current_step = 0
       if (current_step < initial_step) current_step = initial_step
       end_step = current_step + run_step
       if (last_step > 0) then
          if (last_step < current_step) &
               call mp_error('last_step must be -1 or larger than current_step',1)
          if (last_step < end_step) end_step = last_step
       end if
       if (seq_no < 0) seq_no = 0
    end if ! mp_ioproc()

    ! broadcast info that is needed on all processes
    call mp_bcast(current_step)
    call mp_bcast(end_step)
  end subroutine control_read

  !> Flag if input data is to be read from a restart
  !! @returns True if we need to read a restart
  function use_restart()
    logical use_restart
    use_restart = restart
  end function use_restart

  !> Print run information from control module
  !! @param channel I/O channel for screen or log file
  subroutine control_print(channel)
    integer, intent(in) :: channel

    if (mp_ioproc()) then
       write(channel,*) 'Trajectory name prefix:      ', TRIM(prefix)
       write(channel,*) 'Trajectory begins at step:   ', initial_step
       if (last_step > 0) &
            write(channel,*) 'Trajectory finishes at step: ', last_step
       write(channel,*) 'Sequency no. in trajectory:  ', seq_no
       write(channel,*) 'Trajectory currently at step:',current_step
       write(channel,*) 'Run scheduled to end at step:',end_step
       write(channel,*) separator
    endif
  end subroutine control_print

  !> Write restart info for control module
  !! @param channel I/O channel of restart writing unit
  subroutine control_restart(channel)
    integer, intent(in) :: channel
    integer :: ierr

    restart = .false.
    if (mp_ioproc()) then
       write(unit=channel,nml=control,iostat=ierr)
    endif
  end subroutine control_restart
end module control_io
