! module for reading input 
module read_input
  use sysinfo
!  use runinfo
!  use output
  implicit none

  private
  public :: input_init, input_read, input_print

contains

  ! set default values
  subroutine input_init
    call sysinfo_init
  end subroutine input_init

  ! read various input file sections
  subroutine input_read(channel)
    integer, intent(in) :: channel

    call sysinfo_read(channel)
  end subroutine input_read

  ! echo input data to output
  subroutine input_print(channel)
    integer, intent(in) :: channel

    if (channel > 0) then
       call sysinfo_print(channel)
    end if
  end subroutine input_print
end module read_input
