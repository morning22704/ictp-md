! module with custom memory allocator and deallocators 
module memory
  use kinds
  use constants
  implicit none

  private
  real(kind=dp) :: total_mem

  public :: memory_init, memory_print, adjust_mem, get_mem

contains

  ! "constructor"
  subroutine memory_init
    total_mem = d_zero
  end subroutine memory_init

  ! adjust total memory accounting
  subroutine adjust_mem(amount)
    integer, intent(in) :: amount

    total_mem = total_mem + dble(amount)
  end subroutine adjust_mem

  ! return accounted memory
  function get_mem()
    real(kind=dp) :: get_mem

    get_mem = total_mem
  end function get_mem

  ! print accounted memory
  subroutine memory_print(channel)
    integer, intent(in) :: channel

    write(channel,*) 'Total allocated memory in bytes: ', total_mem
  end subroutine memory_print

end module memory
