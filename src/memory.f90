! module with custom memory allocator and deallocators 
module memory
  use kinds
  use constants
  use io, only : separator
  implicit none

  private
  real(kind=dp) :: total_mem

  public :: memory_init, memory_print, adjust_mem, get_mem
  public :: alloc_vec, free_vec

  interface alloc_vec
     module procedure alloc_xyz_vec
     module procedure alloc_dp_vec
     module procedure alloc_int_vec
  end interface

  interface free_vec
     module procedure free_xyz_vec
     module procedure free_dp_vec
     module procedure free_int_vec
  end interface
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
    write(channel,*) separator
  end subroutine memory_print

  ! allocate memory for vector types
  subroutine alloc_xyz_vec(vec,num)
    type(xyz_vec), intent(inout) :: vec
    integer, intent(in) :: num

    allocate(vec%x(num),vec%y(num),vec%z(num))
    vec%size = num
    call adjust_mem(3*num*dp)
  end subroutine alloc_xyz_vec

  subroutine alloc_dp_vec(vec,num)
    type(dp_vec), intent(inout) :: vec
    integer, intent(in) :: num

    allocate(vec%v(num))
    vec%size = num
    call adjust_mem(num*dp)
  end subroutine alloc_dp_vec

  subroutine alloc_int_vec(vec,num)
    type(int_vec), intent(inout) :: vec
    integer, intent(in) :: num

    allocate(vec%v(num))
    vec%size = num
    call adjust_mem(num*sp)
  end subroutine alloc_int_vec

  ! free memory for vector types
  subroutine free_xyz_vec(vec)
    type(xyz_vec), intent(inout) :: vec

    deallocate(vec%x,vec%y,vec%z)
    call adjust_mem(-3*vec%size*dp)
    vec%size = 0
  end subroutine free_xyz_vec

  subroutine free_dp_vec(vec)
    type(dp_vec), intent(inout) :: vec

    deallocate(vec%v)
    call adjust_mem(-vec%size*dp)
    vec%size = 0
  end subroutine free_dp_vec

  subroutine free_int_vec(vec)
    type(int_vec), intent(inout) :: vec

    deallocate(vec%v)
    call adjust_mem(-vec%size*sp)
    vec%size = 0
  end subroutine free_int_vec

end module memory
