! module with custom memory related operations, allocator, and deallocators
module memory
  use kinds
  use constants
  implicit none

  private
  real(kind=dp) :: total_mem

  public :: memory_init, memory_print, adjust_mem, get_mem
  public :: alloc_vec, clear_vec, free_vec

  interface alloc_vec
     module procedure alloc_xyz_vec
     module procedure alloc_dp_vec
     module procedure alloc_int_vec
     module procedure alloc_label_vec
  end interface

  interface clear_vec
     module procedure clear_xyz_vec
     module procedure clear_dp_vec
     module procedure clear_int_vec
     module procedure clear_label_vec
  end interface

  interface free_vec
     module procedure free_xyz_vec
     module procedure free_dp_vec
     module procedure free_int_vec
     module procedure free_label_vec
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
  subroutine memory_print
  use io, only : stdout, separator
    write(stdout,'(A,F20.3,A)') ' Assigned memory:            ', &
         total_mem/kbyte, ' kBytes'
    write(stdout,*) separator
  end subroutine memory_print

  ! allocate memory for vector types
  subroutine alloc_xyz_vec(vec,num)
    type(xyz_vec), intent(inout) :: vec
    integer, intent(in) :: num

    if (num == vec%size) return
    if (vec%size > 0) call free_vec(vec)
    allocate(vec%x(num),vec%y(num),vec%z(num))
    vec%size = num
    call adjust_mem(3*num*dp)
  end subroutine alloc_xyz_vec

  subroutine alloc_dp_vec(vec,num)
    type(dp_vec), intent(inout) :: vec
    integer, intent(in) :: num

    if (num == vec%size) return
    if (vec%size > 0) call free_vec(vec)
    allocate(vec%v(num))
    vec%size = num
    call adjust_mem(num*dp)
  end subroutine alloc_dp_vec

  subroutine alloc_int_vec(vec,num)
    type(int_vec), intent(inout) :: vec
    integer, intent(in) :: num

    if (num == vec%size) return
    if (vec%size > 0) call free_vec(vec)
    allocate(vec%v(num))
    vec%size = num
    call adjust_mem(num*sp)
  end subroutine alloc_int_vec

  subroutine alloc_label_vec(vec,num)
    type(label_vec), intent(inout) :: vec
    integer, intent(in) :: num

    if (num == vec%size) return
    if (vec%size > 0) call free_vec(vec)
    allocate(vec%v(num))
    vec%size = num
    call adjust_mem(num*16)
  end subroutine alloc_label_vec

  ! clear memory for vector types
  subroutine clear_xyz_vec(vec)
    type(xyz_vec), intent(inout) :: vec

    if (vec%size > 0) then
       vec%x(:) = d_zero
       vec%y(:) = d_zero
       vec%z(:) = d_zero
    end if
  end subroutine clear_xyz_vec

  subroutine clear_dp_vec(vec)
    type(dp_vec), intent(inout) :: vec

    if (vec%size > 0) vec%v(:) = d_zero
  end subroutine clear_dp_vec

  subroutine clear_int_vec(vec)
    type(int_vec), intent(inout) :: vec

    if (vec%size > 0) vec%v(:) = 0
  end subroutine clear_int_vec

  subroutine clear_label_vec(vec)
    type(label_vec), intent(inout) :: vec

    if (vec%size > 0) vec%v(:) = '                '
  end subroutine clear_label_vec

  ! free memory for vector types
  subroutine free_xyz_vec(vec)
    type(xyz_vec), intent(inout) :: vec

    if (vec%size > 0) then
       deallocate(vec%x,vec%y,vec%z)
       call adjust_mem(-3*vec%size*dp)
    end if
    vec%size = 0
  end subroutine free_xyz_vec

  subroutine free_dp_vec(vec)
    type(dp_vec), intent(inout) :: vec

    if (vec%size > 0) then
       deallocate(vec%v)
       call adjust_mem(-vec%size*dp)
    end if
    vec%size = 0
  end subroutine free_dp_vec

  subroutine free_int_vec(vec)
    type(int_vec), intent(inout) :: vec

    if (vec%size > 0) then
       deallocate(vec%v)
       call adjust_mem(-vec%size*sp)
    end if
    vec%size = 0
  end subroutine free_int_vec

  subroutine free_label_vec(vec)
    type(label_vec), intent(inout) :: vec

    if (vec%size > 0) then
       deallocate(vec%v)
       call adjust_mem(-vec%size*16-sp)
    end if
    vec%size = 0
  end subroutine free_label_vec

end module memory
