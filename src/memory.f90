! module with custom memory related operations, allocator, and deallocators
module memory
  use kinds
  use constants
  implicit none

  private
  real(kind=dp) :: total_mem
  logical :: is_debug

  public :: memory_init, memory_print, adjust_mem, get_mem, debug_mem
  public :: alloc_vec, clear_vec, free_vec
  public :: alloc_mat, clear_mat, free_mat

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

  interface alloc_mat
     module procedure alloc_dp_mat
     module procedure alloc_int_mat
  end interface

  interface clear_mat
     module procedure clear_dp_mat
     module procedure clear_int_mat
  end interface

  interface free_mat
     module procedure free_dp_mat
     module procedure free_int_mat
  end interface

contains

  ! "constructor"
  subroutine memory_init
    total_mem = d_zero
    is_debug  = .false.
    call adjust_mem(dp+sp)
  end subroutine memory_init

  ! adjust total memory accounting
  subroutine adjust_mem(amount)
    use io, only : stderr
    integer, intent(in) :: amount

    if (is_debug) &
         write(stderr,'(A,I8,A)') ' Adjusting memory by : ', amount, ' Bytes'

    total_mem = total_mem + dble(amount)
  end subroutine adjust_mem

  ! return accounted memory
  function get_mem()
    real(kind=dp) :: get_mem

    get_mem = total_mem
  end function get_mem

  ! turn on memory debugging
  subroutine debug_mem(flag)
    logical :: flag
    is_debug = flag
  end subroutine debug_mem

  ! print accounted memory
  subroutine memory_print
  use io, only : stdout, separator
    write(stdout,*) separator
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
    if (num > 0) then
       allocate(vec%x(num),vec%y(num),vec%z(num))
       call adjust_mem(3*num*dp)
    end if
    vec%size = num
  end subroutine alloc_xyz_vec

  subroutine alloc_dp_vec(vec,num)
    type(dp_vec), intent(inout) :: vec
    integer, intent(in) :: num

    if (num == vec%size) return
    if (vec%size > 0) call free_vec(vec)
    if (num > 0) then
       allocate(vec%v(num))
       call adjust_mem(num*dp)
    end if
    vec%size = num
  end subroutine alloc_dp_vec

  subroutine alloc_int_vec(vec,num)
    type(int_vec), intent(inout) :: vec
    integer, intent(in) :: num

    if (num == vec%size) return
    if (vec%size > 0) call free_vec(vec)
    if (num > 0) then
       allocate(vec%v(num))
       call adjust_mem(num*sp)
    end if
    vec%size = num
  end subroutine alloc_int_vec

  subroutine alloc_label_vec(vec,num)
    type(label_vec), intent(inout) :: vec
    integer, intent(in) :: num

    if (num == vec%size) return
    if (vec%size > 0) call free_vec(vec)
    if (num > 0) then
       allocate(vec%v(num))
       call adjust_mem(num*16)
    end if
    vec%size = num
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

  subroutine alloc_dp_mat(mat,numx,numy)
    type(dp_mat), intent(inout) :: mat
    integer, intent(in) :: numx,numy

    if ((numx == mat%sizex) .and. (numy == mat%sizey)) return
    if (mat%sizex > 0) call free_mat(mat)
    if ((numx > 0) .and. (numy > 0)) then
       allocate(mat%m(numx,numy))
       call adjust_mem(numx*numy*dp)
    end if
    mat%sizex = numx
    mat%sizey = numy
  end subroutine alloc_dp_mat

  subroutine clear_dp_mat(mat)
    type(dp_mat), intent(inout) :: mat

    if (mat%sizex > 0) mat%m(:,:) = d_zero
  end subroutine clear_dp_mat

  subroutine free_dp_mat(mat)
    type(dp_mat), intent(inout) :: mat

    if (mat%sizex > 0) then
       deallocate(mat%m)
       call adjust_mem(-mat%sizex*mat%sizey*sp)
    end if
    mat%sizex = 0
    mat%sizey = 0
  end subroutine free_dp_mat

  subroutine alloc_int_mat(mat,numx,numy)
    type(int_mat), intent(inout) :: mat
    integer, intent(in) :: numx,numy

    if ((numx == mat%sizex) .and. (numy == mat%sizey)) return
    if (mat%sizex > 0) call free_mat(mat)
    if ((numx > 0) .and. (numy > 0)) then
       allocate(mat%m(numx,numy))
       call adjust_mem(numx*numy*dp)
    end if
    mat%sizex = numx
    mat%sizey = numy
  end subroutine alloc_int_mat

  subroutine clear_int_mat(mat)
    type(int_mat), intent(inout) :: mat

    if (mat%sizex > 0) mat%m(:,:) = d_zero
  end subroutine clear_int_mat

  subroutine free_int_mat(mat)
    type(int_mat), intent(inout) :: mat

    if (mat%sizex > 0) then
       deallocate(mat%m)
       call adjust_mem(-mat%sizex*mat%sizey*sp)
    end if
    mat%sizex = 0
    mat%sizey = 0
  end subroutine free_int_mat

end module memory
