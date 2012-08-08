!> Module to define data width constands and custom data types
module kinds
  implicit none

  public
  integer, parameter :: sp = selected_real_kind(6,30)   !< regular 4 byte real
  integer, parameter :: dp = selected_real_kind(14,200) !< regular 8 byte real

  !> derived data type to hold cartesian vector data with explicit size
  type xyz_vec
     real(kind=dp), allocatable :: x(:), y(:), z(:)
     integer :: size
  end type xyz_vec

  !> derived data type hold a single floating point vector with explicit size
  type dp_vec
     real(kind=dp), allocatable :: v(:)
     integer :: size
  end type dp_vec

  !> derived data type hold a single integer vector with explicit size
  type int_vec
     integer, allocatable :: v(:)
     integer :: size
  end type int_vec

end module kinds
