! module to define kinds and custom data types
module kinds
  implicit none

  public
  integer, parameter :: sp = selected_real_kind(6,30)
  integer, parameter :: dp = selected_real_kind(14,200)

  ! derived types to hold vector data
  type xyz_vec
     real(kind=dp), pointer :: x(:), y(:), z(:)
     integer :: size
  end type xyz_vec

  type dp_vec
     real(kind=dp), pointer :: v(:)
     integer :: size
  end type dp_vec

  type int_vec
     integer, pointer :: v(:)
     integer :: size
  end type int_vec

end module kinds
