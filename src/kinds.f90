!> Module to define data width constands and custom data types
module kinds
  implicit none

  public
  integer, parameter :: sp = selected_real_kind(6,30)   !< regular 4 byte real
  integer, parameter :: dp = selected_real_kind(14,200) !< regular 8 byte real

  integer, parameter :: lblen = 16  !< length of atom/type label strings
  integer, parameter :: lilen = 252 !< length of line/file strings

  !> Derived data type to hold a 3-tuple floating point vector with size
  type xyz_vec
     real(kind=dp), pointer :: x(:) !< x component of vector
     real(kind=dp), pointer :: y(:) !< y component of vector
     real(kind=dp), pointer :: z(:) !< z component of vector
     integer :: size                !< length of x, y, and z arrays
  end type xyz_vec

  !> Derived data type to hold a floating point vector with associated size
  type dp_vec
     real(kind=dp), pointer :: v(:) !< data vector
     integer :: size                !< length of data vector
  end type dp_vec

  !> Derived data type to hold a single integer vector with associated size
  type int_vec
     integer, pointer :: v(:)       !< data vector
     integer :: size                !< length of data vector
  end type int_vec

  !> Derived data type to hold a atom type/label vector with associated size
  type label_vec
     character(len=lblen), pointer :: v(:) !< data vector
     integer :: size                       !< length of data vector
  end type label_vec

  !> Derived data type to hold a floating point matrix with associated size
  type dp_mat
     real(kind=dp), pointer :: m(:,:) !< data matrix
     integer :: sizex, sizey          !< dimensions of data matrix
  end type dp_mat

  !> Derived data type to hold a singe integer matrix with associated size
  type int_mat
     integer, pointer :: m(:,:) !< data matrix
     integer :: sizex, sizey    !< dimensions of data matrix
  end type int_mat

  !> Derived data type for a single neighbor cell
  type neigh_cell
     real(kind=dp)    :: offset(3) !< offset of the cell relative to original
     integer, pointer :: list(:)   !< list of atom indices in cell
     integer :: idx                !< unique index number of this cell
     logical :: is_ghost           !< true if this is not a principal cell
  end type neigh_cell

end module kinds
