!> Module to store and maintain per atom data
module atom
  use kinds
  use threading, only: thr_get_num_threads
  use message_passing, only: mp_error
  implicit none

  private
  integer :: natoms, ntypes
  logical :: valid_x_r, valid_x_s
  logical :: have_chg
  type (xyz_vec)   :: x_r, x_s, vel, for
  type (dp_vec)    :: chg, mss
  type (int_vec)   :: typ
  type (label_vec) :: lbl

  public :: atom_init, atom_resize, set_type
  public :: get_ntypes

contains

  ! set default values
  subroutine atom_init(maxtypes)
    use memory, only: adjust_mem, alloc_vec
    integer, intent(in) :: maxtypes

    natoms     = -1
    ntypes     = -1
    call adjust_mem(2*sp)
    valid_x_r  = .false.
    valid_x_s  = .false.
    have_chg   = .false.
    call adjust_mem(5*sp)
    x_r%size = -1
    vel%size = -1
    for%size = -1
    x_s%size = -1
    call adjust_mem(4*(3*dp+sp))
    typ%size = -1
    chg%size = -1
    mss%size = -1
    lbl%size = -1
    call adjust_mem(4*(dp+sp))

    call alloc_vec(mss,maxtypes)
    call alloc_vec(lbl,maxtypes)
  end subroutine atom_init
  
  subroutine atom_resize(size)
    use memory, only: alloc_vec
    integer, intent(in) :: size
    integer :: nthr

    nthr = thr_get_num_threads()
    if (natoms < 0) then
       call alloc_vec(x_r,size)
       call alloc_vec(x_s,size)
       call alloc_vec(vel,size)
       call alloc_vec(for,size*nthr)
       call alloc_vec(typ,size)
       call alloc_vec(chg,size)
    else
       call mp_error('Changing "natoms" currently not supported',natoms)
    end if
  end subroutine atom_resize

  subroutine set_type(idx,name)
    integer, intent(in) :: idx
    character(len=16), intent(in) :: name
    integer :: i, newtype

    if (ntypes < 0) ntypes = 0
    newtype = -1

    if (trim(name) == 'X' .or. trim(name) == 'x') then
       newtype = 0
    else
       do i=1,ntypes
          if (name == lbl%v(i)) newtype = i
       end do
       if (newtype < 0) then
          ntypes = ntypes + 1
          if (ntypes > size(lbl%v)) &
               call mp_error('Too many atom types. Increase maxtypes.',ntypes)
          newtype = ntypes
          lbl%v(newtype) = name
       end if
    end if
    typ%v(idx) = newtype
  end subroutine set_type

  function get_ntypes()
    integer :: get_ntypes

    get_ntypes = ntypes
  end function get_ntypes
  
end module atom
