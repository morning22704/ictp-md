!> Module to store and maintain per atom data
module atoms
  use kinds
  use threading, only: thr_get_num_threads
  implicit none

  private
  integer :: natoms, ntypes
  logical :: valid_x_r, valid_x_s
  logical :: have_chg
  type (xyz_vec)   :: x_r, x_s, vel, for
  type (dp_vec)    :: chg, mss
  type (int_vec)   :: typ
  type (label_vec) :: lbl

  public :: atoms_init, set_natoms!, set_nghosts, set_ntypes

contains

  ! set default values
  subroutine atoms_init(maxtypes)
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
  end subroutine atoms_init
  
  subroutine set_natoms(size)
    use memory, only: alloc_vec, clear_vec
    integer, intent(in) :: size
    integer :: nthr

    if (natoms >= 0) then
    end if
  end subroutine set_natoms

end module atoms
