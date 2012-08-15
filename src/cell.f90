!> Module to manage system cell information
module cell
  use kinds
  use constants
  use message_passing, only: mp_ioproc, mp_bcast, mp_error
  implicit none

  private
  logical :: ortho_cell
  real(kind=dp), dimension(3) :: origin
  real(kind=dp), dimension(3,3) :: hmat, hinv

  public :: cell_init, set_cell

contains

  ! set default values
  subroutine cell_init
    use memory, only: adjust_mem

    ortho_cell = .false.
    call adjust_mem(1*sp)
    origin(:) = d_zero
    call adjust_mem(3*dp)
    hmat(:,:) = d_zero
    hinv = hmat
    call adjust_mem(2*9*dp)
  end subroutine cell_init

  subroutine set_cell

  end subroutine set_cell

end module cell
