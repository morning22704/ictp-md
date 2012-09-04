!> Module to manage system cell information
module cell
  use kinds
  use constants
  use message_passing, only: mp_ioproc, mp_bcast, mp_error
  implicit none

  private
  logical :: ortho_cell
  real(kind=dp), dimension(3) :: origin
  real(kind=dp), dimension(6) :: hmat, hinv

  public :: cell_init, set_cell, cell_replicate
  public :: get_hmat, get_hinv, get_origin

  interface set_cell
     module procedure set_cell_abc
     module procedure set_cell_lammps
  end interface

contains

  !> set default values
  subroutine cell_init
    use memory, only: adjust_mem

    ortho_cell = .true.
    call adjust_mem(1*sp)
    origin(:) = d_zero
    call adjust_mem(3*dp)
    hmat(:) = d_zero
    hinv = hmat
    call adjust_mem(2*6*dp)
  end subroutine cell_init

  !> set new cell dimensions with alpha, beta, gamma info
  subroutine set_cell_abc(new_origin, new_cellparam, is_ortho)
    real(kind=dp), intent(in) :: new_origin(3)
    real(kind=dp), intent(in) :: new_cellparam(6)
    logical, intent(in) :: is_ortho
    real(kind=dp) :: b, c

    ortho_cell = is_ortho
    origin(:) = new_origin(:)

    if (ortho_cell) then
       hmat(1:3) = new_cellparam(1:3)
       hmat(4:6) = d_zero
    else
       hmat(1) = new_cellparam(1)
       b = new_cellparam(2)
       c = new_cellparam(3)

       hmat(5) = c * cos(new_cellparam(5)*d_pi/180_dp); ! xz
       hmat(6) = b * cos(new_cellparam(6)*d_pi/180_dp); ! xy

       hmat(2) = sqrt(b**2 - hmat(6)**2);
       if (abs(hmat(2)) > d_small) then
          hmat(4) = (b*c*cos(new_cellparam(4)*d_pi/180_dp) &
               - hmat(6)*hmat(5)) / hmat(2)             ! yz
       else
          hmat(4) = d_zero
       end if
       hmat(3) = sqrt(c**2 - hmat(5)**2 - hmat(4)**2);
    end if
    call hinvert
  end subroutine set_cell_abc

  !> set new cell dimension with lammps info
  subroutine set_cell_lammps(xlo,xhi,ylo,yhi,zlo,zhi,xy,xz,yz,is_ortho)
    real(kind=dp), intent(in) :: xlo,xhi,ylo,yhi,zlo,zhi,xy,xz,yz
    logical, intent(in) :: is_ortho
    real(kind=dp) :: dx, dy, dz

    ortho_cell = is_ortho
    origin(1) = xlo
    origin(2) = ylo
    origin(3) = zlo
    dx = xhi - xlo
    dy = yhi - ylo
    dz = zhi - zlo

    if (ortho_cell) then
       hmat(1) = dx
       hmat(2) = dy
       hmat(3) = dz
       hmat(4:6) = d_zero
    else
       hmat(1) = dx
       hmat(2) = dy
       hmat(3) = dz
       hmat(4) = yz
       hmat(5) = xz
       hmat(6) = xy
    end if
  end subroutine set_cell_lammps

  subroutine cell_replicate
    use message_passing, only : mp_bcast

    call mp_bcast(ortho_cell)
    call mp_bcast(origin,3)
    call mp_bcast(hmat,6)
    call mp_bcast(hinv,6)
  end subroutine cell_replicate

  subroutine hinvert
    hinv(1) = d_one / hmat(1)
    hinv(2) = d_one / hmat(2)
    hinv(3) = d_one / hmat(3)
    if (ortho_cell) then
       hinv(4:6) = d_zero
    else
       hinv(4) = -hmat(4) / (hmat(2)*hmat(3))
       hinv(5) = (hmat(4)*hmat(6) - hmat(2)*hmat(5))/(hmat(1)*hmat(2)*hmat(3))
       hinv(6) = -hmat(6) / (hmat(1)*hmat(2))
    end if
  end subroutine hinvert

  subroutine get_hmat(vec)
    real(kind=dp), dimension(6), intent(out) :: vec
    vec(:) = hmat(:)
  end subroutine get_hmat

  subroutine get_hinv(vec)
    real(kind=dp), dimension(6), intent(out) :: vec
    vec(:) = hinv(:)
  end subroutine get_hinv

  subroutine get_origin(vec)
    real(kind=dp), dimension(3), intent(out) :: vec
    vec(:) = origin(:)
  end subroutine get_origin

end module cell
