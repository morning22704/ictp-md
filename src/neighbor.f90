!> Module for neighbor/cell list generation

module neighbor
  use kinds
  use constants
  use message_passing, only : mp_error, mp_ioproc, mp_bcast
  implicit none
  private
  real(kind=dp) :: dx, dy, dz
  integer :: next_step, nx, ny, nz, ncells, nlist, maxlist
  logical :: first_call, do_check
  type(neigh_cell), pointer :: list(:,:,:)
  type(xyz_vec) :: old_pos
  public :: neighbor_init, neighbor_setup, neighbor_build
  public :: get_ncells, get_cell, cell2index

contains

  !> Initialize variables of the neighbor module 
  subroutine neighbor_init
    use memory, only : adjust_mem
    dx = -d_one
    dy = -d_one
    dz = -d_one
    nx = -1
    ny = -1
    nz = -1
    ncells = -1
    nlist = 0
    maxlist = 0
    next_step = -1
    first_call = .true.
    do_check = .false.
    nullify(list)
    old_pos%size = -1
    call adjust_mem(3*dp+9*sp+dp+(3*dp+sp))
  end subroutine neighbor_init

  !> Set up, allocate, and prepare  the basic cell list data
  !! When called the first time, print out diagnostic info.
  subroutine neighbor_setup
    use io
    use atoms,      only : get_natoms
    use memory,     only : adjust_mem, memory_print, alloc_vec
    use cell,       only : get_hmat
    use pair_io,    only : get_max_cutoff
    use sysinfo_io, only : get_neigh_nlevel, get_neigh_ratio, get_neigh_skin, &
         get_neigh_check, get_newton
    integer :: nlevel, nlower, nghosts, i, j, k, ip, jp, kp
    real(kind=dp) :: cutoff, ratio, hmat(6), offset(3)

    call get_hmat(hmat)
    nlevel = get_neigh_nlevel()
    ratio  = get_neigh_ratio()
    cutoff = get_max_cutoff() + get_neigh_skin()
    do_check = get_neigh_check()

    ! deallocate previously allocated storage
    if (.not. first_call) then
       if (get_newton()) then
          nghosts = (nx+nlevel+1)*(ny+nlevel+1)*(nz+nlevel+1) - ncells
       else
          nghosts = (nx+2*nlevel+2)*(ny+2*nlevel+2)*(nz+2*nlevel+2) - ncells
       end if
       do i=1,nx
          do j=1,ny
             do k=1,nz
                deallocate(list(i,j,k)%list)
             end do
          end do
       end do
       deallocate(list)
       call adjust_mem(-(3*dp+dp+2*sp)*(ncells+nghosts) - nlist*sp*ncells)
    end if

    nx = int(dble(nlevel)*hmat(1)/cutoff)
    ny = int(dble(nlevel)*hmat(2)/cutoff)
    nz = int(dble(nlevel)*hmat(3)/cutoff)
    ncells = nx*ny*nz
    dx = d_one/dble(nx)
    dy = d_one/dble(ny)
    dz = d_one/dble(nz)
    nlist = int(dble(get_natoms())/dble(ncells)*ratio)

    if (get_newton()) then
       nlower = 1
       nghosts = (nx+nlevel+1)*(ny+nlevel+1)*(nz+nlevel+1) - ncells
    else
       nlower = -nlevel
       nghosts = (nx+2*nlevel+2)*(ny+2*nlevel+2)*(nz+2*nlevel+2) - ncells
    end if
    allocate(list(nlower:nx+nlevel+1,nlower:ny+nlevel+1,nlower:nz+nlevel+1))

    if (do_check) call alloc_vec(old_pos,get_natoms())
    do i=1,nx
       do j=1,ny
          do k=1,nz
             allocate(list(i,j,k)%list(nlist))
             list(i,j,k)%offset(:) = d_zero
             list(i,j,k)%nlist = 0
             list(i,j,k)%is_ghost = .false.
          end do
       end do
    end do

    offset(:) = d_zero
    do i=nlower,nx+nlevel+1
       if (i < 1) then
          ip = i + nx
          offset(1) = -d_one
       elseif (i > nx) then
          ip = i - nx
          offset(1) = d_one
       else 
          ip = i
          offset(1) = d_zero
       end if
       do j=nlower,ny+nlevel+1
          if (j < 1) then
             jp = j + ny
             offset(2) = -d_one
          elseif (j > ny) then
             jp = j - ny
             offset(2) = d_one
          else 
             jp = j
             offset(2) = d_zero
          end if
          do k=nlower,nz+nlevel+1
             if (k < 1) then
                kp = k + nz
                offset(3) = -d_one
             elseif (k > nz) then
                kp = k - nz
                offset(3) = d_one
             else
                kp = k
                offset(3) = d_zero
             end if
             if (i<1 .or. i>nx .or. j<1 .or. j>ny .or. k<1 .or. k>nz) then
                list(i,j,k)%is_ghost = .true.
                list(i,j,k)%nlist = list(ip,jp,kp)%nlist
                list(i,j,k)%list => list(ip,jp,kp)%list
                list(i,j,k)%offset(:) = offset(:)
             endif
          end do
       end do
    end do
    call adjust_mem((3*dp+dp+2*sp)*(ncells+nghosts) + nlist*sp*ncells)
    call neighbor_build

    ! print status only by io task and only on first call
    if (first_call .and. mp_ioproc()) then
       write(stdout,*) 'Cutoff used for neighbor cells  : ', cutoff
       write(stdout,*) 'Actual/average atom/cell ratio  : ', ratio
       write(stdout,*) 'Number of cell levels           : ', nlevel
       write(stdout,fmt='(A,I5,A,I5,A,I5,A,I12,A)') ' Using ', &
            nx, ' x', ny, ' x', nz, ' =', ncells, ' principal cells'
       write(stdout,fmt='(A,G11.6,A,G11.6,A,G11.6)') ' Grid spacing  : ', &
            dx*hmat(1), ' x ', dy*hmat(2), ' x ', dz*hmat(3)
       write(stdout,*) 'Number of ghost cells           : ', nghosts
       write(stdout,*) 'Maximum allowed atoms per cell  : ', nlist
       write(stdout,*) 'Maximum actual atoms per cell   : ', maxlist
       write(stdout,*) 'Average number of atoms per cell: ', &
            dble(get_natoms())/dble(ncells)
       call memory_print
    end if
    first_call = .false.
  end subroutine neighbor_setup

  !> Sort all atoms into their respective cells, update their
  !! image flags (and implicitly wrap the coordinates for periodic
  !! boundary conditions) and update the corresponding ghost cell data 
  subroutine neighbor_build
    use io
    use atoms,      only : get_natoms, get_x_s, update_image
    use sysinfo_io, only : get_neigh_nlevel, get_newton
    real(kind=dp), pointer :: x(:), y(:), z(:)
    integer :: nlevel, natoms, nlower, n, i, j, k, ix, iy, iz, ip, jp, kp

    natoms = get_natoms()
    nlevel = get_neigh_nlevel()
    call get_x_s(x,y,z)

    ! clear list entries
    list(:,:,:)%nlist = 0

    do i=1,natoms
       ix = int(x(i)/dx + d_one)
       iy = int(y(i)/dy + d_one)
       iz = int(z(i)/dz + d_one)
       if (ix < 1) then
          n = (ix - nx) / nx
          ix = ix - n*nx
          call update_image(i,n,'x')
       else if (ix > nx) then
          n = ix / nx
          ix = ix - n*nx
          call update_image(i,n,'x')
       end if
       if (iy < 1) then
          n = (iy - ny) / ny
          iy = iy - n*ny
          call update_image(i,n,'y')
       else if (iy > ny) then
          n = iy / ny
          iy = iy - n*ny
          call update_image(i,n,'y')
       end if
       if (iz < 1) then
          n = (iz - nz) / nz
          iz = iz - n*nz
          call update_image(i,n,'z')
       else if (iz > nz) then
          n = iz / nz
          iz = iz - n*nz
          call update_image(i,n,'z')
       end if
       n = list(ix,iy,iz)%nlist + 1
       if (n > maxlist) maxlist = n
       if (n > nlist) &
            call mp_error('Cell list overflow. Increase neigh_ratio.',n)
       list(ix,iy,iz)%nlist = n
       list(ix,iy,iz)%list(n) = i
    end do

    ! update nlist data in ghost cells
    if (get_newton()) then
       nlower = 1
    else
       nlower = -nlevel
    end if

    do i=nlower,nx+nlevel+1
       if (i < 1) then
          ip = i + nx
       elseif (i > nx) then
          ip = i - nx
       else 
          ip = i
       end if

       do j=nlower,ny+nlevel+1
          if (j < 1) then
             jp = j + ny
          elseif (j > ny) then
             jp = j - ny
          else 
             jp = j
          end if

          do k=nlower,nz+nlevel+1
             if (k < 1) then
                kp = k + nz
             elseif (k > nz) then
                kp = k - nz
             else
                kp = k
             end if
             if (i<1 .or. i>nx .or. j<1 .or. j>ny .or. k<1 .or. k>nz) then
                list(i,j,k)%nlist = list(ip,jp,kp)%nlist
             endif
          end do
       end do
    end do

  end subroutine neighbor_build

  !> Return the number of non-ghost cells
  function get_ncells()
    integer :: get_ncells

    get_ncells = ncells
  end function get_ncells

  !> Return cell info based on i,j,k indices
  !! @param i x index of the cell
  !! @param j y index of the cell
  !! @param k z index of the cell
  function get_cell(i,j,k)
    type(neigh_cell) :: get_cell
    integer, intent(in) :: i,j,k

    get_cell = list(i,j,k)
  end function get_cell

  !> Return non-ghost cell info based on an index between 1 and ncells
  !! @param idx Index of the cell
  !! @param i x index of the cell
  !! @param j y index of the cell
  !! @param k z index of the cell 
  subroutine cell2index(idx,i,j,k)
    integer, intent(in)  :: idx
    integer, intent(out) :: i,j,k

    i = idx/(ny*nz)+1
    j = mod(idx,ny*nz)/nz+1
    k = mod(idx,nz)+1
  end subroutine cell2index

end module neighbor
