!> Module for neighbor/cell list generation

module neighbor
  use kinds
  use constants
  use message_passing, only : mp_error, mp_ioproc, mp_bcast
  implicit none
  private
  real(kind=dp) :: dx, dy, dz
  integer :: next_step, nx, ny, nz, ncells, npairs, nstencil, nlist, maxlist
  logical :: first_call, do_check, newton
  type(neigh_cell), pointer :: list(:,:,:)
  type(xyz_vec) :: old_pos
  type(int_vec) :: cell_pairs
  public :: neighbor_init, neighbor_setup, neighbor_build
  public :: get_ncells, get_npairs, get_cell_pairs, get_cell

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
    npairs = -1
    nstencil = -1
    nlist = 0
    maxlist = 0
    next_step = -1
    first_call = .true.
    do_check = .false.
    newton = .true.
    nullify(list)
    old_pos%size = -1
    call adjust_mem(3*dp+12*sp+dp+(3*dp+sp)+(dp+sp))
  end subroutine neighbor_init

  !> Set up, allocate, and prepare  the basic cell list data
  !! When called the first time, print out diagnostic info.
  subroutine neighbor_setup(max_cutoff)
    use io
    use atoms,      only : get_natoms, coord_s2r
    use memory,     only : adjust_mem, memory_print, alloc_vec
    use cell,       only : get_hmat
    use sysinfo_io, only : get_neigh_nlevel, get_neigh_ratio, get_neigh_skin, &
         get_neigh_check, get_newton
    real(kind=dp), intent(in) :: max_cutoff
    real(kind=dp) :: cutoff, ratio, hmat(6), offset(3), delta(3)
    integer :: nlevel, nlower, nghosts, i, j, k, ip, jp, kp, n, idx, npruned

    call get_hmat(hmat)
    nlevel = get_neigh_nlevel()
    ratio  = get_neigh_ratio()
    cutoff = max_cutoff + get_neigh_skin()
    do_check = get_neigh_check()
    newton = get_newton()

    ! deallocate previously allocated storage
    if (.not. first_call) then
       if (newton) then
          nghosts = (nx+nlevel)*(ny+nlevel)*(nz+nlevel) - ncells
       else
          nghosts = (nx+2*nlevel)*(ny+2*nlevel)*(nz+2*nlevel) - ncells
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
    npruned = 0

    if (newton) then
       nlower   = 1
       nghosts  = (nx+nlevel)*(ny+nlevel)*(nz+nlevel) - ncells
       nstencil = (nlevel+1)**3
    else
       nlower   = -nlevel+1
       nghosts  = (nx+2*nlevel)*(ny+2*nlevel)*(nz+2*nlevel) - ncells
       nstencil = (2*(nlevel)+1)**3
    end if
    allocate(list(nlower:nx+nlevel,nlower:ny+nlevel,nlower:nz+nlevel))
    npairs = ncells * nstencil
    call alloc_vec(cell_pairs,6*npairs)

    if (do_check) call alloc_vec(old_pos,get_natoms())
    n = 0
    do k=1,nz
       do j=1,ny
          do i=1,nx
             allocate(list(i,j,k)%list(0:nlist))
             list(i,j,k)%offset(:) = d_zero
             list(i,j,k)%is_ghost = .false.
             do kp=k+nlower-1,k+nlevel
                do jp=j+nlower-1,j+nlevel
                   do ip=i+nlower-1,i+nlevel
                      ! check if this cell pair can be skipped completely
                      if (i < ip) then
                         delta(1) = dx * (ip - i - 1)
                      else if (i > ip) then
                         delta(1) = dx * (i - ip - 1)
                      else
                         delta(1) = d_zero
                      end if
                      if (j < jp) then
                         delta(2) = dy * (jp - j - 1)
                      else if (j > jp) then
                         delta(2) = dy * (j - jp - 1)
                      else
                         delta(2) = d_zero
                      end if
                      if (k < kp) then
                         delta(3) = dz * (kp - k - 1)
                      else if (k > kp) then
                         delta(3) = dz * (k - kp - 1)
                      else
                         delta(3) = d_zero
                      end if
                      call coord_s2r(delta,offset)
                      ! indicate a pruned cell pair
                      if (sqrt(sum(offset*offset)) > max_cutoff) then
                         npruned = npruned + 1
                         cell_pairs%v(6*n+1) = -1
                      else
                         cell_pairs%v(6*n+1) = i
                         cell_pairs%v(6*n+2) = j
                         cell_pairs%v(6*n+3) = k
                         cell_pairs%v(6*n+4) = ip
                         cell_pairs%v(6*n+5) = jp
                         cell_pairs%v(6*n+6) = kp
                      end if
                      n = n + 1
                   end do
                end do
             end do
          end do
       end do
    end do

    ! map ghost cells to non-ghosts
    offset(:) = d_zero
    idx = 1
    do kp=nlower,nz+nlevel
       if (kp < 1) then
          k = kp + nz
          offset(3) = -d_one
       elseif (kp > nz) then
          k = kp - nz
          offset(3) = d_one
       else
          k = kp
          offset(3) = d_zero
       end if

       do jp=nlower,ny+nlevel
          if (jp < 1) then
             j = jp + ny
             offset(2) = -d_one
          elseif (jp > ny) then
             j = jp - ny
             offset(2) = d_one
          else 
             j = jp
             offset(2) = d_zero
          end if

          do ip=nlower,nx+nlevel
             if (ip < 1) then
                i = ip + nx
                offset(1) = -d_one
             elseif (ip > nx) then
                i = ip - nx
                offset(1) = d_one
             else 
                i = ip
                offset(1) = d_zero
             end if

             list(ip,jp,kp)%idx = idx
             idx = idx + 1
             if ((ip < 1) .or. (ip > nx) .or. (jp<1) .or. (jp > ny) &
                  .or. (kp < 1) .or. (kp > nz)) then
                list(ip,jp,kp)%is_ghost = .true.
                list(ip,jp,kp)%offset(:) = offset(:)
                list(ip,jp,kp)%list => list(i,j,k)%list
             endif
          end do
       end do
    end do
    call adjust_mem((3*dp+dp+2*sp)*(ncells+nghosts) + nlist*sp*ncells)

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
       write(stdout,*) 'Number of stencil cells         : ', nstencil
       write(stdout,*) 'Number of total cell pairs      : ', npairs
       write(stdout,*) 'Number of pruned cell pairs     : ', npruned
       write(stdout,*) 'Average number of atoms per cell: ', &
            dble(get_natoms())/dble(ncells)
       write(stdout,*) 'Maximum allowed atoms per cell  : ', nlist
    end if

    call neighbor_build

    ! print status only by io task and only on first call
    if (first_call .and. mp_ioproc()) then
       write(stdout,*) 'Maximum actual atoms per cell   : ', maxlist
       call memory_print
    end if
    first_call = .false.
  end subroutine neighbor_setup

  !> Sort all atoms into their respective cells, update their image flags
  !! (and implicitly wrap the coordinates for periodic boundary conditions)
  subroutine neighbor_build
    use io
    use atoms,      only : get_natoms, get_x_s, update_image
    real(kind=dp), pointer :: x(:), y(:), z(:)
    integer :: natoms, n, i, ix, iy, iz

    natoms = get_natoms()
    call get_x_s(x,y,z)

    ! clear list entries
    do iz=1,nz
       do iy=1,ny
          do ix=1,nx
             ! FIXME: only zeroeing out of element 0 needed
             list(ix,iy,iz)%list(:) = 0
          end do
       end do
    end do

    do i=1,natoms
       ix = int(x(i)/dx + d_one)
       iy = int(y(i)/dy + d_one)
       iz = int(z(i)/dz + d_one)

       if (ix < 1) then
          n = (ix-nx) / nx
          ix = ix - n*nx
          call update_image(i,n,'x')
       else if (ix > nx) then
          n = (ix-1) / nx
          ix = ix - n*nx
          call update_image(i,n,'x')
       end if

       if (iy < 1) then
          n = (iy-ny) / ny
          iy = iy - n*ny
          call update_image(i,n,'y')
       else if (iy > ny) then
          n = (iy-1) / ny
          iy = iy - n*ny
          call update_image(i,n,'y')
       end if

       if (iz < 1) then
          n = (iz-nz) / nz
          iz = iz - n*nz
          call update_image(i,n,'z')
       else if (iz > nz) then
          n = (iz-1) / nz
          iz = iz - n*nz
          call update_image(i,n,'z')
       end if

       n = list(ix,iy,iz)%list(0) + 1
       if (n > nlist) &
            call mp_error('Cell list overflow. Increase neigh_ratio.',n)
       if (n > maxlist) maxlist = n
       list(ix,iy,iz)%list(0) = n
       list(ix,iy,iz)%list(n) = i
    end do

  end subroutine neighbor_build

  !> Return the number of non-ghost cells
  function get_ncells()
    integer :: get_ncells

    get_ncells = ncells
  end function get_ncells

  !> Return the number of cell pairs
  function get_npairs()
    integer :: get_npairs

    get_npairs = npairs
  end function get_npairs

  !> Give access to the list of cell pairs
  subroutine get_cell_pairs(ptr)
    integer, pointer :: ptr(:)

    ptr => cell_pairs%v
  end subroutine get_cell_pairs

  !> Return cell info based on i,j,k indices
  !! @param i x index of the cell
  !! @param j y index of the cell
  !! @param k z index of the cell
  function get_cell(i,j,k)
    type(neigh_cell) :: get_cell
    integer, intent(in) :: i,j,k

    get_cell = list(i,j,k)
  end function get_cell

end module neighbor
