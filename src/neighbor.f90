!> Module for neighbor/cell list generation

module neighbor
  use kinds
  use constants
  use message_passing, only : mp_error, mp_ioproc, mp_bcast
  implicit none
  private
  real(kind=dp) :: dx, dy, dz, cutoff, sumpruned
  integer :: ncells, nghosts, npairs, nx, ny, nz, ngx, ngy, ngz
  integer :: next_step, nlist, maxlist
  integer :: nbuild, ncheck, ndanger
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
    cutoff = d_zero
    sumpruned = d_zero
    call adjust_mem(5*dp)
    nx = -1
    ny = -1
    nz = -1
    ngx = -1
    ngy = -1
    ngz = -1
    ncells = -1
    nghosts = -1
    npairs = -1
    nlist = 0
    maxlist = 0
    next_step = -1
    nbuild = 0
    ncheck = 0
    ndanger = 0
    call adjust_mem(14*sp)
    first_call = .true.
    do_check = .false.
    newton = .true.
    call adjust_mem(3*sp)
    nullify(list)
    old_pos%size = -1
    call adjust_mem((3*dp+sp)+dp)
  end subroutine neighbor_init

  !> Set up, allocate, and prepare  the basic cell list data
  !! When called the first time, print out diagnostic info.
  subroutine neighbor_setup(max_cutoff)
    use io
    use message_passing, only: mp_error
    use atoms,      only : get_natoms, coord_s2r
    use memory,     only : adjust_mem, memory_print, alloc_vec
    use cell,       only : get_hmat
    use sysinfo_io, only : get_neigh_nlevel, get_neigh_ratio, &
         get_neigh_skin, get_neigh_check, get_newton
    real(kind=dp), intent(in) :: max_cutoff
    real(kind=dp) :: cutsq, ratio, hmat(6), offset(3), delta(3)
    integer :: nlevel, i, j, k, ip, jp, kp, n, idx, jdx, npruned, nstencil

    call get_hmat(hmat)
    nlevel = get_neigh_nlevel()
    ratio  = get_neigh_ratio()
    cutoff = max_cutoff + get_neigh_skin()
    cutsq = cutoff*cutoff
    do_check = get_neigh_check()
    newton = get_newton()

    ! deallocate previously allocated storage
    if (.not. first_call) then
       do i=1,nx
          do j=1,ny
             do k=1,nz
                deallocate(list(i,j,k)%list)
             end do
          end do
       end do
       call adjust_mem(-ncells*(nlist+1)*sp)
       deallocate(list)
       call adjust_mem((-ncells-nghosts)*(4*dp+sp))
    end if

    ! compute local cell grid. allow cutoff larger than 1/2 cell
    nx = int(dble(nlevel)*hmat(1)/cutoff)
    if (nx < 1) nx = 1
    ny = int(dble(nlevel)*hmat(2)/cutoff)
    if (ny < 1) ny = 1
    nz = int(dble(nlevel)*hmat(3)/cutoff)
    if (nz < 1) nz = 1

    dx = d_one/dble(nx)
    dy = d_one/dble(ny)
    dz = d_one/dble(nz)
    ngx = int(cutoff/(dx*hmat(1))+d_one)
    ngy = int(cutoff/(dy*hmat(2))+d_one)
    ngz = int(cutoff/(dz*hmat(3))+d_one)
    ncells = nx*ny*nz
    nghosts = (nx+2*ngx)*(nz+2*ngz)*(nz+2*ngz) - ncells
    nstencil = (2*ngx+1)*(2*ngz+1)*(2*ngz+1)
    nlist = int(dble(get_natoms())/dble(ncells)*ratio)

    allocate(list(1-ngx:nx+ngx,1-ngy:ny+ngy,1-ngz:nz+ngz))
    call adjust_mem((ncells+nghosts)*(4*dp+sp))
    idx = 0
    do k=1,nz
       do j=1,ny
          do i=1,nx
             allocate(list(i,j,k)%list(0:nlist))
             list(i,j,k)%idx = idx
             idx = idx + 1
          end do
       end do
    end do
    call adjust_mem(ncells*(nlist+1)*sp)

    ! map ghost cells to non-ghosts
    do kp=1-ngz,nz+ngz
       offset(3) = d_zero
       k = kp
       do while (k < 1)
          k = k + nz
          offset(3) = offset(3) - d_one
       end do
       do while (k > nz)
          k = k - nz
          offset(3) = offset(3) + d_one
       end do

       do jp=1-ngy,ny+ngy
          offset(2) = d_zero
          j = jp
          do while (j < 1)
             j = j + ny
             offset(2) = offset(2) - d_one
          end do
          do while (j > ny)
             j = j - ny
             offset(2) = offset(2) + d_one
          end do

          do ip=1-ngx,nx+ngx
             offset(1) = d_zero
             i = ip
             do while (i < 1)
                i = i + nx
                offset(1) = offset(1) - d_one
             end do
             do while (i > nx)
                i = i - nx
                offset(1) = offset(1) + d_one
             end do

             list(ip,jp,kp)%offset(:) = offset(:)
             ! if we have a ghost cell, assign pointer to original cell
             if ((ip < 1) .or. (ip > nx) .or. (jp < 1) .or. (jp > ny) &
                  .or. (kp < 1) .or. (kp > nz)) then
                list(ip,jp,kp)%list => list(i,j,k)%list
                list(ip,jp,kp)%idx = idx
                idx = idx + 1
             endif
          end do
       end do
    end do

    ! compute an upper level of entries for cell pair list
    npairs = ncells*((2*ngx+1)*(2*ngz+1)*(2*ngz+1))

    ! build a list of all cell pairs by looping over the non-ghost cells,
    ! taking a stencil around it and pruning off all cells pairs that are
    ! too far apart. this is refined in the neighbor_build call based on
    ! current atom positions plus neighbor skin.
    call alloc_vec(cell_pairs,6*npairs)
    n = 0
    npruned = 0
    do k=1,nz
       do j=1,ny
          do i=1,nx
             do kp=k-ngz,k+ngz
                do jp=j-ngy,j+ngy
                   do ip=i-ngx,i+ngx

                      jdx = list(ip,jp,kp)%idx
                      ! for real j cells, we have to skip half of the pairs 
                      if (newton .and. (jdx < ncells)) then
                         idx = list(i,j,k)%idx
                         if ((idx > jdx) .and. (mod(idx+jdx,2)==0)) cycle
                         if ((idx < jdx) .and. (mod(idx+jdx,2)==1)) cycle
                      end if

                      ! check if this cell pair can be skipped completely
                      if (i /= ip) then
                         delta(1) = dx * (abs(ip-i) - 1)
                      else
                         delta(1) = d_zero
                      end if
                      if (j /= jp) then
                         delta(2) = dy * (abs(jp-j) - 1)
                      else
                         delta(2) = d_zero
                      end if
                      if (k /= kp) then
                         delta(3) = dz * (abs(kp-k) - 1)
                      else
                         delta(3) = d_zero
                      end if
                      call coord_s2r(delta,offset)

                      ! don't add cell pairs at all that are too far away
                      if (sum(offset*offset) > cutsq) then
                         npruned = npruned + 1
                      else
                         cell_pairs%v(6*n+1) = i
                         cell_pairs%v(6*n+2) = j
                         cell_pairs%v(6*n+3) = k
                         cell_pairs%v(6*n+4) = ip
                         cell_pairs%v(6*n+5) = jp
                         cell_pairs%v(6*n+6) = kp
                         n = n + 1
                      end if
                   end do
                end do
             end do
          end do
       end do
    end do
    npairs = n
    if (do_check) call alloc_vec(old_pos,get_natoms())

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
       write(stdout,*) 'Number of cell pairs allocated  : ', cell_pairs%size/6
       write(stdout,*) 'Number of pruned cell pairs     : ', npruned
       write(stdout,*) 'Number of cell pairs used       : ', npairs
       write(stdout,fmt='(A,F15.2)') ' Average number of atoms per cell: ', &
            dble(get_natoms())/dble(ncells)
       write(stdout,*) 'Maximum allowed atoms per cell  : ', nlist
    end if

    call neighbor_build

    ! print status only by io task and only on first call
    if (first_call .and. mp_ioproc()) then
       write(stdout,*) 'Maximum actual atoms per cell   : ', maxlist
       write(stdout,fmt='(A,F15.2)') ' Dynamically pruned cell pairs   : ', &
            sumpruned/dble(nbuild)
       call memory_print
    end if
    first_call = .false.
  end subroutine neighbor_setup

  !> Sort all atoms into their respective cells, update their image flags
  !! (and implicitly wrap the coordinates for periodic boundary conditions)
  subroutine neighbor_build
    use io
    use atoms, only : get_natoms, get_x_s, get_x_r, coord_s2r, &
         update_image, copy_vec
    use sysinfo_io, only : get_neigh_nlevel
    real(kind=dp), pointer :: x(:), y(:), z(:)
    integer, pointer :: ilist(:), jlist(:), cp(:)
    type(neigh_cell) :: icell, jcell
    real(kind=dp) :: joffs(3), xtmp, ytmp, ztmp, delx, dely, delz, rsq,cutsq
    integer :: natoms, n, i, j, ix, iy, iz, in, jn, inum, jnum, nskip
    logical :: hit

    natoms = get_natoms()
    call get_x_s(x,y,z)

    ! clear list entries
    do iz=1,nz
       do iy=1,ny
          do ix=1,nx
             list(ix,iy,iz)%list(0) = 0
          end do
       end do
    end do

    ! compute atom position 
    do i=1,natoms
       ix = int(x(i)/dx+d_one)
       iy = int(y(i)/dy+d_one)
       iz = int(z(i)/dz+d_one)

       do while (ix < 1)
          ix = ix + nx
          call update_image(i,-1,'x')
       end do
       do while (ix > nx)
          ix = ix - nx
          call update_image(i,1,'x')
       end do

       do while (iy < 1)
          iy = iy + ny
          call update_image(i,-1,'y')
       end do
       do while (iy > ny)
          iy = iy - ny
          call update_image(i,1,'y')
       end do

       do while (iz < 1)
          iz = iz + nz
          call update_image(i,-1,'z')
       end do
       do while (iz > nz)
          iz = iz - nz
          call update_image(i,1,'z')
       end do

       n = list(ix,iy,iz)%list(0) + 1
       if (n > nlist) &
            call mp_error('Cell list overflow. Increase neigh_ratio.',n)
       if (n > maxlist) maxlist = n
       list(ix,iy,iz)%list(0) = n
       list(ix,iy,iz)%list(n) = i
    end do

    ! do a second round of pruning, now based on (absolute) atom coordinates
    call get_x_r(x,y,z)
    cutsq = cutoff*cutoff
    cp => cell_pairs%v
    nskip = 0
    do n=0,npairs-1
       icell = list(cp(6*n+1),cp(6*n+2),cp(6*n+3))
       jcell = list(cp(6*n+4),cp(6*n+5),cp(6*n+6))

       ! no chance to prune here
       if (icell%idx == jcell%idx) cycle

       ! loop over pairs of atoms between the two cells
       ilist => icell%list
       inum = ilist(0)
       jlist => jcell%list
       jnum = jlist(0)
       call coord_s2r(jcell%offset,joffs)

       hit = .false.
       do in=1,inum
          i = ilist(in)
          xtmp = x(i) - joffs(1)
          ytmp = y(i) - joffs(2)
          ztmp = z(i) - joffs(3)

          do jn=1,jnum
             j = jlist(jn)

             delx = xtmp - x(j)
             dely = ytmp - y(j)
             delz = ztmp - z(j)
             rsq = delx*delx + dely*dely + delz*delz

             if (rsq <= cutsq) then
                hit = .true.
                exit
             end if
          end do
          ! no need to loop any further
          if (hit) exit
       end do

       ! record whether to skip this cell pair with the current set of atoms
       if (hit) then
          cp(6*n+1) = abs(cp(6*n+1))
       else
          cp(6*n+1) = -abs(cp(6*n+1))
          nskip = nskip + 1
       end if
    end do
    sumpruned = sumpruned + dble(nskip)
    nbuild = nbuild + 1
    if (do_check) call copy_vec(old_pos,'pos')

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
