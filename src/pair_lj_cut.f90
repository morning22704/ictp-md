!> Module for computing a 12-6 Lennard-Jones pairwise potential
!!
module pair_lj_cut
  use kinds
  use constants
  use utils
  use message_passing, only : mp_error, mp_ioproc, mp_bcast
  implicit none

  private
  type(dp_mat)  :: epsil  !< LJ parameter
  type(dp_mat)  :: sigma  !< LJ parameter
  type(dp_mat)  :: cutsq  !< Squared pairwise cutoff
  type(dp_mat)  :: offset !< Energy shift at cutoff
  type(dp_mat)  :: lj1    !< Preprocessed LJ parameter
  type(dp_mat)  :: lj2    !< Preprocessed LJ parameter
  type(dp_mat)  :: lj3    !< Preprocessed LJ parameter
  type(dp_mat)  :: lj4    !< Preprocessed LJ parameter

  public :: pair_lj_cut_init, pair_lj_cut_read, pair_lj_cut_write
  public :: pair_lj_cut_compute

contains

  ! set defaults
  subroutine pair_lj_cut_init
    use io, only: stdout
    use memory, only: adjust_mem

    epsil%sizex=-1
    epsil%sizey=-1
    sigma%sizex=-1
    sigma%sizey=-1
    cutsq%sizex=-1
    cutsq%sizey=-1
    offset%sizex=-1
    offset%sizey=-1
    lj1%sizex=-1
    lj1%sizey=-1
    lj2%sizex=-1
    lj2%sizey=-1
    lj3%sizex=-1
    lj3%sizey=-1
    lj4%sizex=-1
    lj4%sizey=-1
    call adjust_mem(8*(dp+2*sp))
    if (mp_ioproc()) write(stdout,*) 'Adding pair style: lj/cut'
  end subroutine pair_lj_cut_init

  subroutine read_coeffs(channel,ntypes,cutoff_max,setflag)
    integer, intent(in) :: channel, ntypes
    real(kind=dp), intent(inout) :: cutoff_max
    type(int_mat), intent(inout) :: setflag
    character(len=lilen) :: line
    logical :: not_done
    integer :: ierr, t1, t2, count
    real(kind=dp) :: eps, sig, cut

    not_done = .true.
    count = 0
    do while (not_done)
       read(unit=channel,fmt='(A)',iostat=ierr) line
       line = adjustl(line)
       count = count + 1
       if (is_section_end(trim(line))) exit
       if (ierr /= 0) call mp_error('Error reading pair_coeff section',ierr)

       ! try reading with cutoff and if it fails without
       read(line,fmt=*,iostat=ierr) t1, t2, eps, sig, cut
       if (ierr /= 0) then
          read(line,fmt=*,iostat=ierr) t1, t2, eps, sig
          if (ierr /= 0) &
               call mp_error('Bad format in pair_coeff section',count)
          cut = -d_one
       end if

       if ((t1 < 0) .or. (t1 > ntypes)) &
            call mp_error('Illegal 1st type in pair_coeff section',count)
       if ((t2 < 0) .or. (t2 > ntypes)) &
            call mp_error('Illegal 2nd type in pair_coeff section',count)

       if (cut > cutoff_max) cutoff_max = cut

       if ((t1 == 0) .and. (t2 == 0)) then
          epsil%m(:,:) = eps
          sigma%m(:,:) = sig
          if (cut > d_zero) cutsq%m(:,:) = cut*cut
          setflag%m(:,:) = 0
       else if (t1 == 0) then
          epsil%m(t2,:) = eps
          epsil%m(:,t2) = eps
          sigma%m(t2,:) = sig
          sigma%m(:,t2) = sig
          if (cut > d_zero) cutsq%m(t2,:) = cut*cut
          if (cut > d_zero) cutsq%m(:,t2) = cut*cut
          setflag%m(t2,:) = 0
          setflag%m(:,t2) = 0
       else if (t2 == 0) then
          epsil%m(t1,:) = eps
          epsil%m(:,t1) = eps
          sigma%m(t1,:) = sig
          sigma%m(:,t1) = sig
          if (cut > d_zero) cutsq%m(t1,:) = cut*cut
          if (cut > d_zero) cutsq%m(:,t1) = cut*cut
          setflag%m(t1,:) = 0
          setflag%m(:,t1) = 0
       else
          epsil%m(t1,t2) = eps
          epsil%m(t2,t1) = eps
          sigma%m(t1,t2) = sig
          sigma%m(t2,t1) = sig
          if (cut > d_zero) cutsq%m(t1,t2) = cut*cut
          if (cut > d_zero) cutsq%m(t2,t1) = cut*cut
          setflag%m(t1,t2) = 0
          setflag%m(t2,t1) = 0
       end if
    end do
  end subroutine read_coeffs

  !> Read pair style parameters from input
  subroutine pair_lj_cut_read(ntypes,cutoff_def,cutoff_max,shift_pot)
    use io
    use control_io, only : is_restart
    use memory, only : memory_print, alloc_mat, free_mat
    integer, intent(in) :: ntypes
    real(kind=dp), intent(in) :: cutoff_def
    real(kind=dp), intent(inout) :: cutoff_max
    logical, intent(in) :: shift_pot
    type(int_mat) :: setflag
    integer :: ierr,i,j
    real(kind=dp) :: ratio
    setflag%sizex=-1
    setflag%sizey=-1

    ! input is only read by io task
    if (mp_ioproc()) then

       call alloc_mat(epsil,ntypes,ntypes)
       call alloc_mat(sigma,ntypes,ntypes)
       call alloc_mat(cutsq,ntypes,ntypes)
       call alloc_mat(setflag,ntypes,ntypes)
       cutsq%m(:,:) = cutoff_def*cutoff_def
       setflag%m(:,:) = 1

       if (is_restart()) then
          write(stdout,*) 'Reading &pair_coeff section from restart'
          call find_section(resin,'pair_coeff')
          call read_coeffs(resin,ntypes,cutoff_max,setflag)
       end if

       write(stdout,*) 'Reading &pair_coeff section from input'
       call find_section(stdin,'pair_coeff')
       call read_coeffs(stdin,ntypes,cutoff_max,setflag)

       ierr = sum(setflag%m)
       if (ierr /= 0) call mp_error('Not all pair coefficienty are set',ierr)
       call memory_print
       call free_mat(setflag)

       write(stdout,*) 'Distributing and precomputing pair potential data'
       write(stdout,*) 'Maximal overall pair cutoff     : ', cutoff_max
    end if

    call mp_bcast(epsil)
    call mp_bcast(sigma)
    call mp_bcast(cutsq)
    call mp_bcast(cutoff_max)

    ! pre-compute some properties
    call alloc_mat(lj1,ntypes,ntypes)
    call alloc_mat(lj2,ntypes,ntypes)
    call alloc_mat(lj3,ntypes,ntypes)
    call alloc_mat(lj4,ntypes,ntypes)
    call alloc_mat(offset,ntypes,ntypes)
    do i=1,ntypes
       do j=i,ntypes
          lj1%m(i,j) = 48.0_dp * epsil%m(i,j) * sigma%m(i,j)**12
          lj1%m(j,i) = lj1%m(i,j)
          lj2%m(i,j) = 24.0_dp * epsil%m(i,j) * sigma%m(i,j)**6
          lj2%m(j,i) = lj2%m(i,j)
          lj3%m(i,j) =  4.0_dp * epsil%m(i,j) * sigma%m(i,j)**12
          lj3%m(j,i) = lj3%m(i,j)
          lj4%m(i,j) =  4.0_dp * epsil%m(i,j) * sigma%m(i,j)**6
          lj4%m(j,i) = lj4%m(i,j)
          if (shift_pot) then
             ratio = sigma%m(i,j) / cutsq%m(i,j)
             offset%m(i,j) = 4.0_dp * epsil%m(i,j) * (ratio**12 - ratio**6)
          else
             offset%m(i,j) = d_zero
          end if
          offset%m(j,i) = offset%m(i,j)
       end do
    end do
    if (mp_ioproc()) call memory_print
  end subroutine pair_lj_cut_read

  !> compute forces
  subroutine pair_lj_cut_compute(newton)
    use atoms
    use neighbor
    use sysinfo_io
    logical, intent(in) :: newton
    real(kind=dp), pointer :: x(:),y(:),z(:),fx(:),fy(:),fz(:)
    type(neigh_cell) :: icell, jcell
    integer, pointer :: cell_pairs(:), atype(:), ilist(:), jlist(:)
    integer :: n, nnum, inum, jnum, npairs, ncells
    integer :: idx, jdx, i, j, in, jn, itype, jtype
    real(kind=dp) :: xtmp, ytmp, ztmp, fxtmp, fytmp, fztmp, delx, dely, delz
    real(kind=dp) :: rsq, r2inv, r6inv, fpair, epair, evdwl, joffs(3)

    npairs=get_npairs()
    ncells=get_ncells()
    call get_cell_pairs(cell_pairs)
    call get_x_r(x,y,z)
    call get_for(fx,fy,fz)
    call get_typ(atype)
    epair = d_zero

    nnum = 6*npairs-1
    do n=0,nnum,6

       ! check if this is a skipped cell pair
       if (cell_pairs(n+1) < 0) cycle

       icell = get_cell(cell_pairs(n+1),cell_pairs(n+2),cell_pairs(n+3))
       jcell = get_cell(cell_pairs(n+4),cell_pairs(n+5),cell_pairs(n+6))
       idx = icell%idx
       jdx = jcell%idx
       call coord_s2r(jcell%offset,joffs,.false.)

       ! loop over pairs of atoms between the two cells
       ilist => icell%list
       inum = ilist(0)
       jlist => jcell%list
       jnum = jlist(0)

       do in=1,inum
          i = ilist(in)
          xtmp = x(i) - joffs(1)
          ytmp = y(i) - joffs(2)
          ztmp = z(i) - joffs(3)
          itype = atype(i)
          fxtmp = d_zero
          fytmp = d_zero
          fztmp = d_zero

          do jn=1,jnum
             j = jlist(jn)

             ! handle cases we don't need to compute
             if ((idx == jdx) .and. (i == j)) cycle
             if ((idx == jdx) .and. newton) then
                if ((i > j).and. (mod(i+j,2)==0)) cycle
                if ((i < j).and. (mod(i+j,2)==1)) cycle
             end if
             jtype = atype(j)
             delx = xtmp - x(j)
             dely = ytmp - y(j)
             delz = ztmp - z(j)
             rsq = delx*delx + dely*dely + delz*delz

             if (rsq < cutsq%m(itype,jtype)) then
                r2inv = d_one/rsq
                r6inv = r2inv*r2inv*r2inv
                fpair = r6inv * (lj1%m(itype,jtype)*r6inv &
                     - lj2%m(itype,jtype)) * r2inv
                evdwl = r6inv*(lj3%m(itype,jtype)*r6inv &
                     - lj4%m(itype,jtype)) - offset%m(itype,jtype)

                fxtmp = fxtmp + delx*fpair
                fytmp = fytmp + dely*fpair
                fztmp = fztmp + delz*fpair

                if (newton .and. (jdx < ncells)) then
                   fx(j) = fx(j) - delx*fpair
                   fy(j) = fy(j) - dely*fpair
                   fz(j) = fz(j) - delz*fpair
                   epair = epair + evdwl
                else
                   epair = epair + d_half * evdwl
                end if
             end if
          end do
          fx(i) = fx(i) + fztmp
          fy(i) = fy(i) + fytmp
          fz(i) = fz(i) + fxtmp
       end do
    end do

    write(*,fmt='(A,F15.8)') 'epair: ', epair
    open(11,file='forces.xyz',form='formatted',status='unknown')
    call xyz_write(11,'for')
    close(11)
    
  end subroutine pair_lj_cut_compute

  subroutine pair_lj_cut_write(channel,ntypes)
    use io
    integer, intent(in) :: channel, ntypes
    integer :: i,j

    if (mp_ioproc()) then
       write(channel,*) '&PAIR_COEFF'
       do i=1,ntypes
          do j=i,ntypes
             write(channel,*) i,j,epsil%m(i,j),sigma%m(i,j),sqrt(cutsq%m(i,j))
          end do
       end do
       write(channel,*) '/'
    end if
  end subroutine pair_lj_cut_write

end module pair_lj_cut
