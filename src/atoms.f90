!> Module to store and maintain per atom data
module atoms
  use kinds
  use constants
  use io, only: stdout
  use threading, only: thr_get_num
  use message_passing, only: mp_error
  use control_io, only: is_debug
  implicit none

  private
  type (xyz_vec)   :: x_r, x_s, vel, for
  type (dp_vec)    :: chg, mss
  type (int_vec)   :: typ, idx, map, img
  type (label_vec) :: lbl
  integer :: natoms, ntypes
  logical :: valid_x_r, valid_x_s, newton
  logical :: have_chg, have_pos, have_vel
  integer, parameter :: ndeftypes = 16

  public :: atoms_init, atoms_setup, atoms_replicate, types_init
  public :: set_type, set_idx, set_mass, set_charge, set_pos, set_vel
  public :: get_ntypes, get_natoms, get_x_r, get_x_s, get_for, get_typ
  public :: is_chg, is_pos, is_vel
  public :: update_image, force_clear, xyz_write, coord_s2r, copy_vec
  public :: ndeftypes

  interface copy_vec
     module procedure copy_vec_xyz
!     module procedure copy_vec_dp
!     module procedure copy_vec_int
  end interface

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
    have_pos   = .false.
    have_vel   = .false.
    newton     = .true.
    call adjust_mem(6*sp)
    x_r%size = -1
    vel%size = -1
    for%size = -1
    x_s%size = -1
    call adjust_mem(4*(3*dp+sp))
    typ%size = -1
    idx%size = -1
    img%size = -1
    map%size = -1
    chg%size = -1
    mss%size = -1
    lbl%size = -1
    call adjust_mem(7*(dp+sp))

    call alloc_vec(mss,maxtypes)
    call alloc_vec(lbl,maxtypes)
  end subroutine atoms_init

  subroutine atoms_setup(size,newton_flag)
    use memory, only: alloc_vec, clear_vec
    integer, intent(in) :: size
    logical, intent(in) :: newton_flag
    integer :: nthr

    newton = newton_flag
    if (newton) then
       nthr = thr_get_num()
    else
       nthr = 1
    end if

    call alloc_vec(x_r,size)
    call alloc_vec(x_s,size)
    call alloc_vec(vel,size)
    call alloc_vec(for,size*nthr)
    call alloc_vec(typ,size)
    call alloc_vec(idx,size)
    call alloc_vec(img,size)
    call alloc_vec(map,size)
    call alloc_vec(chg,size)
    natoms = size
    call clear_vec(img)
  end subroutine atoms_setup

  subroutine atoms_replicate
    use memory, only: alloc_vec
    use message_passing, only: mp_bcast
    integer :: nthr

    call mp_bcast(natoms)
    call mp_bcast(newton)
    if (newton) then
       nthr = thr_get_num()
    else
       nthr = 1
    end if

    call mp_bcast(x_r)
    valid_x_r = .true.
    valid_x_s = .false.
    call mp_bcast(typ)
    call mp_bcast(idx)
    call mp_bcast(img)
    call mp_bcast(have_chg)
    if (have_chg) call mp_bcast(chg)
    call mp_bcast(mss)

    call alloc_vec(for,natoms*nthr)
    ! make scaled coordinates consistent
    call alloc_vec(x_s,natoms)
    call x2lambda
    call alloc_vec(map,natoms)
    call remap
  end subroutine atoms_replicate

  subroutine types_init(typelist,maxtypes)
    integer, intent(in) :: maxtypes
    character(len=lblen), dimension(ndeftypes), intent(in) :: typelist
    character(len=lblen) :: name
    integer :: n, i, newtype

    if (ntypes < 0) ntypes = 0

    do n=1,ndeftypes
       name=adjustl(typelist(n))
       newtype = -1

       if (trim(name) == 'UNKNOWN' .or. trim(name) == 'unknown') then
          newtype = 0
       elseif (trim(name) == 'X' .or. trim(name) == 'x') then
          call mp_error('Default type may not be "X" or "x"',ntypes)
          newtype = 0
       else
          do i=1,ntypes
             if (name == lbl%v(i)) newtype = i
          end do
          if (newtype < 0) then
             ntypes = ntypes + 1
             if (ntypes > size(lbl%v)) &
                  call mp_error('Too many atom types. Check input &
                  &or increase maxtypes.',ntypes)
             newtype = ntypes
             lbl%v(newtype) = name
             if (is_debug()) write(stdout,fmt='(A,A,A,I3)') &
                  ' Added new type: "', name, '" as type no: ',newtype
          end if
       end if
       if (ntypes == maxtypes) exit
    end do
  end subroutine types_init

  function set_type(id,name)
    integer set_type
    integer, intent(in) :: id
    character(len=lblen), intent(in) :: name
    integer :: i, newtype

    if (ntypes < 0) ntypes = 0
    newtype = -1

    if (trim(name) == 'X' .or. trim(name) == 'x') then
       newtype = 0
    else
       do i=1,ntypes
          if (trim(name) == trim(lbl%v(i))) newtype = i
       end do
       if (newtype < 0) then
          ntypes = ntypes + 1
          if (ntypes > size(lbl%v)) &
               call mp_error('Too many atom types. Check input &
               &or increase maxtypes.',ntypes)
          newtype = ntypes
          lbl%v(newtype) = name
       end if
    end if
    typ%v(id) = newtype
    set_type = newtype
    if (is_debug()) write(stdout,fmt='(A,I6,A,A,A,I3)') &
         ' Set type of atom     ',id,' with label: "', trim(name), &
         '" to no: ',newtype
  end function set_type

  subroutine set_idx(id,i)
    integer, intent(in) :: id,i

    if (i > size(idx%v)) &
         call mp_error('Too large atom index.',i)
    if (i <= 0) call mp_error('Atom index must be > 0',i)

    idx%v(id) = i
    if (is_debug()) write(stdout,fmt='(A,I6,A,I6)') &
         ' Set id of atom       ',id,' to no: ',i
  end subroutine set_idx

  subroutine set_pos(id,pos)
    integer, intent(in) :: id
    real(kind=dp), dimension(3), intent(in) :: pos

    have_pos = .true.
    valid_x_s = .false.
    x_r%x(id) = pos(1)
    x_r%y(id) = pos(2)
    x_r%z(id) = pos(3)
    if (is_debug()) write(stdout,fmt='(A,I6,A,3F12.4)') &
         ' Set position of atom ',id,' to : ',pos(1),pos(2),pos(3)
  end subroutine set_pos

  subroutine set_vel(id,v)
    integer, intent(in) :: id
    real(kind=dp), dimension(3), intent(in) :: v

    have_vel = .true.
    vel%x(id) = v(1)
    vel%y(id) = v(2)
    vel%z(id) = v(3)
    if (is_debug()) write(stdout,fmt='(A,I6,A,3F12.4)') &
         ' Set velocity of atom ',id,' to : ',v(1),v(2),v(3)
  end subroutine set_vel

  subroutine set_mass(id,mass)
    integer, intent(in) :: id
    real(kind=dp), intent(in) :: mass

    if (id > size(mss%v)) &
         call mp_error('Too large atom type. Increase maxtypes.',id)
    if (mass <= d_zero) call mp_error('Mass must be > 0.0',id)

    mss%v(id) = mass
    if (is_debug()) write(stdout,fmt='(A,I6,A,F12.4)') &
         ' Set mass of atom     ',id,' to : ',mass
  end subroutine set_mass

  subroutine set_charge(id,charge)
    integer, intent(in) :: id
    real(kind=dp), intent(in) :: charge

    if (charge /= d_zero) then
       have_chg = .true.
       chg%v(id) = charge
       if (is_debug()) write(stdout,fmt='(A,I6,A,F12.4)') &
            ' Set charge of atom   ',id,' to : ',charge
    end if
  end subroutine set_charge

  function get_ntypes()
    integer :: get_ntypes

    get_ntypes = ntypes
  end function get_ntypes

  function get_natoms()
    integer :: get_natoms

    get_natoms = natoms
  end function get_natoms

  subroutine get_x_r(x,y,z)
    real(kind=dp), pointer :: x(:), y(:), z(:)

    if (.not. valid_x_r) call lambda2x
    x => x_r%x
    y => x_r%y
    z => x_r%z
  end subroutine get_x_r

  subroutine get_x_s(x,y,z)
    real(kind=dp), pointer :: x(:), y(:), z(:)

    if (.not. valid_x_s) call x2lambda
    x => x_s%x
    y => x_s%y
    z => x_s%z
  end subroutine get_x_s

  subroutine get_for(x,y,z)
    use threading, only : thr_get_rank
    real(kind=dp), pointer :: x(:), y(:), z(:)
    integer :: offset

    if (newton) then
       offset = natoms*thr_get_rank() + 1
       x => for%x(offset:offset+natoms)
       y => for%y(offset:offset+natoms)
       z => for%z(offset:offset+natoms)
    else
       x => for%x
       y => for%y
       z => for%z
    end if
  end subroutine get_for

  subroutine get_typ(v)
    integer, pointer :: v(:)

    v => typ%v
  end subroutine get_typ

  function is_chg()
    logical :: is_chg

    is_chg = have_chg
  end function is_chg

  function is_pos()
    logical :: is_pos

    is_pos = have_pos
  end function is_pos

  function is_vel()
    logical :: is_vel

    is_vel = have_vel
  end function is_vel

  subroutine xyz_write(channel,what)
    use io,     only : stdout
    use memory, only : alloc_vec, free_vec
    integer, intent(in)          :: channel
    character(len=3), intent(in) :: what
    type (xyz_vec) :: vec
    real(kind=dp), pointer :: x(:), y(:), z(:)
    integer :: i

    if (what == 'pos') then
       write(stdout,*) 'Writing out unwrapped positions in xyz format'
       call alloc_vec(vec,natoms)
       call lambda2pos(vec)
    else if (what == 'x_r') then
       write(stdout,*) 'Writing out wrapped positions in xyz format'
       vec = x_r
       call lambda2x
    else if (what == 'x_s') then
       write(stdout,*) 'Writing out scaled positions in xyz format'
       vec = x_s
       call x2lambda
    else if (what == 'vel') then
       write(stdout,*) 'Writing out velocities in xyz format '
       vec = vel
    else if (what == 'for') then
       write(stdout,*) 'Writing out forces in xyz format'
       vec = for
    else
       write(stdout,*) 'Ignoring unknown xyz data type: ', what
       return
    end if

    x => vec%x
    y => vec%y
    z => vec%z
    write(channel,*) natoms
    write(channel,*) 'Written by ICTP-MD'
    do i=1,natoms
       write(channel,fmt='(A17,3G21.13)') lbl%v(typ%v(i)), x(i), y(i), z(i)
    end do

    ! avoid memory leak when using a copy
    if (what == 'pos') then
       call free_vec(vec)
    end if
  end subroutine xyz_write

  subroutine copy_vec_xyz(vec,what)
    use memory, only : alloc_vec
    type (xyz_vec), intent(inout) :: vec
    character(len=3), intent(in) :: what

    call alloc_vec(vec,natoms)
    if (what == 'pos') then
       call lambda2pos(vec)
    else if (what == 'x_r') then
       call lambda2x
       vec%x(:) = x_r%x(:)
       vec%y(:) = x_r%y(:)
       vec%z(:) = x_r%z(:)
    else if (what == 'x_s') then
       call x2lambda
       vec%x(:) = x_s%x(:)
       vec%y(:) = x_s%y(:)
       vec%z(:) = x_s%z(:)
    else if (what == 'vel') then
       vec%x(:) = vel%x(:)
       vec%y(:) = vel%y(:)
       vec%z(:) = vel%z(:)
       vec = vel
    else if (what == 'for') then
       vec%x(:) = for%x(1:natoms)
       vec%y(:) = for%y(1:natoms)
       vec%z(:) = for%z(1:natoms)
    end if

  end subroutine copy_vec_xyz

  !>   convert box coords to triclinic 0-1 lambda coords for all atoms
  !!   lambda = H^-1 (x - x0)
  subroutine x2lambda
    use cell, only: get_origin, get_hinv
    real(kind=dp) :: origin(3), hinv(6), delta(3)
    integer :: i

    if (valid_x_s) return

    call get_origin(origin)
    call get_hinv(hinv)

    do i=1,natoms
       delta(1) = x_r%x(i) - origin(1)
       delta(2) = x_r%y(i) - origin(2)
       delta(3) = x_r%z(i) - origin(3)

       x_s%x(i) = hinv(1)*delta(1) + hinv(6)*delta(2) + hinv(5)*delta(3)
       x_s%y(i) = hinv(2)*delta(2) + hinv(4)*delta(3)
       x_s%z(i) = hinv(3)*delta(3)
    end do

    valid_x_s = .true.

  end subroutine x2lambda

  !> convert triclinic 0-1 lambda coords to box coords for all atoms
  !! x = H lambda + x0;
  subroutine lambda2x
    use cell, only: get_origin, get_hmat
    real(kind=dp) :: origin(3), hmat(6), x,y,z
    integer :: i

    if (valid_x_r) return

    call get_origin(origin)
    call get_hmat(hmat)

    do i=1,natoms
       x = x_s%x(i)
       y = x_s%y(i)
       z = x_s%z(i)

       x_r%x(i) = hmat(1)*x + hmat(6)*y + hmat(5)*z + origin(1)
       x_r%y(i) = hmat(2)*y + hmat(4)*z + origin(2)
       x_r%z(i) = hmat(3)*z + origin(3)
    end do
    valid_x_r = .true.
  end subroutine lambda2x

  !> convert triclinic 0-1 lambda coords to box coords
  !! and unwrap according to image flags in the process
  subroutine lambda2pos(vec)
    use cell, only: get_origin, get_hmat
    type (xyz_vec), intent(inout) :: vec
    real(kind=dp) :: origin(3), hmat(6), x,y,z
    integer :: i,n,mx,my,mz

    if (.not.valid_x_s) call x2lambda

    call get_origin(origin)
    call get_hmat(hmat)
    do i=1,natoms
       n = img%v(i)
       mx = 0
       my = 0
       mz = 0
       call mvbits(n, 0,10,mx,0)
       call mvbits(n,10,10,my,0)
       call mvbits(n,20,10,mz,0)
       if (mx > 512) mx = mx - 1024
       if (my > 512) my = my - 1024
       if (mz > 512) mz = mz - 1024
       x = x_s%x(i) + dble(mx)
       y = x_s%y(i) + dble(my)
       z = x_s%z(i) + dble(mz)

       vec%x(i) = hmat(1)*x + hmat(6)*y + hmat(5)*z + origin(1)
       vec%y(i) = hmat(2)*y + hmat(4)*z + origin(2)
       vec%z(i) = hmat(3)*z + origin(3)
    end do

  end subroutine lambda2pos

  !> convert single triclinic 0-1 lambda coord to box coords
  !! x = H lambda + x0;
  subroutine coord_s2r(ivec,ovec,add_origin)
    use cell, only: get_origin, get_hmat
    real(kind=dp), intent(in)  :: ivec(3)
    logical, intent(in) :: add_origin
    real(kind=dp), intent(out) :: ovec(3)
    real(kind=dp) :: origin(3), hmat(6)

    if (add_origin) then
       call get_origin(origin)
    else
       origin(:) = d_zero
    end if
    call get_hmat(hmat)

    ovec(1) = hmat(1)*ivec(1) + hmat(6)*ivec(2) + hmat(5)*ivec(3) + origin(1)
    ovec(2) = hmat(2)*ivec(2) + hmat(4)*ivec(3) + origin(2)
    ovec(3) = hmat(3)*ivec(3) + origin(3)
  end subroutine coord_s2r

  subroutine remap
    integer :: i
    do i=1,natoms
       map%v(idx%v(i)) = i
    end do
  end subroutine remap

  subroutine update_image(i,n,d)
    integer, intent(in) :: i, n
    character, intent(in) :: d
    integer :: m, o

    if (.not. valid_x_s) call x2lambda

    if (d == 'x') then
       o = 0
       x_s%x(i) = x_s%x(i) - n*d_one
       valid_x_r = .false.
    else if (d == 'y') then
       o = 10
       x_s%y(i) = x_s%y(i) - n*d_one
       valid_x_r = .false.
    else if (d == 'z') then
       o = 20
       x_s%z(i) = x_s%z(i) - n*d_one
       valid_x_r = .false.
    else
       o = 30
    endif

    m = 0
    call mvbits(img%v(i),o,10,m,0)
    if (m > 512) m = m - 1024 
    m = m + n
    if (m < 0) m = m + 1024
    call mvbits(m,0,10,img%v(i),o)

  end subroutine update_image

  subroutine force_clear
    use memory, only: clear_vec
    call clear_vec(for)
  end subroutine force_clear
end module atoms
