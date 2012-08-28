!> Module to store and maintain per atom data
module atoms
  use kinds
  use constants
  use io, only: stdout
  use threading, only: thr_get_num_threads
  use message_passing, only: mp_error
  use control_io, only: is_verbose
  implicit none

  private
  integer :: natoms, ntypes
  logical :: valid_x_r, valid_x_s
  logical :: have_chg, have_pos
  integer, parameter :: ndeftypes = 16
  type (xyz_vec)   :: x_r, x_s, vel, for
  type (dp_vec)    :: chg, mss
  type (int_vec)   :: typ, idx
  type (label_vec) :: lbl

  public :: atoms_init, atoms_resize, atoms_replicate, types_init
  public :: set_type, set_idx, set_mass, set_charge, set_pos
  public :: get_ntypes, get_natoms
  public :: ndeftypes

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
    idx%size = -1
    chg%size = -1
    mss%size = -1
    lbl%size = -1
    call adjust_mem(5*(dp+sp))

    call alloc_vec(mss,maxtypes)
    call alloc_vec(lbl,maxtypes)
  end subroutine atoms_init
  
  subroutine atoms_resize(size)
    use memory, only: alloc_vec
    integer, intent(in) :: size
    integer :: nthr

    nthr = thr_get_num_threads()
    call alloc_vec(x_r,size)
    call alloc_vec(x_s,size)
    call alloc_vec(vel,size)
    call alloc_vec(for,size*nthr)
    call alloc_vec(typ,size)
    call alloc_vec(idx,size)
    call alloc_vec(chg,size)
    natoms = size
  end subroutine atoms_resize

  subroutine atoms_replicate(size)
    use memory, only: alloc_vec
    use message_passing, only: mp_bcast
    integer, intent(in) :: size
    integer :: nthr

    nthr = thr_get_num_threads()

    call mp_bcast(natoms)
    call mp_bcast(x_r)
    valid_x_r = .true.
    call mp_bcast(typ)
    call mp_bcast(have_chg)
    if (have_chg) call mp_bcast(chg)
    call mp_bcast(mss)
    call alloc_vec(for,size*nthr)
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
             if (is_verbose()) write(stdout,fmt='(A,A,A,I3)') &
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
    if (is_verbose()) write(stdout,fmt='(A,I6,A,A,A,I3)') &
         ' Set type of atom     ',id,' with label: "', trim(name), &
         '" to no: ',newtype
  end function set_type

  subroutine set_idx(id,i)
    integer, intent(in) :: id,i

    if (i > size(idx%v)) &
         call mp_error('Too large atom index.',i)
    if (i <= 0) call mp_error('Atom index must be > 0',i)

    idx%v(id) = i
    if (is_verbose()) write(stdout,fmt='(A,I6,A,I6)') &
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
    if (is_verbose()) write(stdout,fmt='(A,I6,A,3F12.4)') &
         ' Set position of atom ',id,' to : ',pos(1),pos(2),pos(3)
  end subroutine set_pos

  subroutine set_mass(id,mass)
    integer, intent(in) :: id
    real(kind=dp), intent(in) :: mass

    if (id > size(mss%v)) &
         call mp_error('Too large atom type. Increase maxtypes.',id)
    if (mass <= d_zero) call mp_error('Mass must be > 0.0',id)

    mss%v(id) = mass
    if (is_verbose()) write(stdout,fmt='(A,I6,A,F12.4)') &
         ' Set mass of atom     ',id,' to : ',mass
  end subroutine set_mass

  subroutine set_charge(id,charge)
    integer, intent(in) :: id
    real(kind=dp), intent(in) :: charge
    
    if (charge /= d_zero) then
       have_chg = .true.
       chg%v(id) = charge
       if (is_verbose()) write(stdout,fmt='(A,I6,A,F12.4)') &
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
  
end module atoms
