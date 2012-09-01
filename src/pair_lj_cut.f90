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
    write(stdout,*) 'Adding pair style: lj/cut'
  end subroutine pair_lj_cut_init

  subroutine read_coeffs(channel,ntypes,setflag)
    integer, intent(in) :: channel, ntypes
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
          
       if ((t1 == 0) .and. (t2 == 0)) then
          epsil%m(:,:) = eps
          sigma%m(:,:) = sig
          if (cut > d_zero) cutsq%m(:,:) = cut*cut
          setflag%m(:,:) = 1
       else if (t1 == 0) then
          epsil%m(t2,:) = eps
          epsil%m(:,t2) = eps
          sigma%m(t2,:) = sig
          sigma%m(:,t2) = sig
          if (cut > d_zero) cutsq%m(t2,:) = cut*cut
          if (cut > d_zero) cutsq%m(:,t2) = cut*cut
          setflag%m(t2,:) = 1
          setflag%m(:,t2) = 1
       else if (t2 == 0) then
          epsil%m(t1,:) = eps
          epsil%m(:,t1) = eps
          sigma%m(t1,:) = sig
          sigma%m(:,t1) = sig
          if (cut > d_zero) cutsq%m(t1,:) = cut*cut
          if (cut > d_zero) cutsq%m(:,t1) = cut*cut
          setflag%m(t1,:) = 1
          setflag%m(:,t1) = 1
       else
          epsil%m(t1,t2) = eps
          epsil%m(t2,t1) = eps
          sigma%m(t1,t2) = sig
          sigma%m(t2,t1) = sig
          if (cut > d_zero) cutsq%m(t1,t2) = cut*cut
          if (cut > d_zero) cutsq%m(t2,t1) = cut*cut
          setflag%m(t1,t2) = 1
          setflag%m(t2,t1) = 1
       end if
    end do
  end subroutine read_coeffs

  !> Read pair style parameters from input
  subroutine pair_lj_cut_read(ntypes,cutoff_def)
    use io
    use control_io, only : is_restart
    use memory, only : memory_print, alloc_mat
    integer, intent(in) :: ntypes
    real(kind=dp), intent(in) :: cutoff_def
    type(int_mat) :: setval
    setval%sizex=-1
    setval%sizey=-1

    ! input is only read by io task
    if (mp_ioproc()) then

       call alloc_mat(epsil,ntypes,ntypes)
       call alloc_mat(sigma,ntypes,ntypes)
       call alloc_mat(cutsq,ntypes,ntypes)
       call alloc_mat(setval,ntypes,ntypes)
       cutsq%m(:,:) = cutoff_def*cutoff_def
       setval%m(:,:) = 0

       if (is_restart()) then
          write(stdout,*) 'Reading &pair_coeff section from restart'
          call find_section(resin,'pair_coeff')
          call read_coeffs(resin,ntypes,setval)
       end if

       write(stdout,*) 'Reading &pair_coeff section from input'
       call find_section(stdin,'pair_coeff')
       call read_coeffs(stdin,ntypes,setval)

       write(stdout,*) separator
       call memory_print
    end if

    call mp_bcast(epsil)
    call mp_bcast(sigma)
    call mp_bcast(cutsq)


  end subroutine pair_lj_cut_read

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
