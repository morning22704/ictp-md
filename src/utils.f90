!> Module with utility subroutines

module utils
  use constants
  implicit none
  private
  character(len=lilen) :: line

  public utils_init, find_section, is_section_end

contains

  subroutine utils_init
    use memory, only : adjust_mem

    call adjust_mem(lilen+sp)
  end subroutine utils_init

  function is_section_end(string)
    logical is_section_end
    character(len=*), intent(in) :: string

    is_section_end = .false.
    if (trim(string) == '/') is_section_end = .true.
    if (trim(string) == '&end') is_section_end = .true.
    if (trim(string) == '&END') is_section_end = .true.
    return
  end function is_section_end

  subroutine find_section(channel,section)
    use message_passing, only: mp_error
    integer, intent(in) :: channel
    character(len=*), intent(in) :: section
    integer :: ierr, i, char1, char2
    logical :: not_found

    not_found = .true.
    do while(not_found)
       read(unit=channel,fmt='(A)',iostat=ierr) line
       if (ierr /= 0) call mp_error('Error looking for section marker',ierr)

       ! section has to start with an '&' as first non whitespace character
       line = adjustl(line)
       if (line(1:1) /= '&') cycle

       ! the string length has to match
       line = line(2:)
       if (len(trim(line)) /= len(trim(section))) cycle

       ! case insensitive string comparision
       not_found = .false.
       do i=1,len(section)
          ! convert to lower case by setting bit 6
          char1 = ibset(ichar(section(i:i)),5)
          char2 = ibset(ichar(line(i:i)),5)
          if (char1 /= char2) not_found = .true.
       end do
    end do
  end subroutine find_section
end module utils
