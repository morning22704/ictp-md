#!/bin/sh
# generate new version header file
exec > new.version.f90
date=`date -R`
host=`hostname -s`
flags="$@"
cat <<EOF
!> Module to provide program version info.
module header

contains

!> Print a program version banner
!! @param channel I/O unit to print banner to.
subroutine version(channel)
  use message_passing, only: mp_ioproc
  implicit none
  integer, intent(in) :: channel

  if (mp_ioproc()) then
     write (channel,*) '=================='
     write (channel,*) ' ICTP MD TEMPLATE '
     write (channel,*) '=================='
     write (channel,*) '-------------------------------------------------------'
     write (channel,*) 'Compile date : ${date} on ${host}'
     write (channel,*) 'Compile flags: ${flags}'
     write (channel,*) '-------------------------------------------------------'
EOF
git log -n 1 --pretty="     write (channel,*) 'Last commit  : %H'"
git log -n 1 --pretty="     write (channel,*) 'Commit date  : %aD'" 
git log -n 1 --pretty="     write (channel,*) 'Commit author: %an <%ae>'" 
cat <<EOF
     write (channel,*) '-------------------------------------------------------'
  end if
end subroutine version
end module header
EOF

# check if changed and replace
cmp -s version.f90 new.version.f90 && rm -f new.version.f90 || mv new.version.f90 version.f90
