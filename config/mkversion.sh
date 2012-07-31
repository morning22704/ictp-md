#!/bin/sh
# generate new version header file
exec > new.version.f90
date=`date -R`
host=`hostname -s`
flags="$@"
cat <<EOF
subroutine version
  use io, only: stdout
  use message_passing, only: mp_ioproc
  implicit none

  if (mp_ioproc()) then
     write (stdout,*) '================'
     write (stdout,*) ' ASAP MD REBOOT '
     write (stdout,*) '================'
     write (stdout,*) '-------------------------------------------------------'
     write (stdout,*) 'Compile date : ${date} on ${host}'
     write (stdout,*) 'Compile flags: ${flags}'
     write (stdout,*) '-------------------------------------------------------'
EOF
git log -n 1 --pretty="     write (stdout,*) 'Last commit  : %H'"
git log -n 1 --pretty="     write (stdout,*) 'Commit date  : %aD'" 
git log -n 1 --pretty="     write (stdout,*) 'Commit author: %an <%ae>'" 
cat <<EOF
     write (stdout,*) '-------------------------------------------------------'
  end if
end subroutine version
EOF

# check if changed and replace
cmp -s version.f90 new.version.f90 && rm -f new.version.f90 || mv new.version.f90 version.f90
