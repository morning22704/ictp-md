#!/bin/sh
# generate new version header file
exec > new.version.f90
cat <<EOF
subroutine version
  use io, only: stdout
  use messages, only: myrank, ioproc
  implicit none

  if (myrank == ioproc) then
     write (stdout,*) '=============='
     write (stdout,*) 'ASAP MD REBOOT'
     write (stdout,*) '=============='
EOF
git log -1 HEAD | head -3 | \
    sed -e 's/^commit/Last commit:/' -e "s/^\(.*\)/    write (stdout,*) '\1'/"
cat <<EOF
     write (stdout,*) '=============='
  end if
end subroutine version
EOF

# check if changed and replace
cmp -s version.f90 new.version.f90 && rm -f new.version.f90 || mv new.version.f90 version.f90
