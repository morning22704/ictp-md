! module for io channel constants and general file handling
module io
  implicit none

  private
  public :: stdin, stdout, stderr, resin, resout, separator

  integer, parameter :: stdin  = 5
  integer, parameter :: stdout = 6
  integer, parameter :: stderr = 0
  integer, parameter :: resin  = 15
  integer, parameter :: resout = 16
  character(len=55), parameter :: &
    separator = '-------------------------------------------------------'
end module io
