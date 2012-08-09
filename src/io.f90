!> Module for I/O channel constants and general file handling.
!!
!! The primary purpose of the module is to use more easy to
!! remember constants for I/O channels and also to make more
!! difficult to use the same channel multiple times by accident.
module io
  implicit none

  private
  public :: stdin, stdout, stderr, resin, resout, separator

  integer, parameter :: stdin  = 5  !< predefined channel for console input
  integer, parameter :: stdout = 6  !< predefined channel for console output
  integer, parameter :: stderr = 0  !< predefined channel for error output
  integer, parameter :: resin  = 15 !< channel to be used for reading restarts
  integer, parameter :: resout = 16 !< channel to be used for writing restarts 
  !> string constant for separator lines
  character(len=55), parameter :: &
    separator = '-------------------------------------------------------'

 
end module io
