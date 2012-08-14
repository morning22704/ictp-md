!> Module for I/O channel constants and general file handling.
!!
!! The primary purpose of the module is to use more easy to
!! remember constants for I/O channels and also to make more
!! difficult to use the same channel multiple times by accident.
module io
  implicit none

  public

  integer, parameter :: stdin  = 5  !< predefined channel for console input
  integer, parameter :: stdout = 6  !< predefined channel for console output
  integer, parameter :: stderr = 0  !< predefined channel for error output
  integer, parameter :: resin  = 15 !< channel to be used for reading restarts
  integer, parameter :: resout = 16 !< channel to be used for writing restarts 
  integer, parameter :: topin  = 25 !< channel to be used for reading topologies
  integer, parameter :: topout = 26 !< channel to be used for writing topologies
  integer, parameter :: geoin  = 35 !< channel to be used for reading geometries
  integer, parameter :: geoout = 36 !< channel to be used for writing geometries
  !> string constant for separator lines
  character(len=55), parameter :: &
    separator = '-------------------------------------------------------'
 
end module io
