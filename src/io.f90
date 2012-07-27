! module for io channel constants and general file handling
module io
  implicit none

  private
  public :: stdin, stdout, stderr

  integer, parameter :: stdin  = 5
  integer, parameter :: stdout = 6
  integer, parameter :: stderr = 0
end module io
