! module to define data kinds
module kinds
  implicit none

  private
  public :: sp, dp

  integer, parameter :: sp = selected_real_kind(6,30)
  integer, parameter :: dp = selected_real_kind(14,200)
end module kinds
