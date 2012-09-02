!> Module for various useful constants and conversion factors
!!
!! Defining symbolic constants for simple floating point numbers
!! helps to avoid unintentional loss of accuracy by accidentally
!! using the (default) single precision representation.
!!
!! In addition, defining physical constants and conversion factors
!! in a single location helps to keep them consistent thoughout
!! the entire code and avoids errors due to differences in precision
!! or typos.
module constants
  use kinds
  implicit none

  public
  real(kind=dp), parameter :: d_zero  = 0.0_dp !< double precision zero
  real(kind=dp), parameter :: d_one   = 1.0_dp !< double precision one
  real(kind=dp), parameter :: d_half  = 0.5_dp !< double precision one
  real(kind=dp), parameter :: d_pi    = 3.14159265358979323846_dp !< Pi
  real(kind=dp), parameter :: d_small = 1.0e-12_dp !< very small number
  integer,       parameter :: kbyte  = 1024   !< kilobyte
end module constants
