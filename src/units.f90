!> Module with unit conversion factors

module units
  use kinds
  use constants
  implicit none

  public

  real(kind=dp) :: boltz;       !< Boltzmann constant (eng/degree-K)
  real(kind=dp) :: hplanck;     !< Planck's constant (energy-time)
  real(kind=dp) :: mvv2e;       !< conversion of mv^2 to energy
  real(kind=dp) :: ftm2v;       !< conversion of ft/m to velocity
  real(kind=dp) :: mv2d;        !< conversion of mass/volume to density
  real(kind=dp) :: nktv2p;      !< conversion of NkT/V to pressure
  real(kind=dp) :: qqr2e;       !< conversion of q^2/r to energy
  real(kind=dp) :: qe2f;        !< conversion of qE to force
  real(kind=dp) :: dielectric;  !< dielectric constant
  real(kind=dp) :: qqrd2e;      !< q^2/r to energy w/ dielectric constant
  real(kind=dp) :: angstrom;    !< 1 angstrom in native units
  real(kind=dp) :: femtosecond; !< 1 femtosecond in native units
  real(kind=dp) :: qelectron;   !< 1 electron charge abs() in native units

  real(kind=dp) :: default_dt   !< default value of time_step
  real(kind=dp) :: default_skin !< default value of neighborlist skin

contains

  subroutine set_units(style)
    use memory, only : adjust_mem
    use message_passing, only : mp_error
    character(len=*) :: style

    ! physical constants from:
    ! http://physics.nist.gov/cuu/Constants/Table/allascii.txt
    ! using thermochemical calorie = 4.184 J
    ! synchronized with LAMMPS (Aug 30 2012)

    if ((trim(style) == 'reduced') .or. (trim(style) == 'lj')) then
       boltz   = d_one
       hplanck = 0.18292026_dp  ! using LJ parameters for argon
       mvv2e   = d_one
       ftm2v   = d_one
       mv2d    = d_one
       nktv2p  = d_one
       qqr2e   = d_one
       qe2f    = d_one

       angstrom = d_one
       femtosecond = d_one
       qelectron = d_one

       default_dt   = 0.005_dp
       default_skin = 0.3_dp

    else if (trim(style) == 'real') then
       boltz   = 0.0019872067_dp
       hplanck = 95.306976368_dp
       mvv2e   = 48.88821291_dp**2
       ftm2v   = d_one / mvv2e
       mv2d    = d_one / 0.602214179_dp
       nktv2p  = 68568.415_dp
       qqr2e   = 332.06371_dp
       qe2f    = 23.060549_dp

       angstrom    = d_one
       femtosecond = d_one
       qelectron   = d_one

       default_dt  = d_one
       default_skin = 2.0_dp

    else if (trim(style) == 'metal') then
       boltz   = 8.617343e-5_dp
       hplanck = 4.135667403e-3_dp
       mvv2e   = 1.0364269e-4_dp
       ftm2v   = d_one / mvv2e
       mv2d    = d_one / 0.602214179_dp
       nktv2p  = 1.6021765e6_dp
       qqr2e   = 14.399645_dp
       qe2f    = d_one

       angstrom = d_one
       femtosecond = 1.0e-3_dp
       qelectron = d_one

       default_dt = 0.001_dp
       default_skin = 2.0_dp

    else if (trim(style) == 'si') then
       boltz   = 1.3806504e-23_dp
       hplanck = 6.62606896e-34_dp
       mvv2e   = d_one
       ftm2v   = d_one
       mv2d    = d_one
       nktv2p  = d_one
       qqr2e   = 8.9876e9_dp
       qe2f    = d_one

       angstrom = 1.0e-10_dp
       femtosecond = 1.0e-15_dp
       qelectron = 1.6021765e-19_dp

       default_dt = 1.0e-8_dp
       default_skin = 0.001_dp

    else if (trim(style) == 'cgs') then
       boltz   = 1.3806504e-16_dp
       hplanck = 6.62606896e-27_dp
       mvv2e   = d_one
       ftm2v   = d_one
       mv2d    = d_one
       nktv2p  = d_one
       qqr2e   = d_one
       qe2f    = d_one

       angstrom = 1.0e-8_dp
       femtosecond = 1.0e-15_dp
       qelectron = 4.8032044e-10_dp

       default_dt = 1.0e-8_dp
       default_skin = 0.1_dp

    else if ((trim(style) == 'electron') .or. (trim(style) == 'atomic')) then
       boltz   = 3.16681534e-6_dp
       hplanck = 0.1519829846_dp
       mvv2e   = 1.06657236_dp
       ftm2v   = 0.937582899_dp
       mv2d    = d_one
       nktv2p  = 2.94210108e13_dp
       qqr2e   = d_one
       qe2f    = 1.94469051e-10_dp

       angstrom = 1.88972612_dp
       femtosecond = 0.0241888428_dp
       qelectron = d_one

       default_dt = 0.001_dp
       default_skin = 2.0_dp

    else
       call mp_error('Unknown unit style',1)
    end if

    call adjust_mem(13*dp)
end subroutine set_units

end module units
