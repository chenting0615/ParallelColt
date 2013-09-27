package cern.clhep

/**
 * High Energy Physics coherent Physical Constants. This class is a Java port of
 * the <a href="http://wwwinfo.cern.ch/asd/lhc++/clhep/manual/RefGuide/Units/PhysicalConstants_h.html"
 * >C++ version</a> found in <a
 * href="http://wwwinfo.cern.ch/asd/lhc++/clhep">CLHEP 1.4.0</a>, which in turn
 * has been provided by Geant4 (a simulation toolkit for HEP).
 * <p>
 * For aliasing see {@link #physicalConstants}.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
object PhysicalConstants {

  /**
   * Little trick to allow for "aliasing", that is, renaming this class.
   * Normally you would write
   *
   * <pre>
   * PhysicalConstants.twopi;
   * PhysicalConstants.c_light;
   * PhysicalConstants.h_Planck;
   * </pre>
   *
   * Since this class has only static methods, but no instance methods you can
   * also shorten the name "PhysicalConstants" to a name that better suits
   * you, for example "P".
   *
   * <pre>
   * PhysicalConstants P = PhysicalConstants.physicalConstants; // kind of &quot;alias&quot;
   * P.twopi;
   * P.c_light;
   * P.h_Planck;
   * </pre>
   */
  val physicalConstants = new PhysicalConstants()

  val pi = Math.PI

  val twopi = 2 * pi

  val halfpi = pi / 2

  val pi2 = pi * pi

  val Avogadro = 6.0221367e+23 / Units.mole

  val c_light = 2.99792458e+8 * Units.m / Units.s

  val c_squared = c_light * c_light

  val h_Planck = 6.6260755e-34 * Units.joule * Units.s

  val hbar_Planck = h_Planck / twopi

  val hbarc = hbar_Planck * c_light

  val hbarc_squared = hbarc * hbarc

  val electron_charge = -Units.eplus

  val e_squared = Units.eplus * Units.eplus

  val electron_mass_c2 = 0.51099906 * Units.MeV

  val proton_mass_c2 = 938.27231 * Units.MeV

  val neutron_mass_c2 = 939.56563 * Units.MeV

  val amu_c2 = 931.49432 * Units.MeV

  val amu = amu_c2 / c_squared

  val mu0 = 4 * pi * 1.e-7 * Units.henry / Units.m

  val epsilon0 = 1. / (c_squared * mu0)

  val elm_coupling = e_squared / (4 * pi * epsilon0)

  val fine_structure_const = elm_coupling / hbarc

  val classic_electr_radius = elm_coupling / electron_mass_c2

  val electron_Compton_length = hbarc / electron_mass_c2

  val Bohr_radius = electron_Compton_length / fine_structure_const

  val alpha_rcl2 = fine_structure_const * classic_electr_radius * classic_electr_radius

  val twopi_mc2_rcl2 = twopi * electron_mass_c2 * classic_electr_radius * classic_electr_radius

  val k_Boltzmann = 8.617385e-11 * Units.MeV / Units.kelvin

  val STP_Temperature = 273.15 * Units.kelvin

  val STP_Pressure = 1. * Units.atmosphere

  val kGasThreshold = 10. * Units.mg / Units.cm3

  val universe_mean_density = 1.e-25 * Units.g / Units.cm3
}
