package cern.clhep

/**
 * High Energy Physics coherent system of Units. This class is a Java port of
 * the <a href="http://wwwinfo.cern.ch/asd/lhc++/clhep/manual/RefGuide/Units/SystemOfUnits_h.html"
 * >C++ version</a> found in <a
 * href="http://wwwinfo.cern.ch/asd/lhc++/clhep">CLHEP 1.4.0</a>, which in turn
 * has been provided by Geant4 (a simulation toolkit for HEP).
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
object Units {

  val millimeter = 1.0

  val millimeter2 = millimeter * millimeter

  val millimeter3 = millimeter * millimeter * millimeter

  val centimeter = 10.0 * millimeter

  val centimeter2 = centimeter * centimeter

  val centimeter3 = centimeter * centimeter * centimeter

  val meter = 1000.0 * millimeter

  val meter2 = meter * meter

  val meter3 = meter * meter * meter

  val kilometer = 1000.0 * meter

  val kilometer2 = kilometer * kilometer

  val kilometer3 = kilometer * kilometer * kilometer

  val micrometer = 1.e-6 * meter

  val nanometer = 1.e-9 * meter

  val angstrom = 1.e-10 * meter

  val fermi = 1.e-15 * meter

  val barn = 1.e-28 * meter2

  val millibarn = 1.e-3 * barn

  val microbarn = 1.e-6 * barn

  val nanobarn = 1.e-9 * barn

  val picobarn = 1.e-12 * barn

  val mm = millimeter

  val mm2 = millimeter2

  val mm3 = millimeter3

  val cm = centimeter

  val cm2 = centimeter2

  val cm3 = centimeter3

  val m = meter

  val m2 = meter2

  val m3 = meter3

  val km = kilometer

  val km2 = kilometer2

  val km3 = kilometer3

  val radian = 1.0

  val milliradian = 1.e-3 * radian

  val degree = (Math.PI / 180.0) * radian

  val steradian = 1.0

  val rad = radian

  val mrad = milliradian

  val sr = steradian

  val deg = degree

  val nanosecond = 1.0

  val second = 1.e+9 * nanosecond

  val millisecond = 1.e-3 * second

  val microsecond = 1.e-6 * second

  val picosecond = 1.e-12 * second

  val hertz = 1.0 / second

  val kilohertz = 1.e+3 * hertz

  val megahertz = 1.e+6 * hertz

  val ns = nanosecond

  val s = second

  val ms = millisecond

  val eplus = 1.0

  val e_SI = 1.60217733e-19

  val coulomb = eplus / e_SI

  val megaelectronvolt = 1.0

  val electronvolt = 1.e-6 * megaelectronvolt

  val kiloelectronvolt = 1.e-3 * megaelectronvolt

  val gigaelectronvolt = 1.e+3 * megaelectronvolt

  val teraelectronvolt = 1.e+6 * megaelectronvolt

  val petaelectronvolt = 1.e+9 * megaelectronvolt

  val joule = electronvolt / e_SI

  val MeV = megaelectronvolt

  val eV = electronvolt

  val keV = kiloelectronvolt

  val GeV = gigaelectronvolt

  val TeV = teraelectronvolt

  val PeV = petaelectronvolt

  val kilogram = joule * second * second / (meter * meter)

  val gram = 1.e-3 * kilogram

  val milligram = 1.e-3 * gram

  val kg = kilogram

  val g = gram

  val mg = milligram

  val watt = joule / second

  val newton = joule / meter

  val hep_pascal = newton / m2

  val pascal = hep_pascal

  val bar = 100000 * pascal

  val atmosphere = 101325 * pascal

  val ampere = coulomb / second

  val milliampere = 1.e-3 * ampere

  val microampere = 1.e-6 * ampere

  val nanoampere = 1.e-9 * ampere

  val megavolt = megaelectronvolt / eplus

  val kilovolt = 1.e-3 * megavolt

  val volt = 1.e-6 * megavolt

  val ohm = volt / ampere

  val farad = coulomb / volt

  val millifarad = 1.e-3 * farad

  val microfarad = 1.e-6 * farad

  val nanofarad = 1.e-9 * farad

  val picofarad = 1.e-12 * farad

  val weber = volt * second

  val tesla = volt * second / meter2

  val gauss = 1.e-4 * tesla

  val kilogauss = 1.e-1 * tesla

  val henry = weber / ampere

  val kelvin = 1.0

  val mole = 1.0

  val becquerel = 1.0 / second

  val curie = 3.7e+10 * becquerel

  val gray = joule / kilogram

  val candela = 1.0

  val lumen = candela * steradian

  val lux = lumen / meter2

  val perCent = 0.01

  val perThousand = 0.001

  val perMillion = 0.000001
}
