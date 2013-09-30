package cern.colt.matrix

object Norm extends Enumeration {

  val One = new Norm()

  val Two = new Norm()

  val Frobenius = new Norm()

  val Infinity = new Norm()

  class Norm extends Val

  implicit def convertValue(v: Value): Norm = v.asInstanceOf[Norm]
}
