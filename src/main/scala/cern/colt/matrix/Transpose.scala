package cern.colt.matrix

object Transpose extends Enumeration {

  val NoTranspose = new Transpose()

  val Transpose = new Transpose()

  class Transpose extends Val {

    /**
     * @return the netlib character version of this designation, for use with F2J.
     */
    def netlib(): String = {
      if (this eq NoTranspose) return "N"
      "T"
    }
  }

  implicit def convertValue(v: Value): Transpose = v.asInstanceOf[Transpose]
}
