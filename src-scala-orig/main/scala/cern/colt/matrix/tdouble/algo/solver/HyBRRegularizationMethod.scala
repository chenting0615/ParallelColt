package cern.colt.matrix.tdouble.algo.solver

//remove if not needed
import scala.collection.JavaConversions._

object HyBRRegularizationMethod extends Enumeration {

  val GCV = new HyBRRegularizationMethod()

  val WGCV = new HyBRRegularizationMethod()

  val ADAPTWGCV = new HyBRRegularizationMethod()

  val NONE = new HyBRRegularizationMethod()

  class HyBRRegularizationMethod extends Val

  implicit def convertValue(v: Value): HyBRRegularizationMethod = {
    v.asInstanceOf[HyBRRegularizationMethod]
  }
}
