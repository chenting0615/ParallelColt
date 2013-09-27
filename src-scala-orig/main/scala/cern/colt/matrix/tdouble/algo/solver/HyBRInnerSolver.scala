package cern.colt.matrix.tdouble.algo.solver

//remove if not needed
import scala.collection.JavaConversions._

object HyBRInnerSolver extends Enumeration {

  val TIKHONOV = new HyBRInnerSolver()

  val NONE = new HyBRInnerSolver()

  class HyBRInnerSolver extends Val

  implicit def convertValue(v: Value): HyBRInnerSolver = v.asInstanceOf[HyBRInnerSolver]
}
