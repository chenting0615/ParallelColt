package cern.colt.matrix.tfloat.algo.solver

import cern.colt.matrix.tfloat.algo.solver.preconditioner.FloatSSOR
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of FloatCG with SSOR
 */
class FloatCGSSORTest(arg0: String) extends FloatCGTest(arg0) {

  protected def createSolver() {
    super.createSolver()
    val omega = Math.random().toFloat + 1
    M = new FloatSSOR(A.rows(), true, omega, omega)
  }
}
