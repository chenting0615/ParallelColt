package cern.colt.matrix.tdouble.algo.solver

import cern.colt.matrix.tdouble.algo.solver.preconditioner.DoubleSSOR
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of DoubleBiCG with SSOR
 */
class DoubleBiCGSSORTest(arg0: String) extends DoubleBiCGTest(arg0) {

  protected def createSolver() {
    super.createSolver()
    val omega = Math.random() + 1
    M = new DoubleSSOR(A.rows(), true, omega, omega)
  }
}
