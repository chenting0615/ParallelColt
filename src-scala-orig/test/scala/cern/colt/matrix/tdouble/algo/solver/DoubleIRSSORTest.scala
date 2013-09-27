package cern.colt.matrix.tdouble.algo.solver

import cern.colt.matrix.tdouble.algo.solver.preconditioner.DoubleSSOR
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of DoubleIR with SSOR
 */
class DoubleIRSSORTest(arg0: String) extends DoubleIRTest(arg0) {

  protected def createSolver() {
    super.createSolver()
    val omega = Math.random() + 1
    M = new DoubleSSOR(A.rows(), true, omega, omega)
  }
}
