package cern.colt.matrix.tdouble.algo.solver

import cern.colt.matrix.tdouble.algo.solver.preconditioner.DoubleILU
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of DoubleIR with ILU
 */
class DoubleIRILUTest(arg0: String) extends DoubleIRTest(arg0) {

  protected def createSolver() {
    super.createSolver()
    M = new DoubleILU(A.rows())
  }
}
