package cern.colt.matrix.tdouble.algo.solver

import cern.colt.matrix.tdouble.algo.solver.preconditioner.DoubleILUT
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of DoubleIR with ILUT
 */
class DoubleIRILUTTest(arg0: String) extends DoubleIRTest(arg0) {

  protected def createSolver() {
    super.createSolver()
    M = new DoubleILUT(A.rows())
  }
}
