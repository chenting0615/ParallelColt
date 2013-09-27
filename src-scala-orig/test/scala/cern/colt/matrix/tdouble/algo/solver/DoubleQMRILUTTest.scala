package cern.colt.matrix.tdouble.algo.solver

import cern.colt.matrix.tdouble.algo.solver.preconditioner.DoubleILUT
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of DoubleQMR with ILUT
 */
class DoubleQMRILUTTest(arg0: String) extends DoubleQMRTest(arg0) {

  protected def createSolver() {
    super.createSolver()
    M = new DoubleILUT(A.rows())
  }
}
