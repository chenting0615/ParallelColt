package cern.colt.matrix.tfloat.algo.solver

import cern.colt.matrix.tfloat.algo.solver.preconditioner.FloatILUT
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of FloatIR with ILUT
 */
class FloatIRILUTTest(arg0: String) extends FloatIRTest(arg0) {

  protected def createSolver() {
    super.createSolver()
    M = new FloatILUT(A.rows())
  }
}
