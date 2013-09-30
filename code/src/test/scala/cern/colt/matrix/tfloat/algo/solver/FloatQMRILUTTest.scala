package cern.colt.matrix.tfloat.algo.solver

import cern.colt.matrix.tfloat.algo.solver.preconditioner.FloatILUT
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of FloatQMR with ILUT
 */
class FloatQMRILUTTest(arg0: String) extends FloatQMRTest(arg0) {

  protected def createSolver() {
    super.createSolver()
    M = new FloatILUT(A.rows())
  }
}