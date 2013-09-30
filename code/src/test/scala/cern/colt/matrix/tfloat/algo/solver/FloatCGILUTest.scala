package cern.colt.matrix.tfloat.algo.solver

import cern.colt.matrix.tfloat.algo.solver.preconditioner.FloatILU
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of FloatCG with ILU
 */
class FloatCGILUTest(arg0: String) extends FloatCGTest(arg0) {

  protected def createSolver() {
    super.createSolver()
    M = new FloatILU(A.rows())
  }
}
