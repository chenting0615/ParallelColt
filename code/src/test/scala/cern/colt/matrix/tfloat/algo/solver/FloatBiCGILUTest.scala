package cern.colt.matrix.tfloat.algo.solver

import cern.colt.matrix.tfloat.algo.solver.preconditioner.FloatILU
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of FloatBiCG with ILU
 */
class FloatBiCGILUTest(arg0: String) extends FloatBiCGTest(arg0) {

  protected def createSolver() {
    super.createSolver()
    M = new FloatILU(A.rows())
  }
}
