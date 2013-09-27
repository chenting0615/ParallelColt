package cern.colt.matrix.tfloat.algo.solver

import cern.colt.matrix.tfloat.algo.solver.preconditioner.FloatILUT
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of FloatBiCG with ILUT
 */
class FloatBiCGILUTTest(arg0: String) extends FloatBiCGTest(arg0) {

  protected def createSolver() {
    super.createSolver()
    M = new FloatILUT(A.rows())
  }
}
