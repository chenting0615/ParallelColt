package cern.colt.matrix.tfloat.algo.solver

import cern.colt.matrix.tfloat.algo.solver.preconditioner.FloatICC
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of FloatBiCG with ICC
 */
class FloatBiCGICCTest(arg0: String) extends FloatBiCGTest(arg0) {

  protected def createSolver() {
    super.createSolver()
    M = new FloatICC(A.rows())
  }
}
