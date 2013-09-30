package cern.colt.matrix.tfloat.algo.solver

import cern.colt.matrix.tfloat.algo.solver.preconditioner.FloatICC
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of FloatIR with ICC
 */
class FloatIRICCTest(arg0: String) extends FloatIRTest(arg0) {

  protected def createSolver() {
    super.createSolver()
    M = new FloatICC(A.rows())
  }
}
