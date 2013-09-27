package cern.colt.matrix.tdouble.algo.solver

import cern.colt.matrix.tdouble.algo.solver.preconditioner.DoubleICC
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of DoubleCG with ICC
 */
class DoubleCGICCTest(arg0: String) extends DoubleCGTest(arg0) {

  protected def createSolver() {
    super.createSolver()
    M = new DoubleICC(A.rows())
  }
}
