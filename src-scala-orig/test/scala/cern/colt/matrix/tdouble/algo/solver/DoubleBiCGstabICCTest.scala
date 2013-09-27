package cern.colt.matrix.tdouble.algo.solver

import cern.colt.matrix.tdouble.algo.solver.preconditioner.DoubleICC
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of DoubleBiCGstab with ICC
 */
class DoubleBiCGstabICCTest(arg0: String) extends DoubleBiCGstabTest(arg0) {

  protected def createSolver() {
    super.createSolver()
    M = new DoubleICC(A.rows())
  }
}
