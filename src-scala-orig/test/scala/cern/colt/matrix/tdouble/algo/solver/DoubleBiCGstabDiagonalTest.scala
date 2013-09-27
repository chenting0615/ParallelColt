package cern.colt.matrix.tdouble.algo.solver

import cern.colt.matrix.tdouble.algo.solver.preconditioner.DoubleDiagonal
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of DoubleBiCGstab with diagonal preconditioner
 */
class DoubleBiCGstabDiagonalTest(arg0: String) extends DoubleBiCGstabTest(arg0) {

  protected def createSolver() {
    super.createSolver()
    M = new DoubleDiagonal(A.rows())
  }
}
