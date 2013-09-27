package cern.colt.matrix.tdouble.algo.solver

import cern.colt.matrix.tdouble.algo.solver.preconditioner.DoubleDiagonal
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of DoubleCG with diagonal preconditioner
 */
class DoubleCGDiagonalTest(arg0: String) extends DoubleCGTest(arg0) {

  protected def createSolver() {
    super.createSolver()
    M = new DoubleDiagonal(A.rows())
  }
}
