package cern.colt.matrix.tdouble.algo.solver

import cern.colt.matrix.tdouble.algo.solver.preconditioner.DoubleDiagonal
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of DoubleChebyshev with diagonal preconditioner
 */
class DoubleChebyshevDiagonalTest(arg0: String) extends DoubleChebyshevTest(arg0) {

  protected def createSolver() {
    super.createSolver()
    M = new DoubleDiagonal(A.rows())
  }
}
