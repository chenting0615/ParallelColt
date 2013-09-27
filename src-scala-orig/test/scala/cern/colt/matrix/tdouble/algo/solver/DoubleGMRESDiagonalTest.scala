package cern.colt.matrix.tdouble.algo.solver

import cern.colt.matrix.tdouble.algo.solver.preconditioner.DoubleDiagonal
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of DoubleGMRES with diagonal preconditioner
 */
class DoubleGMRESDiagonalTest(arg0: String) extends DoubleGMRESTest(arg0) {

  protected def createSolver() {
    super.createSolver()
    M = new DoubleDiagonal(A.rows())
  }
}
