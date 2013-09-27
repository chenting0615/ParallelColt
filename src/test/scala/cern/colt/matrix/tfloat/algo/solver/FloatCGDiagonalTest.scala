package cern.colt.matrix.tfloat.algo.solver

import cern.colt.matrix.tfloat.algo.solver.preconditioner.FloatDiagonal
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of FloatCG with diagonal preconditioner
 */
class FloatCGDiagonalTest(arg0: String) extends FloatCGTest(arg0) {

  protected def createSolver() {
    super.createSolver()
    M = new FloatDiagonal(A.rows())
  }
}
