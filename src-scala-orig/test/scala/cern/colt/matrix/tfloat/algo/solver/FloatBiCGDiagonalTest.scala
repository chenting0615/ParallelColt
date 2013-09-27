package cern.colt.matrix.tfloat.algo.solver

import cern.colt.matrix.tfloat.algo.solver.preconditioner.FloatDiagonal
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of FloatBiCG with diagonal preconditioner
 */
class FloatBiCGDiagonalTest(arg0: String) extends FloatBiCGTest(arg0) {

  protected def createSolver() {
    super.createSolver()
    M = new FloatDiagonal(A.rows())
  }
}
