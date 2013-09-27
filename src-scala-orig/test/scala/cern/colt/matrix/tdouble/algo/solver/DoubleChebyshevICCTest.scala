package cern.colt.matrix.tdouble.algo.solver

import cern.colt.matrix.tdouble.algo.solver.preconditioner.DoubleICC
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of DoubleChebyshev with ICC
 */
class DoubleChebyshevICCTest(arg0: String) extends DoubleChebyshevTest(arg0) {

  protected def createSolver() {
    super.createSolver()
    M = new DoubleICC(A.rows())
  }
}
