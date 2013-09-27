package cern.colt.matrix.tdouble.algo.solver

import cern.colt.matrix.tdouble.algo.solver.preconditioner.DoubleILU
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of DoubleGMRES with ILU
 */
class DoubleGMRESILUTest(arg0: String) extends DoubleGMRESTest(arg0) {

  protected def createSolver() {
    super.createSolver()
    M = new DoubleILU(A.rows())
  }
}
