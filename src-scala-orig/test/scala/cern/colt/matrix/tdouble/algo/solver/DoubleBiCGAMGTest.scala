package cern.colt.matrix.tdouble.algo.solver

import cern.colt.matrix.tdouble.algo.solver.preconditioner.DoubleAMG
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of DoubleBiCG wit AMG
 *
 */
class DoubleBiCGAMGTest(arg0: String) extends DoubleBiCGTest(arg0) {

  protected def createSolver() {
    super.createSolver()
    M = new DoubleAMG()
  }
}
