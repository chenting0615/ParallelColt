package cern.colt.matrix.tdouble.algo.solver

import cern.colt.matrix.tdouble.algo.solver.preconditioner.DoubleAMG
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of DoubleCG with AMG
 */
class DoubleCGAMGTest(arg0: String) extends DoubleCGTest(arg0) {

  protected def createSolver() {
    super.createSolver()
    M = new DoubleAMG()
  }
}
