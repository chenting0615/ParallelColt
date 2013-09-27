package cern.colt.matrix.tdouble.algo.solver

import cern.colt.matrix.tdouble.algo.solver.preconditioner.DoubleAMG
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of DoubleQMR with AMG
 */
class DoubleQMRAMGTest(arg0: String) extends DoubleQMRTest(arg0) {

  protected def createSolver() {
    super.createSolver()
    M = new DoubleAMG()
  }
}
