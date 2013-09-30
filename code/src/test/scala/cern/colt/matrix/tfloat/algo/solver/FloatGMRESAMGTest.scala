package cern.colt.matrix.tfloat.algo.solver

import cern.colt.matrix.tfloat.algo.solver.preconditioner.FloatAMG
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of FloatGMRES with AMG
 */
class FloatGMRESAMGTest(arg0: String) extends FloatGMRESTest(arg0) {

  protected def createSolver() {
    super.createSolver()
    M = new FloatAMG()
  }
}
