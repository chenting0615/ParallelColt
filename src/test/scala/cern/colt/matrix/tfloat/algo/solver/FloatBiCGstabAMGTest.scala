package cern.colt.matrix.tfloat.algo.solver

import cern.colt.matrix.tfloat.algo.solver.preconditioner.FloatAMG
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of FloatBiCGstab with AMG
 */
class FloatBiCGstabAMGTest(arg0: String) extends FloatBiCGstabTest(arg0) {

  protected def createSolver() {
    super.createSolver()
    M = new FloatAMG()
  }
}
