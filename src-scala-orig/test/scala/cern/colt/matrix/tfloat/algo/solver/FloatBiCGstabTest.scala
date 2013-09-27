package cern.colt.matrix.tfloat.algo.solver

//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of FloatBiCGstab
 */
class FloatBiCGstabTest(arg0: String) extends FloatIterativeSolverTest(arg0) {

  protected def createSolver() {
    solver = new FloatBiCGstab(x)
    M = solver.getPreconditioner
  }
}
