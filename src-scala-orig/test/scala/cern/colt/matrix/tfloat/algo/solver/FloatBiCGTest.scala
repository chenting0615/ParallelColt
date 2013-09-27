package cern.colt.matrix.tfloat.algo.solver

//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of FloatBiCG
 */
class FloatBiCGTest(arg0: String) extends FloatIterativeSolverTest(arg0) {

  protected def createSolver() {
    solver = new FloatBiCG(x)
    M = solver.getPreconditioner
  }
}
