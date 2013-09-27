package cern.colt.matrix.tfloat.algo.solver

//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of FloatQMR
 */
class FloatQMRTest(arg0: String) extends FloatIterativeSolverTest(arg0) {

  protected def createSolver() {
    solver = new FloatQMR(x)
    M = solver.getPreconditioner
  }
}
