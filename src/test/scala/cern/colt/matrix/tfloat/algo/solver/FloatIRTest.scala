package cern.colt.matrix.tfloat.algo.solver

//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of FloatIR
 */
class FloatIRTest(arg0: String) extends FloatIterativeSolverTest(arg0) {

  protected def createSolver() {
    solver = new FloatIR(x)
    M = solver.getPreconditioner
  }
}
