package cern.colt.matrix.tfloat.algo.solver

//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of FloatGMRES
 */
class FloatGMRESTest(arg0: String) extends FloatIterativeSolverTest(arg0) {

  protected def createSolver() {
    solver = new FloatGMRES(x)
    M = solver.getPreconditioner
  }
}
