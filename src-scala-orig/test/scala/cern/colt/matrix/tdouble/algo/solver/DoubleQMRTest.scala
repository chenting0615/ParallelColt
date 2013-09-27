package cern.colt.matrix.tdouble.algo.solver

//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of DoubleQMR
 */
class DoubleQMRTest(arg0: String) extends DoubleIterativeSolverTest(arg0) {

  protected def createSolver() {
    solver = new DoubleQMR(x)
    M = solver.getPreconditioner
  }
}
