package cern.colt.matrix.tdouble.algo.solver

//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of DoubleCGS
 */
class DoubleCGSTest(arg0: String) extends DoubleIterativeSolverTest(arg0) {

  protected def createSolver() {
    solver = new DoubleCGS(x)
    M = solver.getPreconditioner
  }
}
