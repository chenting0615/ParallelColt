package cern.colt.matrix.tdouble.algo.solver

//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of DoubleGMRES
 */
class DoubleGMRESTest(arg0: String) extends DoubleIterativeSolverTest(arg0) {

  protected def createSolver() {
    solver = new DoubleGMRES(x)
    M = solver.getPreconditioner
  }
}
