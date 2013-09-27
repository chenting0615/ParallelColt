package cern.colt.matrix.tdouble.algo.solver

//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of DoubleBiCG
 */
class DoubleBiCGTest(arg0: String) extends DoubleIterativeSolverTest(arg0) {

  protected def createSolver() {
    solver = new DoubleBiCG(x)
    M = solver.getPreconditioner
  }
}
