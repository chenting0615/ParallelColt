package cern.colt.matrix.tdouble.algo.solver

//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of DoubleBiCGstab
 */
class DoubleBiCGstabTest(arg0: String) extends DoubleIterativeSolverTest(arg0) {

  protected def createSolver() {
    solver = new DoubleBiCGstab(x)
    M = solver.getPreconditioner
  }
}
