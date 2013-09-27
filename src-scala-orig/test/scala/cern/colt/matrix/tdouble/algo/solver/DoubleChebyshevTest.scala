package cern.colt.matrix.tdouble.algo.solver

import cern.colt.matrix.tdouble.algo.DenseDoubleAlgebra
import cern.colt.matrix.tdouble.algo.decomposition.DenseDoubleEigenvalueDecomposition
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of DoubleChebyshev
 */
class DoubleChebyshevTest(arg0: String) extends DoubleIterativeSolverTest(arg0) {

  protected def createSolver() {
    val evd = DenseDoubleAlgebra.DEFAULT.eig(A)
    val eigs = evd.getRealEigenvalues.elements().asInstanceOf[Array[Double]]
    var eigmin = 1
    var eigmax = 1
    if (eigs.length > 0) {
      eigmin = eigs(0)
      eigmax = eigs(eigs.length - 1)
    }
    solver = new DoubleChebyshev(x, eigmin, eigmax)
    M = solver.getPreconditioner
  }
}
