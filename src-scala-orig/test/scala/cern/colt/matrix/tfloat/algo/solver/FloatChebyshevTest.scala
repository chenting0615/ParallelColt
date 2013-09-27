package cern.colt.matrix.tfloat.algo.solver

import cern.colt.matrix.tfloat.algo.DenseFloatAlgebra
import cern.colt.matrix.tfloat.algo.decomposition.DenseFloatEigenvalueDecomposition
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Test of FloatChebyshev
 */
class FloatChebyshevTest(arg0: String) extends FloatIterativeSolverTest(arg0) {

  protected def createSolver() {
    val evd = DenseFloatAlgebra.DEFAULT.eig(A)
    val eigs = evd.getRealEigenvalues.elements().asInstanceOf[Array[Float]]
    var eigmin = 1
    var eigmax = 1
    if (eigs.length > 0) {
      eigmin = eigs(0)
      eigmax = eigs(eigs.length - 1)
    }
    solver = new FloatChebyshev(x, eigmin, eigmax)
    M = solver.getPreconditioner
  }
}
