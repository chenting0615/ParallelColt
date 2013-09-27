package cern.colt.matrix.tdouble.algo.solver

import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.jet.math.tdouble.DoubleFunctions
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Conjugate Gradients solver. CG solves the symmetric positive definite linear
 * system <code>Ax=b</code> using the Conjugate Gradient method.
 *
 * @author Templates
 */
class DoubleCG(template: StrideMatrix1D) extends AbstractDoubleIterativeSolver {

  /**
   * Vectors for use in the iterative solution process
   */
  private var p: StrideMatrix1D = template.copy()

  private var z: StrideMatrix1D = template.copy()

  private var q: StrideMatrix1D = template.copy()

  private var r: StrideMatrix1D = template.copy()

  def solve(A: StrideMatrix2D, b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = {
    checkSizes(A, b, x)
    var alpha = 0
    var beta = 0
    var rho = 0
    var rho_1 = 0
    A.zMult(x, r.assign(b), -1, 1, false)
    iter.setFirst()
    while (!iter.converged(r, x)) {
      M.apply(r, z)
      rho = r.zDotProduct(z)
      if (iter.isFirst) p.assign(z) else {
        beta = rho / rho_1
        p.assign(z, DoubleFunctions.plusMultFirst(beta))
      }
      A.zMult(p, q)
      alpha = rho / p.zDotProduct(q)
      x.assign(p, DoubleFunctions.plusMultSecond(alpha))
      r.assign(q, DoubleFunctions.plusMultSecond(-alpha))
      rho_1 = rho
      iter.next()
    }
    x
  }
}
