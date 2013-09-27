package cern.colt.matrix.tdouble.algo.solver

import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.jet.math.tdouble.DoubleFunctions
//remove if not needed
import scala.collection.JavaConversions._

/**
 * BiCG solver. BiCG solves the unsymmetric linear system <code>Ax = b</code>
 * using the Preconditioned BiConjugate Gradient method.
 *
 * @author Templates
 */
class DoubleBiCG(template: StrideMatrix1D) extends AbstractDoubleIterativeSolver {

  /**
   * Vectors for use in the iterative solution process
   */
  private var z: StrideMatrix1D = template.copy()

  private var p: StrideMatrix1D = template.copy()

  private var q: StrideMatrix1D = template.copy()

  private var r: StrideMatrix1D = template.copy()

  private var ztilde: StrideMatrix1D = template.copy()

  private var ptilde: StrideMatrix1D = template.copy()

  private var qtilde: StrideMatrix1D = template.copy()

  private var rtilde: StrideMatrix1D = template.copy()

  def solve(A: StrideMatrix2D, b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = {
    checkSizes(A, b, x)
    var rho_1 = 1
    var rho_2 = 1
    var alpha = 1
    var beta = 1
    A.zMult(x, r.assign(b), -1, 1, false)
    rtilde.assign(r)
    iter.setFirst()
    while (!iter.converged(r, x)) {
      M.apply(r, z)
      M.transApply(rtilde, ztilde)
      rho_1 = z.zDotProduct(rtilde)
      if (rho_1 == 0.) throw new IterativeSolverDoubleNotConvergedException(DoubleNotConvergedException.Reason.Breakdown,
        "rho", iter)
      if (iter.isFirst) {
        p.assign(z)
        ptilde.assign(ztilde)
      } else {
        beta = rho_1 / rho_2
        p.assign(z, DoubleFunctions.plusMultFirst(beta))
        ptilde.assign(ztilde, DoubleFunctions.plusMultFirst(beta))
      }
      A.zMult(p, q)
      A.zMult(ptilde, qtilde, 1, 0, true)
      alpha = rho_1 / ptilde.zDotProduct(q)
      x.assign(p, DoubleFunctions.plusMultSecond(alpha))
      r.assign(q, DoubleFunctions.plusMultSecond(-alpha))
      rtilde.assign(qtilde, DoubleFunctions.plusMultSecond(-alpha))
      rho_2 = rho_1
      iter.next()
    }
    x
  }
}
