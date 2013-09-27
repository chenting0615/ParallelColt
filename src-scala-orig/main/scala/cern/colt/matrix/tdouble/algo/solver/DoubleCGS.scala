package cern.colt.matrix.tdouble.algo.solver

import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.jet.math.tdouble.DoubleFunctions
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Conjugate Gradients squared solver. CGS solves the unsymmetric linear system
 * <code>Ax = b</code> using the Conjugate Gradient Squared method
 *
 * @author Templates
 */
class DoubleCGS(template: StrideMatrix1D) extends AbstractDoubleIterativeSolver {

  /**
   * Vectors for use in the iterative solution process
   */
  private var p: StrideMatrix1D = template.copy()

  private var q: StrideMatrix1D = template.copy()

  private var u: StrideMatrix1D = template.copy()

  private var phat: StrideMatrix1D = template.copy()

  private var qhat: StrideMatrix1D = template.copy()

  private var vhat: StrideMatrix1D = template.copy()

  private var uhat: StrideMatrix1D = template.copy()

  private var sum: StrideMatrix1D = template.copy()

  private var r: StrideMatrix1D = template.copy()

  private var rtilde: StrideMatrix1D = template.copy()

  def solve(A: StrideMatrix2D, b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = {
    checkSizes(A, b, x)
    var rho_1 = 0
    var rho_2 = 0
    var alpha = 0
    var beta = 0
    A.zMult(x, r.assign(b), -1, 1, false)
    rtilde.assign(r)
    iter.setFirst()
    while (!iter.converged(r, x)) {
      rho_1 = rtilde.zDotProduct(r)
      if (rho_1 == 0) throw new IterativeSolverDoubleNotConvergedException(DoubleNotConvergedException.Reason.Breakdown,
        "rho", iter)
      if (iter.isFirst) {
        u.assign(r)
        p.assign(u)
      } else {
        beta = rho_1 / rho_2
        u.assign(r).assign(q, DoubleFunctions.plusMultSecond(beta))
        sum.assign(q).assign(p, DoubleFunctions.plusMultSecond(beta))
        p.assign(u).assign(sum, DoubleFunctions.plusMultSecond(beta))
      }
      M.apply(p, phat)
      A.zMult(phat, vhat)
      alpha = rho_1 / rtilde.zDotProduct(vhat)
      q.assign(vhat, DoubleFunctions.multSecond(-alpha)).assign(u, DoubleFunctions.plus)
      M.apply(sum.assign(u).assign(q, DoubleFunctions.plus), uhat)
      x.assign(uhat, DoubleFunctions.plusMultSecond(alpha))
      A.zMult(uhat, qhat)
      r.assign(qhat, DoubleFunctions.plusMultSecond(-alpha))
      rho_2 = rho_1
      iter.next()
    }
    x
  }
}
