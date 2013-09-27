package cern.colt.matrix.tdouble.algo.solver

import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.jet.math.tdouble.DoubleFunctions
//remove if not needed
import scala.collection.JavaConversions._

/**
 * BiCG stablized solver. BiCGstab solves the unsymmetric linear system
 * <code>Ax = b</code> using the Preconditioned BiConjugate Gradient Stabilized
 * method
 *
 * @author Templates
 */
class DoubleBiCGstab(template: StrideMatrix1D) extends AbstractDoubleIterativeSolver {

  /**
   * Vectors for use in the iterative solution process
   */
  private var p: StrideMatrix1D = template.copy()

  private var s: StrideMatrix1D = template.copy()

  private var phat: StrideMatrix1D = template.copy()

  private var shat: StrideMatrix1D = template.copy()

  private var t: StrideMatrix1D = template.copy()

  private var v: StrideMatrix1D = template.copy()

  private var temp: StrideMatrix1D = template.copy()

  private var r: StrideMatrix1D = template.copy()

  private var rtilde: StrideMatrix1D = template.copy()

  def solve(A: StrideMatrix2D, b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = {
    checkSizes(A, b, x)
    var rho_1 = 1
    var rho_2 = 1
    var alpha = 1
    var beta = 1
    var omega = 1
    A.zMult(x, r.assign(b), -1, 1, false)
    rtilde.assign(r)
    iter.setFirst()
    while (!iter.converged(r, x)) {
      rho_1 = rtilde.zDotProduct(r)
      if (rho_1 == 0) throw new IterativeSolverDoubleNotConvergedException(DoubleNotConvergedException.Reason.Breakdown,
        "rho", iter)
      if (omega == 0) throw new IterativeSolverDoubleNotConvergedException(DoubleNotConvergedException.Reason.Breakdown,
        "omega", iter)
      if (iter.isFirst) p.assign(r) else {
        beta = (rho_1 / rho_2) * (alpha / omega)
        temp.assign(v, DoubleFunctions.multSecond(-omega)).assign(p, DoubleFunctions.plus)
        p.assign(r).assign(temp, DoubleFunctions.plusMultSecond(beta))
      }
      M.apply(p, phat)
      A.zMult(phat, v)
      alpha = rho_1 / rtilde.zDotProduct(v)
      s.assign(r).assign(v, DoubleFunctions.plusMultSecond(-alpha))
      if (iter.converged(s, x)) return x.assign(phat, DoubleFunctions.plusMultSecond(alpha))

      M.apply(s, shat)
      A.zMult(shat, t)
      omega = t.zDotProduct(s) / t.zDotProduct(t)
      x.assign(phat, DoubleFunctions.plusMultSecond(alpha))
      x.assign(shat, DoubleFunctions.plusMultSecond(omega))
      r.assign(s).assign(t, DoubleFunctions.plusMultSecond(-omega))
      rho_2 = rho_1
      iter.next()
    }
    x
  }
}
