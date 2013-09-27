package cern.colt.matrix.tdouble.algo.solver

import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.jet.math.tdouble.DoubleFunctions
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Chebyshev solver. Solves the symmetric positive definite linear system
 * <code>Ax = b</code> using the Preconditioned Chebyshev Method. Chebyshev
 * requires an acurate estimate on the bounds of the spectrum of the matrix.
 *
 * @author Templates
 */
class DoubleChebyshev(template: StrideMatrix1D, eigmin: Double, eigmax: Double)
    extends AbstractDoubleIterativeSolver {

  /**
   * Estimates for the eigenvalue of the matrix
   */
  private var eigmin: Double = _

  private var eigmax: Double = _

  /**
   * Vectors for use in the iterative solution process
   */
  private var p: StrideMatrix1D = template.copy()

  private var z: StrideMatrix1D = template.copy()

  private var r: StrideMatrix1D = template.copy()

  private var q: StrideMatrix1D = template.copy()

  setEigenvalues(eigmin, eigmax)

  /**
   * Sets the eigenvalue estimates.
   *
   * @param eigmin
   *            Smallest eigenvalue. Must be positive
   * @param eigmax
   *            Largest eigenvalue. Must be positive
   */
  def setEigenvalues(eigmin: Double, eigmax: Double) {
    this.eigmin = eigmin
    this.eigmax = eigmax
    if (eigmin <= 0) throw new IllegalArgumentException("eigmin <= 0")
    if (eigmax <= 0) throw new IllegalArgumentException("eigmax <= 0")
    if (eigmin > eigmax) throw new IllegalArgumentException("eigmin > eigmax")
  }

  def solve(A: StrideMatrix2D, b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = {
    checkSizes(A, b, x)
    var alpha = 0
    var beta = 0
    var c = 0
    var d = 0
    A.zMult(x, r.assign(b), -1, 1, false)
    c = (eigmax - eigmin) / 2
    d = (eigmax + eigmin) / 2
    iter.setFirst()
    while (!iter.converged(r, x)) {
      M.apply(r, z)
      if (iter.isFirst) {
        p.assign(z)
        alpha = 2.0 / d
      } else {
        beta = (alpha * c) / 2.0
        beta *= beta
        alpha = 1.0 / (d - beta)
        p.assign(z, DoubleFunctions.plusMultFirst(beta))
      }
      A.zMult(p, q)
      x.assign(p, DoubleFunctions.plusMultSecond(alpha))
      r.assign(q, DoubleFunctions.plusMultSecond(-alpha))
      iter.next()
    }
    x
  }
}
