package cern.colt.matrix.tdouble.algo.solver

import cern.colt.matrix.Norm
import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.algo.DenseDoubleAlgebra
import cern.colt.matrix.tdouble.impl.DenseMatrix1D
import cern.colt.matrix.tdouble.impl.DenseMatrix2D
import cern.jet.math.tdouble.DoubleFunctions
//remove if not needed
import scala.collection.JavaConversions._

/**
 * GMRES solver. GMRES solves the unsymmetric linear system <code>Ax = b</code>
 * using the Generalized Minimum Residual method. The GMRES iteration is
 * restarted after a given number of iterations. By default it is restarted
 * after 30 iterations.
 *
 * @author Templates
 */
class DoubleGMRES(template: StrideMatrix1D, restart: Int) extends AbstractDoubleIterativeSolver {

  /**
   * After this many iterations, the GMRES will be restarted.
   */
  private var restart: Int = _

  /**
   * Vectors for use in the iterative solution process
   */
  private var w: StrideMatrix1D = template.copy()

  private var u: StrideMatrix1D = template.copy()

  private var r: StrideMatrix1D = template.copy()

  /**
   * Vectors spanning the subspace
   */
  private var v: Array[StrideMatrix1D] = _

  /**
   * Restart vector
   */
  private var s: StrideMatrix1D = _

  /**
   * Hessenberg matrix
   */
  private var H: StrideMatrix2D = _

  /**
   * Givens rotations for the QR factorization
   */
  private var rotation: Array[DoubleGivensRotation] = _

  setRestart(restart)

  /**
   * Constructor for GMRES. Uses the given vector as template for creating
   * scratch vectors. Typically, the solution or the right hand side vector
   * can be passed, and the template is not modified. The iteration is
   * restarted every 30 iterations
   *
   * @param template
   *            Vector to use as template for the work vectors needed in the
   *            solution process
   */
  def this(template: StrideMatrix1D) {
    this(template, 30)
  }

  /**
   * Sets the restart parameter
   *
   * @param restart
   *            GMRES iteration is restarted after this number of iterations
   */
  def setRestart(restart: Int) {
    this.restart = restart
    if (restart <= 0) throw new IllegalArgumentException("restart must be a positive integer")
    s = new DenseMatrix1D(restart + 1)
    H = new DenseMatrix2D(restart + 1, restart)
    rotation = Array.ofDim[DoubleGivensRotation](restart + 1)
    v = Array.ofDim[StrideMatrix1D](restart + 1)
    for (i <- 0 until v.length) v(i) = new DenseMatrix1D(r.size.toInt)
  }

  def solve(A: StrideMatrix2D, b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = {
    checkSizes(A, b, x)
    A.zMult(x, u.assign(b), -1, 1, false)
    M.apply(u, r)
    var normr = DenseDoubleAlgebra.DEFAULT.norm(r, Norm.Two)
    M.apply(b, u)
    iter.setFirst()
    while (!iter.converged(r, x)) {
      v(0).assign(r, DoubleFunctions.multSecond(1 / normr))
      s.assign(0).setQuick(0, normr)
      var i = 0
      while (i < restart && !iter.converged(Math.abs(s.getQuick(i)))) {
        A.zMult(v(i), u)
        M.apply(u, w)
        var k = 0
        while (k <= i) {
          H.setQuick(k, i, w.zDotProduct(v(k)))
          w.assign(v(k), DoubleFunctions.plusMultSecond(-H.getQuick(k, i)))
          k += 1
        }
        H.setQuick(i + 1, i, DenseDoubleAlgebra.DEFAULT.norm(w, Norm.Two))
        v(i + 1).assign(w, DoubleFunctions.multSecond(1. / H.getQuick(i + 1, i)))
        for (k <- 0 until i) rotation(k).apply(H, i, k, k + 1)
        rotation(i) = new DoubleGivensRotation(H.getQuick(i, i), H.getQuick(i + 1, i))
        rotation(i).apply(H, i, i, i + 1)
        rotation(i).apply(s, i, i + 1)
        i += 1
        iter.next()
      }
      s = DenseDoubleAlgebra.DEFAULT.backwardSolve(H.viewPart(0, 0, i, i), s)
      for (j <- 0 until i) x.assign(v(j), DoubleFunctions.plusMultSecond(s.getQuick(j)))
      A.zMult(x, u.assign(b), -1, 1, false)
      M.apply(u, r)
      normr = DenseDoubleAlgebra.DEFAULT.norm(r, Norm.Two)
      iter.next()
    }
    x
  }
}
