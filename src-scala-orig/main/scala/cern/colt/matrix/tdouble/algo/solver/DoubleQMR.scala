package cern.colt.matrix.tdouble.algo.solver

import cern.colt.matrix.Norm
import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.algo.DenseDoubleAlgebra
import cern.colt.matrix.tdouble.algo.solver.preconditioner.DoublePreconditioner
import cern.jet.math.tdouble.DoubleFunctions
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Quasi-Minimal Residual method. QMR solves the unsymmetric linear system
 * <code>Ax = b</code> using the Quasi-Minimal Residual method. QMR uses two
 * preconditioners, and by default these are the same preconditioner.
 *
 * @author Templates
 */
class DoubleQMR(template: StrideMatrix1D) extends AbstractDoubleIterativeSolver {

  /**
   * Left preconditioner
   */
  private var M1: DoublePreconditioner = _

  /**
   * Right preconditioner
   */
  private var M2: DoublePreconditioner = _

  /**
   * Vectors for use in the iterative solution process
   */
  private var r: StrideMatrix1D = template.copy()

  private var y: StrideMatrix1D = template.copy()

  private var z: StrideMatrix1D = template.copy()

  private var v: StrideMatrix1D = template.copy()

  private var w: StrideMatrix1D = template.copy()

  private var p: StrideMatrix1D = template.copy()

  private var q: StrideMatrix1D = template.copy()

  private var d: StrideMatrix1D = template.copy()

  private var s: StrideMatrix1D = template.copy()

  private var v_tld: StrideMatrix1D = template.copy()

  private var w_tld: StrideMatrix1D = template.copy()

  private var y_tld: StrideMatrix1D = template.copy()

  private var z_tld: StrideMatrix1D = template.copy()

  private var p_tld: StrideMatrix1D = template.copy()

  /**
   * Constructor for QMR. Uses the given vector as template for creating
   * scratch vectors. Typically, the solution or the right hand side vector
   * can be passed, and the template is not modified. Allows setting different
   * right and left preconditioners
   *
   * @param template
   *            Vector to use as template for the work vectors needed in the
   *            solution process
   * @param M1
   *            Left preconditioner
   * @param M2
   *            Right preconditioner
   */
  def this(template: StrideMatrix1D, M1: DoublePreconditioner, M2: DoublePreconditioner) {
    this()
    this.M1 = M1
    this.M2 = M2
    r = template.copy()
    y = template.copy()
    z = template.copy()
    v = template.copy()
    w = template.copy()
    p = template.copy()
    q = template.copy()
    d = template.copy()
    s = template.copy()
    v_tld = template.copy()
    w_tld = template.copy()
    y_tld = template.copy()
    z_tld = template.copy()
    p_tld = template.copy()
  }

  def solve(A: StrideMatrix2D, b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = {
    checkSizes(A, b, x)
    var rho = 0
    var rho_1 = 0
    var xi = 0
    var gamma = 1.
    var gamma_1 = 0
    var theta = 0
    var theta_1 = 0
    var eta = -1.
    var delta = 0
    var ep = 0
    var beta = 0
    A.zMult(x, r.assign(b), -1, 1, false)
    v_tld.assign(r)
    M1.apply(v_tld, y)
    rho = DenseDoubleAlgebra.DEFAULT.norm(y, Norm.Two)
    w_tld.assign(r)
    M2.transApply(w_tld, z)
    xi = DenseDoubleAlgebra.DEFAULT.norm(z, Norm.Two)
    iter.setFirst()
    while (!iter.converged(r, x)) {
      if (rho == 0) throw new IterativeSolverDoubleNotConvergedException(DoubleNotConvergedException.Reason.Breakdown,
        "rho", iter)
      if (xi == 0) throw new IterativeSolverDoubleNotConvergedException(DoubleNotConvergedException.Reason.Breakdown,
        "xi", iter)
      v.assign(v_tld, DoubleFunctions.multSecond(1 / rho))
      y.assign(DoubleFunctions.mult(1 / rho))
      w.assign(w_tld, DoubleFunctions.multSecond(1 / xi))
      z.assign(DoubleFunctions.mult(1 / xi))
      delta = z.zDotProduct(y)
      if (delta == 0) throw new IterativeSolverDoubleNotConvergedException(DoubleNotConvergedException.Reason.Breakdown,
        "delta", iter)
      M2.apply(y, y_tld)
      M1.transApply(z, z_tld)
      if (iter.isFirst) {
        p.assign(y_tld)
        q.assign(z_tld)
      } else {
        p.assign(y_tld, DoubleFunctions.plusMultFirst(-xi * delta / ep))
        q.assign(z_tld, DoubleFunctions.plusMultFirst(-rho * delta / ep))
      }
      A.zMult(p, p_tld)
      ep = q.zDotProduct(p_tld)
      if (ep == 0) throw new IterativeSolverDoubleNotConvergedException(DoubleNotConvergedException.Reason.Breakdown,
        "ep", iter)
      beta = ep / delta
      if (beta == 0) throw new IterativeSolverDoubleNotConvergedException(DoubleNotConvergedException.Reason.Breakdown,
        "beta", iter)
      v_tld.assign(v, DoubleFunctions.multSecond(-beta)).assign(p_tld, DoubleFunctions.plus)
      M1.apply(v_tld, y)
      rho_1 = rho
      rho = DenseDoubleAlgebra.DEFAULT.norm(y, Norm.Two)
      A.zMult(q, w_tld.assign(w, DoubleFunctions.multSecond(-beta)), 1, 1, true)
      M2.transApply(w_tld, z)
      xi = DenseDoubleAlgebra.DEFAULT.norm(z, Norm.Two)
      gamma_1 = gamma
      theta_1 = theta
      theta = rho / (gamma_1 * beta)
      gamma = 1 / Math.sqrt(1 + theta * theta)
      if (gamma == 0) throw new IterativeSolverDoubleNotConvergedException(DoubleNotConvergedException.Reason.Breakdown,
        "gamma", iter)
      eta = -eta * rho_1 * gamma * gamma / (beta * gamma_1 * gamma_1)
      if (iter.isFirst) {
        d.assign(p, DoubleFunctions.multSecond(eta))
        s.assign(p_tld, DoubleFunctions.multSecond(eta))
      } else {
        val `val` = theta_1 * theta_1 * gamma * gamma
        d.assign(DoubleFunctions.mult(`val`)).assign(p, DoubleFunctions.plusMultSecond(eta))
        s.assign(DoubleFunctions.mult(`val`)).assign(p_tld, DoubleFunctions.plusMultSecond(eta))
      }
      x.assign(d, DoubleFunctions.plus)
      r.assign(s, DoubleFunctions.minus)
      iter.next()
    }
    x
  }

  def setPreconditioner(M: DoublePreconditioner) {
    super.setPreconditioner(M)
    M1 = M
    M2 = M
  }
}
