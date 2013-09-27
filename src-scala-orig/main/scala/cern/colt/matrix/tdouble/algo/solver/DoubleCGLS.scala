package cern.colt.matrix.tdouble.algo.solver

import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.algo.DenseDoubleAlgebra
import cern.colt.matrix.tdouble.algo.solver.preconditioner.DoubleIdentity
import cern.jet.math.tdouble.DoubleFunctions
import DoubleCGLS._
//remove if not needed
import scala.collection.JavaConversions._

object DoubleCGLS {

  private val alg = DenseDoubleAlgebra.DEFAULT

  val sqrteps = Math.sqrt(Math.pow(2, -52))
}

/**
 * CGLS is Conjugate Gradient for Least Squares method used for solving
 * large-scale, ill-posed inverse problems of the form: b = A*x + noise.
 *
 * <p>
 * Reference:<br>
 * <p>
 * A. Bjorck, "Numerical Methods for Least Squares Problems" SIAM, 1996, pg.
 * 289.
 * </p>
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 *
 */
class DoubleCGLS extends AbstractDoubleIterativeSolver {

  iter = new CGLSDoubleIterationMonitor()

  iter.asInstanceOf[CGLSDoubleIterationMonitor].setRelativeTolerance(-1)

  def solve(A: StrideMatrix2D, b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = {
    var p: StrideMatrix1D = null
    var q: StrideMatrix1D = null
    var r: StrideMatrix1D = null
    var s: StrideMatrix1D = null
    var alpha: Double = 0.0
    var beta: Double = 0.0
    var gamma: Double = 0.0
    var oldgamma = 0
    var rnrm: Double = 0.0
    var nq: Double = 0.0
    if (iter.asInstanceOf[CGLSDoubleIterationMonitor].getRelativeTolerance ==
      -1.0) {
      iter.asInstanceOf[CGLSDoubleIterationMonitor].setRelativeTolerance(sqrteps * alg.norm2(A.zMult(b,
        null, 1, 0, true)))
    }
    s = A.zMult(x, null)
    s.assign(b, DoubleFunctions.plusMultFirst(-1))
    r = A.zMult(s, null, 1, 0, true)
    rnrm = alg.norm2(r)
    if (!(M.isInstanceOf[DoubleIdentity])) {
      r = M.transApply(r, null)
      gamma = alg.norm2(r)
      gamma *= gamma
    } else {
      gamma = rnrm
      gamma *= gamma
    }
    p = r.copy()
    iter.setFirst()
    while (!iter.converged(rnrm, x)) {
      if (!iter.isFirst) {
        beta = gamma / oldgamma
        p.assign(r, DoubleFunctions.plusMultFirst(beta))
      }
      if (!(M.isInstanceOf[DoubleIdentity])) {
        r = M.apply(p, null)
        q = A.zMult(r, null)
      } else {
        q = A.zMult(p, null)
      }
      nq = alg.norm2(q)
      nq = nq * nq
      alpha = gamma / nq
      if (!(M.isInstanceOf[DoubleIdentity])) {
        x.assign(r, DoubleFunctions.plusMultSecond(alpha))
      } else {
        x.assign(p, DoubleFunctions.plusMultSecond(alpha))
      }
      s.assign(q, DoubleFunctions.plusMultSecond(-alpha))
      r = A.zMult(s, null, 1, 0, true)
      rnrm = alg.norm2(r)
      if (!(M.isInstanceOf[DoubleIdentity])) {
        r = M.transApply(r, null)
        oldgamma = gamma
        gamma = alg.norm2(r)
        gamma *= gamma
      } else {
        oldgamma = gamma
        gamma = rnrm
        gamma *= gamma
      }
      iter.next()
    }
    x
  }
}
