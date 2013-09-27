package cern.colt.matrix.tdouble.algo.solver

import cern.colt.list.tint.IntArrayList
import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.algo.DenseDoubleAlgebra
import cern.colt.matrix.tdouble.algo.solver.preconditioner.DoubleIdentity
import cern.jet.math.tdouble.DoubleFunctions
import DoubleMRNSD._
//remove if not needed
import scala.collection.JavaConversions._

object DoubleMRNSD {

  private val alg = DenseDoubleAlgebra.DEFAULT

  val sqrteps = Math.sqrt(Math.pow(2, -52))
}

/**
 * MRNSD is Modified Residual Norm Steepest Descent method used for solving
 * large-scale, ill-posed inverse problems of the form: b = A*x + noise. This
 * algorithm is nonnegatively constrained.
 *
 * <p>
 * References:<br>
 * <p>
 * [1] J. Nagy, Z. Strakos,
 * "Enforcing nonnegativity in image reconstruction algorithms" in Mathematical
 * Modeling, Estimation, and Imaging, David C. Wilson, et.al., Eds., 4121
 * (2000), pg. 182--190.
 * </p>
 * <p>
 * [2] L. Kaufman, "Maximum likelihood, least squares and penalized least
 * squares for PET", IEEE Trans. Med. Imag. 12 (1993) pp. 200--214.
 * </p>
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 *
 */
class DoubleMRNSD extends AbstractDoubleIterativeSolver {

  iter = new MRNSDDoubleIterationMonitor()

  iter.asInstanceOf[MRNSDDoubleIterationMonitor].setRelativeTolerance(-1)

  def solve(A: StrideMatrix2D, b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = {
    if (!(iter.isInstanceOf[MRNSDDoubleIterationMonitor])) {
      iter = new MRNSDDoubleIterationMonitor()
      iter.asInstanceOf[MRNSDDoubleIterationMonitor].setRelativeTolerance(-1)
    }
    var alpha: Double = 0.0
    var gamma: Double = 0.0
    var theta: Double = 0.0
    var rnrm: Double = 0.0
    var r: StrideMatrix1D = null
    var s: StrideMatrix1D = null
    var u: StrideMatrix1D = null
    var w: StrideMatrix1D = null
    var indexList: IntArrayList = null
    val tau = sqrteps
    val sigsq = tau
    val minAndLoc = x.getMinLocation
    val minX = minAndLoc(0)
    if (minX < 0) {
      x.assign(DoubleFunctions.plus(-minX + sigsq))
    }
    if (iter.asInstanceOf[MRNSDDoubleIterationMonitor].getRelativeTolerance ==
      -1.0) {
      iter.asInstanceOf[MRNSDDoubleIterationMonitor].setRelativeTolerance(sqrteps * alg.norm2(A.zMult(b,
        null, 1, 0, true)))
    }
    r = A.zMult(x, null)
    r.assign(b, DoubleFunctions.plusMultFirst(-1))
    if (!(M.isInstanceOf[DoubleIdentity])) {
      r = M.apply(r, null)
      r = M.transApply(r, null)
      r = A.zMult(r, null, 1, 0, true)
      r.assign(DoubleFunctions.neg)
      gamma = x.aggregate(r, DoubleFunctions.plus, DoubleFunctions.multSquare)
      rnrm = alg.norm2(r)
    } else {
      r = A.zMult(r, null, 1, 0, true)
      r.assign(DoubleFunctions.neg)
      gamma = x.aggregate(r, DoubleFunctions.plus, DoubleFunctions.multSquare)
      rnrm = Math.sqrt(gamma)
    }
    indexList = new IntArrayList(b.size.toInt)
    iter.setFirst()
    while (!iter.converged(rnrm, x)) {
      s = x.copy()
      s.assign(r, DoubleFunctions.multNeg)
      u = A.zMult(s, null)
      if (!(M.isInstanceOf[DoubleIdentity])) {
        u = M.apply(u, null)
      }
      theta = gamma /
        u.aggregate(DoubleFunctions.plus, DoubleFunctions.square)
      s.getNegativeValues(indexList, null)
      w = x.copy()
      w.assign(s, DoubleFunctions.divNeg, indexList)
      alpha = Math.min(theta, w.aggregate(DoubleFunctions.min, DoubleFunctions.identity, indexList))
      x.assign(s, DoubleFunctions.plusMultSecond(alpha))
      if (!(M.isInstanceOf[DoubleIdentity])) {
        w = M.transApply(u, null)
        w = A.zMult(w, null, 1, 0, true)
        r.assign(w, DoubleFunctions.plusMultSecond(alpha))
        gamma = x.aggregate(r, DoubleFunctions.plus, DoubleFunctions.multSquare)
        rnrm = alg.norm2(r)
      } else {
        w = A.zMult(u, null, 1, 0, true)
        r.assign(w, DoubleFunctions.plusMultSecond(alpha))
        gamma = x.aggregate(r, DoubleFunctions.plus, DoubleFunctions.multSquare)
        rnrm = Math.sqrt(gamma)
      }
      iter.next()
    }
    x
  }
}
