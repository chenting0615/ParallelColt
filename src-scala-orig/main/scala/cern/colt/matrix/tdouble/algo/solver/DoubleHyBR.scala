package cern.colt.matrix.tdouble.algo.solver

import optimization.DoubleFmin
import optimization.DoubleFmin_methods
import cern.colt.list.tdouble.DoubleArrayList
import cern.colt.matrix.tdouble.DoubleFactory2D
import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.algo.DenseDoubleAlgebra
import cern.colt.matrix.tdouble.algo.decomposition.DenseDoubleSingularValueDecomposition
import cern.colt.matrix.tdouble.algo.solver.preconditioner.DoubleIdentity
import cern.colt.matrix.tdouble.algo.solver.preconditioner.DoublePreconditioner
import cern.colt.matrix.tdouble.impl.DenseColumnDoubleMatrix2D
import cern.colt.matrix.tdouble.impl.DenseMatrix1D
import cern.colt.matrix.tdouble.impl.DenseMatrix2D
import cern.jet.math.tdouble.DoubleFunctions
import cern.jet.stat.tdouble.DoubleDescriptive
import DoubleHyBR._
//remove if not needed
import scala.collection.JavaConversions._

object DoubleHyBR {

  private val alg = DenseDoubleAlgebra.DEFAULT

  private val FMIN_TOL = 1.0e-4

  private class TikFmin2D(var bhat: StrideMatrix1D, var s: Array[Double], var omega: Double)
      extends DoubleFmin_methods {

    def f_to_minimize(alpha: Double): Double = {
      val m = bhat.size.toInt
      val n = s.length
      val t0 = bhat.viewPart(n, m - n).aggregate(DoubleFunctions.plus, DoubleFunctions.square)
      val s2 = new DenseMatrix1D(s)
      s2.assign(DoubleFunctions.square)
      val alpha2 = alpha * alpha
      val work = s2.copy()
      work.assign(DoubleFunctions.plus(alpha2))
      work.assign(DoubleFunctions.inv)
      val t1 = work.copy()
      t1.assign(DoubleFunctions.mult(alpha2))
      val t2 = t1.copy()
      t2.assign(bhat.viewPart(0, n), DoubleFunctions.mult)
      val t3 = work.copy()
      t3.assign(s2, DoubleFunctions.mult)
      t3.assign(DoubleFunctions.mult(1 - omega))
      val denom = t3.aggregate(t1, DoubleFunctions.plus, DoubleFunctions.plus) +
        m -
        n
      n *
        (t2.aggregate(DoubleFunctions.plus, DoubleFunctions.square) +
        t0) /
        (denom * denom)
    }
  }

  private trait DoubleLBD {

    def apply(): Unit

    def getC(): StrideMatrix2D

    def getU(): StrideMatrix2D

    def getV(): StrideMatrix2D
  }
}

/**
 * HyBR is a Hybrid Bidiagonalization Regularization method used for solving
 * large-scale, ill-posed inverse problems of the form: b = A*x + noise The
 * method combines an iterative Lanczos Bidiagonalization (LBD) Method with an
 * SVD-based regularization method to stabilize the semiconvergence behavior
 * that is characteristic of many ill-posed problems. The code is derived from
 * RestoreTools: An Object Oriented Matlab Package for Image Restoration written
 * by James G. Nagy and several of his students, including Julianne Chung,
 * Katrina Palmer, Lisa Perrone, and Ryan Wright.
 *
 * <p>
 * References:<br>
 * <p>
 * [1] Paige and Saunders, "LSQR an algorithm for sparse linear equations an
 * sparse least squares", ACM Trans. Math Software, 8 (1982), pp. 43-71.
 * </p>
 * <p>
 * [2] Bjorck, Grimme and Van Dooren, "An implicit shift bidiagonalization
 * algorithm for ill-posed systems", BIT 34 (11994), pp. 520-534.
 * </p>
 * <p>
 * [3] Chung, Nagy and O'Leary, "A Weighted GCV Method for Lanczos Hybrid
 * Regularization", Elec. Trans. Numer. Anal., 28 (2008), pp. 149--167.
 * </p>
 * </p>
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 *
 */
class DoubleHyBR(var innerSolver: HyBRInnerSolver,
    regularizationMethod: HyBRRegularizationMethod,
    regularizationParameter: Double,
    var omega: Double,
    reorthogonalize: Boolean,
    beginRegularization: Int,
    flatTolerance: Double,
    var computeRnrm: Boolean) extends AbstractDoubleIterativeSolver {

  private var regMethod: HyBRRegularizationMethod = regularizationMethod

  private var regPar: Double = regularizationParameter

  private var reorth: Boolean = reorthogonalize

  private var begReg: Int = beginRegularization

  private var flatTol: Double = flatTolerance

  if ((regularizationParameter < 0.0) || (regularizationParameter > 1.0)) {
    throw new IllegalArgumentException("regularizationParameter must be a number between 0 and 1.")
  }

  if (omega < 0.0) {
    throw new IllegalArgumentException("omega must be a nonnegative number.")
  }

  if (beginRegularization < 2) {
    throw new IllegalArgumentException("beginRegularization must be greater or equal 2")
  }

  if (flatTolerance < 0.0) {
    throw new IllegalArgumentException("flatTolerance must be a nonnegative number.")
  }

  this.iter = new HyBRDoubleIterationMonitor()

  /**
   * Creates new instance of HyBR solver with default parameters:<br>
   * <br>
   * innerSolver = HyBR.InnerSolver.TIKHONOV<br>
   * regularizationMethod = HyBR.RegularizationMethod.ADAPTWGCV<br>
   * regularizationParameter = 0<br>
   * omega = 0<br>
   * reorthogonalize = false<br>
   * beginRegularization = 2<br>
   * flatTolerance = 1e-6<br>
   * computeRnrm = false;
   */
  def this() {
    this(HyBRInnerSolver.TIKHONOV, HyBRRegularizationMethod.ADAPTWGCV, 0, 0, false, 2, 1e-6, false)
  }

  def solve(A: StrideMatrix2D, b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = {
    if (!(iter.isInstanceOf[HyBRDoubleIterationMonitor])) {
      this.iter = new HyBRDoubleIterationMonitor()
    }
    checkSizes(A, b, x)
    val rows = A.rows()
    val columns = A.columns()
    var bump = false
    var warning = false
    var rnrm = -1.0
    var iterationsSave = 0
    var alpha = 0
    var alphaSave = 0
    var beta = 0
    var inSolver = HyBRInnerSolver.NONE
    var lbd: DoubleLBD = null
    var v: StrideMatrix1D = null
    var work: StrideMatrix1D = null
    var Ub: StrideMatrix2D = null
    var Vb: StrideMatrix2D = null
    var f: StrideMatrix1D = null
    var xSave: StrideMatrix1D = null
    var sv: Array[Double] = null
    val omegaList = new DoubleArrayList(Array.ofDim[Double](begReg - 2))
    val GCV = new DoubleArrayList(Array.ofDim[Double](begReg - 2))
    var U = new DenseMatrix2D(1, b.size.toInt)
    var C: StrideMatrix2D = null
    var V: StrideMatrix2D = null
    var svd: DenseDoubleSingularValueDecomposition = null
    if (computeRnrm) {
      work = b.copy()
      A.zMult(x, work, -1, 1, false)
      rnrm = alg.norm2(work)
    }
    if (M.isInstanceOf[DoubleIdentity]) {
      beta = alg.norm2(b)
      U.viewRow(0).assign(b, DoubleFunctions.multSecond(1.0 / beta))
      lbd = new DoubleSimpleLBD(A, U, reorth)
    } else {
      work = new DenseMatrix1D(b.size.toInt)
      work = M.apply(b, work)
      beta = alg.norm2(work)
      U.viewRow(0).assign(work, DoubleFunctions.multSecond(1.0 / beta))
      lbd = new DoublePLBD(M, A, U, reorth)
    }
    iter.setFirst()
    while (!iter.converged(rnrm, x)) {
      lbd.apply()
      U = lbd.getU
      C = lbd.getC
      V = lbd.getV
      v = new DenseMatrix1D(C.columns() + 1)
      v.setQuick(0, beta)
      val i = iter.iterations()
      if (i >= 1) {
        if (i >= begReg - 1) {
          inSolver = innerSolver
        }
        inSolver match {
          case TIKHONOV =>
            svd = alg.svd(C)
            Ub = svd.getU
            sv = svd.getSingularValues
            Vb = svd.getV
            if (regMethod == HyBRRegularizationMethod.ADAPTWGCV) {
              work = new DenseMatrix1D(Ub.rows())
              Ub.zMult(v, work, 1, 0, true)
              omegaList.add(Math.min(1, findOmega(work, sv)))
              omega = DoubleDescriptive.mean(omegaList)
            }
            f = new DenseMatrix1D(Vb.rows())
            alpha = tikhonovSolver(Ub, sv, Vb, v, f)
            GCV.add(GCVstopfun(alpha, Ub.viewRow(0), sv, beta, rows, columns))
            iter.asInstanceOf[HyBRDoubleIterationMonitor].setRegularizationParameter(alpha)
            if (i > 1) {
              if (Math.abs((GCV.getQuick(i - 1) - GCV.getQuick(i - 2))) /
                GCV.get(begReg - 2) <
                flatTol) {
                V.zMult(f, x)
                iter.asInstanceOf[HyBRDoubleIterationMonitor].setStoppingCondition(HyBRDoubleIterationMonitor.HyBRStoppingCondition.FLAT_GCV_CURVE)
                if (computeRnrm) {
                  work = b.copy()
                  A.zMult(x, work, -1, 1, false)
                  iter.asInstanceOf[HyBRDoubleIterationMonitor].residual = alg.norm2(work)
                }
                return x
              } else if ((warning == true) && (GCV.size > iterationsSave + 3)) {
                for (j <- iterationsSave until GCV.size if GCV.getQuick(iterationsSave - 1) > GCV.get(j)) {
                  bump = true
                }
                if (bump == false) {
                  x.assign(xSave)
                  iter.asInstanceOf[HyBRDoubleIterationMonitor].setStoppingCondition(HyBRDoubleIterationMonitor.HyBRStoppingCondition.MIN_OF_GCV_CURVE_WITHIN_WINDOW_OF_4_ITERATIONS)
                  iter.asInstanceOf[HyBRDoubleIterationMonitor].iter = iterationsSave
                  if (computeRnrm) {
                    work = b.copy()
                    A.zMult(x, work, -1, 1, false)
                    iter.asInstanceOf[HyBRDoubleIterationMonitor].residual = alg.norm2(work)
                  }
                  iter.asInstanceOf[HyBRDoubleIterationMonitor].setRegularizationParameter(alphaSave)
                  return x
                } else {
                  bump = false
                  warning = false
                  iterationsSave = iter.getMaxIterations
                }
              } else if (warning == false) {
                if (GCV.get(i - 2) < GCV.get(i - 1)) {
                  warning = true
                  xSave = new DenseMatrix1D(V.rows())
                  alphaSave = alpha
                  V.zMult(f, xSave)
                  iterationsSave = i
                }
              }
            }

          case NONE => f = alg.solve(C, v)
        }
        V.zMult(f, x)
        if (computeRnrm) {
          work = b.copy()
          A.zMult(x, work, -1, 1, false)
          rnrm = alg.norm2(work)
        }
      }
      iter.next()
    }
    x
  }

  protected def checkSizes(A: StrideMatrix2D, b: StrideMatrix1D, x: StrideMatrix1D) {
    if (b.size != A.rows()) throw new IllegalArgumentException("b.size() != A.rows()")
    if (x.size != A.columns()) throw new IllegalArgumentException("x.size() != A.columns()")
  }

  private def findOmega(bhat: StrideMatrix1D, s: Array[Double]): Double = {
    val m = bhat.size.toInt
    val n = s.length
    val alpha = s(n - 1)
    val t0 = bhat.viewPart(n, m - n).aggregate(DoubleFunctions.plus, DoubleFunctions.square)
    var s2 = new DenseMatrix1D(s)
    s2.assign(DoubleFunctions.square)
    val alpha2 = alpha * alpha
    val tt = s2.copy()
    tt.assign(DoubleFunctions.plus(alpha2))
    tt.assign(DoubleFunctions.inv)
    val t1 = s2.aggregate(tt, DoubleFunctions.plus, DoubleFunctions.mult)
    s2 = new DenseMatrix1D(s)
    s2.assign(DoubleFunctions.mult(alpha))
    s2.assign(bhat.viewPart(0, n), DoubleFunctions.mult)
    s2.assign(DoubleFunctions.square)
    var work = tt.copy()
    work.assign(DoubleFunctions.pow(3))
    work.assign(DoubleFunctions.abs)
    val t3 = work.aggregate(s2, DoubleFunctions.plus, DoubleFunctions.mult)
    work = new DenseMatrix1D(s)
    work.assign(tt, DoubleFunctions.mult)
    val t4 = work.aggregate(DoubleFunctions.plus, DoubleFunctions.square)
    work = tt.copy()
    work.assign(bhat.viewPart(0, n), DoubleFunctions.mult)
    work.assign(DoubleFunctions.mult(alpha2))
    val t5 = work.aggregate(DoubleFunctions.plus, DoubleFunctions.square)
    s2 = new DenseMatrix1D(s)
    s2.assign(bhat.viewPart(0, n), DoubleFunctions.mult)
    s2.assign(DoubleFunctions.square)
    tt.assign(DoubleFunctions.pow(3))
    tt.assign(DoubleFunctions.abs)
    val v2 = tt.aggregate(s2, DoubleFunctions.plus, DoubleFunctions.mult)
    (m * alpha2 * v2) / (t1 * t3 + t4 * (t5 + t0))
  }

  private def tikhonovSolver(U: StrideMatrix2D,
      s: Array[Double],
      V: StrideMatrix2D,
      b: StrideMatrix1D,
      x: StrideMatrix1D): Double = {
    var fmin: TikFmin2D = null
    var bhat = new DenseMatrix1D(U.rows())
    U.zMult(b, bhat, 1, 0, true)
    var alpha = 0
    regMethod match {
      case GCV =>
        fmin = new TikFmin2D(bhat, s, 1)
        alpha = DoubleFmin.fmin(0, s(0), fmin, FMIN_TOL)

      case WGCV =>
        fmin = new TikFmin2D(bhat, s, omega)
        alpha = DoubleFmin.fmin(0, s(0), fmin, FMIN_TOL)

      case ADAPTWGCV =>
        fmin = new TikFmin2D(bhat, s, omega)
        alpha = DoubleFmin.fmin(0, s(0), fmin, FMIN_TOL)

      case NONE => alpha = regPar
    }
    val d = new DenseMatrix1D(s)
    d.assign(DoubleFunctions.square)
    d.assign(DoubleFunctions.plus(alpha * alpha))
    bhat = bhat.viewPart(0, s.length)
    val S = new DenseMatrix1D(s)
    bhat.assign(S, DoubleFunctions.mult)
    bhat.assign(d, DoubleFunctions.div)
    V.zMult(bhat, x)
    alpha
  }

  private def GCVstopfun(alpha: Double,
      u: StrideMatrix1D,
      s: Array[Double],
      beta: Double,
      rows: Int,
      columns: Int): Double = {
    val k = s.length
    val beta2 = beta * beta
    val s2 = new DenseMatrix1D(s)
    s2.assign(DoubleFunctions.square)
    val alpha2 = alpha * alpha
    val t1 = s2.copy()
    t1.assign(DoubleFunctions.plus(alpha2))
    t1.assign(DoubleFunctions.inv)
    val t2 = t1.copy()
    t2.assign(u.viewPart(0, k), DoubleFunctions.mult)
    t2.assign(DoubleFunctions.mult(alpha2))
    val num = beta2 *
      (t2.aggregate(DoubleFunctions.plus, DoubleFunctions.square) +
      Math.pow(Math.abs(u.getQuick(k)), 2)) /
      columns
    var den = (rows -
      t1.aggregate(s2, DoubleFunctions.plus, DoubleFunctions.mult)) /
      columns
    den = den * den
    num / den
  }

  private class DoubleSimpleLBD(val A: StrideMatrix2D, var U: StrideMatrix2D, var reorth: Boolean)
      extends DoubleLBD {

    private val alg = DenseDoubleAlgebra.DEFAULT

    private val factory = DoubleFactory2D.dense

    private val alphaBeta = new DenseMatrix2D(2, 1)

    private var C: StrideMatrix2D = null

    private var V: StrideMatrix2D = null

    private var counter: Int = 1

    def apply() {
      if (reorth) {
        val k = U.rows()
        var u: StrideMatrix1D = null
        var v: StrideMatrix1D = null
        var column: StrideMatrix1D = null
        if (k == 1) {
          v = A.zMult(U.viewRow(k - 1), v, 1, 0, true)
        } else {
          v = A.zMult(U.viewRow(k - 1), v, 1, 0, true)
          column = V.viewColumn(k - 2)
          v.assign(column, DoubleFunctions.plusMultSecond(-C.getQuick(k - 1, k - 2)))
          for (j <- 0 until k - 1) {
            column = V.viewColumn(j)
            v.assign(column, DoubleFunctions.plusMultSecond(-column.zDotProduct(v)))
          }
        }
        val alpha = alg.norm2(v)
        v.assign(DoubleFunctions.div(alpha))
        u = A.zMult(v, u)
        column = U.viewRow(k - 1)
        u.assign(column, DoubleFunctions.plusMultSecond(-alpha))
        for (j <- 0 until k) {
          column = U.viewRow(j)
          u.assign(column, DoubleFunctions.plusMultSecond(-column.zDotProduct(u)))
        }
        val beta = alg.norm2(u)
        alphaBeta.setQuick(0, 0, alpha)
        alphaBeta.setQuick(1, 0, beta)
        u.assign(DoubleFunctions.div(beta))
        U = factory.appendRow(U, u)
        if (V == null) {
          V = new DenseColumnDoubleMatrix2D(v.size.toInt, 1)
          V.assign(v.elements().asInstanceOf[Array[Double]])
        } else {
          V = factory.appendColumn(V, v)
        }
        if (C == null) {
          C = new DenseMatrix2D(2, 1)
          C.assign(alphaBeta)
        } else {
          C = factory.composeBidiagonal(C, alphaBeta)
        }
      } else {
        var u: StrideMatrix1D = null
        var v: StrideMatrix1D = null
        var column: StrideMatrix1D = null
        if (counter == 1) {
          v = A.zMult(U.viewRow(0), v, 1, 0, true)
        } else {
          v = A.zMult(U.viewRow(0), v, 1, 0, true)
          column = V.viewColumn(counter - 2)
          v.assign(column, DoubleFunctions.plusMultSecond(-C.getQuick(counter - 1, counter - 2)))
        }
        val alpha = alg.norm2(v)
        v.assign(DoubleFunctions.div(alpha))
        u = A.zMult(v, u)
        column = U.viewRow(0)
        u.assign(column, DoubleFunctions.plusMultSecond(-alpha))
        val beta = alg.norm2(u)
        alphaBeta.setQuick(0, 0, alpha)
        alphaBeta.setQuick(1, 0, beta)
        u.assign(DoubleFunctions.div(beta))
        U.viewRow(0).assign(u)
        if (V == null) {
          V = new DenseColumnDoubleMatrix2D(v.size.toInt, 1)
          V.assign(v.elements().asInstanceOf[Array[Double]])
        } else {
          V = factory.appendColumn(V, v)
        }
        if (C == null) {
          C = new DenseMatrix2D(2, 1)
          C.assign(alphaBeta)
        } else {
          C = factory.composeBidiagonal(C, alphaBeta)
        }
        counter += 1
      }
    }

    def getC(): StrideMatrix2D = C

    def getU(): StrideMatrix2D = U

    def getV(): StrideMatrix2D = V
  }

  private class DoublePLBD(val M: DoublePreconditioner,
      val A: StrideMatrix2D,
      var U: StrideMatrix2D,
      var reorth: Boolean) extends DoubleLBD {

    private val alg = DenseDoubleAlgebra.DEFAULT

    private val factory = DoubleFactory2D.dense

    private val alphaBeta = new DenseMatrix2D(2, 1)

    private var C: StrideMatrix2D = null

    private var V: StrideMatrix2D = null

    private var counter: Int = 1

    def apply() {
      if (reorth) {
        val k = U.rows()
        var u: StrideMatrix1D = null
        var v: StrideMatrix1D = null
        var row: StrideMatrix1D = null
        if (k == 1) {
          row = U.viewRow(k - 1).copy()
          row = M.transApply(row, row)
          v = A.zMult(row, v, 1, 0, true)
        } else {
          row = U.viewRow(k - 1).copy()
          row = M.transApply(row, row)
          v = A.zMult(row, v, 1, 0, true)
          row = V.viewColumn(k - 2)
          v.assign(row, DoubleFunctions.plusMultSecond(-C.getQuick(k - 1, k - 2)))
          for (j <- 0 until k - 1) {
            row = V.viewColumn(j)
            v.assign(row, DoubleFunctions.plusMultSecond(-row.zDotProduct(v)))
          }
        }
        val alpha = alg.norm2(v)
        v.assign(DoubleFunctions.div(alpha))
        row = A.zMult(v, row)
        u = M.apply(row, u)
        row = U.viewRow(k - 1)
        u.assign(row, DoubleFunctions.plusMultSecond(-alpha))
        for (j <- 0 until k) {
          row = U.viewRow(j)
          u.assign(row, DoubleFunctions.plusMultSecond(-row.zDotProduct(u)))
        }
        val beta = alg.norm2(u)
        alphaBeta.setQuick(0, 0, alpha)
        alphaBeta.setQuick(1, 0, beta)
        u.assign(DoubleFunctions.div(beta))
        U = factory.appendRow(U, u)
        if (V == null) {
          V = new DenseColumnDoubleMatrix2D(v.size.toInt, 1)
          V.assign(v.elements().asInstanceOf[Array[Double]])
        } else {
          V = factory.appendColumn(V, v)
        }
        if (C == null) {
          C = new DenseMatrix2D(2, 1)
          C.assign(alphaBeta)
        } else {
          C = factory.composeBidiagonal(C, alphaBeta)
        }
      } else {
        var u: StrideMatrix1D = null
        var v: StrideMatrix1D = null
        var row: StrideMatrix1D = null
        if (counter == 1) {
          row = U.viewRow(0).copy()
          row = M.transApply(row, row)
          v = A.zMult(row, v, 1, 0, true)
        } else {
          row = U.viewRow(0).copy()
          row = M.transApply(row, row)
          v = A.zMult(row, v, 1, 0, true)
          row = V.viewColumn(counter - 2)
          v.assign(row, DoubleFunctions.plusMultSecond(-C.getQuick(counter - 1, counter - 2)))
        }
        val alpha = alg.norm2(v)
        v.assign(DoubleFunctions.div(alpha))
        row = A.zMult(v, row)
        u = M.apply(row, u)
        row = U.viewRow(0)
        u.assign(row, DoubleFunctions.plusMultSecond(-alpha))
        val beta = alg.norm2(u)
        alphaBeta.setQuick(0, 0, alpha)
        alphaBeta.setQuick(1, 0, beta)
        u.assign(DoubleFunctions.div(beta))
        U.viewRow(0).assign(u)
        if (V == null) {
          V = new DenseColumnDoubleMatrix2D(v.size.toInt, 1)
          V.assign(v.elements().asInstanceOf[Array[Double]])
        } else {
          V = factory.appendColumn(V, v)
        }
        if (C == null) {
          C = new DenseMatrix2D(2, 1)
          C.assign(alphaBeta)
        } else {
          C = factory.composeBidiagonal(C, alphaBeta)
        }
        counter += 1
      }
    }

    def getC(): StrideMatrix2D = C

    def getU(): StrideMatrix2D = U

    def getV(): StrideMatrix2D = V
  }
}
