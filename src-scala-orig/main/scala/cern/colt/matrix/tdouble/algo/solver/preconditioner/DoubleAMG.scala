package cern.colt.matrix.tdouble.algo.solver.preconditioner

import java.util.ArrayList
import java.util.Arrays
import java.util.HashMap
import java.util.HashSet
import java.util.LinkedList
import java.util.List
import java.util.Map
import java.util.Set
import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.algo.decomposition.DenseDoubleLUDecompositionQuick
import cern.colt.matrix.tdouble.impl.DenseMatrix1D
import cern.colt.matrix.tdouble.impl.DenseMatrix2D
import cern.colt.matrix.tdouble.impl.SparseCCDoubleMatrix2D
import cern.colt.matrix.tdouble.impl.SparseCCMDoubleMatrix2D
import cern.colt.matrix.tdouble.impl.SparseRCDoubleMatrix2D
import cern.colt.matrix.tdouble.impl.SparseRCMDoubleMatrix2D
import DoubleAMG._
//remove if not needed
import scala.collection.JavaConversions._

object DoubleAMG {

  /**
   * Creates aggregates. These are disjoint sets, each of which represents one
   * node at a coarser mesh by aggregating together a set of fine nodes
   */
  private class Aggregator(A: SparseRCDoubleMatrix2D, eps: Double) {

    /**
     * The aggregates
     */
    private var C: List[Set[Integer]] = createFinalAggregates(C, N, R)

    /**
     * Diagonal indexes into the sparse matrix
     */
    private var diagind: Array[Int] = findDiagonalindexes(A)

    /**
     * The strongly coupled node neighborhood of a given node
     */
    private var N: List[Set[Integer]] = findNodeNeighborhood(A, diagind, eps)

    val R = createInitialR(A)

    /**
     * Gets the aggregates
     */
    def getAggregates(): List[Set[Integer]] = C

    /**
     * Returns the matrix diagonal indexes. This is a by-product of the
     * aggregation
     */
    def getDiagonalindexes(): Array[Int] = diagind

    /**
     * Returns the strongly coupled node neighborhoods of a given node. This
     * is a by-product of the aggregation
     */
    def getNodeNeighborhoods(): List[Set[Integer]] = N

    /**
     * Finds the diagonal indexes of the matrix
     */
    private def findDiagonalindexes(A: SparseRCDoubleMatrix2D): Array[Int] = {
      val rowptr = A.getRowPointers
      val colind = A.getColumnIndexes
      val diagind = Array.ofDim[Int](A.rows())
      for (i <- 0 until A.rows()) {
        diagind(i) = cern.colt.Sorting.binarySearchFromTo(colind, i, rowptr(i), rowptr(i + 1))
        if (diagind(i) < 0) throw new RuntimeException("Matrix is missing a diagonal entry on row " + (i + 1))
      }
      diagind
    }

    /**
     * Finds the strongly coupled node neighborhoods
     */
    private def findNodeNeighborhood(A: SparseRCDoubleMatrix2D, diagind: Array[Int], eps: Double): List[Set[Integer]] = {
      N = new ArrayList[Set[Integer]](A.rows())
      val rowptr = A.getRowPointers
      val colind = A.getColumnIndexes
      val data = A.getValues
      for (i <- 0 until A.rows()) {
        val Ni = new HashSet[Integer]()
        val aii = data(diagind(i))
        for (j <- rowptr(i) until rowptr(i + 1)) {
          val aij = data(j)
          val ajj = data(diagind(colind(j)))
          if (Math.abs(aij) >= eps * Math.sqrt(aii * ajj)) Ni.add(colind(j))
        }
        N.add(Ni)
      }
      N
    }

    /**
     * Creates the initial R-set by including only the connected nodes
     */
    private def createInitialR(A: SparseRCDoubleMatrix2D): Array[Boolean] = {
      val R = Array.ofDim[Boolean](A.rows())
      val rowptr = A.getRowPointers
      val colind = A.getColumnIndexes
      val data = A.getValues
      for (i <- 0 until A.rows()) {
        var hasOffDiagonal = false
        for (j <- rowptr(i) until rowptr(i + 1) if colind(j) != i && data(j) != 0) {
          hasOffDiagonal = true
          //break
        }
        R(i) = hasOffDiagonal
      }
      R
    }

    /**
     * Creates the initial aggregates
     */
    private def createInitialAggregates(N: List[Set[Integer]], R: Array[Boolean]): List[Set[Integer]] = {
      C = new ArrayList[Set[Integer]]()
      for (i <- 0 until R.length) {
        if (!R(i)) //continue
        var free = true
        for (j <- N.get(i)) free &= R(j)
        if (free) {
          C.add(new HashSet[Integer](N.get(i)))
          for (j <- N.get(i)) R(j) = false
        }
      }
      C
    }

    /**
     * Enlarges the aggregates
     */
    private def enlargeAggregates(C: List[Set[Integer]], N: List[Set[Integer]], R: Array[Boolean]): List[Set[Integer]] = {
      val belong = new ArrayList[List[Integer]](R.length)
      for (i <- 0 until R.length) belong.add(new ArrayList[Integer]())
      for (k <- 0 until C.size; j <- C.get(k)) belong.get(j).add(k)
      val intersect = Array.ofDim[Int](C.size)
      for (i <- 0 until R.length) {
        if (!R(i)) //continue
        Arrays.fill(intersect, 0)
        var largest = 0
        var maxValue = 0
        for (j <- N.get(i); k <- belong.get(j)) {
          intersect(k) += 1
          if (intersect(k) > maxValue) {
            largest = k
            maxValue = intersect(largest)
          }
        }
        if (maxValue > 0) {
          R(i) = false
          C.get(largest).add(i)
        }
      }
      C
    }

    /**
     * Creates final aggregates from the remaining unallocated nodes
     */
    private def createFinalAggregates(C: List[Set[Integer]], N: List[Set[Integer]], R: Array[Boolean]): List[Set[Integer]] = {
      for (i <- 0 until R.length) {
        if (!R(i)) //continue
        val Cn = new HashSet[Integer]()
        for (j <- N.get(i) if R(j)) {
          R(j) = false
          Cn.add(j)
        }
        if (!Cn.isEmpty) C.add(Cn)
      }
      C
    }
  }

  /**
   * Creates interpolation (prolongation) operators using based on the
   * aggregates. Can optionally smooth the aggregates
   */
  private class Interpolator(aggregator: Aggregator, A: SparseRCDoubleMatrix2D, omega: Double)
      {

    /**
     * The Galerkin coarse-space operator
     */
    private var Ac: SparseRCDoubleMatrix2D = _

    /**
     * The interpolation (prolongation) matrix
     */
    private var I: SparseCCDoubleMatrix2D = _

    val C = aggregator.getAggregates

    val N = aggregator.getNodeNeighborhoods

    val diagind = aggregator.getDiagonalindexes

    val pt = createTentativeProlongation(C, A.rows())

    if (omega != 0) {
      val P = createSmoothedProlongation(C, N, A, diagind, omega, pt)
      I = createInterpolationMatrix(P, A.rows())
      Ac = createGalerkinSlow(I, A)
    } else {
      Ac = createGalerkinFast(A, pt, C.size)
      I = createInterpolationMatrix(pt, C.size)
    }

    /**
     * Creates the tentative prolongation operator. Since the columns are
     * all disjoint, and its entries are binary, it is possible to store it
     * in a single array. Its length equals the number of fine nodes, and
     * the entries are the indexes to the corresponding aggregate (C-set).
     */
    private def createTentativeProlongation(C: List[Set[Integer]], n: Int): Array[Int] = {
      val pt = Array.ofDim[Int](n)
      Arrays.fill(pt, -1)
      for (i <- 0 until C.size; j <- C.get(i)) pt(j) = i
      pt
    }

    /**
     * Creates the Galerkin operator using the assumption of disjoint
     * (non-smoothed) aggregates
     */
    private def createGalerkinFast(A: SparseRCDoubleMatrix2D, pt: Array[Int], c: Int): SparseRCDoubleMatrix2D = {
      val n = pt.length
      val Ac = new SparseRCMDoubleMatrix2D(c, c)
      val rowptr = A.getRowPointers
      val colind = A.getColumnIndexes
      val data = A.getValues
      for (i <- 0 until n if pt(i) != -1; j <- rowptr(i) until rowptr(i + 1) if pt(colind(j)) != -1) Ac.setQuick(pt(i),
        pt(colind(j)), data(j))
      (new SparseRCDoubleMatrix2D(Ac.rows(), Ac.columns())
        .assign(Ac)).asInstanceOf[SparseRCDoubleMatrix2D]
    }

    /**
     * Creates the interpolation (prolongation) matrix based on the smoothed
     * aggregates
     */
    private def createInterpolationMatrix(P: List[Map[Integer, Double]], n: Int): SparseCCDoubleMatrix2D = {
      val c = P.size
      I = new SparseCCDoubleMatrix2D(n, c)
      for (j <- 0 until c) {
        val Pj = P.get(j)
        for ((key, value) <- Pj) I.setQuick(key, j, value)
      }
      I
    }

    /**
     * Creates the interpolation (prolongation) matrix based on the
     * non-smoothed aggregates
     */
    private def createInterpolationMatrix(pt: Array[Int], c: Int): SparseCCDoubleMatrix2D = {
      val If = new SparseCCMDoubleMatrix2D(pt.length, c)
      for (i <- 0 until pt.length if pt(i) != -1) If.setQuick(i, pt(i), 1)
      (new SparseCCDoubleMatrix2D(If.rows(), If.columns())
        .assign(If)).asInstanceOf[SparseCCDoubleMatrix2D]
    }

    /**
     * Gets the interpolation (prolongation) operator
     */
    def getInterpolationOperator(): SparseCCDoubleMatrix2D = I

    /**
     * Creates the smoothes interpolation (prolongation) operator by a
     * single sweep of the damped Jacobi method
     */
    private def createSmoothedProlongation(C: List[Set[Integer]],
        N: List[Set[Integer]],
        A: SparseRCDoubleMatrix2D,
        diagind: Array[Int],
        omega: Double,
        pt: Array[Int]): List[Map[Integer, Double]] = {
      val n = A.rows()
      val c = C.size
      val P = new ArrayList[Map[Integer, Double]](c)
      for (i <- 0 until c) P.add(new HashMap[Integer, Double]())
      val rowptr = A.getRowPointers
      val colind = A.getColumnIndexes
      val data = A.getValues
      val dot = Array.ofDim[Double](c)
      for (i <- 0 until n) {
        if (pt(i) == -1) //continue
        Arrays.fill(dot, 0)
        val Ni = N.get(i)
        var weakAij = 0
        for (j <- rowptr(i) until rowptr(i + 1)) {
          if (pt(colind(j)) == -1) //continue
          val aij = data(j)
          if (aij != 0 && !Ni.contains(colind(j))) {
            weakAij += aij
            //continue
          }
          dot(pt(colind(j))) += aij
        }
        dot(pt(i)) -= weakAij
        val scale = -omega / data(diagind(i))
        for (j <- 0 until dot.length) dot(j) *= scale
        dot(pt(i)) += 1
        for (j <- 0 until dot.length if dot(j) != 0) P.get(j).put(i, dot(j))
      }
      P
    }

    /**
     * Creates the entries of the Galerkin operator
     * <code>Ac = I<sup>T</sup> A I</code>. This is a very time-consuming
     * operation
     */
    private def createGalerkinSlow(I: SparseCCDoubleMatrix2D, A: SparseRCDoubleMatrix2D): SparseRCDoubleMatrix2D = {
      val n = I.rows()
      val c = I.columns()
      val Ac = new SparseRCMDoubleMatrix2D(c, c)
      val aiCol = Array.ofDim[Double](n)
      val iCol = Array.ofDim[Double](n)
      val aiV = new DenseMatrix1D(n, aiCol, 0, 1, false)
      val iV = new DenseMatrix1D(n, iCol, 0, 1, false)
      val itaiCol = Array.ofDim[Double](c)
      val itaiV = new DenseMatrix1D(c, itaiCol, 0, 1, false)
      val colptr = I.getColumnPointers
      val rowind = I.getRowIndexes
      val Idata = I.getValues
      for (k <- 0 until c) {
        iV.assign(0)
        for (i <- colptr(k) until colptr(k + 1)) iCol(rowind(i)) = Idata(i)
        A.zMult(iV, aiV)
        I.zMult(aiV, itaiV, 1, 0, true)
        for (i <- 0 until c if itaiCol(i) != 0) Ac.setQuick(i, k, itaiCol(i))
      }
      (new SparseRCDoubleMatrix2D(Ac.rows(), Ac.columns())
        .assign(Ac)).asInstanceOf[SparseRCDoubleMatrix2D]
    }

    /**
     * Gets the Galerkin operator
     */
    def getGalerkinOperator(): SparseRCDoubleMatrix2D = Ac
  }
}

/**
 * Algebraic multigrid preconditioner. Uses the smoothed aggregation method
 * described by Vanek, Mandel, and Brezina (1996).
 */
class DoubleAMG(val omegaPreF: Double,
    val omegaPreR: Double,
    val omegaPostF: Double,
    val omegaPostR: Double,
    val nu1: Int,
    val nu2: Int,
    val gamma: Int,
    val min: Int,
    val omega: Double) extends DoublePreconditioner {

  /**
   * Relaxations at each level
   */
  private var preM: Array[DoubleAMG.SSOR] = _

  private var postM: Array[DoubleAMG.SSOR] = _

  /**
   * The number of levels
   */
  private var m: Int = _

  /**
   * System matrix at each level, except at the coarsest
   */
  private var A: Array[SparseRCDoubleMatrix2D] = _

  /**
   * LU factorization at the coarsest level
   */
  private var lu: DenseDoubleLUDecompositionQuick = _

  /**
   * Solution, right-hand side, and residual vectors at each level
   */
  private var u: Array[DenseMatrix1D] = _

  private var f: Array[DenseMatrix1D] = _

  private var r: Array[DenseMatrix1D] = _

  /**
   * Interpolation operators going to a finer mesh
   */
  private var I: Array[SparseCCDoubleMatrix2D] = _

  /**
   * Perform a reverse (backwards) smoothing sweep
   */
  private val reverse = true

  /**
   * Operating in transpose mode?
   */
  private var transpose: Boolean = _

  /**
   * Sets up the algebraic multigrid preconditioner. Uses an SOR method,
   * without the backward sweep in SSOR
   *
   * @param omegaPre
   *            Overrelaxation parameter in the pre-smoothing
   * @param omegaPost
   *            Overrelaxation parameter in the post-smoothing
   * @param nu1
   *            Number of pre-relaxations to perform
   * @param nu2
   *            Number of post-relaxations to perform
   * @param gamma
   *            Number of times to go to a coarser level
   * @param min
   *            Smallest matrix size before using a direct solver
   * @param omega
   *            Jacobi damping parameter, between zero and one. If it equals
   *            zero, the method reduces to the standard aggregate multigrid
   *            method
   */
  def this(omegaPre: Double,
      omegaPost: Double,
      nu1: Int,
      nu2: Int,
      gamma: Int,
      min: Int,
      omega: Double) {
    this()
    this.omegaPreF = omegaPre
    this.omegaPreR = omegaPre
    this.omegaPostF = omegaPost
    this.omegaPostR = omegaPost
    reverse = false
    this.nu1 = nu1
    this.nu2 = nu2
    this.gamma = gamma
    this.min = min
    this.omega = omega
  }

  /**
   * Sets up the algebraic multigrid preconditioner using some default
   * parameters. In the presmoothing, <code>omegaF=1</code> and
   * <code>omegaR=1.85</code>, while in the postsmoothing,
   * <code>omegaF=1.85</code> and <code>omegaR=1</code>. Sets
   * <code>nu1=nu2=gamma=1</code>, has a smallest matrix size of 40, and sets
   * <code>omega=2/3</code>.
   */
  def this() {
    this(1, 1.85, 1.85, 1, 1, 1, 1, 40, 2. / 3)
  }

  def apply(b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = {
    if (x == null) {
      x = b.like()
    }
    u(0).assign(x)
    f(0).assign(b)
    transpose = false
    cycle(0)
    x.assign(u(0))
  }

  def transApply(b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = {
    if (x == null) {
      x = b.like()
    }
    u(0).assign(x)
    f(0).assign(b)
    transpose = true
    cycle(0)
    x.assign(u(0))
  }

  def setMatrix(A: StrideMatrix2D) {
    val Al = new LinkedList[SparseRCDoubleMatrix2D]()
    val Il = new LinkedList[SparseCCDoubleMatrix2D]()
    val Arc = new SparseRCDoubleMatrix2D(A.rows(), A.columns())
    Arc.assign(A)
    if (!Arc.hasColumnIndexesSorted()) Arc.sortColumnIndexes()
    Al.add(Arc)
    var k = 0
    while (Al.get(k).rows() > min) {
      val Af = Al.get(k)
      val eps = 0.08 * Math.pow(0.5, k)
      val aggregator = new Aggregator(Af, eps)
      if (aggregator.getAggregates.size == 0) //break
      val sa = new Interpolator(aggregator, Af, omega)
      Al.add(sa.getGalerkinOperator)
      Il.add(sa.getInterpolationOperator)
      k
    }
    m = Al.size
    if (m == 0) throw new RuntimeException("Matrix too small for AMG")
    I = Array.ofDim[SparseCCDoubleMatrix2D](m - 1)
    this.A = Array.ofDim[SparseRCDoubleMatrix2D](m - 1)
    Il.toArray(I)
    for (i <- 0 until Al.size - 1) this.A(i) = Al.get(i)
    val Ac = new DenseMatrix2D(Al.get(Al.size - 1).toArray())
    lu = new DenseDoubleLUDecompositionQuick()
    lu.decompose(Ac)
    u = Array.ofDim[DenseMatrix1D](m)
    f = Array.ofDim[DenseMatrix1D](m)
    r = Array.ofDim[DenseMatrix1D](m)
    for (k <- 0 until m) {
      val n = Al.get(k).rows()
      u(k) = new DenseMatrix1D(n)
      f(k) = new DenseMatrix1D(n)
      r(k) = new DenseMatrix1D(n)
    }
    preM = Array.ofDim[SSOR](m - 1)
    postM = Array.ofDim[SSOR](m - 1)
    for (k <- 0 until m - 1) {
      val Ak = this.A(k)
      preM(k) = new SSOR(Ak, reverse, omegaPreF, omegaPreR)
      postM(k) = new SSOR(Ak, reverse, omegaPostF, omegaPostR)
      preM(k).setMatrix(Ak)
      postM(k).setMatrix(Ak)
    }
  }

  /**
   * Performs a multigrid cycle
   *
   * @param k
   *            Level to cycle at. Start by calling <code>cycle(0)</code>
   */
  private def cycle(k: Int) {
    if (k == m - 1) directSolve() else {
      preRelax(k)
      u(k + 1).assign(0)
      A(k).zMult(u(k), r(k).assign(f(k)), -1, 1, false)
      I(k).zMult(r(k), f(k + 1), 1, 0, true)
      for (i <- 0 until gamma) cycle(k + 1)
      I(k).zMult(u(k + 1), u(k), 1, 1, false)
      postRelax(k)
    }
  }

  /**
   * Solves directly at the coarsest level
   */
  private def directSolve() {
    val k = m - 1
    u(k).assign(f(k))
    if (transpose) {
      lu.setLU(lu.getLU.viewDice())
      lu.solve(u(k))
      lu.setLU(lu.getLU.viewDice())
    } else lu.solve(u(k))
  }

  /**
   * Applies the relaxation scheme at the given level
   *
   * @param k
   *            Multigrid level
   */
  private def preRelax(k: Int) {
    for (i <- 0 until nu1) if (transpose) preM(k).transApply(f(k), u(k)) else preM(k).apply(f(k), u(k))
  }

  /**
   * Applies the relaxation scheme at the given level
   *
   * @param k
   *            Multigrid level
   */
  private def postRelax(k: Int) {
    for (i <- 0 until nu2) if (transpose) postM(k).transApply(f(k), u(k)) else postM(k).apply(f(k), u(k))
  }

  private class SSOR(val F: SparseRCDoubleMatrix2D,
      val reverse: Boolean,
      omegaF: Double,
      omegaR: Double) extends DoublePreconditioner {

    /**
     * Overrelaxation parameter for the forward sweep
     */
    private var omegaF: Double = _

    /**
     * Overrelaxation parameter for the backwards sweep
     */
    private var omegaR: Double = _

    /**
     * indexes to the diagonal entries of the matrix
     */
    private val diagind = new Array[Int](n)

    /**
     * Temporary vector for holding the half-step state
     */
    private val xx = new Array[Double](n)

    if (F.rows() != F.columns()) throw new IllegalArgumentException("SSOR only applies to square matrices")

    setOmega(omegaF, omegaR)

    val n = F.rows()

    /**
     * Constructor for SSOR. Uses <code>omega=1</code> with a backwards
     * sweep
     *
     * @param F
     *            Matrix to use internally. It will not be modified, thus
     *            the system matrix may be passed
     */
    def this(F: SparseRCDoubleMatrix2D) {
      this(F, true, 1, 1)
    }

    /**
     * Sets the overrelaxation parameters
     *
     * @param omegaF
     *            Overrelaxation parameter for the forward sweep. Between 0
     *            and 2.
     * @param omegaR
     *            Overrelaxation parameter for the backwards sweep. Between
     *            0 and 2.
     */
    def setOmega(omegaF: Double, omegaR: Double) {
      if (omegaF < 0 || omegaF > 2) throw new IllegalArgumentException("omegaF must be between 0 and 2")
      if (omegaR < 0 || omegaR > 2) throw new IllegalArgumentException("omegaR must be between 0 and 2")
      this.omegaF = omegaF
      this.omegaR = omegaR
    }

    def setMatrix(A: StrideMatrix2D) {
      F.assign(A)
      val n = F.rows()
      val rowptr = F.getRowPointers
      val colind = F.getColumnIndexes
      for (k <- 0 until n) {
        diagind(k) = cern.colt.Sorting.binarySearchFromTo(colind, k, rowptr(k), rowptr(k + 1) - 1)
        if (diagind(k) < 0) throw new RuntimeException("Missing diagonal on row " + (k + 1))
      }
    }

    def apply(b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = {
      if (!(b.isInstanceOf[DenseMatrix1D]) || !(x.isInstanceOf[DenseMatrix1D])) throw new IllegalArgumentException("b and x must be a DenseDoubleMatrix1D")
      val rowptr = F.getRowPointers
      val colind = F.getColumnIndexes
      val data = F.getValues
      val bd = b.asInstanceOf[DenseMatrix1D].elements()
      val xd = x.asInstanceOf[DenseMatrix1D].elements()
      val n = F.rows()
      System.arraycopy(xd, 0, xx, 0, n)
      for (i <- 0 until n) {
        var sigma = 0
        for (j <- rowptr(i) until diagind(i)) sigma += data(j) * xx(colind(j))
        for (j <- diagind(i) + 1 until rowptr(i + 1)) sigma += data(j) * xd(colind(j))
        sigma = (bd(i) - sigma) / data(diagind(i))
        xx(i) = xd(i) + omegaF * (sigma - xd(i))
      }
      if (!reverse) {
        System.arraycopy(xx, 0, xd, 0, n)
        return x
      }
      var i = n - 1
      while (i >= 0) {
        var sigma = 0
        for (j <- rowptr(i) until diagind(i)) sigma += data(j) * xx(colind(j))
        for (j <- diagind(i) + 1 until rowptr(i + 1)) sigma += data(j) * xd(colind(j))
        sigma = (bd(i) - sigma) / data(diagind(i))
        xd(i) = xx(i) + omegaR * (sigma - xx(i))
        i
      }
      x.assign(xd)
      x
    }

    def transApply(b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = apply(b, x)
  }
}
