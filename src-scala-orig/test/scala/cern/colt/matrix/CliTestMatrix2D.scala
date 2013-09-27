package cern.colt.matrix

import cern.colt.function.tdouble.Function1
import cern.colt.list.tint.IntArrayList
import cern.colt.map.tdouble.AbstractIntDoubleMap
import cern.colt.map.tdouble.OpenIntDoubleHashMap
import cern.colt.matrix.tdouble.DoubleFactory2D
import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.DoubleMatrix3D
import cern.colt.matrix.tdouble.algo.DenseDoubleAlgebra
import cern.colt.matrix.tdouble.algo.DoubleBlas
import cern.colt.matrix.tdouble.algo.DoubleMatrix2DComparator
import cern.colt.matrix.tdouble.algo.SmpDoubleBlas
import cern.colt.matrix.tdouble.algo.decomposition.DenseDoubleLUDecompositionQuick
import cern.colt.matrix.tdouble.impl.DenseMatrix1D
import cern.colt.matrix.tdouble.impl.DenseMatrix2D
import cern.colt.matrix.tdouble.impl.SparseDoubleMatrix2D
import cern.colt.matrix.tdouble.impl.WrapperMatrix1D
import cern.jet.math.tdouble.DoubleFunctions
//remove if not needed
import scala.collection.JavaConversions._

object CliTestMatrix2D {

  private val F = cern.jet.math.tdouble.DoubleFunctions.functions

  private val Factory2D = cern.colt.matrix.tdouble.DoubleFactory2D.dense

  private val Factory1D = cern.colt.matrix.tdouble.DoubleFactory1D.dense

  private val LinearAlgebra = cern.colt.matrix.tdouble.algo.DenseDoubleAlgebra.DEFAULT

  private val Property = cern.colt.matrix.tdouble.algo.DoubleProperty.DEFAULT

  /**
   */
  def doubleTest() {
    val rows = 4
    val columns = 5
    val master = new DenseMatrix2D(rows, columns)
    println(master)
    master.assign(1)
    println("\n" + master)
    master.viewPart(2, 1, 2, 3).assign(2)
    println("\n" + master)
    val copyPart = master.viewPart(2, 1, 2, 3).copy()
    copyPart.assign(3)
    copyPart.set(0, 0, 4)
    println("\n" + copyPart)
    println("\n" + master)
    val view1 = master.viewPart(0, 3, 4, 2)
    val view2 = view1.viewPart(0, 0, 4, 1)
    println("\n" + view1)
    println("\n" + view2)
  }

  /**
   */
  def doubleTest(rows: Int,
      columns: Int,
      initialCapacity: Int,
      minLoadFactor: Double,
      maxLoadFactor: Double) {
    val matrix = new SparseDoubleMatrix2D(rows, columns, initialCapacity, minLoadFactor, maxLoadFactor)
    println(matrix)
    println("adding...")
    var i = 0
    for (column <- 0 until columns; row <- 0 until rows) {
      matrix.set(row, column, i)
      i += 1
    }
    println(matrix)
    println("removing...")
    for (column <- 0 until columns; row <- 0 until rows) {
      matrix.set(row, column, 0)
    }
    println(matrix)
    println("bye bye.")
  }

  /**
   */
  def doubleTest10() {
    val rows = 6
    val columns = 7
    val master = Factory2D.ascending(rows, columns)
    master.assign(DoubleFunctions.mult(Math.sin(0.3)))
    println("\n" + master)
    val rowIndexes = Array(0, 1, 2, 3)
    val columnIndexes = Array(0, 1, 2, 3)
    val rowIndexes2 = Array(3, 0, 3)
    val columnIndexes2 = Array(3, 0, 3)
    var view1 = master.viewPart(1, 1, 4, 5).viewSelection(rowIndexes, columnIndexes)
    println("\nview1=" + view1)
    val view9 = view1.viewStrides(2, 2).viewStrides(2, 1)
    println("\nview9=" + view9)
    view1 = view1.viewSelection(rowIndexes2, columnIndexes2)
    println("\nview1=" + view1)
    val view2 = view1.viewPart(1, 1, 2, 2)
    println("\nview2=" + view2)
    val view3 = view2.viewRowFlip()
    println("\nview3=" + view3)
    view3.assign(Factory2D.ascending(view3.rows(), view3.columns()))
    println("\nview3=" + view3)
    println("\nmaster replaced" + master)
    println("\nview1 replaced" + view1)
    println("\nview2 replaced" + view2)
    println("\nview3 replaced" + view3)
  }

  /**
   */
  def doubleTest11() {
    val rows = 4
    val columns = 5
    val master = new DenseMatrix2D(1, 1)
    master.assign(2)
    println("\n" + master)
    val rowIndexes = Array.ofDim[Int](rows)
    val columnIndexes = Array.ofDim[Int](columns)
    val view1 = master.viewSelection(rowIndexes, columnIndexes)
    println(view1)
    master.assign(1)
    println("\n" + master)
    println(view1)
  }

  /**
   */
  def doubleTest12() {
    var A: StrideMatrix2D = null
    var B: StrideMatrix2D = null
    var C: StrideMatrix2D = null
    var D: StrideMatrix2D = null
    var E: StrideMatrix2D = null
    var F: StrideMatrix2D = null
    var G: StrideMatrix2D = null
    var H: StrideMatrix2D = null
    var I: StrideMatrix2D = null
    var J: StrideMatrix2D = null
    A = Factory2D.make(2, 3, 9)
    B = Factory2D.make(4, 3, 8)
    C = Factory2D.appendRows(A, B)
    println("\nA=" + A)
    println("\nB=" + B)
    println("\nC=" + C)
    D = Factory2D.make(3, 2, 7)
    E = Factory2D.make(3, 4, 6)
    F = Factory2D.appendColumns(D, E)
    println("\nD=" + D)
    println("\nE=" + E)
    println("\nF=" + F)
    G = Factory2D.appendRows(C, F)
    println("\nG=" + G)
    H = Factory2D.ascending(2, 3)
    println("\nH=" + H)
    I = Factory2D.repeat(H, 2, 3)
    println("\nI=" + I)
  }

  /**
   */
  def doubleTest13() {
    val values = Array(0, 1, 2, 3)
    val matrix = new DenseMatrix1D(values)
    println(matrix)
    println(matrix.viewSelection(new cern.colt.function.tdouble.Procedure1() {

      def apply(a: Double): Boolean = return a % 2 == 0
    }))
    println(matrix.aggregate(F.plus, F.square))
    println(matrix.aggregate(F.plus, F.pow(3)))
    println(matrix.aggregate(F.plus, F.identity))
    println(matrix.aggregate(F.min, F.identity))
    println(matrix.aggregate(F.max, F.chain(F.div(2), F.sqrt)))
    println(matrix.aggregate(F.plus, F.between(0, 2)))
    println(matrix.aggregate(F.plus, F.chain(F.between(0.8, 1.2), F.log2)))
    println(matrix.aggregate(F.mult, F.identity))
    val limit = 1
    val f = new Function1() {

      def apply(a: Double): Double = return if (a > limit) a else 1
    }
    println(matrix.aggregate(F.mult, f))
    var otherMatrix1D = matrix.copy()
    println(matrix.aggregate(otherMatrix1D, F.plus, F.chain(F.square, F.plus)))
    matrix.assign(F.plus(1))
    otherMatrix1D = matrix.copy()
    println(matrix)
    println(otherMatrix1D)
    println(matrix.aggregate(otherMatrix1D, F.plus, F.chain(F.mult(Math.PI), F.chain(F.log, F.swapArgs(F.div)))))
    println(matrix.aggregate(otherMatrix1D, F.plus, new DoubleDoubleFunction() {

      def apply(a: Double, b: Double): Double = return Math.PI * Math.log(b / a)
    }))
    val x = cern.colt.matrix.tdouble.DoubleFactory3D.dense.ascending(2, 2, 2)
    println(x)
    println(x.aggregate(F.plus, F.square))
    val y = x.copy()
    println(x.aggregate(y, F.plus, F.chain(F.square, F.plus)))
    println(matrix.assign(F.random()))
    println(matrix.assign(new cern.jet.random.tdouble.Poisson(5, cern.jet.random.tdouble.Poisson.makeDefaultGenerator())))
  }

  /**
   */
  def doubleTest14(r1: Int, c: Int, r2: Int) {
    val values = Array(0, 1, 2, 3)
    val a = DoubleFactory2D.dense.ascending(r1, c)
    val b = DoubleFactory2D.dense.ascending(c, r2).assign(DoubleFunctions.mult(-1))
    a.assign(0)
    b.assign(0)
    val timer = new cern.colt.Timer().start()
    LinearAlgebra.mult(a, b)
    timer.stop().display()
  }

  /**
   */
  def doubleTest15(size: Int, runs: Int) {
    println("\n\n")
    val values = Array(Array(0, 5, 9), Array(2, 6, 10), Array(3, 7, 11))
    val A = Factory2D.make(size, size)
    val value = 5
    var i = size
    while (i >= 0) {
      A.setQuick(i, i, value)
    }
    A.viewRow(0).assign(value)
    val timer = new cern.colt.Timer().start()
    var inv: StrideMatrix2D = null
    for (run <- 0 until runs) {
      inv = LinearAlgebra.inverse(A)
    }
    timer.stop().display()
  }

  /**
   */
  def doubleTest17(size: Int) {
    println("\n\n")
    val A = Factory2D.ascending(3, 4)
    val B = Factory2D.ascending(2, 3)
    val C = Factory2D.ascending(1, 2)
    B.assign(F.plus(A.zSum()))
    C.assign(F.plus(B.zSum()))
  }

  /**
   */
  def doubleTest18(size: Int) {
    println("\n\n")
    val s = 2
    var A00: StrideMatrix2D = null
    var A01: StrideMatrix2D = null
    var A02: StrideMatrix2D = null
    var A10: StrideMatrix2D = null
    var A11: StrideMatrix2D = null
    var A12: StrideMatrix2D = null
    var A20: StrideMatrix2D = null
    var A21: StrideMatrix2D = null
    var A22: StrideMatrix2D = null
    var empty: StrideMatrix2D = null
    empty = Factory2D.make(0, 0)
    A00 = Factory2D.ascending(s, s)
    A01 = Factory2D.ascending(s, s).assign(F.plus(A00.getQuick(s - 1, s - 1)))
    A02 = Factory2D.ascending(s, s).assign(F.plus(A01.getQuick(s - 1, s - 1)))
    A10 = Factory2D.ascending(s, s).assign(F.plus(A02.getQuick(s - 1, s - 1)))
    A11 = null
    A12 = Factory2D.ascending(s, s).assign(F.plus(A10.getQuick(s - 1, s - 1)))
    A20 = Factory2D.ascending(s, s).assign(F.plus(A12.getQuick(s - 1, s - 1)))
    A21 = empty
    A22 = Factory2D.ascending(s, s).assign(F.plus(A20.getQuick(s - 1, s - 1)))
    println("\n" + A00)
    println("\n" + A01)
    println("\n" + A02)
    println("\n" + A10)
    println("\n" + A11)
    println("\n" + A12)
    println("\n" + A20)
    println("\n" + A21)
    println("\n" + A22)
  }

  /**
   */
  def doubleTest19() {
    println("\n\n")
    var A: StrideMatrix2D = null
    var k: Int = 0
    var uk: Int = 0
    var lk: Int = 0
    val values5 = Array(Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(0, 0, 0, 0))
    A = Factory2D.make(values5)
    k = cern.colt.matrix.tdouble.algo.DoubleProperty.DEFAULT
      .semiBandwidth(A)
    uk = cern.colt.matrix.tdouble.algo.DoubleProperty.DEFAULT
      .upperBandwidth(A)
    lk = cern.colt.matrix.tdouble.algo.DoubleProperty.DEFAULT
      .lowerBandwidth(A)
    println("\n\nupperBandwidth=" + uk)
    println("lowerBandwidth=" + lk)
    println("bandwidth=" + k + " " + A)
    val values4 = Array(Array(1, 0, 0, 0), Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(0, 0, 0, 1))
    A = Factory2D.make(values4)
    k = cern.colt.matrix.tdouble.algo.DoubleProperty.DEFAULT
      .semiBandwidth(A)
    uk = cern.colt.matrix.tdouble.algo.DoubleProperty.DEFAULT
      .upperBandwidth(A)
    lk = cern.colt.matrix.tdouble.algo.DoubleProperty.DEFAULT
      .lowerBandwidth(A)
    println("\n\nupperBandwidth=" + uk)
    println("lowerBandwidth=" + lk)
    println("bandwidth=" + k + " " + A)
    val values1 = Array(Array(1, 1, 0, 0), Array(1, 1, 1, 0), Array(0, 1, 1, 1), Array(0, 0, 1, 1))
    A = Factory2D.make(values1)
    k = cern.colt.matrix.tdouble.algo.DoubleProperty.DEFAULT
      .semiBandwidth(A)
    uk = cern.colt.matrix.tdouble.algo.DoubleProperty.DEFAULT
      .upperBandwidth(A)
    lk = cern.colt.matrix.tdouble.algo.DoubleProperty.DEFAULT
      .lowerBandwidth(A)
    println("\n\nupperBandwidth=" + uk)
    println("lowerBandwidth=" + lk)
    println("bandwidth=" + k + " " + A)
    val values6 = Array(Array(0, 1, 1, 1), Array(0, 1, 1, 1), Array(0, 0, 0, 1), Array(0, 0, 0, 1))
    A = Factory2D.make(values6)
    k = cern.colt.matrix.tdouble.algo.DoubleProperty.DEFAULT
      .semiBandwidth(A)
    uk = cern.colt.matrix.tdouble.algo.DoubleProperty.DEFAULT
      .upperBandwidth(A)
    lk = cern.colt.matrix.tdouble.algo.DoubleProperty.DEFAULT
      .lowerBandwidth(A)
    println("\n\nupperBandwidth=" + uk)
    println("lowerBandwidth=" + lk)
    println("bandwidth=" + k + " " + A)
    val values7 = Array(Array(0, 0, 0, 0), Array(1, 1, 0, 0), Array(1, 1, 0, 0), Array(1, 1, 1, 1))
    A = Factory2D.make(values7)
    k = cern.colt.matrix.tdouble.algo.DoubleProperty.DEFAULT
      .semiBandwidth(A)
    uk = cern.colt.matrix.tdouble.algo.DoubleProperty.DEFAULT
      .upperBandwidth(A)
    lk = cern.colt.matrix.tdouble.algo.DoubleProperty.DEFAULT
      .lowerBandwidth(A)
    println("\n\nupperBandwidth=" + uk)
    println("lowerBandwidth=" + lk)
    println("bandwidth=" + k + " " + A)
    val values2 = Array(Array(1, 1, 0, 0), Array(0, 1, 1, 0), Array(0, 1, 0, 1), Array(1, 0, 1, 1))
    A = Factory2D.make(values2)
    k = cern.colt.matrix.tdouble.algo.DoubleProperty.DEFAULT
      .semiBandwidth(A)
    uk = cern.colt.matrix.tdouble.algo.DoubleProperty.DEFAULT
      .upperBandwidth(A)
    lk = cern.colt.matrix.tdouble.algo.DoubleProperty.DEFAULT
      .lowerBandwidth(A)
    println("\n\nupperBandwidth=" + uk)
    println("lowerBandwidth=" + lk)
    println("bandwidth=" + k + " " + A)
    val values3 = Array(Array(1, 1, 1, 0), Array(0, 1, 0, 0), Array(1, 1, 0, 1), Array(0, 0, 1, 1))
    A = Factory2D.make(values3)
    k = cern.colt.matrix.tdouble.algo.DoubleProperty.DEFAULT
      .semiBandwidth(A)
    uk = cern.colt.matrix.tdouble.algo.DoubleProperty.DEFAULT
      .upperBandwidth(A)
    lk = cern.colt.matrix.tdouble.algo.DoubleProperty.DEFAULT
      .lowerBandwidth(A)
    println("\n\nupperBandwidth=" + uk)
    println("lowerBandwidth=" + lk)
    println("bandwidth=" + k + " " + A)
  }

  /**
   */
  def doubleTest19(size: Int) {
    println("\n\n")
    val s = 2
    var A00: StrideMatrix2D = null
    var A01: StrideMatrix2D = null
    var A02: StrideMatrix2D = null
    var A10: StrideMatrix2D = null
    var A11: StrideMatrix2D = null
    var A12: StrideMatrix2D = null
    var A20: StrideMatrix2D = null
    var A21: StrideMatrix2D = null
    var A22: StrideMatrix2D = null
    var empty: StrideMatrix2D = null
    empty = Factory2D.make(0, 0)
    A00 = Factory2D.ascending(s, s)
    A01 = Factory2D.ascending(s, s).assign(F.plus(A00.getQuick(s - 1, s - 1)))
    A02 = Factory2D.ascending(s, s).assign(F.plus(A01.getQuick(s - 1, s - 1)))
    A10 = Factory2D.ascending(s, s).assign(F.plus(A02.getQuick(s - 1, s - 1)))
    A11 = null
    A12 = Factory2D.ascending(s, s).assign(F.plus(A10.getQuick(s - 1, s - 1)))
    A20 = Factory2D.ascending(s, s).assign(F.plus(A12.getQuick(s - 1, s - 1)))
    A21 = empty
    A22 = Factory2D.ascending(s, s).assign(F.plus(A20.getQuick(s - 1, s - 1)))
    println("\n" + A00)
    println("\n" + A01)
    println("\n" + A02)
    println("\n" + A10)
    println("\n" + A11)
    println("\n" + A12)
    println("\n" + A20)
    println("\n" + A21)
    println("\n" + A22)
  }

  /**
   */
  def doubleTest2() {
    val keys = Array(0, 3, 100000, 9)
    val values = Array(100.0, 1000.0, 70.0, 71.0)
    val size = keys.length
    val map = new OpenIntDoubleHashMap(size * 2, 0.2, 0.5)
    for (i <- 0 until keys.length) {
      map.put(keys(i), values(i).toInt)
    }
    println(map.containsKey(3))
    println(map.get(3))
    println(map.containsKey(4))
    println(map.get(4))
    println(map.containsValue(71.0.toInt))
    println(map.keyOf(71.0.toInt))
    println(map)
  }

  /**
   */
  def doubleTest20() {
    println("\n\n")
    var A: StrideMatrix2D = null
    val k: Int = 0
    val uk: Int = 0
    val lk: Int = 0
    val values1 = Array(Array(0, 1, 0, 0), Array(3, 0, 2, 0), Array(0, 2, 0, 3), Array(0, 0, 1, 0))
    A = Factory2D.make(values1)
    println("\n\n" + LinearAlgebra.toVerboseString(A))
    val values2 = Array(Array(1.0000000000000167, -0.3623577544766736, -0.3623577544766736), Array(0, 0.9320390859672374, -0.3377315902755755), Array(0, 0, 0.8686968577706282), Array(0, 0, 0), Array(0, 0, 0))
    A = Factory2D.make(values2)
    println("\n\n" + LinearAlgebra.toVerboseString(A))
    val values3 = Array(Array(611, 196, -192, 407, -8, -52, -49, 29), Array(196, 899, 113, -192, -71, -43, -8, -44), Array(-192, 113, 899, 196, 61, 49, 8, 52), Array(407, -192, 196, 611, 8, 44, 59, -23), Array(-8, -71, 61, 8, 411, -599, 208, 208), Array(-52, -43, 49, 44, -599, 411, 208, 208), Array(-49, -8, 8, 59, 208, 208, 99, -911), Array(29, -44, 52, -23, 208, 208, -911, 99))
    A = Factory2D.make(values3)
    println("\n\n" + LinearAlgebra.toVerboseString(A))
    val a = Math.sqrt(10405)
    val b = Math.sqrt(26)
    val e = Array(-10 * a, 0, 510 - 100 * b, 1000, 1000, 510 + 100 * b, 1020, 10 * a)
    println(Factory1D.dense.make(e))
  }

  /**
   */
  def doubleTest21() {
    println("\n\n")
    var A: StrideMatrix2D = null
    val k: Int = 0
    val uk: Int = 0
    val lk: Int = 0
    val values1 = Array(Array(1 / 3, 2 / 3, Math.PI, 0), Array(3, 9, 0, 0), Array(0, 2, 7, 0), Array(0, 0, 3, 9))
    A = Factory2D.make(values1)
    println(A)
    println(new cern.colt.matrix.tdouble.algo.DoubleFormatter(null)
       toString A)
  }

  /**
   */
  def doubleTest22() {
    println("\n\n")
    var A: StrideMatrix2D = null
    val k: Int = 0
    val uk: Int = 0
    val lk: Int = 0
    val values1 = Array(Array(1 / 3, 2 / 3, Math.PI, 0), Array(3, 9, 0, 0), Array(0, 2, 7, 0), Array(0, 0, 3, 9))
    A = Factory2D.make(values1)
    println(A)
    println(Property.isDiagonallyDominantByRow(A))
    println(Property.isDiagonallyDominantByColumn(A))
    Property.generateNonSingular(A)
    println(A)
    println(Property.isDiagonallyDominantByRow(A))
    println(Property.isDiagonallyDominantByColumn(A))
  }

  /**
   */
  def doubleTest23(runs: Int,
      size: Int,
      nonZeroFraction: Double,
      dense: Boolean) {
    println("\n\n")
    println("initializing...")
    var A: StrideMatrix2D = null
    var LU: StrideMatrix2D = null
    var I: StrideMatrix2D = null
    var Inv: StrideMatrix2D = null
    var b: StrideMatrix1D = null
    var solved: StrideMatrix1D = null
    val mean = 5.0
    val stdDev = 3.0
    val random = new cern.jet.random.tdouble.Normal(mean, stdDev, new cern.jet.random.tdouble.engine.DoubleMersenneTwister())
    println("sampling...")
    val value = 2
    A = if (dense) Factory2D.dense.sample(size, size, value, nonZeroFraction) else Factory2D.sparse.sample(size,
      size, value, nonZeroFraction)
    b = A.like1D(size).assign(1)
    println("generating invertible matrix...")
    Property.generateNonSingular(A)
    LU = A.like()
    solved = b.like()
    val lu = new DenseDoubleLUDecompositionQuick()
    println("benchmarking assignment...")
    val timer = new cern.colt.Timer().start()
    LU.assign(A)
    solved.assign(b)
    timer.stop().display()
    LU.assign(A)
    lu.decompose(LU)
    println("benchmarking LU...")
    timer.reset().start()
    var i = runs
    while (i >= 0) {
      solved.assign(b)
      lu.solve(solved)
    }
    timer.stop().display()
    println("done.")
  }

  /**
   */
  def doubleTest24(runs: Int, size: Int, dense: Boolean) {
    println("\n\n")
    println("initializing...")
    var A: StrideMatrix2D = null
    var factory: DoubleFactory2D = null
    factory = if (dense) Factory2D.dense else Factory2D.sparse
    val value = 2
    val omega = 1.25
    val alpha = omega * 0.25
    val beta = 1 - omega
    A = factory.make(size, size, value)
    val function = new cern.colt.function.tdouble.Double9Function() {

      def apply(a00: Double,
          a01: Double,
          a02: Double,
          a10: Double,
          a11: Double,
          a12: Double,
          a20: Double,
          a21: Double,
          a22: Double): Double = {
        return alpha * a11 + beta * (a01 + a10 + a12 + a21)
      }
    }
    val timer = new cern.colt.Timer().start()
    println("benchmarking stencil...")
    for (i <- 0 until runs) {
      A.zAssign8Neighbors(A, function)
    }
    timer.stop().display()
    A = null
    val B = factory.make(size, size, value).toArray()
    timer.reset().start()
    println("benchmarking stencil scimark...")
    for (i <- 0 until runs) {
    }
    timer.stop().display()
    println("done.")
  }

  /**
   */
  def doubleTest25(size: Int) {
    println("\n\n")
    println("initializing...")
    val dense = true
    var A: StrideMatrix2D = null
    var factory: DoubleFactory2D = null
    factory = if (dense) Factory2D.dense else Factory2D.sparse
    val value = 0.5
    A = factory.make(size, size, value)
    Property.generateNonSingular(A)
    val timer = new cern.colt.Timer().start()
    println(A)
    println(DenseDoubleAlgebra.ZERO.inverse(A))
    timer.stop().display()
    println("done.")
  }

  /**
   */
  def doubleTest26(size: Int) {
    println("\n\n")
    println("initializing...")
    val dense = true
    var A: StrideMatrix2D = null
    var factory: DoubleFactory2D = null
    factory = if (dense) Factory2D.dense else Factory2D.sparse
    val value = 0.5
    A = factory.make(size, size, value)
    Property.generateNonSingular(A)
    val timer = new cern.colt.Timer().start()
    val fun = new DoubleMatrix2DComparator() {

      def compare(a: StrideMatrix2D, b: StrideMatrix2D): Int = {
        return if (a.zSum() == b.zSum()) 1 else 0
      }
    }
    println(A)
    println(DenseDoubleAlgebra.ZERO.inverse(A))
    timer.stop().display()
    println("done.")
  }

  /**
   */
  def doubleTest27() {
    println("\n\n")
    println("initializing...")
    val rows = 51
    val columns = 10
    val trainingSet = Array.ofDim[Double](columns, rows)
    var i = columns
    while (i >= 0) trainingSet(i)(i) = 2.0
    var patternIndex = 0
    var unitIndex = 0
    var patternMatrix: StrideMatrix2D = null
    var transposeMatrix: StrideMatrix2D = null
    var QMatrix: StrideMatrix2D = null
    var inverseQMatrix: StrideMatrix2D = null
    var pseudoInverseMatrix: StrideMatrix2D = null
    var weightMatrix: StrideMatrix2D = null
    patternMatrix = DoubleFactory2D.dense.make(rows, columns)
    patternIndex = 0
    while (patternIndex < columns) {
      unitIndex = 0
      while (unitIndex < rows) {
        patternMatrix.setQuick(unitIndex, patternIndex, trainingSet(patternIndex)(unitIndex))
        unitIndex += 1
      }
      patternIndex += 1
    }
    transposeMatrix = DenseDoubleAlgebra.DEFAULT.transpose(patternMatrix)
    QMatrix = DenseDoubleAlgebra.DEFAULT.mult(transposeMatrix, patternMatrix)
    inverseQMatrix = DenseDoubleAlgebra.DEFAULT.inverse(QMatrix)
    pseudoInverseMatrix = DenseDoubleAlgebra.DEFAULT.mult(inverseQMatrix, transposeMatrix)
    weightMatrix = DenseDoubleAlgebra.DEFAULT.mult(patternMatrix, pseudoInverseMatrix)
    println("done.")
  }

  /**
   */
  def doubleTest28() {
    val data = Array(1, 2, 3, 4, 5, 6)
    val arrMatrix = Array(Array(1, 2, 3, 4, 5, 6), Array(2, 3, 4, 5, 6, 7))
    val f = DoubleFactory2D.dense
    val vector = new DenseMatrix1D(data)
    val matrix = f.make(arrMatrix)
    val res = vector.like(matrix.rows())
    matrix.zMult(vector, res)
    println(res)
  }

  /**
   */
  def doubleTest28(f: DoubleFactory2D) {
    val data = Array(1, 2, 3, 4, 5, 6)
    val arrMatrix = Array(Array(1, 2, 3, 4, 5, 6), Array(2, 3, 4, 5, 6, 7))
    val vector = new DenseMatrix1D(data)
    val matrix = f.make(arrMatrix)
    val res = vector.like(matrix.rows())
    matrix.zMult(vector, res)
    println(res)
  }

  /**
   */
  def doubleTest29(size: Int) {
  }

  /**
   */
  def doubleTest29(size: Int, f: DoubleFactory2D) {
    val x = new DenseMatrix2D(size, size).assign(0.5)
    val matrix = f.sample(size, size, 0.5, 0.001)
    val timer = new cern.colt.Timer().start()
    val res = matrix.zMult(x, null)
    timer.stop().display()
  }

  /**
   */
  def doubleTest29(f: DoubleFactory2D) {
    val data = Array(Array(6, 5, 4), Array(7, 6, 3), Array(6, 5, 4), Array(7, 6, 3), Array(6, 5, 4), Array(7, 6, 3))
    val arrMatrix = Array(Array(1, 2, 3, 4, 5, 6), Array(2, 3, 4, 5, 6, 7))
    val x = new DenseMatrix2D(data)
    val matrix = f.make(arrMatrix)
    val res = matrix.zMult(x, null)
    println(res)
  }

  /**
   */
  def doubleTest3() {
    val rows = 4
    val columns = 5
    val master = new DenseMatrix2D(rows, columns)
    println(master)
    master.assign(1)
    println("\n" + master)
    master.viewPart(2, 0, 2, 3).assign(2)
    println("\n" + master)
    val flip1 = master.viewColumnFlip()
    println("flip around columns=" + flip1)
    val flip2 = flip1.viewRowFlip()
    println("further flip around rows=" + flip2)
    flip2.viewPart(0, 0, 2, 2).assign(3)
    println("master replaced" + master)
    println("flip1 replaced" + flip1)
    println("flip2 replaced" + flip2)
  }

  /**
   */
  def doubleTest30() {
    val data = Array(Array(6, 5), Array(7, 6))
    val x = Array(1, 2)
    val y = Array(3, 4)
    val A = new DenseMatrix2D(data)
    val seqBlas = new SmpDoubleBlas()
    seqBlas.dger(1, new DenseMatrix1D(x), new DenseMatrix1D(y), A)
    println(A)
  }

  /**
   */
  def doubleTest30(size: Int) {
    val values = Array(0, 2, 3, 5, 7)
    val list = new IntArrayList(values)
    val `val` = 3
    var sum = 0
    val timer = new cern.colt.Timer().start()
    var i = size
    while (i >= 0) {
      val k = list.binarySearchFromTo(`val`, 0, values.length - 1)
      println(list + ", " + `val` + " --> " + k)
      sum += k
    }
    timer.stop().display()
  }

  /**
   */
  def doubleTest30(size: Int, `val`: Int) {
    val values = Array(2)
    val list = new IntArrayList(values)
    val l = values.length - 1
    var sum = 0
    val timer = new cern.colt.Timer().start()
    var i = size
    while (i >= 0) {
      val k = cern.colt.Sorting.binarySearchFromTo(values, `val`, 0, l)
      sum += k
    }
    timer.stop().display()
    println("sum = " + sum)
  }

  /**
   */
  def doubleTest31(size: Int) {
    println("\ninit")
    val a = Factory1D.dense.descending(size)
    var b = new WrapperMatrix1D(a)
    val c = b.viewPart(2, 3)
    val d = c.viewFlip()
    d.set(0, 99)
    b = b.viewSorted()
    println("a = " + a)
    println("b = " + b)
    println("c = " + c)
    println("d = " + d)
    println("done")
  }

  /**
   */
  def doubleTest33() {
    val nan = Double.NaN
    val inf = Double.POSITIVE_INFINITY
    val ninf = Double.NEGATIVE_INFINITY
    val data = Array(Array(ninf, nan))
    val x = new DenseMatrix2D(data)
    println("\n\n\n" + x)
    println("\n" + x == ninf)
  }

  /**
   */
  def doubleTest34() {
    val data = Array(Array(3, 0, 0, 0), Array(0, 4, 2, 0), Array(0, 0, 0, 0), Array(0, 0, 0, 0))
    val A = new DenseMatrix2D(data)
    Property.DEFAULT.generateNonSingular(A)
    val inv = DenseDoubleAlgebra.DEFAULT.inverse(A)
    println("\n\n\n" + A)
    println("\n" + inv)
    val B = A.zMult(inv, null)
    println(B)
    if (!(B == DoubleFactory2D.dense.identity(A.rows()))) {
      throw new InternalError()
    }
  }

  /**
   * Title: Aero3D
   * <p>
   * Description: A Program to analyse aeroelestic evects in transonic wings
   * <p>
   * Copyright: Copyright (c) 1998
   * <p>
   * Company: PIERSOL Engineering Inc.
   * <p>
   *
   * @author John R. Piersol
   * @version
   */
  def doubleTest35() {
  }

  /**
   * Title: Aero3D
   * <p>
   * Description: A Program to analyse aeroelestic evects in transonic wings
   * <p>
   * Copyright: Copyright (c) 1998
   * <p>
   * Company: PIERSOL Engineering Inc.
   * <p>
   *
   * @author John R. Piersol
   * @version
   */
  def doubleTest36() {
    val testSort = Array.ofDim[Double](5)
    testSort(0) = 5
    testSort(1) = Double.NaN
    testSort(2) = 2
    testSort(3) = Double.NaN
    testSort(4) = 1
    var doubleDense = new DenseMatrix1D(testSort)
    println("orig = " + doubleDense)
    doubleDense = doubleDense.viewSorted()
    doubleDense.toArray(testSort)
    println("sort = " + doubleDense)
    println("done\n")
  }

  /**
   */
  def doubleTest4() {
    val rows = 4
    val columns = 5
    val master = new DenseMatrix2D(rows, columns)
    println(master)
    master.assign(1)
    val view = master.viewPart(2, 0, 2, 3).assign(2)
    println("\n" + master)
    println("\n" + view)
    view.assign(DoubleFunctions.mult(3))
    println("\n" + master)
    println("\n" + view)
  }

  /**
   */
  def doubleTest5() {
  }

  /**
   */
  def doubleTest6() {
    val rows = 4
    val columns = 5
    val master = Factory2D.ascending(rows, columns)
    println("\n" + master)
    master.viewPart(2, 0, 2, 3).assign(2)
    println("\n" + master)
    val indexes = Array(0, 1, 3, 0, 1, 2)
    val view1 = master.viewRow(0).viewSelection(indexes)
    println("view1=" + view1)
    val view2 = view1.viewPart(0, 3)
    println("view2=" + view2)
    view2.viewPart(0, 2).assign(-1)
    println("master replaced" + master)
    println("flip1 replaced" + view1)
    println("flip2 replaced" + view2)
  }

  /**
   */
  def doubleTest7() {
    val rows = 4
    val columns = 5
    val master = Factory2D.ascending(rows, columns)
    println("\n" + master)
    val rowIndexes = Array(0, 1, 3, 0)
    val columnIndexes = Array(0, 2)
    val view1 = master.viewSelection(rowIndexes, columnIndexes)
    println("view1=" + view1)
    val view2 = view1.viewPart(0, 0, 2, 2)
    println("view2=" + view2)
    view2.assign(-1)
    println("master replaced" + master)
    println("flip1 replaced" + view1)
    println("flip2 replaced" + view2)
  }

  /**
   */
  def doubleTest8() {
    val rows = 2
    val columns = 3
    val master = Factory2D.ascending(rows, columns)
    println("\n" + master)
    val view1 = master.viewDice()
    println("view1=" + view1)
    val view2 = view1.viewDice()
    println("view2=" + view2)
    view2.assign(-1)
    println("master replaced" + master)
    println("flip1 replaced" + view1)
    println("flip2 replaced" + view2)
  }

  /**
   */
  def doubleTest9() {
    val rows = 2
    val columns = 3
    val master = Factory2D.ascending(rows, columns)
    println("\n" + master)
    val view1 = master.viewRowFlip()
    println("view1=" + view1)
    val view2 = view1.viewRowFlip()
    println("view2=" + view2)
    view2.assign(-1)
    println("master replaced" + master)
    println("flip1 replaced" + view1)
    println("flip2 replaced" + view2)
  }

  def doubleTestQR() {
    val x0 = Array(-6.221564, -9.002113, 2.678001, 6.483597, -7.934148)
    val y0 = Array(-7.291898, -7.346928, 0.520158, 5.012548, -8.223725)
    val x1 = Array(1.185925, -2.523077, 0.135380, 0.412556, -2.980280)
    val y1 = Array(13.561087, -15.204410, 16.496829, 16.470860, 0.822198)
    solve(x1.length, x1, y1)
    solve(x0.length, x0, y0)
  }

  /**
   */
  def main(args: Array[String]) {
    val runs = Integer.parseInt(args(0))
    val `val` = Integer.parseInt(args(1))
    doubleTest30(runs, `val`)
  }

  def randomMatrix(dof: Int, RANDOM: cern.jet.random.tdouble.engine.DoubleMersenneTwister): Array[Array[Double]] = {
    val m = Array.ofDim[Double](dof, dof)
    for (i <- 0 until dof; j <- 0 until dof) {
      m(i)(j) = 5
    }
    m
  }

  def solve(numpnt: Int, x: Array[Double], y: Array[Double]) {
  }

  /**
   */
  def testLU() {
    val vals = Array(Array(-0.074683, 0.321248, -0.014656, 0.286586, 0), Array(-0.344852, -0.16278, 0.173711, 0.00064, 0), Array(-0.181924, -0.092926, 0.184153, 0.177966, 1), Array(-0.166829, -0.10321, 0.582301, 0.142583, 0), Array(0, -0.112952, -0.04932, -0.700157, 0), Array(0, 0, 0, 0, 0))
    val H = new DenseMatrix2D(vals)
    println("\nHplus=" + H.viewDice().zMult(H, null))
    val Hplus = DenseDoubleAlgebra.DEFAULT.inverse(H.viewDice().zMult(H, null))
      .zMult(H.viewDice(), null)
    Hplus.assign(cern.jet.math.tdouble.DoubleFunctions.round(1.0E-10))
    println("\nHplus=" + Hplus)
  }

  /**
   */
  def testMax() {
    val temp = Array.ofDim[Double](2)
    temp(0) = 8.9
    temp(1) = 1
    val d1Double = new DenseMatrix1D(temp)
    val d1ynamicBin = cern.colt.matrix.tdouble.algo.DoubleStatistic.bin(d1Double)
    val max = d1ynamicBin.max()
    println("max = " + max)
  }
}
