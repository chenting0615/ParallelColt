package cern.colt.matrix.tdouble.impl

import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.DoubleMatrix2DTest
import cern.jet.math.tdouble.DoubleFunctions
//remove if not needed
import scala.collection.JavaConversions._

class SparseDoubleMatrix2DTest(arg0: String) extends DoubleMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseDoubleMatrix2D(NROWS, NCOLUMNS)
    B = new SparseDoubleMatrix2D(NROWS, NCOLUMNS)
    Bt = new SparseDoubleMatrix2D(NCOLUMNS, NROWS)
  }

  def testGetRowCompressed() {
    val SIZE = NROWS * NCOLUMNS
    val rowindexes = Array.ofDim[Int](SIZE)
    val columnindexes = Array.ofDim[Int](SIZE)
    val values = Array.ofDim[Double](SIZE)
    for (i <- 0 until SIZE) {
      rowindexes(i) = Math.abs(random.nextInt() % NROWS).toInt
      columnindexes(i) = Math.abs(random.nextInt() % NCOLUMNS).toInt
      values(i) = Math.random()
    }
    val A = new SparseDoubleMatrix2D(NROWS, NCOLUMNS, rowindexes, columnindexes, values)
    var B = A.getRowCompressed(false)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(A.getQuick(r, c), B.getQuick(r, c))
    }
    B = A.getRowCompressed(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(A.getQuick(r, c), B.getQuick(r, c))
    }
  }

  def testGetRowCompressedModified() {
    val SIZE = A.rows() * A.columns()
    val rowindexes = Array.ofDim[Int](SIZE)
    val columnindexes = Array.ofDim[Int](SIZE)
    val values = Array.ofDim[Double](SIZE)
    for (i <- 0 until SIZE) {
      rowindexes(i) = Math.abs(random.nextInt() % NROWS).toInt
      columnindexes(i) = Math.abs(random.nextInt() % NCOLUMNS).toInt
      values(i) = Math.random()
    }
    val S = new SparseDoubleMatrix2D(A.rows(), A.columns(), rowindexes, columnindexes, values)
    val B = S.getRowCompressedModified
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(S.getQuick(r, c), B.getQuick(r, c))
    }
  }

  def testGetColumnCompressed() {
    val SIZE = A.rows() * A.columns()
    val rowindexes = Array.ofDim[Int](SIZE)
    val columnindexes = Array.ofDim[Int](SIZE)
    val values = Array.ofDim[Double](SIZE)
    for (i <- 0 until SIZE) {
      rowindexes(i) = Math.abs(random.nextInt() % NROWS).toInt
      columnindexes(i) = Math.abs(random.nextInt() % NCOLUMNS).toInt
      values(i) = Math.random()
    }
    val S = new SparseDoubleMatrix2D(A.rows(), A.columns(), rowindexes, columnindexes, values)
    var B = S.getColumnCompressed(false)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(S.getQuick(r, c), B.getQuick(r, c))
    }
    B = S.getColumnCompressed(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(S.getQuick(r, c), B.getQuick(r, c))
    }
  }

  def testGetColumnCompressedModified() {
    val SIZE = A.rows() * A.columns()
    val rowindexes = Array.ofDim[Int](SIZE)
    val columnindexes = Array.ofDim[Int](SIZE)
    val values = Array.ofDim[Double](SIZE)
    for (i <- 0 until SIZE) {
      rowindexes(i) = Math.abs(random.nextInt() % NROWS).toInt
      columnindexes(i) = Math.abs(random.nextInt() % NCOLUMNS).toInt
      values(i) = Math.random()
    }
    val S = new SparseDoubleMatrix2D(A.rows(), A.columns(), rowindexes, columnindexes, values)
    val B = S.getColumnCompressedModified
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(S.getQuick(r, c), B.getQuick(r, c))
    }
  }

  def testAssignIntArrayIntArrayDoubleArrayDoubleDoubleFunction() {
    val SIZE = A.rows() * A.columns()
    val rowindexes = Array.ofDim[Int](SIZE)
    val columnindexes = Array.ofDim[Int](SIZE)
    val values = Array.ofDim[Double](SIZE)
    val Adense = new DenseMatrix2D(A.rows(), A.columns())
    for (i <- 0 until SIZE) {
      rowindexes(i) = i % A.rows()
      columnindexes(i) = i % A.columns()
      values(i) = Math.random()
      Adense.setQuick(rowindexes(i), columnindexes(i), values(i))
    }
    val S = new SparseDoubleMatrix2D(A.rows(), A.columns())
    S.assign(rowindexes, columnindexes, values, DoubleFunctions.multSecond(2))
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(2 * Adense.getQuick(r, c), S.getQuick(r, c))
    }
  }
}
