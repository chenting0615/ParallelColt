package cern.colt.matrix.tfloat.impl

import cern.colt.matrix.tfloat.FloatMatrix2DTest
import cern.colt.matrix.MatrixTypes.SparseFloatMatrix2D
import org.junit.Assert
import scala.util.Random

class SparseFloatMatrix2DTest(arg0: String) extends FloatMatrix2DTest(arg0) {
  val random = new Random()

  override protected def createMatrices() {
    A = new SparseFloatMatrix2D(NROWS, NCOLUMNS)
    B = new SparseFloatMatrix2D(NROWS, NCOLUMNS)
    Bt = new SparseFloatMatrix2D(NCOLUMNS, NROWS)
  }

  def testGetRowCompressed() {
    val SIZE = A.rows * A.columns
    val rowindexes = Array.ofDim[Int](SIZE)
    val columnindexes = Array.ofDim[Int](SIZE)
    val values = Array.ofDim[Float](SIZE)
    for (i <- 0 until SIZE) {
      rowindexes(i) = Math.abs(random.nextInt() % NROWS)
      columnindexes(i) = Math.abs(random.nextInt() % NCOLUMNS)
      values(i) = Math.random().toFloat
    }
    val S = new SparseFloatMatrix2D(A.rows, A.columns, rowindexes, columnindexes, values)
    var B = S.getRowCompressed
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      Assert.assertEquals(S.getQuick(r, c), B.getQuick(r, c), TOL)
    }
  }

  def testGetRowCompressedModified() {
    val SIZE = A.rows * A.columns
    val rowindexes = Array.ofDim[Int](SIZE)
    val columnindexes = Array.ofDim[Int](SIZE)
    val values = Array.ofDim[Float](SIZE)
    for (i <- 0 until SIZE) {
      rowindexes(i) = Math.abs(random.nextInt() % NROWS)
      columnindexes(i) = Math.abs(random.nextInt() % NCOLUMNS)
      values(i) = Math.random().toFloat
    }
    val S = new SparseFloatMatrix2D(A.rows, A.columns, rowindexes, columnindexes, values)
    val B = S.getRowCompressedModified
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      Assert.assertEquals(S.getQuick(r, c), B.getQuick(r, c), TOL)
    }
  }

  def testGetColumnCompressed() {
    val SIZE = A.rows * A.columns
    val rowindexes = Array.ofDim[Int](SIZE)
    val columnindexes = Array.ofDim[Int](SIZE)
    val values = Array.ofDim[Float](SIZE)
    for (i <- 0 until SIZE) {
      rowindexes(i) = Math.abs(random.nextInt() % NROWS)
      columnindexes(i) = Math.abs(random.nextInt() % NCOLUMNS)
      values(i) = Math.random().toFloat
    }
    val S = new SparseFloatMatrix2D(A.rows, A.columns, rowindexes, columnindexes, values)
    var B = S.getColumnCompressed
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      Assert.assertEquals(S.getQuick(r, c), B.getQuick(r, c), TOL)
    }
  }

  def testGetColumnCompressedModified() {
    val SIZE = A.rows * A.columns
    val rowindexes = Array.ofDim[Int](SIZE)
    val columnindexes = Array.ofDim[Int](SIZE)
    val values = Array.ofDim[Float](SIZE)
    for (i <- 0 until SIZE) {
      rowindexes(i) = Math.abs(random.nextInt() % NROWS)
      columnindexes(i) = Math.abs(random.nextInt() % NCOLUMNS)
      values(i) = Math.random().toFloat
    }
    val S = new SparseFloatMatrix2D(A.rows, A.columns, rowindexes, columnindexes, values)
    val B = S.getColumnCompressedModified
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      Assert.assertEquals(S.getQuick(r, c), B.getQuick(r, c), TOL)
    }
  }

/*
  def testAssignIntArrayIntArrayFloatArrayFloatFloatFunction() {
    val SIZE = A.rows * A.columns
    val rowindexes = Array.ofDim[Int](SIZE)
    val columnindexes = Array.ofDim[Int](SIZE)
    val values = Array.ofDim[Float](SIZE)
    val Adense = new DenseFloatMatrix2D(A.rows, A.columns)
    for (i <- 0 until SIZE) {
      rowindexes(i) = i % A.rows
      columnindexes(i) = i % A.columns
      values(i) = Math.random().toFloat
      Adense.setQuick(rowindexes(i), columnindexes(i), values(i))
    }
    val S = new SparseFloatMatrix2D(A.rows, A.columns)
    S.assign(rowindexes, columnindexes, values, FloatFunctions.plusMultSecond(2))
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      Assert.assertEquals(2 * Adense.getQuick(r, c), S.getQuick(r, c))
    }
  }
*/
}
