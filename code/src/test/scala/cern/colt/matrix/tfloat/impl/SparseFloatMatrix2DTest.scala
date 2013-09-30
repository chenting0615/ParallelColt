package cern.colt.matrix.tfloat.impl

import cern.colt.matrix.tfloat.FloatMatrix2D
import cern.colt.matrix.tfloat.FloatMatrix2DTest
import cern.jet.math.tfloat.FloatFunctions
//remove if not needed
import scala.collection.JavaConversions._

class SparseFloatMatrix2DTest(arg0: String) extends FloatMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseFloatMatrix2D(NROWS, NCOLUMNS)
    B = new SparseFloatMatrix2D(NROWS, NCOLUMNS)
    Bt = new SparseFloatMatrix2D(NCOLUMNS, NROWS)
  }

  def testGetRowCompressed() {
    val SIZE = A.rows() * A.columns()
    val rowindexes = Array.ofDim[Int](SIZE)
    val columnindexes = Array.ofDim[Int](SIZE)
    val values = Array.ofDim[Float](SIZE)
    for (i <- 0 until SIZE) {
      rowindexes(i) = Math.abs(random.nextInt() % NROWS).toInt
      columnindexes(i) = Math.abs(random.nextInt() % NCOLUMNS).toInt
      values(i) = Math.random().toFloat
    }
    val S = new SparseFloatMatrix2D(A.rows(), A.columns(), rowindexes, columnindexes, values)
    var B = S.getRowCompressed(false)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(S.getQuick(r, c), B.getQuick(r, c))
    }
    B = S.getRowCompressed(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(S.getQuick(r, c), B.getQuick(r, c))
    }
  }

  def testGetRowCompressedModified() {
    val SIZE = A.rows() * A.columns()
    val rowindexes = Array.ofDim[Int](SIZE)
    val columnindexes = Array.ofDim[Int](SIZE)
    val values = Array.ofDim[Float](SIZE)
    for (i <- 0 until SIZE) {
      rowindexes(i) = Math.abs(random.nextInt() % NROWS).toInt
      columnindexes(i) = Math.abs(random.nextInt() % NCOLUMNS).toInt
      values(i) = Math.random().toFloat
    }
    val S = new SparseFloatMatrix2D(A.rows(), A.columns(), rowindexes, columnindexes, values)
    val B = S.getRowCompressedModified
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(S.getQuick(r, c), B.getQuick(r, c))
    }
  }

  def testGetColumnCompressed() {
    val SIZE = A.rows() * A.columns()
    val rowindexes = Array.ofDim[Int](SIZE)
    val columnindexes = Array.ofDim[Int](SIZE)
    val values = Array.ofDim[Float](SIZE)
    for (i <- 0 until SIZE) {
      rowindexes(i) = Math.abs(random.nextInt() % NROWS).toInt
      columnindexes(i) = Math.abs(random.nextInt() % NCOLUMNS).toInt
      values(i) = Math.random().toFloat
    }
    val S = new SparseFloatMatrix2D(A.rows(), A.columns(), rowindexes, columnindexes, values)
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
    val values = Array.ofDim[Float](SIZE)
    for (i <- 0 until SIZE) {
      rowindexes(i) = Math.abs(random.nextInt() % NROWS).toInt
      columnindexes(i) = Math.abs(random.nextInt() % NCOLUMNS).toInt
      values(i) = Math.random().toFloat
    }
    val S = new SparseFloatMatrix2D(A.rows(), A.columns(), rowindexes, columnindexes, values)
    val B = S.getColumnCompressedModified
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(S.getQuick(r, c), B.getQuick(r, c))
    }
  }

  def testAssignIntArrayIntArrayFloatArrayFloatFloatFunction() {
    val SIZE = A.rows() * A.columns()
    val rowindexes = Array.ofDim[Int](SIZE)
    val columnindexes = Array.ofDim[Int](SIZE)
    val values = Array.ofDim[Float](SIZE)
    val Adense = new DenseFloatMatrix2D(A.rows(), A.columns())
    for (i <- 0 until SIZE) {
      rowindexes(i) = i % A.rows()
      columnindexes(i) = i % A.columns()
      values(i) = Math.random().toFloat
      Adense.setQuick(rowindexes(i), columnindexes(i), values(i))
    }
    val S = new SparseFloatMatrix2D(A.rows(), A.columns())
    S.assign(rowindexes, columnindexes, values, FloatFunctions.plusMultSecond(2))
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(2 * Adense.getQuick(r, c), S.getQuick(r, c))
    }
  }
}
