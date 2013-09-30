package cern.colt.matrix.tint.impl

import cern.colt.matrix.tint.IntMatrix2DTest
//remove if not needed
import scala.collection.JavaConversions._

class DenseIntMatrix2DTest(arg0: String) extends IntMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new DenseIntMatrix2D(NROWS, NCOLUMNS)
    B = new DenseIntMatrix2D(NROWS, NCOLUMNS)
    Bt = new DenseIntMatrix2D(NCOLUMNS, NROWS)
  }

  def testAssignIntArray() {
    val expected = Array.ofDim[Int](A.size.toInt)
    for (i <- 0 until A.size) {
      expected(i) = rand.nextInt()
    }
    A.assign(expected)
    var idx = 0
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(0, Math.abs(expected(idx += 1) - A.getQuick(r, c)))
    }
  }
}
