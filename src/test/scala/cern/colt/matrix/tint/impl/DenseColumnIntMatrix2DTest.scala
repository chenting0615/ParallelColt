package cern.colt.matrix.tint.impl

import cern.colt.matrix.tint.IntMatrix2DTest
//remove if not needed
import scala.collection.JavaConversions._

class DenseColumnIntMatrix2DTest(arg0: String) extends IntMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new DenseColumnIntMatrix2D(NROWS, NCOLUMNS)
    B = new DenseColumnIntMatrix2D(NROWS, NCOLUMNS)
    Bt = new DenseColumnIntMatrix2D(NCOLUMNS, NROWS)
  }

  def testAssignIntArray() {
    val expected = Array.ofDim[Int](A.size.toInt)
    for (i <- 0 until A.size) {
      expected(i) = Math.max(1, rand.nextInt() % A.rows())
    }
    A.assign(expected)
    var idx = 0
    for (c <- 0 until A.columns(); r <- 0 until A.rows()) {
      assertEquals(expected(idx += 1), A.getQuick(r, c))
    }
  }
}
