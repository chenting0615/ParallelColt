package cern.colt.matrix.tlong.impl

import cern.colt.matrix.tlong.LongMatrix2DTest
//remove if not needed
import scala.collection.JavaConversions._

class DenseColumnLongMatrix2DTest(arg0: String) extends LongMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new DenseColumnLongMatrix2D(NROWS, NCOLUMNS)
    B = new DenseColumnLongMatrix2D(NROWS, NCOLUMNS)
    Bt = new DenseColumnLongMatrix2D(NCOLUMNS, NROWS)
  }

  def testAssignLongArray() {
    val expected = Array.ofDim[Long](A.size.toInt)
    for (i <- 0 until A.size) {
      expected(i) = Math.max(1, rand.nextLong() % A.rows())
    }
    A.assign(expected)
    var idx = 0
    for (c <- 0 until A.columns(); r <- 0 until A.rows()) {
      assertEquals(expected(idx += 1), A.getQuick(r, c))
    }
  }
}
