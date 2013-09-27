package cern.colt.matrix.tlong.impl

import cern.colt.matrix.tlong.LongMatrix2DTest
//remove if not needed
import scala.collection.JavaConversions._

class DenseLongMatrix2DTest(arg0: String) extends LongMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new DenseLongMatrix2D(NROWS, NCOLUMNS)
    B = new DenseLongMatrix2D(NROWS, NCOLUMNS)
    Bt = new DenseLongMatrix2D(NCOLUMNS, NROWS)
  }

  def testAssignLongArray() {
    val expected = Array.ofDim[Long](A.size.toInt)
    for (i <- 0 until A.size) {
      expected(i) = rand.nextLong()
    }
    A.assign(expected)
    var idx = 0
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(0, Math.abs(expected(idx += 1) - A.getQuick(r, c)))
    }
  }
}
