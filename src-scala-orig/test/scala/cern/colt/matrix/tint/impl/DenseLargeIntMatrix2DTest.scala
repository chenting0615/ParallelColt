package cern.colt.matrix.tint.impl

import cern.colt.matrix.tint.IntMatrix2DTest
//remove if not needed
import scala.collection.JavaConversions._

class DenseLargeIntMatrix2DTest(arg0: String) extends IntMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new DenseLargeIntMatrix2D(NROWS, NCOLUMNS)
    B = new DenseLargeIntMatrix2D(NROWS, NCOLUMNS)
    Bt = new DenseLargeIntMatrix2D(NCOLUMNS, NROWS)
  }
}
