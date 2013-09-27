package cern.colt.matrix.tint.impl

import cern.colt.matrix.tint.IntMatrix2DTest
//remove if not needed
import scala.collection.JavaConversions._

class SparseRCMIntMatrix2DTest(arg0: String) extends IntMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseRCMIntMatrix2D(NROWS, NCOLUMNS)
    B = new SparseRCMIntMatrix2D(NROWS, NCOLUMNS)
    Bt = new SparseRCMIntMatrix2D(NCOLUMNS, NROWS)
  }
}
