package cern.colt.matrix.tint.impl

import cern.colt.matrix.tint.IntMatrix2DTest
//remove if not needed
import scala.collection.JavaConversions._

class SparseCCMIntMatrix2DTest(arg0: String) extends IntMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseCCMIntMatrix2D(NROWS, NCOLUMNS)
    B = new SparseCCMIntMatrix2D(NROWS, NCOLUMNS)
    Bt = new SparseCCMIntMatrix2D(NCOLUMNS, NROWS)
  }
}
