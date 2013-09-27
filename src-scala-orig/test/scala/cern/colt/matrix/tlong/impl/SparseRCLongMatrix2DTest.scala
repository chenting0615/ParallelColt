package cern.colt.matrix.tlong.impl

import cern.colt.matrix.tlong.LongMatrix2DTest
//remove if not needed
import scala.collection.JavaConversions._

class SparseRCLongMatrix2DTest(arg0: String) extends LongMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseRCLongMatrix2D(NROWS, NCOLUMNS)
    B = new SparseRCLongMatrix2D(NROWS, NCOLUMNS)
    Bt = new SparseRCLongMatrix2D(NCOLUMNS, NROWS)
  }
}
