package cern.colt.matrix.tlong.impl

import cern.colt.matrix.tlong.LongMatrix2DTest
//remove if not needed
import scala.collection.JavaConversions._

class SparseRCMLongMatrix2DTest(arg0: String) extends LongMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseRCMLongMatrix2D(NROWS, NCOLUMNS)
    B = new SparseRCMLongMatrix2D(NROWS, NCOLUMNS)
    Bt = new SparseRCMLongMatrix2D(NCOLUMNS, NROWS)
  }
}
