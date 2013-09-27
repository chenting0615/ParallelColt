package cern.colt.matrix.tdouble.impl

import cern.colt.matrix.impl.DoubleMatrix2DTest

//remove if not needed
import scala.collection.JavaConversions._

class SparseRCMDoubleMatrix2DTest(arg0: String) extends DoubleMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseRCMDoubleMatrix2D(NROWS, NCOLUMNS)
    B = new SparseRCMDoubleMatrix2D(NROWS, NCOLUMNS)
    Bt = new SparseRCMDoubleMatrix2D(NCOLUMNS, NROWS)
  }
}
