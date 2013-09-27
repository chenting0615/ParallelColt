package cern.colt.matrix.tdouble.impl

import cern.colt.matrix.tdouble.DoubleMatrix2DTest
//remove if not needed
import scala.collection.JavaConversions._

class SparseRCDoubleMatrix2DTest(arg0: String) extends DoubleMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseRCDoubleMatrix2D(NROWS, NCOLUMNS)
    B = new SparseRCDoubleMatrix2D(NROWS, NCOLUMNS)
    Bt = new SparseRCDoubleMatrix2D(NCOLUMNS, NROWS)
  }
}
