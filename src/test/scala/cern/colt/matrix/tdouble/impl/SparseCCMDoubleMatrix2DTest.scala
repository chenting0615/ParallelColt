package cern.colt.matrix.tdouble.impl

import cern.colt.matrix.impl.DoubleMatrix2DTest

//remove if not needed
import scala.collection.JavaConversions._

class SparseCCMDoubleMatrix2DTest(arg0: String) extends DoubleMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseCCMDoubleMatrix2D(NROWS, NCOLUMNS)
    B = new SparseCCMDoubleMatrix2D(NROWS, NCOLUMNS)
    Bt = new SparseCCMDoubleMatrix2D(NCOLUMNS, NROWS)
  }
}
