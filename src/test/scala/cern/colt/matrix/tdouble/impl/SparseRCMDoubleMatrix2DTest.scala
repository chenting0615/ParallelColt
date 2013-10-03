package cern.colt.matrix.tdouble.impl

import cern.colt.matrix.MatrixTypes.SparseRCMDoubleMatrix2D

class SparseRCMDoubleMatrix2DTest(arg0: String) extends DoubleMatrix2DTest(arg0) {

  override protected def createMatrices() {
    A = new SparseRCMDoubleMatrix2D(NROWS, NCOLUMNS)
    B = new SparseRCMDoubleMatrix2D(NROWS, NCOLUMNS)
    Bt = new SparseRCMDoubleMatrix2D(NCOLUMNS, NROWS)
  }
}
