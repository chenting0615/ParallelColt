package cern.colt.matrix.tdouble.impl

import cern.colt.matrix.MatrixTypes.SparseRCDoubleMatrix2D

class SparseRCDoubleMatrix2DTest(arg0: String) extends DoubleMatrix2DTest(arg0) {

  override protected def createMatrices() {
    A = new SparseRCDoubleMatrix2D(NROWS, NCOLUMNS)
    B = new SparseRCDoubleMatrix2D(NROWS, NCOLUMNS)
    Bt = new SparseRCDoubleMatrix2D(NCOLUMNS, NROWS)
  }
}
