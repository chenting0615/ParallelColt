package cern.colt.matrix.tdouble.impl

import cern.colt.matrix.MatrixTypes.SparseRCMDoubleMatrix2D

class SparseRCMDoubleMatrix2DViewTest(arg0: String) extends SparseRCMDoubleMatrix2DTest(arg0) {

  override protected def createMatrices() {
    A = new SparseRCMDoubleMatrix2D(NCOLUMNS, NROWS).viewTranspose()
    B = new SparseRCMDoubleMatrix2D(NCOLUMNS, NROWS).viewTranspose()
    Bt = new SparseRCMDoubleMatrix2D(NROWS, NCOLUMNS).viewTranspose()
  }
}
