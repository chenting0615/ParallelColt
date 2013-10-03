package cern.colt.matrix.tdouble.impl

import cern.colt.matrix.MatrixTypes.SparseRCDoubleMatrix2D

class SparseRCDoubleMatrix2DViewTest(arg0: String) extends SparseRCDoubleMatrix2DTest(arg0) {

  override protected def createMatrices() {
    A = new SparseRCDoubleMatrix2D(NCOLUMNS, NROWS).viewTranspose()
    B = new SparseRCDoubleMatrix2D(NCOLUMNS, NROWS).viewTranspose()
    Bt = new SparseRCDoubleMatrix2D(NROWS, NCOLUMNS).viewTranspose()
  }
}
