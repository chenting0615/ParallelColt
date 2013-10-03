package cern.colt.matrix.tdouble.impl

import cern.colt.matrix.MatrixTypes.DenseColumnDoubleMatrix2D

class DenseColumnDoubleMatrix2DViewTest(arg0: String) extends DenseColumnDoubleMatrix2DTest(arg0) {

  override protected def createMatrices() {
    A = new DenseColumnDoubleMatrix2D(NCOLUMNS, NROWS).viewTranspose()
    B = new DenseColumnDoubleMatrix2D(NCOLUMNS, NROWS).viewTranspose()
    Bt = new DenseColumnDoubleMatrix2D(NROWS, NCOLUMNS).viewTranspose()
  }
}
