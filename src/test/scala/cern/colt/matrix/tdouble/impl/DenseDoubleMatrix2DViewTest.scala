package cern.colt.matrix.tdouble.impl

import cern.colt.matrix.MatrixTypes.DenseDoubleMatrix2D

class DenseDoubleMatrix2DViewTest(arg0: String) extends DenseMatrix2DTest(arg0) {

  override protected def createMatrices() {
    A = new DenseDoubleMatrix2D(NCOLUMNS, NROWS).viewTranspose()
    B = new DenseDoubleMatrix2D(NCOLUMNS, NROWS).viewTranspose()
    Bt = new DenseDoubleMatrix2D(NROWS, NCOLUMNS).viewTranspose()
  }
}
