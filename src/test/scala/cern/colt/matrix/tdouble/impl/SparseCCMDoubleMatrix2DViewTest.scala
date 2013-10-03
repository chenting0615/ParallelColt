package cern.colt.matrix.tdouble.impl

import cern.colt.matrix.MatrixTypes.SparseCCMDoubleMatrix2D

class SparseCCMDoubleMatrix2DViewTest(arg0: String) extends SparseCCMDoubleMatrix2DTest(arg0) {

  override protected def createMatrices() {
    A = new SparseCCMDoubleMatrix2D(NCOLUMNS, NROWS).viewTranspose()
    B = new SparseCCMDoubleMatrix2D(NCOLUMNS, NROWS).viewTranspose()
    Bt = new SparseCCMDoubleMatrix2D(NROWS, NCOLUMNS).viewTranspose()
  }
}
