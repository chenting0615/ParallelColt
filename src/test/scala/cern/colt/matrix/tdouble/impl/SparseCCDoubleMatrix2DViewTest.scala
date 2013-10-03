package cern.colt.matrix.tdouble.impl

import cern.colt.matrix.MatrixTypes.SparseCCDoubleMatrix2D

class SparseCCDoubleMatrix2DViewTest(arg0: String) extends SparseCCDoubleMatrix2DTest(arg0) {

  override protected def createMatrices() {
    A = new SparseCCDoubleMatrix2D(NCOLUMNS, NROWS).viewTranspose()
    B = new SparseCCDoubleMatrix2D(NCOLUMNS, NROWS).viewTranspose()
    Bt = new SparseCCDoubleMatrix2D(NROWS, NCOLUMNS).viewTranspose()
  }
}
