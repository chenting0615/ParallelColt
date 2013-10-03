package cern.colt.matrix.tdouble.impl

import cern.colt.matrix.MatrixTypes.DenseLargeDoubleMatrix2D

class DenseLargeDoubleMatrix2DViewTest(arg0: String) extends DenseLargeMatrix2DTest(arg0) {

  override protected def createMatrices() {
    A = new DenseLargeDoubleMatrix2D(NCOLUMNS, NROWS).viewTranspose()
    B = new DenseLargeDoubleMatrix2D(NCOLUMNS, NROWS).viewTranspose()
    Bt = new DenseLargeDoubleMatrix2D(NROWS, NCOLUMNS).viewTranspose()
  }
}
