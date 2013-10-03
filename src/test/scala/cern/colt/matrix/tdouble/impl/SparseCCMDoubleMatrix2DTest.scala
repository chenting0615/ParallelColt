package cern.colt.matrix.tdouble.impl

import cern.colt.matrix.MatrixTypes.SparseCCMDoubleMatrix2D

class SparseCCMDoubleMatrix2DTest(arg0: String) extends DoubleMatrix2DTest(arg0) {

  override protected def createMatrices() {
    A = new SparseCCMDoubleMatrix2D(NROWS, NCOLUMNS)
    B = new SparseCCMDoubleMatrix2D(NROWS, NCOLUMNS)
    Bt = new SparseCCMDoubleMatrix2D(NCOLUMNS, NROWS)
  }
}
