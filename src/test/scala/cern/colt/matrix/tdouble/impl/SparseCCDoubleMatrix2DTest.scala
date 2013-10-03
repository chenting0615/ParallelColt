package cern.colt.matrix.tdouble.impl

import cern.colt.matrix.MatrixTypes.SparseCCDoubleMatrix2D

class SparseCCDoubleMatrix2DTest(arg0: String) extends DoubleMatrix2DTest(arg0) {

  override protected def createMatrices() {
    A = new SparseCCDoubleMatrix2D(NROWS, NCOLUMNS)
    B = new SparseCCDoubleMatrix2D(NROWS, NCOLUMNS)
    Bt = new SparseCCDoubleMatrix2D(NCOLUMNS, NROWS)
  }
}
