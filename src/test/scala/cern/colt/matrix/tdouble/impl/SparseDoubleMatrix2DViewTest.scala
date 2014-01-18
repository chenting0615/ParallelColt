package cern.colt.matrix.tdouble.impl

import cern.colt.matrix.MatrixTypes.SparseDoubleMatrix2D

class SparseDoubleMatrix2DViewTest(arg0: String) extends SparseDoubleMatrix2DTest(arg0) {

  override protected def createMatrices() {
    val foo = new SparseDoubleMatrix2D(NCOLUMNS, NROWS)
    A = foo.viewTranspose()
    B = new SparseDoubleMatrix2D(NCOLUMNS, NROWS).viewTranspose()
    Bt = new SparseDoubleMatrix2D(NROWS, NCOLUMNS).viewTranspose()
  }
}
