package cern.colt.matrix.tfloat.impl

import cern.colt.matrix.MatrixTypes.SparseFloatMatrix2D

class SparseFloatMatrix2DViewTest(arg0: String) extends SparseFloatMatrix2DTest(arg0) {

  override protected def createMatrices() {
    A = new SparseFloatMatrix2D(NCOLUMNS, NROWS).viewTranspose()
    B = new SparseFloatMatrix2D(NCOLUMNS, NROWS).viewTranspose()
    Bt = new SparseFloatMatrix2D(NROWS, NCOLUMNS).viewTranspose()
  }
}
