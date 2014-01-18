package cern.colt.matrix.tfloat.impl

import cern.colt.matrix.MatrixTypes.SparseRCFloatMatrix2D

class SparseRCFloatMatrix2DViewTest(arg0: String) extends SparseRCFloatMatrix2DTest(arg0) {

  override protected def createMatrices() {
    A = new SparseRCFloatMatrix2D(NCOLUMNS, NROWS).viewTranspose()
    B = new SparseRCFloatMatrix2D(NCOLUMNS, NROWS).viewTranspose()
    Bt = new SparseRCFloatMatrix2D(NROWS, NCOLUMNS).viewTranspose()
  }
}
