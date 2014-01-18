package cern.colt.matrix.tfloat.impl

import cern.colt.matrix.MatrixTypes.SparseRCMFloatMatrix2D

class SparseRCMFloatMatrix2DViewTest(arg0: String) extends SparseRCMFloatMatrix2DTest(arg0) {

  override protected def createMatrices() {
    A = new SparseRCMFloatMatrix2D(NCOLUMNS, NROWS).viewTranspose()
    B = new SparseRCMFloatMatrix2D(NCOLUMNS, NROWS).viewTranspose()
    Bt = new SparseRCMFloatMatrix2D(NROWS, NCOLUMNS).viewTranspose()
  }
}
