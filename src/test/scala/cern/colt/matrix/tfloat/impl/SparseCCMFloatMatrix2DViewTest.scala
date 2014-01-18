package cern.colt.matrix.tfloat.impl

import cern.colt.matrix.MatrixTypes.SparseCCMFloatMatrix2D

class SparseCCMFloatMatrix2DViewTest(arg0: String) extends SparseCCMFloatMatrix2DTest(arg0) {

  override protected def createMatrices() {
    A = new SparseCCMFloatMatrix2D(NCOLUMNS, NROWS).viewTranspose()
    B = new SparseCCMFloatMatrix2D(NCOLUMNS, NROWS).viewTranspose()
    Bt = new SparseCCMFloatMatrix2D(NROWS, NCOLUMNS).viewTranspose()
  }
}
