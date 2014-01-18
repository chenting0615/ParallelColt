package cern.colt.matrix.tfloat.impl

import cern.colt.matrix.MatrixTypes.DenseLargeFloatMatrix2D

class DenseLargeFloatMatrix2DViewTest(arg0: String) extends DenseLargeFloatMatrix2DTest(arg0) {

  override protected def createMatrices() {
    A = new DenseLargeFloatMatrix2D(NCOLUMNS, NROWS).viewTranspose()
    B = new DenseLargeFloatMatrix2D(NCOLUMNS, NROWS).viewTranspose()
    Bt = new DenseLargeFloatMatrix2D(NROWS, NCOLUMNS).viewTranspose()
  }
}
