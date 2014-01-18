package cern.colt.matrix.tfloat.impl

import cern.colt.matrix.MatrixTypes.SparseCCFloatMatrix2D

class SparseCCFloatMatrix2DViewTest(arg0: String) extends SparseCCFloatMatrix2DTest(arg0) {

  override protected def createMatrices() {
    A = new SparseCCFloatMatrix2D(NCOLUMNS, NROWS).viewTranspose()
    B = new SparseCCFloatMatrix2D(NCOLUMNS, NROWS).viewTranspose()
    Bt = new SparseCCFloatMatrix2D(NROWS, NCOLUMNS).viewTranspose()
  }
}
