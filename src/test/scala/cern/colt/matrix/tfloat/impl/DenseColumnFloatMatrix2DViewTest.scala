package cern.colt.matrix.tfloat.impl

import cern.colt.matrix.MatrixTypes.DenseColumnFloatMatrix2D
import cern.colt.matrix.MatrixTypes


class DenseColumnFloatMatrix2DViewTest(arg0: String) extends DenseColumnFloatMatrix2DTest(arg0) {

  override protected def createMatrices() {
    A = new DenseColumnFloatMatrix2D(NCOLUMNS, NROWS).viewTranspose()
    B = new MatrixTypes.DenseColumnFloatMatrix2D(NCOLUMNS, NROWS).viewTranspose()
    Bt = new MatrixTypes.DenseColumnFloatMatrix2D(NROWS, NCOLUMNS).viewTranspose()
  }
}
