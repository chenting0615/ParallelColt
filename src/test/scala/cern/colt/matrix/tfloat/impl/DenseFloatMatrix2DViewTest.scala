package cern.colt.matrix.tfloat.impl

import cern.colt.matrix.MatrixTypes.DenseFloatMatrix2D

class DenseFloatMatrix2DViewTest(arg0: String) extends DenseFloatMatrix2DTest(arg0) {

  override protected def createMatrices() {
    A = new DenseFloatMatrix2D(NCOLUMNS, NROWS).viewTranspose()
    B = new DenseFloatMatrix2D(NCOLUMNS, NROWS).viewTranspose()
    Bt = new DenseFloatMatrix2D(NROWS, NCOLUMNS).viewTranspose()
  }
}
