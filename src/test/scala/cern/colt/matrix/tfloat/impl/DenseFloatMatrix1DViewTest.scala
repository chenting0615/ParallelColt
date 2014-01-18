package cern.colt.matrix.tfloat.impl

import cern.colt.matrix.MatrixTypes.DenseFloatMatrix1D

class DenseFloatMatrix1DViewTest(arg0: String) extends DenseFloatMatrix1DTest(arg0) {

  override protected def createMatrices() {
    A = new DenseFloatMatrix1D(SIZE).viewFlip()
    B = new DenseFloatMatrix1D(SIZE).viewFlip()
  }
}
