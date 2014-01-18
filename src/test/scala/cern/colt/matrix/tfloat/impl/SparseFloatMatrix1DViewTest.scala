package cern.colt.matrix.tfloat.impl

import cern.colt.matrix.MatrixTypes.SparseFloatMatrix1D

class SparseFloatMatrix1DViewTest(arg0: String) extends SparseFloatMatrix1DTest(arg0) {

  override protected def createMatrices() {
    A = new SparseFloatMatrix1D(SIZE).viewFlip()
    B = new SparseFloatMatrix1D(SIZE).viewFlip()
  }
}
