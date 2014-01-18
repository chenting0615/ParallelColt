package cern.colt.matrix.tfloat.impl

import cern.colt.matrix.tfloat.FloatMatrix1DTest
import cern.colt.matrix.MatrixTypes.SparseFloatMatrix1D

class SparseFloatMatrix1DTest(arg0: String) extends FloatMatrix1DTest(arg0) {

  protected def createMatrices() {
    A = new SparseFloatMatrix1D(SIZE)
    B = new SparseFloatMatrix1D(SIZE)
  }
}
