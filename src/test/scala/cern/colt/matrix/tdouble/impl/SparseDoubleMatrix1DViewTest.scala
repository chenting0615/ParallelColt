package cern.colt.matrix.tdouble.impl

import cern.colt.matrix.MatrixTypes.SparseDoubleMatrix1D

class SparseDoubleMatrix1DViewTest(arg0: String) extends SparseMatrix1DTest(arg0) {

  override protected def createMatrices() {
    A = new SparseDoubleMatrix1D(SIZE).viewFlip()
    B = new SparseDoubleMatrix1D(SIZE).viewFlip()
  }
}
