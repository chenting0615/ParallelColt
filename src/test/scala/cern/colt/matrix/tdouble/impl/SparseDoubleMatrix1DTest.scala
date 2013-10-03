package cern.colt.matrix.tdouble.impl

import cern.colt.matrix.MatrixTypes.SparseDoubleMatrix1D

class SparseDoubleMatrix1DTest(arg0: String) extends StrideDoubleMatrix1DTest(arg0) {

  override protected def createMatrices() {
    A = new SparseDoubleMatrix1D(SIZE)
    B = new SparseDoubleMatrix1D(SIZE)
  }
}
