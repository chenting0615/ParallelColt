package cern.colt.matrix.tdouble.impl

import cern.colt.matrix.impl.DenseMatrix1D

class DenseDoubleMatrix1DTest(arg0: String) extends StrideDoubleMatrix1DTest(arg0) {

  protected def createMatrices() {
    A = new DenseMatrix1D[Double](SIZE)
    B = new DenseMatrix1D[Double](SIZE)
  }
}
