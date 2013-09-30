package cern.colt.matrix.tdouble.impl

import cern.colt.matrix.impl.DenseMatrix1D

class DenseDoubleMatrix1DViewTest(arg0: String) extends DenseDoubleMatrix1DTest(arg0) {

  override protected def createMatrices() {
    A = new DenseMatrix1D[Double](SIZE).viewFlip()
    B = new DenseMatrix1D[Double](SIZE).viewFlip()
  }
}
