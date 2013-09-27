package cern.colt.matrix.tdouble.impl

import cern.colt.matrix.tdouble.DoubleMatrix1DTest
//remove if not needed
import scala.collection.JavaConversions._

class SparseDoubleMatrix1DTest(arg0: String) extends DoubleMatrix1DTest(arg0) {

  protected def createMatrices() {
    A = new SparseDoubleMatrix1D(SIZE)
    B = new SparseDoubleMatrix1D(SIZE)
  }
}
