package cern.colt.matrix.tdouble.impl

import cern.colt.matrix.impl.StrideMatrix1DTest

//remove if not needed
import scala.collection.JavaConversions._

class SparseMatrix1DTest(arg0: String) extends StrideMatrix1DTest(arg0) {

  protected def createMatrices() {
    A = new SparseMatrix1D(SIZE)
    B = new SparseMatrix1D(SIZE)
  }
}
