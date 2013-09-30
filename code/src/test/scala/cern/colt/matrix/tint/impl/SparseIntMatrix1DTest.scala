package cern.colt.matrix.tint.impl

import cern.colt.matrix.tint.IntMatrix1DTest
//remove if not needed
import scala.collection.JavaConversions._

class SparseIntMatrix1DTest(arg0: String) extends IntMatrix1DTest(arg0) {

  protected def createMatrices() {
    A = new SparseIntMatrix1D(SIZE)
    B = new SparseIntMatrix1D(SIZE)
  }
}
