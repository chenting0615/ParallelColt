package cern.colt.matrix.tint.impl

import cern.colt.matrix.tint.IntMatrix1DTest
//remove if not needed
import scala.collection.JavaConversions._

class DenseIntMatrix1DTest(arg0: String) extends IntMatrix1DTest(arg0) {

  protected def createMatrices() {
    A = new DenseIntMatrix1D(SIZE)
    B = new DenseIntMatrix1D(SIZE)
  }
}
