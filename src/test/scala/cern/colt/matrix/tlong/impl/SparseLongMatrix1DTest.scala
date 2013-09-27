package cern.colt.matrix.tlong.impl

import cern.colt.matrix.tlong.LongMatrix1DTest
//remove if not needed
import scala.collection.JavaConversions._

class SparseLongMatrix1DTest(arg0: String) extends LongMatrix1DTest(arg0) {

  protected def createMatrices() {
    A = new SparseLongMatrix1D(SIZE)
    B = new SparseLongMatrix1D(SIZE)
  }
}
