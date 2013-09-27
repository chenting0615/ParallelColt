package cern.colt.matrix.tlong.impl

import cern.colt.matrix.tlong.LongMatrix1DTest
//remove if not needed
import scala.collection.JavaConversions._

class DenseLongMatrix1DTest(arg0: String) extends LongMatrix1DTest(arg0) {

  protected def createMatrices() {
    A = new DenseLongMatrix1D(SIZE)
    B = new DenseLongMatrix1D(SIZE)
  }
}
