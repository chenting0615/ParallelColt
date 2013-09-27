package cern.colt.matrix.tlong.impl

import cern.colt.matrix.tlong.LongMatrix3DTest
//remove if not needed
import scala.collection.JavaConversions._

class SparseLongMatrix3DTest(arg0: String) extends LongMatrix3DTest(arg0) {

  protected def createMatrices() {
    A = new SparseLongMatrix3D(NSLICES, NROWS, NCOLUMNS)
    B = new SparseLongMatrix3D(NSLICES, NROWS, NCOLUMNS)
  }
}
