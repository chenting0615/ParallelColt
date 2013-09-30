package cern.colt.matrix.tint.impl

import cern.colt.matrix.tint.IntMatrix3DTest
//remove if not needed
import scala.collection.JavaConversions._

class DenseLargeIntMatrix3DTest(arg0: String) extends IntMatrix3DTest(arg0) {

  protected def createMatrices() {
    A = new DenseLargeIntMatrix3D(NSLICES, NROWS, NCOLUMNS)
    B = new DenseLargeIntMatrix3D(NSLICES, NROWS, NCOLUMNS)
  }
}
