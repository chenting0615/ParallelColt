package cern.colt.matrix.tint.impl

import cern.colt.matrix.tint.IntMatrix3DTest
//remove if not needed
import scala.collection.JavaConversions._

class DenseIntMatrix3DTest(arg0: String) extends IntMatrix3DTest(arg0) {

  protected def createMatrices() {
    A = new DenseIntMatrix3D(NSLICES, NROWS, NCOLUMNS)
    B = new DenseIntMatrix3D(NSLICES, NROWS, NCOLUMNS)
  }
}
