package cern.colt.matrix.tfloat.impl

import cern.colt.matrix.tfloat.FloatMatrix3DTest
//remove if not needed
import scala.collection.JavaConversions._

class SparseFloatMatrix3DTest(arg0: String) extends FloatMatrix3DTest(arg0) {

  protected def createMatrices() {
    A = new SparseFloatMatrix3D(NSLICES, NROWS, NCOLUMNS)
    B = new SparseFloatMatrix3D(NSLICES, NROWS, NCOLUMNS)
  }
}
