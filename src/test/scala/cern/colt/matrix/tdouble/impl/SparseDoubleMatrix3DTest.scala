package cern.colt.matrix.tdouble.impl

import cern.colt.matrix.impl.DoubleMatrix3DTest

//remove if not needed
import scala.collection.JavaConversions._

class SparseDoubleMatrix3DTest(arg0: String) extends DoubleMatrix3DTest(arg0) {

  protected def createMatrices() {
    A = new SparseDoubleMatrix3D(NSLICES, NROWS, NCOLUMNS)
    B = new SparseDoubleMatrix3D(NSLICES, NROWS, NCOLUMNS)
  }
}
