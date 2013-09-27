package cern.colt.matrix.tdcomplex.impl

import cern.colt.matrix.tdcomplex.DComplexMatrix3DTest
//remove if not needed
import scala.collection.JavaConversions._

class SparseDComplexMatrix3DTest(arg0: String) extends DComplexMatrix3DTest(arg0) {

  protected def createMatrices() {
    A = new SparseDComplexMatrix3D(NSLICES, NROWS, NCOLUMNS)
    B = new SparseDComplexMatrix3D(NSLICES, NROWS, NCOLUMNS)
  }
}
