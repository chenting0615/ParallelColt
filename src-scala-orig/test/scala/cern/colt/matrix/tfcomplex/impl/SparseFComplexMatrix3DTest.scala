package cern.colt.matrix.tfcomplex.impl

import cern.colt.matrix.tfcomplex.FComplexMatrix3DTest
//remove if not needed
import scala.collection.JavaConversions._

class SparseFComplexMatrix3DTest(arg0: String) extends FComplexMatrix3DTest(arg0) {

  protected def createMatrices() {
    A = new SparseFComplexMatrix3D(NSLICES, NROWS, NCOLUMNS)
    B = new SparseFComplexMatrix3D(NSLICES, NROWS, NCOLUMNS)
  }
}
