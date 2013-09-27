package cern.colt.matrix.tdcomplex.impl

import cern.colt.matrix.tdcomplex.DComplexMatrix1DTest
//remove if not needed
import scala.collection.JavaConversions._

class SparseDComplexMatrix1DTest(arg0: String) extends DComplexMatrix1DTest(arg0) {

  protected def createMatrices() {
    A = new SparseDComplexMatrix1D(SIZE)
    B = new SparseDComplexMatrix1D(SIZE)
  }
}
