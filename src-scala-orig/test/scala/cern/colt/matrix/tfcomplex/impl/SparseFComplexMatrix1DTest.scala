package cern.colt.matrix.tfcomplex.impl

import cern.colt.matrix.tfcomplex.FComplexMatrix1DTest
//remove if not needed
import scala.collection.JavaConversions._

class SparseFComplexMatrix1DTest(arg0: String) extends FComplexMatrix1DTest(arg0) {

  protected def createMatrices() {
    A = new SparseFComplexMatrix1D(SIZE)
    B = new SparseFComplexMatrix1D(SIZE)
  }
}
