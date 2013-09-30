package cern.colt.matrix.tfcomplex.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseFComplexMatrix3DViewTest(arg0: String) extends SparseFComplexMatrix3DTest(arg0) {

  protected def createMatrices() {
    A = new SparseFComplexMatrix3D(NCOLUMNS, NROWS, NSLICES)
      .viewDice(2, 1, 0)
    B = new SparseFComplexMatrix3D(NCOLUMNS, NROWS, NSLICES)
      .viewDice(2, 1, 0)
  }
}
