package cern.colt.matrix.tfcomplex.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseRCMFComplexMatrix2DViewTest(arg0: String) extends SparseRCMFComplexMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseRCMFComplexMatrix2D(NROWS, NCOLUMNS).viewDice()
    B = new SparseRCMFComplexMatrix2D(NROWS, NCOLUMNS).viewDice()
    Bt = new SparseRCMFComplexMatrix2D(NCOLUMNS, NROWS).viewDice()
  }
}
